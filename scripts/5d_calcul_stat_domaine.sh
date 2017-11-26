#!/bin/bash
#
# Controle les sorties cartoprox_nox et cartoprox_PM10 
# et calcule les statistiques par mini-domaines
######################################################

### FICHIERS ENTREES DE SIRANE
localdir=`pwd`
script=5b_stat_CARTOPROX_minimaille.sh

# Laisse le choix d'utiliser des recepteurs USER (yes/no)
irecept=no #yes

periode_defaut=2009

##### debut d'intervention utilisateur
case $# in
1)
code_domaine=$1
annee=${periode_defaut}
anneeplus1=`expr ${annee} \+ 1`
deb_j=${annee}0101
deb_h=00
fin_j=${anneeplus1}0101
fin_h=23
;;
3)
code_domaine=$1
deb_j=$2
deb_h=00
fin_j=$3
fin_h=23
;;
5)
code_domaine=$1
deb_j=$2
deb_h=$3
fin_j=$4
fin_h=$5
;;
*)
echo "Syntaxe $0 code_domaine debut[YYYYMMJJ (HH)] fin[YYYYMMJJ (HH)]"
exit
;;
esac

#par defaut
export polluant=1

############################################################################################
# INCLUDE
############################################################################################
# declaration de la periode de calcul 
if [ "${periode}" == "" ] ; then
  periode=${deb_j}_${fin_j}
fi

deb_j=`echo ${periode} | gawk -F "_" '{print $1}'`
fin_j=`echo ${periode} | gawk -F "_" '{print $2}'`
annee=`date -d ${deb_j} +%Y`

#Include
source ${cartoprox}/cartoprox.inc ${periode} || \
  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; }
############################################################################################

## TMPDIR ###########################################
TMPDIR=${SCRATCH}/${script}.${cartoprox_domaine}_${code_domaine}.$USER
echo "Travaille dans ${TMPDIR}"
mkdir -p ${TMPDIR}

# parametres du run
do_statnc=0  #..statistiques NetCDF (seulement si do_nco=1)
do_clean=1   #..Clean SCRATCH

############### Transformation dates -> iterations ############
deb_a=`date -d "${deb_j}" +%Y`
fin_a=`date -d "${fin_j}" +%Y`
nrs=`date -d "${deb_j}" +%j | gawk '{ print ($1-1)*24 + '${deb_h}' + 1 }'`
if [ ${fin_a} -eq ${deb_a} ] ; then # multi-annees ?
  nre=`date -d "${fin_j}" +%j | gawk '{ print ($1-1)*24 + '${fin_h}' + 1 }'`
else
  fin_tmp_j=${deb_a}1231
  nre_1=`date -d "${fin_tmp_j}" +%j | gawk '{ print ($1-1)*24 + 23 + 1 }'`
  nre_2=`date -d "${fin_j}" +%j | gawk '{ print ($1-1)*24 + '${fin_h}' + 1 }'`  
  nre=`expr ${nre_1} \+ ${nre_2}`
fi
NTOTAL=`date -d "${deb_a}1231" +%j | gawk '{ print $1 * 24 }'`

#Pour 2010: l'année n'est pas complète
if [ ${NTOTAL} -gt ${nre} ] ; then
  NTOTAL=${nre}
fi

#echo "*** info: Liste des iterations : ${nrs} ->  ${nre} (NTOTAL=${NTOTAL})"

################### Definition du run ###################
run=${deb_j}${deb_h}_${fin_j}${fin_h}

#NOM DES FICHIERS
ChemRes=${ChemRes1}/${code_domaine}

#fic_cartoprox_nc= #${ChemRes}/cartoprox.${run}.nc      #cartoprox_*.nc
fic_cartoprox_nc[1]=${ChemRes}/cartoprox_nox.${run}.nc  #cartoprox_*.nc
fic_cartoprox_nc[2]=${ChemRes}/cartoprox_PM10.${run}.nc #cartoprox_*.nc
fic_cartoprox_nc[3]=${ChemRes}/cartoprox_PM25.${run}.nc #cartoprox_*.nc

fic_sirane_nc[1]=${ChemRes}/sirane_nox.${run}.nc  #sirane_*.nc
fic_sirane_nc[2]=${ChemRes}/sirane_PM10.${run}.nc #sirane_*.nc
fic_sirane_nc[3]=${ChemRes}/sirane_PM25.${run}.nc #sirane_*.nc

fic_suremis_nc[1]=${ChemRes}/suremis_nox.${run}.nc  #sirane_*.nc
fic_suremis_nc[2]=${ChemRes}/suremis_PM10.${run}.nc #sirane_*.nc
fic_suremis_nc[3]=${ChemRes}/suremis_PM25.${run}.nc #sirane_*.nc

statFile=${ChemRes}/stat.${run}.nc

# test le nombre de recepteurs dans le fichier STATISTIQUES
source ./utils/infos_mailleur.sh ${cartoprox_domaine} ${code_domaine} ${periode}
echo "====================================================================================================="
echo "Domaine ${cartoprox_domaine} mini-domaine ${code_domaine} xc=${xc} yc=${yc} dx=${dx} cadre_dx=${cadre_dx}"
echo "====================================================================================================="	 
#nrecepts=`./utils/applique_masque_agglo.sh  ${cartoprox_domaine} ${code_domaine} | wc -l `

#Calcul du nombre de recepteurs SIRANE a partir des fichiers de mailleur
nrecepts=`cat ${ficrecept_mailleur} ${ficbrin_mailleur} | wc -l`

#TEST1: VERIFIE L EXISTENCE DU FICHIER ET LE NOMBRE DE RECEPTEURS
test1=0
test2=0

#liste_fic="${fic_cartoprox_nc[1]} ${fic_cartoprox_nc[2]} ${fic_cartoprox_nc[3]} \
#${fic_sirane_nc[1]} ${fic_sirane_nc[2]} ${fic_sirane_nc[3]}"

liste_fic="${fic_cartoprox_nc[1]} ${fic_cartoprox_nc[2]} \
${fic_sirane_nc[1]} ${fic_sirane_nc[2]} "

for file in ${liste_fic} ; do

if [ -f ${file} ] ; then
  echo -e "$VERT" "Test 1 OK : fichier existe ${file}"
  npoints=`ncdump -h ${file} | grep "Point =" | sed 's/;//g' | sed 's/ //g' | sed 's/\t//g' | gawk -F '=' '{print $2}'`
  if [ ${npoints} -ne ${nrecepts} ] ; then
    echo "Domaine ${idom} sur ${ndom}: recepteurs ${npoints}/${nrecepts} PB (${file})"   
    echo -e "$ROUGE" "Test 2 FAILED : ${npoints} different de ${nrecepts} dans fichier ${file}" "$NORMAL"
    test2=1 #PB    
    echo -n "ATTENTION : supprime ${file} dans 3 secs..."
    #sleep 3s
    rm ${file}
    echo -e "$ROUGE" "SUPPRIME" "$NORMAL"    
    fic_suremis=`echo  ${file} | sed -e "s#/sirane_#/suremis_#" | sed -e "s#/cartoprox_#/cartoprox_#"`
    if [ -f ${fic_suremis} ] ; then
      echo -n "ATTENTION : supprime ${fic_suremis} dans 3 secs..."
      echo -e "$ROUGE" "SUPPRIME" "$NORMAL"    
    fi
  else
    echo -e "$VERT" "Test 2 OK : ${npoints}/${nrecepts} fichier ${file} OK" "$NORMAL"
  fi
else
  echo -e "$ROUGE" "Test 1 FAILED : fichier absent ${file}" "$NORMAL"
  test1=1 #PB
fi #file

done

#TEST 2: VERIFIE L'EXPORT
test3=0
test4=0
if [ -f ${statFile} ] ; then
  echo "Test 3 OK : fichier existe ${statFile}"
  for var in no2_moy_an pm10_moy_an nb_dep_50_jour ; do
    typ_op=""
    nrs=""
    nre=""
    syntaxe="../utils/carto_gmt/export_cdf_ascii.sh ${statFile} ${var} ${typ_op} ${nrs} ${nre}"
    ${syntaxe} > ${TMPDIR}/conc.${var} || \
      { echo "export_cdf_ascii.sh: ERREUR dans la lecture de ${file}" ; exit 1 ; }
    npoints=`cat ${TMPDIR}/conc.${var} | wc -l`
    if [ ${npoints} -ne ${nrecepts} ] ; then
      # Si probleme dans l'export...
      echo -e "$ROUGE" "Test 4 FAILED : variable ${var}:${npoints}/${nrecepts} fichier ${statFile} -> do_statnc=1" "$NORMAL"
      echo -n "ATTENTION : supprime ${statFile} dans 3 secs..."
      rm ${statFile}
      echo -e "$ROUGE" "SUPPRIME" "$NORMAL"
      test4=1
    else
      echo -e "$VERT" "Test 4 OK : variable ${var}:${npoints}/${nrecepts} fichier ${statFile}" "$NORMAL"
    fi
  done

else
  echo "Test 3 FAILED : fichier absent ${statFile}"
  test3=1
fi #file

#Si les test 1 et 2 sont OK${test2} et que le test 3 est FAILED
#echo ${test1} ${test2} ${test3} ${test4}
test5=`expr ${test3} \+ ${test4}`
echo -n "test1=${test1} test2=${test2} test3=${test3} test4=${test4} test5=${test5}"
if [ ${test1} -eq 0 ] && [ ${test2} -eq 0 ] && [ ${test5} -ge 1 ] ; then # test 3 peu importe
  do_statnc=1
  echo "-> genere le fichier ${statFile} (do_statnc=1)"
else
  do_statnc=0
  echo "-> ne genere pas le fichier ${statFile} (do_statnc=0)"  
fi

#do_statnc=0

#DEBUG
#exit 1
#rm -rf ${TMPDIR};exit 1

if [ ${do_statnc} -eq 1 ] ; then    
# Calcule les Statistiques du run a la volée (independant du run PM10 ou gaz)

#rm -f ${statFile}

for file in ${fic_cartoprox_nc[1]} ${fic_cartoprox_nc[2]} ${fic_cartoprox_nc[3]}  ; do

syntaxe="${statistics_exe} ${file} ${statFile}" #> ${localdir}/logs/sirane2cdf_stats.log  2>&1 "
echo "Calcul STATISTIQUES en cours sur ${file} -> ${cartoprox}/logs/sirane2cdf_stats.log"
${syntaxe} \
 || { echo "$0: ERREUR du calcul des STATS ${statFile}" ; exit 1 ; }

done

if [ -f ${statFile} ] ; then
  echo "-> Statistiques CARTOPROX NetCDF OK dans ${statFile} "  
fi

fi

if test ${do_clean} -eq 1 ; then
  echo "-> Supprime ${TMPDIR}" # dans 5 secondes"
  #sleep 5  
  rm -rf ${TMPDIR}
fi #do_clean


echo "*** info: $0 - fin du traitement `date`"

if [ ! -f ${statFile} ] ; then 
  exit 1
else
  exit 0
fi

