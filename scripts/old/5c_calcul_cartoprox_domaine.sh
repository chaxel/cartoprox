#!/bin/bash
#
# creation de donnees.dat pour sirane LYON 
# + lancement de run mensuel
# 
# usage : run_mensuel_CARTOPROX_ville.sh + le mois voulu
#  ou     run_mensuel_CARTOPROX_ville.sh 
#         dans ce dernier cas : boucle sur les mois de liste_mois
#############################################

### FICHIERS ENTREES DE SIRANE
localdir=`pwd`

# Laisse le choix d'utiliser des recepteurs USER (yes/no)
irecept=no #yes

deb_h=00
fin_h=23

##### debut d'intervention utilisateur
case $# in
1)
code_domaine=$1
periode=2009
annee=2009
anneeplus1=`expr ${periode} \+ 1`
deb_j=${annee}0101
fin_j=${anneeplus1}0101
;;
3)
code_domaine=$1
deb_j=$2
fin_j=$3
;;
4)
code_domaine=$1
deb_j=$2
fin_j=$3
irecept=no
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

############################################################################################
# POLLUANT
############################################################################################
case ${polluant} in
0|1|2)echo "polluant=${polluant}";; 
*)
echo "$0: Fixer export polluant="
exit 1
;;
esac

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

############################################################################################

### dependant du domaine de calcul#####################################
source ./utils/infos_domaine.sh ${cartoprox_domaine} ${code_domaine} ${periode}
#######################################################################

if [ "${periode}" == "" ] ; then
  echo "Maille ${cartoprox_domaine} et mini-domaine ${code_domaine} inconnu dans ${params_mailles}"
  exit
else
  echo "====================================================================================================="
  echo "Domaine ${cartoprox_domaine} mini-domaine ${code_domaine} xc=${xc} yc=${yc} dx=${dx} cadre_dx=${cadre_dx}"
  echo "====================================================================================================="     
fi

# parametres du run
do_cartoprox=1 #..genere fichiers cartoprox

#DEBUG
drop_sirane=0          #..supprime le fichier NetCDF de resultats SIRANE aux recepteurs (ATTENTION !)
drop_input=1           #..supprime les fichiers NetCDF REGIONAL & SUREMIS existants
drop_output_suremis=0  #..supprime les fichier NetCDF SUREMIS aux recepteurs (les conserver pour statistiques)
drop_output_reg=0      #..supprime les fichier NetCDF REGIONAL aux recepteurs (les conserver pour statistiques)

############### Transformation dates -> iterations ############
deb_a=`date -d "${deb_j}" +%Y`
fin_a=`date -d "${fin_j}" +%Y`
if [ ${deb_a} -eq ${fin_a} ] ; then
  deb_julh=`./utils/date2iter.sh ${deb_j} ${deb_h}` #iteration par rapport au 1 jan    
  fin_julh=`./utils/date2iter.sh ${fin_j} ${fin_h}`
else
  echo "Attention: annee de debut differente de l'annee de fin !"
  nre_1=`./utils/date2iter.sh ${deb_a}1231 23`
  nre_2=`./utils/date2iter.sh ${fin_j} ${fin_h}`
  nre=`expr ${nre_1} \+ ${nre_2}`
  exit 1
fi
niter=`expr ${fin_julh} \- ${deb_julh} \+ 1`

#Seulement J-1 et J0
#niter=48

nrs=1
nre=`expr ${nrs} \+ ${niter} \- 1`

echo "*** info: Liste des iterations : ${nrs} ->  ${nre} (niter=${niter})"
################### Definition du run ###################
run=${deb_j}${deb_h}_${fin_j}${fin_h}

case ${polluant} in
0)polList="${var_traceur}";prefix_nc="_traceur";;
1)liste_var="O3 NO NO2";prefix_nc="_nox";oper="_h";;
2)liste_var="${var_pm}";prefix_nc="_${var_pm}";oper="_avg24";;
esac

fic_sirane_nc=${ChemRes}/sirane${prefix_nc}.${run}.nc            #sirane_*.nc
fic_carto_nc=${ChemRes}/cartoprox${prefix_nc}.${run}.nc    #cartoprox_*.nc
fic_reg_nc=${ChemRes}/reg${prefix_nc}.${run}.recepts.nc
fic_sur_nc=${ChemRes}/suremis${prefix_nc}.${run}.recepts.nc

for fic in ${fic_sirane_nc} ${fic_carto_nc} ; do
  if [ -f ${fic} ] ; then
    echo -e "$VERT" "${fic} OK" "$NORMAL" 
  else
    echo -e "$ROUGE" "${fic} INDISPONIBLE" "$NORMAL" 
  fi
done

echo "do_cartoprox=${do_cartoprox}"
echo "drop_sirane=${drop_sirane}"
echo "drop_input=${drop_input}"
echo "drop_output=${drop_output}"
##########################################################################################
# Extrait PREVALP aux recepteurs et rempli la variable pol_grille
##########################################################################################
#Calcul les suremissions aux recepteurs
for prefix in out suremis ; do

case ${prefix} in
out)ifnout=${fic_reg_nc};;
suremis)ifnout=${fic_sur_nc};;
esac

if [ ${drop_input} -eq 1 ] && [ -f ${ifnout} ] ; then
  echo "Supprime ${ifnout}"
  rm ${ifnout} #supprime le fichier REGIONAL (gain de place)
fi 

fond_fic_maille=${prevalp_raster}/${cartoprox_domaine}/${prefix}${prefix_nc}.${deb_j}${deb_h}_${fin_j}${fin_h}_${dom_chimere}${selection}.nc
xminc=`echo ${maille_xmin} ${fond_dx} | awk '{print int($1 - $2) }'`
yminc=`echo ${maille_ymin} ${fond_dx} | awk '{print int($1 - $2) }'`
dxc=${fond_dx}
echo -e "$BLEU" "Extrait les valeurs de PREVALP aux recepteurs" "$NORMAL"
  
if [ ! -f ${fond_fic_maille} ] ; then
  echo -e "$ROUGE" "ERREUR : Fichier REGIONAL n existe pas: fond_fic_maille=${fond_fic_maille}" "$NORMAL"
  echo "Lancer ./utils/extrait_mini_reg.sh ${deb_j} ${fin_j}"
  exit 1
else
  echo -e "$VERT" "Fichier REGIONAL ${fond_fic_maille} OK" "$NORMAL"
fi  

#Creer le fichier de stations PREVALP a partir du fichier SIRANE
if [ ! -f ${ifnout} ] ; then
  echo "Genere ${ifnout} a partir de ${fic_sirane_nc}"
  ncks -O -v Times,easting_pts,northing_pts -o ${ifnout} ${fic_sirane_nc}
  if [ ! -f ${ifnout} ] ; then
    echo -e "$ROUGE" "ERREUR : Fichier STATIONS/DATE n existe pas: ${ifnout}" "$NORMAL"
    exit 1
  else
    echo -e "$VERT" "Fichier STATIONS/DATE ${ifnout} OK" "$NORMAL"
  fi

  for var in ${liste_var} ; do
    #Pour les PM10, utilise un fichier PREVALP journalier
    if [ "${var}" == "PM10" ] ; then
      ijour="-jour"
    else
      ijour=
    fi
    syntaxe="${extract_val_grille_exe} -i ${fond_fic_maille} -xmin ${xminc} -ymin ${yminc} -dx ${dxc} -s ${ifnout} -var ${var} ${ijour}" 
    echo $syntaxe
    ${syntaxe} \
      || { echo "$0: ERREUR de l extraction PREVALP -> ${ifnout}" ; exit 1 ; }
   echo "-> ${var} OK"
  done
  do_cartoprox=1
else
  echo -e "$VERT"  "${ifnout}" "$NORMAL"
fi #${ifnout} existe ?

done #out suremis

##########################################################################################
# Calcule la concentration CARTOPROX aux recepteurs...
# conc = conc_sirane - conc_fond + conc_prevalp             (version 2)
# conc = conc_sirane - conc_fond + conc_prevalp - conc_prox (version 3)
##########################################################################################
if [ ${do_cartoprox} -eq 1 ] ; then

cd ${ChemRes}

echo -e "$BLEU" "Calcul la concentration CARTOPROX (do_cartoprox=${do_cartoprox})" "$NORMAL"
#Genere la liste des variables a placer dans le fichier cartoprox
liste_var_csv=""
for var in ${liste_var} ; do
  liste_var_csv="${liste_var_csv},${var}" #,${var}_fond" # ,${var}_prox" #,${var}_grille
done

#Extrait de SIRANE les variables de $liste_var + x_pts + y_pts +  easting_pts + easting_pts
#ATTENTION: bien s'assurer de leur présence dans SIRANE !
liste_var_csv="x_pts,y_pts,easting_pts,northing_pts,area_pts,Times" #,vars${liste_var_csv}"

# -> genere le fichier de sortie ${fic_carto_nc}
#if [ ! -f ${fic_carto_nc} ] ; then
echo "Extraction de ${liste_var_csv} pour pas de temps 1"
echo "${fic_sirane_nc} -> ${fic_carto_nc}"
syntaxe="${ncks} -O -v ${liste_var_csv} -d Time,0,0 ${fic_sirane_nc} ${fic_carto_nc}"    #ML 27072011
echo ${syntaxe}
${syntaxe} || \
  { echo "$0: ERREUR de l extraction ${ncks}:" ; echo ${syntaxe} ; exit 1 ; }
#fi

# Applique le calcul CARTOPROX = SIRANE - FOND + GRILLE (que faire des valeurs non valides ?)
# exemple : NO2 = NO2_sirane - NO2_fond + NO2_grille - NO2_prox
echo "Calcul CARTOPROX (cartoprox=sirane+grille-fond) en cours"
syntaxe="${calc_cartoprox_nc_exe} ${fic_sirane_nc} ${fic_reg_nc} ${fic_sur_nc} ${fic_carto_nc}"
echo ${syntaxe}
${syntaxe}  || \
  { echo "$0: ERREUR du calcul CARTOPROX:" ; echo ${syntaxe} ; exit 1 ; }
fi #do_cartoprox

##########################################################################################
# Verfifie les sorties NetCDF
##########################################################################################
ifnout_ok=0
ifnout=${fic_carto_nc}
if [ -f ${ifnout} ] ; then # simul existe ?
  ifnout_steps=`ncdump -h ${ifnout} | grep "currently" | gawk -F '( // |  )' '{print $2}' | sed 's#(##g' | sed 's# currently)##g'`
  simul_steps=${niter}
  if [ ${ifnout_steps} -eq ${simul_steps} ] ; then
    echo -e "Fichier ${ifnout} steps: $VERT ${ifnout_steps}/${simul_steps}" "$NORMAL"
    ifnout_ok=1
  else
    echo -e "Fichier ${ifnout} steps: $ROUGE ${ifnout_steps}/${simul_steps}" "$NORMAL"
    ifnout_ok=0
  fi
else
  ifnout_ok=0
fi

#Verifie les sorties
#for var in ${liste_var}ds ; do
#  #typ_op="avg"
#  syntaxe="../utils/carto_gmt/export_cdf_ascii.sh ${ifnout} ${var} ${typ_op} ${nrs} ${nre}"
#  npoints=`${syntaxe} > ${ifnout}.tmp.${var}` || \
#    { echo "export_cdf_ascii.sh: ERREUR dans la lecture de ${ifnout}" ; exit 1 ; }
#  if [ ${npoints} -ne ${nrecepts} ] ; then
#    echo "Variable ${var}:${npoints}/${nrecepts} fichier ${ifnout}"   
#  fi  
#done

if [ ${ifnout_ok} -eq 1 ] ; then # simul existe ?  
  echo -e "$0:" "$VERT" "SUCCESS" "$NORMAL"
  #Supprime SIRANE aux recepteurs?
  if [ ${drop_sirane} -eq 1 ] && [ -f ${fic_sirane_nc} ] ; then
    echo "Supprime ${fic_sirane_nc}"
    rm ${fic_sirane_nc} #supprime le fichier SIRANE (gain de place)
  fi
  #Supprime SUREMIS aux recepteurs?  
  if [ ${drop_output_suremis} -eq 1 ] && [ -f ${fic_sur_nc} ] ; then
    echo "Supprime ${fic_sur_nc}"
    rm -f ${fic_sur_nc} #supprime le fichier SUREMIS (gain de place)
  fi 
  #Supprime REGIONAL aux recepteurs?   
  if [ ${drop_output_reg} -eq 1 ] && [ -f ${fic_reg_nc} ] ; then
    echo "Supprime ${fic_reg_nc}"
    rm -f ${fic_reg_nc} #supprime le fichier REGIONAL (gain de place)
  fi   
else
  echo -e "$0:" "$ROUGE" "ERREUR de $0: pas de fichier de sortie ${ifnout} genere" "$NORMAL"
  rm -f ${ifnout}
  exit 1
fi

echo "*** info: ${run} : fin du traitement `date`"
