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
recept_user=no #yes

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
recept_user=no
;;
5)
code_domaine=$1
deb_j=$2
deb_h=$3
fin_j=$4
fin_h=$5
;;
6)
code_domaine=$1
deb_j=$2
deb_h=$3
fin_j=$4
fin_h=$5
recept_user=no
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
0|1|2|3)echo "polluant=${polluant}";; 
*)
echo "$0: Fixer polluant="
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
annee=`date -d "${deb_j}" +%Y`

#Include
source ${cartoprox}/cartoprox.inc ${periode} || \
  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; }
############################################################################################

############################################################################################
ChemRes=${ChemRes1}/${code_domaine}

# parametres du run
do_dat=1       #..creation fichier de données input_$run.dat, de l'executable sirane et des fichiers d'entrees
do_press=1     #..calcul d'une pression par periode a partir d un fichier annuel
do_emis=1      #..calcul des émissions
do_run=1       #..activer les runs SIRANE
do_clean=1     #..supprimer les repertoires temporaires a la fin des runs mensuels
do_netcdf=1    #..conversion recepteurs ASCII SIRANE -> NetCDF
do_suremis=1   #..calcule la suremissions de SIRANE sur la maille PREVALP

#DEBUG
drop_sirane=0  #..supprime le fichier NetCDF de resultats SIRANE aux recepteurs

# parametres entrees/sorties
calcul_grille=0  #..run avec ou sans grille (0/1) [ ATTENTION : pour CARTOPROX, choisir 0]
Classout=0       #..0=un fichier par iteration 1=un fichier par recepteur [ ATTENTION : pour CARTOPROX, choisir 0]
output=0         #..niveau de detail de l'affichage [min 0..2 max]

################### Definition du run ###################
run=${deb_j}${deb_h}_${fin_j}${fin_h}

case ${polluant} in
0)polList="${var_traceur}";prefix_nc="_traceur";;
1)polList="O3 NO NO2 NOX";prefix_nc="_nox";;
2|3)polList="${var_pm}";prefix_nc="_${var_pm}";;
esac

fic_sirane_nc=${ChemRes}/sirane${prefix_nc}.${run}.nc            #sirane_*.nc
fic_sur_raster_nc=${ChemRes}/suremis${prefix_nc}.${run}.nc

if [ ${drop_sirane} -eq 0 ] && [ -f ${fic_sur_raster_nc} ] && [ ! -f ${fic_sirane_nc} ] ; then
  rm ${fic_sur_raster_nc}
fi

if [ -f ${fic_sirane_nc} ] ; then
do_dat=1       #..creation fichier de données input_$run.dat, de l'executable sirane et des fichiers d'entrees
do_press=0     #..calcul d'une pression par periode a partir d un fichier annuel
do_emis=0      #..calcul des émissions
do_run=0       #..activer les runs SIRANE
do_netcdf=0    #..conversion recepteurs ASCII SIRANE -> NetCDF
do_suremis=1   #..conversion recepteurs ASCII SIRANE -> NetCDF
fi

if [ -f ${fic_sur_raster_nc} ] ; then
do_suremis=0   #..conversion recepteurs ASCII SIRANE -> NetCDF
fi

echo "do_dat=${do_dat}"
echo "do_press=${do_press}"
echo "do_emis=${do_emis}"
echo "do_run=${do_run}"
echo "do_clean=${do_clean}"
echo "do_netcdf=${do_netcdf}"
echo "do_suremis=${do_suremis}"

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
#echo "*** info: Liste des iterations : ${nrs} ->  ${nre} (niter=${niter})"

if [ ${do_dat} -eq 1 ] ; then

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

vsirane=SRC${version_num}${version_src}

# chemins des fichiers appli
SiraneSRC=${cartoprox}/sources/${vsirane}

################# creation repertoire temporaire ##########
SiraneTMP=${cartoprox_scratch}/tmp_${cartoprox_domaine}_${periode}_${code_domaine}_pol${polluant} #.$$
mkdir -p ${SiraneTMP}
if [ ! -d ${SiraneTMP} ] ; then echo "*** CANNOT MOVE TO DIR. ${SiraneTMP}" ; exit 1 ; fi

# [dans cartoprox.inc]
#polluant=0  #..0 = polluant passif sans reaction chimique
             #..1 = melange NO/NO2, les emissions sont données en NOX equivalent NO2
	     #..2 = particules de meme diametre et de meme masse volumique

#..taux NO2/NOx
# [dans cartoprox.inc] tauxno2=0.15

#... parametres sur la mesure (inutiles avec meteo DIAGNOSTIQUE)
# [dans cartoprox.inc] altvent=10.0
# [dans cartoprox.inc] rugmesure=0.1
# [dans cartoprox.inc] depmesure=1

#... parametres du quartier
# Latitude=${latc}
# [dans cartoprox.inc] Rug_quartier=0.9
# [dans cartoprox.inc] Epaisseur_deplacement=13.0
# [dans cartoprox.inc] Hmoyenne=20.0
# [dans cartoprox.inc] Rugo_bati=0.05

for fic in ${fic_sirane_nc} ; do
  if [ -f ${fic} ] ; then
    echo -e "$VERT" "${fic} OK" "$NORMAL" 
  else
    echo -e "$ROUGE" "${fic} INDISPONIBLE" "$NORMAL" 
  fi
done

# Fichier MIF du reseau SIRANE sur l'ensemble du domaine code_domaine
if [ "${ficmif}" == "" ] ; then
  echo "***ERREUR : Indiquer export ficmif=fichier MIF avec reseau SIRANE"
  exit
fi

# mini-domaine/full-domaine
case ${domaine} in
full)
syntaxe_mailleur="-optimal -i ${ficmif} -o ${ficrecept_mailleur} -brin_dx ${brin_dx} -recept_dx ${recept_dx} -cadre_dx ${cadre_dx}";;
mini)
syntaxe_mailleur="-optimal -i ${ficmif} -o ${ficrecept_mailleur} -brin_dx ${brin_dx} -recept_dx ${recept_dx} -utm 31 -xc ${xc} -yc ${yc} -dx ${dx} -cadre_dx ${cadre_dx}";;
esac

#supprime le maillage au lancement
#rm -rf ${sirane_recept_user}/${recept_user}

####################### GEOM ##########################
ficnoeuds=${sirane_geom}/${cartoprox_domaine}/${code_domaine}/noeuds.txt
ficrues=${sirane_geom}/${cartoprox_domaine}/${code_domaine}/rues.txt
ficponct=${sirane_geom}/${cartoprox_domaine}/${code_domaine}/ponct.txt

# fichier de récepteurs (pour VALIDATION)
ficrecept=${sirane_recept_user}/${code_domaine}_sites.dat
# ficrecept=aucun
# fichier des stats
ficstat=${sirane_input}/stat_polluant_${polluant}.dat

####################### EMISSIONS ##########################
# pour info : dan cartoprox.inc
# ChemRes=${ChemRes1}/${code_domaine}
# fichier EMIS-* (cartoprox.inc)
ficemisnoeuds=${sirane_geom}/${cartoprox_domaine}/${code_domaine}/emis_${vcadastre}_noeuds_${polluant}.txt
#ficemisrues=${sirane_geom}/${cartoprox_domaine}/${code_domaine}/emis_${vcadastre}_rues_${polluant}.txt
ficemisponct=${sirane_geom}/${cartoprox_domaine}/${code_domaine}/emis_${vcadastre}_ponct_${polluant}.txt

####################### PREVALP ##########################
ficout_dir=${sirane_prevalp}/${cartoprox_domaine}/${periode}/${code_domaine}
# fichier meteo SOL (Lit les dates dans ce fichier)
ficmeteo=${ficout_dir}/meteo_${periode}_prevalp.dat
# fichier pression annuel input
ficpress=${ficout_dir}/pression_${periode}_prevalp.dat
# fichier fond_periode.dat
ficfond=${ficout_dir}/fond_${periode}_prevalp.dat
# fichier EVOL-METEO (si disponible)
ficevolmeteo=${ficout_dir}/evol-meteo_${periode}_prevalp.dat

####################### EVOL EMIS ##########################

# fichier EVOL-EMIS-* (generes a partir de EVOL-EMIS-*-STD)
ficevolrues=${ChemRes}/evol-emis-rues_pol${polluant}.dat
ficevolnoeuds=${ChemRes}/evol-emis-noeuds_pol${polluant}.dat
ficevolponct=${ChemRes}/evol-emis-ponct_pol${polluant}.dat

# fichier EVOL-EMIS-*-STD
#ficevolrues_std=${sirane_input}/evol-emis-rues_${periode}.dat
#ficevolnoeuds_std=${sirane_input}/evol-emis-noeuds_${periode}.dat
#ficevolponct_std=${sirane_input}/evol-emis-ponct_${periode}.dat

# Profils temporels par code_domaine
#ficevolrues_domaine=${sirane_evol}/evol-emis-rues_${code_domaine}.txt

# creer repertoire sorties
mkdir -p ${ChemRes}

fi

if [ ${do_emis} -eq 1 ] ; then
####################### Genere les EMISSIONS de la simulation #####################

if [ "${emis_type}" == "" ] ; then
  emis_type=annuelle
fi

case ${polluant} in
0|2|3)nom_polluant_emis=${var_pm};; # passif PM10
1)nom_polluant_emis=NOx;; # reactif NOx
esac

case ${emis_type} in
##################################################################################
annuelle)
##################################################################################
#creation des emis annuelles
echo ${ficemisruesrc_AN}
if [ ! -f ${ficemisruesrc_AN} ] ; then
echo -e "$ROUGE" "Fichier emissions annuelles: ${ficemisruesrc_AN}" "$NORMAL" 
exit 1
fi
#mkdir -p ${ChemRes}/emis_AN
#ficemisrues=${ChemRes}/emis_AN/emis-rues_${nom_polluant_emis}_${vcadastre}.txt
mkdir -p ${sirane_emis}/${cartoprox_domaine}/emis_AN/${code_domaine}
ficemisrues=${sirane_emis}/${cartoprox_domaine}/emis_AN/${code_domaine}/emis-rues_${nom_polluant_emis}_${vcadastre}.txt

syntaxe_emis="./utils/emisrues_minidomaine.sh ${nom_polluant_emis} ${code_domaine} ${ficrues} ${ficemisruesrc_AN} ${ficemisrues}"
${syntaxe_emis}

if [ -f ${ficemisponctsrc_AN} ] ; then
  echo "Genere emissions ponctuelles"
  syntaxe_emis="./utils/emisrues_minidomaine.sh ${nom_polluant_emis} ${code_domaine} ${ficponct} ${ficemisponctsrc_AN} ${ficemisponct}"
  ${syntaxe_emis}
fi
 
#profil temporels
ficevolrues_domaine_annee=${sirane_evol}/evol-emis-rues_profil${emis_profil}_${annee}.txt
ficevolrues_domaine=${ChemRes}/evol-emis-rues_profil${emis_profil}_${periode}.txt
if [ ! -f ${ficevolrues_domaine_annee} ] ; then
  ficevolrues_domaine_annee=${sirane_evol}/evol-emis-rues_DEFAUT_${annee}.txt
  echo "***ATTENTION: profils par DEFAUT : ${ficevolrues_domaine_annee}"
fi
if [ -f ${ficevolrues_domaine_annee} ] ; then
  echo "***profils temporels ANNUELS : ${ficevolrues_domaine_annee}"
  gawk '(NR >= '${deb_julh}' && NR <= '${fin_julh}'){print}' ${ficevolrues_domaine_annee} > ${ficevolrues_domaine}
else
  echo "***ERREUR: pas de profils temporels ANNUELS : ${ficevolrues_domaine_annee}"
  exit 1  
fi 
echo "*** export des profils temporels heures : ${deb_julh} ->  ${fin_julh} (niter=${niter})"

if [ -f ${ficevolrues_domaine} ] ; then
  echo "***info: utilise profils temporels : ${ficevolrues_domaine} pour RUES, NOEUDS et PONCTUELLES"
  #sleep 2
  ficevolrues_std=${ficevolrues_domaine}
  ficevolnoeuds_std=${ficevolrues_domaine}
  ficevolponct_std=${ficevolrues_domaine}
else
  echo "***ERREUR: pas de profils temporels : ${ficevolrues_domaine}"
  echo "***info: utilise profils standards ${ficevolrues_std}"
  exit 1
fi

# Evol-EMIS
ficemisrues_tmp=`gawk '!($4 in a){a[$4];print $4}'  ${ficevolrues_std}` #v1.16
ficemisnoeuds_tmp=`gawk '!($4 in a){a[$4];print $4}' ${ficevolnoeuds_std}`
ficemisponct_tmp=`gawk '!($4 in a){a[$4];print $4}'  ${ficevolponct_std}`

echo "Remplace ${ficemisrues_tmp} ${ficemisnoeuds_tmp} ${ficemisponct_tmp} dans ${ficevolrues_std}"
sed -e "s#${ficemisrues_tmp}#${ficemisrues}#g"     ${ficevolrues_std}   > ${ficevolrues}
sed -e "s#${ficemisnoeuds_tmp}#${ficemisnoeuds}#g" ${ficevolnoeuds_std} > ${ficevolnoeuds}
sed -e "s#${ficemisponct_tmp}#${ficemisponct}#g"   ${ficevolponct_std}  > ${ficevolponct}
;;#Fin emissions annuelles
##################################################################################
horaire) #A7 Cartoprox V3 (08/2011)
##################################################################################
#Calcul des iterations debut et fin
deb_a=`date -d "${deb_j}" +%Y`
deb_julh=`./utils/date2iter.sh ${deb_j} ${deb_h}`
fin_julh=`./utils/date2iter.sh ${fin_j} ${fin_h}`
echo "deb_julh=${deb_julh} fin_julh=${fin_julh}"
genere_fic=1 #Regenere fichier ? O/N
if [ -f ${ficevolrues} ] ; then # seulement s'il n'existe pas et nombre d'iterations incorrect
  nlignes_fic=`cat ${ficevolrues} | wc -l`
  if [ ${nlignes_fic} -eq ${niter} ] ; then
    genere_fic=0
  fi
fi
if [ ${genere_fic} -eq 1 ] ; then
echo "Genere: ${ficevolrues}"
echo "Genere: ${ficevolnoeuds}"
echo "Genere: ${ficevolponct}"
rm -f ${ficevolrues} ${ficevolnoeuds} ${ficevolponct}
touch ${ficevolrues} ${ficevolnoeuds} ${ficevolponct}

#Genere un  fichier evol-emis avec le nom des fichiers d'emissions
#RAPPEL: format de evol-emis
#01/01/2010 00:00 0.188282 EMIS/emis_rue.txt

repemisrues_H=${sirane_emis}/${cartoprox_domaine}/emis_H/${code_domaine}/${nom_polluant_emis}/${mois_emis}
#repemisrues_H=${sirane_emis}/${cartoprox_domaine}/emis_H_valence_ssE3/${code_domaine}/${nom_polluant_emis}/${mois_emis}
#repemisrues_H=${sirane_emis}/${cartoprox_domaine}/emis_H_test_sansA7/${code_domaine}/${nom_polluant_emis}/${mois_emis}
#repemisrues_H=${sirane_emis}/${cartoprox_domaine}/emis_H_test_sansPL/${code_domaine}/${nom_polluant_emis}/${mois_emis}
#repemisrues_H=${sirane_emis}/${cartoprox_domaine}/emis_H_test_sansE3/${code_domaine}/${nom_polluant_emis}/${mois_emis}
echo "Fichiers emissions horaires des RUES -> ${repemisrues_H}"
echo "Repertoire des emissions ${repemisruesrc_H}"
#Boucle sur les iteration
iter=${deb_julh}
iter_step=`echo ${fin_julh} | gawk '{print int($1/100) }'` # 1 step = 1 %
while [ ${iter} -le ${fin_julh} ] ; do #Boucle sur les iterations de la simulation (emissions horaires)
dateSIRANE=`./utils/iter2date.sh ${iter} ${deb_a} std`
dateYYYYMMDDHH=`./utils/iter2date.sh ${iter} ${deb_a} yyyymmddhh`
dateYYYYMMDD=`./utils/iter2date.sh ${iter} ${deb_a} yyyymmdd`
ficemisruesrc_H=${repemisruesrc_H}/emis-rues-${dateYYYYMMDDHH}.txt #repemisruesrc_H fixe dans domaine_*.inc
#ficemisruesrc_H=${repemisruesrc_H}/emis-rues-${dateYYYYMMDDHH}_sansA7.txt #repemisruesrc_H fixe dans domaine_*.inc  
#emis_rues_txt=${ChemRes}/emis_H/${nom_polluant_emis}/emis-rues-${dateYYYYMMDDHH}_${nom_polluant_emis}.txt
# classement par mois
mois_emis=`date -d "${dateYYYYMMDD}" +%m`
#mkdir -p ${repemisrues_H}
ficemisrues=${repemisrues_H}/emis-rues-${dateYYYYMMDDHH}_${nom_polluant_emis}.txt

#Test existence du fichier d'emissions horaires du domaine 
#if [ ! -f ${ficemisruesrc_H} ] ; then
#echo -e "$ROUGE" "Manquant: ${ficemisruesrc_H}" "$NORMAL" 
#echo "Verifier les fichiers dans le chemin repemisruesrc_H=${repemisruesrc_H} ou desactiver la fonction emis_type=horaire"
#exit 1
#fi
#Le fichier existe ? nombres de lignes correct ?
#ficemisrues_ok=0
#if [ -f ${ficemisrues} ] ; then
#  nrues_norm=`head -n 1 ${ficemisrues} | awk '(NR == 1){print}' | sed -e "s/ //g"` #nb de lignes theoriques    
#  if [ "${nrues_norm}" == "" ] ; then
#    nrues_norm=9999
#  fi
#  nrues_fic=`cat ${ficemisrues} | awk '(NR > 1){print}' | wc -l` #nb de rues du fichier
#  if [ ${nrues_fic} -eq ${nrues_norm} ] ; then
#    ficemisrues_ok=1
  #else
  #  echo -n ${nrues_fic}/${nrues_norm}
#  fi
#fi
#
#ficemisrues_ok=1 
#echo -n "ficemisrues_ok=${ficemisrues_ok}"
#Fichier inexistant ou incomplet : genere le fichier d'emissions horaires pour le mini-domaine
#if [ ${ficemisrues_ok} -eq 0 ] ; then
#creation des emis horaires
#syntaxe_emis="./utils/emisrues_minidomaine.sh ${nom_polluant_emis} ${code_domaine} ${ficrues} ${ficemisruesrc_H} ${ficemisrues}"
#${syntaxe_emis} > /dev/null || { echo "$0: ERREUR de ${syntaxe_emis}" ; exit 1 ; }
#echo ${syntaxe_emis}
#echo "--> ${ficemisrues}"
#fi
#Controle le fichier d'emissions horaires pour le mini-domaine
#if [ ! -f ${ficemisrues} ] ; then
#echo -e "$ROUGE" "Manquant: ${ficemisrues}" "$NORMAL" 
#exit 1
#else
#echo -e "$VERT" "Trouve: ${emis_rues_txt}" "$NORMAL" 
echo "${dateSIRANE};1.00;${ficemisrues}"   | sed -e "s/;/\t/g" >> ${ficevolrues}
echo "${dateSIRANE};0.00;${ficemisnoeuds}" | sed -e "s/;/\t/g" >> ${ficevolnoeuds}
echo "${dateSIRANE};0.00;${ficemisponct}"  | sed -e "s/;/\t/g" >> ${ficevolponct}
#fi
#Affichage de l'avancement du traitement heure par heure
if [ `echo ${iter} | gawk '{print ($1 - 1) % '${iter_step}'}'` -eq 0 ] ; then
  echo -n -e "\r" "`echo ${iter} | gawk '{print int($1 / '${fin_julh}' *100)/1 }'` % (${iter}/${fin_julh} )"
fi
iter=`expr ${iter} \+ 1`
done #Boucle sur les iterations de la simulation (emissions horaires)
  echo -n "Generation des fichiers evol-emis-* horaires..."
  echo -e "$VERT" "OK" "$NORMAL" 
else #SKIP la preparation
  echo -n "Fichiers evol-emis-* horaires..."
  echo -e "$VERT" "OK" "$NORMAL" 
fi
echo -e "$BLEU" "Trouve ${ficevolrues} (${niter} iterations)" "$NORMAL" 
head -n 1 ${ficevolrues}
echo -e "$BLEU" "Trouve ${ficevolnoeuds} (${niter} iterations)" "$NORMAL" 
head -n 1 ${ficevolnoeuds}
echo -e "$BLEU" "Trouve ${ficevolponct} (${niter} iterations)" "$NORMAL" 
head -n 1 ${ficevolponct}

;;
esac
#Fin emissions
fi

####################### COMMENCE TRAITEMENTS #####################
if [ -f ${ficrecept} ] && [ "${recept_user}" == "yes"  ] ; then
Classout=0 # version 2 : traitement identique pour tous les recepteurs
echo "***info: utilise fichier recepteurs ${ficrecept}"
else 
Classout=0
echo "***info: utilise MAILLEUR OPTIMAL"
fi

# recepteurs USER si Classout=1 sinon fichier recepteurs MAILLEUR
case ${Classout} in 
0)
# regles speciales si pas de recepteurs USER --> MAILLEUR
calcul_stat=0   #..calcul des stats 0/1
do_netcdf=1  #..conversion NetCDF
;;
1)
# regles speciales si recepteurs USER (n'utilise plus cette fonction en V2)
# traite les recepteurs USER comme les recepteurs du MAILLEUR
calcul_stat=1   #..calcul des stats 0/1
do_netcdf=0  #..conversion NetCDF
;;
esac
##### fin d'intervention utilisateur

#############################################################################
#VERIFICATION DES FICHIERS EN ENTREES
#############################################################################
if [ ${do_dat} -eq 1 ] ; then
echo "======== Verification des fichiers en input"

ficliste="${ficmeteo} ${ficrues} ${ficnoeuds} ${ficponct} ${ficstat} ${ficpress} \
${ficevolrues} ${ficevolnoeuds} ${ficevolponct} ${ficfond} \
${ficemisrues} ${ficemisnoeuds} ${ficemisponct}"

# Ajoute le fichier recepteur USER si 
if [ "${recept_user}" == "yes" ] ; then
ficliste="${ficliste} ${ficrecept}"
fi

error=0

for fic in ${ficliste} ; do
if [ ! -f ${fic} ] ; then
echo "*** ERREUR: ne trouve pas ${fic}"
error=1
else
echo "*** info: trouve inputs ${fic}"
fi
done

# diag/mto
if [ ! -f ${ficevolmeteo} ] ; then
echo "*** ERREUR: ne trouve pas ${ficevolmeteo} -> passe en mode meteo STATION"
version_src=mto
vsirane=SRC${version_num}${version_src}
SiraneSRC=/appli/SIRANE/sources/${vsirane}
else
echo "*** info: trouve inputs ${ficevolmeteo} -> utilise mode meteo REGIONALE DIAGNOSTIQUE "
fi

if [ ${error} -eq 1 ] ; then
echo "*** info: Une ou plusieurs detectees: verifier vos fichiers en input de SIRANE"
exit
else
echo "*** info: Pas d erreur detectee: fichiers en input de SIRANE corrects"
fi

fi #fin de verification
#############################################################################
# GRILLE SIRANE 
#############################################################################

# fichier de parametres de grille SIRANE (contient les X/Y)
ficgrille=${ChemRes}/grille_${cartoprox_domaine}_${code_domaine}.inc
echo "ficgrille=${ficgrille}"
echo "xc="${xc}
echo "yc="${yc}

# converti les coordonnees UTM 31 -> Lambert 2 Etendu
${mailleur_exe} ${syntaxe_mailleur} -g ${ficgrille}

Xout=225
Yout=225

#Lambert
Xmin=`gawk '($1 == "xmin") {print $2}' ${ficgrille}` 
Xmax=`gawk '($1 == "xmax") {print $2}' ${ficgrille}` 
Ymin=`gawk '($1 == "ymin") {print $2}' ${ficgrille}` 
Ymax=`gawk '($1 == "ymax") {print $2}' ${ficgrille}` 

#...maillage de calcul

Xcmin=${Xmin}
Xcmax=${Xmax}
Ycmin=${Ymin}
Ycmax=${Ymax}

echo "Ecrit la grille dans ${ficgrille}"
echo "Xcmin=${Xmin}"
echo "Xcmax=${Xmax}"
echo "Ycmin=${Ymin}"
echo "Ycmax=${Ymax}"

#..Autres parametres de grille
Xcnpt=1
Ycnpt=1
#...Ratio d'interpolation de la grille de sortie
Rinterp=1
#...Nombre de cellules d'influence du maillage de calcul :
Ncinf=1

#############################################################################
#DOMAINE 
#############################################################################

# defini parametres grille/pas grille
CALCSTAT=0

# creer le repertoire de resultats
echo "*** info: Creer ${ChemRes}" 
mkdir -p ${ChemRes}

#run=${deb_j}_${fin_j}
REPRES=resultats_${run}_pol${polluant}
REPSURF=SURFER_${run}

cd ${localdir}

echo "======== debut et fin de run : "
echo "nrs=${nrs}"
echo "nre=${nre}"
echo "run=${run}"

######################################
# Preparation du run
######################################
if [ ${do_dat} -eq 1 ] ; then

# defini parametres grille/pas grille
case ${calcul_grille} in
0)repertoire_liste="${REPRES}";;
1)repertoire_liste="${REPRES} ${REPSURF}";;
esac

# creer les repertoires de sorties et temporaires
for rep in ${repertoire_liste} ; do
 if [ -d  ${ChemRes}/${rep} ] ; then    
    echo "*** ERREUR: Repertoire sorties existe : ${ChemRes}/${rep}"    
    echo "*** info: Supprime ${ChemRes}/${rep} dans 3 secondes..."
    sleep 3
    rm -rf ${ChemRes}/${rep}
 fi
done #rep

########################## MAILLEUR OPTIMAL -> GENERATION DU MAILLAGE ##########################
# dans prepMAILLAGE

####################### PRESSION ##########################
if test ${do_press} -eq 1 ;  then
  echo "======== calcul de la pression moyenne "
  Pmoy=`${localdir}/utils/calcPmoy.sh ${ficpress} ${nrs} ${nre} `
  echo "*** info: Pmoy = "${Pmoy}
  if test "${Pmoy}" == "" ; then
    echo "PB PMOY VIDE"
    exit
  fi
fi #press

####################### RECEPTEURS ##########################
ficrecept_tmp=${SiraneTMP}/recept.tmp 
ficrecept_dom=${SiraneTMP}/recept.dom
recept_tmp=${SiraneTMP}/recepteurs.user  

if [ ! -f ${ficrecept_mailleur} ] && [ "${recept_user}" == "yes" ] ; then
  echo "Fichier introuvable: ${ficrecept_mailleur}"
  exit 1
fi

echo "recept_user=${recept_user}"
echo "ficrecept_mailleur=${ficrecept_mailleur}"

if [ -f ${ficrecept_mailleur} ] && [ "${recept_user}" == "no" ] ; then
echo "======== Utilise recepteurs MAILLEUR OPTIMAL ${ficrecept_mailleur}"
  #compte les recepteurs dans fichier USER
  gawk '{print "pt_"$1" "$2" "$3}' ${ficrecept_mailleur} >  ${ficrecept_tmp}
  # additionne les brins de route (desactif)
  if [ -f ${ficbrin_mailleur} ] ; then
    echo "*** info: Utilise fichier brins "${ficbrin_mailleur}
    gawk '{print "br_"$1" "$2" "$3}' ${ficbrin_mailleur}   >> ${ficrecept_tmp}
  else
    echo "*** warning: N'utilise aucun fichier de brins"
  fi
  echo "*** info: Nombre recepteurs dans fichier USER="`wc -l  ${ficrecept_tmp} | gawk '{print $1}'`  
  #Applique le masque par agglo (seulement Lyon au 15/09/2010)
  cd ${localdir}
  echo "*** info: je suis dans ${localdir}"
  
  ####################### MASQUE AGGLOS (CARTO 2009) ################################  
  ./utils/applique_masque_agglo.sh  ${periode} ${cartoprox_domaine} ${code_domaine} > ${ficrecept_tmp}        
else
  echo "======== Utilise recepteurs USER ${ficrecept}"
  gawk '( NR != 1) {print}' ${ficrecept} > ${ficrecept_tmp}
fi  

#compte les recepteurs dans fichier USER
nrecept=`wc -l  ${ficrecept_tmp} | gawk '{print $1}'`
echo "*** info: Nombre recepteurs dans fichier MASQUE="${nrecept}

# S'assure que les recepteurs sont dans le domaine SIRANE
gawk '( $2 >= '${Xcmin}' && $2 <= '${Xcmax}'  && $3 >= '${Ycmin}' && $3 <= '${Ycmax}'  ) {print $1" "$2" "$3}' ${ficrecept_tmp} > ${ficrecept_dom}
rm ${ficrecept_tmp}

#compte les recepteurs dans domaine SIRANE et ecrit un fichier de recepteurs
nrecept_dom=`wc -l  ${ficrecept_dom} | gawk '{print $1}'`
echo "*** info: Nombre recepteurs dans domaine SIRANE="${nrecept_dom}
if [ ${nrecept} -gt ${nrecept_max} ] ; then
  echo "*** warning: Nombre recepteurs > nrecept_max=${nrecept_max}"
  nrecept=${nrecept_max}
fi

if [ ${nrecept_dom} -lt  ${nrecept} ] ; then
  if [ "${recept_user}" == "yes" ] ; then
    echo "***ERREUR: des recepteurs sont hors domaine SIRANE"
    echo "-> ${ficrecept_tmp}"
    exit 1
  else
    echo "***info: des recepteurs sont hors domaine SIRANE"    
  fi
fi
if [ ${nrecept_dom} -eq  0 ] ; then
  echo "***ERREUR: aucun recepteur dans le domaine - SKIP CALCUL SIRANE"
  echo "-> ${ficrecept_tmp}"
  exit 0 
fi

echo ${nrecept_dom} > ${recept_tmp}
gawk '{print $1"\t"$2"\t"$3}' ${ficrecept_dom} >> ${recept_tmp}
rm ${ficrecept_dom}   
echo "*** info: recepteurs ${recept_tmp}"
ficrecept=${recept_tmp}

####################### ${SiraneTMP} ##########################
cd ${SiraneTMP}
echo "*** info: je suis dans ${SiraneTMP}"

####################### FICHIER INPUT ##########################
nech=`expr ${nre} - ${nrs}`
echo "*** info: nombre d echeances calculees : "`expr ${nech} + 1`

case ${polluant} in
3)polluant_sirane=2;; #PM
*)polluant_sirane=${polluant};;
esac

# Parametres definis dans infos_domaine.sh
sed -e "s/YEAR/${annee}/g" \
    -e "s/RUN/${run}/g" \
    -e "s/_NTOTAL_/${niter}/g" \
    -e "s/_NSTART_/${nrs}/g" \
    -e "s/_NSTOP_/${nre}/g" \
    -e "s/_ALTMESURE_/${altvent}/g"  \
    -e "s/_RUGMESURE_/${rugmesure}/g"  \
    -e "s/_DEPMESURE_/${depmesure}/g"  \
    -e "s/_CALCSTAT_/${calcul_stat}/g" \
    -e "s/_CALCGRILLE_/${calcul_grille}/g" \
    -e "s/output/${output}/g" \
    -e "s/Classout/${Classout}/g" \
    -e "s/Rinterp/${Rinterp}/g" \
    -e "s/Xout/${Xout}/g" \
    -e "s/Yout/${Yout}/g" \
    -e "s/Xmin/${Xmin}/g" \
    -e "s/Xmax/${Xmax}/g" \
    -e "s/Ymin/${Ymin}/g" \
    -e "s/Ymax/${Ymax}/g" \
    -e "s/Xcnpt/${Xcnpt}/g" \
    -e "s/Ycnpt/${Ycnpt}/g" \
    -e "s/Xcmin/${Xcmin}/g" \
    -e "s/Xcmax/${Xcmax}/g" \
    -e "s/Ycmin/${Ycmin}/g" \
    -e "s/Ycmax/${Ycmax}/g" \
    -e "s/Ncinf/${Ncinf}/g" \
    -e "s/_PRESREF_/${Pmoy}/g" \
    -e "s/_REPRES_/${REPRES}/g" \
    -e "s/_REPSURF_/${REPSURF}/g" \
    -e "s/_LATITUDE_/${Latitude}/g" \
    -e "s/_RUGQART_/${Rug_quartier}/g" \
    -e "s/_DEPLA_/${Epaisseur_deplacement}/g" \
    -e "s/_HMOY_/${Hmoyenne}/g" \
    -e "s/_RUGBATI_/${Rugo_bati}/g" \
    -e "s/_POLLUANT_/${polluant_sirane}/g" \
    -e "s/_TAUXNO2_/${tauxno2}/g" \
    -e "s/_pm_diametre_/${pm_diametre}/g" \
    -e "s/_pm_massvol_/${pm_massvol}/g" \
    -e "s/_pm_lessiva_/${pm_lessiva}/g" \
    -e "s/_pm_lessivb_/${pm_lessivb}/g" \
    ${input_sed} > ${SiraneTMP}/input_${run}.dat

echo "*** info: fichier cree : "${SiraneTMP}/input_${run}.dat

cp ${SiraneTMP}/input_${run}.dat ${ChemRes}/input_${run}.dat 
#
# Copie toutes les sources dans le repertoire temporaire
if [ -d ${SiraneSRC} ] ; then
  cp -r ${SiraneSRC}/* ${SiraneTMP}
else
  echo "*** ERREUR: sources absentes ${SiraneSRC}"
fi

####################### Compilation ##########################
#if [ ! -f sirane.exe ] ; then
rm -f sirane.exe *.o
echo "*** info: Compilation of all programs..."
sed -e "s/_EXECTMP_/sirane.exe/g" ${SiraneTMP}/Makefile.tmp > ${SiraneTMP}/Makefile
make clean >  ${SiraneTMP}/make.log 2>&1 # menage des sources
make linux >> ${SiraneTMP}/make.log 2>&1 # compilation 
#fi

# Creer repertoires dans le repertoire temporaire
mkdir  -p ${SiraneTMP}/INPUT
mkdir  -p ${SiraneTMP}/GEOM

cd ${SiraneTMP}

####################### Copie des fichiers temporaires ##########################

# Liste les entrees et supprime les fichiers existants
liste_fichiers="GEOM/rues.txt GEOM/noeuds.txt GEOM/sources-ponct.dat GEOM/Recepteurs.dat GEOM/Stat-Chimie.dat \
INPUT/meteo.dat INPUT/evol-emis-rues.dat INPUT/evol-emis-noeuds.dat INPUT/evol-emis-ponct.dat INPUT/fond.dat INPUT/evol-meteo.dat"
for fic in ${liste_fichiers} ; do
if [ -f ${fic} ] ; then
  rm ${fic}
fi
done

echo "Copie les entrees..."
# Copie les fichiers d'entres
cp ${ficrues}       ${SiraneTMP}/GEOM/rues.txt
cp ${ficnoeuds}     ${SiraneTMP}/GEOM/noeuds.txt
cp ${ficponct}      ${SiraneTMP}/GEOM/sources-ponct.dat
cp ${ficrecept}     ${SiraneTMP}/GEOM/Recepteurs.dat
cp ${ficstat}       ${SiraneTMP}/GEOM/Stat-Chimie.dat

cp ${ficmeteo}      ${SiraneTMP}/INPUT/meteo.dat
cp ${ficevolrues}   ${SiraneTMP}/INPUT/evol-emis-rues.dat
cp ${ficevolnoeuds} ${SiraneTMP}/INPUT/evol-emis-noeuds.dat
cp ${ficevolponct}  ${SiraneTMP}/INPUT/evol-emis-ponct.dat
#cp $ficfond        INPUT/fond.dat

fond_pm_defaut=20. #microg/m3 c'est le fond par defaut de SIRANE en PM10
case ${polluant} in
1)gawk '{ print $1" "$2"\t"$3"\t"$4"\t"$5 }' ${ficfond} > ${SiraneTMP}/INPUT/fond.dat;;
0|2|3)gawk '{print $1" "$2"\t'${fond_pm_defaut}'"}' ${ficfond} > ${SiraneTMP}/INPUT/fond.dat ;;
esac

# determine un facteur de correction de la vitesse du vent
case ${version_src} in
diag)
var_w10m="fac_w10m"
if [ -f ${meteo_fin_fic} ] ; then
if [ ! -f ${ficevolmeteo}.${var_w10m} ] ; then
  echo "Point d'extraction: x=${xb} y=${yb} var=${var_w10m}"
  syntaxe_fin="${extract_val_grille_exe} \
    -xmin ${met_fin_xmin} -ymin ${met_fin_ymin} -dx ${met_fin_dx} -x ${xb} -y ${yb} -hour -var ${var_w10m} -smooth 3 -i ${meteo_fin_fic}"
  echo "syntaxe_fin=${syntaxe_fin}"
  echo "Extrait la variable ${var_w10m} de ${meteo_fin_fic} ..."
  ${syntaxe_fin} | gawk '{print $1"\t"$2}' >  INPUT/evol-fac_w10m.dat 
  echo "Facteur sur w10m(fin)=`gawk 'BEGIN{moy=0.;n=0.};($2>0.){moy=moy+$2;n=n+1.};END{print moy/n}' INPUT/evol-fac_w10m.dat`" 
  cp INPUT/evol-fac_w10m.dat ${ficevolmeteo}.${var_w10m}
  echo "Copie -> ${ficevolmeteo}.${var_w10m}"  
else
  echo "Utilise le fichier avec la vitesse du vent corrigee: ${ficevolmeteo}.${var_w10m}"
fi
fi
if [ -f ${ficevolmeteo}.${var_w10m} ] ; then
  # remplace la vitesse du vent par la valeur calculée -> applique aussi changement a U*
  fac_w10m_moy=`gawk 'BEGIN{moy=0.;n=0.};($2>0.){moy=moy+$2;n=n+1.};END{print moy/n}' ${ficevolmeteo}.${var_w10m}`
  gawk '{print $2}' ${ficevolmeteo}.${var_w10m} > INPUT/evol-fac_w10m.dat  
  echo "Facteur correctif W10M moyen=${fac_w10m_moy}"
  echo paste  INPUT/evol-fac_w10m.dat ${ficevolmeteo}
  paste  INPUT/evol-fac_w10m.dat ${ficevolmeteo} | \
  gawk '{print $2"\t"$3"\t"$4"\t"$5*$1"\t"$6"\t"$7*$1"\t"$8"\t"$9"\t"$10*$1"\t"$11"\t"$12"\t"$13"\t"$14"\t"$15"\t"$16"\t"$17}' \
   > INPUT/evol-meteo_mef.dat   
else
  cp ${ficevolmeteo} INPUT/evol-meteo_mef.dat 
fi

# Verifie que la vitesse U* n'est pas inferieure a ${ustar_min} (defini dans cartoprox.inc)
if [ "${ustar_fac}" == "" ] ; then
ustar_fac=1.0
fi
echo "Facteur multiplicatif U* ustar_fac=${ustar_fac}"
gawk '{if ($9*'${ustar_fac}'<'${ustar_min}'){
print $1"\t"$2"\t"$3"\t"$4"\t"$5"\t"$6"\t"$7"\t"$8"\t'${ustar_min}'\t"$10"\t"$11"\t"$12"\t"$13"\t"$14"\t"$15"\t"$16
} 
else {
print $1"\t"$2"\t"$3"\t"$4"\t"$5"\t"$6"\t"$7"\t"$8"\t"$9*'${ustar_fac}'"\t"$10"\t"$11"\t"$12"\t"$13"\t"$14"\t"$15"\t"$16
}}' INPUT/evol-meteo_mef.dat > INPUT/evol-meteo.dat  

;;
esac
# determine un facteur de correction de la vitesse du vent

fi #do_dat

####################### Lance SIRANE ##########################
if test ${do_run} -eq 1 ; then
  echo "============= SIRANE ============="
# lance SIRANE
 echo "*** info: Lance SIRANE. log dans ${ChemRes}/sirane_${run}_pol${polluant}.log"  
time ${SiraneTMP}/sirane.exe  input_${run}.dat  \
     > ${ChemRes}/sirane_${run}_pol${polluant}.log 2>&1

# Copie des données entrées
cp ${ficrecept}                 ${SiraneTMP}/${REPRES}/Recepteurs.dat
cp ${SiraneTMP}/input_${run}.dat  ${SiraneTMP}/${REPRES}/.
#cp ${SiraneTMP}/sirane_${run}.log ${SiraneTMP}/${REPRES}/.

echo "*** info: je suis dans ${SiraneTMP}"

##########################################################################################
# POSTTRAITEMENT CDF
##########################################################################################
if [ ${do_netcdf} -eq 1 ] ; then
echo "*** info: ${run} : NetCDF `date`"
echo "-> Ecrit NetCDF ${ChemRes}/sirane${prefix_nc}.${run}.nc > ${ChemRes}/sirane2cdf_iteration_pol${polluant}.log"

#Repertoire post-traitement CDF
sirane2cdf=${cartoprox}/utils/recept2cdf
cd ${sirane2cdf}

#Syntaxe
syntaxe="./sirane2cdf_iteration.sh \
${periode} \
${SiraneTMP}/${REPRES} ${SiraneTMP}/${REPRES}/Recepteurs.dat \
${ChemRes}/sirane${prefix_nc}.${run}.nc \
${polluant} \
${SiraneTMP}/INPUT/fond.dat"

#Lance le programme
${syntaxe} > ${ChemRes}/sirane2cdf_iteration_pol${polluant}.log 2>&1 \
  || { echo "$0: ERREUR de l executable de conversion SIRANE->NetCDF" ; exit 1 ; }
 
#Section ou on gere un grand nombre de fichier : on utilise find ./${REPRES} -name 'Iteration-recept-*.*' -delete
if [ -f ${ChemRes}/sirane${prefix_nc}.${run}.nc ] ; then
  echo -e "$VERT" "-> Sorties NetCDF ${ChemRes}/sirane${prefix_nc}.${run}.nc OK" "$NORMAL" 
else 
  echo -e "$ROUGE" "***ERREUR: pas de sorties SIRANE en NetCDF" "$NORMAL" 
  exit 1
fi

#Supprime les fichiers Recepteurs
cd ${SiraneTMP}
echo "-> supprime ${SiraneTMP}/${REPRES}/Iteration-recept-*"  
find ./${REPRES} -name 'Iteration-recept-*.*' -delete

#Supprime les fichiers rue
cd ${SiraneTMP}
#echo "-> compresse les sorties dans ${ChemRes}/Iteration-rue.${run}.tgz"
#list=`find ./${REPRES} -name \*Iteration-rue-* -print`
#tar zcf ${ChemRes}/Iteration-rue.${run}.tgz ${list}
echo "-> supprime ${SiraneTMP}/${REPRES}/Iteration-rue-*"
find ./${REPRES} -name 'Iteration-rue-*.*' -delete

# deplace les fichiers de sorties ${SiraneTMP}/${rep} -> ${ChemRes}/${rep}
cd ${ChemRes}
for rep in ${repertoire_liste} ; do
  mv ${SiraneTMP}/${rep} ${ChemRes}/
  echo "*** info: Deplace ${SiraneTMP}/${rep} -> ${ChemRes}/${rep}"    
done #rep

do_suremis=1

fi #do_netcdf

###
fi #do_run

# Choix des polluants a extraire de PREVALP (ATTENTION : ne pas mettre uniquement UN espace entre les variables)
case ${polluant} in
0)varlist="TRACEUR";;
1)varlist="O3 NO NO2";;
2|3)varlist="${var_pm}";;
esac

##########################################################################################
# Calcule la SUREMISSION sur le maillage RASTER
##########################################################################################
if [ ${do_suremis} -eq 1 ] ; then

cd ${ChemRes}

echo -e "$BLEU" "Calcul la concentration SUREMISSIONS (do_suremis=${do_suremis})" "$NORMAL"

#Xmin, xmax de la grille
xmin_m=`gawk '($1 == "xmin1") {print int($2)}' ${ficgrille}` 
ymin_m=`gawk '($1 == "ymin1") {print int($2)}' ${ficgrille}` 
xmax_m=`gawk '($1 == "xmax1") {print int($2)}' ${ficgrille}` 
ymax_m=`gawk '($1 == "ymax1") {print int($2)}' ${ficgrille}`
sur_nx=`echo ${xmin_m} ${xmax_m} ${fond_dx} | awk '{print int(($2-$1)/$3) }'`
sur_ny=`echo ${ymin_m} ${ymax_m} ${fond_dx} | awk '{print int(($2-$1)/$3) }'`
sur_dx=${fond_dx} # 1 km ou 3 km

#Suremissions
echo "Calcul SUREMIS (cartoprox=sirane+grille-fond) en cours"
echo "xmin_m=${xmin_m}"
echo "ymin_m=${xmax_m}"
echo "xmax_m=${ymin_m}"
echo "ymax_m=${ymax_m}"
echo "sur_nx=${sur_nx}"
echo "sur_ny=${sur_ny}"
echo "sur_dx=${sur_dx}"

syntaxe="${calc_suremis_nc_exe} ${sur_nx} ${sur_ny} ${xmin_m} ${ymin_m} ${sur_dx} ${fic_sirane_nc} ${fic_sur_raster_nc}"
echo ${syntaxe}
${syntaxe}  || \
  { echo "$0: ERREUR du calcul SUREMIS:" ; echo ${syntaxe} ; exit 1 ; }

fi #do_suremis

##########################################################################################
# Clean -> supprime les fichiers temporaires
##########################################################################################
if [ ${do_clean} -eq 1 ] ; then
  echo "-> Supprime ${SiraneTMP} dans 3 secondes"
  sleep 3  
  rm -rf ${SiraneTMP}
fi #do_clean

##########################################################################################
# Verfifie les sorties NetCDF
##########################################################################################
for ifnout in ${fic_sirane_nc} ${fic_sur_raster_nc} ; do
ifnout_ok=0
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

if [ ${ifnout_ok} -eq 1 ] ; then # simul existe ?  
  echo -e "$0:" "$VERT" "SUCCESS" "$NORMAL"
else
  echo -e "$0:" "$ROUGE" "ERREUR de $0: pas de fichier de sortie ${ifnout} genere" "$NORMAL"
  rm -f ${ifnout}
  exit 1
fi
done

echo "*** info: ${run} : fin du traitement `date`"
