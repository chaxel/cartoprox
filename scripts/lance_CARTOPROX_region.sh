#!/bin/bash
#
# TOP-SCRIPT pour CARTOPROX
# * lance le calcul sur la region
# Atmo Rhone-Alpes 2010
#############################################
localdir=`pwd`
export cartoprox=`pwd | gawk -F '/scripts' '{print $1}'`
${cartoprox}/scripts/utils/header.sh
ulimit -s unlimited
# declaration de la periode de calcul 
if [ "${periode}" != "" ] ; then
  deb_j=`echo ${periode} | gawk -F "_" '{print $1}'`
  fin_j=`echo ${periode} | gawk -F "_" '{print $2}'`
  deb_h=00
  fin_h=23
else
  annee_defaut=2010
  deb_j=${annee_defaut}0101
  fin_j=${annee_defaut}1231
  deb_h=00
  fin_h=23
fi

#Entrees utilisateur
case $# in
1)
mode=$1
;;
3)
mode=$1
deb_j=$2
fin_j=$3
;;
5)
mode=$1
deb_j=$2
deb_h=$3
fin_j=$4
fin_h=$5
;;
*)
echo "Syntaxe $0 mode[grid,prep,emis,sirane_gaz,sirane_pm10,sirane_pm25,suremis,cartoprox_gaz,cartoprox_pm10,cartoprox_pm25,stat,zoom] deb_j (deb_h) fin_j (fin_h)"
#echo "Continue avec valeurs par defaut de cartoprox.inc dans 3 secondes..."
exit
;;
esac

## Polluant ###########################################
case ${mode} in
*gaz)export polluant=1;;
*aer|*pm10)export polluant=2;;
*pm25)export polluant=3;;
*)export polluant=1;;
esac

## PERIODE ###########################################
periode=${deb_j}_${fin_j}

## INCLUDE ###########################################
source ${cartoprox}/cartoprox.inc ${periode} || \
  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; } 

#mode=run # grid,prep,run,stat,emis
echo "mode="${mode}
do_grid=0
do_reg=0
do_emis=0
do_sirane=0
do_suremis=0
do_cartoprox=0
do_stat=0
do_zoom=0

case ${mode} in
prep)
do_grid=1
do_reg=1
;;
grid)
do_grid=1
;;
emis)
do_emis=1
;;
sirane_*)
do_sirane=1
;;
suremis)
do_suremis=1
;;
cartoprox_*)
do_cartoprox=1
;;
stat)
do_stat=1
;;
zoom)
do_zoom=1
;;
*)
echo "Choisir parmi: prep grid emis sirane_gaz sirane_pm10 sirane_pm25 suremis cartoprox_gaz cartoprox_pm10 cartoprox_pm25 stat zoom"
echo "MODE INCONNU: ${mode} - STOP"
exit 1
;;
esac

############## Transformation dates -> iterations ############
./utils/check_dates.sh ${deb_j} ${deb_h} ${fin_j} ${fin_h} || \
   { echo "ERREUR dans les dates" ; exit ; }

deb_a=`date -d "${deb_j}" +%Y`
fin_a=`date -d "${fin_j}" +%Y`
if [ ${deb_a} -eq ${fin_a} ] ; then
  deb_julh=`./utils/date2iter.sh ${deb_j} ${deb_h}` #iteration par rapport au 1 jan    
  fin_julh=`./utils/date2iter.sh ${fin_j} ${fin_h}`
else
  echo "Attention: annee de debut differente de l'annee de fin !"
  exit 1
fi
simul_steps=`expr ${fin_julh} \- ${deb_julh} \+ 1`

# pour le mode "run" gestion du multi-tâches 
# cree des fichiers dans ${localdir}/etat
# Avant de lancer un run, supprime le contenu de ce repertoire
run=${deb_j}${deb_h}_${fin_j}${fin_h}
#path_status=${cartoprox}/statut_calcul/${cartoprox_domaine}_${run} #${selection} #_pol${polluant}
path_status=${cartoprox}/statut_calcul/${cartoprox_domaine}_${run}${selection} #_pol${polluant}
mkdir -p ${path_status}
mkdir -p ${localdir}/tmp
id=`date +%Y%m%d"-"%H%M%S`
liste_domaines_run=${localdir}/tmp/liste_domaines_run.${id}
while [ -f ${liste_domaines_run} ] ; do
id=`date +%Y%m%d"-"%H%M%S`
liste_domaines_run=${localdir}/tmp/liste_domaines_run.${id}
done

##########################################################################
echo "Domaines actifs pour ${cartoprox_domaine} extrait de ${params_mailles} --> ${liste_domaines_run}"

#Utilise les domaines pour lesquels emis_profil != 0 (colonne 5)
#Liste + classement
case ${mode} in
grid) #Traite tous les mini-domaines y compris ceux sans emissions
awk '( $1 =="'${cartoprox_domaine}'" ) { print $2 }'  ${params_mailles} | sort > ${liste_domaines_run}
;;
*) #Traite seulement les mini-domaines avec des emissions
awk '( $1 =="'${cartoprox_domaine}'" && $5 != 0 ) { print $2 }'  ${params_mailles} | sort > ${liste_domaines_run}
;;
esac

#Nombre de mini domaines
ndom=`wc -l ${liste_domaines_run} | gawk '{print $1}'` 

echo -e "$BLEU" "Selection: ${selection} --> ${ndom} mini-domaines" "$NORMAL"

if [ "${selection}" == "" ] ; then
  echo "Aucune selection. CONTINUE MALGRE TOUT ?"
  echo "Definir une selection export selection="
  read continuer
fi

if [ ${do_grid} -eq 1 ] ; then
###########################################################################
# MAILLAGE OPTIMAL (chaque mini-domaine)
###########################################################################
echo "***info: determine les domaines ou generer le maillage..."
# <<<<< DEBUT BOUCLE SUR LES MINI-DOMAINES >>>>>>
idom=0
cat ${liste_domaines_run} | while read code_domaine ; do
# <<<<< DEBUT BOUCLE SUR LES MINI-DOMAINES >>>>>>
idom=`expr ${idom} \+ 1`
### dependant du domaine de calcul####################################
# recupere les donnees nécessaires au mailleur
source ./utils/infos_mailleur.sh ${cartoprox_domaine} ${code_domaine} ${periode} > /dev/null || \
  { echo "Erreur dans ./utils/infos_mailleur.sh" ; exit 1 ; }
######################################################################
if [ ! -f ${ficrecept_mailleur} ] ; then
echo "***************************************************************************************************************"
echo " MAILLAGE Domaine: ${cartoprox_domaine} Selection: ${selection} Mini-domaine: ${code_domaine} (${idom} sur ${ndom})"
echo "***************************************************************************************************************"
ChemRes=${ChemRes1}/${code_domaine}
mkdir -p ${ChemRes}
log_domaine=${ChemRes}/${run}${selection}_grid_pol${polluant}.log
syntaxe="${maillage_script} ${code_domaine}"
echo "-> syntaxe : ${syntaxe}"
echo "-> log dans ${log_domaine}"
${syntaxe}  > ${log_domaine} 2>&1
./utils/infos_domaine.sh ${cartoprox_domaine} ${code_domaine} ${periode}
fi
# <<<<< FIN BOUCLE SUR LES MINI-DOMAINES >>>>>>
done
# <<<<< FIN BOUCLE SUR LES MINI-DOMAINES >>>>>>
fi
###########################################################################
# SUREMIS
###########################################################################
if [ ${do_suremis} -eq 1 ]  ; then  
echo "***************************************************************************************************************"
echo " SUREMIS: ${cartoprox_domaine} Selection: ${selection}"
echo " Dates : ${deb_j} ${deb_h} TU au ${fin_j} ${fin_h} TU"
echo " Nous sommes le : `date`"
echo "***************************************************************************************************************"
syntaxe="${suremis_script} ${deb_j} ${deb_h} ${fin_j} ${fin_h}"
ChemRes=${ChemRes1}
log_domaine=${ChemRes1}/${run}${selection}_suremis.log
echo "-> syntaxe : ${syntaxe}"
echo "-> log dans ${log_domaine}"
echo ${syntaxe}
${syntaxe} > ${log_domaine} || \
      { echo "$0: ERREUR du script ${suremis_script}" ; exit 1; } 


fi  #do_suremis

if [ ${do_reg} -eq 1 ] ; then
###########################################################################
# PREVALP (1 fois)
###########################################################################
echo "***info: determine les domaines ou extraire PREVALP..."
liste_domaines_prevalp=${localdir}/liste_domaines_prevalp.txt
rm -rf ${liste_domaines_prevalp}
touch ${liste_domaines_prevalp}
# <<<<< DEBUT BOUCLE SUR LES MINI-DOMAINES >>>>>>
idom=0
cat ${liste_domaines_run} | while read code_domaine ; do
# <<<<< DEBUT BOUCLE SUR LES MINI-DOMAINES >>>>>>
idom=`expr ${idom} \+ 1`
### dependant du domaine de calcul####################################
source ./utils/infos_domaine.sh ${cartoprox_domaine} ${code_domaine} ${periode} #> /dev/null || \
  #{ echo "Erreur dans ./utils/infos_domaine.sh" ; exit 1 ; }
######################################################################
ChemOut=${sirane_prevalp}/${cartoprox_domaine}/${periode}
ficout_dir=${ChemOut}/${code_domaine}
ficout_pression=${ficout_dir}/${prefix_pression}_${periode}_prevalp.dat
ficout_evolmto=${ficout_dir}/${prefix_evolmeteo}_${periode}_prevalp.dat
ficout_fond=${ficout_dir}/${prefix_fond}_${periode}_prevalp.dat
#Regenere tous les fichiers a chaque lancement (decocher)
#rm -rf ${ficout_fond} ${ficout_evolmto} ${ficout_pression}
if [ ! -f ${ficout_pression} ] || [ ! -f ${ficout_evolmto} ] || [ ! -f ${ficout_fond} ] ; then
 echo ${cartoprox_domaine} ${code_domaine} ${periode} ${vcadastre}  ${version_num} ${version_src} ${xb} ${yb} ${dx}  ${cadre_dx} \
   >> ${liste_domaines_prevalp}
fi
# <<<<< FIN BOUCLE SUR LES MINI-DOMAINES >>>>>>
done
# <<<<< FIN BOUCLE SUR LES MINI-DOMAINES >>>>>>

echo "${liste_domaines_prevalp} / ${liste_domaines_run}"

# <<<<< EXECUTION : 1 FOIS >>>>>>
# des domaines a traiter ?
ndom_prevalp=`wc -l ${liste_domaines_prevalp} | gawk '{print $1}'`
if [ ${ndom_prevalp} -gt 0 ] ; then
echo "***info: EXPORT PREVALP sur domaines:"
gawk '{print $2}' ${liste_domaines_prevalp}
# Lance le module regional
syntaxe="${regional_script} ${liste_domaines_prevalp} ${periode}"
echo "-> syntaxe : ${syntaxe}"
#echo "-> log dans ${log}"
${syntaxe}  || \
  { echo "Erreur dans ${syntaxe}" ; exit 1 ; }
else
  echo -e "$VERT " "***info: EXPORT PREVALP sur tous les domaines OK" "$NORMAL"
fi
# <<<<< EXECUTION : 1 FOIS >>>>>>

echo -n "***info: verifie l extraction PREVALP..."
# <<<<< DEBUT BOUCLE SUR LES MINI-DOMAINES >>>>>>
idom=0
cat ${liste_domaines_run} | while read code_domaine ; do
# <<<<< DEBUT BOUCLE SUR LES MINI-DOMAINES >>>>>>
idom=`expr ${idom} \+ 1`

# Pas besoin d'executer infos_domaine.sh
ChemOut=${sirane_prevalp}/${cartoprox_domaine}/${periode}
ficout_dir=${ChemOut}/${code_domaine}
ficout_pression=${ficout_dir}/${prefix_pression}_${periode}_prevalp.dat
ficout_evolmto=${ficout_dir}/${prefix_evolmeteo}_${periode}_prevalp.dat
ficout_fond=${ficout_dir}/${prefix_fond}_${periode}_prevalp.dat
# Verifie presence des fichiers
for fic in ${ficout_pression} ${ficout_evolmto} ${ficout_fond} ; do
 if [ ! -f ${fic} ]  ; then
   echo -e "$ROUGE" " PB (${idom} sur ${ndom_prevalp}) ${fic} --> STOP" "$NORMAL"
   exit
 fi 
done
# <<<<< FIN BOUCLE SUR LES MINI-DOMAINES >>>>>>
done
echo -e "$VERT" "OK" "$NORMAL"
# <<<<< FIN BOUCLE SUR LES MINI-DOMAINES >>>>>>
fi # do_reg

###########################################################################
# EMISSIONS + SIRANE + STAT (chaque mini-domaine)
###########################################################################
# <<<<< DEBUT BOUCLE SUR LES MINI-DOMAINES >>>>>>
idom=0
cat ${liste_domaines_run} | while read code_domaine ; do
# <<<<< DEBUT BOUCLE SUR LES MINI-DOMAINES >>>>>>
idom=`expr ${idom} \+ 1`

#Creation du repertoire de sortie
ChemRes=${ChemRes1}/${code_domaine} # OPTIMISATION: sort cette declaration pour eviter d'executer infos_domaine.sh
mkdir -p ${ChemRes}
#
#Nom des fichiers de sortie
case ${polluant} in
0)prefix_nc=_passif;;
1)prefix_nc=_nox;;
2|3)prefix_nc=_${var_pm};;
esac
run=${deb_j}${deb_h}_${fin_j}${fin_h}
fic_sirane_nc=${ChemRes}/sirane${prefix_nc}.${run}.nc
fic_suremis_nc=${ChemRes}/suremis${prefix_nc}.${run}.nc
fic_carto_nc=${ChemRes}/cartoprox${prefix_nc}.${run}.nc
fic_stat_nc=${ChemRes}/stat.${run}.nc
case ${mode} in
sirane_*)liste_fic_nc="${fic_sirane_nc} ${fic_suremis_nc}";;
cartoprox_*)liste_fic_nc=${fic_carto_nc};;
stat)liste_fic_nc=${fic_stat_nc};;
esac
case ${mode} in
sirane_*|cartoprox_*)
for fic_nc in ${liste_fic_nc} ; do
#echo ${fic_nc}
if [ ! -f ${fic_nc} ] && [ -f ${path_status}/${code_domaine}_pol${polluant}_OK ] ; then
  rm ${path_status}/${code_domaine}_pol${polluant}_OK
fi
if [ -f ${fic_nc} ] && \
 [ ! -f ${path_status}/${code_domaine}_pol${polluant}_OK  ]  && \
 [ ! -f ${path_status}/${code_domaine}_pol${polluant}_RUN ]; then  
  ifnout_steps=`ncdump -h ${fic_nc} | grep "currently" | gawk -F '( // |  )' '{print $2}' | sed 's#(##g' | sed 's# currently)##g'`
  if [ ${ifnout_steps} -eq ${simul_steps} ] ; then
    echo -e "Fichier ${fic_nc} steps: $VERT ${ifnout_steps}/${simul_steps}" "$NORMAL"
    touch ${path_status}/${code_domaine}_pol${polluant}_OK
  else
    echo -e "Fichier ${fic_nc} steps: $ROUGE ${ifnout_steps}/${simul_steps}" "$NORMAL"
    rm -f ${path_status}/${code_domaine}_pol${polluant}_OK
  fi
fi
done
;;
stat)
for fic_nc in ${liste_fic_nc} ; do
if [ ! -f ${fic_nc} ] && [ -f ${path_status}/${code_domaine}_OK ] ; then
  rm ${path_status}/${code_domaine}_OK
fi
done
#if [ -f ${fic_nc} ] && \
# [ ! -f ${path_status}/${code_domaine}_OK  ]  && \
# [ ! -f ${path_status}/${code_domaine}_RUN ]; then
#  touch ${path_status}/${code_domaine}_OK
#  echo ${fic_nc} OK
#fi
;;
esac

#++ reseau/emissions +++++++++++++++++++++++++++++++++++++++++++++
if [ ${do_emis} -eq 1 ]  ; then 

### dependant du domaine de calcul####################################
source ./utils/infos_domaine.sh ${cartoprox_domaine} ${code_domaine} ${periode} #> /dev/null || \
#  { echo "Erreur dans ./utils/infos_domaine.sh" ; exit 1 ; }
######################################################################
echo -e "$BLEU" "***************************************************************************************************************" "$NORMAL"
echo -e "$BLEU" " EMISSIONS Domaine: ${cartoprox_domaine} Selection: ${selection} Mini-domaine: ${code_domaine} (${idom} sur ${ndom})" "$NORMAL"
echo -e "$BLEU" "***************************************************************************************************************" "$NORMAL"
syntaxe="${emis_script} ${code_domaine} ${periode}"

#LOG
ChemRes=${ChemRes1}/${code_domaine}
mkdir -p ${ChemRes}

for pol_tmp in 1 2 3 ; do

  #Chemin des fichiers a l'identique de ${emis_script}
  ficemisnoeuds=${sirane_geom}/${cartoprox_domaine}/${code_domaine}/emis_${vcadastre}_noeuds_${pol_tmp}.txt
  ficemisrues=${sirane_geom}/${cartoprox_domaine}/${code_domaine}/emis_${vcadastre}_rues_${pol_tmp}.txt
  ficemisponct=${sirane_geom}/${cartoprox_domaine}/${code_domaine}/emis_${vcadastre}_ponct_${pol_tmp}.txt  
  log_domaine=${ChemRes}/${run}${selection}_emis_pol${pol_tmp}.log
  if [ ! -f ${ficemisnoeuds}  ] || [ ! -f ${ficemisponct}  ] ; then
  echo "-> syntaxe : ${syntaxe}"
  echo "-> log dans ${log_domaine}"  
  ${syntaxe} ${pol_tmp} > ${log_domaine} || \
      { echo "$0: ERREUR du script ${emis_script} voir le log :"; echo ${log_domaine} ; label=PB ; couleur=$ROUGE ; } #>> ${log} 2>&1
  fi
    
  if [ ! -f ${ficemisnoeuds} ]  ; then
    echo -e "$ROUGE" "***erreur : generation de ${ficemisnoeuds} echoue" "$NORMAL"
    echo "-> voir le log"
    exit 1
  else
   echo -e "$VERT" "***info : generation de ${ficemisnoeuds} OK" "$NORMAL"
  fi
  
  if [ ! -f ${ficemisponct} ] ; then
    echo -e "$ROUGE" "***erreur : generation de ${ficemisponct} echoue" "$NORMAL"
    #echo "-> voir le log"
    #exit 1
  else
   echo -e "$VERT" "***info : generation de ${ficemisponct} OK" "$NORMAL"
  fi  

done

fi # do_emis

#++ run SIRANE +++++++++++++++++++++++++++++++++++++++++++++++++++
if [ ${do_sirane} -eq 1 ] && \
   [ ! -f ${path_status}/${code_domaine}_pol${polluant}_RUN ] && \
   [ ! -f ${path_status}/${code_domaine}_pol${polluant}_OK  ] && \
   [ ! -f ${path_status}/${code_domaine}_pol${polluant}_PB  ] ; then
touch ${path_status}/${code_domaine}_pol${polluant}_RUN
label=RUN
echo -e "$BLEU" "***************************************************************************************************************" "$NORMAL"
echo -e "$BLEU" " SIRANE: ${cartoprox_domaine} Polluant:${polluant} Selection: ${selection} Mini-domaine: ${code_domaine} (${idom} sur ${ndom})" "$NORMAL"
echo -e "$BLEU" " Dates : ${deb_j} ${deb_h} TU au ${fin_j} ${fin_h} TU" "$NORMAL"
echo -e "$BLEU" " Infos domaine ${code_domaine}:" "$NORMAL"
### dependant du domaine de calcul####################################
source ./utils/infos_domaine.sh ${cartoprox_domaine} ${code_domaine} ${periode} #> /dev/null
######################################################################
echo -e "$BLEU" " Nous sommes le : `date`" "$NORMAL"
echo -e "$BLEU" "***************************************************************************************************************" "$NORMAL"
#Calcul sur des grilles ou a des recepteurs ?
case ${selection} in 
_campagnes)syntaxe="${run_script} ${code_domaine} ${deb_j} ${deb_h} ${fin_j} ${fin_h}";;
*)syntaxe="${run_script} ${code_domaine} ${deb_j} ${deb_h} ${fin_j} ${fin_h} irecept_no";;
esac
#LOG
ChemRes=${ChemRes1}/${code_domaine}
log_domaine=${ChemRes}/${run}${selection}_run_pol${polluant}.log
echo "-> syntaxe : ${syntaxe}"
echo "-> log dans ${log_domaine}" # 
${syntaxe} 1> ${log_domaine} || \
      { echo "$0: ERREUR du script ${run_script} voir le log :"; echo ${log_domaine} ; label=PB ; couleur=$ROUGE ; } #>> ${log} 2>&1

if [ "${label}" != "PB" ] ; then
  if [ -f ${fic_sirane_nc} ] ; then
    label=OK
    couleur=$VERT
  else
    label=SKIP
    couleur=$ROUGE
  fi
fi
mv ${path_status}/${code_domaine}_pol${polluant}_RUN ${path_status}/${code_domaine}_pol${polluant}_${label}
echo -e "$couleur" "-> ${fic_sirane_nc}" "$NORMAL"
fi # do_sirane

#++ run CARTOPROX +++++++++++++++++++++++++++++++++++++++++++++++++++
if [ ${do_cartoprox} -eq 1 ] && \
   [ ! -f ${path_status}/${code_domaine}_pol${polluant}_RUN ] && \
   [ ! -f ${path_status}/${code_domaine}_pol${polluant}_OK  ] && \
   [ ! -f ${path_status}/${code_domaine}_pol${polluant}_PB  ] ; then
touch ${path_status}/${code_domaine}_pol${polluant}_RUN
label=RUN
echo -e "$BLEU" "***************************************************************************************************************" "$NORMAL"
echo -e "$BLEU" " CARTOPROX: ${cartoprox_domaine} Polluant:${polluant} Selection: ${selection} Mini-domaine: ${code_domaine} (${idom} sur ${ndom})" "$NORMAL"
echo -e "$BLEU" " Dates : ${deb_j} ${deb_h} TU au ${fin_j} ${fin_h} TU" "$NORMAL"
echo -e "$BLEU" " Infos domaine ${code_domaine}:" "$NORMAL"
### dependant du domaine de calcul####################################
source ./utils/infos_domaine.sh ${cartoprox_domaine} ${code_domaine} ${periode} #> /dev/null
######################################################################
echo -e "$BLEU" " Nous sommes le : `date`" "$NORMAL"
echo -e "$BLEU" "***************************************************************************************************************" "$NORMAL"
syntaxe="${cartoprox_script} ${code_domaine} ${deb_j} ${deb_h} ${fin_j} ${fin_h}"
#LOG
ChemRes=${ChemRes1}/${code_domaine}
log_domaine=${ChemRes}/${run}${selection}_cartoprox.log
echo "-> syntaxe : ${syntaxe}"
echo "-> log dans ${log_domaine}" # 1> ${log_domaine} || \
${syntaxe} 1> ${log_domaine} || \
      { echo "$0: ERREUR du script ${run_script} voir le log :"; echo ${log_domaine} ; label=PB ; couleur=$ROUGE ; } 

if [ "${label}" != "PB" ] ; then
  if [ -f ${fic_carto_nc} ] ; then
    label=OK
    couleur=$VERT
    echo -e "Efface le log" #Mng
    rm $log_domaine  #Mng
  else
    label=SKIP
    couleur=$ROUGE
  fi
fi
mv ${path_status}/${code_domaine}_pol${polluant}_RUN ${path_status}/${code_domaine}_pol${polluant}_${label}
echo -e "$couleur" "-> ${fic_carto_nc}" "$NORMAL"
fi # do_cartoprox

#++ STAT +++++++++++++++++++++++++++++++++++++++++++++++++++
if [ ${do_stat} -eq 1 ] && \
   [ ! -f ${path_status}/${code_domaine}_pol1_RUN ] && \
   [ ! -f ${path_status}/${code_domaine}_pol1_PB  ] && \
   [ ! -f ${path_status}/${code_domaine}_pol2_RUN ] && \
   [ ! -f ${path_status}/${code_domaine}_pol2_PB  ] && \
   [ ! -f ${path_status}/${code_domaine}_OK  ] && \
   [ ! -f ${path_status}/${code_domaine}_PB  ] && \
   [ ! -f ${path_status}/${code_domaine}_RUN ] ; then
touch ${path_status}/${code_domaine}_RUN
label=RUN
echo -e "$BLEU" "***************************************************************************************************************" "$NORMAL"
echo -e "$BLEU" " STAT: ${cartoprox_domaine} Selection: ${selection} Mini-domaine: ${code_domaine} (${idom} sur ${ndom})" "$NORMAL"
echo -e "$BLEU" " Dates : ${deb_j} ${deb_h} TU au ${fin_j} ${fin_h} TU" "$NORMAL"
echo -e "$BLEU" " Nous sommes le : `date`" "$NORMAL"
echo -e "$BLEU" "***************************************************************************************************************" "$NORMAL"
syntaxe="${stat_script} ${code_domaine} ${deb_j} ${deb_h} ${fin_j} ${fin_h}"
ChemRes=${ChemRes1}/${code_domaine}
log_domaine=${ChemRes}/${run}${selection}_stat.log
echo "-> syntaxe : ${syntaxe}"
echo "-> log dans ${log_domaine}"
echo ${syntaxe}
${syntaxe} > ${log_domaine} || \
      { echo "$0: ERREUR du script ${stat_script}" ; echo ${log_domaine} ; label=PB ; couleur=$ROUGE ; } 
if [ "${label}" != "PB" ] ; then
  if [ -f ${fic_stat_nc} ] ; then
    label=OK
    couleur=$VERT
  else
    label=SKIP
    couleur=$ROUGE
  fi
fi
mv ${path_status}/${code_domaine}_RUN ${path_status}/${code_domaine}_${label}
echo -e "$couleur" "-> ${fic_stat_nc}" "$NORMAL"
fi  # do_stat

# <<<<< FIN BOUCLE SUR LES MINI-DOMAINES >>>>>>
done
# <<<<< FIN BOUCLE SUR LES MINI-DOMAINES >>>>>>

rm ${liste_domaines_run}

###########################################################################
# EMISSIONS + SIRANE + STAT (1 fois)
###########################################################################

###########################################################################
# MAP genere les ZOOM
###########################################################################
if [ ${do_zoom} -eq 1 ] ; then

#liste_zoom_level="1 2 3 4 5 6" # dans cartoprox.inc
#liste_variable="no2_moy_an pm10_moy_an nb_dep_50_jour"
liste_variable="pm10_moy_an nb_dep_50_jour"  #"pm10_moy_an"  #pm25_moy_an

for zoom_level in ${liste_zoom_level} ; do
echo "***************************************************************************************************************"
echo " ZOOM ${zoom_level}: Domaine: ${cartoprox_domaine} Selection: ${selection}"
echo " Dates : ${deb_j} ${deb_h} TU au ${fin_j} ${fin_h} TU"
echo " Nous sommes le : `date`"
echo "***************************************************************************************************************"
ChemRes=${ChemRes1}/${code_domaine}
log_domaine=${ChemRes}/${run}${selection}_stat.log
for variable in ${liste_variable} ; do
syntaxe="${zoom_script} ${zoom_level} ${variable} ${deb_j} ${deb_h} ${fin_j} ${fin_h}"
echo "-> syntaxe : ${syntaxe}"
${syntaxe} || \
    { echo "$0: ERREUR du script ${zoom_script}" ; } 
done
done

fi  # do_zoom

echo "***info: fin du script $0"

date

