#!/bin/ksh
#
# Crerer une selection pour CARTOPROX 
# 
# usage : plot_CARTOPROX.sh + le mois voulu
#############################################

localdir=`pwd`

export cartoprox=`pwd | gawk -F '/scripts' '{print $1}'`

annee_defaut=2010 # dans cartoprox.inc
deb_j=${annee_defaut}0101
fin_j=${annee_defaut}1231
deb_h=00
fin_h=23

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
source ${cartoprox}/include/cartoprox.inc ${periode} || \
  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; }
############################################################################################
coord=utm #utm/lambert

case $# in
1)selection=$1;;
*)
echo "Syntaxe $0 selection"
echo "Continue dans 5 secondes avec tous les recepteurs de la selection ${selection}"
sleep 5
;;
esac

params_mailles_brut=${cartoprox}/selections/mailles_${cartoprox_domaine}${selection}.txt
fic_infos=${cartoprox}/selections/infos/infos_recepteurs_${coord}_${cartoprox_domaine}${selection}.csv
mkdir -p ${cartoprox}/selections/infos

if [ ! -f ${params_mailles} ] ; then
  echo "***erreur: ${params_mailles}"
  exit 1
fi

id=`date +%Y%m%d"-"%H%M%S`
liste_domaines_run=${localdir}/tmp/liste_domaines_run.${id}
while [ -f ${liste_domaines_run} ] ; do
id=`date +%Y%m%d"-"%H%M%S`
liste_domaines_run=${localdir}/tmp/liste_domaines_run.${id}
done

##########################################################################
echo "Domaines actifs pour ${cartoprox_domaine} extrait de ${params_mailles} --> ${liste_domaines_run}"

# Liste + classement
gawk '( $1 =="'${cartoprox_domaine}'" ) { print $2 }'  ${params_mailles} | sort > ${liste_domaines_run}

# nombre de mini domaines
ndom=`wc -l ${liste_domaines_run} | gawk '{print $1}'` 

echo "Selection${selection} --> ${ndom} mini-domaines"

# Fichier info
rm -rf ${fic_infos}
#echo "recept_brin;cartoprox_domaine;code_domaine;OccupSol;emis_profil;recepteurs;xc;yc;xb;yb;xrecept;yrecept" > ${fic_infos}
echo "recept_brin,cartoprox_domaine,id,X_m,Y_m" > ${fic_infos}

echo "***info: remplissage de ${fic_infos} <- ${params_mailles}"

# <<<<< DEBUT BOUCLE SUR LES MINI-DOMAINES >>>>>>
idom=0
cat ${liste_domaines_run} | while read code_domaine ; do
# <<<<< DEBUT BOUCLE SUR LES MINI-DOMAINES >>>>>>
idom=`expr ${idom} \+ 1`

### dependant du domaine de calcul####################################
source ./utils/infos_mailleur.sh ${cartoprox_domaine} ${code_domaine} ${periode} > /dev/null
######################################################################

# Regenere tous les maillage (DECOMMENTER)
#rm -rf ${ficrecept_mailleur}

if [ ! -f ${ficrecept_mailleur} ] ; then
  echo -n "${idom}/${ndom} ***info: genere ${ficrecept_mailleur}..."
  syntaxe="${maillage_script} ${code_domaine}"
  ${syntaxe} > /dev/null
  echo "OK"
fi  

if [ ! -f  ${ficrecept_mailleur} ] ; then
  echo "${idom}/${ndom} ***ERREUR: maillage n existe pas dans ${ficrecept_mailleur}..."
  exit  
else

### dependant du domaine de calcul####################################
source ./utils/infos_domaine.sh ${cartoprox_domaine} ${code_domaine} ${periode} > /dev/null
######################################################################

#Recupere les coordonnées des recepteurs

# formats diferents
gawk '{ print "R,'${code_domaine}',"$1 }' ${ficrecept_mailleur} >  tmp.info
gawk '{ print "B,'${code_domaine}',"$1 }' ${ficbrin_mailleur}   >> tmp.info

case ${coord} in
utm)
gawk '{ print $4","$5 }' ${ficrecept_mailleur} >  tmp.xy
gawk '{ print $4","$5 }' ${ficbrin_mailleur}   >> tmp.xy
;;
lambert)
gawk '{ print $2","$3 }' ${ficrecept_mailleur} >  tmp.xy
gawk '{ print $2","$3 }' ${ficbrin_mailleur}   >> tmp.xy
;;
esac
paste -d , tmp.info tmp.xy >> ${fic_infos}
rm tmp.info tmp.xy 

fi

if [ -f  ${ficrecept_mailleur} ] ; then
echo "${idom}/${ndom} ${ficrecept_mailleur} -> ${nrecepts}" 
fi

# <<<<< FIN BOUCLE SUR LES MINI-DOMAINES >>>>>>
done
# <<<<< FIN BOUCLE SUR LES MINI-DOMAINES >>>>>>

echo "${fic_infos} OK"
rm ${liste_domaines_run}
