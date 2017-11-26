#!/bin/ksh
#
# creation de donnees.dat pour sirane LYON 
# + lancement de run mensuel
# 
# usage : lance_CARTOPROX.sh + le mois voulu
#############################################

localdir=`pwd`

export cartoprox=`pwd | gawk -F '/scripts' '{print $1}'`

params_campagnes= # code_domaine,deb_j,deb_h,fin_j,fin_h

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

case $# in
0)
if [ "${deb_j}" == "" ] && [ "${fin_j}" == "" ] ; then
echo "Date debut simulation (format AAAAMMJJ)?"
read deb_j
echo "Date fin simulation (format AAAAMMJJ)?"
read fin_j
fi
;;
2)
deb_j=$1
fin_j=$2
;;
4)
deb_j=$1
deb_h=$2
fin_j=$3
fin_h=$4
;;
*)
echo "Syntaxe $0 deb_j (deb_h) fin_j (fin_h)"
exit
;;
esac

#Nom des fichiers de campagne
params_campagnes=${cartoprox}/inputs/CAMPAGNES/campagnes_dates_${cartoprox_domaine}${selection}.txt   
params_sites=${cartoprox}/inputs/CAMPAGNES/campagnes_sites_${cartoprox_domaine}${selection}.txt    

#Verifie l'existence des fichiers de campagnes
if [ ! -f ${params_campagnes} ] ; then
  echo "Fichier de DATES introuvable: ${params_campagnes}"
  echo "Format: nom_campagne deb_j(AAAAMMJJ) deb_h(HH) fin_j(AAAAMMJJ) fin_h(HH)"
  exit 1
fi

if [ ! -f ${params_sites} ] ; then
  echo "Fichier de SITES introuvable: ${params_sites}"
  echo "Format: nom_site utm_x_metres utm_y_metres"
  exit 1
fi

echo "Liste des campagnes pour ${cartoprox_domaine} dans ${params_campagnes}"
# Liste + classement
gawk '{ print $0 }' ${params_campagnes} | sort > ${localdir}/campagnes.txt 

#dos2unix ${localdir}/campagnes.txt 

# nombre de mini domaines
#ndom=`wc -l ${localdir}/campagnes.txt | gawk '{print $1}'` 
#idom=0
#cat ${localdir}/campagnes.txt
# <<<<< DEBUT BOUCLE SUR LES CAMPAGNES >>>>>>
cat ${params_campagnes} | while read ligne_camp ; do

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
# Infos campagne
############################################################################################
nom_camp=`echo ${ligne_camp} | gawk '{print $1}'`
deb_camp_j=`echo ${ligne_camp} | gawk '{print $2}'`
deb_camp_h=`echo ${ligne_camp} | gawk '{print $3}'`
fin_camp_j=`echo ${ligne_camp} | gawk '{print $4}'`
fin_camp_h=`echo ${ligne_camp} | gawk '{print $5}'`

############################################################################################
# La campagne est-elle contenue dans la plage de PREVALP ?
############################################################################################
if [ ${deb_camp_j}${deb_camp_h} -ge ${deb_j}${deb_h} ] && [ ${fin_camp_j}${fin_camp_h} -le ${fin_j}${fin_h} ] ; then
  #Le fichier REGIONAL a utiliser est le fichier annuel
  deb_reg_j=${deb_j}
  deb_reg_h=${deb_h}
  fin_reg_j=${fin_j}
  fin_reg_h=${fin_h}
  do_stat=1
else # Fichier REGIONAL de campagne
  do_stat=0
fi

#La campagne existe ?
if [ ${do_stat} -eq 1 ] ; then
 if [ "${deb_camp_j}" != "" ] && [ "${fin_camp_j}" != "" ]  ; then
  syntaxe="${campagnes_stat_script} ${nom_camp} ${params_sites} ${deb_reg_j} ${deb_reg_h} ${fin_reg_j} ${fin_reg_h} ${deb_camp_j} ${deb_camp_h} ${fin_camp_j} ${fin_camp_h}"
  liste_stat="j" #avg="moyenne annuelle" j="journaliere" h="horaire"
  for s in ${liste_stat} ; do
    echo "-> syntaxe : ${syntaxe} $s"
    ${syntaxe} $s || { echo "Erreur dans ${campagnes_stat_script} pour stat $s" ; exit 1 ; }
  done
 else
  echo "SKIP ${code_domaine} -> pas de date"
 fi 
else
 echo "Dates de campagnes ${deb_camp_j} --> ${fin_camp_j} differe de la periode ${deb_j} --> ${fin_j}"
fi

done
date
