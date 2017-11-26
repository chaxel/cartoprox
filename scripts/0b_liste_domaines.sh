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

case $# in
2)selection=$1;periode=$2;;
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
source ${cartoprox}/include/cartoprox.inc ${periode} || \
  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; }
############################################################################################
  
params_mailles=${cartoprox}/selections/mailles_${cartoprox_domaine}${selection}.txt
if [ ! -f ${params_mailles} ] ; then
  echo "***erreur: ${params_mailles}"
  exit 1
fi
#echo "Domaines actifs pour ${cartoprox_domaine} extrait de ${params_mailles}"
gawk '( $1 =="'${cartoprox_domaine}'" ) { print $2 }'  ${params_mailles} | sort 
