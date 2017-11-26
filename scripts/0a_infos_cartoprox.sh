#!/bin/bash
#############################################
#Infos sur domaine $cartoprox_domaine
#############################################
localdir=`pwd`

export cartoprox=`pwd | gawk -F '/scripts' '{print $1}'`

annee_defaut=2010 # dans cartoprox.inc
deb_j=${annee_defaut}0101
fin_j=${annee_defaut}1231
deb_h=00
fin_h=23

case $# in
1|2)
selection=$1
periode=$2
;;
*)
echo "Syntaxe: $0 selection periode"
echo "Continue? ENTER"
read continuer
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
source ${cartoprox}/include/cartoprox.inc ${periode} || \
  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; }
############################################################################################
  
params_mailles=${cartoprox}/selections/mailles_${cartoprox_domaine}${selection}.txt
if [ ! -f ${params_mailles} ] ; then
  echo "***erreur: ${params_mailles}"
  exit 1
fi
#echo "Domaines actifs pour ${cartoprox_domaine} extrait de ${params_mailles}"
gawk '( $1 =="'${cartoprox_domaine}'" ) { print "code_domaine="$2" domaine_dx="$9" cadre_dx="$10" profil_emis="$5 }'  ${params_mailles} | sort 

#nombre de mini domaines
ndoms=`gawk '( $1 =="'${cartoprox_domaine}'" ) { print $2 }'  ${params_mailles} | wc -l` 
echo "Selection ${selection} --> ${ndoms} mini-domaines"

#calcul du domaine de CARTOGRAPHIE
maille_xmin=`awk 'BEGIN { min =  1.E9 }; ($7-$9/2 < min && $7 != "" && $9 != "") {min = $7-$9/2}; END { print int(min) }'  ${params_mailles}`
maille_ymin=`awk 'BEGIN { min =  1.E9 }; ($8-$9/2 < min && $8 != "" && $9 != "") {min = $8-$9/2}; END { print int(min) }'  ${params_mailles}`
maille_xmax=`awk 'BEGIN { max = -1.E9 }; ($7+$9/2 > max && $7 != "" && $9 != "") {max = $7+$9/2}; END { print int(max) }'  ${params_mailles}`
maille_ymax=`awk 'BEGIN { max = -1.E9 }; ($8+$9/2 > max && $8 != "" && $9 != "") {max = $8+$9/2}; END { print int(max) }'  ${params_mailles}`

maille_dx=`echo ${maille_xmin} ${maille_xmax} | awk '{print $2-$1}'`
maille_dy=`echo ${maille_ymin} ${maille_ymax} | awk '{print $2-$1}'`

echo "domaine     ${cartoprox_domaine}"
echo "selection   ${selection}"
echo "maille_dx   ${maille_dx}"
echo "maille_dy   ${maille_dy}" 
echo "maille_xmin ${maille_xmin}"
echo "maille_ymin ${maille_ymin}"
echo "maille_xmax ${maille_xmax}"
echo "maille_ymax ${maille_ymax}" 
