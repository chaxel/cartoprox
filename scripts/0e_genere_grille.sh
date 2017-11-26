#!/bin/bash
#Infos sur domaine cartoprox

localdir=`pwd`

export cartoprox=`pwd | gawk -F '/scripts' '{print $1}'`

annee_defaut=2010 # dans cartoprox.inc
deb_j=${annee_defaut}0101
fin_j=${annee_defaut}1231
deb_h=00
fin_h=23

#case $# in
#0|1)selection=$1;;
#esac

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
params_mailles_out=${cartoprox}/selections/mailles_${cartoprox_domaine}_toutes.txt

if [ ! -f ${params_mailles} ] ; then
  echo "***ATTENTION: ${params_mailles} n existe pas --> la creer"
  touch ${params_mailles}
  #exit 1
  sleep 2s
fi

#La grille existe (A COMMENTER SI N'EXISTE PAS)
maille_xmin=`awk 'BEGIN { min =  1.E9 }; ($7 < min && $7 != "") {min = $7}; END { print int(min - '${cartoprox_dx}'/2) }' ${params_mailles}`
maille_ymin=`awk 'BEGIN { min =  1.E9 }; ($8 < min && $8 != "") {min = $8}; END { print int(min - '${cartoprox_dx}'/2) }' ${params_mailles}`
maille_xmax=`awk 'BEGIN { max = -1.E9 }; ($7 > max && $7 != "") {max = $7}; END { print int(max + '${cartoprox_dx}'/2) }' ${params_mailles}`
maille_ymax=`awk 'BEGIN { max = -1.E9 }; ($8 > max && $8 != "") {max = $8}; END { print int(max + '${cartoprox_dx}'/2) }' ${params_mailles}`
echo "maille_xmin=${maille_xmin}"
echo "maille_ymin=${maille_ymin}"
echo "maille_xmax=${maille_xmax}"
echo "maille_ymax=${maille_ymax}"

#La grille n'existe pas
cadre_dx=1000
xmin=${maille_xmin}
ymin=${maille_ymin}
nx=`echo ${maille_xmin} ${maille_xmax} ${cartoprox_dx} | awk '{print int(($2-$1)/$3)}'`
ny=`echo ${maille_ymin} ${maille_ymax} ${cartoprox_dx} | awk '{print int(($2-$1)/$3)}'`
dx=${cartoprox_dx}
dy=${cartoprox_dx}
profil_defaut=0
emissions_defaut="2010_2011_h"

cat ${params_mailles} > grille.tmp
echo ${xmin} ${ymin} ${nx} ${ny} ${dx} ${dy} | \
awk '{ 
  for (i=1;i<=$3;i++){
    for (j=1;j<=$4;j++){
      x = int($1+(i-.5)*$5);
      y = int($2+(j-.5)*$6);
      print "'${cartoprox_domaine}'\t"x"_"y"\t'${annee_defaut}'\t'${emissions_defaut}'\t'${profil_defaut}'\tnone\t"x"\t"y"\t'${cartoprox_dx}'\t'${cadre_dx}'"
    }
  }
}' >> grille.tmp

cat grille.tmp | sort -t$'\t' -k2,3 -u  > ${params_mailles_out}
rm grille.tmp

wc -l ${params_mailles_out}

