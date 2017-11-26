#!/bin/bash
#
# CARTOPROX
# export des données en un point
#
annee_defaut=2010 # dans cartoprox.inc
deb_j=${annee_defaut}0101
fin_j=${annee_defaut}1231
deb_h=00
fin_h=23
periode_loc=${deb_j}_${fin_j}

case $# in
2)
xp=$1
yp=$2
;;
*)
echo "Syntaxe $0 x(m) y(m)"
exit
;;
esac
# calcul du centre de la maille PREVALP
# xc = xmin + int((xp-xmin)/dx+1 - 1) * dx + dx/2

#Include
source ${cartoprox}/cartoprox.inc ${periode_loc} #> /dev/null || \
#  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; }

#trouve le code domaine de CARTOPROX
ix=`echo "${cartoprox_xmin} ${cartoprox_dx} ${xp}" | gawk '{ print int( ($3-$1)/$2 + 1 )  }'`
iy=`echo "${cartoprox_ymin} ${cartoprox_dy} ${yp}" | gawk '{ print int( ($3-$1)/$2 + 1 )  }'`
#echo "ix=${ix} iy=${iy}"
xcentre=`echo "${cartoprox_xmin} ${cartoprox_dx} ${ix}" | gawk '{ print int( $1 + ($3-.5) * $2 )  }'`
ycentre=`echo "${cartoprox_ymin} ${cartoprox_dy} ${iy}" | gawk '{ print int( $1 + ($3-.5) * $2 )  }'`

code_domaine=${xcentre}_${ycentre}
#echo "code_domaine=${code_domaine}"

echo ${code_domaine}
