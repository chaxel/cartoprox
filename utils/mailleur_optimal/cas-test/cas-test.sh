#!/bin/bash

#version=1.2

version=$1

fic_mif=../data/reseau_sirane_valence_v1.MIF
fac_recept_list="1.1 1.2 1.3 1.4 1.5 1.6 1.7 1.8 1.9 2.0"

#Distance inter-recepteurs
recept_dx=10

#n_recept_list="70"
#fac_recept_list="1.1"

resultdir=./resultats_v${version}_dx${recept_dx}

if [ ! -d ../src_V${version} ] ; then
  echo "Version inconnue:${version} - STOP"
  exit
fi

localdir=`pwd`
cd ../src_V${version}
make
cd ${localdir}

for fac_recept in ${fac_recept_list} ; do

#maillage
txtfile=${resultdir}/maillage_nc${n_recept}_fc${fac_recept}.txt
if [ ! -f ${txtfile} ] ; then
mkdir -p ${resultdir}
../src_V${version}/mailleur.exe -i ${fic_mif} -brin_dx 5 -recept_dx ${recept_dx} -cadre_dx 1000 -fac_geom ${fac_recept} \
         -utm 31 -xc 649500 -yc 4977500 -dx 3000 -d \
         -o ${txtfile}
fi

# grahique GMT
psfile=${resultdir}/maillage_nc${n_recept}_fc${fac_recept}.ps
awk  '{print $4/1000" "$5/1000}' ${txtfile} > ${txtfile}.xy

psxy ${txtfile}.xy -R648/651/4976/4979 -Jx3/3 -Sp -B500/500 > ${psfile}
echo  ${psfile} OK
rm  ${txtfile}.xy
ps2raster -A -P -Tg -E1000 ${psfile}
rm ${psfile}
done
