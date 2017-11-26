#!/bin/ksh
# lance
# lance_cartoprox.sh zoom sur une liste de selection 
# + 6c_map_raster.sh sur une liste de polluants
# creer les rasters de la region 
# Mng 29/08/2012 pour cartoprox region 2011
# 
#############################################

localdir=`pwd`
echo $localdir
. ${cartoprox}/include/zoom.inc
. ${cartoprox}/include/common.inc
source ${cartoprox}/cartoprox.inc ${periode} || \
  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; }

#############################################
list_selec="_B2_lyon" #_test8 _test12 _test16 _test20"  #_test4   #_B2_lyon _C3_grenoble
listvar="no2_moy_an"    #pm10_moy_an nb_dep_50_jour"
ref=2011
Test=sansA7 # sansE3 sansPL
ChemRes=/home/oper/data/CARTOPROX
ChemResRef=$ChemRes/res_V31_A7_20110101_20111231
ChemResTest=$ChemRes/test_coupure_A7_$Test
cd $ChemResTest
list_maille=ls -d */
echo $list_maille
##############################################

for maille in $list_maille
do
  sirane_diff=${ref}-${Test}
  cartoprox_diff=cartoprox_${Test}
  
  echo "===================== traitement de la maille $maille  =========================="
  echo -e "$VERT" " raster de $selec en $var cree : ${fic_out_base}_${zoom_projection} " "$NORMAL"
done # maille

