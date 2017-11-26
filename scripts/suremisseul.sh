#!/bin/bash

#source ${cartoprox}/cartoprox.inc ${periode} || \
#  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; }
#export cartoprox=/mnt/mod4/appli/CARTOPROX_V352
#export cartoprox_domaine=region_A6_A89
#export periode=20110101_20111231

ChemRes=/home/oper/data/CARTOPROX/res_V31_region_A6_A89_20110101_20111231/650500_5053500
cd ${ChemRes}


#Xmin, xmax de la grille
ficgrille=grille_region_A6_A89_650500_5053500.inc
fond_dx=3
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

calc_suremis_nc_exe=/mnt/mod4/appli/CARTOPROX_V352/utils/calc_cartoprox_nc_V33/calc_suremis_nc.exe
fic_sirane_nc=sirane_PM10.2011010100_2011123123.nc
fic_sur_raster_nc=suremis_PM10.test.nc
syntaxe="${calc_suremis_nc_exe} ${sur_nx} ${sur_ny} ${xmin_m} ${ymin_m} ${sur_dx} ${fic_sirane_nc} ${fic_sur_raster_nc}"
echo ${syntaxe}
${syntaxe}  || \
  { echo "$0: ERREUR du calcul SUREMIS:" ; echo ${syntaxe} ; exit 1 ; } 

