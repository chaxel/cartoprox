#!/bin/bash
# change les dates des fichiers evol 2011 en 2015
#############################################
year=2011
listpol="nox PM10 PM25"
ChemData=/home/oper/data/CARTOPROX/res_V31_A7_${year}0101_${year}1231
ChemWork=$ChemData
listrep=`awk '{ if(NR>=1) {printf("%s\n",$1)}  }' ${ChemWork}/list_rep.txt` 
#listrep=632500_4912500 #644500_4984500    #650500_5044500

##############################################

for code_maille in $listrep
do
  echo -e "\\033[0;34m" "====================traite maille $code_maille=====================" "\\033[0;39m"
  for pol in $listpol
  do
    ChemRes=$ChemWork/$code_maille
    if [ -f ${ChemRes}/sirane_${pol}.${year}010100_${year}123123.nc ] ; then 
      echo "-> efface  ${ChemRes}/sirane_${pol}.${year}010100_${year}123123.nc "  
#      ls ${ChemRes}/sirane_${pol}.${year}010100_${year}123123.nc
      rm ${ChemRes}/sirane_${pol}.${year}010100_${year}123123.nc
    else
      echo pas de fichier ${ChemRes}/sirane_${pol}.${year}010100_${year}123123.nc
    fi
  done # pol
done # code_maille
date
###############################################


