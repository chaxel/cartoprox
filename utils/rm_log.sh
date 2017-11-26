#!/bin/bash
# change les dates des fichiers evol 2011 en 2015
#############################################
year=2011
ChemData=/home/oper/data/CARTOPROX/res_V31_A7_${year}0101_${year}1231
ChemWork=$ChemData
listrep=`awk '{ if(NR>=1) {printf("%s\n",$1)}  }' ${ChemWork}/list_rep.txt` 
#listrep=632500_4912500 #644500_4984500    #650500_5044500

##############################################
for code_maille in $listrep
do
    echo -e "\\033[0;34m" "====================traite maille $code_maille=====================" "\\033[0;39m"
    ChemRes=$ChemWork/$code_maille
    if [ -f ${ChemRes}/sirane_${year}010100_${year}123123_pol1.log ] ; then 
      echo -e "\\033[1;32m" "-> efface les log" "\\033[0;39m" 
      ls $ChemRes/*.log
      rm $ChemRes/*.log
    else
      echo pas de fichier log
  fi
done # code_maille
date
###############################################


