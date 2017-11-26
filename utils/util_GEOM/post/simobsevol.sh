#!/bin/ksh
#
# formatage sim/obs evolution temporelle
#
# FTr 2009/11/16
##############################################
YEAR=2008
ChemDisk=/mnt/mod2
ChemProg=$ChemDisk/appli/CARTOPROX/Util/post

station=ROUSSILLON
station=AZNI

POL=NO2 
POL=NOx

. $ChemProg/corresp_station_recept_mailleur.sh

echo extrait de grid=$grid idrecept=$idrecept

. $ChemProg/include_post.sh

 ficim=$ChemWork/evol-$station.dat # cree par iter2evol.sh
#
 case $station in
   AZNI ) stationmes=A7NI ;;
   *    ) stationmes=$station ;;
 esac
 ficmes=$ChemWork/${YEAR}_$stationmes.txt
 ficout=$ChemWork/evolsimobs_$station.$POL.txt
#
 case $POL in
   NO2 ) colsim=6
         colmes=4 ;;
   NOx ) colsim=4
         colmes=1000 ;;
  esac
#......
  awk '{ if(NR<=8784) printf("%s\n",$colsim) }' colsim=$colsim $ficim > $ChemWork/sim.tmp
  case $POL in
    NO2 ) awk '{ if(NR>1 && NR<=8785) { printf("%s\n",$colmes) } }' colmes=$colmes $ficmes > $ChemWork/mes.tmp ;;
    NOx ) awk '{ if(NR>1 && NR<=8785) { printf("%s %s\n",$3,$4) } }' colmes=$colmes $ficmes > $ChemWork/mes.tmp ;; # fera calcul ss R
  esac

  paste $ChemWork/sim.tmp $ChemWork/mes.tmp > $ficout
  echo fichier $ficout

#  echo "dataF <- read.table($ficout,header=FALSE,na.strings="-999")" > $ChemWork/inputR.tmp
#  echo "data <- na.omit(dataF)"  >> $ChemWork/inputR.tmp
#  echo fichier pour R : $ChemWork/inputR.tmp

