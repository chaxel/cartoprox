#!/bin/ksh
# qqplot 
#
# FT 2009/04/07
########################################
ChemProg=/appli/SIRANE_LIMOGES/UTIL

vcadastre=adms_fbdg_rues
ChemData=/data/SIRANE_LIMOGES/m2007_c$vcadastre/simobs
ChemWork=/data/SIRANE_LIMOGES/m2007_c$vcadastre/simobs/qq

mkdir -p $ChemWork

run=2007all

listpol="NO2" # NOx"
listpol="O3 NOx"


for POL in $listpol
do
 case $POL in
   NO2 | NOx ) liststation="PRESIDIAL MADOUMIER" ;;
   O3        ) liststation="PRESIDIAL GARROS" ;;
 esac

for station in $liststation
do


ficin=$run.$station.simobs.$POL.R

 if [ ! -f $ChemData/$run.$station.simobs.R ] ; then
  echo il manque le fichier $ChemData/$run.$station.simobs.R
  echo "relancer dat2R.sh mes2R.sh puis boxsimobs2R.sh ou par6...."
  exit
 fi
 case $POL in
  NO2 ) awk '{ print($8,$15)}' $ChemData/$run.$station.simobs.R > $ChemWork/$ficin 
        ymax=200 ;;
  NOx ) awk '{ print($6,$17)}' $ChemData/$run.$station.simobs.R > $ChemWork/$ficin 
        ymax=800 ;;
  O3 ) awk '{ print($9,$16)}' $ChemData/$run.$station.simobs.R > $ChemWork/$ficin
        ymax=200 ;;
 esac

if [ ! -f $ChemWork/$ficin ] ; then
 echo il manque le fichier $ChemWork/$ficin
 exit
fi 

cd $ChemWork
ficout=$run.$station.qq.$POL
sed -e "s/_FICIN_/$ficin/g" \
    -e "s/_STATION_/$station/g" \
    -e "s/_FICOUT_/$ficout/g" \
    -e "s/_RUN_/$run/g" \
    -e "s/_POL_/$POL/g" \
    -e "s/_YMAX_/$ymax/g" \
    -e "s/_PLOTOUT_/$ficout/g" \
     $ChemProg/scriptRqq.SED > $ChemWork/plotqq.r
echo lance R
R CMD BATCH   plotqq.r
echo convertit le pdf en jpeg...
convert Rplots.pdf $ficout.jpeg

echo fichier cree : $ficout.jpeg

done #POL
done #station
