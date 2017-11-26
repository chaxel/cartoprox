#!/bin/ksh
#
# recupere evolution temporelle à partir des fichiers iter
#
# FTr 2009/11/16
############################################################
YEAR=2008
ChemDisk=/mnt/mod2
ChemProg=$ChemDisk/appli/CARTOPROX/Util/post

station=ROUSSILLON
station=AZNI

. $ChemProg/corresp_station_recept_mailleur.sh

echo extrait de grid=$grid idrecept=$idrecept

. $ChemProg/include_post.sh
ChemSim=$ChemSimSrc/${grid}_5km/RESULTAT_${grid}_$YEAR


rm -f $ChemWork/evol-$station.dat

iter=1

while test $iter -le 8784
do 
 ficsim=Iteration-recept-$iter.dat
 if [ ! -f $ChemSim/$ficsim ] 
 then 
   echo pas de fichier iter :  $ChemSim/$ficsim
   exit
 fi
 grep $station $ChemSim/$ficsim >> $ChemWork/evol-$station.dat
 echo $ficsim traite
 iter=`expr $iter + 1`
done #iter

echo fichier cree : $ChemWork/evol-$station.dat

