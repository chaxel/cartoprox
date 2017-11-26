#!/bin/ksh
#
ChemDisk=/mnt/mod2
ChemProg=$ChemDisk/appli/CARTOPROX/Util/post
. $ChemProg/include_post.sh

liststa="ROUSSILLON1" # ROUSSILLON2 A7NI1 A7NI2 A7NI3 A7NI4"
liststa="A7NI1 A7NI2 A7NI3 A7NI4"
liststa="ROUSSILLON1 ROUSSILLON2"
liststa="ROUSSILLON ROUSSILLON1 ROUSSILLON2 ROUSSILLON3 ROUSSILLON4 AZNI"

YEAR=2008
POL=NO2
POL=NOx #NO2
domes=0

case POL in
 NO2 ) colsim=4
       colmes=4 ;;
 NO  ) colsim=4
       colmes=3 ;;
 NOx ) colsim=4 ;;
esac


for station in $liststa
do
 # . $ChemProg/corresp_station_recept.sh
  . $ChemProg/corresp_station_recept_mailleur.sh
  echo station=$station idrecept=$idrecept

  ficsim=stat-recept-$POL.dat
  ficmes=${YEAR}_$station.txt


  ln -sf $ChemMes/${ficmes} $ChemWork/.
  ChemSim=$ChemSimSrc/${grid}_5km
  ln -sf $ChemSim/RESULTAT_${grid}_$YEAR/$ficsim $ChemWork/.

#awk si let obs pour avoir fichier simobs
#  echo aaaammjj hh $POL > $ChemWork/sim.tmp 
#...n'a pas les evol temp  awk '{ if(NR>1) {printf("%s %s %s\n",$1,$2,$3) } }' ncol=$colsim $ChemWork/$ficsim >> $ChemWork/sim.tmp

  sim=`awk '{ if($1~id) { print($4) }  }' id=$idrecept col=$colsim $ChemWork/$ficsim`

  awk '{ if(NR==1) {print($0) }
         if($1~id) { print($0) }  }' id=$idrecept $ChemWork/$ficsim > $ChemWork/${idrecept}_$station.sim.txt
  echo $POL sim=$sim
#
#....
if test $domes -eq  1
then
  
  sed -e "s/_FIC_/$ficmes/g" $ChemProg/moyR.SED > $ChemWork/moy.r

  cd $ChemWork
  ficout=stat.$POL.txt
  R CMD BATCH moy.r  $ficout
  mv mean$POL.out mean$POL.$station.out
  mv mean.out mean.$station.out
  echo fichiers ecrits dans $ChemWork : mean$POL.$station.out mean.$station.out
  head mean$POL.$station.out mean.$station.out
fi
done #station
 
