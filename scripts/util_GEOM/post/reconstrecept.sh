#!/bin/ksh
#
# reconstitution output sirane cartoprox
#
# FT 2009/09/11
##########################################
ChemDisk=/mnt/mod2
ChemProg=$ChemDisk/appli/CARTOPROX/Util
. $ChemProg/include_path.sh

if test $# -eq 1
then
 listmonth=$1
else
 listmonth="1 2 3 4 5 6 7 8 9 10 11 12 "
 listmonth=2008
fi

YEAR=2008
month=$listmonth

type=recept

POL=NO2 # PM
POL=NOx

  ficin=stat-recept-$POL.dat 
  nrdeb=1
  echo "Id	X	Y	Cmoy	Cmax	Hr_C>200	Jr_C>200	C98.0	C50.0" > $ChemWork/entete.tmp
#...
echo
echo
echo regroupement de tous les fichiers $ficin
echo
echo
#.............
if [ ! -f $ChemDataSrc/$ficgrid ]
 then
 echo fichier inexistant !! $ChemDataSrc/$ficgrid
 echo bye bye
 exit
fi
listgrid=`awk '{ if(NR>1) {printf("%s\n",$1)}  }' $ChemDataSrc/$ficgrid`

listgrid="AE42 AE43 AE44 AE45 AE46 AE47 AF42"
#listgrid="AE44 AE45 AE46 AE47" 
#.............
rm -f $ChemWork/recup.tmp
for grid in $listgrid
do
 . $ChemProg/aaaamm_bi.sh
 . $ChemProg/include_path.sh
  echo $ChemRes/$RESRUN/$ficin
 awk '{ if(NR>nrdeb) print($0) }' nrdeb=$nrdeb $ChemRes/$RESRUN/$ficin >> $ChemWork/recup.tmp
 echo ok pour $ficin `awk 'END {print NR}' $ChemWork/recup.tmp`
done #grid
 
 ficout=$ficin.txt
 case $type in
  bln ) mv $ChemWork/recup.tmp $ChemWork/$ficout ;;
  *   ) cat $ChemWork/entete.tmp $ChemWork/recup.tmp > $ChemWork/$ficout ;;
 esac

 echo "===="
 echo fichier cree : $ChemWork/$ficout
 echo "===="
 echo ok pour $ficin `awk 'END {print NR}' $ChemWork/$ficout`

