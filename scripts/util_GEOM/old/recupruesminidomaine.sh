#!/bin/ksh
#
# reconstitution output sirane cartoprox
# rues minidomaine quand zone tampon
# FT 2009/10/06
##########################################

if test $# -eq 1
then
 listmonth=$1
else
 listmonth="1 2 3 4 5 6 7 8 9 10 11 12 "
fi

YEAR=2008
month=$listmonth

type=statrue
#type=recept

POL=NO2 # PM

ficin=stat-rue-NO2.dat 
nrdeb=1
echo "Id	X	Y	Cmoy	Cmax	Hr_C>200	Jr_C>200	C98.0	C50.0" > $ChemWork/entete.tmp
#...
echo
echo
echo regroupement de tous les fichiers $ficin
echo
echo

 run=$YEAR
#.............
#if [ ! -f $ChemDataSrc/$ficgrid ]
# then
#echo fichier inexistant !! $ChemDataSrc/$ficgrid
#echo bye bye
#exit
#fi
#listgrid=`awk '{ if(NR>1) {printf("%s\n",$1)}  }' $ChemDataSrc/$ficgrid`

listgrid="AE44 AE45 AE46 AE47"
#.............
rm -f $ChemWork/recup.tmp
for grid in $listgrid
do
 . ./aaaamm_bi.sh
 . $ChemProg/include_path.sh
  echo $ChemRes/$RESRUN/$ficin


# recup la liste des rues du minidom
 listrueminidom=`awk '{ if($7 ~ minidom) {print ($1)} }' minidom=$grid $ChemGrid/rues_$grid.descr.txt `
 if [ -d $ChemRes/${RESRUN}_1 ] ; then
    ChemIn=$ChemRes/${RESRUN}_1
 else
    ChemIn=$ChemRes/${RESRUN}
 fi
 echo numrue $listrueminidom
 for rue in $listrueminidom
 do
  awk '{ if(NR>nrdeb && $1==rue ) print($0) }' rue=$rue nrdeb=$nrdeb $ChemIn/$ficin >> $ChemWork/recup.tmp
 done #rue
 # tout awk '{ if(NR>nrdeb) print($0) }' nrdeb=$nrdeb $ChemRes/$RESRUN/$ficin >> $ChemWork/recup.tmp
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

