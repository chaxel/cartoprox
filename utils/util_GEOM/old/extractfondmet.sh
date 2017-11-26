#!/bin/ksh
#
# FTr
########################################################
grid=$1
ChemDisk=/mnt/mod2
ChemProg=$ChemDisk/appli/CARTOPROX/Util
. $ChemProg/include_path.sh

 echo "===================================================="
 echo "====== extraction WRF + Chimere pour $grid ========="
 echo "===================================================="

LATGRID=`awk '{ if($1==grid) {printf("%s\n",$4)}  }' grid=$grid $ChemDataSrc/$ficgrid`
LONGRID=`awk '{ if($1==grid) {printf("%s\n",$5)}  }' grid=$grid $ChemDataSrc/$ficgrid`

echo grid=$grid LATGRID=$LATGRID LONGRID=$LONGRID

#...n extrait que si fichiers non present
if [ ! -f  $ChemGrid/fondX-$grid.dat ]
then
  ssh -i /home/troude/.ssh/cle_ppa oper@$ipWRF "cd /appli/PREVALP_200611/interfaces/SIRANE; ./export_vers_SIRANE_CARTOPROX.sh $LATGRID $LONGRID $grid $ChemGrid"
  ssh -i /home/troude/.ssh/cle_ppa oper@$ipWRF "cd /appli/PREVALP_200611/interfaces/SIRANE; ./send2mod2.sh evol-meteo-$grid.dat $ChemGrid"
  ssh -i /home/troude/.ssh/cle_ppa oper@$ipWRF "cd /appli/PREVALP_200611/interfaces/SIRANE; ./send2mod2.sh conc_fond-$grid.txt $ChemGrid"

#..duplique échéance 1h en 0h du 1er janvier
  awk '{ if(NR==1) printf("%s 00:00 %s %s %s\n",$2,$4,$5,$6)} ' $ChemGrid/conc_fond-$grid.txt > $ChemGrid/fondX-$grid.dat
  awk '{ printf("%s %s %s %s %s\n",$2,$3,$4,$5,$6)} ' $ChemGrid/conc_fond-$grid.txt >> $ChemGrid/fondX-$grid.dat
  rm -f $ChemGrid/conc_fond-$grid.txt 
#
precip=0

  awk '{ printf("%s %s %s %s %s %s %s\n",$2,$3,$4,$5,$7,$11,precip)} ' precip=$precip $ChemGrid/evol-meteo-$grid.dat > $ChemGrid/meteoWRF-$grid.dat
  rm -f $ChemGrid/evol-meteo-$grid.dat
#
else
 echo  "==== pas de formatage car fichiers déjà présents "
fi
  ln -fs $ChemGrid/fondX-$grid.dat  $ChemInput/minifond_$YEAR.dat
  ln -fs $ChemGrid/meteoWRF-$grid.dat $ChemInput/meteo_$YEAR.dat


