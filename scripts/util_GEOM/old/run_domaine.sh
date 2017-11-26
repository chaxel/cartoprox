#!/bin/ksh
#
# lancement du run
#MLN 2009/09/02
#############################################
ChemDisk=/mnt/mod2
ChemProg=$ChemDisk/appli/CARTOPROX/Util
. $ChemProg/include_path.sh

if test $# -eq 1
then
 listmonth=$1
else
 listmonth="1 2 3 4 5 6 7 8 9 10 11 12 "
fi
if test $# -eq 0
then
 listmonth=2008
fi

YEAR=2008

POL=NOX # PM


if [ ! -f $ChemDataSrc/$ficgrid ]
 then
 echo fichier inexistant !! $ChemDataSrc/$ficgrid
 echo bye bye
 exit
fi
listgrid=`awk '{ if(NR>1) {printf("%s\n",$1)}  }' $ChemDataSrc/$ficgrid`

listgrid="AE44 AE45 AE46 AE47" # AE42 AE43 AF42"
listgrid="AE42  AF42" # AE43 "
listgrid="AE44 AE45 AE46 AE47 AE43 AE42 AF42"
listgrid="AE44 AE45 AE46 AE47 AE42 AE43 AF42"

echo $listgrid

for grid in $listgrid
 do

echo "************" $grid : debut du traitement `date` >> $ficlog

. $ChemProg/include_path.sh
. $ChemProg/minigrille.sh $grid
. $ChemProg/minigeom.sh $grid
. $ChemProg/extractfondmet.sh $grid

#..............

ficpress=2008_Press.txt

#..creation fichier donnees.dat
do_dat=1
do_press=1
#..
do_run=1
#
grille=0 #1

vsirane=SRC

for month in $listmonth
do
############
  cd $ChemProg
  . ./aaaamm_bi.sh
  . ./include_path.sh
#
#...recup liste des fichiers recepteurs
 listficrecept=`cat $ChemRes/listrecept.txt `

#...caracteristique du mini domaine
#. $ChemProg/minigrille.sh $grid

#...on veut les iter (class=0) et non les evol
# pour evol et stat : Classout=1 EVOL=0 CALCSTAT=1
# pour iter et stat : Classout=0 EVOL=0 CALCSTAT=1
Classout=0
EVOL=0
CALCSTAT=1
FORMATSURF=0
Ncinf=1
tauxno2=0.15
case $POL in
 NOX ) TYPEPOL=1 ;;
 PM  ) TYPEPOL=2 ;;
esac


if [ ! -f $ChemInput/meteo_${YEAR}.dat ]
then
 echo PB avec meteo
 echo $ChemInput/meteo_${YEAR}.dat
 exit
else
 echo ok pour fichier meteo utilise :  $ChemInput/meteo_${YEAR}.dat 
fi

echo "======== debut et fin de run : "
 echo nrs=$nrs
 echo nre=$nre
 echo run=$run

if test $do_dat -eq 1
then
 if test $do_press -eq 1
 then
   echo " calcul de la pression moyenne "
   Pmoy=`$ChemProg/calcPmoy.sh $ChemInput/$ficpress $nrs $nre `
   echo Pmoy = $Pmoy
   if test "$Pmoy" == ""
   then
     echo PB PMOY VIDE
     exit
   fi
 fi #press

n=`expr $nre - $nrs`
DT=`expr $n + 1`
echo nombre d echeances calculees : $NDT

sed -e "s/YEAR/$YEAR/g" \
    -e "s/_GRID_/$grid/g" \
    -e "s/NDT/$NDT/g" \
    -e "s/NSTART/$nrs/g" \
    -e "s/NSTOP/$nre/g" \
    -e "s/ALTMESURE/$ALTMESURE/g"  \
    -e "s/RUGMESURE/$RUGMESURE/g"  \
    -e "s/DEPMESURE/$DEPMESURE/g"  \
    -e "s/CALCSTAT/$CALCSTAT/g" \
    -e "s/_TYPEPOL_/$TYPEPOL/g" \
    -e "s/EVOL/$EVOL/g" \
    -e "s/output/$output/g" \
    -e "s/Classout/$Classout/g" \
    -e "s/Rinterp/$Rinterp/g" \
    -e "s/Xout/$Xout/g" \
    -e "s/Yout/$Yout/g" \
    -e "s/Xmin/$Xmin/g" \
    -e "s/Xmax/$Xmax/g" \
    -e "s/Ymin/$Ymin/g" \
    -e "s/Ymax/$Ymax/g" \
    -e "s/Xcnpt/$Xcnpt/g" \
    -e "s/Ycnpt/$Ycnpt/g" \
    -e "s/Xcmin/$Xcmin/g" \
    -e "s/Xcmax/$Xcmax/g" \
    -e "s/Ycmin/$Ycmin/g" \
    -e "s/Ycmax/$Ycmax/g" \
    -e "s/Ncinf/$Ncinf/g" \
    -e "s/press.par/$Pmoy/g" \
    -e "s/_REPRES_/$REPRES/g" \
    -e "s/_REPSURF_/$REPSURF/g" \
    -e "s/_LATITUDE_/$LATGRID/g" \
    -e "s/_RUGQART_/$RUGGRIDQUART/g" \
    -e "s/_DEPLA_/$DEPGRID/g" \
    -e "s/_HMOY_/$HMOYGRID/g" \
    -e "s/_RUGBATI_/$RUGGRID/g" \
    -e "s/_TAUXNO2_/$tauxno2/g" \
    -e "s/_FORMATSURF_/$FORMATSURF/g" \
    $ChemProg/donnees_v1.16_mini.par > $ChemGrid/donnees_${grid}_$run.dat

echo fichier cree : $ChemGrid/donnees_${grid}_${run}.dat
fi #do_dat
#
if test $do_run -eq 1
then
#
#... rempli GEOM-MINi et EMIS-MINI en fonction du grid
#
  echo supprime les fichiers de EMIS-MIN et GEOM-MINI
  rm -f $ChemSir/EMIS-MINI/* $ChemSir/GEOM-MINI/*
#
  echo cree les liens depuis $ChemGrid
  ln -s $ChemGrid/emis_noeuds_$grid.txt $ChemSir/EMIS-MINI/emis-mininoeuds.dat
  ln -s $ChemGrid/emis_rues_$grid.txt $ChemSir/EMIS-MINI/emis-minirues.dat

  for fic in noeuds_$grid.txt rues_$grid.txt   recepts_$grid.txt  
  do
      ln -s $ChemGrid/$fic $ChemSir/GEOM-MINI/.
  done
  ln -s $ChemData/GEOM/sources-ponct.dat $ChemSir/GEOM-MINI/.
#
#A FAIRE
#  cp $ChemData/INPUT/fond_2008.dat $ChemInput/minifond_2008.dat #a remplacer
# ln -s $ChemGrid/minifond_2008.dat $ChemSir/GEOM-MINI/.
#
#...creation repertoire temporaire
  ChemTMP=$ChemTMP/${grid}_m${run}.$$
  mkdir -p $ChemTMP
  if [ ! -d $ChemTMP ] ; then echo "*** CANNOT MOVE TO DIR. $ChemTMP" ; exit 1 ; fi
  cd $ChemTMP
  echo "run dans $ChemTMP"
  cp -r $ChemSir/* $ChemTMP
#
#..compilation sirane dans le repertoire temporaire
  sed -e "s/_EXECTMP_/sirane_$grid.$$/g" $ChemTMP/Makefile.tmp > $ChemTMP/Makefile
  echo
   make clean #menage des sources
   echo "Compilation of all programs..."
   make linux #compilation
   $ChemProg/verifok.sh $?
#...
#...liens dans le repertoire temporaire
   ln -sf $ChemInput  $ChemTMP/INPUT

   ln -s $ChemGrid/donnees_${grid}_${run}.dat $ChemTMP/donnees.dat
#
#...run
  cpt=0
  for ficrecpt in $listficrecept
  do
  cpt=`expr $cpt + 1`
      ln -sf $ChemGrid/$ficrecpt $ChemTMP/GEOM-MINI/recepts_$grid.txt
      $ChemTMP/sirane_$grid.$$  donnees.dat
#...menage
      mv $ChemTMP/$REPRES $ChemRes/RESULTAT_${grid}_${run}_$cpt
      if test $FORMATSURF -eq 1 ; then
        mv $ChemTMP/$REPSURF $ChemRes/SURFER_${grid}_${run}_$cpt
      fi
  done
  case $cpt in
    1 ) mv $ChemRes/RESULTAT_${grid}_${run}_$cpt $ChemRes/RESULTAT_${grid}_${run}
        if test $FORMATSURF -eq 1 ; then
           mv $ChemRes/SURFER_${grid}_${run}_$cpt $ChemRes/SURFER_${grid}_${run}
        fi
        ;;
    2 ) mkdir -p $ChemRes/RESULTAT_${grid}_${run}
        for POL in NO NO2 O3 NOx
        do
          ficstat=stat-recept-$POL.dat
          cat $ChemRes/RESULTAT_${grid}_${run}_1/$ficstat $ChemRes/RESULTAT_${grid}_${run}_2/$ficstat > $ChemRes/RESULTAT_${grid}_${run}/$ficstat
        done
          cat $ChemGrid/recepts_AE44.txt.1 $ChemGrid/recepts_AE44.txt.2 > $ChemGrid/recepts_AE44.txt
        if test $FORMATSURF -eq 1 ; then
           echo !!!!!!!! attention SURFER non conservé quand 2 fichiers recepteurs !!! >> $ficlog
        fi
        ;;
  esac
#
   cd $ChemWork
#   echo suppression repertoire tmp $ChemTMP
#   \rm -rf $ChemTMP
###
fi #do_run
$ChemProg/verifok.sh $? fin_de_run$grid
#
# verif presence fichier stat
  if [ ! -s $ChemRes/SURFER_${grid}_$run/stat-recept-NO2.dat ]
  then
    echo "!!!!!!!!!!!!!!! attention pas de fichier stat !!!!!!! " >> $ficlog
  fi
#
echo $grid ${run} : fin du traitement `date`
echo $grid ${run} : fin du traitement `date` >> $ficlog
                                                                                                                           
done # month

done #grid
