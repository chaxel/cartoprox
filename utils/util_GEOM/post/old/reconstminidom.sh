#!/bin/ksh
#
# reconstitue 1 seul fichier qd split car trop de recepteurs
#
# FT 2009/09/17
################################################################
listgrid="AE45 AE44 AE47 AE43"
#listgrid=$1
#type=$2
#run=$3
type=recept
run=2008
#......
ChemResSrc=/data/CARTOPROX/LYON_VALENCE

for grid in $listgrid
do
 ChemRes=$ChemResSrc/${grid}_5km
 mkdir -p $ChemRes $ChemRes/RESULTAT_${grid}_${run}


  for POL in NO NO2 O3 NOx
  do
    ficstat=stat-$type-$POL.dat
   case $type in
    recept ) cat $ChemRes/RESULTAT_${grid}_${run}_1/$ficstat $ChemRes/RESULTAT_${grid}_${run}_2/$ficstat > $ChemRes/RESULTAT_${grid}_${run}/$ficstat ;;
    rue    ) cp $ChemRes/RESULTAT_${grid}_${run}_1/$ficstat $ChemRes/RESULTAT_${grid}_${run}/$ficstat ;;
   esac


    echo fichier cree : $ChemRes/RESULTAT_${grid}_${run}/$ficstat


  done #POL 
done #grid
