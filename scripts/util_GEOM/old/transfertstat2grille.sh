#!/bin/ksh
# transfert de fichiers...
# FT 2008/10/21
#############################
ChemData=/appli/CARTOPROX
YEAR=2008
DirSrc=$ChemData/m2008_ct_NAgrille
DirDest=$ChemData/m2008_ct_NA

listmonth="01 02 03 04 05 06 07 08 09 10 11 12"

#...menu
dostat=0   # pour transfert des fichiers stat (resultat) 
dosurf=1   # pour transfert des répertoires SURFER_(grille).
dormsurf=0  # pour rm des répertoires SURFER_...
dormres=0   # pour rm des répertoires  RESULTAT_...

#............................
if test $dostat -eq 1
then
for month in $listmonth
do
 cd $DirSrc/RESULTAT_${YEAR}$month
 listfic=`ls  *stat* `
 for fic in $listfic 
 do
 echo fic transfere :  RESULTAT_${YEAR}$month $fic
 if [ -f $DirDest/RESULTAT_${YEAR}$month/$fic ]
  then
   echo attention le fichier existe deja !!! risque d ecrasement 
   sleep 2 s
 fi
 cp $DirSrc/RESULTAT_${YEAR}$month/$fic $DirDest/RESULTAT_${YEAR}$month/.
 done
done
fi

#......................
if test $dormres -eq 1
then
for month in $listmonth
 do
  echo je vais effacer $DirSrc/RESULTAT_${YEAR}$month
  rm -rf $DirSrc/RESULTAT_${YEAR}$month
 done
fi

#.................
if test $dosurf -eq 1
then
for month in $listmonth
do
 cd $DirSrc/SURFER_${YEAR}$month
 listfic=`ls *.grd `
 
 for fic in $listfic 
 do
 echo fic transfere :  SURFER_${YEAR}$month $fic
 cp $DirSrc/SURFER_${YEAR}$month/$fic $DirDest/SURFER_${YEAR}$month/.
 done
done
fi

#...
if test $dormsurf -eq 1
then
 for month in $listmonth
 do
 echo je vais effacer $DirSrc/SURFER_${YEAR}$month
 rm -rf $DirSrc/SURFER_${YEAR}$month
  done
fi
#...
