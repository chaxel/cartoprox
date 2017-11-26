#!/bin/ksh
ChemDisk=/mnt/mod2

ChemProg=$ChemDisk/appli/CARTOPROX/Util/post

#. $ChemProg/include_post.sh
ChemSimSrc=$ChemDisk/data/CARTOPROX/LYON_VALENCE
ChemWork=$ChemDisk/data/CARTOPROX/LYON_VALENCE/workpost

POL=NO2
dx=5km
ficout=rues_agreg.txt
YEAR=2008

listgrd="AE44_5km  AE45_5km  AE46_5km  AE47_5km"
listgrd="AE42 AE43 AE44 AE45 AE46 AE47 AF42"

  rm -f $ChemWork/recup.tmp $ChemWork/$ficout.tmp
  echo "Id  X       Y       Cmoy    Cmax    Hr_C>200        Jr_C>200        C98.0   C50.0" > $ChemWork/entete.tmp

  for grid in $listgrd
  do
 
ChemRun=$ChemSimSrc/${grid}_$dx/RESULTAT_${grid}_$YEAR
#..recup liste des rues de la grille (pour ne pas prendre les rues hors domaine avec concentration vide)
      ficrue=$ChemSimSrc/${grid}_$dx/rues_$grid.descr.txt
      listrue=`awk '{ if($7~grid) { print($1) }  }' grid=$grid $ficrue`
      echo rue de la grille $grid : $listrue

      ficin=stat-rue-$POL.dat
      nrdeb=1

      if [ ! -f $ChemRun/$ficin ] 
      then 
        ln -sf ${ChemRun}_1/$ficin $ChemRun/$ficin  
      fi

      rm $ChemWork/recup.tmp
      for rue in $listrue 
      do
        echo extrait rue $rue # $ChemRun $ficin
         awk '{ if($1==rue) print($0) }' rue=$rue $ChemRun/$ficin >> $ChemWork/recup.tmp
#         echo ok pour $ficin `awk 'END {print NR}' $ChemWork/recup.tmp`
      done #rue
      cat $ChemWork/recup.tmp >> $ChemWork/$ficout.tmp
   done #grid

   cat $ChemWork/entete.tmp $ChemWork/$ficout.tmp > $ChemWork/$ficout

   nb=`awk 'END {print NR}' $ChemWork/$ficout`
   echo nb rues : $nb
   echo fichier cree : $ChemWork/$ficout
   if test $nb -le 1 ; then
     echo
     echo
     echo
     echo !!!!!!! attention fichier vide !!! $ChemWork/$ficout
     echo
     echo
     echo
   fi
