#!/bin/ksh
#
# FT 2009/09/02
##############################
minidom=$1
grid=$1

# attention chemwork specifique
ChemWork=$ChemWorkRecept
#
#ficreceptsrcok=/data/CARTOPROX/recepteurs/r_lyon_valence.txt.ok
ficreceptsrcok=$ficreceptsrc
ficreceptRA=r_lyon_valence.txt.ok
ln -fs $ficreceptsrcok $ChemWork/$ficreceptRA

#formattab=0 # a ne passer a 1 que si on change de domaine d'etude...(verifier que le fichier $ficreceptsrcok soit ok...
#ficreceptsrcbrut=/data/CARTOPROX/recepteurs/r_lyon_valence.txt
#...modifie pour fortran .  a verifier.......
#if test $formattab -eq 1
#then
# sed -e "s/\"//g" \
#     -e "s/t          /t_/g" \
#     -e "s/t       /t_/g" \
#     -e "s/       /_/g" \
#     -e "s/     //g" \
#     -e "s/t_  /t_/g" \
#     -e "s/t_ /t_/g" \
#     -e "s/_  /_/g" \
#     -e "s/_       /_/g" \
#     $ChemWork/$ficreceptsrc > $ChemWork/$ficreceptsrcok.tmp
#awk '{ printf("%s      %s %s %s\n",$1,$2,$3,$4) }' $ChemWork/$ficreceptsrcok.tmp > $ChemWork/$ficreceptsrcok
#fi

nb=`awk 'END {print NR-1}' $ficreceptsrcok`


 echo "==============================================================="
 echo "====== extraction  recepteurs pour $minidom ========="
 echo "==============================================================="
 
 frecept=recepts_${minidom}.txt
 
 echo $ficreceptRA > $ChemWork/input.tmp
 echo $nb >> $ChemWork/input.tmp
 echo $minidom >> $ChemWork/input.tmp
 echo $frecept.tmp >> $ChemWork/input.tmp
 
 echo fichier input : $ChemWork/input.tmp
 cat $ChemWork/input.tmp
 echo ls chemwork
 ls $ChemWork

 sleep 5 s
 
 cd $ChemProg
 ${F90} extractminirecept.f90 -o extractminirecept
 
 cd ${ChemWork}
 ${ChemProg}/extractminirecept < ${ChemWork}/input.tmp

 nb=`awk 'END {print NR}' ${ChemWork}/$frecept.tmp`
 echo "nb pour ce domaine : ${nb}"
#
#...test si nbre de recept pas trop grand
 nbseuil=7420
 nbbig=`expr $nbseuil \* 2`
 if test $nb -gt $nbbig
 then
   echo "!!!!!! attention nb recepteurs vraiment trop grand = " $nb >> $ficlog
   echo stop >> $ficlog
   listficrecept=""
   exit 1
 fi
 if test $nb -gt $nbseuil
   then
     nb1=$nbseuil
     nb2=`expr $nb - $nbseuil`
     listnb=`echo $nb1 $nb2`
     echo $nb1 > $ChemWork/nb.1.tmp
     head -n 5 $ChemWork/$frecept.tmp
     awk '{ if(NR<=nbseuil) print($0) }' nbseuil=$nbseuil $ChemWork/$frecept.tmp > $ChemWork/$frecept.1.tmp
     cat $ChemWork/nb.1.tmp  $ChemWork/$frecept.1.tmp > $ChemRes/$frecept.1

     echo $nb2 > $ChemWork/nb.2.tmp
     awk '{ if(NR>nbseuil) print($0) }' nbseuil=$nbseuil $ChemWork/$frecept.tmp > $ChemWork/$frecept.2.tmp
     cat $ChemWork/nb.2.tmp $ChemWork/$frecept.2.tmp > $ChemRes/$frecept.2

     $ChemProg/funcvide.sh $ChemRes/$frecept.1
     $ChemProg/funcvide.sh $ChemRes/$frecept.2
   listficrecept="$frecept.1 $frecept.2"

  fi 
  if test $nb -le $nbseuil
  then 
   echo $nb > $ChemWork/nb.tmp
   cat $ChemWork/nb.tmp  $ChemWork/$frecept.tmp > $ChemRes/$frecept
#   head $ChemRes/$frecept
#
   fic=$ChemRes/$frecept
   $ChemProg/funcvide.sh $fic
   listficrecept="$frecept"
  fi

   echo fichier recept  : $ChemRes/listrecept.txt
   echo $listficrecept > $ChemRes/listrecept.txt
   
rm -f $ChemWork/*tmp

