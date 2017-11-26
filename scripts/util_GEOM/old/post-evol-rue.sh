#!/bin/ksh
# extrait des mois simules des rues
# reconstitue evolution temporelle des rues
# MLNG 06/01/2009
###################################################"
YEAR=2008
vcadastre=t_PM

lab=m${YEAR}_c${vcadastre}
ChemDataSrc=/appli/CARTOPROX/$lab #/appli/SIRANE_GRENOBLE/$lab

ChemWork=$ChemDataSrc/evo_rues
mkdir -p $ChemWork
ChemRes=$ChemWork
ChemData=$ChemRes
ChemRue=$ChemWork

ficrue=Id_rue.txt
listrue=`awk '{ if(NR>1) {printf("%s\n",$1)}  }' $ChemRue/$ficrue`

echo liste rue = $listrue

############################################################ exctraction des mois ou periodes simulees
echo exctraction

#
#ChemProg=/appli/SIRANE_GRENOBLE/UTIL_SIRANE_GRENOBLE
#. $ChemProg/include_path_data.sh
#
#listperiode="1 2a 2b 2c 2d 2e 2f 3a 3b 4 5 6 7 8a 8b 9a 9b 10 11 12a 12b" #
listperiode="1 2 3 4 5 6 7 8 9 10 11 12"

for periode in $listperiode
do
#  case $periode in
#01 | 03 | 05 | 07 | 08 | 10 | 12 ) ficres=RESULTAT_2007${periode}01_2007${periode}31 ;;
#04 | 06 | 09 | 11 ) ficres=RESULTAT_2007${periode}01_2007${periode}30 ;;
#02 ) ficres=RESULTAT_2007${periode}01_2007${periode}28 ;;
#  esac
  
  month=$periode
  ChemProg=/appli/CARTOPROX/Util
  . $ChemProg/aaaamm_bi.sh # recup nrs nre run
  ChemDataRun=$ChemDataSrc/RESULTAT_$run
#  ChemDataRun=$ChemDataSrc/$ficres
 echo
 echo $ChemDataRun
 echo "campagne..... "$periode
 echo

  for rue in $listrue
  do
   #echo traitement de 
  
   ficout=$month.rue.$rue.dat
   if [ -f $ChemRes/$ficout ]
   then 
     rm -f $ChemRes/$ficout
   fi
   
   ficrue=Evol-rue-$rue.dat
      if [ ! -f $ChemDataRun/$ficrue ]
   then
    echo fichier inexistant !! $ChemDataRun/$ficrue
    echo bye bye
    exit
   fi
#   cat $ChemDataRun/$ficrue> $ChemWork/$ficout   
   awk '{ if(NR==1) {print($0)}
          if(NR>=nrs+1 && NR<=nre+1) {print($0) } }' nrs=$nrs nre=$nre $ChemDataRun/$ficrue > $ChemWork/$ficout
  
  echo fichier ecrit : $ChemWork/$ficout
  done #rue

done #periode

########################################################## reconstitue evolution temporelle annuelle des recepteurs
#listmonth="01 02a 02b 02c 02d 02e 02f 03a 03b 04 05 06 07 08a 08b 09a 09b 10 11 12a 12b" #
listmonth="1 2 3 4 5 6 7 8 9 10 11 12"
#periode="12"
echo
echo reconstitution
echo

for rue in $listrue
do
#ficout=${YEAR}${periode}.$site.dat
 for month in $listmonth
 do
# cat $ChemData/${YEAR}${month}.$site.dat >> $ChemWork/evo.$periode.$site.tmp >> $ChemWork/$ficout
 cat $ChemData/${month}.rue.$rue.dat >> $ChemWork/evo.$YEAR.rue.$rue.tmp #>> $ChemWork/$ficout
 rm -f $ChemData/${month}.rue.$rue.dat
# rm -f $ChemWork/evo.$YEAR.$site.tmp
 done #mois
done #rue

#####################################################supprime les entetes de colonnes sauf pour le premier mois   
for rue in $listrue
do
 rm -f $ChemWork/evo.$YEAR.rue.$rue.dat
 awk ' {if(NR==1) {print ($0) } 
       if($1 != "i") {print ($0) } 
       }' $ChemWork/evo.$YEAR.rue.$rue.tmp >> $ChemWork/evo.$YEAR.rue.$rue.dat 
echo fichier ecrit $ChemWork/evo.$YEAR.rue.$rue.dat
done #rue

rm $ChemWork/*tmp

############################################################### enlève les slash et les :00 pour enlevefond.sh 
echo formate pour enlevefond.sh
for rue in $listrue
do
  rm -f $ChemWork/evo.$YEAR.rue.$rue.txt
  sed "s/\///g" $ChemWork/evo.$YEAR.rue.$rue.dat >> $ChemWork/evo.$YEAR.rue.$rue.tmp
  sed "s/\:00//g" $ChemWork/evo.$YEAR.rue.$rue.tmp >> $ChemWork/evo.$YEAR.rue.$rue.txt
#  rm -f $ChemWork/evo.$YEAR.rue.$rue.dat
  echo fichier cree : $ChemWork/evo.$YEAR.rue.$rue.txt
done #rue





