#!/bin/bash
#
# TOP-SCRIPT pour CARTOPROX operationnel
# Atmo Rhone-Alpes 2010
#############################################

#Repertoires des scripts
#export cartoprox=/mnt/mod4/appli/CARTOPROX

#Domaine
#export cartoprox_domaine=region_ARN

#Scripts
source ${cartoprox}/include/common.inc
${cartoprox}/scripts/utils/header.sh
cd ${cartoprox}/scripts

########################
# MODE PREVIS
########################
fh=00
ndays=4 
case $# in
#0)today=`date  +%Y%m%d`;;
1)today=$1;;
2)
deb_j=$1
deb_h=00
fin_j=$2
fin_h=23
;;
4)
deb_j=$1
deb_h=$2
fin_j=$3
fin_h=$4
;;
*)
echo "Specifier date du jour au format AAAAMMJJ (-1=hier, 0=aujourd'hui) ?"
read today
;;
esac

#Date du run
case ${today} in
-2|-1|0)
today=`date -d "${today} days " +%Y%m%d`
ndays=4
deb_j=`date -d "${today} 1 days ago" +%Y%m%d`
fin_j=`date -d "${deb_j} ${ndays} days " +%Y%m%d`
deb_h=00
fin_h=00
;;
esac

echo today=${today}
echo deb_j=${deb_j}
echo fin_j=${fin_j}

if [ "${PROCS}" == "" ] ; then
  echo "PROCS?"
  read PROCS
fi

if [ "${mode}" == "" ] ; then
  echo "mode(sirane,cartoprox,stat)?"
  read mode
fi

#Temps de latence pour éviter que les traitements se chevauchent
sleep=10

#Preparation entrees PREVALP + EMISSIONS
#syntaxe="./lance_CARTOPROX_region.sh prep ${deb_j} ${deb_h} ${fin_j} ${fin_h}"
#echo -e "$ROUGE" ${syntaxe} "$NORMAL"
#sleep ${sleep}
#time ${syntaxe}

#Preparation entrees EMISSIONS (1 fois par an)
#syntaxe="./lance_CARTOPROX_region.sh emis ${deb_j} ${deb_h} ${fin_j} ${fin_h}"
#echo -e "$ROUGE" ${syntaxe} "$NORMAL"
#sleep ${sleep}
#time ${syntaxe}

case ${mode} in
sirane|cartoprox)liste_mode="${mode}_gaz ${mode}_aer";;
*)liste_mode="${mode}";;
esac

#Calcul CARTOPROX

iproc=1
while [ ${iproc} -le ${PROCS} ] ; do 
  for mode in ${liste_mode} ; do   
    syntaxe="./lance_CARTOPROX_region.sh ${mode} ${deb_j} ${deb_h} ${fin_j} ${fin_h}"
    echo -e "$ROUGE" "PROC ${iproc}/${PROCS} ${syntaxe}" "$NORMAL"    
    ${syntaxe} &
    sleep ${sleep}
    iproc=`expr ${iproc} \+ 1`
  done
done
wait

echo "Fin normale de $0 le `date`"
