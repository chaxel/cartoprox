#!/bin/ksh

case $# in
2)
iter=$1
annee=$2
format=std
;;
3)
iter=$1
annee=$2
format=$3
;;
*)
echo "Usage $0 iter annee"
exit 1
;;
esac

iter=`expr ${iter} \- 1`

hour=`echo ${iter} | gawk '{ print $1-int($1/24)*24 }'`
julian=`echo ${iter} | gawk '{ print int($1/24)+1 }'`

#echo $julian $annee

if [ ${hour} -lt 10 ] ; then
hhour="0${hour}"
else
hhour="${hour}"
fi

case ${format} in
yyyymmddhh)
ddate=`date -d "${annee}-01-01 +$((${julian}-1))days" +%Y%m%d`
echo "${ddate}${hhour}"
;;
yyyymmdd)
ddate=`date -d "${annee}-01-01 +$((${julian}-1))days" +%Y%m%d`
echo "${ddate}"
;;
std)
ddate=`date -d "${annee}-01-01 +$((${julian}-1))days" +%d/%m/%Y`
echo "${ddate} ${hhour}:00"
;;
*)
echo "ERREUR_FORMAT"
exit 1
;;
esac
