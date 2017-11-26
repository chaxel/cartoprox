#!/bin/ksh

date_ref=
heure_ref=

case $# in
1)
date=$1
heure=00
;;
2)
date=$1
heure=$2
;;
4)
date_ref=$1
heure_ref=$2
date=$3
heure=$4
;;
*)
echo "Usage $0 date (heure)"
exit 1
;;
esac


if [ "${date_ref}" == "" ] && [ "${heure_ref}" == "" ] ; then
  j_ref=1 #1er janvier
  h_ref=0
else
 j_ref=`date -d "${date_ref}" +%j` # calcule la date
 h_ref=`expr ${heure_ref}`
fi
#echo ${date_ref}
#echo ${j_ref} ${h_ref}
#echo ${date}
j_=`date -d "${date}" +%j`
h_=${heure}
echo ${j_ref} ${j_} ${h_ref} ${h_}  | awk '{ print ( $2 - $1 ) * 24 + $4 - $3 + 1 }'

