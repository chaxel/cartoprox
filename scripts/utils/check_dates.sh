#!/bin/ksh
#
# Verifie les dates
#
## INCLUDE ###########################################
localdir=`pwd`


deb_j=$1
deb_h=$2
fin_j=$3
fin_h=$4

if [ "${deb_j}" == "" ] ; then
  echo "ERREUR date deb_j="${deb_j}
  exit 1
fi

if [ "${fin_j}" == "" ] ; then
  echo "ERREUR date fin_j="${fin_j}
  exit 1
fi


if [ "${deb_h}" == "" ] ; then
  echo "ERREUR heure deb_h="${deb_h}
  exit 1
fi


if [ "${fin_h}" == "" ] ; then
  echo "ERREUR heure fin_h="${fin_h}
  exit 1
fi

if [ ${deb_j}${deb_h} -ge ${fin_j}${fin_h} ] ; then
  echo "ERREUR debut ${deb_j}${deb_h} > fin ${fin_j}${fin_h}"
  exit 1
fi

echo "Dates OK"
