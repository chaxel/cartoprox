#!/bin/ksh
fic=$1
#:echo fichier teste si vide : $fic
nb=`awk 'END {print NR}' $fic`
echo nb=$nb
 if test $nb -le 1 ; then
  echo
  echo
  echo
  echo !!!!!!! attention fichier vide $fic
  echo
  echo
  echo
sleep 15 s
fi
