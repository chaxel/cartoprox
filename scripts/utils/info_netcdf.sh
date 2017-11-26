#!/bin/sh -f
#   *********************************************************************
#   *		 SCRIPT POUR RAPPATRIER LES DONNEES GIERSA     		*
#   *				  JANUARY 2004				*
#   *          AUTHOR: E. CHAXEL					*
#   *			LABORATOIRE DES ECOULEMENTS GEOPHYSIQUES	*
#   *			ET INDUSTRIELS  				*
#   *			BP49 - 38041 GRENOBLE				*
#   *			CHAXEL@HMG.INPG.FR                              *
#   *********************************************************************

localdir=`pwd`

case $# in
0)
echo "Syntaxe $0 netcdf [-t,-v]"
exit
;;
1)
fic_netcdf=$1
opt="-t";; #defaut
2)
fic_netcdf=$1
opt=$2;; #defaut
*)
echo "Syntaxe $0 netcdf [-t,-v]"
exit
;;
esac


if [ ! -f ${fic_netcdf} ] ; then
  echo "Fichier n existe pas :"${fic_netcdf}
  exit
fi

ntimes=``
var=``

case ${opt} in
-t)ncdump -h ${fic_netcdf} | grep "currently" | gawk -F '( // |  )' '{print $2}' | sed 's#(##g' | sed 's# currently)##g';;
-v)ncdump -h ${fic_netcdf} | sed 's/(/ /g' | gawk '{print $2}';;
*)echo "Option inconnue: ${opt}";; 
esac



