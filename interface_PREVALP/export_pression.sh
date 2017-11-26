#!/bin/sh
######################################################################################################
# INTERFACE PREVALP/SIRANE
# GIE Atmo Rhone-Alpes (juillet 2009)
######################################################################################################
# Utilisation : Calcul de la pression a partir du fichier 
#                comparaison_CARTOPROX_mto_wrf_bourgoin.csv

case $# in
1)
fichier_mto_wrf=$1
gawk -F '(;)' '(FNR > 1) {print $4";"$7 } ' ${fichier_mto_wrf} \
| sed 's/_/;/g' | sed 's/:00:00/:00/g'  | sed 's/-/;/g' \
| gawk -F '(;)' '{print $3"/"$2"/"$1" "$4";"$5 }'  | sed 's/;/\t/g'
;;
2)
fichier_mto=$1
pression_defaut=$2
gawk '{print $1" "$2"\t'${pression_defaut}'" } ' ${fichier_mto} 
;;
*)
echo "Syntaxe : $0 fichier_mto_wrf [valeur_defaut]"
exit

;;
esac


