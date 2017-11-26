#!/bin/ksh
#
# Calcul du baricentre d'un fichier de recepteur
#################################################

fic_recept=$1
coord=$2

ix=4
iy=5

x=`gawk 'BEGIN { avg =  0.00 };{ avg += $'${ix}';n+=1 }; END { print avg/n }' ${fic_recept}`
y=`gawk 'BEGIN { avg =  0.00 };{ avg += $'${iy}';n+=1 }; END { print avg/n }' ${fic_recept}`

case ${coord} in
x|X)echo ${x};;
y|Y)echo ${y};;
*)echo ${x} ${y};;
esac
