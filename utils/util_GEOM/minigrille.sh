#!/bin/ksh
#Pour un domaine
#Calcul des coordonnee de la grille de calcul a partir du centre du domaine
#
#MLNG 03/09/2009
#####################################################
code_domaine=$1
xl2c=$2
yl2c=$3
grillesizex=$4
grillesizey=$5
                                                                                                                         
echo "================================================="
echo "====== creation grille de calcul pour ${code_domaine} ========="
echo "================================================="
                                                                                                                           
#extrait les coordonnees de la grille 
ficres=${ChemRes}/grille_${code_domaine}.sh

#ficsrc=Grille3km_LV_test.txt
#awk ' { if($1==code_domaine) {printf("%s %s\n",$2,$3)} }' code_domaine=${code_domaine} ${ficgrid} > ${ficres}.tmp
echo ${xl2c} ${yl2c}

if [ -s ${ficres}.tmp ] ; then
echo "${ficres}.tmp non vide"
else
echo "Attention pas de coordonnees pour la grille ${code_domaine} !!!!!"
exit
fi

#grillesizex=3000 mis dans include_path
echo ${grillesizex} ${grillesizey} >> ${ficres}.tmp
echo "'"${ficres}"'"               >> ${ficres}.tmp

${F90} ${ChemProg}/calcgrille.f90 -o ${ChemProg}/calcgrille.exe
cd ${ChemWork}
${ChemProg}/calcgrille.exe < ${ficres}.tmp

#...range la chambre...
echo
echo "fichier cree : ${ficres}"
