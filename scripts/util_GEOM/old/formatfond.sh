#!/bin/ksh 
# formatage du fichier fond sirane pour une rapide lecture ss f90
#
# FT 20090701
###################################################################
ChemWork=/appli/CARTOPROX/m2008_ct_PM/Workfond
ChemData=/appli/CARTOPROX/INPUT_2008
ficin=fond_2008.dat
ficout=$ficin.4f902

if [ ! -f $ChemData/$ficin ] ; then
 echo traitement impossible : manque le fond !!! $ChemData/$ficin
 exit
fi

sed -e "s?/? ?g" \
    -e "s?:00? ?g" \
    $ChemData/$ficin > $ChemWork/$ficout

echo fichier cree : $ChemWork/$ficout 
head $ChemWork/$ficout

