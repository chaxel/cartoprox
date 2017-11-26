#!/bin/ksh
# recupere coord rues pour integration ss netcdf
###################
ChemData=/mnt/MOD-SIRANE/appli/CARTOPROX/m2008_ct_PM/RESULTAT_200801 # peu importe le run du moment qu'on ait le bon nbre de rues
ChemWork=/mnt/MOD-SIRANE/appli/CARTOPROX/m2008_ct_PM/Workfond/
awk '{ printf("%s %s %s\n",$1,$2,$3) } ' $ChemData/stat-rue-.dat > $ChemWork/coordrues.dat 

echo $ChemWork/coordrues.dat
head $ChemWork/coordrues.dat
