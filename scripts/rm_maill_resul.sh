#!/bin/bash
#############################################
domaine=region_A6_A89
year=2011
projet=aera
ref=tv
sce=ssPLe2   #ssPLe2  #ssPLe3 #ssPL
list_selection="grenoble"    #"2011" #  "villefranche roanne st_etienne vallees_MT chamonix valence montelimar"    #annecy  #chambery  #grenoble  #lyon
ChemRes=/home/oper/data/CARTOPROX
ChemRecep=/mnt/mod4/appli/CARTOPROX_V352/inputs/RECEPT/${domaine}  #/home/oper/data/CARTOPROX
ChemGeom=/mnt/mod4/appli/CARTOPROX_V352/inputs/GEOM/drome_AirB
periode=20110101_20111231
################################################
for selection in $list_selection
do
  ficmaille=mailles_${domaine}_${selection}.txt   #region_A6_A89_lyon.txt  #mailles_chamonix_a_rm.txt
  ChemRef=$ChemRes/res_V352_${projet}_${ref}_${year}
  ChemCartoprox=$ChemRes/res_V352_${projet}_${sce}_${year}   #res_V31_region_A6_A89_${periode}
  #listmaille=`awk '{ if(NR>1) {print $1}  }' /home/oper/data/CARTOPROX/${ficmaille}`     #${ChemRes}/${ficmaille}`
#  listmaille=`awk '{ if(NR>=1 && $5!=0) {print $2}  }' /mnt/mod4/appli/CARTOPROX_V352/selections/${ficmaille}` #/home/oper/data/CARTOPROX/${ficmaille}`
  listmaille="647500_4972500 647500_4975500 647500_4978500 647500_4981500 650500_4966500 650500_4969500 650500_4972500 650500_4975500 650500_4978500 650500_4981500 653500_4972500 653500_4975500 653500_4978500 653500_4981500" 
  echo $listmaille
#exit
  for maille in $listmaille
  do
    echo
    echo ========= traitement de la selection $selection et de la maille $maille ==========
    echo
    cd $ChemCartoprox
    RepRes=$ChemCartoprox/$maille
#    echo creation du lien de suremission
#    rm $ChemCartoprox/$maille/suremis_PM10.2011010100_2011123123.nc   #$ChemCartoprox/$maille/suremis_nox.2011010100_2011123123.nc
#     ls $ChemRef/$maille/suremis_PM10.2011010100_2011123123.nc $ChemCartoprox/$maille/.
#    ln -s $ChemRef/$maille/suremis_PM10.2011010100_2011123123.nc $ChemCartoprox/$maille/.
#     ln -s $ChemRef/$maille/suremis_nox.2011010100_2011123123.nc $ChemCartoprox/$maille/.
#    echo "j efface  les fichiers de ${RepRes}/"  ##suremis_PM10.${periode}.nc
#    sleep 1s
#    ncdump -h  $RepRes/suremis_PM10.*.nc
#    rm -r  $RepRes/
#    rm  $RepRes/sirane_PM10.*.nc
#    rm  $RepRes/sirane_nox.*.nc   
#    rm  $RepRes/suremis_PM10.*.nc
#    rm  $RepRes/suremis_nox.*.nc
#    rm  $RepRes/cartoprox_nox.*.nc
#    rm  $RepRes/cartoprox_PM10.*.nc
#    echo "efface stat.nc"
#    rm $RepRes/stat.*.nc
#    echo "j efface  les fichiers recepteurs de ${ChemRecep}/$maille" 
#    rm -r $ChemRecep/$maille
    echo "j efface  le fichier rues de ${ChemGeom}/$maille" 
    rm -r $ChemGeom/$maille   #/rues.txt

#break
  done # maille
done #selec
date
