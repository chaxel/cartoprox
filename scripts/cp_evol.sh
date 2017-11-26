#!/bin/bash
# change les dates des fichiers evol 2011 en 2015
#############################################
listpolluant="1 2 3"
domaine=region_A6_A89
listselection="_annecy _grenoble _chamonix _vallees_MT"
vemisin=2011v182    #emis_2011v182_ponct_1.txt
vemisout=2015v156
ChemGeom=/mnt/mod4/appli/CARTOPROX_V353/inputs/GEOM/region_A6_A89
#ChemWork=/data/CARTOPROX/res_V353_aera_tend_2015v156
#listrep=`awk '{ if(NR>=1) {printf("%s\n",$1)}  }' ${ChemWork}/list_rep.txt`
#listrep=698500_5017500 #grenoble    #632500_4912500    #650500_5044500
#echo $listrep
##############################################
for selection in $listselection
do
  ficmaille=mailles_${domaine}${selection}.txt 
  listmaille=`awk '{ if(NR>=1 && $5!=0) {print $2}  }' /mnt/mod4/appli/CARTOPROX_V353/selections/${ficmaille}`
#  listmaille=698500_5017500  #grenoble
  for maille in $listmaille
  do
    echo
    echo ========= traitement de la selection $selection et de la maille $maille ==========
    echo

    for polluant in $listpolluant
    do
#      ficevolrues=evol-emis-rues_pol${polluant}.dat
#      ficevolnoeuds=evol-emis-noeuds_pol${polluant}.dat
#      ficevolponct=evol-emis-ponct_pol${polluant}.dat

      ficemisnoeudsin=emis_${vemisin}_noeuds_${polluant}.txt
      ficemisponctin=emis_${vemisin}_ponct_${polluant}.txt
      ficemisnoeudsout=emis_${vemisout}_noeuds_${polluant}.txt
      ficemisponctout=emis_${vemisout}_ponct_${polluant}.txt

      ChemData=$ChemGeom/$maille    #### A changer
      ChemRes=$ChemData     #### A changer
      if [ ! -d ${ChemRes} ] ; then
       echo pb maille n existe pas
      fi
      cd $ChemData
#      if [ ! -f ${ficevolrues} ] ; then
#       echo "pas de fichier noeuds.txt dans $maille"
#       exit
#      else
#      sed -e "s/632500_4909500/$code_maille/g" $ChemData/${ficevolrues} > $ChemRes/${ficevolrues} #### A changer
#      sed -e "s/632500_4909500/$code_maille/g" $ChemData/${ficevolnoeuds} > $ChemRes/${ficevolnoeuds} #### A changer
#      sed -e "s/632500_4909500/$code_maille/g" $ChemData/${ficevolponct} > $ChemRes/${ficevolponct} #### A changer
#      echo -e "\\033[1;32m" "fichiers ecrits $ficevolrues" "\\033[0;39m"
#      head -2 $ChemRes/${ficevolrues}  # $ChemRes/${ficevolnoeuds} $ChemRes/${ficevolponct}
#      fi

      if [ ! -f ${ficemisnoeudsin} ] ; then
       echo "pas de fichier ${ficemisnoeudsin} dans $maille"
       exit
      else
       ln -s $ficemisnoeudsin $ficemisnoeudsout
       ln -s $ficemisponctin $ficemisponctout
       echo -e "\\033[1;32m" "fichiers ecrits $ficemisnoeudsout" "\\033[0;39m"
       head -2 $ChemRes/$ficemisnoeudsout 
      fi

    done #polluant    
  done #maille
done #selection

