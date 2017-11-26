#!/bin/bash

localdir=`pwd`
script=5a_calcul_suremission.sh

#Principe
#1.Liste les fichiers CARTOPROX de sortie
#2.Fournit la grille PREVALP
#3.Utilitaire -> lit un fichier SIRANE -> moyenne les concentrations dans les mailles PREVALP pour heure 1 a 8760
#4.Fichier PREVALP suremissions OK
#5.Corrige le fichier CARTOPROX
deb_h=00
fin_h=23

############################################################################################
# Inputs
############################################################################################
case $# in
2)
deb_j=$1
fin_j=$2
;;
4)
deb_j=$1
deb_h=$2
fin_j=$3
fin_h=$4
;;
*)
echo -e "$BLEU" "Syntaxe $0 deb_j (deb_h) fin_j (fin_h)" "$NORMAL"

exit
;;
esac

############################################################################################
# INCLUDE
############################################################################################
export polluant=1

# declaration de la periode de calcul 
if [ "${periode}" == "" ] ; then
  export periode=${deb_j}_${fin_j}
fi

deb_j=`echo ${periode} | gawk -F "_" '{print $1}'`
fin_j=`echo ${periode} | gawk -F "_" '{print $2}'`
annee=`date -d ${deb_j} +%Y`

#Include
source ${cartoprox}/include/cartoprox.inc ${periode} || \
  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; }
############################################################################################

liste_variable="NO2 NO O3 PM10"  # PM25"
liste_prefix_nc="_nox _PM10" #  _PM25"

./0b_liste_domaines.sh > ${localdir}/liste_domaine

#SUPPRIME LES FICHIERS de suremissions
for prefix_nc in ${liste_prefix_nc} ; do
fic_suremis_coarse=${prevalp_raster}/${cartoprox_domaine}/suremis${prefix_nc}.${deb_j}${deb_h}_${fin_j}${fin_h}_${dom_chimere}${selection}.nc
fic_fond_coarse=${prevalp_raster}/${cartoprox_domaine}/out${prefix_nc}.${deb_j}${deb_h}_${fin_j}${fin_h}_${dom_chimere}${selection}.nc
echo "Supprime ${fic_suremis_coarse}"
rm -f  ${fic_suremis_coarse} 
echo "Supprime ${fic_fond_coarse}"
rm -f  ${fic_fond_coarse} 
done

for variable in ${liste_variable} ; do

case ${variable} in
NO2|NO|O3)export polluant=1;prefix_nc="_nox";;
PM10)export polluant=2;prefix_nc="_${variable}";;
PM25)export polluant=3;prefix_nc="_${variable}";;
*)
echo "Specifier le polluant :"
exit
;;
esac



#Include
source ${cartoprox}/cartoprox.inc ${periode} || \
  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; }

##############################################################################################################################
#Etape 1: Extrait PREVALP sur un voisinage (pratique pour les petits domaines)
##############################################################################################################################
#Domaine coarse
#Concentration ppbV
xminc=`echo ${maille_xmin} ${fond_dx} | awk '{print int($1 - $2) }'`
yminc=`echo ${maille_ymin} ${fond_dx} | awk '{print int($1 - $2) }'`
dxc=${fond_dx}

#Concentration en ppbV
fic_suremis_coarse=${prevalp_raster}/${cartoprox_domaine}/suremis${prefix_nc}.${deb_j}${deb_h}_${fin_j}${fin_h}_${dom_chimere}${selection}.nc

#Concentration en ppbV
fic_fond_coarse=${prevalp_raster}/${cartoprox_domaine}/out${prefix_nc}.${deb_j}${deb_h}_${fin_j}${fin_h}_${dom_chimere}${selection}.nc

#Concentration en microg/m3
fic_stat_coarse=${prevalp_raster}/${cartoprox_domaine}/stat${prefix_nc}.${deb_j}${deb_h}_${fin_j}${fin_h}_${dom_chimere}${selection}.nc

if [ ! -f ${fic_suremis_coarse} ] || [ ! -f ${fic_fond_coarse} ] || [ ! -f ${fic_stat_coarse} ] ; then
  mkdir -p ${prevalp_raster}/${cartoprox_domaine}
  ./utils/extrait_mini_reg.sh ${deb_j} ${deb_h} ${fin_j} ${fin_h} ${fic_fond_coarse} ${fic_suremis_coarse} ${fic_stat_coarse} || \
  { echo -e "$ROUGE" "ERREUR dans  ./utils/extrait_mini_reg.sh" "$NORMAL"  ; exit 1 ; }
fi

if [ -f ${fic_suremis_coarse} ]  ; then
  echo -e "$VERT" "${fic_suremis_coarse}" "$NORMAL"
else
  echo -e "$ROUGE" "ERREUR sur ${fic_suremis_coarse}" "$NORMAL"
fi

if [ -f ${fic_fond_coarse} ] ; then
  echo -e "$VERT" "${fic_fond_coarse}" "$NORMAL" 
else
  echo -e "$ROUGE" "ERREUR sur ${fic_fond_coarse}" "$NORMAL"
fi

#echo "DEBUG $0 - STOP"
#exit 0

##############################################################################################################################
#Etape 2: reconstitue un fichier MOSAIQUE
##############################################################################################################################

#ATTENTION: les dates dans les fichiers PREVALP et suremis doivent être les mêmes que celles
#           des sorties SIRANE de suremissions ($fic_suremis_fine)
cat ${localdir}/liste_domaine | while read code_domaine ; do

  # Domaine fin
  # Suremissions en ppbV
  fic_suremis_fine=${ChemRes1}/${code_domaine}/suremis${prefix_nc}.${deb_j}${deb_h}_${fin_j}${fin_h}.nc
  
  #Domain fine`
  #x_y=`echo ${code_domaine} | awk -F '_' '{print $1" "$2}'`
  xcf=`awk '( $1 == "'${cartoprox_domaine}'" && $2 == "'${code_domaine}'" ) { print int($7) }' ${params_mailles}`   # X utm 31
  ycf=`awk '( $1 == "'${cartoprox_domaine}'" && $2 == "'${code_domaine}'" ) { print int($8) }' ${params_mailles}`   # Y utm 31
  dxf=`awk '( $1 == "'${cartoprox_domaine}'" && $2 == "'${code_domaine}'" ) { print int($9) }' ${params_mailles}`  
  xminf=`echo ${xcf} ${ycf} ${dxf} | awk '{print int($1 - $3 / 2) }'`
  yminf=`echo ${xcf} ${ycf} ${dxf} | awk '{print int($2 - $3 / 2) }'`
  echo "Domaine fin:"
  echo "xcf=${xcf}"
  echo "ycf=${ycf}"
  echo "dxf=${dxf}"
  echo "xminf=${xminf}"
  echo "yminf=${yminf}"
  
  #redifinition de la resolution du mini-domaine pour la suremission
  dxf=${fond_dx}
  
DEBUG= #"-d"
syntaxe="/mnt/mod4/appli/CARTOPROX_V352/utils/mosaique_grille/src/mosaique_grille.exe \
     -i ${fic_suremis_fine} \
     -xminf ${xminf} -yminf ${yminf} -dxf ${dxf} \
     -o ${fic_suremis_coarse} \
     -xminc ${xminc} -yminc ${yminc} -dxc ${dxc} \
     -v ${variable} ${DEBUG}"
#echo $syntaxe

#Sorties dans le fichier maillé ${fic_suremis_coarse} (en ppbV)


if [ -f ${fic_suremis_fine} ] ; then
  echo -n "Traite ${variable} de ${fic_suremis_fine} " 
  ${syntaxe} || \
  { echo -e "$ROUGE" "ERREUR" "$NORMAL"  ; exit 1 ; }
  echo -e "$VERT" "OK" "$NORMAL"
else
  echo "SKIP ${fic_suremis_fine} --> existe pas" 
fi

done
 
echo "-> ${fic_suremis_coarse}" 
     
done

rm -f ${localdir}/liste_domaine

