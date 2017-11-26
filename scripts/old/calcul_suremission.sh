#!/bin/bash

. ../include/common.inc

export cartoprox=`pwd | gawk -F '/scripts' '{print $1}'`

TMPDIR=${SCRATCH}/cartoprox_user_oper

deb_h=00
fin_h=23

############################################################################################
# Inputs
############################################################################################
case $# in
2)
liste_variable="no2_moy_an nb_dep_50_jour pm10_moy_an" #nb_dep_200_jour
deb_j=$2
fin_j=$3
;;
3)
liste_variable="$1"
deb_j=$2
fin_j=$3
;;
5)
liste_variable="$1"
deb_j=$2
deb_h=$3
fin_j=$4
fin_h=$5
;;
*)
echo -e "$BLEU" "Syntaxe $0 (variable) deb_j (deb_h) fin_j (fin_h)" "$NORMAL"

exit
;;
esac

############################################################################################
# INCLUDE
############################################################################################
# declaration de la periode de calcul 
if [ "${periode}" == "" ] ; then
  export periode=${deb_j}_${fin_j}
fi

deb_j=`echo ${periode} | gawk -F "_" '{print $1}'`
fin_j=`echo ${periode} | gawk -F "_" '{print $2}'`
annee=`date -d ${deb_j} +%Y`

#Include
source ${cartoprox}/cartoprox.inc ${periode} || \
  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; }
############################################################################################

zoom_level=9         #Zoom de la pour la grille de suremissions
grille_dx=${fond_dx} #Resolution de la grille de suremission. Cette taille peut être la même que le modèle REGIONAL
zoom_region=9000     #Taille du domaine utilisé pour le zoom : egal à la taille de domaine CARTOPROX (forcement carré à ce stade)
                     #Fixer cette valeur dans ./include/zoom.inc

for variable in ${liste_variable} ; do

# Variable 
case ${variable} in 
no2_moy_an|nb_dep_50_jour|*NO*|*O3*)export polluant=1;;
nb_dep_50_jour|pm10_moy_an|*PM10*)export polluant=2;;
esac

#Include
source ${cartoprox}/cartoprox.inc ${periode} || \
  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; }

#Genere une grille reguliere de 1000 m x 1000 m
#1000 m est la taille du voisinage utilisé
maille_dx=${grille_dx}
maille_dy=${grille_dx}
maille_nx=`echo ${maille_xmin} ${maille_xmax} ${maille_dx} | gawk '{ print int(($2+1.-$1)/$3)}'`
maille_ny=`echo ${maille_ymin} ${maille_ymax} ${maille_dy} | gawk '{ print int(($2+1.-$1)/$3)}'`
maille_x=`echo ${maille_xmin} ${maille_xmax} | gawk '{ print int(($2+$1)/2)}'`	#Centre X
maille_y=`echo ${maille_ymin} ${maille_ymax} | gawk '{ print int(($2+$1)/2)}'`	#Centre Y
maille_xx=`echo ${maille_xmin} ${maille_xmax} | gawk '{ print int(($2-$1))}'` #Largeur
maille_yy=`echo ${maille_ymin} ${maille_ymax} | gawk '{ print int(($2-$1))}'` #Hauteur

RepCarto=${ZoomRes}/${periode}/${code_maille}/zoom${zoom_level}_${zoom_region}_${zoom_region}_${grille_dx}m # multi_polluants

fic_cdf=${RepCarto}/cdf/export_cartoprox_${maille_x}_${maille_y}_${zoom_region}_${zoom_region}.cdf

#Genere la carte des concentrations moyennées à CARTOPROX à 1 km
#rm -f ${fic_ascii}
##if [ ! -f ${fic_ascii} ] ; then  
#  ./6c_plot_CARTOPROX_1000m.sh ${variable} 
#else
#  echo -e "${fic_ascii}" "$VERT" "OK" "$NORMAL"  
#fi

#if [ ! -f ${fic_ascii} ] ; then
#  echo "Erreur sur ${fic_ascii}"
#  exit
#fi

#rm -f ${fic_cdf}
if [ ! -f ${fic_cdf} ] ; then
  echo -e "${fic_cdf}" "$ROUGE" "INDISPONIBLE" "$NORMAL"
  exit
  #cd ${cartoprox}/utils/recept2grid
  #./recept2grid.sh ${fic_ascii} ${variable} ${fic_cdf} ${fic_grd} ${fic_vm} 1000 ${maille_xmin} ${maille_ymin} ${maille_xmax} ${maille_ymax}
else
  echo -e "${fic_cdf}" "$VERT" "OK" "$NORMAL"
fi

##############################################################################################################################
#ETAPE 1: Extrait PREVALP sur un voisinage (pratique pour les petits domaines)
##############################################################################################################################
echo "Extrait un voisinage de PREVALP aux recepteurs (reduit le temps de calcul pour les petits domaines)"
#PREVALP est defini sur une grille reguliere ${fond_xmin} ${fond_ymin} ${fond_dx}  ${fond_nx} ${fond_ny}

#calcule le voisinage de PREVALP                                                          NCO  voisinage pour interpolation
i_min=`echo ${maille_xmin} ${fond_xmin} ${fond_dx} | awk '{print int(($1+$3/2-$2)/$3) + 1 - 1 - 1 }'`
j_min=`echo ${maille_ymin} ${fond_ymin} ${fond_dx} | awk '{print int(($1+$3/2-$2)/$3) + 1 - 1 - 1 }'`
i_max=`echo ${maille_xmax} ${fond_xmin} ${fond_dx} | awk '{print int(($1-$3/2-$2)/$3) + 1 - 1 + 1 }'`
j_max=`echo ${maille_ymax} ${fond_ymin} ${fond_dx} | awk '{print int(($1-$3/2-$2)/$3) + 1 - 1 + 1 }'`
fond_xmin_maille=`echo ${maille_xmin} ${fond_dx} | awk '{print int( $1 - $2 ) }'`
fond_ymin_maille=`echo ${maille_ymin} ${fond_dx} | awk '{print int( $1 - $2 ) }'`

echo "i_min ${i_min}"
echo "j_min ${j_min}"
echo "i_max ${i_max}"
echo "j_max ${j_max}"

#Nom du fichier de sortie
fond_fic_maille=`basename ${fond_fic_stat} | sed -e "s/.nc/_mini.nc/"`
fond_fic_maille=${TMPDIR}/${fond_fic_maille}

echo -n "${fond_fic_maille}..."
${NCO}/bin/ncks -O -o ${fond_fic_maille} \
     -d west_east,${i_min},${i_max} -d south_north,${j_min},${j_max} \
     -v ${variable} ${fond_fic_stat} || { echo  -e "$ROUGE" "ERREUR extraction. Dimensions OK ?" "$NORMAL"; exit 1; }

if [ -f ${fond_fic_maille} ] ; then
echo -e "$VERT" "OK" "$NORMAL"
else
echo -e "$ROUGE" "ECHOUE" "$NORMAL"
exit
fi

#ncdump -h ${fond_fic_stat_maille}

##############################################################################################################################
#ETAPE 2: Genere une grille reguliere
##############################################################################################################################
cd ${cartoprox}/utils/recept2grid
echo " Genere une grile reguliere..."
echo "xmin ${maille_xmin}"
echo "ymin ${maille_ymin}"
echo "maille_x ${maille_dx}"
echo "maille_y ${maille_dy}"
echo "maille_nx ${maille_nx}"
echo "maille_ny ${maille_ny}"
echo "maille_dx ${maille_dx}"
echo "maille_dy ${maille_dy}"
rm -rf ${TMPDIR}/grille.xyz genere_grille.exe
${F90} -o ${TMPDIR}/genere_grille.exe ./src/genere_grille.f90
syntaxe="${TMPDIR}/genere_grille.exe -xmin ${maille_xmin} -ymin ${maille_ymin} -nx ${maille_nx} -ny ${maille_ny} -dx ${maille_dx} -dy ${maille_dy}"
echo $syntaxe
${syntaxe} | gawk '{print $1" "$2" 0."}' > ${TMPDIR}/grille.xy
echo -e "-> ${TMPDIR}/grille.xy" "$VERT" "`wc -l ${TMPDIR}/grille.xy | gawk '{print $1}'` points" "$NORMAL"

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< BOUCLE SUR LES ITERATIONS >>>>>>>>>>>>>>>>>>>>>>>>>

##############################################################################################################################
#ETAPE 3: extrait PREVALP sur la grille de sortie
##############################################################################################################################
#Extrait PREVALP a cette grille
echo "Extrait les valeurs de PREVALP aux recepteurs..."
syntaxe="${extract_val_grille_exe} \
    -i ${fond_fic_maille} -xmin ${fond_xmin_maille} -ymin ${fond_ymin_maille} -dx ${fond_dx} -s ${TMPDIR}/grille.xy -var ${variable}" 
${syntaxe} | gawk '{ print $1" "$2" "$4 }' > ${TMPDIR}/prevalp_xyz.txt  \
 || { echo "$0: ERREUR de l extraction PREVALP ${TMPDIR}/grille.xy" ; exit 1 ; }
echo -e "-> ${TMPDIR}/prevalp_xyz.txt" "$VERT" "OK" "$NORMAL"
head -n 5 ${TMPDIR}/prevalp_xyz.txt

#calcule les indices i et j des mailles (sur domaine réduit)
gawk '{print int(($1-'${maille_xmin}')/'${maille_dx}') + 1" "int(($2-'${maille_ymin}')/'${maille_dy}') + 1" "$3  }' \
  ${TMPDIR}/prevalp_xyz.txt >  ${TMPDIR}/prevalp_ijz.txt
echo -e "-> ${TMPDIR}/prevalp_ijz.txt" "$VERT" "OK" "$NORMAL"
head -n 5 ${TMPDIR}/prevalp_ijz.txt
#cat ${TMPDIR}/prevalp_ijz.txt

##############################################################################################################################
#ETAPE 4: rempli le NetCDF de suremission pour tout le domaine 
##############################################################################################################################

#Nom des fichiers
suremission_nc=${ZoomRes}/${periode}/${code_maille}/suremission/${variable}_${code_maille}${selection}.nc
suremission_nc_tmp=${TMPDIR}/suremissions_${code_maille}.nc

#Creer le fichier resultat
cp ${fic_cdf} ${suremission_nc_tmp}

min_conc=0.
#L'executable soustrait Les valeurs de PREVALP aux concentrations
echo "Rempli ${suremission_nc_tmp}..."
${cartoprox}/scripts_agglos/src/fill_cdf.e ${suremission_nc_tmp} ${TMPDIR}/prevalp_ijz.txt ${variable} ${min_conc} | head -n 5
echo -e "-> ${suremission_nc_tmp}" "$VERT" "OK" "$NORMAL"

mkdir -p       ${ZoomRes}/${periode}/${code_maille}/suremission
mv ${suremission_nc_tmp} ${suremission_nc}

echo -e "-> ${suremission_nc}" "$VERT" "OK" "$NORMAL"

done #varaible

