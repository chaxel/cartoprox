#!/bin/ksh
#
# Cartographie de CARTOPROX sur une maille
# Utilisation de X niveaux de zooms
#############################################
# Principe : decoupe la region en n domaines et assigne a chaquer domaine une resolution en fonction de la taille
#           se base sur le script 6a_plot_CARTOPROX.sh pour le decoupage des domaines

localdir=`pwd`
script=6b_zoom_raster.sh
export cartoprox=`pwd | gawk -F '/scripts' '{print $1}'`

############################################################################################
# Niveau de zoom
############################################################################################
. ${cartoprox}/include/zoom.inc
. ${cartoprox}/include/common.inc
############################################################################################
# systeme de coordonnees de cartographie
############################################################################################
if [ "${zoom_projection}" == "" ] ; then 
  zoom_projection=lambert93  #utm31/lambert93
fi
############################################################################################
# Inputs
############################################################################################
case $# in
3)
zoom=$1
liste_variable="no2_moy_an nb_dep_50_jour pm10_moy_an" #nb_dep_200_jour pm25_moy_an
deb_j=$2
deb_h=00
fin_j=$3
fin_h=23
;;
4)
zoom=$1
liste_variable="$2"
deb_j=$3
deb_h=00
fin_j=$4
fin_h=23
;;
6)
zoom=$1
liste_variable="$2"
deb_j=$3
deb_h=$4
fin_j=$5
fin_h=$6
;;
*)
echo -e "$BLEU" "Syntaxe $0 zoom (variable) deb_j (deb_h) fin_j (fin_h)" "$NORMAL"
#Info sur les zooms
for zoom in 1 2 3 4 5 6 7 8 ; do 
echo "zoom=${zoom} ${zoom_desc[${zoom}]} google=${zoom_GM[${zoom}]} dx=${zoom_dx[${zoom}]} m carre=${zoom_region[${zoom}]} m"
done

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
  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; }  # Mng raster cartoprox 2011
############################################################################################

############################################################################################
# domaine de cartographie REGION -> domaine du "cartoprox_domaine" (cartoprox.inc)
#(region_ARN,st_exyupery)
############################################################################################
echo "Domaine (UTM 31)"
echo "maille_xmin=${maille_xmin}"
echo "maille_ymin=${maille_ymin}"
echo "maille_xmax=${maille_xmax}"
echo "maille_ymax=${maille_ymax}"

region_xmin=`echo ${maille_xmin} ${cartoprox_xmin} ${zoom_region[$zoom]} | awk '{ print int((int(($1-$2)/$3)+1) * $3 + $2 - $3 ) }'`
region_ymin=`echo ${maille_ymin} ${cartoprox_ymin} ${zoom_region[$zoom]} | awk '{ print int((int(($1-$2)/$3)+1) * $3 + $2 - $3 ) }'`
region_xmax=`echo ${maille_xmax} ${cartoprox_xmin} ${zoom_region[$zoom]} | awk '{ print int((int(($1-$2)/$3)+1) * $3 + $2 - $3 ) }'`
region_ymax=`echo ${maille_ymax} ${cartoprox_ymin} ${zoom_region[$zoom]} | awk '{ print int((int(($1-$2)/$3)+1) * $3 + $2 - $3 ) }'`

# Determine les nx, ny de la region...
region_nx=`echo ${region_xmin} ${region_xmax} ${zoom_region[$zoom]} | awk '{ print int(($2-$1)/$3)  }'`
region_ny=`echo ${region_ymin} ${region_ymax} ${zoom_region[$zoom]} | awk '{ print int(($2-$1)/$3)  }'`

#Pour avoir du Lambert 93, se base sur les 4 points du domaine en UTM
case ${zoom_projection} in
lambert93)
xmin_l93=`${conversion_exe} -utm 31 -xc ${region_xmin} -yc ${region_ymin} -l93 | awk '{print int($1)}'`
ymin_l93=`${conversion_exe} -utm 31 -xc ${region_xmin} -yc ${region_ymin} -l93 | awk '{print int($2)}'`
xmax_l93=`${conversion_exe} -utm 31 -xc ${region_xmax} -yc ${region_ymax} -l93 | awk '{print int($1)}'`
ymax_l93=`${conversion_exe} -utm 31 -xc ${region_xmax} -yc ${region_ymax} -l93 | awk '{print int($2)}'`

region_xmin=${xmin_l93}
region_ymin=${ymin_l93}
region_xmax=${xmax_l93}
region_ymax=${ymax_l93}
;;
esac
###########################################################################################

#
if [ "${zoom_level[$zoom]}" == "" ] ; then
  echo "Definir ce zoom dans ${cartoprox}/include/zoom.inc: zoom=$zoom"
  exit
fi

echo "-------------------------------------------"
echo "Zoom level=${zoom_level[$zoom]} (Google Earth/Maps=${zoom_GM[$zoom]})"
echo "-------------------------------------------"
echo "region_xmin=${region_xmin}"
echo "region_xmax=${region_xmax}"
echo "region_ymin=${region_ymin}"
echo "region_ymax=${region_ymax}"
echo "zoom_region=${zoom_region[$zoom]}"
echo "region_nx=${region_nx}"
echo "region_ny=${region_ny}"
echo "zoom_dx=${zoom_dx[$zoom]}"

#Repertoire de sortie de la cartographie ZOOM
RepCarto=${ZoomRes}/${periode}/${cartoprox_domaine}${selection}/zoom${zoom_level[$zoom]}_${zoom_region[$zoom]}_${zoom_region[$zoom]}_${zoom_dx[$zoom]}m
ListeMailles=${RepCarto}/liste.mailles.${zoom_projection}

#Re-initialise la liste des mailles
rm -rf ${ListeMailles}

#sleep 3

ix=1

while [ ${ix} -le ${region_nx} ] ; do

iy=1

while [ ${iy} -le ${region_ny} ] ; do

xc_p=`echo ${region_xmin} ${zoom_region[$zoom]} ${ix} |gawk '{print int($1 + ( $3-.5 ) * $2) }'`
yc_p=`echo ${region_ymin} ${zoom_region[$zoom]} ${iy} |gawk '{print int($1 + ( $3-.5 ) * $2) }'`
dx_p=${zoom_region[$zoom]}
dy_p=${zoom_region[$zoom]}

#<<<<<<<<<<<<<<<<<< BOUCLE SUR LES VARIABLES >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
for var in ${liste_variable} ; do

# Variable 
case ${var} in 
no2_moy_an|nb_dep_50_jour|*NO*|*O3*)export polluant=1;;
nb_dep_50_jour|pm10_moy_an|*PM10*)export polluant=2;;
pm25_moy_an|*PM25*)export polluant=3;;
esac

case ${var} in 
*_J0|J0_*)echeance=0;;
*_J1|J1_*)echeance=1;;
*_J2|J2_*)echeance=2;;
*)echeance=-9;;
esac

# Variable 
case ${var} in 
*_J-1|J-1_*)echeance=-1;;
*_J0|J0_*)echeance=0;;
*_J1|J1_*)echeance=1;;
*_J2|J2_*)echeance=2;;
*)echeance=-9;;
esac
var_cdf=`echo ${var} | sed -e "s/_J${echeance}//" | sed -e "s/J${echeance}_//"`

#Liste des fichiers en sortie de plot_script
# A MODIFIER SUIVANT plot_script C eST ULTRA-TORDU CETTE PARTIE !!!!
resolution=${zoom_dx[$zoom]}
selection_user=_user_$USER
tmpdir=${SCRATCH}/cartoprox${selection_user}
fic_base_loc=conc_${deb_j}${deb_h}_${fin_j}${fin_h}_${var}${selection_user}_${xc_p}_${yc_p}_res${resolution}_${zoom_projection} # extension
fic_cdf_loc=${tmpdir}/${fic_base_loc}.nc

#Sortie de ZOOM
label=${xc_p}_${yc_p}_${dx_p}_${dy_p}
liste_extension="txt vm grd gif kmz"
fic_base="export_cartoprox_${label}_${var}" # extension
fic_cdf="${RepCarto}/cdf/export_cartoprox_${label}.cdf"
fic_log="${RepCarto}/log/${var}/export_cartoprox_${label}.log"

if [ ! -f ${fic_cdf} ] ; then
  do_grid=1
else
  if [ "`ncdump -h  ${fic_cdf} | grep "${var_cdf}"`" != "" ] ; then
    do_grid=0
  else
    do_grid=1     
  fi
fi

##################################################################################
if [ ${do_grid} -eq 1 ] ; then

echo "***********************************************************************************************************"
echo "Traite ${var} [${ix}/${region_nx}] [${iy}/${region_ny}] ${xc_p} ${yc_p} ${dx_p} ${dy_p} ${zoom_dx[$zoom]}"
echo "***********************************************************************************************************"

#Repertoire de sortie
cd ${localdir}
mkdir -p ${RepCarto}

if [ -f  ${fic_cdf} ] ; then
  echo "Attention fichier  ${fic_cdf} existe deja -> UTILISE"
  mkdir -p ${tmpdir}  
  cp  ${fic_cdf} ${fic_cdf_loc}
fi 

#Repertoire de LOG (1 par variable)
cd ${localdir}

# Supprime les sorties
#rm -rf ${RepCarto}/cdf/${fic_cdf} # on complete ce fichier s'il existe
liste_extension="txt vm grd gif kmz log"
for extension in ${liste_extension} ; do
  if [ -f ${RepCarto}/${extension}/${fic_base}.${extension} ] ; then
    rm ${RepCarto}/${extension}/${fic_base}.${extension}
  fi
done

mkdir -p ${RepCarto}/log/${var}

#Systeme de coordonnées PAR DEFAUT
if [ "${zoom_projection}" == "" ] ; then
  zoom_projection=utm31 #utm31/lambert93
fi

#Optimisation: seulement CDF
echo "${raster_script} ${var} ${xc_p} ${yc_p} ${dx_p} ${dy_p} ${resolution} ${zoom_projection}_cdf > ${fic_log}"
time ${raster_script} ${var} ${xc_p} ${yc_p} ${dx_p} ${dy_p} ${resolution} ${zoom_projection}_cdf \
  > ${fic_log}  2>&1

if [ -f ${fic_cdf_loc} ] ; then

mkdir -p ${RepCarto}

#Copie les fichiers genere par raster_script dans le repertoire de carto
echo "-> ${RepCarto}/"
for extension in ${liste_extension} ; do
  if [ -f ${tmpdir}/${fic_base_loc}.${extension} ] ; then
    mkdir -p  ${RepCarto}/${extension}/${var}
    mv ${tmpdir}/${fic_base_loc}.${extension} ${RepCarto}/${extension}/${var}/${fic_base}.${extension}
    echo "${extension}/${var}/${fic_base}.${extension}"
  fi  
done

#CDF 1 pour tous les polluant
if [ -f ${fic_cdf_loc} ] ; then
  mkdir -p  ${RepCarto}/cdf
  mv ${fic_cdf_loc} ${fic_cdf}
  echo -e "$VERT" "${fic_cdf} -> genere" "$NORMAL"
fi

else

#echo "${fic_gif_loc} -> ERREUR"
echo -e "$ROUGE" "${fic_cdf_loc} -> ERREUR" "$NORMAL"

#exit 1

fi

#else # do_grid=0

#echo "${var} ${ix}/${region_nx} ${iy}/${region_ny} -> SKIP"

fi # do_grid
##################################################################################

done #liste_variable
#<<<<<<<<<<<<<<<<<< BOUCLE SUR LES VARIABLES >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#Liste la maille dans le fichier
if [ -f ${fic_cdf} ] ; then
echo "Traitement OK ${var} [${ix}/${region_nx}] [${iy}/${region_ny}] ${fic_cdf} -> ${ListeMailles}"
touch ${ListeMailles}
echo toutes_variables ${ix} ${iy} ${xc_p} ${yc_p} ${dx_p} ${dy_p} >> ${ListeMailles}
fi

iy=`expr ${iy} \+ 1`

done

ix=`expr ${ix} \+ 1`

done
echo "-> ${ListeMailles} (equivalent Google ${zoom_GM})"

echo "***info: Fin du script $0"
