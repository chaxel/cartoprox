#!/bin/bash

script=6d_map_raster_jp2.sh
jp2_script=${cartoprox}/utils/raster_jp2_v2

# Include
source ${cartoprox}/include/zoom.inc
source ${cartoprox}/include/common.inc

case $# in
  3)
    var=$1
    zoom=$2
    proj=$3
    ;;
  *)
    echo -e "$BLEU" "usage: $0 variable zoom[1-9] proj[l93|utm31]" "$NORMAL"
    exit 1
    ;;
esac

# Projection
case ${proj} in
  l93) epsg=2154 ;;
  utm31) epsg=32631 ;;
  *)
    echo "projection inconnue ${proj} dans $0"
    echo "choix possible entre 'l93' et 'ut31'"
    exit 1
  ;;
esac

# Polluant
case ${var} in
  no2_moy_an|nb_dep_200_jour|*NO*|*NO2*|*O3*) export polluant=1 ;;
  pm10_moy_an|nb_dep_50_jour|*PM10*) export polluant=2 ;;
  pm25_moy_an|*PM25*) export polluant=3 ;;
  *)
    echo "variable inconnue ${var} dans $0"
    exit 1
    ;;
esac

# Include
source ${cartoprox}/cartoprox.inc ${periode} || \
  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; }

# Dossier
RepCarto=${ZoomRes}/${periode}/${cartoprox_domaine}${selection}/zoom${zoom_level[$zoom]}_${zoom_region[$zoom]}_${zoom_region[$zoom]}_${zoom_dx[$zoom]}m
RepOut=${RepCarto}/jp2
mkdir -p ${RepOut}/rgb ${RepOut}/val
fic_rgb=${RepOut}/rgb/raster_${cartoprox_domaine}_${periode}_zoom${zoom_level[$zoom]}_${zoom_dx[$zoom]}m_${var}_rgb
fic_val=${RepOut}/val/raster_${cartoprox_domaine}_${periode}_zoom${zoom_level[$zoom]}_${zoom_dx[$zoom]}m_${var}_val

# Conversion des fichiers cdf présents en jp2 coloré rgb
cd ${jp2_script}
./mkjp2-rgb.sh ${RepCarto} ${fic_rgb} ${var} ${epsg}
[ $? -ne 0 ] && { echo -e "$ROUGE""erreur dans la conversion des fichiers cdf en jp2 rgb !""$NORMAL"; exit 1; }
echo -e "$VERT""$0 OK""${NORMAL}"

# Conversion des fichiers cdf présents en raster de valeur tif
cd ${jp2_script}
./mkrast-val.sh ${RepCarto} ${fic_val} ${var} ${epsg}
[ $? -ne 0 ] && { echo -e "$ROUGE""erreur dans la conversion des fichiers cdf en raster de valeur tiff !""$NORMAL"; exit 1; }
echo -e "$VERT""$0 OK""${NORMAL}"

exit 0
