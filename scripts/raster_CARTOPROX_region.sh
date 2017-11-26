#!/bin/ksh
# lance
# lance_cartoprox.sh zoom sur une liste de selection 
# + 6c_map_raster.sh sur une liste de polluants
# creer les rasters de la region 
# Mng 29/08/2012 pour cartoprox region 2011
# 
#############################################

localdir=`pwd`
echo $localdir
. ${cartoprox}/include/zoom.inc
source ${cartoprox}/include/common.inc
source ${cartoprox}/include/cartoprox.inc ${periode} || \
  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; }

#############################################
#ZoomRes=/mnt/mod4/data/CARTOPROX/carto_V${cartoprox_version}
zoom=7
#RepCarto=${ZoomRes}/${periode}/${cartoprox_domaine}${selection}/zoom${zoom_level[$zoom]}_${zoom_region[$zoom]}_${zoom_region[$zoom]}_${zoom_dx[$zoom]}m
echo "Repertoire de cartographie" $RepCarto
selection_user="_user_$USER"
#RepOut=${SCRATCH}/cartoprox${selection_user}
echo "Repertoire de sortie" $RepOut
echo "SCENARIO =" $sce
sleep 2
zoom_projection=lambert93   #utm31 #/l93
########
do_zoom=1
do_raster=1
##############################################
if [ ${do_zoom} -eq 1 ] ; then
  list_selec="_annecy _chambery _grenoble _chamonix"  # _vallees_MT" # _2011" #_montelimar _stetienne _lyon  _valence
  for selec in $list_selec
  do
    export selection=$selec
    echo "===================== traitement de la selection $selec  =========================="
    echo "===================== lance le zoom ======================"
    echo "./lance_CARTOPROX_region.sh zoom  > ${RepCarto}/log/$selec.log" 
    time ./lance_CARTOPROX_region.sh zoom  #> ${RepCarto}/log/$selec.log
  sleep 3
    #rm -r ${SCRATCH}/recept2grid.sh.*
  done #selec
fi #zoom
#exit
##############################################
if [ ${do_raster} -eq 1 ] ; then
#  export SCRATCH=/mnt/mod3/scratch 
  list_selec="_annecy _chambery _grenoble _chamonix"
  listvar="no2_moy_an pm10_moy_an nb_dep_50_jour"    #pm10_moy_an nb_dep_50_jour"
  echo "===================== lance le 6c ======================"
  for selec in $list_selec
  do
    export selection=$selec
    echo "===================== traitement de la selection $selec  =========================="
    for var in $listvar
    do
      fic_out_base=${RepOut}/${var}${selection}_zoom${zoom}
 #    echo "./6c_map_raster.sh $var $zoom >> ${localdir}/listing_6c$selec.log" 
      time ./6c_map_raster.sh $var $zoom #> ${localdir}/listing_6c$selec.log
      echo -e "$VERT" " raster de $selec en $var cree : ${fic_out_base}_${zoom_projection} " "$NORMAL"
      date
    done #var
  done # selec
fi #raster
