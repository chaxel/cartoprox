#!/bin/ksh
#####################################################################
# Cartographie de CARTOPROX sur un domaine complet
# usage : 6c_map_CARTOPROX.sh 
#####################################################################

localdir=`pwd`
script=6c_map_raster.sh
export cartoprox=`pwd | gawk -F '/scripts' '{print $1}'`

do_export=1
do_grd=1
do_gif=0
do_png=0
do_kmz=0
do_jp2=1
do_suremis=0 #prend en compte la suremission ?
#do_gmt=0
do_rmgrd=1

annee_defaut=2010 # dans cartoprox.inc
deb_j=${annee_defaut}0101
fin_j=${annee_defaut}1231
deb_h=00
fin_h=23

############################################################################################
# Niveau de zoom
############################################################################################
. ${cartoprox}/include/zoom.inc
. ${cartoprox}/include/common.inc
############################################################################################
# systeme de coordonnees de cartographie
############################################################################################
if [ "${zoom_projection}" == "" ] ; then 
  zoom_projection=lambert93 #utm31/lambert93
fi 
############################################################################################
# domaine de cartographie
############################################################################################
# automatique : avec tout domaine dans cartoprox.inc
xc_p="x"
yc_p="x"
dx_p="x"
dy_p="x"
xmin_p=-9999999
ymin_p=-9999999
xmax_p=9999999
ymax_p=9999999
code_domaine_loc=""
zoom=4
############################################################################################
# Fond utilis� pour la cartographie
############################################################################################
ifond=2 # 2 = recepteur + fond interpole CHIMERE (fournir le fichier)
############################################################################################

case $# in
1)
var=$1
zoom=7 # resolution native de CARTOPROX (10 m)
;;
2)
var=$1
zoom=$2 # 1-7
;;
5)
var=$1
xc_p=$2 #easting en UTM31 wgs84
yc_p=$3 #northing en UTM31 wgs84
dx_p=$4
dy_p=$5
;;
6)
var=$1
xc_p=$2 #easting en UTM31 wgs84
yc_p=$3 #northing en UTM31 wgs84
dx_p=$4
dy_p=$5
zoom=$6
;;
*)
echo -e "$BLEU" "Syntaxe $0  variable (Xc Yc dx dy) zoom[1-9]" "$NORMAL" # (Xmin Ymin Xmax Ymax)" #(debut[YYYYMMJJ (HH)] fin[YYYYMMJJ (HH)])"
#Info sur les zooms
for zoom in 1 2 3 4 5 6 7 8 9 ; do 
echo "zoom=${zoom} ${zoom_desc[${zoom}]} google=${zoom_GM[${zoom}]} dx=${zoom_dx[${zoom}]} m carre=${zoom_region[${zoom}]} m"
done
exit
;;
esac

############################################################################################
# Variable
############################################################################################
export polluant=1 #par defaut

############################################################################################
# Niveau de zoom
############################################################################################
# voir zoom.inc
grille_dx=${zoom_dx[$zoom]}

############################################################################################
# INCLUDE
############################################################################################
# declaration de la periode de calcul 
if [ "${periode}" == "" ] ; then
  periode=${deb_j}_${fin_j}
fi

deb_j=`echo ${periode} | gawk -F "_" '{print $1}'`
fin_j=`echo ${periode} | gawk -F "_" '{print $2}'`
annee=`date -d ${deb_j} +%Y`

#Include
source ${cartoprox}/cartoprox.inc ${periode} || \
  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; }
############################################################################################

############################################################################################
# Sorties
############################################################################################
#RepCarto=/mnt/mod4/data/CARTOPROX/carto/${var}_zoom${zoom_level}
RepCarto=${ZoomRes}/${periode}/${cartoprox_domaine}${selection}/zoom${zoom_level[$zoom]}_${zoom_region[$zoom]}_${zoom_region[$zoom]}_${zoom_dx[$zoom]}m # multi_polluants
ListeMailles=${RepCarto}/liste.mailles.${zoom_projection}

if [ ! -d ${RepCarto} ] ; then
  echo "Repertoire avec sorties maillees n existe pas : ${RepCarto}"
  for zoom in 1 2 3 4 5 6 7 8 9 ; do 
    echo "zoom=${zoom} ${zoom_desc[${zoom}]} google=${zoom_GM[${zoom}]} dx=${zoom_dx[${zoom}]} m carre=${zoom_region[${zoom}]} m"
  done  
  echo "Relancer ${zoom_script}"
  exit 1
fi

if [ ! -f ${ListeMailles} ] ; then
  echo "Fichier de liste des maille ABSENT: ${ListeMailles}"
  for zoom in 1 2 3 4 5 6 7 8 9 ; do 
    echo "zoom=${zoom} ${zoom_desc[${zoom}]} google=${zoom_GM[${zoom}]} dx=${zoom_dx[${zoom}]} m carre=${zoom_region[${zoom}]} m"
  done
  exit 1
fi

# Variable ######################################
case ${var} in 
*_J-1|*_Jm1|J-1_*|Jm1_*)echeance=-1;;
*_J0|J0_*)echeance=0;;
*_J1|J1_*)echeance=1;;
*_J2|J2_*)echeance=2;;
*)echeance=-9;;
esac
var_cdf=`echo ${var} | sed -e "s/_J${echeance}//" | sed -e "s/J${echeance}_//"`

# Choix du mode de CARTOPROX ######################################
case ${var} in
no2_moy_an|nb_dep_200_jour|*NO*|*NO2*|*O3*)export polluant=1;;
pm10_moy_an|nb_dep_50_jour|*PM10*)export polluant=2;;
pm25_moy_an|*PM25*)export polluant=3;;
*)
echo "variable inconnue ${var} dans $0"
exit 1
;;
esac

## INCLUDE ########################################################
. ${cartoprox}/cartoprox.inc ${periode} || \
  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; }
selection_user="_user_$USER" #domaine defini par utilisateur

#Sorties GRD/netCDF/GIF
RepOut=${SCRATCH}/cartoprox${selection_user}
mkdir -p ${RepOut}

if [ ! -d ${RepOut} ] ; then
  echo "Impossible de creer : ${RepOut}"
  exit 1
fi

###################################################################
# Fin des modifications USER
###################################################################
echo "cartoprox_domaine=${cartoprox_domaine}"
echo "selection=${selection}"
echo "selection_user=${selection_user}"
echo "Parametre du domaine d extraction(UTM 31) :"
# defini le domaine de PLOT
if [ "${xc_p}" != "x" ] && [ "${yc_p}" != "x" ]  ; then
xmin_p_utm31=`echo ${xc_p} ${dx_p} | gawk '{print int($1 - $2/2.)}'`
xmax_p_utm31=`echo ${xc_p} ${dx_p} | gawk '{print int($1 + $2/2.)}'`
ymin_p_utm31=`echo ${yc_p} ${dy_p} | gawk '{print int($1 - $2/2.)}'`
ymax_p_utm31=`echo ${yc_p} ${dy_p} | gawk '{print int($1 + $2/2.)}'`
else
xmin_p_utm31=${maille_xmin}
ymin_p_utm31=${maille_ymin}
xmax_p_utm31=${maille_xmax}
ymax_p_utm31=${maille_ymax}
fi

#Si le domaine d'export est en dehors du domaine CARTOPROX
if [ ${xmin_p_utm31} -lt ${maille_xmin} ] ; then
  xmin_p_utm31=${maille_xmin}
fi
if [ ${ymin_p_utm31} -lt ${maille_ymin} ] ; then
  ymin_p_utm31=${maille_ymin}
fi
if [ ${xmax_p_utm31} -gt ${maille_xmax} ] ; then
  xmax_p_utm31=${maille_xmax}
fi
if [ ${ymax_p_utm31} -gt ${maille_ymax} ] ; then
  ymax_p_utm31=${maille_ymax}
fi

case ${zoom_projection} in
lambert93)
#Pour avoir du Lambert 93
echo "++ lambert 93 ++"
xmin_p=`${conversion_exe} -utm 31 -l93 -xc ${xmin_p_utm31} -yc ${ymin_p_utm31} | awk '{print int($1)}'`
ymin_p=`${conversion_exe} -utm 31 -l93 -xc ${xmin_p_utm31} -yc ${ymin_p_utm31} | awk '{print int($2)}'`
xmax_p=`${conversion_exe} -utm 31 -l93 -xc ${xmax_p_utm31} -yc ${ymax_p_utm31} | awk '{print int($1)}'`
ymax_p=`${conversion_exe} -utm 31 -l93 -xc ${xmax_p_utm31} -yc ${ymax_p_utm31} | awk '{print int($2)}'`
epsg=2154
;;
*)
echo "++ wgs84 / utm31 nord ++"
xmin_p=${xmin_p_utm31}
ymin_p=${ymin_p_utm31}
xmax_p=${xmax_p_utm31}
ymax_p=${ymax_p_utm31}
epsg=32631
;;
esac

#Calcul des dimensions
dx_p=`echo ${xmin_p} ${xmax_p} | gawk '{print $2 - $1}'`
dy_p=`echo ${ymin_p} ${ymax_p} | gawk '{print $2 - $1}'`

nx_p=`echo ${dx_p} ${grille_dx} | gawk '{print int( $1 / $2) }'`
ny_p=`echo ${dy_p} ${grille_dx} | gawk '{print int( $1 / $2) }'`

echo "xmin=${xmin_p}"
echo "ymin=${ymin_p}"
echo "xmax=${xmax_p}"
echo "ymax=${ymax_p}"
echo "dx=${dx_p}"
echo "dy=${dy_p}"
echo "nx=${nx_p}"
echo "ny=${ny_p}"

total_pts=`expr ${nx_p} \* ${ny_p}`
if [ ${total_pts} -ge 5000000 ] ; then
  echo "ATTENTION: Grille de grande dimensions ${nx_p}x${ny_p} (${total_pts} points)"
  if [ "${seuil_var}" == "" ] ; then
    echo "*** Choix :specifier export seuil_var=valeur_limite?"
#    read seuil_var
  else
    echo "seuil_var=${seuil_var}"
    echo "Continue dans 5 sec..."
    sleep 5
  fi  
fi 

# repertoire temporaire ###########################################
TMPDIR=${SCRATCH}/${script}.${xmin_plot}_${ymin_plot}_${xmax_plot}_${ymax_plot}.$RANDOM
while [ -d ${SCRATCH}/${script}.${xmin_plot}_${ymin_plot}_${xmax_plot}_${ymax_plot}.$RANDOM ] ; do
TMPDIR=${SCRATCH}/${script}.${xmin_plot}_${ymin_plot}_${xmax_plot}_${ymax_plot}.$RANDOM
done
echo "Travaille dans ${TMPDIR}"
mkdir -p ${TMPDIR}

# selectionne les minidomaines dans le domaine de PLOT#############
echo "Recherche les mailles dans ${ListeMailles}"
gawk '(($4+$6/2>'${xmin_p}')&&($4-$6/2<'${xmax_p}')&&\
       ($5+$7/2>'${ymin_p}')&&($5-$7/2<'${ymax_p}')) \
 { print $4"_"$5"_"$6"_"$7 }' ${ListeMailles} > ${TMPDIR}/mailles.txt

resolution=`gawk '(NR==1){print $6}' ${ListeMailles}`

nmailles=`cat ${TMPDIR}/mailles.txt | wc -l`
echo "Liste des mailles : "${nmailles}
#cat ${TMPDIR}/mailles.txt | wc -l
if [ ${nmailles} -eq 0 ] ; then
echo "***info : aucune maille a traiter - STOP"
exit
fi

# log
log=${TMPDIR}/silence.log
##################################################################
#for var in ${var_list} ; do
##################################################################
#fic_out_base=${RepOut}/${var}_${xc_p}_${yc_p}_zoom${zoom}
#fic_out_base=${RepOut}/${var}${selection}_zoom${zoom}
fic_out_base=${RepOut}/${var}${selection}_${sce}

if [ ${do_export} -eq 1 ] ; then

#fichier ASCII
fic_ascii=${TMPDIR}/tmp.${var}
if [ -f ${fic_ascii} ] ; then 
 rm ${fic_ascii}
fi

#Genere un fichier avec la variable concern�e
touch ${fic_ascii}

#utilise-t-on un seuil de valeur ?
if [ "${seuil_var}" == "" ] ; then
  seuil_var=-999
else
  # desactive toutes les sorties  
  echo "***info : seuil_var=${seuil_var} -> desactive les sorties graphiques"
  do_grd=0
  do_gif=0
  do_png=0
  do_kmz=0
  do_jp2=0
  fic_out_base="${fic_out_base}_seuil${seuil_var}"
fi

#Nom du fichier ASCII avec les points recepteurs
fic_xyz=${fic_out_base}_${zoom_projection}.txt
echo ${fic_xyz}

##################################################################################
ndom=`wc -l ${TMPDIR}/mailles.txt | gawk '{print $1}'`
#<<<<<<<<<<<<<<<<<<< Boucle sur les mini domaines>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
idom=0
cat ${TMPDIR}/mailles.txt | while read cartoprox_domaine_loc ; do
#<<<<<<<<<<<<<<<<<<< Boucle sur les mini domaines>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
idom=`expr ${idom} \+ 1`

# NOMS DES FICHIERS ##################################################
fic_cartoprox_nc=${RepCarto}/cdf/export_cartoprox_${cartoprox_domaine_loc}.cdf

if [ -f ${fic_cartoprox_nc} ] ; then

# extrait les valeurs du NetCDF -> fichier ASCII 

# Directement en UTM
${ncks} -s '%f \n' -H -C -v easting    ${fic_cartoprox_nc} > ${TMPDIR}/tmp.x
${ncks} -s '%f \n' -H -C -v northing   ${fic_cartoprox_nc} > ${TMPDIR}/tmp.y
${ncks} -s '%f \n' -H -C -v ${var_cdf} ${fic_cartoprox_nc} > ${TMPDIR}/tmp.var
fic_ascii_tmp=${TMPDIR}/tmp.xyz

paste ${TMPDIR}/tmp.x ${TMPDIR}/tmp.y ${TMPDIR}/tmp.var | \
gawk '($1>='${xmin_p}')&&($1<='${xmax_p}')&&($2>='${ymin_p}')&&($2<='${ymax_p}') \
      &&($3>'${seuil_var}') { print $1" "$2" "$3 }' > ${fic_ascii_tmp} || \
      { echo "ERREUR dans la lecture de ${fic_cartoprox_nc}" ; exit 1 ; }     
npts=`wc -l ${fic_ascii_tmp} | gawk '{print $1}'`
if [ ${npts} -eq 0 ] ; then
#il semblerait qu'il y ai un pb avec cette maille...
#echo -e "$ROUGE" "[${idom}/${ndom}] -> extrait MAILLE ${fic_cartoprox_nc} (${npts} points)" "$NORMAL"
echo -e "$ROUGE" "[${idom}/${ndom}] ${fic_cartoprox_nc} (${npts} points)" "$NORMAL"
else
#echo -e "$VERT" "[${idom}/${ndom}] -> extrait MAILLE ${fic_cartoprox_nc} (${npts} points)" "$NORMAL"
echo -e "$VERT" "[${idom}/${ndom}] ${fic_cartoprox_nc} (${npts} points)" "$NORMAL"
fi

#------------------------------------------------------------------------------------------------------------
#Gere la suremissions (calcul_suremissions.sh)
#------------------------------------------------------------------------------------------------------------
suremission_nc=${ZoomRes}/${periode}/${cartoprox_domaine}/suremission/${var}_${cartoprox_domaine}${selection}.nc
if [ -f ${suremission_nc} ] && [ ${do_suremis} -eq 1 ] ; then
  syntaxe="${extract_val_grille_exe} \
    -i ${suremission_nc} -xmin ${maille_xmin} -ymin ${maille_ymin} -dx ${fond_dx} -s ${fic_ascii_tmp} -var ${var}" 
  #echo $syntaxe
  #exit
  ${syntaxe} | gawk '{ print $1" "$2" "$3-$4 }' > ${fic_ascii_tmp}.tmp  \
  || { echo "$0: ERREUR de l extraction ${suremission_nc} -> ${fic_ascii_tmp}.tmp" ; exit 1 ; }
  mv ${fic_ascii_tmp}.tmp ${fic_ascii_tmp}
  head -n 1 ${fic_ascii_tmp} 
  echo -e "$VERT" "-> Calcul la suremission de ${suremission_nc}" "$NORMAL" 
#else 
#  echo -e "$ROUGE"  "-> SKIP la suremission (${suremission_nc})"  "$NORMAL"
fi
cat ${fic_ascii_tmp} >> ${fic_ascii}
#------------------------------------------------------------------------------------------------------------
else
echo "Warning: pas de fichier ${fic_cartoprox_nc}"
fi 
######################################################################
done # code_domaine
# <<<<< FIN BOUCLE SUR LES MINI-DOMAINES >>>>>>

if [ ! -f ${fic_ascii}  ] ; then
echo "Erreur sur ${fic_ascii}"
exit 1
fi

if [ `wc -l ${fic_ascii} | gawk '{print $1}'` -gt 0 ] ; then
echo "-> ${fic_ascii} OK"
echo "Points pour carte = `cat  ${fic_ascii} | wc -l` "
else
echo "Erreur"
exit 1
fi

####### ASCII ##
gawk '{print $1" "$2" "$3 }' ${fic_ascii} | sort -n > ${fic_xyz}
if [ -f ${fic_xyz} ] ; then
echo "-> ${fic_xyz} OK"  
rm ${fic_ascii}
else
echo "Probleme d ecriture - EXIT"
echo "${fic_ascii} -> ${fic_xyz}"
exit 1
fi

fi # [ ${do_export} -eq 1 ] ; then

echo ${fic_xyz}
#exit 0

######################################################################
# SORTIES GIF, GRD, NetCDF
######################################################################
fic_cdf=${fic_out_base}_${zoom_projection}.nc  #NetCDF grille
fic_grd=${fic_out_base}_${zoom_projection}.grd #SURFER GRD ASCII
fic_gif=${fic_out_base}_${zoom_projection}.gif #image GIF
fic_png=${fic_out_base}_${zoom_projection}.png #image GIF
fic_vm=${fic_out_base}_${zoom_projection}.vm   #Vertical Mapper
fic_kmz=${fic_out_base}_${zoom_projection}.kmz #Google Earth
fic_jp2=${fic_out_base}_${zoom_projection}.jp2 #Raster jpeg2000

######################################################################
#si les fichiers existent -> on les regenere
######################################################################
for fic in ${fic_cdf} ${fic_grd} ${fic_gif} ${fic_png} ${fic_vm} ${fic_kmz} ${fic_jp2}; do
  if [ -f ${fic} ] ; then
    echo -n "Supprime ${fic}..."
    rm ${fic}
    echo "OK"
  fi
done

######################################################################
# SORTIES GRD, NetCDF, Vertical Mapper
######################################################################
if [ ${do_grd} -eq 1 ] ; then
#Interpole les recepteurs X, Y, Z vers une grille automatique
#Export en fichier NetCDF
#Export en fichier SURFER GRD ASCII 
#Export en fichier Vertical Mapper

rm -rf ${fic_cdf} ${fic_grd} ${fic_vm} 
cd ${cartoprox}/utils/recept2grid
#Indiquer la taille de grille de sortie (qui peut �tre diff�rente de celle d'entr�e)
grille_dx_p=${grille_dx}
#echo ./recept2grid.sh ${fic_xyz} ${var_cdf} ${fic_cdf} ${fic_grd} ${fic_vm} ${grille_dx_p} ${xmin_p} ${ymin_p} ${xmax_p} ${ymax_p}
#echo "Recepteur -> grille NetCDF ${fic_cdf}"
#  if [ "$cartoprox_domaine" == "region_A6_A89" ] ; then 
#   ./recept2grid.sh ${fic_xyz} ${var_cdf} ${fic_cdf} ${fic_grd}
#  else
   ./recept2grid.sh ${fic_xyz} ${var_cdf} ${fic_cdf} ${fic_grd} ${fic_vm} ${grille_dx_p} ${xmin_p} ${ymin_p} ${xmax_p} ${ymax_p} #> /dev/null
#  fi
fi 

if [ ! -f ${fic_cdf} ] ; then
  do_gif=0
  do_kmz=0
  do_png=0
  do_jp2=0
fi

######################################################################
# SORTIES GIF
######################################################################
if [ ${do_gif} -eq 1 ] ; then
echo "Plot GIF ${fic_gif}"
rm -f ${fic_gif}
cd ${cartoprox}/utils/plot_ferret
xmin_plot=-9999999
ymin_plot=-9999999
xmax_plot=9999999
ymax_plot=9999999
./plot_ferret.sh ${fic_cdf} ${var_cdf} ${fic_gif} ${xmin_p} ${ymin_p} ${xmax_p} ${ymax_p} ${zoom} > /dev/null
fi

######################################################################
# SORTIES PNG
######################################################################
#PNG du domaine -> sans axes
if [ ${do_png} -eq 1 ] ; then
echo "Plot PNG ${fic_png}"
rm -f ${fic_png}
cd ${cartoprox}/utils/plot_ferret
./plot_ferret_highres_PNG.sh ${fic_cdf} ${var_cdf} ${fic_png}  > /dev/null
fi

######################################################################
# SORTIES KMZ
######################################################################
if [ ${do_kmz} -eq 1 ] ; then
echo "Plot KMZ ${fic_kmz}"
rm -f ${fic_kmz}
cd ${cartoprox}/utils/plot_ferret
./make_kmz.sh    ${fic_cdf} ${var_cdf} ${fic_kmz} > /dev/null 
fi 

######################################################################
# SORTIES JP2
######################################################################
if [ ${do_jp2} -eq 1 ] ; then
echo "Plot JP2 ${fic_jp2}"
rm -f ${fic_jp2}
cd ${cartoprox}/utils/raster_jp2    #_v2
echo "je suis dans `pwd`"
#echo "./mkjp2-rgb.sh ${xmin_p} ${xmax_p} ${ymin_p} ${ymax_p} ${fic_cdf} ${fic_jp2} ${var_cdf} ${epsg}"
echo "./mkjp2.sh ${xmin_p} ${xmax_p} ${ymin_p} ${ymax_p} ${fic_cdf} ${fic_jp2} ${var_cdf} ${epsg}"
#./mkjp2-rgb.sh ${xmin_p} ${xmax_p} ${ymin_p} ${ymax_p} ${fic_cdf} ${fic_jp2} ${var_cdf} ${epsg}
./mkjp2.sh ${xmin_p} ${xmax_p} ${ymin_p} ${ymax_p} ${fic_cdf} ${fic_jp2} ${var_cdf} ${epsg}
fi

######################################################################
# Liste des sorties
######################################################################
erreur=0
for fic in ${fic_xyz} ${fic_cdf} ${fic_grd} ${fic_gif} ${fic_png} ${fic_vm} ${fic_kmz} ${fic_jp2}; do
  if [ -f ${fic} ] ; then
    echo -e "${fic}" "$VERT" "OK" "$NORMAL"
  else
    echo -e "${fic}" "$ROUGE" "INDISPONIBLE" "$NORMAL"
    erreur=1
  fi
done

######################################################################
if [ ${erreur} -eq 0 ] ; then
  echo -n "Supprime le repertoire TMPDIR: ${TMPDIR}..."
  cd ${localdir}
  rm -rf ${TMPDIR}
  echo "OK"
fi
######################################################################
# menage
if test $do_rmgrd -eq 1
then
  echo rm fichiers grd et logjp2 de la selection
  rm -f ${fic_grd}
  rm -f ${fic_jp2}.log
fi
######################################################################
date
echo "***info: Fin du script $0"
