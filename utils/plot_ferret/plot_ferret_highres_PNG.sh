#!/bin/sh
##################################################
# Plot FERRET d'un fichier grille		 #
# E. Chaxel (echaxel@atmo-rhonealpes.org)	 #
# déc 2010					 #
##################################################

localdir=`pwd`
script=plot_ferret_V2.sh
limits=""
fac_km=1000.
nx=1
ny=1

case $# in
2)# entrees utilisateur
fichier_cdf=$1
polluant=$2
fichier_gif=${localdir}/defaut.gif
;;
3)# entrees utilisateur
fichier_cdf=$1
polluant=$2
fichier_gif=$3
;;
7|8)# entrees utilisateur
fichier_cdf=$1
polluant=$2
fichier_gif=$3
xmin_plot=`echo $4 | gawk '{print $1 /'${fac_km}'}'`
ymin_plot=`echo $5 | gawk '{print $1 /'${fac_km}'}'`
xmax_plot=`echo $6 | gawk '{print $1 /'${fac_km}'}'`
ymax_plot=`echo $7 | gawk '{print $1 /'${fac_km}'}'`
limits="/hlimits=${xmin_plot}:${xmax_plot}/vlimits=${ymin_plot}:${ymax_plot}"
zoom_level=$8
;;
*)
echo "Syntaxe: $0 fichier_cdf polluant"
exit
;;
esac

echo "polluant=${polluant}"

# repertoire temporaire
TMPDIR=${SCRATCH}/${script}.$RANDOM
while [ -d ${SCRATCH}/${script}.$RANDOM ] ; do
TMPDIR=${SCRATCH}/${script}.$RANDOM
done
echo "Travaille dans ${TMPDIR}"
mkdir -p ${TMPDIR}

tmp_gif=carto_${polluant}.gif

#Palette par defaut
palette_defaut="${cartoprox}/data/ferret/palettes/rainbow3_cmyk.spk"

#Par defaut
palette="/palette=\"${palette_defaut}\""
valeur_limite[1]=""

# FERRET
case ${polluant} in
no2_moy_an)
titre="Moyenne annuelle NO2 (microg/m3)"
level="/level=(0,50,2)(150)"
valeur_limite[1]=40
;;
pm10_moy_an)
titre="Moyenne annuelle PM10 (microg/m3)"
level="/level=(0,50,2)(150)"
valeur_limite[1]=40
;;
pm25_moy_an)
titre="Moyenne annuelle PM25 (microg/m3)"
level="/level=(0,30,2)(100)"
valeur_limite[1]=25
;;
nb_dep_50_jour)
titre="Nombre de jours de depassement PM10>50 microg/m3"
level="/level=(0,40,2)(150)"
valeur_limite[1]=35
;;
nb_dep_200_jour)
titre="Nombre de jours de depassement NO2>200 microg/m3"
level="/level=(0,100,5)(200)"
valeur_limite[1]=0
;;
NO2|NO)
titre="Concentration max. en ${polluant}"
level="/level=(0,200,10)(400)"
;;
O3)
titre="Concentration max. en ${polluant}"
level="/level=(0,180,10)(240)"
;;
${var_pm})
titre="Concentration moyenne en ${polluant}"
level="/level=(0,80,5)(125)"
;;
*)titre="${polluant}";;
esac

shade="fill"
contour="contour/color=red"
shade_opt="/d=1/title=\"${titre}\"" #/nokeys/nolabel/noaxis"

cd ${TMPDIR}
mkdir -p ${TMPDIR}/files

output_mode=gif
aspect=`echo ${nx} ${ny} | gawk '{print $2/$1}'`
echo "aspect=${aspect}"

echo "zoom_level=${zoom_level}"

#syntaxe="${shade}${shade_opt}${level}${palette}${limits} ${polluant},easting/${fac_km},northing/${fac_km}"

#Genere l'aspect du graphique
axes=noaxes

draw_overlay=1
draw_villes=1
draw_routes=1
draw_fleuves=1
draw_region=1

draw_logo=1
post_opt="aucune"

#Configuration sans axes (pour Google Earth)
case ${axes} in
noaxes)
nolabel="/nolabel" # nolabel
nokey="/nokey" # nolabel
syntaxe_noaxes="PPL AXSET 0,0,0,0;PPL AXLABP 0,0; PPL TICS 0,0,0,0;PPL PLOT"
draw_villes=0
draw_logo=0
draw_overlay=0
post_opt="crop"
;;
esac

#N'affiche les communes qu'à partir du zoom 11 (Communes aux hautes resolution)
if [ "${zoom_level}" == "" ] ; then
  zoom_level=99
fi
if [ ${zoom_level} -ge 5 ] ; then
draw_communes=1
else
draw_communes=0
fi

#N'affiche la valeur limite qu'à partir du zoom 12
if [ ${zoom_level} -ge 6 ] && [ ${zoom_level} -ne 99 ] && [ "${axes}" != "noaxes" ] && [ "${valeur_limite[1]}" != "" ] ; then
syntaxe_VL[1]="${contour}/ov/lev=(${valeur_limite[1]})/nolabel${nokey} ${polluant},easting/${fac_km},northing/${fac_km}"
fi

#Syntaxe principale
syntaxe="${shade}${shade_opt}${level}${palette}${nolabel}${nokey} ${polluant},easting/${fac_km},northing/${fac_km}"

overlay_dir=${prevalphome}/overlay/cdf

#Thickness en fonction du niveau de zoom
thick_fleuves=1
thick_depts=1
color_communes="black"
color_depts="black"
max_villes=10
case ${zoom_level} in
1)
max_villes=3
;;
2)
max_villes=5
;;
3)
max_villes=8
;;
4)
max_villes=15
;;
5|6)
thick_fleuves=2
thick_depts=2
color_depts="black"
;;
7|8|9)
thick_fleuves=3
thick_depts=2
color_depts="black"
;;
esac

echo "syntaxe ferret: ${syntaxe}"

#SYNTAXE FERRET DE BASE
cat << EOD > ${TMPDIR}/carto.jnl
!Zoom level=${zoom_level}
use "${fichier_cdf}"
set w /a=${aspect}
${syntaxe_noaxes}
${syntaxe}
${syntaxe_VL[1]}
EOD

#OVERLAY (fleuves, limites administratives, routes)
iover=2

#polygones disponibles ?
if [ ! -d ${overlay_dir} ] ; then
  draw_overlay=0
fi

#Ajoute les polygones
if [ ${draw_overlay} -eq 1 ] ; then

if [ ${draw_fleuves} -eq 1 ] ; then
overlay[${iover}]=${overlay_dir}/utm31_hydro.cdf
cat << EOD >> ${TMPDIR}/carto.jnl
use "${overlay[${iover}]}"
plot/ov/vs/line/color=blue/thickness=${thick_fleuves}/nolabel/d=${iover}  xutm31/1000,yutm31/1000 !Fleuves
EOD
iover=`expr ${iover} \+ 1`
fi

if [ ${draw_communes} -eq 1 ] ; then
overlay[${iover}]=${overlay_dir}/utm31_RA_communes.cdf
cat << EOD >> ${TMPDIR}/carto.jnl
use "${overlay[${iover}]}"
plot/ov/vs/line/color=${color_communes}/thickness=1/nolabel/d=${iover}  xutm31/1000,yutm31/1000       !Communes
EOD
iover=`expr ${iover} \+ 1`
fi

if [ ${draw_region} -eq 1 ] ; then
overlay[${iover}]=${overlay_dir}/utm31_RA_depts.cdf
cat << EOD >> ${TMPDIR}/carto.jnl
use "${overlay[${iover}]}"
plot/ov/vs/line/color=${color_depts}/thickness=${thick_depts}/nolabel/d=${iover}   xutm31/1000,yutm31/1000      !Limites departements
EOD
iover=`expr ${iover} \+ 1`
fi

fi

#
if [ ${draw_villes} -eq 1 ] ; then
#LABELS DES VILLES (x27 = apostrophe)
${localdir}/extract_villes_bd.sh ${xmin_plot} ${ymin_plot} ${xmax_plot} ${ymax_plot} ferret_utm31_km ${max_villes} | sed 's/\x27//g' >> ${TMPDIR}/carto.jnl
fi

#EXPORT SOUS FORMAT GIF##########################################################
echo "frame/file=${tmp_gif}" >> ${TMPDIR}/carto.jnl

echo "APERCU de carto.jnl"
cat ${TMPDIR}/carto.jnl

#shade${shade_opt}${level}${palette} ${polluant}

#LANCE FERRET##########################################################
echo "${TMPDIR}/carto.jnl"
echo "go carto.jnl"  >  ${TMPDIR}/ferret.com
echo "cancel d /a"   >> ${TMPDIR}/ferret.com		   
echo "exit"          >> ${TMPDIR}/ferret.com

echo "Execute FERRET"
cd ${TMPDIR}

#EXPORT SOUS FORMAT GIF/PS
ferret_mode=ps

case ${ferret_mode} in
gif)
img_ext="gif"
ferret_batch=gif
echo "frame/file=tmp.gif" >> ${TMPDIR}/carto.jnl
tmp_pict="files/carto_${polluant}.gif"
fichier_gif=${fichier_gif}
;;
ps)
img_ext="png"
ferret_batch="batch tmp.ps"
ps2raster="ps2raster -Tg" # PNG (voir man ps2raster)
tmp_pict="files/carto_${polluant}.png"
fichier_gif=`echo ${fichier_gif} | sed 's/.gif/.png/'`
draw_logo=0
;;
esac

ferret -${ferret_batch} < ${TMPDIR}/ferret.com > ${TMPDIR}/ferret.log

#POST-TRAITEMENT##########################################################
case ${ferret_mode} in
ps)
#converti en png
${ps2raster} -A tmp.ps
#Coupe l'image pour ne pas avoir la legende
convert -trim +repage tmp.${img_ext} ${tmp_pict}
;;
gif)
#Coupe l'image pour ne pas avoir la legende
convert -trim +repage tmp.${img_ext} ${tmp_pict}
;;
esac

#POST-TRAITEMENT##########################################################
mv ${tmp_pict} ${fichier_gif}

#LOGO##########################################################
if [ ${draw_logo} -eq 1 ] ; then
# LOGO
logo_gif=${localdir}/logo_gie_673.gif
composite_script=/usr/local/bin/composite
save_old=no
echo "Ajoute logo GIE Atmo-RA sur ${tmp_gif}"
rm -rf ${TMPDIR}/temp.gif
${composite_script} ${logo_gif} ${tmp_gif} temp.gif
if  [ -f  ${TMPDIR}/temp.gif ] ; then
  if [ "${save_old}" == "yes" ] ; then
    mv ${tmp_gif} ${file_gif}.old
  fi
  mv ${TMPDIR}/temp.gif ${tmp_gif}
fi
fi

cd ${localdir}
rm -rf ${TMPDIR}

echo "-> ${fichier_gif}"
exit 0
