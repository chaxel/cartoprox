#!/bin/sh
##################################################
# Plot FERRET d'un fichier grille		 #
# E. Chaxel (echaxel@atmo-rhonealpes.org)	 #
# déc 2010					 #
##################################################

localdir=`pwd`
script=make_kmz.sh
limits=""
nx=320 
ny=360

case $# in
2)# entrees utilisateur
fichier_cdf=$1
polluant=$2
fichier_kmz=${localdir}/defaut.kmz
;;
3)# entrees utilisateur
fichier_cdf=$1
polluant=$2
fichier_kmz=$3
;;
*)
echo "Syntaxe: $0 fichier_cdf polluant"
exit
;;
esac

echo "polluant=${polluant}"

if [ "`ncdump -h ${fichier_cdf} | grep 'float lon('`" == "" ] ; then
  do_kmz=0
else
  do_kmz=1
  echo "************************ Genere KMZ *******************************"  
fi

if [ ${do_kmz} -eq 1 ] ; then

# repertoire temporaire
TMPDIR=${SCRATCH}/${script}.$RANDOM
while [ -d ${SCRATCH}/${script}.$RANDOM ] ; do
TMPDIR=${SCRATCH}/${script}.$RANDOM
done
echo "Travaille dans ${TMPDIR}"
mkdir -p ${TMPDIR}/files

#Palette par defaut
palette_defaut="${cartoprox}/data/ferret/palettes/rainbow3_cmyk.spk"

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
shade_opt="/d=1/title=\"${titre}\"" #/nokeys/nolabel/noaxis"

cd ${TMPDIR}

aspect=`echo ${nx} ${ny} | gawk '{print $2/$1}'`
echo "aspect=${aspect}"

echo "zoom_level=${zoom_level}"

#Genere l'aspect du graphique
axes=noaxes

#Determine les min et max de l'image
ncks -s '%f \n' -H -C -v lon ${fichier_cdf} > ${TMPDIR}/tmp.x
ncks -s '%f \n' -H -C -v lat ${fichier_cdf} > ${TMPDIR}/tmp.y

xmin=`gawk 'BEGIN { min =  1.E9 }; ($1 < min && $1 != "") {min = $1}; END { print min }' ${TMPDIR}/tmp.x`
ymin=`gawk 'BEGIN { min =  1.E9 }; ($1 < min && $1 != "") {min = $1}; END { print min }' ${TMPDIR}/tmp.y`
xmax=`gawk 'BEGIN { max = -1.E9 }; ($1 > max && $1 != "") {max = $1}; END { print max }' ${TMPDIR}/tmp.x`
ymax=`gawk 'BEGIN { max = -1.E9 }; ($1 > max && $1 != "") {max = $1}; END { print max }' ${TMPDIR}/tmp.y`

dlon=`echo ${xmin} ${xmax} | gawk '{print ($2 - $1)/20 }'`
dlat=`echo ${ymin} ${ymax} | gawk '{print ($2 - $1)/20 }'`

# Rogne de dlon/20 et dlat/20

xmin=`echo ${xmin} ${dlon} | gawk '{print $1 + $2 }'`
xmax=`echo ${xmax} ${dlon} | gawk '{print $1 - $2 }'`
ymin=`echo ${ymin} ${dlat} | gawk '{print $1 + $2 }'`
ymax=`echo ${ymax} ${dlat} | gawk '{print $1 - $2 }'`

limits="/hlimits=${xmin}:${xmax}/vlimits=${ymin}:${ymax}"

head ${TMPDIR}/tmp.x ${TMPDIR}/tmp.y

case ${axes} in
noaxes)
nolabel="/nolabel" # nolabel
nokey="/nokey" # nolabel
syntaxe_noaxes="PPL AXSET 0,0,0,0;PPL AXLABP 0,0; PPL TICS 0,0,0,0;PPL PLOT"
post_opt="crop"
;;
esac

syntaxe="${shade}${shade_opt}${level}${palette}${nolabel}${nokey}${limits} ${polluant},lon,lat"

echo "syntaxe ferret: ${syntaxe}"

#SYNTAXE FERRET DE BASE
cat << EOD > ${TMPDIR}/carto.jnl
!Zoom level=${zoom_level}
use "${fichier_cdf}"
set w /a=${aspect}
${syntaxe_noaxes}
${syntaxe}
EOD

#EXPORT SOUS FORMAT GIF/PS
ferret_mode=ps

case ${ferret_mode} in
gif)
img_ext="gif"
ferret_batch=gif
echo "frame/file=tmp.gif" >> ${TMPDIR}/carto.jnl
tmp_pict="files/carto_${polluant}.gif"
;;
ps)
img_ext="png"
ferret_batch="batch tmp.ps"
ps2raster="ps2raster -Tg" # PNG (voir man ps2raster)
tmp_pict="files/carto_${polluant}.png"
;;
esac


echo "APERCU de carto.jnl"
cat ${TMPDIR}/carto.jnl

#shade${shade_opt}${level}${palette} ${polluant}

#LANCE FERRET
echo "${TMPDIR}/carto.jnl"
echo "go carto.jnl"  >  ${TMPDIR}/ferret.com
echo "cancel d /a"   >> ${TMPDIR}/ferret.com		   
echo "exit"          >> ${TMPDIR}/ferret.com
 
echo "Execute FERRET"
cd ${TMPDIR}
ferret -${ferret_batch} < ${TMPDIR}/ferret.com > ${TMPDIR}/ferret.log

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

cat << EOM > doc.kml
<?xml version="1.0" encoding="UTF-8"?>
<kml xmlns="http://www.opengis.net/kml/2.2" xmlns:gx="http://www.google.com/kml/ext/2.2" xmlns:kml="http://www.opengis.net/kml/2.2" xmlns:atom="http://www.w3.org/2005/Atom">
<GroundOverlay>
	<name>Cartoprox - ${titre}</name>
	<color>99ffffff</color>
	<Icon>
		<href>${tmp_pict}</href>
		<viewBoundScale>0.75</viewBoundScale>
	</Icon>
	<LatLonBox>
		<north>${ymax}</north>
		<south>${ymin}</south>
		<east>${xmax}</east>
		<west>${xmin}</west>
	</LatLonBox>
</GroundOverlay>
</kml>
EOM

echo "<<<<<<<<<<<<<<<<<<<<< doc.kml >>>>>>>>>>>>>>>>>>>>>>"
cat doc.kml
echo "<<<<<<<<<<<<<<<<<<<<< doc.kml >>>>>>>>>>>>>>>>>>>>>>"

#Creer l'archive KMZ avec l'image et sa doc: aussi simple que ca
if [ -f doc.kml ] && [ -f ${tmp_pict} ] ; then
zip ${fichier_kmz} doc.kml ${tmp_pict}
echo "-> ${fichier_kmz}"
fi

cd ${localdir}

rm -rf ${TMPDIR}/files
rm ${TMPDIR}/*
rmdir ${TMPDIR}/

fi

