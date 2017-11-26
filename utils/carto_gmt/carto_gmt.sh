#!/bin/sh

#######################################################################
# Script de cartographie par krigeage #
#######################################################################
localdir=`pwd`

# test
# carte_krigeage.sh recept_pts.txt 797000 2060000 200 200 20 20 5000

type_entree=cdf # cdf/ascii
do_krig=0
do_gmt=1

ui=m # m/km

############################

case $# in
6)
type_entree=cdf
fichier_netcdf=$1
var=$2
typ_op=$3
it1=$4
it2=$5
fichier_jpg=$6
export_cdf_ascii.sh ${fichier_netcdf} ${var} ${typ_op} ${it1} ${it2}  > input.txt
fichier_valeurs_point=input.txt
;;

3)
fichier_valeurs_point=$1
var=$2
fichier_jpg=$3
type_entree=ascii
;;

8)
fichier_valeurs_point=$1
kb2d_xmin=$2
kb2d_ymin=$3
kb2d_nx=$4
kb2d_ny=$5
kb2d_dx=$6
kb2d_dy=$7
kb2d_radius=$8;;
*)
echo "Syntaxe $0 fichier_valeurs_point xmin xmax nx ny dx dy radius"
echo "Syntaxe $0 fichier_netcdf var it_start it_stop fichier_jpg"
exit
;;
esac

echo "-> Trouve `wc -l ${fichier_valeurs_point} | gawk '{print $1}'` points"

if [ ${do_gmt} -eq 1 ] ; then

#Conversion geographique Lambert2 -> UTM31
cat ${fichier_valeurs_point} | gawk '{print $1" "$2" "$3}' > input.gmt

# definition des domaines
xmin=`gawk 'BEGIN { min =  1.E9 }; ($1 < min && $1 != "") {min = $1}; END { print int(min) }' input.gmt`
ymin=`gawk 'BEGIN { min =  1.E9 }; ($2 < min && $2 != "") {min = $2}; END { print int(min) }' input.gmt`
xmax=`gawk 'BEGIN { max = -1.E9 }; ($1 > max && $1 != "") {max = $1}; END { print int(max) }' input.gmt`
ymax=`gawk 'BEGIN { max = -1.E9 }; ($2 > max && $2 != "") {max = $2}; END { print int(max) }' input.gmt`

echo "xmin=${xmin}"
echo "ymin=${ymin}"
echo "xmax=${xmax}"
echo "ymax=${ymax}"

map=${localdir}/map.ps
map_jpg=${localdir}/map.jpg
data_file=${localdir}/input.gmt
pal=${localdir}/test.cpt

# position et taille du domaine de sortie
# 1. fixe la taille en x
# 2. determine la taille en y
size_x=14
size_y=`echo ${xmin} ${xmax} ${ymin} ${ymax} ${size_x} | gawk  '{ print $5 * ( $4 - $3 ) / ( $2 - $1 ) }'`
largeur=`echo ${xmin} ${xmax} ${ymin} ${ymax} | gawk  '{ print int( ( ( $2 - $1 ) + ( $4 - $3 ) )/2. ) }'`
echo "largeur="${largeur}" ${ui}"

# ramene a 21/29.7
size_y_max=14
facteur_page=`echo ${size_y} ${size_y_max}  | gawk ' { print $2 / $1 }'`
size_x=`echo ${size_x} ${facteur_page}  | gawk ' { print $1 * $2 }'`
size_y=`echo ${size_y} ${facteur_page}  | gawk ' { print $1 * $2 }'`
x_title=`echo ${size_x}   | gawk ' { print $1 / 2. }'`
y_title=`echo ${size_y}   | gawk ' { print $1 + 1   }'`

# position et taille de l'echelle
pos_x_pal=`echo ${size_x} | gawk ' { print $1 +.5 }' `
pos_y_pal=`echo ${size_y} | gawk ' { print $1 / 2. }' `
width_pal=0.2
length_pal=`echo ${size_y} | gawk ' { print $1 }' `

case ${ui} in
m)
if [ ${largeur} -gt 100000 ] ; then
echo "ERREUR: Largeur non supportee dans $0"
exit
fi
if [ ${largeur} -le 100000 ] ; then
dx1=10000
dx2=5000
fi
if [ ${largeur} -le 50000 ] ; then
dx1=5000
dx2=2500
fi
if [ ${largeur} -le 10000 ] ; then
dx1=2000
dx2=1000
fi
if [ ${largeur} -le 5000 ] ; then
dx1=1000
dx2=500
fi
pars="-R${xmin}/${xmax}/${ymin}/${ymax} -JX${size_x}/${size_y} -Bf${dx2}a${dx1}:easting_m:/f${dx2}a${dx1}:northing_m:WeSn";;
km)pars="-R${xmin}/${xmax}/${ymin}/${ymax} -JX${size_x}/${size_y} -Bf0.5a1:easting_km:/f0.5a1:northing_km:WeSn";;
esac
parsca="-D${pos_x_pal}/${pos_y_pal}/${length_pal}/${width_pal}"

echo ${pars}

#exit

cd $SCRATCH

# shading
echo "-> Genere la carte POSTSCRIPT ${map}"

mkdir -p ${localdir}/palettes
case ${var} in
NO2)palpar="-T0/400/20" ;;
NO) palpar="-T0/1000/50";;
NOX)palpar="-T0/1500/50";;
O3) palpar="-T0/200/10" ;;
esac

makecpt ${palpar} > ${localdir}/palettes/${var}.cpt
pal=${localdir}/palettes/${var}.cpt

## PALETTE AUTOMATIQUE###############################
valmin=`gawk 'BEGIN { min = +1.E9 }; ($3 < min && $3 != "") {min = $3}; END { print (int(min/10.)-1)*10 }' ${data_file}`
valmin=0
valmax=`gawk 'BEGIN { max = -1.E9 }; ($3 > max && $3 != "") {max = $3}; END { print (int(max/10.)+1)*10 }' ${data_file}`

valdx=100
if [ ${valmax} -lt 1000 ] ; then
valdx=50 
fi
if [ ${valmax} -lt 500  ] ; then
 valdx=20   
fi
if [ ${valmax} -lt 200  ] ; then
 valdx=10  
fi 
if [ ${valmax} -lt 100  ] ; then
 valdx=5  
fi 
if [ ${valmax} -lt 50   ] ; then
 valdx=2    
fi
if [ ${valmax} -lt 10   ] ; then
 valdx=1    
fi

#valdx=`echo ${valmax} | gawk ' { print int($1 / 10) }' `

palpar="-T${valmin}/${valmax}/${valdx}"

echo "Palettes="${palpar}
makecpt ${palpar} > ${localdir}/palettes/auto.cpt
pal=${localdir}/palettes/auto.cpt
####################################################

#basemap
echo psbasemap ${pars} -K 
psbasemap ${pars} -K > ${map}

#contour
echo pscontour ${data_file} ${pars} -C${pal} -O -I 
pscontour ${data_file} ${pars} -C${pal} -S -V -O -K -I  >> ${map}

# echelle
echo psscale -C${pal} ${parsca} -L -O -K
psscale -C${pal} ${parsca} -L -O -K   >> ${map}

# titre
echo "${x_title} ${y_title} 20 0 1 6 Conc. ${var} moyen iteration(s) $3->$4 (ug/m3)"
echo "${x_title} ${y_title} 20 0 1 6 Conc. ${var} moyen iteration $3->$4" | pstext ${pars} -O >> ${map} # -W -N -G200 -O >> ${map}

#echo "-> Genere la carte JPG ${map_jpg}"
rm -rf ${map_jpg}
ps2raster  ${map} 

mv ${map_jpg} ${fichier_jpg}
echo "-> Genere la carte JPG ${fichier_jpg}"
#echo "-> Genere la carte JPG ${fichier_jpg}2"
#rm ${map} 

# uncomment if you want labelled isocontours
#pscontour ${data_file} ${pars} -C${pal} -W1 -O -K -Af10o >> ${map}
# coast and border lines
#pscoast ${pars} -Bf2/f2WeSn -A5000 -W3 -Dl -N1 -O -K >> ${map}

fi
