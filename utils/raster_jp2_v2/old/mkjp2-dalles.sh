#!/bin/bash

## Création d'un raster au format jpg2000 ##

kdu_home=/opt/kakadu

case $# in
  4)
    indir=$1
    out=$2
    var=$3
    epsg=$4
    ;;
  *)
    echo "usage: $0 indir out.jp2 var epsg"
    exit 1
    ;;
esac

# tmp dir
if [ "${cartoprox_scratch}" != "" ]; then
	tmpdir=${cartoprox_scratch}
else
	if [ "${SCRATCH}" == "" ]; then
		tmpdir=/tmp
	else
		tmpdir=${SCRATCH}
	fi
fi
tmpdir=${tmpdir}/`basename $0`.$$

# lecture de la liste des mailles
fnm=${indir}/liste.mailles
[ -e ${fnm} ] || { echo "erreur: pas de fichier ${fnm} !"; exit 1; }
cdfs=`awk '{ print "'${indir}/cdf/'export_cartoprox_"$4"_"$5"_"$6"_"$7".cdf" }' ${fnm}`
ncdf=`wc -l ${fnm} | cut -d" " -f1`
[ "${cdfs}" == "" ] && { echo "erreur: pas de fichier .cdf dans ${indir}/cdf !"; exit 1; }

mkdir -p ${tmpdir}
log=${out}.log
rm -f ${log}
echo "log > ${log}"
echo "tmp dir > ${tmpdir}"

# Bornes du raster final
#rxmin=
#rxmax=
#rymin=
#rymax=

dalles=""
i=1

echo "génération des rasters" | tee -a ${log}
for cdf in ${cdfs}; do
	
	# Information sur le raster
	xc=`basename ${cdf} | cut -d'_' -f3`
	yc=`basename ${cdf} | cut -d'_' -f4`
	dx=`basename ${cdf} | cut -d'_' -f5`
	dy=`basename ${cdf} | cut -d'_' -f6`
	xmin=`echo ${xc} ${dx} | gawk '{ print $1 - $2 / 2 }'`
	xmax=`echo ${xc} ${dx} | gawk '{ print $1 + $2 / 2 }'`
	ymin=`echo ${yc} ${dy} | gawk '{ print $1 - $2 / 2 }'`
	ymax=`echo ${yc} ${dy} | gawk '{ print $1 + $2 / 2 }'`
	
	# Min et max du raster global
	#[ -z "${rxmin}" ] && rxmin=${xmin}
	#[ -z "${rxmax}" ] && rxmax=${xmax}
	#[ -z "${rymin}" ] && rymin=${ymin}
	#[ -z "${rymax}" ] && rymax=${ymax}

	#[ ${xmin} -lt ${rxmin} ] && rxmin=${xmin}
	#[ ${ymin} -lt ${rymin} ] && rymin=${ymin}
	#[ ${xmax} -gt ${rxmax} ] && rxmax=${xmax}
	#[ ${ymax} -gt ${rymax} ] && rymax=${ymax}

	echo "traitement de ${cdf}" >>${log}
	
	[ -e echelle_${var}.txt ] || { echo "erreur: pas d'echelle de couleur pour la variable ${var} (echelle_${var}.txt) !"; exit 1; }
	
	# Application de l'echelle -> image
	cmd="gdaldem color-relief NETCDF:\"${cdf}\":${var} echelle_${var}.txt -alpha -of GTiff ${tmpdir}/img_${xc}_${yc}_${var}.tif"
	echo ${cmd} >>${log}
	${cmd} 1>>${log} 2>>${log}
	[ $? -ne 0 ] && { echo "erreur dans la création de l'image coloré img_${xc}_${yc}_${var}.tif"; exit 1; }
	
	# Inversion de l'axe Y -> image
	cmd="convert ${tmpdir}/img_${xc}_${yc}_${var}.tif -flip ${tmpdir}/img_${xc}_${yc}_${var}.flip.tif"
	echo ${cmd} >>${log}
	${cmd} 1>>${log} 2>>${log}
	[ $? -ne 0 ] && { echo "erreur dans la correction Y de l'image coloré img_${xc}_${yc}_${var}.flip.tif"; exit 1; }
	
	# Géoréférencement de l'image -> raster
	cmd="gdal_translate -a_ullr ${xmin} ${ymax} ${xmax} ${ymin} -a_srs EPSG:${epsg} -of GTiff ${tmpdir}/img_${xc}_${yc}_${var}.flip.tif ${tmpdir}/raster_${xc}_${yc}_${var}.tif"
	echo ${cmd} >>${log}
	${cmd} 1>>${log} 2>>${log}
	[ $? -ne 0 ] && { echo "erreur dans la création du raster raster_${xc}_${yc}_${var}.tif !"; exit 1; }

	rm -f ${tmpdir}/img_${xc}_${yc}_${var}.tif ${tmpdir}/img_${xc}_${yc}_${var}.flip.tif

	# Affichage
	echo " -> raster_${xc}_${yc}_${var}.tif >> ${i}/${ncdf} [ok]" | tee -a ${log}
	echo >>${log}
	dalles="${dalles}${tmpdir}/raster_${xc}_${yc}_${var}.tif "
	
	i=`expr ${i} + 1`

done

# Fusion des rasters
echo "fusion des rasters" | tee -a ${log}
cmd="gdal_merge.py -o ${tmpdir}/raster_merge_${var}.tif -of GTiff ${dalles}"
echo ${cmd} >>${log}
${cmd} 1>>${log} 2>>${log}
[ $? -ne 0 ] && { echo "erreur dans la fusion des rasters"; exit 1; }
echo >>${log}

# Information sur le raster final
gdalinfo ${tmpdir}/raster_merge_${var}.tif > ${tmpdir}/raster_merge_${var}.info
px=`cat ${tmpdir}/raster_merge_${var}.info | grep "^Size is" | sed 's/Size is//' | sed 's/ //g' | cut -d',' -f1`
py=`cat ${tmpdir}/raster_merge_${var}.info | grep "^Size is" | sed 's/Size is//' | sed 's/ //g' | cut -d',' -f2`
ox=`cat ${tmpdir}/raster_merge_${var}.info | grep "^Origin =" | sed 's/Origin = (//' | sed 's/)//g' | cut -d',' -f1`
oy=`cat ${tmpdir}/raster_merge_${var}.info | grep "^Origin =" | sed 's/Origin = (//' | sed 's/)//g' | cut -d',' -f2`
dx=`cat ${tmpdir}/raster_merge_${var}.info | grep "^Pixel Size =" | sed 's/Pixel Size = (//' | sed 's/)//g' | cut -d',' -f1`
dy=`cat ${tmpdir}/raster_merge_${var}.info | grep "^Pixel Size =" | sed 's/Pixel Size = (//' | sed 's/)//g' | cut -d',' -f2`
rxmin=${ox}
rxmax=`echo "${ox} + ${px} * ${dx}" | bc`
rymin=`echo "${oy} + ${py} * ${dy}" | bc`
rymax=${oy}

# Compression en jp2
echo "compression en jp2" | tee -a ${log}
cmd="${kdu_home}/kdu_compress -i ${tmpdir}/raster_merge_${var}.tif -o ${out}"
echo ${cmd} >>${log}
${cmd} 1>>${log} 2>>${log}
[ $? -ne 0 ] && { echo "erreur dans la création du raster jp2"; exit 1; }

echo " -> ${out}"
echo "    image = ${px}, ${py}"
echo "    xmin = ${rxmin}"
echo "    xmax = ${rxmax}"
echo "    ymin = ${rymin}"
echo "    ymax = ${rymax}"

# Création du fichier .tab pour Mapinfo
tab=`echo ${out} | sed 's/\.jp2$/\.tab/g'`
echo "!table" >${tab}
echo "!version 300" >>${tab}
echo "!charset WindowsLatin1" >>${tab}
echo >>${tab}
echo "Definition Table" >>${tab}
echo "  File \"`basename ${out}`\"" >>${tab}
echo "  Type \"RASTER\"" >>${tab}
echo "  (${rxmin},${rymax}) (0,0) Label \"Pt 1\"," >>${tab}
echo "  (${rxmax},${rymax}) (${px},0) Label \"Pt 2\"," >>${tab}
echo "  (${rxmax},${rymin}) (${px},${py}) Label \"Pt 3\"," >>${tab}
echo "  (${rxmin},${rymin}) (0,${py}) Label \"Pt 4\"" >>${tab}
#case ${epsg} in
#  32631)
#    echo "  CoordSys Earth Projection 8, 104, \"m\", 3, 0, 0.9996, 500000, 0" >>${tab}
#    echo "  Units \"m\"" >>${tab}
#  ;;
#  *)
#    echo "  Projection inconnu" >>${tab}
#    echo "  Units \"m\"" >>${tab}
#  ;;
#esac
coordsys=`python -c "from osgeo import osr ; sr = osr.SpatialReference() ; sr.ImportFromEPSG(${epsg}) ; print sr.ExportToMICoordSys()"`
isgeo=`python -c "from osgeo import osr ; sr = osr.SpatialReference() ; sr.ImportFromEPSG(${epsg}) ; print sr.IsGeographic()"`
echo "  CoordSys ${coordsys}" >>${tab}
case ${isgeo} in
  0) echo "  Units \"m\"" >>${tab} ;;
  *) echo "  Units \"degree\"" >>${tab} ;;
esac

# Suppression des fichiers temporaires
if [ ${clean} == "no" ]; then
        echo "Conservation du dossier temporaire ${tmpdir} !"
else   
        rm -rf ${tmpdir}
fi

# Fin
exit 0
