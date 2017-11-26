#!/bin/bash

## Création d'un raster au format jpg2000 ##

kdu_home=/opt/kakadu

case $# in
  8)
    xmin=$1
    xmax=$2
    ymin=$3
    ymax=$4
    fni=$5
    fno=$6
    var=$7
    epsg=$8
    ;;
  *)
    echo "usage: $0 xmin xmax ymin ymax fic_nc fic_jp2 var epsg"
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

mkdir -p ${tmpdir}
log=${fno}.log
rm -f ${log}
echo "log > ${log}"
echo "tmp dir > ${tmpdir}"

echo "génération du raster jp2" | tee -a ${log}
echo " + xmin = ${xmin}" | tee -a ${log}
echo " + xmax = ${xmax}" | tee -a ${log}
echo " + ymin = ${ymin}" | tee -a ${log}
echo " + ymax = ${ymax}" | tee -a ${log}
echo >>${log}

[ -e echelle_${var}.txt ] || { echo "erreur: pas d'echelle de couleur pour la variable ${var} (echelle_${var}.txt) !"; exit 1; }

cmd="gdaldem color-relief NETCDF:\"${fni}\":${var} echelle_${var}.txt -of GTiff ${tmpdir}/img.tif"
echo ${cmd} >>${log}
${cmd} 1>>${log} 2>>${log}
[ $? -ne 0 ] && { echo "erreur dans la création de l'image coloré"; exit 1; }
echo >>${log}

cmd="convert ${tmpdir}/img.tif -flip ${tmpdir}/img.flip.tif"
echo ${cmd} >>${log}
${cmd} 1>>${log} 2>>${log}
[ $? -ne 0 ] && { echo "erreur dans la correction Y de l'image coloré"; exit 1; }
echo >>${log}

cmd="gdal_translate -a_ullr ${xmin} ${ymax} ${xmax} ${ymin} -a_srs EPSG:${epsg} -of GTiff ${tmpdir}/img.flip.tif ${tmpdir}/raster.tif"
echo ${cmd} >>${log}
${cmd} 1>>${log} 2>>${log}
[ $? -ne 0 ] && { echo "erreur dans la création du raster !"; exit 1; }
echo >>${log}

cmd="${kdu_home}/kdu_compress -i ${tmpdir}/raster.tif -o ${fno}"
echo ${cmd} >>${log}
${cmd} 1>>${log} 2>>${log}
[ $? -ne 0 ] && { echo "erreur dans la création du raster jp2"; exit 1; }
echo >>${log}

rm -rf ${tmpdir}
echo "-> création du raster ${fno}" | tee -a ${log}
exit 0
