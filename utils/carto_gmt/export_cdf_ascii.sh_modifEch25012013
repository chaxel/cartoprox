#!/bin/sh -f

#source $HOME/.bashrc

. ${cartoprox}/cartoprox.inc > /dev/null 2>&1

# export les resultats d'un fichier en Lambert2 en UTM
script=export_cdf_ascii.sh
#ncra=${NCO}/bin/ncra
#ncks=${NCO}/bin/ncks
#conversion_exe=${cartoprox}/utils/conversion_geographique/src/conversion.exe

if [ $# -lt 2 ] ; then
echo "Specifier $0 fichier_netCDF nom_variable pas_de_temps1 pas_de_temps2"
exit
fi

case $# in 
1|2|3|4|5)
fichier_netcdf=$1
var=$2
typ_op=$3 # avg/max
it1=$4
it2=$5
;;
*)
echo "Usage: $0 netcdf variable (typ_op[avg/max/min] it1[1..n] it2[1..n])"
exit
;;
esac

# repertoire temporaire ###########################################
nom_fic=`basename ${fichier_netcdf}`
if [ "${cartoprox_scratch}" != "" ] ; then
  SCRATCH=${cartoprox_scratch}
fi
TMPDIR=${SCRATCH}/${script}_${nom_fic}.$RANDOM
while [ -d ${SCRATCH}/${script}_${nom_fic}.$RANDOM ] ; do
TMPDIR=${SCRATCH}/${script}_${nom_fic}.$RANDOM
done
#echo "Travaille dans ${TMPDIR}"
mkdir -p ${TMPDIR}

# MOYENNES/MIN/MAX
#if [ "${typ_op}" == "" ]  ; then
#  typ_op="avg"  
#fi
if [ "${it1}" != "" ] && [ "${it2}" != "" ]  ; then
  it1=`expr ${it1} \- 1`
  it2=`expr ${it2} \- 1`  
  interval="-d Time,${it1},${it2}"
else
  interval=""
fi

rm -f ${TMPDIR}/tmp.x ${TMPDIR}/tmp.y ${TMPDIR}/tmp.var ${TMPDIR}/tmp.nc ${TMPDIR}/tmp.xy_utm

# calcul de la moyenne, minimum, maximum des valeurs de ${var} sur la periode aux n recepteurs 
if [ "${typ_op}" != "" ] ; then
${ncra} -y ${typ_op} ${interval} -O -v ${var} ${fichier_netcdf} ${TMPDIR}/tmp.nc || \
   { echo "export_cdf_ascii.sh: ERREUR dans la lecture de ${fichier_netcdf}" ; exit 1 ; }
else
  ln -sf ${fichier_netcdf} ${TMPDIR}/tmp.nc
fi

#Recherche des noms des X, Y
name_x=`ncdump -h ${fichier_netcdf} | grep easting  | head -n 1 | sed -e 's/(/ /' | sed -e 's/float/ /' | sed -e 's/integer/ /' | awk '{print $1}'`
name_y=`ncdump -h ${fichier_netcdf} | grep northing | head -n 1 | sed -e 's/(/ /' | sed -e 's/float/ /' | sed -e 's/integer/ /' | awk '{print $1}'`

#echo "ncdump -h ${fichier_netcdf} | grep -o northing | head -n 1"

# Directement en UTM (version 2.1)
case ${var} in
*_prox|*_fond)
${ncks} -s '%f' -H -C -v ${var} ${TMPDIR}/tmp.nc  
;;
*)
${ncks} -s '%f \n' -H -C -v ${name_x} ${fichier_netcdf} > ${TMPDIR}/tmp.x
${ncks} -s '%f \n' -H -C -v ${name_y} ${fichier_netcdf} > ${TMPDIR}/tmp.y
${ncks} -s '%f \n' -H -C -v ${var}    ${TMPDIR}/tmp.nc  > ${TMPDIR}/tmp.var
#${ncks} -s '%f \n' -H -C -v area_pts      ${fichier_netcdf} > ${TMPDIR}/tmp.area
paste ${TMPDIR}/tmp.x ${TMPDIR}/tmp.y > ${TMPDIR}/tmp.xy_utm
 
# Affiche a l'ecran le contenu du fichier X, Y, Z 
paste ${TMPDIR}/tmp.xy_utm ${TMPDIR}/tmp.var | \
   gawk '( $3 != "inf" && $3 != "nan" && $1 != "" && $3 >= 0.) { printf "%10.2f %s %10.2f %s %12.3f\n",$1," ",$2," ",$3 }' 
#paste ${TMPDIR}/tmp.xy_utm ${TMPDIR}/tmp.var | \
#   gawk '( $1 != "" && $3 >= 0.) { printf "%10.2f %s %10.2f %s %12.3f\n",$1," ",$2," ",$3 }'

;;
esac


rm -f ${TMPDIR}/tmp.x ${TMPDIR}/tmp.y ${TMPDIR}/tmp.var ${TMPDIR}/tmp.nc ${TMPDIR}/tmp.xy_utm
rmdir ${TMPDIR}
