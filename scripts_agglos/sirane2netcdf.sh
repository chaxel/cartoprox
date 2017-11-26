#!/bin/sh
#############################################
# SIRANE2NetCDF
# Atmo Rhone-Alpes 2010
#############################################

# Usage: fournir le fichier X_UTM, Y_UTM, var

# Convertit ce fichier en NetCDF en utilisant les utilitaires de CARTOPROX
# ${cartoprox}/utils/recept2cdf

script=sirane2netcdf.sh
recept2cdf=${cartoprox}/utils/recept2cdf
localdir=`pwd`
conversion_exe=${prevalphome}/utils/conversion_geographique/src/conversion.exe

case $# in
3)
date=$1
fichier_recepteur=$2
var=$3 # nom de la variable
projection_in=lamb2
fichier_netcdf="sirane.nc"
;;
4)
date=$1
fichier_recepteur=$2
var=$3 # nom de la variable
projection_in=$4
fichier_netcdf="sirane.nc"
;;
5)
date=$1
fichier_recepteur=$2
var=$3 # nom de la variable
projection_in=$4 # lamb2/utm31/geo
fichier_netcdf=$5
;;
*)
echo "Syntaxe: $0 date[AAAAMMJJ] fichier_sirane[X,Y,Z] variable (fichier_netcdf) (projection_in[lamb2\utm31\geo]) "
exit
;;
esac

# repertoire temporaire
TMPDIR=${SCRATCH}/${script}.$RANDOM
while [ -d ${SCRATCH}/${script}.$RANDOM ] ; do
TMPDIR=${SCRATCH}/${script}.$RANDOM
done

echo "Travaille dans ${TMPDIR}"
mkdir -p ${TMPDIR}

#ETAPE 1 :Liste les recepteurs et 
nrecepts=`wc -l ${fichier_recepteur} | gawk '{print $1}'`
echo "nrecepts="${nrecepts}
if [ ${nrecepts} -gt 0 ] ; then
  echo "Trouve ${nrecepts} recepteurs"
else
  echo "ERREUR en lisant ${fichier_recepteur}"
  exit
fi

echo "-> Conversion du format SIRANE tab au format csv..."
# Conversion de coordonnées Lambert 2 -> UTM 31
if [ ! -f ${conversion_exe} ] ; then
  echo "ERREUR : ne trouve pas l utilitaire de conversion geographique"
  echo "->"${conversion_exe}
  exit 1
fi
gawk '{print $1" "$2}' ${fichier_recepteur} > ${TMPDIR}/recept_tmp.xy

case ${projection_in} in
lamb2)mv ${TMPDIR}/recept_tmp.xy ${TMPDIR}/recept_tmp.xylamb2;;
utm31)mv ${TMPDIR}/recept_tmp.xy ${TMPDIR}/recept_tmp.xyutm;;
geo)${conversion_exe} -geo -geoid WGS84 -i ${TMPDIR}/recept_tmp.xy -utm 31 -geoid WGS84 -o ${TMPDIR}/recept_tmp.xyutm || \
      { echo "$0: ERREUR dans la conversion de coordonnées" ; exit 1  ; };;
esac

# Generation LAMBERT 2 a partir d'UTM
if  [ ! -f ${TMPDIR}/recept_tmp.xylamb2 ] ; then
${conversion_exe} -utm 31 -geoid WGS84 -i ${TMPDIR}/recept_tmp.xyutm -l2 -geoid NTF -o ${TMPDIR}/recept_tmp.xylamb2 || \
      { echo "$0: ERREUR dans la conversion de coordonnées" ; exit 1  ; }
fi

# Generation d'UTM a partir de LAMBERT 2
if  [ ! -f ${TMPDIR}/recept_tmp.xyutm ] ; then
${conversion_exe}  -l2 -geoid NTF -i ${TMPDIR}/recept_tmp.xylamb2  -utm 31 -geoid WGS84 -o ${TMPDIR}/recept_tmp.xyutm || \
      { echo "$0: ERREUR dans la conversion de coordonnées" ; exit 1  ; }
fi

if  [ ! -f ${TMPDIR}/recept_tmp.xylamb2 ] || [ ! -f ${TMPDIR}/recept_tmp.xyutm ] ; then
  echo "Erreur de conversion"
  rm -rf ${TMPDIR}
  exit 1
fi

#echo ${nrecepts} > ${TMPDIR}/recept_tmp.list
paste ${TMPDIR}/recept_tmp.xylamb2 ${TMPDIR}/recept_tmp.xyutm | gawk '{print "pt_"NR" "$1" "$2" "$3" "$4 }' > ${TMPDIR}/recept_tmp.list

head ${TMPDIR}/recept_tmp.list

# COMPILATION
echo "-> Compilation"
cd ${recept2cdf}/src;make;cd ${localdir}

#ETAPE 2 : CREE UN FICHIER NetCDF AVEC LES RECEPTEURS, les COORDONNEES et les polluants
echo "-> Creation du NetCDF ${fichier_cdf}"
rm -rf ${TMPDIR}/pol.txt ; touch ${TMPDIR}/pol.txt 
echo ${var} >>  ${TMPDIR}/var.txt
${recept2cdf}/src/create_cdf.e ${TMPDIR}/recept_tmp.list ${TMPDIR}/var.txt ${fichier_netcdf} no_fond || \
      { echo "$0: ERREUR lors de la creation de ${fichier_netcdf}" ; exit 1 ; } #>> ${log} 2>&1


#ETAPE 3 : REMPLI FICHIER NetCDF AVEC LES CONCENTRATIONS DES POLLUANTS
echo "-> Remplissage NetCDF"
syntaxe="${recept2cdf}/src/write_cdf_sirane_Grille.e ${date} ${fichier_recepteur} ${fichier_netcdf}"

${syntaxe} || \
      { echo "$0: ERREUR de l executable de la conversion ${fichier_recepteur} -> ${fichier_netcdf}" ; exit 1 ; } #>> ${log} 2>&1

niter=`${cartoprox}/scripts/utils/info_netcdf.sh ${fichier_netcdf}`

if [ ${niter} -eq 0 ] ; then
  echo "$0: Pas de temps incorrect=0"
  exit 1
fi

# SUPPRESSION
rm -rf ${TMPDIR}

echo "OK... Fichier de sortie : ${fichier_netcdf}"
