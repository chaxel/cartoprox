#!/bin/ksh
#
# calcul l'occupation des sols a partir du fichier de LANDUSE de PREVALP
# indique si la maille est urbaine = 1 ou rurale = 0
# 
#############################################

localdir=`pwd`

fic_recept=$1
verbose=$2

## INCLUDE ###########################################
# Charge les paramètres pour l'année N
source ${cartoprox}/cartoprox.inc 9999 > /dev/null || \
  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; } 

ix=4
iy=5
ivar=4
var_lu=GroundClass
landuse_fic=${landuse_fic}

# Fraction de la maille à partir de laquelle elle est considérée comme urbaine
#fraction_urbain=50 # %

if [ ! -f  ${fic_recept} ] ; then
echo "***ERREUR: pas de fichier ${fic_recept}"
exit
fi

if [ ! -f  ${landuse_fic} ] ; then
echo "***ERREUR: pas de fichier ${landuse_fic}"
exit
fi

#selectionne les minidomaines dans le domaine de PLOT
heure_now=`date +%H%M%S`
gawk '{ print $'${ix}'" "$'${iy}'" 0." }' ${fic_recept} > ${fic_recept}.tmp${heure_now}

syntaxe="${extract_val_grille_exe} \
    -i ${landuse_fic} -xmin ${landuse_xmin} -ymin ${landuse_ymin} -dx ${landuse_dx} -s ${fic_recept}.tmp${heure_now} -var ${var_lu}"

ntotal=`wc -l ${fic_recept}.tmp${heure_now} | gawk '{print $1}'`

# Compte les mailles urbaines de Corine Land cover
# tout ce qui est inférieur à CLC <= 9 est urbain
urbain_clc=9
nurbain=`${syntaxe} | gawk 'BEGIN { sum = 0 };($'${ivar}'< '${urbain_clc}' ){ sum += 1 }; END { print sum }'`
urbain_deci=`echo "${nurbain} ${ntotal}" | gawk '{ print int($1 / $2 * 100. )  }'`
urbain=`echo "${nurbain} ${ntotal}" | gawk '{ print int( $1 / $2 + 1 - '${fraction_urbain}'/100. ) }'`

#VERBOSE
if [ "${verbose}" == "-v" ] ; then
echo "urbain=${urbain}"
echo "nurbain=${nurbain}/${ntotal}"
echo "fraction urbaine = ${urbain_deci} % (seuil ${fraction_urbain} %)"
else
echo ${urbain}
fi

rm ${fic_recept}.tmp${heure_now}
