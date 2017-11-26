#!/bin/bash
#
# CARTOPROX
# export des données en un point
#
## INCLUDE ###########################################
localdir=`pwd`

#Calcule la moyenne a un recepteur pour une periode donnée
#Version avec fichier cartoprox*.*.nc (déc. 2010)

# Recupere les infos sur la grille CARTOPROX
xmin=${cartoprox_xmin}
ymin=${cartoprox_xmax}
nx=${cartoprox_nx}
ny=${cartoprox_ny}
dx=${cartoprox_dx}
dy=${dx}

deb_h=00
fin_h=00

ihour=0

case $# in
11)
irecept=$1
xp=$2
yp=$3
polluant=$4
deb_j=$5
deb_h=$6
fin_j=$7
fin_h=$8
nrs=$9
nre=${10}
ihour=${11}
date_format=std
;;
12)
irecept=$1
xp=$2
yp=$3
polluant=$4
deb_j=$5
deb_h=$6
fin_j=$7
fin_h=$8
nrs=$9
nre=${10}
ihour=${11}
date_format=${12}
;;
13)
irecept=$1
xp=$2
yp=$3
polluant=$4
deb_j=$5
deb_h=$6
fin_j=$7
fin_h=$8
nrs=$9
nre=${10}
ihour=${11}
date_format=${12}
code_domaine=${13}
;;
*)
echo "Syntaxe $0 x_utm y_utm irecept polluant deb_j (deb_h) fin_j (fin_h)"
exit 1
esac

# declaration de l'année de calcul
if [ "${annee}" == "" ] ; then
annee=`date -d ${deb_j} +%Y`
fi

## INCLUDE ###########################################
source ${cartoprox}/cartoprox.inc ${annee}

#polluant=NO2
typ_op="avg"

# recherche du code_domaine  si non fourni
# xc = xmin + int((xp-xmin)/dx+1 - 1) * dx + dx/2
if [ "${code_domaine}" == "" ] ; then
ix=`echo "${xmin} ${dx} ${xp}" | gawk '{ print int($1 + int(($3-$1)/$2+1-1) * $2 + $2/2) }'`
iy=`echo "${ymin} ${dy} ${yp}" | gawk '{ print int($1 + int(($3-$1)/$2+1-1) * $2 + $2/2) }'`
code_domaine=${ix}_${iy}
fi
#echo "${ix} ${iy}"

source ./utils/infos_domaine.sh ${code_maille} ${code_domaine} ${annee} 1> /dev/nul 2>&1

case ${ihour} in
0)it=${nre};;
1)it=${nrs};;
esac

while [ ${it} -le ${nre} ] ; do

case ${ihour} in
0)
it1=${nrs}
it2=${nre}
;;
1)
it1=${it}
it2=${it}
;;
esac

# Valeur du calcul SIRANE
if [ "${it1}" != "" ] && [ "${it2}" != "" ]  ; then
  it1_nc=`expr ${it1} \- 1`
  it2_nc=`expr ${it2} \- 1`  
  interval="-d Time,${it1_nc},${it2_nc}"
else
  interval=""
fi

irecept_nc=`expr ${irecept} \- 1`

# Fichier de resultat CARTOPROX
case ${polluant} in
NO|NO2|O3)prefixnc=cartoprox_nox;;
${var_pm})prefixnc=cartoprox_${var_pm};;
esac
run=${deb_j}${deb_h}_${fin_j}${fin_h}
fic_netcdf=${ChemRes}/${prefixnc}.${run}.nc

# calcul de la moyenne, minimum, maximum des valeurs de ${var} sur la periode aux n recepteurs 
if [ ${it1} -ne ${it2} ] ; then
ncra -y ${typ_op} ${interval} -O -v ${polluant} ${fic_netcdf} tmp_${code_maille}.nc   || \
   { echo "export_cdf_ascii.sh: ERREUR dans la lecture de ${fic_netcdf}" ; exit 1 ; }
conc_cartoprox=`ncks -s '%f \n' -H -C -d  Point,${irecept_nc},${irecept_nc} -v ${polluant}        tmp_${code_maille}.nc`
#conc_fond=`     ncks -s '%f \n' -H -C -d Point,${irecept_nc},${irecept_nc} -v ${polluant}_fond   tmp_${code_maille}.nc`
#conc_prevalp=`  ncks -s '%f \n' -H -C -d Point,${irecept_nc},${irecept_nc} -v ${polluant}_grille tmp_${code_maille}.nc`
rm tmp_${code_maille}.nc
else
conc_cartoprox=`ncks -s '%f \n' -H -C -d  Point,${irecept_nc},${irecept_nc} -v ${polluant} -d Time,${it1_nc},${it2_nc} ${fic_netcdf}`
#conc_fond=`     ncks -s '%f \n' -H -C -v ${polluant}_fond   -d  Point,${irecept_nc},${irecept_nc} -d Time,${it1_nc},${it2_nc} ${fic_netcdf}`
#conc_prevalp=`  ncks -s '%f \n' -H -C -v ${polluant}_grille -d  Point,${irecept_nc},${irecept_nc} -d Time,${it1_nc},${it2_nc} ${fic_netcdf}`
fi

# Calcul des concentrations de SIRANE et de PROX
#conc_sirane=`echo ${conc_fond} ${conc_prevalp} ${conc_cartoprox}  | gawk '{print $3 - $2 + $1 }'`
#conc_prox=`echo ${conc_prevalp} ${conc_cartoprox} | gawk '{print $2 - $1 }'`

# Affichage
case ${ihour} in
1)
case ${date_format} in
std)echo "`./utils/iter2date.sh ${it} ${annee}`;${conc_prevalp};${conc_sirane};${conc_fond};${conc_cartoprox}";;
no_date)echo "${conc_prevalp};${conc_sirane};${conc_fond};${conc_cartoprox}";;
esac
;;
0)
echo "${conc_prevalp};${conc_sirane};${conc_fond};${conc_cartoprox}"
;;
esac

it=`expr ${it} \+ 1`

done

exit 0
