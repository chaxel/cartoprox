#!/bin/ksh
#
# CARTOPROX
# export des données en un point
#
## INCLUDE ###########################################
localdir=`pwd`

# Recupere les infos sur la grille CARTOPROX
#cartoprox_xmin=550000.
#cartoprox_xmax=4884000.
#cartoprox_nx=94
#cartoprox_ny=92
#cartoprox_dx=3000.

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

prefixnc=sirane_nox

run=${deb_j}${deb_h}_${fin_j}${fin_h}
#nrs=`./utils/date2iter.sh  ${deb_j} ${deb_h}`
#nre=`./utils/date2iter.sh  ${fin_j} ${fin_h}`

# extrait le niveau de fond
case ${polluant} in
NO)pol_fac=1.25;;
NO2)pol_fac=1.91;;
O3)pol_fac=2.0;;
esac

case ${polluant} in
NO)icol=3;;
NO2)icol=4;;
O3)icol=5;;	 
esac

source ./utils/infos_domaine.sh ${code_maille} ${code_domaine} ${annee} > log.txt
rm log.txt

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

#echo $it1 $it2

# Valeur de PREVALP interpolée
syntaxe="${extract_val_grille_exe} \
    -i ${fond_fic} -xmin ${fond_xmin} -ymin ${fond_ymin} -dx ${fond_dx} -x ${xp} -y ${yp} -it1 ${it1} -it2 ${it2} -var ${polluant}"    
conc_prevalp=`${syntaxe} | gawk '{print $4*'${pol_fac}' }'`

# Valeur du fond du calcul SIRANE
fic_fond=${sirane_prevalp}/${code_maille}/${code_domaine}/fond_${annee}_prevalp.dat
conc_fond=`../utils/carto_gmt/export_ascii.sh ${fic_fond} ${icol} ${typ_op} ${it1} ${it2}`

# Valeur du calcul SIRANE
fic_netcdf=${ChemRes}/${prefixnc}.${run}.nc
if [ "${it1}" != "" ] && [ "${it2}" != "" ]  ; then
  it1_nc=`expr ${it1} \- 1`
  it2_nc=`expr ${it2} \- 1`  
  interval="-d Time,${it1_nc},${it2_nc}"
else
  interval=""
fi
irecept_nc=`expr ${irecept} \- 1`
# calcul de la moyenne, minimum, maximum des valeurs de ${var} sur la periode aux n recepteurs 
if [ ${it1} -ne ${it2} ] ; then
ncra -y ${typ_op} ${interval} -O -v ${polluant} ${fic_netcdf} tmp_${code_maille}.nc   || \
   { echo "export_cdf_ascii.sh: ERREUR dans la lecture de ${fic_netcdf}" ; exit 1 ; }
conc_sirane=`ncks -s '%f \n' -H -C -d Point,${irecept_nc},${irecept_nc} -v ${polluant} tmp_${code_maille}.nc`
rm tmp_${code_maille}.nc
else
conc_sirane=`ncks -s '%f \n' -H -C -v ${polluant} -d  Point,${irecept_nc},${irecept_nc} -d Time,${it1_nc},${it2_nc} ${fic_netcdf}`
fi

#../utils/carto_gmt/export_cdf_ascii.sh ${pts} ${fic_netcdf} ${polluant} ${typ_op} ${it1} ${it2} > recept.tmp
#${F90} -o ./utils/interp_bilin.exe ./utils/interp_bilin.f90
#conc_sirane=`./utils/interp_bilin.exe recept.tmp ${xp} ${yp} | sed 's/ //g'`

# Calcul des concentrations
conc_sirane=`echo  ${conc_sirane} | gawk '{print $1 }'`
conc_prox=`echo  ${conc_fond} ${conc_sirane} | gawk '{print $2 - $1 }'`
conc_cartoprox=`echo  ${conc_prevalp} ${conc_prox} | gawk '{print $2 + $1 }'`

case ${ihour} in
1)
case ${date_format} in
std)echo "`./utils/iter2date.sh ${it} ${annee}`;${conc_prevalp};${conc_sirane};${conc_fond};${conc_cartoprox}";;
no_date)echo "${conc_prevalp};${conc_sirane};${conc_fond};${conc_cartoprox}";;
esac
;;
0)
#echo "prevalp ${conc_prevalp} (1)"
#echo "fond ${conc_fond} (2)"
#echo "sirane ${conc_sirane} (3)"
#echo "prox[3-2] ${conc_prox} (4)"
#echo "cartoprox[4+1] ${conc_cartoprox}"
echo "${conc_prevalp};${conc_sirane};${conc_fond};${conc_cartoprox}"
;;
esac

it=`expr ${it} \+ 1`

done

exit 0
