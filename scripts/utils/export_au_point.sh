#!/bin/bash
#
# CARTOPROX
# export des données en un point
#
## INCLUDE ###########################################
localdir=`pwd`

# Recupere les infos sur la grille CARTOPROX
xmin=${cartoprox_xmin}
ymin=${cartoprox_xmax}
nx=${cartoprox_nx}
ny=${cartoprox_ny}
dx=${cartoprox_dx}
dy=${dx}

deb_h=00
fin_h=00

arrondi=1000.

case $# in
6)
xp=$1
yp=$2
variable=$3
deb_j=$4
fin_j=$5
typ_op=$6
date_format=std
;;
8)
xp=$1
yp=$2
variable=$3
deb_j=$4
fin_j=$5
typ_op=$6
nrs=$7
nre=$8
date_format=std
;;
10)
xp=$1
yp=$2
variable=$3
deb_j=$4
deb_h=$5
fin_j=$6
fin_h=$7
typ_op=$8
nrs=$9
nre=${10}
date_format=std
;;
11)
xp=$1
yp=$2
variable=$3
deb_j=$4
deb_h=$5
fin_j=$6
fin_h=$7
typ_op=$8
nrs=$9
nre=${10}
date_format=${11}
;;
*)
echo "Syntaxe $0 x_utm y_utm variable annee typ_op it_start it_stop"
exit 1
esac

#Defaut
if [ "${nrs}" == "" ] ; then
nrs=1
fi
if [ "${nre}" == "" ] ; then
nre=1
fi

#Polluant
case ${variable} in
NO2|NO|O3)export polluant=1;;
PM10)export polluant=2;;
PM25)export polluant=3;;
esac

############################################################################################
# INCLUDE
############################################################################################
# declaration de la periode de calcul 
if [ "${periode}" == "" ] ; then
  periode=${deb_j}_${fin_j}
fi

deb_j=`echo ${periode} | gawk -F "_" '{print $1}'`
fin_j=`echo ${periode} | gawk -F "_" '{print $2}'`
annee=`date -d ${deb_j} +%Y`

#Include
source ${cartoprox}/cartoprox.inc ${periode} 1> /dev/null || \
  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; }
############################################################################################

# calcul du centre de la maille PREVALP
# xc = xmin + int((xp-xmin)/dx+1 - 1) * dx + dx/2

#trouve le code domaine de CARTOPROX
if [ "${code_domaine}" == "" ] ; then
  code_domaine=`${cartoprox}/scripts/utils/quel_minidomaine.sh ${xp} ${yp}`
fi

#echo "code_domaine=${code_domaine}"

case ${polluant} in
1)prefix_nc="_nox";;
2|3)prefix_nc="_${var_pm}";;
esac

run=${deb_j}${deb_h}_${fin_j}${fin_h}
#nrs=`./utils/date2iter.sh  ${deb_j} ${deb_h}`
#nre=`./utils/date2iter.sh  ${fin_j} ${fin_h}`

# extrait le niveau de fond
source ./utils/infos_domaine.sh ${cartoprox_domaine} ${code_domaine} ${periode} 1> /dev/null || \
  { echo "Erreur dans $0: ./utils/infos_domaine.sh" ; exit 1 ; }

#typ_op="avg"

case ${typ_op} in
avg|max)
it=${nre}
timestep=1 #h
;;
h)
it=${nrs}
timestep=1 #h
;;
j)
it=${nrs}
timestep=24 #h
;;
esac

#echo "it=${it}"
#echo "nre=${nre}"

while [ ${it} -le ${nre} ] ; do

case ${typ_op} in
avg|max)
it1=${nrs}
it2=${nre}
typ_op_ncks=${typ_op}
;;
h|j)
it1=${it}
it2=`expr ${it} \+ ${timestep} \- 1`
typ_op_ncks=avg #moyenne horaire/journaliere
;;
esac

# condition pour que les valeurs existent
if [ ${it2} -le ${nre} ] ; then

# Valeur du calcul SIRANE
fic_sirane_recept=${ChemRes}/sirane${prefix_nc}.${run}.nc
fic_cartoprox_recept=${ChemRes}/cartoprox${prefix_nc}.${run}.nc
fic_reg_recept=${ChemRes}/reg${prefix_nc}.${run}.recepts.nc
fic_suremis_recept=${ChemRes}/suremis${prefix_nc}.${run}.recepts.nc

if [ ! -f ${fic_netcdf} ] ; then
echo "Erreur: export echoue - fichier introuvable: ${fic_netcdf}"
exit 1
fi

rm -f ./sirane.recept ./cartoprox.recept ./reg.recept ./suremis.recept

#Export des concentration CARTOPROX au point
if [ -f ${fic_sirane_recept} ] ; then
  syntaxe="${cartoprox}/utils/carto_gmt/export_cdf_ascii.sh ${fic_sirane_recept} ${variable} ${typ_op_ncks} ${it1} ${it2}"
#  echo ${syntaxe}
  ${syntaxe} | awk '{print $1";"$2";"$3}' > ./sirane.recept
#else
#  echo "Inexistant: ${fic_sirane_recept}"
fi
if [ -f ${fic_cartoprox_recept} ] ; then
  syntaxe="${cartoprox}/utils/carto_gmt/export_cdf_ascii.sh ${fic_cartoprox_recept} ${variable} ${typ_op_ncks} ${it1} ${it2}"
#  echo ${syntaxe}
  ${syntaxe} | awk '{print $1";"$2";"$3}' > ./cartoprox.recept
#else
#  echo "Inexistant: ${fic_cartoprox_recept}"
fi
if [ -f ${fic_reg_recept} ] ; then
  syntaxe="${cartoprox}/utils/carto_gmt/export_cdf_ascii.sh ${fic_reg_recept} ${variable} ${typ_op_ncks} ${it1} ${it2}"
#  echo ${syntaxe}
  ${syntaxe} | awk '{print $1";"$2";"$3}' > ./reg.recept
#else
#  echo "Inexistant: ${fic_reg_recept}"
fi
if [ -f ${fic_suremis_recept} ] ; then  
  syntaxe="${cartoprox}/utils/carto_gmt/export_cdf_ascii.sh ${fic_suremis_recept}   ${variable} ${typ_op_ncks} ${it1} ${it2}"
#  echo ${syntaxe}
  ${syntaxe} | awk '{print $1";"$2";"$3}' > ./suremis.recept
#else
#  echo "Inexistant: ${fic_suremis_recept}"
fi

#exit
#for fic in cartoprox reg suremis ; do
#  echo "------------------------${fic}.recept------------------------------"
#  head ${fic}.recept
#done
# Calcul des concentrations
option_interpolation=1

case ${option_interpolation} in
1)
#Avec R
for fic in ./sirane.recept ./cartoprox.recept ./reg.recept ./suremis.recept ; do

#Interpolation R
rm -f ./grille.xyz ./points.xyz
if [ -f ${fic} ] ; then
#echo ${fic}
#head -n 5 ${fic}
mv ${fic} points.xyz
echo "${xp};${yp}" > ./grille.xy
#echo "Interpolation R en cours ${xp} ${yp}"
R CMD BATCH ./utils/interp2d.R
fi
 
#Interpolation OK ?
if [ -f ./grille.xyz ] ; then
  conc_tmp=`gawk -F ";" '{print $3}' grille.xyz`
else
#  echo "${fic}: interpolation echoue !"
  #tail -n 10 ./utils/interp2d.Rout
  conc_tmp=-999.
fi
#echo $conc_tmp
rm -f ./grille.xyz ./grille.xy ./points.xyz
case ${fic} in
*sirane*)conc_sirane=${conc_tmp};;
*cartoprox*)conc_cartoprox=${conc_tmp};;
*reg*)conc_prevalp=${conc_tmp};;
*suremis*)conc_prox=${conc_tmp};;
esac
done
;;
2)
#Avec interp_bilin.exe
#echo "Interpolation inverse des distances en cours ${xp} ${yp}"
if [ ! -f ./utils/interp_bilin.exe ] ; then
${F90} -o ./utils/interp_bilin.exe ./utils/interp_bilin.f90
fi
nrecepts=`cat recept.tmp | wc -l`
echo ./utils/interp_bilin.exe ./recept.tmp ${xp} ${yp} ${nrecepts}
conc_cartoprox=`./utils/interp_bilin.exe ./recept.tmp ${xp} ${yp} | sed 's/ //g'`
;;
esac
#echo "conc_cartoprox=${conc_cartoprox}"
#rm recept.tmp grille.xyz grille.xy points.xyz

# Valeur du fond du calcul SIRANE
##if [ -f ${fic_sirane_recept} ] ; then
##  syntaxe="${cartoprox}/utils/carto_gmt/export_cdf_ascii.sh ${fic_sirane_recept} ${variable}_fond ${typ_op_ncks} ${it1} ${it2}"
##  conc_fond=`${syntaxe}`
##else
##  conc_fond=-999.
##fi

# Valeur du prox moy du calcul SIRANE (version 3)
#syntaxe="${cartoprox}/utils/carto_gmt/export_cdf_ascii.sh ${fic_netcdf} ${variable}_prox ${typ_op_ncks} ${it1} ${it2}"
#conc_prox=`${syntaxe}`

# Valeur de PREVALP interpolée
##if [ "${conc_prevalp}" == "-999." ] ; then
##case ${variable} in
##PM*)#jour
##it1_reg=`echo ${it1} | awk '{print int($1/24.+1)}'`
##it2_reg=`echo ${it2} | awk '{print int($1/24.+1)}'`
##;;
##*)#horaire
##it1_reg=${it1}
##it2_reg=${it2}
##;;
##esac
##syntaxe="${extract_val_grille_exe} \
##    -i ${fond_fic} -xmin ${fond_xmin} -ymin ${fond_ymin} -dx ${fond_dx} -x ${xp} -y ${yp} -it1 ${it1_reg} -it2 ${it2_reg} -var ${variable}"
##conc_prevalp=`${syntaxe} | gawk '{print $4}'` #arrondi 2 chiffres
##fi

#Conversion PREVALP
case ${variable} in
NO)pol_fac=1.25;;
NO2)pol_fac=1.91;;
O3)pol_fac=1.99;;
*)pol_fac=1.0;;
esac
##if [ "${conc_prevalp}" != "-999." ] ; then
##  conc_prevalp=`echo ${conc_prevalp} | gawk '{print int( ($1*'${pol_fac}')*100.) / 100. }'` #arrondi 2 chiffres
##fi

#Arrondis
conc_cartoprox=`echo ${conc_cartoprox} | gawk '{ print int($1 * '${arrondi}') / '${arrondi}' }' `
conc_prox=`echo ${conc_prox} | gawk '{ print int($1 * '${arrondi}') / '${arrondi}' }' `
conc_fond=`echo ${conc_fond} | gawk '{ print int($1 * '${arrondi}') / '${arrondi}' }' `
conc_prevalp=`echo ${conc_prevalp} | gawk '{ print int($1 * '${arrondi}') / '${arrondi}' }' `

##if [ "${conc_sirane}" == "-999." ] ; then
##  if [ "${conc_cartoprox}" != "-999." ] && [ "${conc_prox}" != "-999." ] && [ "${conc_fond}" != "-999." ] && [ "${conc_prevalp}" != "-999." ] ; then
    #conc SIRANE (sortie de MODELE) 
##    conc_sirane=`echo ${conc_cartoprox} ${conc_prevalp} ${conc_prox}  ${conc_fond} | gawk '{ print int(($1 - $2 + $3 + $4 ) * '${arrondi}') / '${arrondi}' }'` #arrondi 2 chiffres
##  fi
##fi

# Affichage
case ${typ_op} in
avg)
echo "${conc_prevalp};${conc_sirane};${conc_fond};${conc_prox};${conc_cartoprox}"
;;
*)
case ${date_format} in
std)echo "`./utils/iter2date.sh ${it} ${annee}`;${conc_prevalp};${conc_sirane};${conc_fond};${conc_prox};${conc_cartoprox}";;
no_date)echo "${conc_prevalp};${conc_sirane};${conc_fond};${conc_prox};${conc_cartoprox}";;
esac
;;

esac

fi # it2 <= nre

it=`expr ${it} \+ ${timestep}`

done

exit 0
