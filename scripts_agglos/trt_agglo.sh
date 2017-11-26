#!/bin/ksh
#
# Traitement des agglos dans CARTOPROX 
# Atmo Rhone-Alpes 2010
#############################################

# Usage: fournir le fichier sirane_agglo.txt X_UTM, Y_UTM, NO2_total 
#                           fond_agglo.txt 

localdir=`pwd`

deb_h=00
fin_h=00

case $# in
2)
ville=$1
var=$2
deb_j=20090101
fin_j=20100101
;;
4)
ville=$1
var=$2
deb_j=$3
fin_j=$4
;;
6)
ville=$1
var=$2
deb_j=$3
deb_h=$4
fin_j=$5
fin_h=$6
;;
*)
echo "Syntaxe $0 ville var deb_j (deb_h) fin_j (fin_h)"
exit
;;
esac

# Switch de calcul
do_info=1
do_prox=0
do_netcdf=1
do_fillcdf=1
do_cartoprox=1
do_conversion=1
do_cdf=1

#DEBUG
#do_info=0
#do_prox=0
#do_netcdf=0
#do_fillcdf=0
#do_cartoprox=0
#do_conversion=0
#do_cdf=1

# declaration de l'année de calcul
if [ "${annee}" == "" ] ; then
annee=`date -d ${deb_j} +%Y`
echo "annee="${annee}
fi

## INCLUDE ###########################################
source ${cartoprox}/cartoprox.inc ${annee}

data_agglos=/mnt/mod4/data/CARTOPROX/inputs/AGGLOS
fic_fond_sirane=${data_agglos}/data_src/fond_agglo.txt


#mode=run # prep/run/plot
echo "ville="${ville}

case ${var} in
no2_*)
var1=NO2
;;
o3_*|*dep_200*)
var1=NO2
;;
no_*)
var1=NO
;;
pm10_*|pm25_*|*dep_50*)
var1=PM10
;;
esac
#unites dans le NetCDF
var_units="microg/m3"
case ${var} in
*dep*)
var_units="jours"
;;
esac

# Fichier avec les concentration SIRANE (separateur ;, espace ou TAB) ENTETE 1 ligne
fic_conc_sirane=${data_agglos}/data_src/sirane_${ville}_${var1}_${var}.txt

# utilisation du fichier PREVALP de statistiques
var_cdf=${var}

conc_fond_ugm3=`gawk '( $1 == '${annee}' && $2 == "'${ville}'" && $3 == "'${var}'" ) {print $4 }' ${fic_fond_sirane}`

echo "var1=${var1}"
echo "var=${var}"
echo "conc_fond=${conc_fond_ugm3} ${var_units}"

#Supprime la liste des mailles
rm -rf  liste_maille_prevalp_${ville}

##############################################################################################################################
#ETAPE 1.1 : determination des dimensions et des mailles PREVALP 
#Parcours le fichier SIRANE pour determiner les xmin, xmax, ymin, ymax de PREVALP
#arrondi au kilometre
##############################################################################################################################
if [ ! -f liste_maille_prevalp_${ville} ] ; then
do_info=1
fi
if [ ${do_info} -eq 1 ] ; then
echo "Calcul les infos de ${fic_conc_sirane}"
xmin=`gawk -F '( |;)' 'BEGIN { min =  1.E12 }; ( FNR>1 && $1 < min && $1 != "") {min = $1}; END { print int(min/1000  )*1000 }' ${fic_conc_sirane}`
ymin=`gawk -F '( |;)' 'BEGIN { min =  1.E12 }; ( FNR>1 && $2 < min && $2 != "") {min = $2}; END { print int(min/1000  )*1000 }' ${fic_conc_sirane}`
xmax=`gawk -F '( |;)' 'BEGIN { max = -1.E12 }; ( FNR>1 && $1 > max && $1 != "") {max = $1}; END { print int(max/1000+1)*1000 }' ${fic_conc_sirane}`
ymax=`gawk -F '( |;)' 'BEGIN { max = -1.E12 }; ( FNR>1 && $2 > max && $2 != "") {max = $2}; END { print int(max/1000+1)*1000 }' ${fic_conc_sirane}`
gawk -F '( |;)' '( FNR>1 ) { print int($1/1000.)*1000.+500." "int( $2/1000.)*1000.+500. }' ${fic_conc_sirane} \
| gawk '!($1_$2 in a) {a[$1_$2];print $1"_"$2 }' > liste_maille_prevalp_${ville}
echo "xmin=${xmin}"
echo "ymin=${ymin}"
echo "xmax=${xmax}"
echo "ymax=${ymax}"
nmailles_prevalp=`wc -l liste_maille_prevalp_${ville} | gawk '{print $1}`
echo "nb mailles PREVALP="${nmailles_prevalp}
fi

##############################################################################################################################
#ETAPE 1.2 : generation d'un fichier PREVALP en moyenne annuelle
##############################################################################################################################
#fic_h_prevalp_nc=${fond_fic}
fic_prevalp_nc=${data_agglos}/temp/prevalp_${var}.nc
fic_prox_prevalp_nc=${data_agglos}/temp/${ville}/prevalp_prox_${ville}_${var}.nc

mkdir -p ${data_agglos}/temp/${ville}

#if [ -f ${fic_moy_prevalp_nc} ] ; then
#do_netcdf=0
#fi
#if [ ${do_netcdf} -eq 1 ] ; then
#if [ ! -f ${fic_h_prevalp_nc} ] ; then
#  echo "***erreur: ${fic_h_prevalp_nc}"
#  exit
#fi
#echo "Genere NetCDF ${fic_moy_prevalp_nc} a partir de ${fic_h_prevalp_nc}..."
#if [ "${typ_op}" == "" ]  ; then
#  typ_op="avg"  
#fi
#if [ "${it1}" != "" ] && [ "${it2}" != "" ]  ; then
#  it1=`expr ${it1} \- 1`
#  it2=`expr ${it2} \- 1`  
#  interval="-d Time,${it1},${it2}"
#else
#  interval=""
#fi
#ncra -y ${typ_op} ${interval} -O -v lon,lat,Time,${var_cdf} ${fic_h_prevalp_nc} ${fic_moy_prevalp_nc} || \
#   { echo "export_cdf_ascii.sh: ERREUR dans la lecture de ${fichier_netcdf}" ; exit 1 ; }
#fi
##############################################################################################################################
#ETAPE 1.3 : utilisation d'un fichier PREVALP de statistiques
##############################################################################################################################
if [ ! -f ${fic_prevalp_nc} ] ; then
echo "Extrait ${var_cdf} de ${fond_fic_stat}"
ncks -O -v lon,lat,Time,${var_cdf} ${fond_fic_stat} ${fic_prevalp_nc} || \
   { echo "export_cdf_ascii.sh: ERREUR dans la lecture de ${fond_fic_stat}" ; exit 1 ; }
fi
##############################################################################################################################
#ETAPE 2 : calcul de la somme de la concentration de prox dans les mailles PREVALP
##############################################################################################################################

fic_prox_prevalp=${data_agglos}/temp/${ville}/prevalp_prox_${ville}_${var}.txt

if [ ! -f ${fic_prox_prevalp} ] ; then
do_prox=1
fi
if [ ${do_prox} -eq 1 ] ; then
echo "Calcul la part de proximité ${fic_prox_prevalp}"
rm -rf ${fic_prox_prevalp}
touch ${fic_prox_prevalp}
imaille=0
cat liste_maille_prevalp_${ville} | while read maille_prevalp ; do
imaille=`expr ${imaille} \+ 1`
x_utm=`echo ${maille_prevalp} | gawk -F "_" '{print $1}'`
y_utm=`echo ${maille_prevalp} | gawk -F "_" '{print $2}'`
ix=`echo ${fond_xmin} ${fond_dx} ${x_utm} | gawk '{print int(int(($3-$1)/$2) + 1 ) }'`
iy=`echo ${fond_ymin} ${fond_dx} ${y_utm} | gawk '{print int(int(($3-$1)/$2) + 1 ) }'`
#Recupere la valeur PREVALP
syntaxe="${extract_val_grille_exe} \
    -i ${fic_prevalp_nc} -xmin ${fond_xmin} -ymin ${fond_ymin} -dx ${fond_dx} -x ${x_utm} -y ${y_utm} -var ${var_cdf}"    
#echo $syntaxe

#Fait la somme des mailles SIRANE 10 m dans les mailles PREVALP 1 km et divise par le nombre theorique de maille SIRANE dans une maille PREVALP
dx_prevalp=1000
dx_sirane=10
ratio=`expr ${dx_prevalp} \/ ${dx_sirane}`
prox_avg=`gawk -F '( |;)'  'BEGIN {sum=0.};(FNR>1 && $1>'${x_utm}'-500. && $1<='${x_utm}'+500. && $2>'${y_utm}'-500. && $2<='${y_utm}'+500.) \
{sum=sum+$3-('${conc_fond_ugm3}')};\
END{print sum/('${ratio}'*'${ratio}')}' ${fic_conc_sirane}`

##############################################################################################################################
#ETAPE 3 : Ecrit un ${fic_prox_prevalp} avec la valeur de PREVALP, la prox de SIRANE et la difference PREVALP-prox SIRANE
##############################################################################################################################
case ${var} in
*_moy_an)prevalp=`${syntaxe} | gawk '{ print $4 }'`;;
nb_dep_*)prevalp=${prox_avg};;
*)echo "Variable inconnue ${var}"
exit 1
;;
esac
echo ${x_utm} ${y_utm} ${ix} ${iy} ${prevalp} ${prox_avg} | gawk '{print $1" "$2" "$3" "$4" "$5" "$6" "($5-$6) }' >> ${fic_prox_prevalp}

echo "${imaille}/${nmailles_prevalp} ${x_utm} ${y_utm} ${ix} ${iy} prevalp=${prevalp} prox=${prox_avg} ${var_units}/(10*10m2)"

done
mv ${fic_prox_prevalp} ${fic_prox_prevalp}.tmp 
sort -n ${fic_prox_prevalp}.tmp  > ${fic_prox_prevalp}
rm ${fic_prox_prevalp}.tmp 
fi

echo "Selection les mailles PREVALP > fond SIRANE"
gawk '( $7 > '${conc_fond_ugm3}' ) {print}' ${fic_prox_prevalp} 

##############################################################################################################################
#ETAPE 4 : rempli le NetCDF ${fic_moy_prevalp_nc}
# Soustraie en microg/m3 la proximité SIRANE a la concentration moyenne PREVALP dans une maille
# Si cette concentration est inferieure a la min_conc, fixe la concentration min_conc
##############################################################################################################################
if [ ${do_fillcdf} -eq 1 ] ; then
#cp ${fic_moy_prevalp_nc} ${fic_moy_prevalp_nc}.1
cp ${fic_prevalp_nc} ${fic_prox_prevalp_nc}
gawk '{ print $3" "$4" "$6 }' ${fic_prox_prevalp} > ${data_agglos}/temp/${ville}/val_prox_prevalp
min_conc=${conc_fond_ugm3}
${localdir}/src/fill_cdf.e ${fic_prox_prevalp_nc} ${data_agglos}/temp/${ville}/val_prox_prevalp ${var_cdf} ${min_conc}
rm ${data_agglos}/temp/${ville}/val_prox_prevalp
fi

fic_cartoprox_utm=${data_agglos}/resultats/${ville}/cartoprox_${var}_${annee}_${ville}_utm31.txt
fic_cartoprox_latlong=${data_agglos}/resultats/${ville}/cartoprox_${var}_${annee}_${ville}_latlong.txt
fic_cartoprox_lambert=${data_agglos}/resultats/${ville}/cartoprox_${var}_${annee}_${ville}_lambert.txt

mkdir -p ${data_agglos}/resultats/${ville}

##############################################################################################################################
#ETAPE 5 : Calcul la valeur CARTROPROX ${fic_prox_prevalp_nc} + sirane_prox
##############################################################################################################################
if [ ${do_cartoprox} -eq 1 ] ; then

# 2 traitements : *
# - en moyenne an, on utilise la moyenne de PREVALP + proximité SIRANE
# - en nombre de dépassements, on utilise la valeur de SIRANE brute 

case ${var} in
*_moy_an)
# export de PREVALP aux recepteurs SIRANE
gawk -F '( |;)'  '( FNR>1 ){ print $1" "$2" "$3}' ${fic_conc_sirane} > ${data_agglos}/temp/${ville}/tmp_xyz # UTM31
echo "-> Extrait PREVALP a `wc -l ${data_agglos}/temp/${ville}/tmp_xyz | gawk '{print $1}'` points de ${fic_prox_prevalp_nc}"
syntaxe="${extract_val_grille_exe} \
    -i ${fic_prox_prevalp_nc} -xmin ${fond_xmin} -ymin ${fond_ymin} -dx ${fond_dx} -s ${data_agglos}/temp/${ville}/tmp_xyz -var ${var_cdf}"    
#secho $syntaxe
${syntaxe} | gawk '{print $1" "$2" "$3-('${conc_fond_ugm3}')+$4 }' > ${fic_cartoprox_utm}
echo "-> ${fic_cartoprox_utm} OK"
rm ${data_agglos}/temp/${ville}/tmp_xyz
;;
nb_dep_*)
gawk -F '( |;)'  '( FNR>1 ){ print $1" "$2" "$3}' ${fic_conc_sirane} > ${fic_cartoprox_utm} # UTM31
;;
esac

rm ${fic_prox_prevalp_nc}

fi

##############################################################################################################################
#ETAPE 6 : conversion geographique utm31 -> lat/long + lambert 2
##############################################################################################################################
if [ ${do_conversion} -eq 1 ] ; then
# convertit les coordonnées en lat/long
gawk '{print $1" "$2}' ${fic_cartoprox_utm} > ${data_agglos}/temp/${ville}/tmp.xy_utm  
gawk '{print	  $3}' ${fic_cartoprox_utm} > ${data_agglos}/temp/${ville}/tmp.val
# UTM 31 -> lat/long
syntaxe="${conversion_exe} -utm 31 -geoid WGS84 -geo -geoid WGS84 -i ${data_agglos}/temp/${ville}/tmp.xy_utm -o ${data_agglos}/temp/${ville}/tmp.latlong"  
${syntaxe}
# UTM 31 -> lambert
syntaxe="${conversion_exe} -utm 31 -geoid WGS84 -l2 -geoid NTF -i ${data_agglos}/temp/${ville}/tmp.xy_utm -o ${data_agglos}/temp/${ville}/tmp.lambert"  
${syntaxe}  
paste ${data_agglos}/temp/${ville}/tmp.latlong ${data_agglos}/temp/${ville}/tmp.val | gawk '{print $1" "$2" "$3 }' > ${fic_cartoprox_latlong}  
echo "-> ${fic_cartoprox_latlong} OK"
paste ${data_agglos}/temp/${ville}/tmp.lambert ${data_agglos}/temp/${ville}/tmp.val | gawk '{print $1" "$2" "$3 }' > ${fic_cartoprox_lambert}  
echo "-> ${fic_cartoprox_lambert} OK"  
rm ${data_agglos}/temp/${ville}/tmp.xy_utm  ${data_agglos}/temp/${ville}/tmp.val 
rm ${data_agglos}/temp/${ville}/tmp.latlong ${data_agglos}/temp/${ville}/tmp.lambert
fi

##############################################################################################################################
#ETAPE 7 : conversion X,Y,Z ASCII -> 1 NetCDF / mini-maille
##############################################################################################################################
if [ ${do_cdf} -eq 1 ] ; then

# Determine qu elle maille de CARTOPROX sont en jeu...
gawk '( $1 == "'${code_maille}'" && \
 ($7+ $9/2 >= '${xmin}')  && \
 ($7- $9/2 <= '${xmax}')  &&\
 ($8+$10/2 >= '${ymin}')  &&\
 ($8-$10/2 <= '${ymax}') ) \
 { print $2 }' ${params_mailles} > ${localdir}/minidomaines.txt

echo "domaines CARTOPROX dans  ${agglo} :"
cat ${localdir}/minidomaines.txt




cat ${localdir}/minidomaines.txt | while read code_domaine ; do

xmin_dom=`gawk '($1=="'${code_maille}'" && $2=="'${code_domaine}'"){print $7 - $9/2 }' ${params_mailles}`
xmax_dom=`gawk '($1=="'${code_maille}'" && $2=="'${code_domaine}'"){print $7 + $9/2 }' ${params_mailles}`
ymin_dom=`gawk '($1=="'${code_maille}'" && $2=="'${code_domaine}'"){print $8 - $10/2}' ${params_mailles}`
ymax_dom=`gawk '($1=="'${code_maille}'" && $2=="'${code_domaine}'"){print $8 + $10/2}' ${params_mailles}`

echo ${xmin_dom} ${xmax_dom} ${ymin_dom} ${ymax_dom}

fic_cartoprox_dom_utm=${data_agglos}/temp/tmp.utm31.${code_domaine}
fic_cartoprox_dom_nc=${data_agglos}/resultats/domaines/${code_domaine}_cartoprox_${var}_${annee}.nc
mkdir -p ${data_agglos}/resultats/domaines

gawk '( ( $1 >= '${xmin_dom}' )&&\
        ( $1 <  '${xmax_dom}' )&&\
        ( $2 >= '${ymin_dom}' )&&\
        ( $2 <  '${ymax_dom}' )  \
      )' ${fic_cartoprox_utm} > ${fic_cartoprox_dom_utm}
	 
nrecept=`wc -l  ${fic_cartoprox_dom_utm} | gawk '{print $1}'`
echo  "${fic_cartoprox_dom_utm} -> ${nrecept} points"

if [ ${nrecept} -gt 0 ] ; then
${localdir}/sirane2netcdf.sh ${annee}0101 ${fic_cartoprox_dom_utm} ${var} utm31 ${data_agglos}/temp/${ville}/tmp.nc > sirane2netcdf.log 2>&1
ncks -O -v x_pts,y_pts,easting_pts,northing_pts,Times,${var} ${data_agglos}/temp/${ville}/tmp.nc ${fic_cartoprox_dom_nc}
rm ${data_agglos}/temp/${ville}/tmp.nc
ncatted -a units,${var},m,c,"${var_units}" ${fic_cartoprox_dom_nc}
echo "-> ${fic_cartoprox_dom_nc}"
fi

rm ${fic_cartoprox_dom_utm}

done

#fic_cartoprox_nc=${data_agglos}/resultats/${ville}/cartoprox_${var}_${annee}_${ville}.nc
#${localdir}/sirane2netcdf.sh ${annee}0101 ${fic_cartoprox_lambert} ${var} lamb2 ${data_agglos}/temp/${ville}/tmp.nc
#ncks -O -v x_pts,y_pts,easting_pts,northing_pts,Times,${var} ${data_agglos}/temp/${ville}/tmp.nc ${fic_cartoprox_nc}
#rm ${data_agglos}/temp/${ville}/tmp.nc
#ncatted -a units,${var},m,c,"${var_units}" ${fic_cartoprox_nc}
#echo "-> Fichier de sortie NetCDF CARTOPROX: ${fic_cartoprox_nc}"

fi

echo "Fin du script $0"

rm -rf  liste_maille_prevalp_${ville}
