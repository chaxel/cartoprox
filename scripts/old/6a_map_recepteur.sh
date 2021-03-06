#!/bin/bash
#
# Cartographie de CARTOPROX sur une maille (exemple: bourgoin)
# 
# usage : plot_CARTOPROX.sh + le mois voulu
#############################################

localdir=`pwd`
script=6a_plot_CARTOPROX.sh
export cartoprox=`pwd | gawk -F '/scripts' '{print $1}'`

#switch
do_export=1 #export les valeurs recepteurs CDF-->ASCII
do_grd=1    #genere un grd et netCDF
do_gif=1    #genere un fichier GIF
do_gmt=0    #map GMT
do_kmz=1    #genere un PNG et un KMZ

annee_defaut=2010 # dans cartoprox.inc
deb_j=${annee_defaut}0101
fin_j=${annee_defaut}1231
deb_h=00
fin_h=23

############################################################################################
# domaine de cartographie
############################################################################################
# automatique : avec tout domaine dans cartoprox.inc
xc_p="x"
yc_p="x"
xmin_p=-9999999
ymin_p=-9999999
xmax_p=9999999
ymax_p=9999999
code_domaine_loc=""
resolution=auto
#resolution=5 #Decommenter pour forcer la resolution
############################################################################################
# Fond utilis� pour la cartographie
############################################################################################
ifond=2 # 2 = recepteur + fond interpole CHIMERE (fournir le fichier)
############################################################################################

case $# in
1)
var=$1
#resolution=10 #metres
;;
2)
var=$1
code_domaine_loc=$2
;;
3)
var=$1
deb_j=$2
fin_j=$3
;;
4)
var=$1
code_domaine_loc=$2
deb_j=$3
fin_j=$4
;;
5)
var=$1
xc_p=$2
yc_p=$3
dx_p=$4
dy_p=$5
;;
6|7)
var=$1
xc_p=$2
yc_p=$3
dx_p=$4
dy_p=$5
resolution=$6
opt=$7
;;
*)
echo "Syntaxe $0  variable [(code_domaine deb_j fin_j) (Xc Yc dx dy)]" # (Xmin Ymin Xmax Ymax)" #(debut[YYYYMMJJ (HH)] fin[YYYYMMJJ (HH)])"
exit
;;
esac

if [ "${opt}" == "cdf" ]; then
  do_gif=0
  do_grd=1
  do_gmt=0
  do_kmz=0
fi

############################################################################################
# Choix du mode de CARTOPROX
############################################################################################
case ${var} in
no2_moy_an|nb_dep_200_jour|NO|NO2|O3)
export polluant=1
seuil_var=-999
;;
pm10_moy_an|nb_dep_50_jour|PM10)
export polluant=2
seuil_var=-999
;;
pm25_moy_an|PM25)
export polluant=3
seuil_var=-999
;;
*)
echo "Variable inconnue ${var}"
exit 1
;;
esac

# Seuil de concentration (pour REPORTING EXPLOITATION)
if [ ${seuil_var} -ge 0 ] ; then
seuil_str=_seuil${seuil_var} #"seuil${seuil_var}"
else
seuil_str=""
fi

############################################################################################
# INCLUDE
############################################################################################
# declaration de la periode de calcul 
if [ "${periode}" == "" ] ; then
  periode=${deb_j}_${fin_j}
  echo "periode=${periode} (DEFAUT)"
  sleep 2s
fi
deb_j=`echo ${periode} | gawk -F "_" '{print $1}'`
fin_j=`echo ${periode} | gawk -F "_" '{print $2}'`
annee=`date -d ${deb_j} +%Y`

#Include
source ${cartoprox}/cartoprox.inc ${periode} || \
  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; }
  
export periode # pour recept2grid
############################################################################################

## SI MINI-DOMAINE #################################################
if [ "${code_domaine_loc}" != "" ] ; then
  #definition domaine
  xc_p=`gawk '( $1 =="'${cartoprox_domaine}'" && $2 =="'${code_domaine_loc}'" ) { print $7}' ${params_mailles}`
  yc_p=`gawk '( $1 =="'${cartoprox_domaine}'" && $2 =="'${code_domaine_loc}'" ) { print $8}' ${params_mailles}`
  dx_p=`gawk '( $1 =="'${cartoprox_domaine}'" && $2 =="'${code_domaine_loc}'" ) { print $9}' ${params_mailles}`
  dy_p=${dx_p}  
  #Sorties GRD/netCDF/GIF
  selection_user=${selection}
  RepCarto=${ChemRes1}/cartes${selection_user}  
else
  selection_user="_user_$USER" #domaine defini par utilisateur  
  #Sorties GRD/netCDF/GIF
  RepCarto=${SCRATCH}/cartoprox${selection_user}    
fi
mkdir -p ${RepCarto}

## SI REGION #######################################################
if [ "${xc_p}" != "x" ] && [ "${yc_p}" != "x" ]  ; then
# defini le domaine de PLOT
xmin_plot=`echo ${xc_p} ${dx_p} | gawk '{print int($1 - $2/2) }'`
xmax_plot=`echo ${xc_p} ${dx_p} | gawk '{print int($1 + $2/2) }'`
ymin_plot=`echo ${yc_p} ${dy_p} | gawk '{print int($1 - $2/2) }'`
ymax_plot=`echo ${yc_p} ${dy_p} | gawk '{print int($1 + $2/2) }'`

else

xmin_plot=${maille_xmin}
xmax_plot=${maille_xmax}
ymin_plot=${maille_ymin}
ymax_plot=${maille_ymax}

fi

# defini le domaine d'extraction
cadre_interp=1000 #metres
xmin_p=`echo ${xmin_plot} ${cadre_interp} | gawk '{print int($1 - $2) }'`
xmax_p=`echo ${xmax_plot} ${cadre_interp} | gawk '{print int($1 + $2) }'`
ymin_p=`echo ${ymin_plot} ${cadre_interp} | gawk '{print int($1 - $2) }'`
ymax_p=`echo ${ymax_plot} ${cadre_interp} | gawk '{print int($1 + $2) }'`

# Seuil de concentration ##########################################
seuil_var=-999

# Variable
case ${var} in 
*_J-1|*_Jm1|J-1_*|Jm1_*)echeance=-1;;
*_J0|J0_*)echeance=0;;
*_J1|J1_*)echeance=1;;
*_J2|J2_*)echeance=2;;
*)echeance=-9;;
esac
var=`echo ${var} | sed -e "s/_J${echeance}//" | sed -e "s/J${echeance}_//"`


###################################################################
# Fin des modifications USER
###################################################################
echo "selection_user=${selection_user}"

echo "Parametre du domaine d extraction(UTM 31) :"
echo "xmin_p=${xmin_p}"
echo "ymin_p=${ymin_p}"
echo "xmax_p=${xmax_p}"
echo "ymax_p=${ymax_p}"

echo "Parametre du domaine de PLOT (UTM 31) :"
echo "xmin_plot=${xmin_plot}"
echo "ymin_plot=${ymin_plot}"
echo "xmax_plot=${xmax_plot}"
echo "ymax_plot=${ymax_plot}"

sleep 1

# repertoire temporaire ###########################################
TMPDIR=${SCRATCH}/${script}.${xmin_plot}_${ymin_plot}_${xmax_plot}_${ymax_plot}.$USER
#while [ -d ${SCRATCH}/${script}.$RANDOM ] ; do
#TMPDIR=${SCRATCH}/${script}.$RANDOM
#done
echo "Travaille dans ${TMPDIR}"
mkdir -p ${TMPDIR}

# selectionne les minidomaines dans le domaine de PLOT#############
echo "Recherche les domaines dans ${params_mailles}"
echo "cartoprox_domaine=${cartoprox_domaine}"
if [ "${code_domaine_loc}" == "" ] ; then
gawk '( $1 =="'${cartoprox_domaine}'" && \
(($7+'${cartoprox_dx}'/2>'${xmin_p}')&&($7-'${cartoprox_dx}'/2<'${xmax_p}')&&\
 ($8+'${cartoprox_dy}'/2>'${ymin_p}')&&($8-'${cartoprox_dy}'/2<'${ymax_p}')) \
) { print $2 }' ${params_mailles} > ${TMPDIR}/minidomaines.txt
else
gawk '( $1 =="'${cartoprox_domaine}'" && $2 =="'${code_domaine_loc}'" && \
(($7+'${cartoprox_dx}'/2>'${xmin_p}')&&($7-'${cartoprox_dx}'/2<'${xmax_p}')&&\
 ($8+'${cartoprox_dy}'/2>'${ymin_p}')&&($8-'${cartoprox_dy}'/2<'${ymax_p}')) \
) { print $2 }' ${params_mailles} > ${TMPDIR}/minidomaines.txt
fi

ndomaines=`cat ${TMPDIR}/minidomaines.txt | wc -l`
echo "Liste des ${ndomaines} domaine(s) : "
cat ${TMPDIR}/minidomaines.txt #| wc -l
if [ ${ndomaines} -eq 0 ] ; then
echo "***info : aucun domaine a traiter - STOP"
rm -rf ${TMPDIR}
exit 0
fi

#echo "Plot CARTOPROX sur minidomaine ${code_domaine} dans 5 secondes..."
run=${deb_j}${deb_h}_${fin_j}${fin_h}

case ${echeance} in
-9)
nrs=`./utils/date2iter.sh  ${deb_j} ${deb_h}`
nre=`./utils/date2iter.sh  ${fin_j} ${fin_h}`
if [ ${nre} -eq 1 ] ; then
  fin_j_1=`date -d "${fin_j} 1 day ago" +%Y%m%d`
  nre=`./utils/date2iter.sh  ${fin_j_1} 23`
fi
#Retranche le premier pas de temps
nre=`expr ${nre} \- ${nrs} \+ 1`   
nrs=`expr ${nrs} \- ${nrs} \+ 1`

#Nom echeance dans fichier de fortie
echeancestr=""
;;
-1)
nrs=1
nre=24
echeance_str="_Jm1"
;;
0)
nrs=25
nre=48
echeance_str="_J0"
;;
1)
nrs=49
nre=72
echeance_str="_J1"
;;
2)
nrs=73
nre=96
echeance_str="_J2"
;;
esac #echeance

#defini si besoin les iterations
if [ "${it1}" == "" ] || [ "${it2}" == "" ]  ; then
  it1=`expr ${nrs} \- 1`
  it2=`expr ${nre} \- 1`    
fi

# log
log=${TMPDIR}/silence.log
##################################################################
#for var in ${var_list} ; do
##################################################################

if [ ${ndomaines} -eq 1 ] && [ "${code_domaine_loc}" != ""  ] ; then
fic_out_base=${RepCarto}/conc_${run}${echeance_str}_${var}${seuil_str}${selection_user}_${code_domaine_loc}
else
fic_out_base=${RepCarto}/conc_${run}${echeance_str}_${var}${seuil_str}${selection_user}_${xc_p}_${yc_p}_res${resolution}
fi

fic_xyz=${fic_out_base}_utm31.txt
fic_latlong=${fic_out_base}_latlong.txt
fic_lambert=${fic_out_base}_lambert.txt

echo ${fic_xyz}

if [ -f ${fic_xyz} ]  ; then #|| [ -f ${fic_latlong} ] || [ -f ${fic_lambert} ] ; then 
#  do_export=0
rm ${fic_xyz}
fi


if [ ${do_export} -eq 1 ] ; then

#fichier ASCII
fic_ascii=${TMPDIR}/tmp.${var}

if [ -f ${fic_ascii} ] ; then 
 rm ${fic_ascii}
fi

#Genere un fichier avec la variable concern�e
touch ${fic_ascii}

case ${var} in 
NO*|O3*)
prefix_nc="_nox"
interval="-d Time,${it1},${it2}"
typ_op="max" #
;;
${var_pm}*)
prefix_nc="_${var_pm}"
interval="-d Time,${it1},${it2}"
typ_op="avg"
;;
*)
interval=""
typ_op=""
nrs=""
nre=""
;;
esac

case ${var} in 
NO*|O3*|no2_moy_an)prefix_nc="_nox";;
PM10*|pm10_moy_an|nb_dep_50_jour)prefix_nc="_PM10";;
PM25*|pm25_moy_an)prefix_nc="_PM25";;
*)
echo "Variable inconnue: ${var}"
exit 1
;;
esac

##################################################################################

ndom=`wc -l ${TMPDIR}/minidomaines.txt | gawk '{print $1}'`

#PREVALP
#Domaine reduit (voir 5b_suremis)
fic_fond_coarse=${prevalp_raster}/${cartoprox_domaine}/out${prefix_nc}.${deb_j}${deb_h}_${fin_j}${fin_h}_${dom_chimere}${selection}.nc
fic_stat_coarse=${prevalp_raster}/${cartoprox_domaine}/stat${prefix_nc}.${deb_j}${deb_h}_${fin_j}${fin_h}_${dom_chimere}${selection}.nc
xminc=`echo ${maille_xmin} ${fond_dx} | awk '{print int($1 - $2) }'`
yminc=`echo ${maille_ymin} ${fond_dx} | awk '{print int($1 - $2) }'`
dxc=${fond_dx}

# Optimisation PREVALP

if [ "${typ_op}" != "" ] ; then
  echo "Genere ${TMPDIR}/tmp_${var}.nc a partir de ${fic_grille_nc} (${interval}) "
  ${ncra} -O -y ${typ_op} ${interval} -v lon,lat,${var} ${fic_fond_coarse} ${TMPDIR}/tmp_${var}.nc || \
      { echo "$0: ERREUR dans la lecture de ${fic_grille_nc}" ; exit 1 ; }
  fic_fond_coarse=${TMPDIR}/tmp_${var}.nc
fi
# Optimisation PREVALP

#<<<<<<<<<<<<<<<<<<< Boucle sur les mini domaines>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
idom=0
cat ${TMPDIR}/minidomaines.txt | while read code_domaine ; do
#<<<<<<<<<<<<<<<<<<< Boucle sur les mini domaines>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
idom=`expr ${idom} \+ 1`

code_domaine_loc=${code_domaine}

### dependant du domaine de calcul####################################
source ./utils/infos_mailleur.sh ${cartoprox_domaine} ${code_domaine} ${periode} #> ${log}
######################################################################

# NOMS DES FICHIERS ##################################################
#Aux recepteurs
fic_cartoprox_nc=${ChemRes}/cartoprox${prefix_nc}.${run}.nc
fic_stat_nc=${ChemRes}/stat.${run}.nc

case ${var} in 
NO*|O3*|${var_pm}*)
fic_grille_nc=${fic_fond_coarse}
fic_recept_nc=${fic_cartoprox_nc}
;;
*)
fic_grille_nc=${fic_stat_coarse}
fic_recept_nc=${fic_stat_nc}
;;
esac

fic_agglos_nc=/mnt/mod4/data/CARTOPROX/inputs/AGGLOS/resultats/domaines/${code_domaine}_cartoprox_${var}_${periode}.nc

# Maillage PREVALP de secours
if [ ! -f ${ficrecept_mailleur} ] ; then
  emis_profil=-999 # pas d'export
fi 

echo "fic_recept_nc=${fic_recept_nc}"
if [ ! -f ${fic_recept_nc} ] ; then 
  echo "N EXISTE PAS!"
fi

if [ ! -f ${fic_recept_nc} ] && [ ${emis_profil} -ne 0 ]; then
  nrecept_masque=`./utils/applique_masque_agglo.sh  ${cartoprox_domaine} ${code_domaine} | wc -l`  
  echo "***info: tous les recepteurs sont dans le MASQUE -> emis_profil=-999"    
  emis_profil=-999  
fi

case ${emis_profil} in 
### dependant du domaine de calcul####################################
-999)# emis_profil=-999 PAS DE RECEPTEUR VALIDES (masque) -> aucun export � ce point
######################################################################
echo "***info: AUCUN POINT VALIDE dans le masque -> SKIP"
;;

### dependant du domaine de calcul####################################
0)# emis_profil=0 Export de PREVALP � ce point
######################################################################
valeur_prevalp=0.
case ${var} in
NO2)var_fac=1.81;;
O3)var_fac=1.99;;
*)var_fac=1.;;
esac
var_cdf=${var}
#on suppose que tous les points PREVALP sont hors masque... 
awk '{ print $4" "$5" '${valeur_prevalp}'" }' ${ficrecept_mailleur} >  ${TMPDIR}/prevalp.tmp
awk '{ print $4" "$5" '${valeur_prevalp}'" }' ${ficbrin_mailleur}   >> ${TMPDIR}/prevalp.tmp
# Lance le programme de calcul des valeurs aux recepteurs depuis la grille reguliere UTM 31 de PREVALP sur le fichier conc.tmp2 (X_utm Y_utm conc_ugm3)
npts_prevalp=`cat ${TMPDIR}/prevalp.tmp | wc -l`
echo "[${idom}/${ndom}] -> Extrait PREVALP var ${var} a ${npts_prevalp} points de ${code_domaine}"
#cat ${TMPDIR}/prevalp.tmp #DEBUG
syntaxe="${extract_val_grille_exe} \
    -i ${fic_grille_nc} -xmin ${xminc} -ymin ${yminc} -dx ${dxc} -s ${TMPDIR}/prevalp.tmp -var ${var_cdf}"
if [ ! -f ${fic_grille_nc} ] ; then
  echo "Fichier absent:"
  echo -e "$ROUGE""${fic_grille_nc}""$NORMAL"
  echo "Relancer script suremis"
  exit 1
fi
#echo $syntaxe #DEBUG
${syntaxe} | gawk '($4*'${var_fac}' > '${seuil_var}' && \
                    ($1>='${xmin_p}')&&($1<='${xmax_p}')&&($2>='${ymin_p}')&&($2<='${ymax_p}') )\
                    {print $1" "$2" "$4*'${var_fac}' }' >> ${fic_ascii}
tail -n ${npts_prevalp} ${fic_ascii} #DEBUG
rm ${TMPDIR}/prevalp.tmp
;;
### dependant du domaine de calcul####################################
*)# emis_profil=* Export de SIRANE+PREVALP � ce point
######################################################################
# Export les donn�es ASCII et les filtre sur le domaine
if [ -f ${fic_recept_nc} ] ; then
# extrait les valeurs du NetCDF -> fichier ASCII 
syntaxe="../utils/carto_gmt/export_cdf_ascii.sh ${fic_recept_nc} ${var} ${typ_op} ${nrs} ${nre}"
echo "${syntaxe} > ${TMPDIR}/conc.tmp"
${syntaxe} > ${TMPDIR}/conc.tmp || \
      { echo "export_cdf_ascii.sh: ERREUR dans la lecture de ${fic_recept_nc}" ; exit 1 ; }
npoints=`cat ${TMPDIR}/conc.tmp | wc -l`
awk '($1>='${xmin_p}')&&($1<='${xmax_p}')&&($2>='${ymin_p}')&&($2<='${ymax_p}') \
      &&($3>'${seuil_var}') { print $1" "$2" "$3 }' ${TMPDIR}/conc.tmp >> ${fic_ascii} 
echo "[${idom}/${ndom}] -> extrait SIRANE a `cat ${TMPDIR}/conc.tmp | wc -l` points de ${fic_recept_nc}"
if [ ${npoints} -eq 0 ] ; then
echo "!!!!! Warning: 0 point dans cet export  !!!!!"
fi
rm ${TMPDIR}/conc.tmp
fi 
;;
######################################################################
######################################################################
esac # case ${emis_profil} 

######################################################################
#AGGLOS
######################################################################
# EXport les donn�es ASCII et les filtre sur le domaine
if [ -f ${fic_agglos_nc} ] ; then
# extrait les valeurs du NetCDF -> fichier ASCII 
syntaxe="../utils/carto_gmt/export_cdf_ascii.sh ${fic_agglos_nc} ${var} ${typ_op} ${nrs} ${nre}"
#echo ${syntaxe}
${syntaxe} > ${TMPDIR}/conc.tmp || \
      { echo "export_cdf_ascii.sh: ERREUR dans la lecture de ${fic_agglos_nc}" ; exit 1 ; }

gawk '($1>='${xmin_p}'-1)&&($1<='${xmax_p}'+1)&&($2>='${ymin_p}'-1)&&($2<='${ymax_p}'+1) \
      &&($3>'${seuil_var}') { print $1" "$2" "$3 }' ${TMPDIR}/conc.tmp >> ${fic_ascii}
 
echo "[${idom}/${ndom}] -> extrait AGGLOS a `cat ${TMPDIR}/conc.tmp | wc -l` points de ${fic_agglos_nc}"
rm ${TMPDIR}/conc.tmp
#else
#  echo "*** warning: ${fic_agglos_nc} -> SKIP"
fi 
######################################################################
######################################################################
done # code_domaine
# <<<<< FIN BOUCLE SUR LES MINI-DOMAINES >>>>>>

if [ ! -f ${fic_ascii}  ] ; then
echo "Erreur sur ${fic_ascii}"
exit 1
fi

if [ `wc -l ${fic_ascii} | gawk '{print $1}'` -gt 0 ] ; then
echo "-> ${fic_ascii} OK"
echo "Points pour carte = `cat  ${fic_ascii} | wc -l` "
else
echo "Erreur -> aucune donn�e dans ${fic_ascii}"
exit 1
fi

####### ASCII ##
echo "*** info: creer XYZ sur domaine ${cartoprox_domaine} minidomaine ${code_domaine} var ${var} agregation ${typ_op}" 
gawk '{print $1" "$2" "$3 }' ${fic_ascii} > ${fic_xyz}
echo "-> ${fic_xyz} OK"  
rm ${fic_ascii}

else
  echo "-> WARNING : ${fic_xyz} existe deja !"  
fi # ${fic_ascii} OK

######################################################################
# Conversion geographique
######################################################################
# Genere des sorties en UTM31 (SURFER/SIG) et lat/long WGS84 (SURFER + Google Earth)
do_conversion=0
if [ ${do_conversion} -eq 1 ] ; then
#Change les coordonn�es (X,Y) des recepteurs X, Y, Z vers u autre syst�me
# convertit les coordonn�es en lat/long
gawk '{print $1" "$2}' ${fic_ascii} > ${TMPDIR}/tmp.xy_utm  
gawk '{print	  $3}' ${fic_ascii} > ${TMPDIR}/tmp.val
# UTM 31 -> lat/long
syntaxe="${conversion_exe} -utm 31 -geoid WGS84 -geo -geoid WGS84 -i tmp.xy_utm -o tmp.latlong"  
${syntaxe}
# UTM 31 -> lambert
syntaxe="${conversion_exe} -utm 31 -geoid WGS84 -l2 -geoid NTF -i tmp.xy_utm -o tmp.lambert"  
${syntaxe}
paste ${TMPDIR}/tmp.latlong ${TMPDIR}/tmp.val | gawk '{print $1" "$2" "$3 }' > ${fic_latlong}  
echo "-> ${fic_latlong} OK"
paste ${TMPDIR}/tmp.lambert ${TMPDIR}/tmp.val | gawk '{print $1" "$2" "$3 }' > ${fic_lambert}  
echo "-> ${fic_lambert} OK" 
rm -rf ${TMPDIR}/tmp.xy_utm ${TMPDIR}/tmp.val ${TMPDIR}/tmp.latlong ${TMPDIR}/tmp.lambert
fi 

######################################################################
# SORTIES GIF, GRD, NetCDF
######################################################################
fic_cdf=${fic_out_base}_utm31.nc  #NetCDF grille
fic_grd=${fic_out_base}_utm31.grd #SURFER GRD ASCII
fic_gif=${fic_out_base}_utm31.gif #image GIF
fic_vm=${fic_out_base}_utm31.vm   #Vertical Mapper
fic_kmz=${fic_out_base}_utm31.kmz  #Google Earth

if [ ${do_grd} -eq 1 ] ; then
#Interpole les recepteurs X, Y, Z vers une grille automatique
#Export en fichier NetCDF
#Export en fichier SURFER GRD ASCII 
#Export en fichier Vertical Mapper
rm -rf ${fic_grd} ${fic_vm} 
if [ -f ${fic_cdf}  ] ; then
#rm -rf ${fic_cdf} !Complete le fichier s'il existe (3/01/2011)
echo "*** ATTENTION le fichier ${fic_cdf} existe deja > ncdump -h"
ncdump -h ${fic_cdf}
fi
cd ${cartoprox}/utils/recept2grid
syntaxe="./recept2grid.sh ${fic_xyz} ${var} ${fic_cdf} ${fic_grd} ${fic_vm} ${resolution} ${xmin_p} ${ymin_p} ${xmax_p} ${ymax_p} ${cadre_interp}"
echo ${syntaxe}
${syntaxe}
fi 

if [ ${do_gif} -eq 1 ] ; then
rm -rf ${fic_gif}
cd ${cartoprox}/utils/plot_ferret
./plot_ferret.sh ${fic_cdf} ${var} ${fic_gif} ${xmin_plot} ${ymin_plot} ${xmax_plot} ${ymax_plot}
fi

if [ ${do_kmz} -eq 1 ] ; then
rm -rf ${fic_kmz}
cd ${cartoprox}/utils/plot_ferret
./make_kmz.sh    ${fic_cdf} ${var} ${fic_kmz} 
fi 

for ext in nc grd gif vm kmz ; do
  fic=${fic_out_base}_utm31.${ext}
  if [ -f ${fic} ] ; then
    echo "-> ${fic_out_base}_utm31.${ext}"
  fi
done
######################################################################
cd ${localdir}
rm -rf ${TMPDIR}

echo "***info: Fin du script $0"
