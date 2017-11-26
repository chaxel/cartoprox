#!/bin/sh
##################################################
# Export X,Y,Z vers grille			 #
# E. Chaxel (echaxel@atmo-rhonealpes.org)	 #
# déc 2010					 #
##################################################
# Utilisation: Le script lance un script R 
# 16/12/2010 : creation du script

localdir=${cartoprox}/utils/recept2grid
script=recept2grid.sh

##########################################
# Modifications utilisateur
##########################################
fichier_cdf=${localdir}/defaut_grille.nc
fichier_grd=${localdir}/defaut_grille.grd
fichier_vm=${localdir}/defaut_grille.vm
polluant=defaut
resolution=auto #auto/max
resolution_max=5 #metres

interpolation=R

##########################################
# A partir de la ne plus rien modifier
##########################################

#Conversion
#conversion_exe=${cartoprox}/utils/conversion_geographique/src/conversion.exe
. ${cartoprox}/cartoprox.inc

coord_in=utm31

cadre_interp_defaut=1000 # taille du cadre en metres a rogner

case $# in
1)# entrees utilisateur
fichier_xyz=$1
cadre_interp=${cadre_interp_defaut}
;;
2)# entrees utilisateur
fichier_xyz=$1
polluant=$2
cadre_interp=${cadre_interp_defaut}
;;
3)# entrees utilisateur
fichier_xyz=$1
polluant=$2
fichier_cdf=$3
cadre_interp=${cadre_interp_defaut}
;;
4)# entrees utilisateur
fichier_xyz=$1
polluant=$2
fichier_cdf=$3
fichier_grd=$4
cadre_interp=${cadre_interp_defaut}
;;
5|6)# entrees utilisateur
fichier_xyz=$1
polluant=$2
fichier_cdf=$3
fichier_grd=$4
fichier_vm=$5
resolution_user=$6 # resolution USER en metres
cadre_interp=${cadre_interp_defaut}
;;
10|11)# entrees utilisateur
fichier_xyz=$1
polluant=$2
fichier_cdf=$3
fichier_grd=$4
fichier_vm=$5
resolution_user=$6 # resolution USER en metres
xmini=$7
ymini=$8
xmaxi=$9
ymaxi=${10}
cadre_interp=${11}
;;
12)# entrees utilisateur
fichier_xyz=$1
polluant=$2
fichier_cdf=$3
fichier_grd=$4
fichier_vm=$5
resolution_user=$6 # resolution USER en metres
xmini=$7
ymini=$8
xmaxi=$9
ymaxi=${10}
cadre_interp=${11}
grid_geo=${12}
;;
*)
echo "Syntaxe: $0 fichier_xyz polluant [fichier_cdf] "
exit
;;
esac

echo "polluant=${polluant}"

# REsolution fournie par l'utilsiateur
if [ "${cadre_interp}" == "" ] ; then
  cadre_interp=0
fi

# REsolution fournie par l'utilsiateur
if [ "${resolution_user}" != "" ] ; then
  resolution=${resolution_user}
fi

# PARAMETRES DOMAINE ##########################################
# definition des domaines
if [ "${xmini}" == "" ] ; then
xmini=`gawk 'BEGIN { min =  1.E9 }; ($1 < min && $1 != "") {min = $1}; END { print int(min) }' ${fichier_xyz}`
ymini=`gawk 'BEGIN { min =  1.E9 }; ($2 < min && $2 != "") {min = $2}; END { print int(min) }' ${fichier_xyz}`
xmaxi=`gawk 'BEGIN { max = -1.E9 }; ($1 > max && $1 != "") {max = $1}; END { print int(max) }' ${fichier_xyz}`
ymaxi=`gawk 'BEGIN { max = -1.E9 }; ($2 > max && $2 != "") {max = $2}; END { print int(max) }' ${fichier_xyz}`
fi

taille_xi=`expr ${xmaxi} \- ${xmini}`
taille_yi=`expr ${ymaxi} \- ${ymini}`

echo "GRILLE ENTREE (AVEC DONNEES)"
echo "xmini=${xmini}"
echo "ymini=${ymini}"
echo "xmaxi=${xmaxi}"
echo "ymaxi=${ymaxi}"
echo "taille_xi=${taille_xi}"
echo "taille_yi=${taille_yi}"

echo "cadre_interp=${cadre_interp}"

taille=${taille_xi}
if [ ${taille_yi} -gt ${taille} ] ; then
taille=${taille_yi}
fi

dx=${resolution_max}
dy=${resolution_max}

case ${resolution} in
auto)
if [ ${taille} -gt 2000 ] ; then
dx=10
dy=10
fi
if [ ${taille} -gt 5000 ] ; then
dx=25
dy=25
fi
if [ ${taille} -gt 15000 ] ; then
dx=50
dy=50
fi
if [ ${taille} -gt 30000 ] ; then
dx=75
dy=75
fi
if [ ${taille} -gt 50000 ] ; then
dx=100
dy=100
fi
if [ ${taille} -gt 100000 ] ; then
dx=200
dy=200
fi
if [ ${taille} -gt 150000 ] ; then
dx=250
dy=250
fi
if [ ${taille} -gt 200000 ] ; then
echo "Resolution AUTO : taille de domaine non supportee"
exit 1
fi
echo "dx=${dx}m"
echo "dy=${dy}m"
;;
max)
echo "ATTENTION: resolution MAX dx=${dx}m dy=${dy}m"
echo "           interpolation peut prendre du temps !"
sleep 3
;;
*)
dx=${resolution}
dy=${resolution}
echo "Force la resolution dx=${dx}m dy=${dy}m"
;;
esac

xmino=`echo ${xmini} ${cadre_interp}  | gawk '{ print $1 + $2 }'`
ymino=`echo ${ymini} ${cadre_interp}  | gawk '{ print $1 + $2 }'`
xmaxo=`echo ${xmaxi} ${cadre_interp}  | gawk '{ print $1 - $2 }'`
ymaxo=`echo ${ymaxi} ${cadre_interp}  | gawk '{ print $1 - $2 }'`

# taille du cadre ennombre de mailles a rogner
nxi=`echo ${xmini} ${xmaxi} ${dx}   | gawk '{ print int(($2+1.-$1)/$3)}'`
nyi=`echo ${ymini} ${ymaxi} ${dy}   | gawk '{ print int(($2+1.-$1)/$3)}'`

# taille du cadre de sortie
nxo=`echo ${xmino} ${xmaxo} ${dx}   | gawk '{ print int(($2+1.-$1)/$3)}'`
nyo=`echo ${ymino} ${ymaxo} ${dy}   | gawk '{ print int(($2+1.-$1)/$3)}'`

# CREATION SCRATCH ##########################################
nom_grille=${xmino}_${xmaxo}_${xmaxo}_${ymaxo}_${polluant}

TMPDIR=${SCRATCH}/${script}.${nom_grille}.$RANDOM
while [ -d ${SCRATCH}/${script}.${nom_grille}.$RANDOM ] ; do
TMPDIR=${SCRATCH}/${script}.${nom_grille}.$RANDOM
done
echo "Travaille dans ${TMPDIR}"
mkdir -p ${TMPDIR}

# COPIE DONNEES ##########################################
echo "Fichier entree"
echo "-> ${fichier_xyz} `wc -l ${fichier_xyz} | gawk '{print $1}'` points"

cd ${TMPDIR}

#A-t-on besoin d'une interpolation ?
npoints_xyz=`cat ${fichier_xyz} | wc -l`
npoints_grille=`expr ${nxo} \* ${nyo}`

echo "${fichier_xyz} -> ${npoints_xyz} points" 
echo "Grille sortie -> ${npoints_grille} points" 

#Si aucun point n'est en dehors du cadre, on n'utilise pas d'interpolation
if [ ${cadre_interp} -eq 0 ] ; then 
echo "-> Pas d'interpolation"
interpolation=NO
else
echo "-> Interpolation ${interpolation}" # NO ou R
fi

case ${interpolation} in
R)
# Copie le script R
cp  ${localdir}/interp2d.R  ${TMPDIR}/interp2d.R 
echo "-> ${TMPDIR}/interp2d.R"

# Moyenne les doublons #######################################
echo "Elimine les doublons->moyenne sur la grille d interpolation"
#rm -rf elimine_doublons_grille.exe
if [ ! -f ${localdir}/src/elimine_doublons_grille.exe ] ; then
  ${F90} -o ${localdir}/src/elimine_doublons_grille.exe ${localdir}/src/elimine_doublons_grille.f90
fi
echo "fichier_xyz=${fichier_xyz}"
echo "xmini=${xmini}"
echo "ymini=${ymini}"
echo "nxi=${nxi}"
echo "nyi=${nyi}"
echo "dx=${dx}"
echo "dy=${dy}"
echo "-------------------------------------"
head -n 5 ${fichier_xyz}
echo "-------------------------------------"
#si le programme elimine doublon renvoit des valeurs NaN condition >0 dans le awk...
${localdir}/src/elimine_doublons_grille.exe -xmin ${xmini} -ymin ${ymini} -nx ${nxi} -ny ${nyi} -dx ${dx} -dy ${dy} \
  -i ${fichier_xyz}  | sort -r -n -k4 | gawk '($3>=0){print $1";"$2";"$3";"$4}' >   ${TMPDIR}/points.xyz
echo "-> ${TMPDIR}/points.xyz `wc -l ${TMPDIR}/points.xyz | gawk '{print $1}'` points"
echo "-------------------------------------"
head -n 5 ${TMPDIR}/points.xyz
echo "-------------------------------------"
num_grille=`expr ${nyi} \* ${nxi}`
num_points=`wc -l ${TMPDIR}/points.xyz | gawk '{print $1}'`
echo "Points XYZ=${num_points}"
echo "Points GRILLE=${num_grille}"

# GENERE LA GRILLE ##########################################
echo "Genere la grille"
# genere une grile reguliere
rm -rf ${TMPDIR}/grille.xyz
#rm -rf genere_grille.exe
if [ ! -f ${localdir}/src/genere_grille.exe ] ; then
  ${F90} -o ${localdir}/src/genere_grille.exe ${localdir}/src/genere_grille.f90
fi
${localdir}/src/genere_grille.exe -xmin ${xmino} -ymin ${ymino} -nx ${nxo} -ny ${nyo} -dx ${dx} -dy ${dy} | \
  gawk '{print $1";"$2}' > ${TMPDIR}/grille.xy
echo "-> ${TMPDIR}/grille.xy `wc -l ${TMPDIR}/grille.xy | gawk '{print $1}'` points"

# SCRIPT R ##################################################
# Fait un test en laissant l'interpolateur se debrouiller 
# tout seul avec les doublons dans les mailles...
# lit points.xyz
# creer grille.xyz
echo "Interpolation R en cours"
R CMD BATCH interp2d.R 
do_vm=0
do_grd=0
;;

NO)
# PAS D INTERPOLATION ##########################################
echo "Pas d interpolation points(${num_points})=grille(${num_grille})"
gawk '{print $1";"$2";"$3}' ${fichier_xyz} > ${TMPDIR}/grille.xyz
do_vm=1
do_grd=1
grid_geo=grid_geo
;;
esac

#Ecrire les variables long/lat dans netCDF de sortie
grid_geo=grid_geo

if [ ! -f  ${TMPDIR}/grille.xyz ] ;then
echo "Interpolation echoue $0"
echo "Voir ${TMPDIR}/interp2d.Rout"
exit 1
fi

taille_xo=`expr ${xmaxo} \- ${xmino}`
taille_yo=`expr ${ymaxo} \- ${ymino}`

if [ "${periode}" == "" ] ; then
  periode="indefini"
fi

echo "GRILLE DE SORTIE"
echo "periode=${periode}"
echo "xmino=${xmino}"
echo "ymino=${ymino}"
echo "xmaxo=${xmaxo}"
echo "ymaxo=${ymaxo}"
echo "taille_xo=${taille_xo}"
echo "taille_yo=${taille_yo}"
echo "nxo=${nxo}"
echo "nyo=${nyo}"

# Post-traitement ##################################################
echo "${periode} ${nxo} ${nyo} '${polluant}' ${xmino} ${ymino} ${dx} ${dy}" > ${TMPDIR}/grille.input
gawk -F ';' '($1>='${xmino}'-'${resolution}' && $1<='${xmaxo}'+'${resolution}'  && \
              $2>='${ymino}'-'${resolution}' && $2<='${ymaxo}'+'${resolution}' ){ print $1" "$2" "$3  }' \
${TMPDIR}/grille.xyz | sed 's/NA/-9999./g' >> ${TMPDIR}/grille.input

# NetCDF ##################################################
# Converti en NetCDF pour ensuite cartographie
# easting, northing, var,( long, lat )
#rm -rf ${TMPDIR}/carte.nc
echo "Converti ${TMPDIR}/grille.input -> NetCDF"
if [ ! -f ${localdir}/src/grille2nc.exe ] ; then
cd ${localdir}/src
make
fi

# --> repertoire temporaire
cd ${TMPDIR}
#Construit une grille UTM31 + option lat/lon
#grille2nc.exe ${TMPDIR}/grille.input ${fichier_cdf} ${grid_geo}
${localdir}/src/grille2nc.exe ${TMPDIR}/grille.input ${fichier_cdf} ${grid_geo}

if [ ! -f  ${fichier_cdf} ] ; then
echo "Conversion NetCDF echoue $0"
exit 1
else
echo "-> ${fichier_cdf}"
ncdump -h ${fichier_cdf}
fi

if [ ${do_grd} -eq 1 ] ; then
# GRID ##################################################
# Converti en GRID SURFER ASCII pour ensuite cartographie
# easting, northing, var,( long, lat )
echo "Converti ${TMPDIR}/grille.input -> SURFER GRD"
if [ ! -f ${localdir}/src/grille2grd.exe ] ; then
  ${F90} -o ${localdir}/src/grille2grd.exe  ${localdir}/src/grille2grd.f90
fi
${localdir}/src/grille2grd.exe ${TMPDIR}/grille.input  | gawk '{print $1" "$2" "$3" "$4" "$5" "$6" "$7" "$8" "$9" "$10 }' > ${TMPDIR}/carte.grd

if [ ! -f ${TMPDIR}/carte.grd ] ; then
echo "Conversion SURFER GRD ASCII echoue $0"
exit 1
else
mv ${TMPDIR}/carte.grd ${fichier_grd}
echo "-> ${fichier_grd}"
fi
fi

if [ ${do_vm} -eq 1 ] ; then
# Vertcal Mapper ########################################
# Converti en format Vertaical Mapper
# easting, northing, var,( long, lat )
echo "Converti ${TMPDIR}/grille.input -> Vertical Mapper"
if [ ! -f ${localdir}/src/grille2VM.exe ] ; then
  ${F90} -o ${localdir}/src/grille2VM.exe  ${localdir}/src/grille2VM.f90
fi
#Genere la grille avec des separateurs "1 espace"
${localdir}/src/grille2VM.exe ${TMPDIR}/grille.input > ${TMPDIR}/carte.vm
#| sed 's/^[ ]*//' | gawk -F "( *)" -v OFS=" " '$1=$1' > ${TMPDIR}/carte.vm || \
#   { rm -rf ${TMPDIR}/carte.vm ; }

if [ ! -f ${TMPDIR}/carte.vm ] ; then
echo "Conversion Vertical Mapper echoue $0"
else
mv ${TMPDIR}/carte.vm ${fichier_vm}
echo "-> ${fichier_vm}"
fi
fi

# SUPPRESSION
cd ${localdir}
rm -rf ${TMPDIR} #TOTO

echo "OK... Fichier de sortie : ${fichier_cdf}"
echo "fin du script $0"
exit 0
