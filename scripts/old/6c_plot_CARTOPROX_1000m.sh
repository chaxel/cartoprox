#!/bin/ksh
#
# Cartographie de CARTOPROX sur une maille (exemple: bourgoin)
# Force la grille a un pas de 1000 m
# 
# usage : plot_CARTOPROX.sh + le mois voulu
#############################################

localdir=`pwd`
script=6c_plot_CARTOPROX.sh

do_export=1
do_grd=1
do_gif=1
do_png=1
do_kmz=1
#do_gmt=0

deb_j=20100101
deb_h=00
fin_j=20101231
fin_h=23

############################################################################################
# domaine de cartographie
############################################################################################
# automatique : avec tout domaine dans cartoprox.inc
xc_p="x"
yc_p="x"
dx_p="x"
dy_p="x"
xmin_p=-9999999
ymin_p=-9999999
xmax_p=9999999
ymax_p=9999999
code_domaine_loc=""
zoom_level=4
############################################################################################
# Fond utilisé pour la cartographie
############################################################################################
ifond=2 # 2 = recepteur + fond interpole CHIMERE (fournir le fichier)
############################################################################################

zoom_level=9 # resolution native de CARTOPROX (10 m)
grille_dx=1000
zoom_region=9000

case $# in
1)
var=$1
;;
3)
var=$1
deb_j=$2
deb_h=00
fin_j=$3
fin_h=23
;;
5)
var=$1
deb_j=$2
deb_h=$3
fin_j=$4
fin_h=$5
;;
*)
echo "Syntaxe $0  variable"
exit
;;
esac

############################################################################################
# Niveau de zoom
############################################################################################

############################################################################################
# INCLUDE
############################################################################################
# declaration de la periode de calcul 
if [ "${periode}" == "" ] ; then
  export periode=${deb_j}_${fin_j}
fi

deb_j=`echo ${periode} | gawk -F "_" '{print $1}'`
fin_j=`echo ${periode} | gawk -F "_" '{print $2}'`
annee=`date -d ${deb_j} +%Y`

#Include
source ${cartoprox}/cartoprox.inc ${periode} || \
  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; }
############################################################################################

############################################################################################
# Sorties
############################################################################################
#RepCarto=/mnt/mod4/data/CARTOPROX/carto/${var}_zoom${zoom_level}
RepCarto=${ZoomRes}/${periode}/${code_maille}/zoom${zoom_level}_${zoom_region}m_${grille_dx}m # multi_polluants
ListeMailles=${RepCarto}/liste.mailles

if [ ! -d ${RepCarto} ] ; then
  echo "Repertoire avec sorties maillees n existe pas : ${RepCarto}"
  echo "Relancer ${zoom_script}"
  exit 1
fi


# Choix du mode de CARTOPROX ######################################
case ${var} in
pm10_moy_an|nb_dep_50_jour)export polluant=2;;
no2_moy_an|nb_dep_200_jour)export polluant=1;;
*)
echo "Variable inconnue ${var}"
exit 1
;;
esac

## INCLUDE ########################################################
source ${cartoprox}/cartoprox.inc ${annee} || \
  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; }
selection="_user_$USER" #domaine defini par utilisateur
#Sorties GRD/netCDF/GIF
RepOut=${SCRATCH}/cartoprox${selection}
mkdir -p ${RepOut}

if [ ! -d ${RepOut} ] ; then
  echo "Impossible de creer : ${RepOut}"
  exit 1
fi

###################################################################
# Fin des modifications USER
###################################################################
echo "selection=${selection}"
echo "Parametre du domaine d extraction(UTM 31) :"
# defini le domaine de PLOT
if [ "${xc_p}" != "x" ] && [ "${yc_p}" != "x" ]  ; then
xmin_p=`echo ${xc_p} ${dx_p} | gawk '{print int($1 - $2/2.) }'`
xmax_p=`echo ${xc_p} ${dx_p} | gawk '{print int($1 + $2/2.) }'`
ymin_p=`echo ${yc_p} ${dy_p} | gawk '{print int($1 - $2/2.) }'`
ymax_p=`echo ${yc_p} ${dy_p} | gawk '{print int($1 + $2/2.) }'`
else
xmin_p=${maille_xmin}
ymin_p=${maille_ymin}
xmax_p=${maille_xmax}
ymax_p=${maille_ymax}
dx_p=`echo ${xmin_p} ${xmax_p} | gawk '{print $2 - $1 }'`
dy_p=`echo ${ymin_p} ${ymax_p} | gawk '{print $2 - $1 }'`
fi

nx_p=`echo ${dx_p} ${grille_dx} | gawk '{print int( $1 / $2) }'`
ny_p=`echo ${dy_p} ${grille_dx} | gawk '{print int( $1 / $2) }'`

echo "xmin=${xmin_p}"
echo "ymin=${ymin_p}"
echo "xmax=${xmax_p}"
echo "ymax=${ymax_p}"
echo "nx=${nx_p}"
echo "ny=${ny_p}"

if [ ${nx_p} -ge 2000 ] && [ ${ny_p} -ge 2000 ] ; then
  echo "ATTENTION: Grille de grande dimensions ${nx_p}x${ny_p}"
  if [ "${seuil_var}" == "" ] ; then
    echo "*** Erreur :specifier export seuil_var=valeur_limite"
#    exit 1
  else
    echo "seuil_var=${seuil_var}"
    echo "Continue dans 5 sec..."
    sleep 5
  fi  
fi 

# repertoire temporaire ###########################################
TMPDIR=${SCRATCH}/${script}.$RANDOM
while [ -d ${SCRATCH}/${script}.$RANDOM ] ; do
TMPDIR=${SCRATCH}/${script}.$RANDOM
done
echo "Travaille dans ${TMPDIR}"
mkdir -p ${TMPDIR}

# selectionne les minidomaines dans le domaine de PLOT#############
echo "Recherche les mailles dans ${ListeMailles}"
gawk '(($4+$6/2>'${xmin_p}')&&($4-$6/2<'${xmax_p}')&&\
       ($5+$7/2>'${ymin_p}')&&($5-$7/2<'${ymax_p}')) \
 { print $4"_"$5"_"$6"_"$7 }' ${ListeMailles} > ${TMPDIR}/mailles.txt

resolution=`gawk '(NR==1){print $6}' ${ListeMailles}`

nmailles=`wc -l ${TMPDIR}/mailles.txt | gawk '{print $1}'`
echo "Liste des mailles : "${nmailles}
cat ${TMPDIR}/mailles.txt | wc -l
if [ ${nmailles} -eq 0 ] ; then
echo "***info : aucune maille a traiter - STOP"
exit
fi

# log
log=${TMPDIR}/silence.log
##################################################################
#for var in ${var_list} ; do
##################################################################
fic_out_base=${RepOut}/${var}_${xc_p}_${yc_p}_zoom${zoom_level}

if [ ${do_export} -eq 1 ] ; then

#fichier ASCII
fic_ascii=${TMPDIR}/tmp.${var}
if [ -f ${fic_ascii} ] ; then 
 rm ${fic_ascii}
fi

#Genere un fichier avec la variable concernée
touch ${fic_ascii}

#utilise-t-on un seuil de valeur ?
if [ "${seuil_var}" == "" ] ; then
  seuil_var=-999
else
  # desactive toutes les sorties  
  echo "***info : seuil_var=${seuil_var} -> desactive les sorties graphiques"
  do_grd=0
  do_gif=0
  do_png=0
  do_kmz=0
  fic_out_base="${fic_out_base}_seuil${seuil_var}"
fi

#Nom du fichier ASCII avec les points recepteurs
fic_xyz=${fic_out_base}_utm31.txt
echo ${fic_xyz}

##################################################################################
ndom=`wc -l ${TMPDIR}/mailles.txt | gawk '{print $1}'`
#<<<<<<<<<<<<<<<<<<< Boucle sur les mini domaines>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
idom=0
cat ${TMPDIR}/mailles.txt | while read code_maille ; do
#<<<<<<<<<<<<<<<<<<< Boucle sur les mini domaines>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
idom=`expr ${idom} \+ 1`

# NOMS DES FICHIERS ##################################################
fic_cartoprox_nc=${RepCarto}/cdf/export_cartoprox_${code_maille}.cdf

if [ -f ${fic_cartoprox_nc} ] ; then

# extrait les valeurs du NetCDF -> fichier ASCII 

# Directement en UTM
${ncks} -s '%f \n' -H -C -v easting   ${fic_cartoprox_nc} > ${TMPDIR}/tmp.x
${ncks} -s '%f \n' -H -C -v northing  ${fic_cartoprox_nc} > ${TMPDIR}/tmp.y
${ncks} -s '%f \n' -H -C -v ${var}    ${fic_cartoprox_nc} > ${TMPDIR}/tmp.var
fic_ascii_tmp=${TMPDIR}/tmp.xyz

paste ${TMPDIR}/tmp.x ${TMPDIR}/tmp.y ${TMPDIR}/tmp.var | \
gawk '($1>='${xmin_p}')&&($1<='${xmax_p}')&&($2>='${ymin_p}')&&($2<='${ymax_p}') \
      &&($3>'${seuil_var}') { print $1" "$2" "$3 }' > ${fic_ascii_tmp} || \
      { echo "ERREUR dans la lecture de ${fic_cartoprox_nc}" ; exit 1 ; }     
echo "[${idom}/${ndom}] -> extrait MAILLE ${fic_cartoprox_nc} (`wc -l ${fic_ascii_tmp} | gawk '{print $1}'` points)"

cat ${fic_ascii_tmp} >> ${fic_ascii}

else
echo "Warning: pas de fichier ${fic_cartoprox_nc}"
fi 
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
echo "Erreur"
exit 1
fi

####### ASCII ##
gawk '{print $1" "$2" "$3 }' ${fic_ascii} | sort -n > ${fic_xyz}
if [ -f ${fic_xyz} ] ; then
echo "-> ${fic_xyz} OK"  
rm ${fic_ascii}
else
echo "Probleme d ecriture - EXIT"
echo "${fic_ascii} -> ${fic_xyz}"
exit 1
fi


fi # [ ${do_export} -eq 1 ] ; then

echo ${fic_xyz}
#exit 0

######################################################################
# SORTIES GIF, GRD, NetCDF
######################################################################
fic_cdf=${fic_out_base}_utm31.nc  #NetCDF grille
fic_grd=${fic_out_base}_utm31.grd #SURFER GRD ASCII
fic_gif=${fic_out_base}_utm31.gif #image GIF
fic_png=${fic_out_base}_utm31.png #image GIF
fic_vm=${fic_out_base}_utm31.vm   #Vertical Mapper
fic_kmz=${fic_out_base}_utm31.kmz #Google Earth

#si le fichier existe -> on le regenere
if [ -f ${fic_cdf} ] && [ ${do_grd} -eq 1 ]  ; then
  rm ${fic_cdf}
fi

######################################################################
# SORTIES GRD, NetCDF, Vertical Mapper
######################################################################
if [ ${do_grd} -eq 1 ] ; then
#Interpole les recepteurs X, Y, Z vers une grille automatique
#Export en fichier NetCDF
#Export en fichier SURFER GRD ASCII 
#Export en fichier Vertical Mapper

rm -rf ${fic_cdf} ${fic_grd} ${fic_vm} 
cd ${cartoprox}/utils/recept2grid
grille_dx_p=1000
syntaxe="./recept2grid.sh ${fic_xyz} ${var} ${fic_cdf} ${fic_grd} ${fic_vm} ${grille_dx_p} ${xmin_p} ${ymin_p} ${xmax_p} ${ymax_p}"
echo ${syntaxe}
${syntaxe}
#Sors uniquement le NetCDF
do_gif=0
do_kmz=0
do_png=0
fi 

if [ ! -f ${fic_cdf} ] ; then
  do_gif=0
  do_kmz=0
  do_png=0
fi

######################################################################
# SORTIES GIF
######################################################################
if [ ${do_gif} -eq 1 ] ; then
rm -rf ${fic_gif}
cd ${cartoprox}/utils/plot_ferret
xmin_plot=-9999999
ymin_plot=-9999999
xmax_plot=9999999
ymax_plot=9999999
./plot_ferret.sh ${fic_cdf} ${var} ${fic_gif} ${xmin_p} ${ymin_p} ${xmax_p} ${ymax_p} ${zoom_level}
fi

######################################################################
# SORTIES PNG
######################################################################
#PNG du domaine -> sans axes
if [ ${do_png} -eq 1 ] ; then
rm -rf ${fic_png}
./plot_ferret_V2.sh    ${fic_cdf} ${var} ${fic_png} 
fi

######################################################################
# SORTIES KMZ
######################################################################
if [ ${do_kmz} -eq 1 ] ; then
rm -rf ${fic_kmz}
./make_kmz.sh    ${fic_cdf} ${var} ${fic_kmz} 
fi 

######################################################################
cd ${localdir}
rm -rf ${TMPDIR}

echo "***info: Fin du script $0"
