#!/bin/sh

echo Content-Type: text/html
echo ""

#################### PARAMETRES #####################################
image_dx=673
image_dy=756

#################### FUNCTIONS #####################################
COMMENCE_HTML() {
/bin/cat << EOM
<html>
<head><title>Export PREVALP/CARTOPROX en cours...</title>
<style type="text/css"> 
body {
    margin: 10;
    margin-bottom: 3em;
    padding: 10;
    font-size: 10pt;
    font-family: Verdana, Arial, Helvetica, sans-serif;
    background-color: #fff;
    color: #000;
}
</style> 
</HEAD>
<body>
EOM
}

TERMINE_HTML() {
/bin/cat << EOM
</BODY>
</HTML>
EOM
}


##########################################################
#ARGUMENTS
##########################################################
commande=$( echo $QUERY_STRING | tr '&' ';')
#echo $commande
eval $commande
#echo ${var} ${x} ${y} ${dx} ${dy} ${zoom}
var_loc=${var}
x_loc=${x}
y_loc=${y}
dx_loc=${dx}
dy_loc=${dy}
zoom_loc=${zoom}

############################################################
#DATES
############################################################
date=`date +%d/%m/%Y' '%H':'%M`
date_html=`date +%d/%m/%Y' &agrave; '%H':'%M`

############################################################
#VERIFIE LES ARGUMENTS
############################################################
if  [ "${x_loc}" == "" ] && [ "${zoom_loc}" == "" ] ; then
COMMENCE_HTML
/bin/cat << EOM
<br><br>
<strong><font color="#F0000">ERREUR: Probleme avec la lecture des arguments<br><br>
Utilisez : $0&var=${var}&x=${x}&y=${y}&dx=${dx}&dy=${dy}&zoom=${zoom}
</font></strong>
EOM
TERMINE_HTML
exit 1
fi

############################################################
#VERIFIE LA DISPONIBILITE DU SERVEUR (MAX 5 THREADS)
############################################################

nthreads=`ps -ef  | grep export_cartoprox.sh | wc -l `
if  [ ${nthreads} -gt 5 ] ; then
COMMENCE_HTML
/bin/cat << EOM
<br><br>
<strong><font color="#F0000">ERREUR: Serveur Occup&eacute; - nombres de taches en cours : ${nthreads}<br><br>
Patientez quelques instants et r&eacute;essayer. Merci.<br><br>
Date : ${date}
</font></strong>
EOM
TERMINE_HTML
exit 1
fi



############################################################
#CHEMIN DES FICHIERS
############################################################
#echo La valeur de PREVALP au point x ${x_utm} y ${y_utm} est : 
dir_out="/data/web/cartoprox/export" #Repertoire des exports
dir_lvl="/data/web/cartoprox/level"  #Repertoire des levels SURFER
www_dir_export="http://172.16.30.93/cartoprox/export"

#label=$RANDOM
label=`date +%Y%m%d'_'%H%M%S`
fic_log="/data/web/cartoprox/log/export_cartoprox_${label}.log"
www_log="http://172.16.30.93/cartoprox/log/export_cartoprox_${label}.log"
fic_readme="lisez-moi_${label}.txt"
fic_zip="export_cartoprox_${label}.zip"
fic_gif="export_cartoprox_${label}.gif"
fic_grd="export_cartoprox_${label}.grd"
fic_lvl="export_cartoprox_${label}.lvl"
fic_vm="export_cartoprox_${label}.vm"
fic_kmz="export_cartoprox_${label}.kmz"

if [ -f ${fic_log} ] ; then
rm    ${fic_log}
fi
touch ${fic_log}

############################################################################################
#Charge les variables locales
############################################################################################
source /home/oper/.bashrc  >> ${fic_log} 2>&1
export polluant=1
source /appli/CARTOPROX/cartoprox.inc 2009  >> ${fic_log} 2>&1

############################################################
#Repertoire temporaire
############################################################
selection=_user_
tmp_dir="${SCRATCH}/cartoprox${selection}"
mkdir -p ${tmp_dir} >> ${fic_log} 2>&1

############################################################################################
#Nom des variables & unites
############################################################################################
unites_variable="microgrammes par mètre cube"
unites_html="&mu;g/m<SUP>3</SUP>"
case ${var_loc} in
no2_moy_an)
nom_variable="moyenne annuelle de NO2"
nom_html="moyenne annuelle de NO<SUB>2</SUB>"
;;
pm10_moy_an)
nom_variable="moyenne annuelle de PM10"
nom_html="moyenne annuelle de PM10"
;;
nb_dep_50_jour)
nom_variable="nombre de jours dépassant 50 microg/m3"
nom_html="nombre de jours dépassant 50 &mu;g/m3"
unites_variable="nombre de jours"
unites_html="nombre de jours"
;;
*)
nom_variable=${var_loc}
nom_html=${nom_variable}
;;
esac

############################################################################################
# Niveau de zoom
############################################################################################
case ${zoom_loc} in
14)
#zoom_GM=14 #QUARTIER
region_dx=2000
grille_dx=5  
;;
13)
#zoom_GM=13 #VILLAGE
region_dx=3000
grille_dx=10
;;
12)
#zoom_GM=12 #VILLE
region_dx=5000
grille_dx=25
;;
11)
#zoom_GM=11 #AGGLO
region_dx=10000
grille_dx=50
;;
10)
#zoom_GM=10 #DEPARTEMENT
region_dx=15000
grille_dx=100
;;
9)
#zoom_GM=9 #REGION
region_dx=20000
grille_dx=200
;;
8)
#zoom_GM=8 #REGION
region_dx=40000
grille_dx=400
;;
7)
#zoom_GM=7 #INTER-REGION
region_dx=80000
grille_dx=800

xmin_loc=553000
ymin_loc=4887000
xmax_loc=828000
ymax_loc=5155000
echo "Regle speciale pour zoom " >> ${fic_log} 2>&1
x_loc=`echo ${xmin_loc} ${xmax_loc} | awk '{print int(($1+$2)/2) }'`
y_loc=`echo ${ymin_loc} ${ymax_loc} | awk '{print int(($1+$2)/2) }'` 
dx_loc=`echo ${xmin_loc} ${xmax_loc} | gawk '{print $2 - $1 }'` 
dy_loc=`echo ${ymin_loc} ${ymax_loc} | gawk '{print $2 - $1 }'` 
echo "x_loc=${x_loc}" >> ${fic_log} 2>&1
echo "y_loc=${y_loc}" >> ${fic_log} 2>&1
echo "dx_loc=${dx_loc}" >> ${fic_log} 2>&1
echo "dy_loc=${dy_loc}" >> ${fic_log} 2>&1
;;
*)
COMMENCE_HTML
/bin/cat << EOM
<br><br>
<strong><font color="#F0000">ERREUR: Niveau de zoom non support&eacute;: ${zoom_loc}<br>
<br>Choisir un niveau de zoom entre 7 et 14</font>
EOM
TERMINE_HTML
exit 1
;;
esac

############################################################################################
# Noms des fichiers
############################################################################################
fic_base="conc_2009010100_2010010100_${var_loc}_seuil0${selection}_${x_loc}_${y_loc}_resauto_utm31"

case ${zoom_loc} in
7|8|9|10|11|12)
plot_script=6c_plot_CARTOPROX_V2.sh
script_res=${zoom_loc}
fic_base="${var_loc}_${x_loc}_${y_loc}_zoom${zoom_loc}_utm31"
;;
13|14)
COMMENCE_HTML
/bin/cat << EOM
P<br>
Choisir un niveau de zoom entre 7 et 14
EOM
TERMINE_HTML
exit 0

plot_script=6a_plot_CARTOPROX.sh
script_res=
fic_base="conc_2009010100_2010010100_${var_loc}_seuil0${selection}_${x_loc}_${y_loc}_resauto_utm31"
;;
*)
COMMENCE_HTML
/bin/cat << EOM
Niveau de zoom non support&eacute;: ${zoom_loc}<br>
Choisir un niveau de zoom entre 7 et 14
EOM
TERMINE_HTML
exit 0
;;
esac

### Syntaxe du script EXPORT ###
syntaxe="${plot_script} ${var_loc} ${x_loc} ${y_loc} ${dx_loc} ${dy_loc} ${script_res}" 
echo "${syntaxe}"  >> ${fic_log} 2>&1

### REQUETE
#Lance la requete
cd ${cartoprox}/scripts
${syntaxe} >> ${fic_log} 2>&1

#Liste des fichiers en sortie de 6a_plot_CARTOPROX.sh
#Nom du fichier comme defini dans 6a_plot_CARTOPROX.sh
fic_gif_loc=${tmp_dir}/${fic_base}.gif
fic_txt_loc=${tmp_dir}/${fic_base}.txt
fic_cdf_loc=${tmp_dir}/${fic_base}.nc
fic_grd_loc=${tmp_dir}/${fic_base}.grd
fic_vm_loc=${tmp_dir}/${fic_base}.vm
fic_kmz_loc=${tmp_dir}/${fic_base}.kmz

fic_lvl_loc=${dir_lvl}/${var_loc}.lvl

chmod uog+w ${fic_gif} ${fic_grd} ${fic_cdf_loc} ${fic_txt_loc}

############################################################################################
# IMAGE
############################################################################################
if [ -f ${fic_gif_loc} ] ; then
mv ${fic_gif_loc} ${dir_out}/${fic_gif}
chmod uog+r ${fic_gif}
fi

############################################################################################
# ZIP ARCHIVE
############################################################################################
if [ -f ${fic_kmz_loc} ] ; then
mv ${fic_kmz_loc} ${dir_out}/${fic_kmz}
fi

if [ -f ${fic_vm_loc} ] && [ -f ${fic_grd_loc} ] ; then
# Deplace les fichiers
mv ${fic_grd_loc} ${dir_out}/${fic_grd}
mv ${fic_vm_loc}  ${dir_out}/${fic_vm}
# Copie l'échelle de couleur
cp ${fic_lvl_loc} ${dir_out}/${fic_lvl}
cd ${dir_out}
zip ${fic_zip} ${fic_gif} ${fic_vm} ${fic_grd} ${fic_lvl} >> ${fic_log} 2>&1
chmod uog+r ${fic_zip}
rm ${fic_vm} ${fic_grd} ${fic_lvl}
else
rm ${fic_zip}
fi

############################################################################################
#Supprime NetCDF et ASCII
############################################################################################
rm  ${fic_txt_loc}  ${fic_cdf_loc}

############################################################################################
# READ ME
############################################################################################
/bin/cat << EOM > ${dir_out}/${fic_readme}
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++    
+ Cartoprox - version 2009v08
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Ce fichier a été généré automatiquement le ${date}, il restera disponible pour une durée de 8 jours 
à l'adresse : ${www_dir_export}/${fic_readme}
L'image qui l'accompagne est disponible a l'adresse : ${www_dir_export}/${fic_gif}

Explication
-----------
Les donnnées de qualité de l'air ont été générées avec le modèle CARTOPROX.
CARTOPROX est un modèle de calcul de la dispersion des polluants sur les axes inter-urbains qui
couvre toute la région Rhône-Alpes. CARTOPROX utilise la pollution de fond issue du modèle PREVALP assimilé.
Le modèle PREVALP prend en compte les données de mesures des stations fixes rurales, urbaines et périurbaines. 
CARTOPROX inclus également les calculs réalisés sur certaines agglomérations avec SIRANE pour l'année 
2009 : Annecy, Annemasse, Chambéry, Grenoble, Lyon et Valence.
Les axes routiers pris en compte dans le calcul, en plus de ceux des agglomérations citées plus haut,
sont les AUTOROUTES et anciennes routes NATIONALES. Les autres axes routiers ne sont pas explicitement
décrits par CARTOPROX et possède une valeur de fond.

La cartographie représente la variable "${nom_variable}" sur un domaine défini en UTM 31 par son
centre (X=${x_loc} m,Y=${y_loc} m) et son étendue (DX=${dx_loc} m,DY=${dy_loc} m)
Les unités sont : ${unites_variable}
EOM

if [ -f ${dir_out}/${fic_zip} ]  ; then
/bin/cat << EOM >> ${dir_out}/${fic_readme}

Contenu de l'archive
--------------------

L'archive au format ZIP ${www_dir_export}/${fic_zip} comprend :
- ${fic_gif} : L'image en format GIF générée sur ce domaine pour la variable ${var_loc}. Les axes sont en km UTM 31 WGS84.
- ${fic_vm}  : La grille au format Vertical Mapper pour Mapinfo la résolution ${grille_dx} mètres en UTM 31 WGS84.
- ${fic_grd} : La grille au format Surfer Grid ASCII pour Surfer la résolution ${grille_dx} mètres en UTM 31 WGS84.
- ${fic_lvl} : L'échelle de couleur pour Surfer

Le fichier de données restera diponible pour une durée de 8 jours à l'adresse : 
${www_dir_export}/${fic_zip}
EOM
fi

if [ -f ${dir_out}/${fic_kmz} ]  ; then
/bin/cat << EOM >> ${dir_out}/${fic_readme}

Fichier KMZ Google Earth
------------------------
Le fichier KMZ pour Google Earth: ${www_dir_export}/${fic_kmz}.
Ce fichier est généré pour des valeurs du zoom allant jusqu'à 11. La résolution de la carte est de ${grille_dx} mètres. 
EOM
fi

/bin/cat << EOM >> ${dir_out}/${fic_readme}

AVERTISSEMENT
-------------
La résolution des données correspond au niveau de zoom.
La résolution de vos données est de ${grille_dx} mètres. La résolution maximale de 
CARTOPROX est de 5 mètres. Cette résolution s'obtient en zoomant au maximum (Zoom 14).

Références
----------
Rapport région 2010 MESURES
http://visionair/projets/cartoprox09/Documents/2009_CartoProx_Rapport_2-1_Mesures_FINAL.pdf
Rapport région 2010 MODELISATION
http://visionair/projets/cartoprox09/Documents/2009_CartoProx_Rapport_2-2_Mod%C3%A9lisation_FINAL.pdf

Pour toute question sur l'utilisation des données, s'adresser au service modélisation :
modelisation@atmo-rhonealpes.org

Bonne utilisation de CARTOPROX !
EOM

# Ajoute a l'archive
if [ -f ${dir_out}/${fic_zip} ]  ; then
cd ${dir_out}
zip ${fic_zip} ${fic_readme}  >> ${fic_log} 2>&1
fi

############################################################################################
#HEADER
############################################################################################
COMMENCE_HTML
if [ ! -f ${dir_out}/${fic_gif} ] ; then
/bin/cat << EOM
<p>
Export echoue ! <a href="${www_log}">fichier log</a><br>
Vous &ecirc;tes en dehors du domaine de la r&eacute;gion ?<br>
Sinon contacter: <a href=\"mailto:modelisation@atmo-rhonealpes.org?subject=CARTOPROX\">modelisation@atmo-rhonealpes.org</a>
</p>
EOM
TERMINE_HTML
exit 1
fi

############################################################################################
#affichage HTML DONNEES
############################################################################################
/bin/cat << EOM
<p>
<strong>Cartographie CARTOPROX ann&eacute;e 2009</strong> - Version: 2009v08 - Carte g&eacute;n&eacute;r&eacute;e le ${date_html}<br>
Variable: <font color="f0000">${nom_html}</font> - Unit&eacute;s: <font color="f0000">${unites_html}</font> - Axes: kilom&egrave;tres UTM 31 WGS84<br>
R&eacute;solution: <font color="f0000">${grille_dx} m&egrave;tres</font></p>

<p>Fichier <a href='${www_dir_export}/${fic_readme}'>Lisez-moi</a>
EOM

if [ -f ${dir_out}/${fic_zip} ] ; then
echo " - <strong>T&eacute;l&eacute;chargez la <a href='${www_dir_export}/${fic_zip}'>carte au format Mapinfo/Surfer/GIF</a></strong>"
fi

if [ -f ${dir_out}/${fic_kmz} ] ; then
echo " - <strong>T&eacute;l&eacute;chargez la <a href='${www_dir_export}/${fic_kmz}'>carte au format Google Earth KMZ</a></strong>"
fi

echo "</p>"
############################################################################################
#affichage HTML IMAGE
############################################################################################
echo "<p><img src='${www_dir_export}/${fic_gif}' width=\"${image_dx}\" height=\"${image_dy}\"></p>"

echo "</div>"

TERMINE_HTML

exit 0


