#!/bin/sh
#----------------------------------------------------------------------------
# CGI-BIN CARTOPROX (12/2010)
#----------------------------------------------------------------------------
# MODE D'EMPLOI
#1.Ecrire un script export_cartoprox.sh dans le CGI-BIN de la machine avec le serveur WEB :
#  !/bin/sh
#  /mnt/mod4/appli/CARTOPROX/interface_web/export_cartoprox.sh $QUERY_STRING
#  exit 0
#
#2.Autoriser la lecture/ecriture du repertoire $SCRATCH a l'utilisateur wwwrun
#
#3.Autoriser la lecture/ecriture des repertoires $webdir/CARTOPROX/log et 
#  $webdir/CARTOPROX/export a l'utilisateur wwwrun

#MODE MAINTENANCE 0/1 (1=en maintenance)
maintenance=0

#SERVEUR=
#serveur="172.16.30.89"
serveur_ip=`/sbin/ifconfig | grep "172.16.30." |gawk -F '( *|:)' '{print $4}'`
serveur_name=`echo $HOSTNAME`
serveur=${serveur_ip}
webdir="/mnt/mod3/data/www/html"

#Lit les arguments renvoyés par le CGI-BIN
QUERY_STRING=$1

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
periode_loc=${periode}
domaine_loc=${domaine}
selection_loc=${selection}

############################################################
#DATES
############################################################
date=`date +%d/%m/%Y' '%H':'%M`
date_html=`date +%d/%m/%Y' &agrave; '%H':'%M`

############################################################
#MAINTENANCE
############################################################
if  [ "${maintenance}" == "1" ]  ; then
COMMENCE_HTML
/bin/cat << EOM
<br><br>
<strong><font color="#F0000">
Le site est en cours de maintenance<br><br>
Merci de votre compr&eacute;hension
</font></strong>
EOM
TERMINE_HTML
exit 1
fi

############################################################
#CALCUL EN COURS (6b_grid_CARTOPROX.sh)
############################################################
if  [ `ps -ef | grep '6c_map_raster.sh' | wc -l` -gt 3 ]  ; then
COMMENCE_HTML
/bin/cat << EOM
<br><br>
<strong><font color="#F0000">
Le mod&egrave;le CARTOPROX est en cours de calcul (6b_grid_CARTOPROX.sh)<br><br>
Cette interface est indisponible pour le moment, merci de votre compr&eacute;hension
</font></strong>
EOM
TERMINE_HTML
exit 1
fi

############################################################
#VERIFIE LES ARGUMENTS
############################################################
if  [ "${x_loc}" == "" ] && [ "${zoom_loc}" == "" ] ; then
COMMENCE_HTML
/bin/cat << EOM
<br><br>
<strong><font color="#F0000">
ERREUR: Probleme avec la lecture des arguments<br><br>
Utilisez : $0&var=${var}&x=${x}&y=${y}&dx=${dx}&dy=${dy}&zoom=${zoom}&domaine=${domaine}&selection=${selection}&periode=${periode}
</font></strong>
EOM
TERMINE_HTML
exit 1
fi

############################################################
#VERIFIE LA DISPONIBILITE DU SERVEUR (MAX 5 THREADS)
############################################################
nthreads=`ps -ef  | grep export_cartoprox.sh | wc -l `
if  [ ${nthreads} -gt 10 ] ; then
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
dir_out="${webdir}/cartoprox/export" #Repertoire des exports
dir_lvl="${webdir}/cartoprox/level"  #Repertoire des levels SURFER
www_dir_export="http://${serveur}/cartoprox/export"

#label=$RANDOM
label=`date +%Y%m%d'_'%H%M%S`
fic_log="${webdir}/cartoprox/log/export_cartoprox_${label}.log"
www_log="http://${serveur}/cartoprox/log/export_cartoprox_${label}.log"
fic_readme="lisez-moi_${label}.txt"
fic_zip="export_cartoprox_${label}.zip"
fic_gif="export_cartoprox_${label}.gif"
fic_grd="export_cartoprox_${label}.grd"
fic_lvl="export_cartoprox_${label}.lvl"
fic_vm="export_cartoprox_${label}.vm"
fic_kmz="export_cartoprox_${label}.kmz"
fic_png="export_cartoprox_${label}.png"

if [ -f ${fic_log} ] ; then
rm    ${fic_log}
fi
#touch /mnt/mod3/data/tmp/toto
touch ${fic_log} #> /mnt/mod3/data/tmp/toto 2>&1
#echo "${fic_log}<br>"
echo "Export en cours sur serveur ${serveur_name} (IP ${serveur_ip})..."

############################################################################################
#Charge les variables locales
############################################################################################
#echo "Charge les variables SYSTEM<br>"
source /home/oper/.bashrc  >> ${fic_log} 2>&1
export polluant=1
export cartoprox_domaine=${domaine_loc}
export selection=${selection_loc}
export periode=${periode_loc}
export cartoprox=/mnt/mod4/appli/CARTOPROX_V3
#export cartoprox_domaine=region_ARN
source ${cartoprox}/cartoprox.inc ${periode} >> ${fic_log} 2>&1

deb_j=`echo ${periode} | gawk -F "_" '{print $1}'`
fin_j=`echo ${periode} | gawk -F "_" '{print $2}'`
deb_h=00
fin_h=23

############################################################
#Repertoire temporaire
############################################################
selection_user=_user_
tmp_dir="${SCRATCH}/cartoprox${selection_user}"
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
nb_dep_200_jour)
nom_variable="nombre de jours dépassant 200 microg/m3"
nom_html="nombre de jours avec un de&eacute;passement de 200 &mu;g/m</SUP>3</SUP>"
;;
pm10_moy_an)
nom_variable="moyenne annuelle de PM10"
nom_html="moyenne annuelle de PM10"
;;
nb_dep_50_jour)
nom_variable="nombre de jours dépassant 50 microg/m3"
nom_html="nombre de jours d&eacute;passant 50 &mu;g/m</SUP>3</SUP>"
unites_variable="nombre de jours"
unites_html="nombre de jours"
;;
no2_moy_an)
nom_variable="moyenne annuelle de NO2"
nom_html="moyenne annuelle de NO<SUB>2</SUB>"
;;
nb_dep_200_jour)
nom_variable="nombre de jours dépassant 200 microg/m3"
nom_html="nombre de jours avec un de&eacute;passement de 200 &mu;g/m</SUP>3</SUP>"
;;
pm10_moy_an)
nom_variable="moyenne annuelle de PM10"
nom_html="moyenne annuelle de PM10"
;;
*NO2*)
nom_variable="Maximum NO2"
nom_html="Maximum O</SUP>3</SUP>"
unites_variable="microg/m3"
unites_html="&mu;g/m3"
;;
*NO*)
nom_variable="Maximum NO"
nom_html="Maximum O</SUP>3</SUP>"
unites_variable="microg/m3"
unites_html="&mu;g/m3"
;;
*O3*)
nom_variable="Maximum O3"
nom_html="Maximum O</SUP>3</SUP>"
unites_variable="microg/m3"
unites_html="&mu;g/m3"
;;
*)
COMMENCE_HTML
/bin/cat << EOM
<br><br>
<strong><font color="#F0000">ERREUR: "Variable inconnue dans $0"</font></strong>
EOM
TERMINE_HTML
exit 1
;;
esac

############################################################################################
# Niveau de zoom (zoom_loc=zomm_cartoprox)
############################################################################################
case ${zoom_loc} in
14)
zoom_level=8
#zoom_GM=14 #QUARTIER
grille_dx=5  
;;
13)
zoom_level=7
#zoom_GM=13 #VILLAGE
grille_dx=10
;;
12)
zoom_level=6
#zoom_GM=12 #VILLE
grille_dx=25
;;
11)
zoom_level=5
#zoom_GM=11 #AGGLO
grille_dx=50
;;
10)
zoom_level=4
#zoom_GM=10 #DEPARTEMENT
grille_dx=75
;;
9)
zoom_level=3
#zoom_GM=9 #REGION
grille_dx=150
;;
8)
zoom_level=2
#zoom_GM=8 #REGION
grille_dx=250
;;
7)
zoom_level=1
#zoom_GM=7 #INTER-REGION
#region_dx=80000
grille_dx=500

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
<br>Choisir un niveau de zoom entre 7 et 14</font></strong>
EOM
TERMINE_HTML
exit 1
;;
esac
############################################################################################
# Calcul de la taille de la grille (taille max 2000*2000)
############################################################################################
# defini le domaine de PLOT
nx_p=`echo ${dx_loc} ${grille_dx} | gawk '{print int( $1 / $2) }'`
ny_p=`echo ${dy_loc} ${grille_dx} | gawk '{print int( $1 / $2) }'`

if [ ${nx_p} -ge 2000 ] && [ ${ny_p} -ge 2000 ] ; then
COMMENCE_HTML
/bin/cat << EOM
<br><br>
<strong><font color="#F0000">ERREUR: image de sortie trop grande ${nx_p}x${ny_p} (limite 2000x2000)<br>
<br>1. Choisir un niveau de zoom plus faible<br>
<br>2. Passez en mode R&eacute;solution auto</font></strong>
EOM
TERMINE_HTML
exit 1
fi 

############################################################################################
# Noms des fichiers
############################################################################################
case ${zoom_level} in
1|2|3|4|5|6)
plot_script=${map_script}
script_res=${zoom_level}
fic_base="${var_loc}_${x_loc}_${y_loc}_zoom${zoom_level}_utm31"
;;
7|8)
#COMMENCE_HTML
#/bin/cat << EOM
#<br><br>
#<strong><font color="#F0000">En raison de probl&egrave;me informatiques<br>
#<br>les niveaux de zoom 13 et 14 ne sont pas disponibles</font></strong>
#EOM
#TERMINE_HTML
#exit 1
plot_script=${raster_script}
script_res=
fic_base="conc_${deb_j}${deb_h}_${fin_j}${fin_h}_${var_loc}${selection_user}_${x_loc}_${y_loc}_resauto_utm31"
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
fic_png_loc=${tmp_dir}/${fic_base}.png

fic_lvl_loc=${dir_lvl}/${var_loc}.lvl

chmod uog+w ${fic_gif} ${fic_grd} ${fic_cdf_loc} ${fic_txt_loc}

############################################################################################
# IMAGE
############################################################################################
if [ -f ${fic_gif_loc} ] ; then
mv ${fic_gif_loc} ${dir_out}/${fic_gif}
chmod uog+rw ${dir_out}/${fic_gif}
fi

############################################################################################
# KMZ Google Earth
############################################################################################
if [ -f ${fic_kmz_loc} ] ; then
mv ${fic_kmz_loc} ${dir_out}/${fic_kmz}
chmod uog+rw ${dir_out}/${fic_kmz}
fi

############################################################################################
# PNG haute-resolution sans echelle
############################################################################################
if [ -f ${fic_png_loc} ] ; then
mv ${fic_png_loc} ${dir_out}/${fic_png}
chmod uog+rw ${dir_out}/${fic_png}
fi

############################################################################################
# ZIP ARCHIVE
############################################################################################
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
rm ${fic_txt_loc} ${fic_cdf_loc} #${fic_png_loc}

############################################################################################
# READ ME
############################################################################################
/bin/cat << EOM > ${dir_out}/${fic_readme}
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++    
+ Cartoprox - version 2009v08
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Ce fichier a été généré automatiquement le ${date}, il restera disponible pour une durée de 48 heures 
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

Le fichier de données restera diponible pour une durée de 48 heures à l'adresse : 
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
chmod uog+rw ${fic_zip}
fi

#Donne les droits d'ecriture a tous (pour suppression)
cd ${dir_out}
chmod uog+rw ${fic_readme}
chmod uog+rw ${fic_log}
chmod uog+rw ${fic_gif}
chmod uog+rw ${fic_kmz}
chmod uog+rw ${fic_png}

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

if [ -f ${dir_out}/${fic_png} ] ; then
echo " - <strong>T&eacute;l&eacute;chargez la <a href='${www_dir_export}/${fic_png}'>carte haute-r&eacute;solution au format PNG (sans &eacute;chelle)</a></strong>"
fi

echo "</p>"
############################################################################################
#affichage HTML IMAGE
############################################################################################
echo "<p><img src='${www_dir_export}/${fic_gif}' width=\"${image_dx}\" height=\"${image_dy}\"></p>"

echo "</div>"

TERMINE_HTML

exit 0


