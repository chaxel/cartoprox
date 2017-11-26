#!/bin/bash
############################################################################################
#
# ##### ##### #### ##### #### ##### #### #### #    #           
# #     #   # #  #   #   #  # #   # #  # #  #  #  #               ####
# #     ##### # #    #   #  # ####  # #  #  #   ##      #     #      #
# #     #   # ##     #   #  # #     ##   #  #   ##       #   #    ####  
# #     #   # # #    #   #  # #     # #  #  #  #  #       # #        #
# ##### #   # #  #   #   #### #     #  # #### #    #       #  #   ####
#
############################################################################################
cartoprox_version=353   #31 # version -> change le repertoire des sorties et le traitement cartoprox

############## Annee de calcul##############
case $# in
1)periode_loc=$1;;
*)periode_loc=9999
echo "cartoprox.inc:periode_loc=${periode_loc}"
;;
esac

############################################################################################
#COMMON
############################################################################################
source ${cartoprox}/include/common.inc

############################################################################################
# 1. Charge les parametres du domaines
############################################################################################
fic_domaine_inc=${cartoprox}/include/domaine_${cartoprox_domaine}.inc
if [ "${cartoprox_domaine}" == "" ] ; then
  echo -e "$ROUGE" "Specifier export cartoprox_domaine=" "$NORMAL"
  exit 1
else
  if [ -f ${fic_domaine_inc} ] ; then
    export code_maille=${cartoprox_domaine} #compatibilité
    source ${fic_domaine_inc}   
  else
    echo -e "$ROUGE" "Creer un fichier de parametres pour le domaine: ${fic_domaine_inc}" "$NORMAL"
    exit 1
  fi
fi

params_mailles=${cartoprox}/selections/mailles_${cartoprox_domaine}${selection}.txt
if [ ! -f ${params_mailles} ] ; then
  echo -e "$ROUGE" "Creer un fichier de mailles pour le domaine: ${params_mailles}" "$NORMAL"
  exit 1
fi

############################################################################################
# 2. Parametres generaux
############################################################################################
#export cartoprox=`pwd | gawk -F '/scripts' '{print $1}'`
scripts=${cartoprox}/scripts
vcadastre=2015v156  #2011v167   #2011v182   #2011v161   #2011v114  #2007v150    #2009v89
version_num=1.16
version_src=diag 
#diag=meteo diagnostique WRF (vitesse, température, HCL, flux chaleur, monin-obukov, nebulosité)
#mto=données mto sol (vitesse, temperature, precipitation, nebulosité)

############################################################################################
# 3. Parametres de SIRANE par defaut
############################################################################################

# fichier de parametres SIRANE
input_sed=${cartoprox}/include/input_sirane.sed

# Choix du polluant
#..0 = polluant passif sans reaction chimique
#..1 = melange NO/NO2, les emissions sont données en NOX equivalent NO2
#..2 = particules de meme diametre et de meme masse volumique
#polluant=2
if [ "${polluant}" == "" ] ; then
echo "Fixer le polluant polluant=[0/1/2]"
#exit 1
fi 

#..par defaut
var_pm="PM10"

#..nom du polluant PM
case ${polluant} in
2)export var_pm="PM10";;
3)export var_pm="PM25";;
esac

#..parametres PM
case ${var_pm} in
PM25)
pm_diametre=2.5e-6
pm_massvol=1000.
;;
PM10)
pm_diametre=10.0e-6
pm_massvol=1000.
;;
*)#Erreur
echo "Erreur sur definition des PM dans cartoprox.inc"
exit 1
;;
esac

############################################################################################
# 4. Noms des repertoires SORTIES + EXECUTABLES
############################################################################################

#Scripts de preparation des emissions (base sur scripts de Florence) 
# -> encore un peu tordu cette declaration de variables
ChemProg=${scripts}/util_GEOM
if [ "${cartoprox_scratch}" == "" ] ; then
  cartoprox_scratch=/mnt/mod3/scratch #/data/CARTOPROX/tmp
fi

# Fichiers de noeuds/rues INPUT (generes sur Mapinfo par ML Nguyen)
ChemData=/mnt/mod3/data/AirBruit

# Repertoire resultats -> VERSION 2 ou 3
if [ "${ChemRes1}" == "" ] ; then
  ChemRes1=${ChemData}/res_V${cartoprox_version}_${cartoprox_domaine}_${periode_loc} #${selection}
fi

# MASQUE DES AGGLOS
fichier_masque=aucun #${cartoprox}/inputs/MASQUES/infos_recepteurs${selection}_utm_masque.txt

#Mailleur optimal
#mailleur_exe=${cartoprox}/utils/mailleur_optimal/src_V1.03/mailleur.exe
#test
mailleur_version=1.03 #Cartoprox 2009
#mailleur_version=1.2 #en TEST 09/11/2011
mailleur_exe=${cartoprox}/utils/mailleur_optimal/src_V${mailleur_version}/mailleur.exe

# Convertisseur geographique
conversion_exe=${cartoprox}/utils/conversion_geographique/src/conversion.exe

# Export de grille CHIMERE
extract_val_grille_exe=${cartoprox}/utils/extract_val_grille_V5/extract_val_grille.exe

# Calcul CARTOPROX -> VERSION 2 ou 3
calc_cartoprox_nc_exe=${cartoprox}/utils/calc_cartoprox_nc_V33/calc_cartoprox_nc.exe

calc_suremis_nc_exe=${cartoprox}/utils/calc_cartoprox_nc_V33/calc_suremis_nc.exe

# STATISTIQUES
statistics_exe=${cartoprox}/utils/statistics_netcdf/src_v1.21/statistics.exe

# Repertoire resultats STATISTIQUES
StatRes=${ChemRes1}/stat_V${cartoprox_version}_${cartoprox_domaine}${selection}

# Repertoire des zooms
ZoomRes=/mnt/MOD-PREVALP/data/CARTOPROX/carto_V${cartoprox_version}   #/mnt/mod4/data/CARTOPROX/carto_V${cartoprox_version}

# NCO
if [ "${NCO}" == "" ] ; then
  export NCO=/opt/nco
fi
export NCO_BIN=${NCO}/bin
export ncks=${NCO}/bin/ncks
export ncap=${NCO}/bin/ncap
export ncra=${NCO}/bin/ncra

############################################################################################
# 5. Scripts run/plot
############################################################################################
sirane_prevalp=${cartoprox}/inputs/PREVALP_V3 #${cartoprox_version}
sirane_input=${cartoprox}/inputs/COMMON
sirane_evol=${cartoprox}/inputs/EVOL
sirane_geom=${cartoprox}/inputs/GEOM
sirane_emis=${cartoprox}/inputs/EMIS
sirane_emisrues=${cartoprox}/inputs/EMISRUES
sirane_emisponct=${cartoprox}/inputs/PONCTUELLES
sirane_mif=${cartoprox}/inputs/MIF
sirane_recept=${cartoprox}/inputs/RECEPT_${mailleur_version} # Marie-Laure
sirane_recept_user=${cartoprox}/inputs/SITE_CAMPAGNE # Marie-Laure
prevalp_raster=${cartoprox}/inputs/RASTER

############################################################################################
# 6. Scripts CARTOPROX
############################################################################################
# A executer 1 fois/an
emis_script="./2_prepare_emis_domaine.sh"
maillage_script="./3_prepare_maillage_domaine.sh"
regional_script="./4_prepare_regional.sh"

# A executer 1 fois/campagne
run_script="./5a_calcul_sirane_domaine.sh"
suremis_script="./5b_calcul_suremission.sh"
cartoprox_script="./5c_calcul_cartoprox_domaine_4.sh" #DEBUG
#cartoprox_script="./5c_calcul_cartoprox_domaine.sh"
stat_script="./5d_calcul_stat_domaine.sh"
raster_script="./6a_map_recepteur_l93.sh"
zoom_script="./6b_zoom_raster.sh"
map_script="./6c_map_raster.sh"
#agglo_script="./6b_plot_agglos_CARTOPROX.sh"
campagnes_stat_script="./7_stat_campagnes.sh"

############################################################################################
# 8. Fichiers entrees PREVALP utilisé pour la cartographie
############################################################################################
# les fichiers PREVALP pour le fond SIRANE sont dans interface_PREVALP
case ${polluant} in 
1)pol_type="gaz";;
2)pol_type="aer";;
3)pol_type="pm25";;
esac

#Fichier de configuration du calcul regional
if [ "${calcul_regional}" == "" ] || [ ! -f ${cartoprox}/include/regional_${calcul_regional}.inc ] ; then
. ${cartoprox}/include/regional.inc ${periode_loc} ${pol_type}
else
. ${cartoprox}/include/regional_${calcul_regional}.inc ${periode_loc} ${pol_type}
fi

# PARAMETRES DE L'INTERFACE REGIONALE (PREVALP)
interface_regional=${cartoprox}/interface_PREVALP
script_regional=./export_vers_SIRANE_CARTOPROX.sh
prefix_pression=pression
prefix_evolmeteo=evol-meteo
prefix_fond=fond
prefix_meteo=meteo

# PARAMETRES DE L'OCCUPATION DES SOLS
landuse_fic=${prevalp}/domains/${dom_landuse}/${dom_landuse}_corine.cdf
# Fraction de la maille à partir de laquelle elle est considérée comme urbaine
fraction_urbain=50 # %

############################################################################################
# 9.PARAMETRES DU MAILLEUR (DEFAUT)
############################################################################################
# Le mailleur est utilise :
# 1. pour caluler le centre d'un domaine de calcul SIRANE en Lambert 2 etendu a partir des données X, Y UTM
# 2. calculer l'emplacement des recepteurs (ATTENTION: necessite Classout=1)
if [ "${mailleur}" == "" ] ; then
mailleur=optimal   # optimal/ : espacement des points varie avec distance a l'axe ?
domaine=mini       # full/mini
#dx=3000           # largeur x hauteur d'un mini-domaine (taille des mailles CHIMERE/WRF)
brin_dx=5          # discretisation des brins pour le calcul des recepteurs
recept_dx=10       # espacement minimum des recepteurs a proximité de l'axe
#cadre_dx=3000     # cadre autour du domaine calcule par le mailleur (par defaut prendre 100 m pour s'assurer des pb de projection geo)
nrecept_max=500000 # nombre max. de recepteurs
fi
