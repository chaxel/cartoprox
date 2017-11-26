#!/bin/bash

periode_loc=$1
pol_type=$2

#version du calcul annuel météo
version_meteo=meteo

case ${periode_loc} in

#CAMPAGNES A6_89 et A7
#ATTENTION les dates de fin sont J-1 par rapport au dates PREVALP
20110222_20110307|20110308_20110322|20110623_20110707|20110708_20110721|20110310_20110325|20110330_20110413|20110420_20110504)
version_simulation=campagnes
reg_deb_j=`echo ${periode_loc} | gawk -F "_" '{print $1}'`
reg_fin_j=`echo ${periode_loc} | gawk -F "_" '{print $2}'`
reg_fin_j=`date -d "${reg_fin_j} 1 day" +%Y%m%d`
reg_deb_h=00
reg_fin_h=00
mto_deb_j=`date -d "${reg_deb_j} 1 day ago" +%Y%m%d`
mto_fin_j=${reg_fin_j}
mto_deb_h=00
mto_fin_h=00
reg_deb_a=`date -d "${reg_deb_j}" +%Y`
mto_deb_a=`date -d "${mto_deb_j}" +%Y`
;;

#Calcul 2007
2007*)#2 calculs differents
case ${pol_type} in
gaz)
version_simulation=emi2007v31 #carto 2007 NO, NO2, O3 krigée pour les ZAPAs 2007v05 #version utilisée pour les données réglementaires
prefix_fic_chimie=ana_h;;
aer)
version_simulation=emi2007v31 #version emi2007v31 PM10 utilisée dans le cadre des PPA/SRACE en 2011
prefix_fic_chimie=ana_avg24;;
pm25)
version_simulation=emi2007v31
prefix_fic_chimie=out;;
esac
reg_deb_j=20070101
reg_fin_j=20071231
;;

#Calcul 2009 
2009*)
version_simulation=2009v08
version_meteo=meteo
case ${pol_type} in
gaz)prefix_fic_chimie=ana_h;;
aer)prefix_fic_chimie=ana_avg24;;
pm25)prefix_fic_chimie=out;;
esac
reg_deb_j=20090101
reg_fin_j=20091231
;;

#Calcul 2010 
2010*)#2 calculs differents
version_meteo=meteo
case ${pol_type} in
gaz)
version_simulation=2010v02_gaz
prefix_fic_chimie=ana_h;;
aer)
version_simulation=emi2007v31
prefix_fic_chimie=ana_avg24;;
pm25)
version_simulation=emi2007v31
prefix_fic_chimie=out;;
esac
reg_deb_j=20100101
reg_fin_j=20101231
;;

#2011*)#PREVISION
#version_simulation=periode
#reg_deb_h=00
#reg_fin_h=00
#reg_deb_j=${deb_j}
#reg_fin_j=`date -d "${fin_j} 1 day" +%Y%m%d`
#;;

#Calcul 2011 

#2011*)#2 calcul A7
#version_simulation=2011obsA7
#version_meteo=wrf_3km_fddao_rsrt
#case ${pol_type} in
#gaz)prefix_fic_chimie=analyse_h;;
#aer)prefix_fic_chimie=analyse_avg24h;;
#pm25)prefix_fic_chimie=analyse_avg24h;;
#esac
#reg_deb_j=20110101
#reg_fin_j=20111231
#;;

2011*) #calcul region
version_simulation=2011v02beta  #2011v01
version_meteo=wrf_3km_fddao_rsrt
case ${pol_type} in
gaz)prefix_fic_chimie=ana_h;;
aer)prefix_fic_chimie=ana_avg24;;
pm25)prefix_fic_chimie=out;;
esac
reg_deb_j=20110101
reg_fin_j=20111231
;;

2015*)#
version_simulation=2015obsA7
version_meteo=2015obsA7    #wrf_3km_fddao_rsrt
case ${pol_type} in
gaz)prefix_fic_chimie=analyse_h;;
aer)prefix_fic_chimie=analyse_avg24h;;
pm25)prefix_fic_chimie=analyse_avg24h;;
esac
reg_deb_j=20150101
reg_fin_j=20151231
;;

2020*)#
version_simulation=2020obsA7
version_meteo=2020obsA7    #wrf_3km_fddao_rsrt
case ${pol_type} in
gaz)prefix_fic_chimie=analyse_h;;
aer)prefix_fic_chimie=analyse_avg24h;;
pm25)prefix_fic_chimie=analyse_avg24h;;
esac
reg_deb_j=20200101
reg_fin_j=20201231
;;


#Autres
9999)
version_simulation=aucune
;;

esac

case ${periode_loc} in
2007|2009|2010)
annee=${periode_loc}
anneeplus1=`expr ${annee} \+ 1`
deb_j=${annee}0101
fin_j=${anneeplus1}0101
;;
9999)#aucune date
echo "$0: aucune periode (AAAAMMJJ_AAAAMMJJ) specifie dans cartoprox.inc"
;;
*)#Format YYYYMMDD_YYYYMMDD
deb_j=`echo ${periode_loc} | gawk -F "_" '{print $1}'`
fin_j=`echo ${periode_loc} | gawk -F "_" '{print $2}'`
annee=`date -d "${deb_j}" +%Y`
;;
esac

#Type de repertoire
case ${version_simulation} in

periode) #Version pour l'opertationnel
ChemArchMet=/mnt/mod3/data/PREVALP_DATA/out/${reg_deb_j}${reg_deb_h}_${reg_fin_j}${reg_deb_h}.ncep/WRF.op_wrf
ChemArchChim=/mnt/mod3/data/PREVALP_DATA/out/${reg_deb_j}${reg_deb_h}_${reg_fin_j}${reg_deb_h}.ncep/CHIMERE.op_wrf.aerosols
dom_mto_wrf="wrf_med_6" 
dom_mto_chimere="MED06KM" 
dom_chimere="MED06KM" 
fichier_mto_wrf=${ChemArchMet}/wrfout_${dom_mto_wrf}.nc
fichier_mto_diag=${ChemArchChim}/meteo_niveau1.${reg_deb_j}_${reg_fin_j}_${dom_mto_chimere}.nc
fichier_chimie=${ChemArchChim}/out.${reg_deb_j}_${reg_fin_j}_${dom_chimere}.nc

#Option d'export PREVALP -> SIRANE
option_chimie=1
# option_chimie=1 : option de la version 1 : utilise l'interpolation au centre de la maille (recommandé pour des grosses mailles dom_chimere > 1 km)
# option_chimie=2 : utilise la moyenne sur n mailles autour du point (recommandé pour des mailles dom_chimere ~ 1 km)
option_chimie_n=1
;;

campagnes) #Version les campagnes 
config=wrf
meteo=wrf_3km_fddao
chimie=emi2007v31
data_mto_wrf=/mnt/mod5/data/PREVALP_DATA/${mto_deb_a}_meteo/${mto_deb_j}${mto_deb_h}_${mto_fin_j}${mto_deb_h}.${config}/WRF.${meteo}
data_mto_diag=/mnt/mod5/data/PREVALP_DATA/${reg_deb_a}_chimere/chimere.${meteo}.${chimie}
data_chimere=/mnt/mod5/data/PREVALP_DATA/${reg_deb_a}_assimilation/assimilation.${meteo}.${chimie}
dom_mto_wrf="WRFREG03KM" 
dom_mto_diag="REG04KM" 
dom_chimere="REG01KM" 
case ${pol_type} in
gaz)prefix_fic_chimie=ana_h;;
aer)prefix_fic_chimie=ana_avg24;;
pm25)
#pas d'assimilation de données pour PM2.5
data_chimere=/mnt/mod7/data/PREVALP_DATA/${reg_deb_a}_giraf/giraf.${meteo}.${chimie}
prefix_fic_chimie=out
;;
esac
fichier_mto_wrf=${data_mto_wrf}/wrfout_${dom_mto_wrf}.nc
fichier_mto_diag=${data_mto_diag}/meteo_niveau1.${reg_deb_j}_${reg_fin_j}_${dom_mto_diag}.nc
fichier_chimie=${data_chimere}/${prefix_fic_chimie}.${reg_deb_j}_${reg_fin_j}_${dom_chimere}.nc

#Option d'export PREVALP -> SIRANE
option_chimie=2
#option_chimie=1: option de la version 1 : utilise l'interpolation au centre de la maille (recommandé pour des grosses mailles dom_chimere > 1 km)
#option_chimie=2: utilise la moyenne sur n mailles autour du point (recommandé pour des mailles dom_chimere ~ 1 km)
option_chimie_n=1
;;

2011obsA7)
version_meteo=2011obsA7
ChemArchMet=/mnt/mod4/data/PREVALP_DATA/cartos/carto_${annee}_annuel/${version_meteo}
ChemArchChim=/mnt/mod4/data/PREVALP_DATA/cartos/carto_${annee}_annuel/${version_simulation}
dom_mto_wrf="VR6" 
dom_mto_chimere="VR6" 
dom_chimere="VR3"
fichier_mto_wrf=${ChemArchMet}/wrfout.${reg_deb_j}_${reg_fin_j}_${dom_mto_wrf}.nc
fichier_mto_diag=${ChemArchMet}/meteo.${reg_deb_j}_${reg_fin_j}_${dom_mto_chimere}.nc
fichier_chimie=${ChemArchChim}/${prefix_fic_chimie}.${reg_deb_j}_${reg_fin_j}_${dom_chimere}.nc
fichier_stat=${ChemArchChim}/stat_${prefix_fic_chimie}.${reg_deb_j}_${reg_fin_j}_${dom_chimere}.nc
;;

2015obsA7)
version_meteo=2015obsA7
ChemArchMet=/mnt/mod4/data/PREVALP_DATA/cartos/carto_${annee}_annuel/${version_meteo}
ChemArchChim=/mnt/mod4/data/PREVALP_DATA/cartos/carto_${annee}_annuel/${version_simulation}
dom_mto_wrf="VR6"
dom_mto_chimere="VR6"
dom_chimere="VR3"
fichier_mto_wrf=${ChemArchMet}/wrfout.${reg_deb_j}_${reg_fin_j}_${dom_mto_wrf}.nc
fichier_mto_diag=${ChemArchMet}/meteo.${reg_deb_j}_${reg_fin_j}_${dom_mto_chimere}.nc
fichier_chimie=${ChemArchChim}/${prefix_fic_chimie}.${reg_deb_j}_${reg_fin_j}_${dom_chimere}.nc
fichier_stat=${ChemArchChim}/stat_${prefix_fic_chimie}.${reg_deb_j}_${reg_fin_j}_${dom_chimere}.nc


#Option d'export PREVALP -> SIRANE
option_chimie=1
# option_chimie=1 : option de la version 1 : utilise l'interpolation au centre de la maille (recommandé pour des grosses mailles dom_chimere > 1 km)
# option_chimie=2 : utilise la moyenne sur n mailles autour du point (recommandé pour des mailles dom_chimere ~ 1 km)
option_chimie_n=1

;;

2020obsA7)
version_meteo=2020obsA7
ChemArchMet=/mnt/mod4/data/PREVALP_DATA/cartos/carto_${annee}_annuel/${version_meteo}
ChemArchChim=/mnt/mod4/data/PREVALP_DATA/cartos/carto_${annee}_annuel/${version_simulation}
dom_mto_wrf="VR6"
dom_mto_chimere="VR6"
dom_chimere="VR3"
fichier_mto_wrf=${ChemArchMet}/wrfout.${reg_deb_j}_${reg_fin_j}_${dom_mto_wrf}.nc
fichier_mto_diag=${ChemArchMet}/meteo.${reg_deb_j}_${reg_fin_j}_${dom_mto_chimere}.nc
fichier_chimie=${ChemArchChim}/${prefix_fic_chimie}.${reg_deb_j}_${reg_fin_j}_${dom_chimere}.nc
fichier_stat=${ChemArchChim}/stat_${prefix_fic_chimie}.${reg_deb_j}_${reg_fin_j}_${dom_chimere}.nc


#Option d'export PREVALP -> SIRANE
option_chimie=1
# option_chimie=1 : option de la version 1 : utilise l'interpolation au centre de la maille (recommandé pour des grosses mailles dom_chimere > 1 km)
# option_chimie=2 : utilise la moyenne sur n mailles autour du point (recommandé pour des mailles dom_chimere ~ 1 km)
option_chimie_n=1

;;


*)
#Calcul ANNUEL PREVALP
#Annees disponibles 2007, 2009, 2010
ChemArchMet=/mnt/mod4/data/PREVALP_DATA/cartos/carto_${annee}_annuel/${version_meteo}
ChemArchChim=/mnt/mod4/data/PREVALP_DATA/cartos/carto_${annee}_annuel/${version_simulation}
dom_mto_wrf="WRFREG03KM" 
dom_mto_chimere="REG03KM" 
dom_chimere="REG01KM"
fichier_mto_wrf=${ChemArchMet}/wrfout.${reg_deb_j}_${reg_fin_j}_${dom_mto_wrf}.nc
fichier_mto_diag=${ChemArchMet}/meteo.${reg_deb_j}_${reg_fin_j}_${dom_mto_chimere}.nc
fichier_chimie=${ChemArchChim}/${prefix_fic_chimie}.${reg_deb_j}_${reg_fin_j}_${dom_chimere}.nc
fichier_stat=${ChemArchChim}/stat_${prefix_fic_chimie}.${reg_deb_j}_${reg_fin_j}_${dom_chimere}.nc

#Option d'export PREVALP -> SIRANE
option_chimie=2
# option_chimie=1 : option de la version 1 : utilise l'interpolation au centre de la maille (recommandé pour des grosses mailles dom_chimere > 1 km)
# option_chimie=2 : utilise la moyenne sur n mailles autour du point (recommandé pour des mailles dom_chimere ~ 1 km)
option_chimie_n=1
;;
esac

#Parametres de la grille PREVALP pour l'extraction du FOND
case ${dom_chimere} in
VR3)
fond_xmin=586000
fond_ymin=4734000
fond_dx=3000
dom_landuse="REG01KM" 
;;
REG01KM)
fond_xmin=553000
fond_ymin=4887000
fond_dx=1000
dom_landuse="REG01KM" 
;;
MED06KM)
fond_xmin=380000   
fond_ymin=4680000
fond_dx=6000
dom_landuse="REG01KM"
;;
REG06KM)
fond_xmin=550000
fond_ymin=4884000
fond_dx=6000
dom_landuse="REG01KM"
;;
PACA3-A7)
#Parametres de la grille PREVALP pour l'extraction du FOND
fond_xmin=586000   
fond_ymin=4734000
fond_dx=3000
dom_landuse="PACA1-A7"
;;
esac
fond_dy=${fond_dx}

#Parametres du LANDUSE
case ${dom_landuse} in
REG01KM)
landuse_xmin=553000
landuse_ymin=4887000
landuse_dx=1000
;;
PACA1-A7)
landuse_xmin=586000
landuse_ymin=4734000
landuse_dx=1000
;;
esac

fond_fic=${fichier_chimie}
#fond_fic_moy=${ChemArchChim}/moy_${prefix_fic_chimie}.${deb_j}_${fin_j}_${dom_chimere}.nc
fond_fic_stat=${fichier_stat}

#Fonctionnalité pour corriger la vitesse du vent avec la vitesse du vent w10m de meteo_fic
met_fin_xmin=553000.
met_fin_ymin=4887000.
met_fin_dx=1000.

#met_coa_xmin=550000.
#met_coa_ymin=4884000.
#met_coa_dx=3000.

#Meteo fine (pour 2009 seulement)
meteo_fin_fic=aucun
case ${periode_loc} in
2009)
prevalp_meteo_dir=/mnt/mod4/data/PREVALP_DATA/cartos/carto_2009_annuel/meteo
meteo_fin_fic=${prevalp_meteo_dir}/fac_w10m.20090101_20100101_REG01KM.nc
;;
esac
