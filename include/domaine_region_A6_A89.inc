############################################################################################
# 1. Selection du domaine
############################################################################################
#code_maille=A7 
#selection=_montelimar      #_vienne

############################################################################################
# 2. Taille du domaine CARTOPROX en metres
############################################################################################
#cartoprox_dx=3000 # dx du mini domaine LU

#Domaine sur lequel sont definis les mini-domaines
cartoprox_xmin=550000
cartoprox_ymin=4884000
cartoprox_nx=94
cartoprox_ny=92
cartoprox_dx=3000
cartoprox_dy=3000

#domaine de cartographie = Region Rhone-Alpes
#maille_xmin=553000 
#maille_ymin=4887000
#maille_xmax=828000
#maille_ymax=5155000

maille_xmin=556000
maille_ymin=4890000
maille_xmax=825000
maille_ymax=5152000

# domaine de cartographie MAILLE (region_ARN, st_exupery, etc.)
case ${selection} in
*suremis)
maille_xmin=664000
maille_ymin=5055000
maille_xmax=673000
maille_ymax=5064000
;;
*2011)
maille_xmin=631000
maille_ymin=5070000
maille_xmax=640000
maille_ymax=5082000
;;
*annecy)
maille_xmin=691000
maille_ymin=5085000
maille_xmax=760000
maille_ymax=5148000
;;
*chambery)
maille_xmin=691000
maille_ymin=5019000
maille_xmax=760000
maille_ymax=5088000
;;
*chamonix)
maille_xmin=757000
maille_ymin=5067000
maille_xmax=808000
maille_ymax=5148000
;;
*grenoble)
maille_xmin=694000
maille_ymin=4947000
maille_xmax=760000
maille_ymax=5022000
;;
*lyon)
maille_xmin=622000
maille_ymin=5019000
maille_xmax=694000
maille_ymax=5088000
;;
*montelimar)
maille_xmin=571000
maille_ymin=4902000
maille_xmax=652000
maille_ymax=4959000
;;
*roanne)
maille_xmin=556000
maille_ymin=5061000
maille_xmax=625000
maille_ymax=5121000
;;
*stetienne)
maille_xmin=589000
maille_ymin=5001000
maille_xmax=625000
maille_ymax=5064000
;;
*valence)
maille_xmin=622000
maille_ymin=4956000
maille_xmax=697000
maille_ymax=5022000
;;
*vallees_MT)
maille_xmin=757000
maille_ymin=5007000
maille_xmax=814000
maille_ymax=5070000
;;
*villefranche)
maille_xmin=622000
maille_ymin=5085000
maille_xmax=694000
maille_ymax=5148000
;;
*)
#Affichage d'un warning si la selection n'existe pas
if [ "${selection}" != "" ] ; then
echo "************************************************************************************"
echo "ATTENTION: selection inconue dans domaine${selection}.inc ! (selection=${selection})"
echo "************************************************************************************"
fi
;;
esac

############################################################################################
# 3.PARAMETRES DU MAILLEUR
############################################################################################
# Le mailleur est utilise :
# 1. pour caluler le centre d'un domaine de calcul SIRANE en Lambert 2 etendu a partir des données X, Y UTM
# 2. calculer l'emplacement des recepteurs (ATTENTION: necessite Classout=1)

mailleur=optimal   # optimal/ : espacement des points varie avec distance a l'axe ?
domaine=mini       # full/mini
#dx=3000           # largeur x hauteur d'un mini-domaine (taille des mailles CHIMERE/WRF)
brin_dx=5          # discretisation des brins pour le calcul des recepteurs
recept_dx=10       # espacement minimum des recepteurs a proximité de l'axe
#cadre_dx=3000     # cadre autour du domaine calcule par le mailleur (par defaut prendre 100 m pour s'assurer des pb de projection geo)
nrecept_max=500000 # nombre max. de recepteurs

############################################################################################
# 4. Parametres de SIRANE par defaut
############################################################################################
#..taux NO2/NOx
tauxno2=0.35   #0.24   #0.15

#..lessivage (doc SIRANE: tx lessivage Lambda=a*Pr^b)
pm_lessiva=1.0e-4
pm_lessivb=1.0
pm_lessiva=0.0 # sans depot humide

#... parametres sur la mesure (inutiles avec meteo DIAGNOSTIQUE)
altvent=10.0
rugmesure=0.1
depmesure=1
ustar_min=0.005 # valeur minimum de U* (evite les bugs par vents faibles)

#...valeurs urbaines
OccupationSol[1]="URBAINE"
Rug_quartier_lu[1]=0.5
Epaisseur_deplacement_lu[1]=1.0
Hmoyenne_lu[1]=6.0
Rugo_bati_lu[1]=0.05

#...valeurs rurales
OccupationSol[0]="RURALE"
Rug_quartier_lu[0]=0.05
Epaisseur_deplacement_lu[0]=0.0
Hmoyenne_lu[0]=1.0
Rugo_bati_lu[0]=0.05

############################################################################################
# 5. Reseaux rues + emissions
############################################################################################
emis_type=annuelle #horaire/annuelle
projet=aera
sce=tend  #ssPLe2  #ssPLe1  #ssPLe2   #ssPLe3 #ssPL  #tv
year=2015v156
ficnoeudsrc=${sirane_emisrues}/${cartoprox_domaine}/noeuds_region_A6_A89.txt
ficruessrc=${sirane_emisrues}/${cartoprox_domaine}/rues_region_A6_A89.txt
repemisruesrc_H=/mnt/mod2/appli/utilitaire-sirane/script_boucle/result_region_A6_A89 # Format fichier emis-rues-${dateYYYYMMDDHH}.txt
ficemisruesrc_AN=${sirane_emisrues}/${cartoprox_domaine}/emis-rues-${year}-${projet}-${sce}.txt #2011v182 #emis-rues-2011v161-RA-ARN.txt #emis-rues-2011v114-region-A6-A89.txt #Pour emissions annuelles

# Fichier MIF du reseau SIRANE
ficmif=${sirane_mif}/${cartoprox_domaine}/reseau_region_A6_A89.mif

############################################################################################
# 6. INTERFACE CARTOGRAPHIQUE: liste des zooms a calculer
############################################################################################
#liste_zoom_level="1 2 3 4 5 6 7"
liste_zoom_level="7"
zoom_projection=lambert93     #lambert93/utm31

############################################################################################
# 7. REPERTOIRE DE SORTIE
############################################################################################
#ChemRes1=/home/oper/data/CARTOPROX/res_V${cartoprox_version}_${cartoprox_domaine}_${periode_loc}
#ChemRes1=/home/oper/data/CARTOPROX/res_V${cartoprox_version}_${projet}_${sce}_${year}
#ChemRes1=/mnt/mod8/data/AERA/res_V${cartoprox_version}_${projet}_${sce}_${year}
#ChemRes1=/home/oper/data/CARTOPROX/res_V352_aera_tv_2011
ChemRes1=/mnt/MOD-PREVALP/data/CARTOPROX/res_V${cartoprox_version}_${projet}_${sce}_${year}
############################################################################################
# 8.SELECTION DU CALCUL REGION (optionnel)
############################################################################################
calcul_regional=prevalp

