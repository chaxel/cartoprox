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

maille_xmin=619000
maille_ymin=4890000
maille_xmax=706000
maille_ymax=5031000

# domaine de cartographie MAILLE (region_ARN, st_exupery, etc.)
case ${selection} in
*2011)
maille_xmin=625000
maille_ymin=4896000
maille_xmax=703000
maille_ymax=5028000
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
tauxno2=0.23

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
projet=air_bruit
sce=drome
year=2011
ficnoeudsrc=${sirane_emisrues}/${cartoprox_domaine}/noeuds_drome.txt
ficruessrc=${sirane_emisrues}/${cartoprox_domaine}/rues_drome.txt
#repemisruesrc_H=/mnt/mod2/appli/utilitaire-sirane/script_boucle/result_region_A6_A89 # Format fichier emis-rues-${dateYYYYMMDDHH}.txt
ficemisruesrc_AN=${sirane_emisrues}/${cartoprox_domaine}/emis-rues-2011v167-drome.txt #Pour emissions annuelles

# Fichier MIF du reseau SIRANE
ficmif=${sirane_mif}/${cartoprox_domaine}/reseau_A7_RA_drome.mif

############################################################################################
# 6. INTERFACE CARTOGRAPHIQUE: liste des zooms a calculer
############################################################################################
#liste_zoom_level="1 2 3 4 5 6 7"
liste_zoom_level="7"
zoom_projection=utm31  #lambert93 #lambert93/utm31

############################################################################################
# 7. REPERTOIRE DE SORTIE
############################################################################################
ChemRes1=/home/oper/data/CARTOPROX/res_V${cartoprox_version}_${projet}_${sce}_${year}

############################################################################################
# 8.SELECTION DU CALCUL REGION (optionnel)
############################################################################################
calcul_regional=prevalp

