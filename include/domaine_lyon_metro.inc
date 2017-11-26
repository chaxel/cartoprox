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

# domaine de cartographie MAILLE (region_ARN, st_exupery, etc.)
case ${selection} in
_test1km9)
maille_xmin=640000
maille_ymin=5064000
maille_xmax=649000
maille_ymax=5073000
;;
_test*)
maille_xmin=643000
maille_ymin=5067000
maille_xmax=646000
maille_ymax=5070000
;;
*)
#domaine de cartographie (region_ARN, st_exupery, A7, etc.)
maille_xmin=628000
maille_ymin=5031000
maille_xmax=688000
maille_ymax=5091000
;;
esac

#domaine de cartographie = Region Rhone-Alpes
#maille_xmin=553000 
#maille_ymin=4887000
#maille_xmax=828000
#maille_ymax=5155000

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
tauxno2=0.22   #0.15

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

ficnoeudsrc=${sirane_emisrues}/${cartoprox_domaine}/noeuds_lyon_metro.txt
ficruessrc=${sirane_emisrues}/${cartoprox_domaine}/rues_lyon_metro.txt
ficponctsrc=${sirane_emisrues}/${cartoprox_domaine}/ponct_lyon_metro.txt #Genere automatiquement

repemisruesrc_H=/mnt/mod2/appli/utilitaire-sirane/script_boucle/result_lyon_metro # Format fichier emis-rues-${dateYYYYMMDDHH}.txt
ficemisruesrc_AN=${sirane_emisrues}/${cartoprox_domaine}/emis-rues-2007v150-lyon-metro.txt #Pour emissions annuelles

ficemisponctsrc_AN=${sirane_emisponct}/${cartoprox_domaine}/emis_ponct_lyon_metro.txt #Genere automatiquement

# Fichier MIF du reseau SIRANE
ficmif=${sirane_mif}/${cartoprox_domaine}/reseau_lyon_metro.mif

############################################################################################
# 6. INTERFACE CARTOGRAPHIQUE: liste des zooms a calculer
############################################################################################
#liste_zoom_level="1 2 3 4 5 6"
liste_zoom_level="3 4 5 6"
#zoom_projection=lambert93
