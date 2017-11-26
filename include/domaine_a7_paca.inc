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
cartoprox_xmin=586000
cartoprox_ymin=4734000
cartoprox_nx=100
cartoprox_ny=92
cartoprox_dx=3000
cartoprox_dy=3000

# domaine de cartographie MAILLE (region_ARN, st_exupery, etc.)
case ${selection} in
*avignon)
maille_xmin=649000 
maille_ymin=4857000
maille_xmax=658000
maille_ymax=4866000
;;
*)
#domaine de cartographie = domaine A7 de PACA
maille_xmin=631000
maille_ymin=4791000
maille_xmax=700000
maille_ymax=4917000
;;
esac

#domaine de cartographie = region PACA
#maille_xmin=586000 
#maille_ymin=4734000
#maille_xmax=886000
#maille_ymax=5010000

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
tauxno2=0.21   #0.15

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

ficnoeudsrc=${sirane_emisrues}/${cartoprox_domaine}/noeuds_a7_paca.txt
ficruessrc=${sirane_emisrues}/${cartoprox_domaine}/rues_a7_paca.txt

repemisruesrc_H=/mnt/mod8/data/CARTOPROX/paca/emis/a7/emis-rues  # Format fichier emis-rues-${dateYYYYMMDDHH}.dat
#A commenter si emissions horaires:
ficemisruesrc_AN=${sirane_emisrues}/${cartoprox_domaine}/emis-rues-bidon-a7-paca.txt  #Pour emissions annuelles

# Fichier MIF du reseau SIRANE
ficmif=${sirane_mif}/${cartoprox_domaine}/reseau_a7_paca.mif

############################################################################################
# 6. INTERFACE CARTOGRAPHIQUE: liste des zooms a calculer
############################################################################################
#liste_zoom_level="1 2 3 4 5 6"
liste_zoom_level="6 7"
