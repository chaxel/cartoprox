############################################################################################
# 1. Selection du domaine
############################################################################################
#code_maille=region_ARN
#selection=_grandlyon

############################################################################################
# 2. Fichier des mini-domaines et taille en metres
############################################################################################
dx=3000 # dx du mini domaine
cartoprox_xmin=550000.
cartoprox_ymin=4884000.
cartoprox_nx=94
cartoprox_ny=92
cartoprox_dx=3000.
cartoprox_dy=3000.

# domaine de cartographie MAILLE (region_ARN, st_exupery, etc.)
maille_xmin=553000 
maille_ymin=4887000
maille_xmax=828000
maille_ymax=5155000

#Lyon
maille_xmin=635000
maille_xmax=658000
maille_ymin=5048000
maille_ymax=5075000

#Grand-Lyon
maille_xmin=620000
maille_xmax=666000
maille_ymin=5045000
maille_ymax=5090000

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
tauxno2=0.15

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

# Fichiers rues et emissions
ficnoeudsrc=${sirane_emisrues}/${cartoprox_domaine}/noeuds_A_RN_3km_LII.txt
ficruessrc=${sirane_emisrues}/${cartoprox_domaine}/rues_A_RN_3km_LII.txt
ficemisruesrc_AN=${sirane_emisrues}/${cartoprox_domaine}/emis-rues-RA-ARN-2009v89.txt
# Fichier MIF du reseau SIRANE
ficmif=${sirane_mif}/${cartoprox_domaine}/A_RN_3km_LII.mif
