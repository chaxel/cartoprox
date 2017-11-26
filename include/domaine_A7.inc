############################################################################################
# 1. Selection du domaine
############################################################################################
#code_maille=A7
#selection=_montelimar

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
ustar_fac=0.75 #Parametres multiplicatif de ustar

#domaine de cartographie = A7
maille_xmin=631000
maille_ymin=4911000 #gestion du recouvrement avec paca    #4908000 domaine de calcul
maille_xmax=655000
maille_ymax=5052000

# domaine de cartographie MAILLE (region_ARN, st_exupery, etc.)
case ${selection} in
*vienne)
maille_xmin=640000 
maille_ymin=5040000
maille_xmax=649000
maille_ymax=5049000
ustar_fac=0.75 #Parametres multiplicatif de ustar
;;
*montelimar)
maille_xmin=637000
maille_ymin=4935000
maille_xmax=646000
maille_ymax=4944000
ustar_fac=0.75  #Parametres multiplicatif de ustar
;;
*sud_lyonnais)
maille_xmin=640000
maille_ymin=5040000
maille_xmax=643000
maille_ymax=5043000
;;
*nord_isere)
maille_xmin=637000
maille_ymin=5019000
maille_xmax=646000
maille_ymax=5028000
ustar_fac=0.75  #Parametres multiplicatif de ustar
;;
*valence_trafic)
maille_xmin=646000
maille_ymin=4974000
maille_xmax=655000
maille_ymax=4986000
ustar_fac=0.75  #Parametres multiplicatif de ustar
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

#Calcul de l'année
deb_j=`echo ${periode} | gawk -F "_" '{print $1}'`
deb_a=`date -d "${deb_j}" +%Y`

############################################################################################
# 5. Reseaux rues + emissions
############################################################################################
emis_type=horaire #horaire/annuelle

ficnoeudsrc=${sirane_emisrues}/${cartoprox_domaine}/noeuds_A7_RA.txt
ficruessrc=${sirane_emisrues}/${cartoprox_domaine}/rues_A7_RA.txt
repemisruesrc_H=/mnt/mod2/appli/utilitaire-sirane/script_boucle/result_A7_${deb_a} # Format fichier emis-rues-${dateYYYYMMDDHH}.txt
repemisruesrc_H=/mnt/mod2/appli/utilitaire-sirane/script_boucle/result_A7_emi_test_sansA7    #2011v169   #2011v168  # emi_test_sansA7 # 
#ficemisruesrc_AN=${sirane_emisrues}/${cartoprox_domaine}/emis-rues-RA-ARN-2009v89-vienne.txt #Pour emissions annuelles

# Fichier MIF du reseau SIRANE
#ficmif=${sirane_mif}/${cartoprox_domaine}/reseau_A7_vienne.mif
ficmif=${sirane_mif}/${cartoprox_domaine}/reseau_A7_RA.mif

############################################################################################
# 6. INTERFACE CARTOGRAPHIQUE: liste des zooms a calculer
############################################################################################
#liste_zoom_level="1 2 3 4 5 6"
liste_zoom_level="7"
zoom_projection=lambert93 #lambert93/utm31

############################################################################################
# 7. REPERTOIRE DE SORTIE
############################################################################################
ChemRes1=/home/oper/data/CARTOPROX/res_V${cartoprox_version}_${cartoprox_domaine}_${periode_loc}
#ChemRes1=/home/oper/data/CARTOPROX/test_coupure_${cartoprox_domaine}_sansE3    #sansPL     #sansA7
#ChemRes1=/home/oper/data/CARTOPROX/test_coupure_${cartoprox_domaine}_valence_ssE3    #sansE3 #_2011    #sansPL     #sansA7

