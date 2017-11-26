############################################################################################
# 1. Selection du domaine
############################################################################################
#code_maille=stetienne
selection=

############################################################################################
# 2. Taille du domaine CARTOPROX en metres
############################################################################################
dx=3000 # dx du mini domaine
cartoprox_xmin=550000.
cartoprox_ymin=4884000.
cartoprox_nx=94
cartoprox_ny=92
cartoprox_dx=3000.
cartoprox_dy=3000.

# domaine de cartographie MAILLE (region_ARN, st_exupery, etc.)
maille_xmin=595000 
maille_ymin=5021000
maille_xmax=628000
maille_ymax=5046000

############################################################################################
# 3. Parametres de SIRANE par defaut
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
# 4. Reseaux rues + emissions
############################################################################################
# Fichiers rues et emissions
ficnoeudsrc=${sirane_emisrues}/${code_maille}/noeuds_stetienne.txt
ficruessrc=${sirane_emisrues}/${code_maille}/rues_stetienne.txt
ficemisruesrc_AN=${sirane_emisrues}/${code_maille}/emis-rues-2009v114-stetienne.txt
# Fichier MIF du reseau SIRANE
ficmif=${sirane_mif}/${code_maille}/reseau_stetienne.mif
