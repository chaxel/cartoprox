############################################################################################
# 1. Selection du domaine
############################################################################################
#code_maille=A7 
selection=_vienne

############################################################################################
# 2. Fichier des mini-domaines et taille en metres
############################################################################################
code_maille=A7    #stetienne        #region_ARN
selection=_vienne    #selection exemple: _lyon
params_mailles=${scripts}/selections/mailles_${code_maille}${selection}.txt

dx=3000 # dx du mini domaine
cartoprox_xmin=550000.
cartoprox_ymin=4884000.
cartoprox_nx=94
cartoprox_ny=92
cartoprox_dx=3000.
cartoprox_dy=3000.

# domaine de cartographie MAILLE (region_ARN, st_exupery, etc.)
maille_xmin=640000 
maille_ymin=5040000
maille_xmax=649000
maille_ymax=5049000

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
ficnoeudsrc=${sirane_emisrues}/${code_maille}/noeuds-bourgoin.dat
ficruessrc=${sirane_emisrues}/${code_maille}/rues-bourgoin.dat
ficemisruesrc_AN=${sirane_emisrues}/${code_maille}/emis-rues-bourgoin-2009v89.txt
# Fichier MIF du reseau SIRANE
ficmif=${sirane_mif}/${code_maille}/sirane_BJ_LII.MIF
