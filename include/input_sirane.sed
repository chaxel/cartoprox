/******************************************
/** Donnees pour l'utilisation de SIRANE **
/******************************************
/ Donnees spatiales :
/--------------------
Fichier de rue = GEOM/rues.txt
Fichier de noeud = GEOM/noeuds.txt
Fichier de sources ponctuelles = GEOM/sources-ponct.dat
/ Donnees temporelles :
/----------------------
Nombre de pas de temps = _NTOTAL_
Pas de temps de debut = _NSTART_
Pas de temps de fin = _NSTOP_
Donnees sequentielles [0/1] = 1
Activation du modele de dispersion [0/1] = 1
Fichier meteo = INPUT/meteo.dat
Fichier diagnostique meteo = INPUT/evol-meteo.dat
Fichier d'emissions des rues = INPUT/evol-emis-rues.dat
Fichier d'emissions des noeuds = INPUT/evol-emis-noeuds.dat
Fichier d'emissions ponctuelles = INPUT/evol-emis-ponct.dat
Fichier de pollution de fond = INPUT/fond.dat
/ Caracteristiques du quartier :
/-------------------------------
Latitude [deg] = _LATITUDE_
Rugosite aerodynamique du quartier [m] = _RUGQART_
Epaisseur de deplacement du quartier [m] = _DEPLA_
Hauteur moyenne des batiments [m] = _HMOY_
Rugosite aerodynamique des batiments [m] = _RUGBATI_
Albedo = 0.1873
Emissivite = 0.88
Coefficient de Priestley-Taylor = 0.5
/ Conditions meteorologiques :
/-----------------------------
Conditions meteorologiques [0/1] = 1
Prise en compte des precipitations [0/1] = 1
Type de mesure du vent [0/1] = 1
Altitude du vent mesure [m] = _ALTMESURE_
Rugosite aerodynamique du site de mesure [m] = _RUGMESURE_
Epaisseur de deplacement du site de mesure [m] = _DEPMESURE_
Calcul de la hauteur de la CLA [0/1] = 1
Gradient thermique dans l'atmosphere libre [deg C/m] = -0.0065
Pression de reference [Pa] = _PRESREF_
Longueur de Monin-Obukhov minimale [m] = 90.0
Vitesse du vent minimale [m/s] = 0.5
Ecart-type de vitesse sigmav minimal [m/s] = 0.5
Ecart-type de vitesse sigmaw minimal [m/s] = 0.3
/ Type de polluant :
/-------------------
Type de polluant [0/1/2] = _POLLUANT_
Taux de NO2 a l'emission = _TAUXNO2_
Modele de constantes chimiques [0/1] = 1 
Rapport k1/k3 [ppb] = 20.0
Diametre moyen des particules [m] = _pm_diametre_
Masse volumique des particules [kg/m3] = _pm_massvol_
/ Lessivage :
/------------
Constante a du modele de lessivage [h/mm/s] = _pm_lessiva_
Constante b du modele de lessivage [-] = _pm_lessivb_
/ Turbulence :
/-------------
Modele de diffusion [0/1/2] = 2
Diffusivite turbulente [m2/s] = 10.0
Sigma theta [deg] = 10.0
/ Sortie des resultats :
/-----------------------
Fichier de position des recepteurs ponctuels = GEOM/Recepteurs.dat
Fichier de definition des parametres statistiques = GEOM/Stat-Chimie.dat
Niveau d'affichage [0/1/2] = output
Classement des resultats [0/1] = Classout
Sortie des champs [0/1/2] = _CALCGRILLE_
Calcul des statistiques [0/1] = _CALCSTAT_
Format surfer [0/1] = 1
Repertoire d'ecriture des resultats = _REPRES_
Repertoire d'ecriture des fichiers de champs = _REPSURF_
Nombre de points selon X du maillage de sortie = Xout
Nombre de points selon Y du maillage de sortie = Yout
Abscisse minimum du maillage de sortie [m] = Xmin
Abscisse maximum du maillage de sortie [m] = Xmax
Ordonnee minimum du maillage de sortie [m] = Ymin
Ordonnee maximum du maillage de sortie [m] = Ymax
Ratio d'interpolation de la grille de sortie = Rinterp
/ Parametres de calcul :
/-----------------------
Nombre de cellules selon X du maillage de calcul = Xcnpt
Nombre de cellules selon Y du maillage de calcul = Ycnpt
Abscisse minimum du maillage de calcul [m] = Xcmin
Abscisse maximum du maillage de calcul [m] = Xcmax
Ordonnee minimum du maillage de calcul [m] = Ycmin
Ordonnee maximum du maillage de calcul [m] = Ycmax
Nombre de cellules d'influence du maillage de calcul = Ncinf
