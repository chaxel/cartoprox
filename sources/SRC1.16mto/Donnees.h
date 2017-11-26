/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Donnees.h --> Mots-cles du fichier de       */
/*               parametres                    */
/*                                             */
/***********************************************/
#ifndef _MOTS_CLE_
#define _MOTS_CLE_

#define FICH_RUE "Fichier de rue "
#define FICH_NOEUD "Fichier de noeud "
#define FICH_SOURCES_PONCT "Fichier de sources ponctuelles "
#define FICH_EVOL_EMIS_RUE "Fichier d'emissions des rues "
#define FICH_EVOL_EMIS_NOEUD "Fichier d'emissions des noeuds "
#define FICH_EVOL_EMIS_PONCT "Fichier d'emissions ponctuelles "
#define FICH_METEO "Fichier meteo "
#define FICH_POLLU "Fichier de pollution de fond "
#define FICH_RECEPT "Fichier de position des recepteurs ponctuels "
#define FICH_STAT "Fichier de definition des parametres statistiques "
#define FICH_DIR_RESUL "Repertoire d'ecriture des resultats "
#define FICH_DIR_SURFER "Repertoire d'ecriture des fichiers de champs "
#define N_TEMPS "Nombre de pas de temps "
#define ITER_DEB "Pas de temps de debut "
#define ITER_FIN "Pas de temps de fin "
#define SEQUENT "Donnees sequentielles [0/1] "
#define CALC_DISP "Activation du modele de dispersion [0/1] "

#define LAT "Latitude [deg] "
#define Z0D "Rugosite aerodynamique du quartier [m] "
#define ZD "Epaisseur de deplacement du quartier [m] "
#define HMOY "Hauteur moyenne des batiments [m] "
#define Z0D_BAT "Rugosite aerodynamique des batiments [m] "
#define ALB "Albedo "
#define EMISSIVITE "Emissivite "
#define PRIESTLEY_TAYLOR "Coefficient de Priestley-Taylor "

#define TYPE_METEO "Conditions meteorologiques [0/1] "
#define TYPE_MES_VENT "Type de mesure du vent [0/1] "
#define PRECIPIT "Prise en compte des precipitations [0/1] "
#define ZEXT "Altitude du vent mesure [m] "
#define Z0DEXT "Rugosite aerodynamique du site de mesure [m] "
#define DEXT "Epaisseur de deplacement du site de mesure [m] "
#define TYPE_HAUT "Calcul de la hauteur de la CLA [0/1] "
#define GRADTEXT "Gradient thermique dans l'atmosphere libre [deg C/m] "
#define P0 "Pression de reference [Pa] "
#define LMO_MIN "Longueur de Monin-Obukhov minimale [m] "
#define U_MIN "Vitesse du vent minimale [m/s] "
#define SIGMAV_MIN "Ecart-type de vitesse sigmav minimal [m/s] "
#define SIGMAW_MIN "Ecart-type de vitesse sigmaw minimal [m/s] "

#define TYPE_POL "Type de polluant [0/1/2] "
#define TAUX_NO2 "Taux de NO2 a l'emission "
#define MOD_CSTE_CHIM "Modele de constantes chimiques [0/1] "
#define K1K3 "Rapport k1/k3 [ppb] "
#define DIAM_PART "Diametre moyen des particules [m] "
#define RHO_PART "Masse volumique des particules [kg/m3] "

#define LESSIV_A "Constante a du modele de lessivage [h/mm/s] "
#define LESSIV_B "Constante b du modele de lessivage [-] "

#define TYPE_DISP "Modele de diffusion [0/1/2] "
#define DI "Diffusivite turbulente [m2/s] "
#define SIGMATHETA "Sigma theta [deg] "
#define AFFICH "Niveau d'affichage [0/1/2] "
#define CALCSTAT "Calcul des statistiques [0/1] "
#define ORDRE "Classement des resultats [0/1] "
#define CHAMP "Sortie des champs [0/1/2] "
#define NX_OUT "Nombre de points selon X du maillage de sortie "
#define NY_OUT "Nombre de points selon Y du maillage de sortie "
#define XMIN_OUT "Abscisse minimum du maillage de sortie [m] "
#define XMAX_OUT "Abscisse maximum du maillage de sortie [m] "
#define YMIN_OUT "Ordonnee minimum du maillage de sortie [m] "
#define YMAX_OUT "Ordonnee maximum du maillage de sortie [m] "
#define GRD_RATIO "Ratio d'interpolation de la grille de sortie "
#define NX_CALC "Nombre de cellules selon X du maillage de calcul "
#define NY_CALC "Nombre de cellules selon Y du maillage de calcul "
#define XMIN_CALC "Abscisse minimum du maillage de calcul [m] "
#define XMAX_CALC "Abscisse maximum du maillage de calcul [m] "
#define YMIN_CALC "Ordonnee minimum du maillage de calcul [m] "
#define YMAX_CALC "Ordonnee maximum du maillage de calcul [m] "
#define N_INFL "Nombre de cellules d'influence du maillage de calcul "
#endif
