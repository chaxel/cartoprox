/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Def.h --> Definition des includes,          */
/*           des constantes et des structures  */
/*                                             */
/***********************************************/
#ifndef _DEF_
#define _DEF_
#include <errno.h>
#include <stdio.h>
#include <math.h>
#include <malloc.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>

/* Definition de la precision */
#define DBL double

/* Valeur de Pi */
#define pi 3.1415926535898
/* Constante d'Euler */
#define gamma 0.57721566
/* Valeur de sqrt(2*Pi) */
#define Sqrt2Pi 2.506628275
/* Valeur de sqrt(Pi/2) */
#define SqrtPi2 1.253314137
/* Chaleur specifique de l'air a pression constante */
#define Cp 1005.0
/* Viscosite cinematique de l'air */
#define nuair 1.43E-5
/* Acceleration de la pesanteur (m/s2) */
#define g 9.81
/* Gradient thermique adiabatique (degC/m) */
#define GammaAdiab 0.009761
/* Constante de Karman */
#define kappa 0.4
/* Vitesse de rotation de la Terre (rad/s) */
#define omega 7.2722E-05
/* Masse molaire de l'air sec (kg/mol) */
#define M_air_sec 0.028966
/* Constante des gaz parfaits pour de l'air sec (J/kg/K) */
#define R_air_sec 287.043
/* Constante de passage de Celsius a Kelvin */
#define T0abs 273.15
/* Masse molaire de NO2 (g/mol) */
#define M_NO2 46.0
/* Masse molaire de NO (g/mol) */
#define M_NO 30.0
/* Masse molaire de O3 (g/mol) */
#define M_O3 48.0
/* Dimension des tables pour les     */
/* coefficients de dispersion        */
/* A conserver < 10000 par coherence */
/* avec les formules de Briggs !!    */
#define N_Point_Disp 12000
/* Nombre maximum de rues par intersection */
#define Nmax_Rue_Inters 20
/* Flags pour les noms de fichiers de rue et de recepteurs */
#define FLAG_RUE "rue"
#define FLAG_RECEPT "recept"
/* Nom des fichiers temporaires */
#define TMP_FICH_RUE "res-rue.tmp"
#define TMP_FICH_RECEPT "res-recept.tmp"

#endif
/*----------------------------*/
/* Declaration des structures */
/*----------------------------*/

/*----------------------*/
/* Noeud d'intersection */
/*----------------------*/
typedef struct Noeud{
  /* Identifiant */
  /*-------------*/
  char Id[10];
  /* Categorie */
  /*-----------*/
  int Categ;
  /* Geometrie */
  /*-----------*/
  DBL x,y;
  DBL xv,yv,Rayon,H;
  int type;
  /* Rues connectees */
  /*-----------------*/
  int N_Rue,*Num;
  /* Concentration */
  /*---------------*/
  DBL delta_x_z;
  DBL Cint,Cext;
  /* Source */
  /*--------*/
  DBL Qsrce;
  /* Flux convectif vertical */
  /*-------------------------*/
  DBL Pconv_v,Qconv_v;
  DBL Pconv_v_entr,Pconv_v_sort;
  DBL Qconv_v_entr,Qconv_v_sort;
  /* Flux convectif horizontal */
  /*---------------------------*/
  DBL Pconv_entr,Pconv_sort;
  DBL **Flux;
  /* Grille de travail */
  /*-------------------*/
  int Cell_i;
} Noeud;

/*----------------*/
/* Troncon de rue */
/*----------------*/
typedef struct Rue{
  /* Identifiant */
  /*-------------*/
  char Id[10];
  /* Categorie */
  /*-----------*/
  int Categ;
  int Autoroute;
  /* Geometrie */
  /*-----------*/
  DBL x,y,ix,iy;
  DBL xv,yv,ixv,iyv;
  char IdDeb[10],IdFin[10];
  int Deb,Fin;
  int type;
  DBL L,H,W;
  /* Vitesse */
  /*---------*/
  DBL us_para,us_perp;
  DBL Umoy,Uh;
  DBL Kmoy,Kh;
  /* Concentration */
  /*---------------*/
  DBL Cint,Cext;
  /* Source */
  /*--------*/
  DBL Qsrce;
  /* Dispersion initiale au-dessus des routes */
  /*------------------------------------------*/
  DBL L0,delta_x;
  /* Flux convectif horizontal */
  /*---------------------------*/
  DBL Pconv_h,Pconv_h_evac,Qconv_h;
  int ndent,ndsort;
  /* Flux diffusif vertical */
  /*------------------------*/
  DBL Pdiff_v,Qdiff_v;
  /* Lessivage */
  /*-----------*/
  DBL P_lessiv;
  /* Fichier de resultats */
  /*----------------------*/
  char name_res_rue[100];
  /* Grille de travail */
  /*-------------------*/
  int Cell_i;
} Rue;

/*-------------------*/
/* Source ponctuelle */
/*-------------------*/
typedef struct Source_Ponct{
  /* Identifiant */
  /*-------------*/
  char Id[10];
  /* Geometrie */
  /*-----------*/
  DBL x,y,H,H_sol;
  DBL xv,yv,Rs;
  /* Source */
  /*--------*/
  DBL Qsrce,Ws,Ts;
} Source_Ponct;

/*----------------------*/
/* Recepteurs ponctuels */
/*----------------------*/
typedef struct Recept{
  /* Identifiant */
  /*-------------*/
  char Id[10];
  /* Geometrie */
  /*-----------*/
  DBL x,y;
  /* Fichier de resultats */
  /*----------------------*/
  char name_res_recept[100];
} Recept;

/*--------------------------*/
/* Informations temporelles */
/*--------------------------*/
typedef struct Date{
  DBL a,m,j,jm,js,h,min;
  char NomJourSem[8];
} Date; 

/*------------------------------*/
/* Informations meteorologiques */
/*------------------------------*/
typedef struct Meteo{
  /*------------------*/
  DBL rho,To,thetao,Nu,Vo;
  DBL Ug,Uext,Uh,Angle,Cld,Precip,Precip_prec;
  DBL ix,iy;
  /*------------------*/
  DBL elev_sol;
  DBL Fo,Lmo,h,us,thetas,h_prec;
  DBL ws,sigmatheta,sigma_wH,Taux_Lessiv;
  char Pasq;
  /*------------------*/
}Meteo;

/*-----------------------------------*/
/* Informations sur les pas de temps */
/*-----------------------------------*/
typedef struct DonIter{
  /*------------------*/
  char name_emisrue[100];
  char name_emisnoeud[100];
  char name_emisponct[100];
  /*------------------*/
  int valid;
  Date date;
  /*------------------*/
  Meteo met;
  /*------------------*/
  DBL Mod_rue_centre,Mod_rue_autoroute,Mod_noeud,Mod_ponct;
  /*------------------*/
  DBL Cext,Cb_NO,Cb_NO2,Cb_O3;
  DBL k1,k3;
  /*------------------*/
} DonIter;

typedef struct ParamNonGauss{
  /*------------------*/
  DBL ap,am,sigmazm,sigmazp,wchapt;
  /*------------------*/
}ParamNonGauss;

/*---------------------------*/
/* Description d'un maillage */
/*---------------------------*/
typedef struct Grid{
  int Nx,Ny;
  DBL xmin,xmax,dx,ymin,ymax,dy,zmin,zmax;
} Grid;

/*---------------------------------------*/
/* Description d'une cellule de maillage */
/*---------------------------------------*/
typedef struct Cell{
  DBL x,y,Qrue_nd,Qsurf;
  int N_Rue,N_Noeud,Nrue_Infl,Nnoeud_Infl;
  int *NumRue,*NumNd,*Num_Infl;
} Cell;

/*-------------------*/
/* Donnees generales */
/*-------------------*/
typedef struct Donnees{
  /* Noms des fichiers */
  /*-------------------*/
  char name_rue[100];
  char name_noeud[100];
  char name_srce_ponct[100];
  char name_meteo[100];
  char name_evol_emis_rue[100];
  char name_evol_emis_noeud[100];
  char name_evol_emis_ponct[100];
  char name_pollu[100];
  char name_recept[100];
  char name_stat[100];
  char name_dir_resul[100];
  char name_dir_Surfer[100];
  char name_res_met[100];
  /* Tailles des tableaux */
  /*----------------------*/
  int N_Noeud;
  int N_Rue;
  int N_Srce_ponct;
  int N_Recept;
  int N_Temps;
  int iter_deb,iter_fin;
  /* Caracteristiques du quartier */
  /*------------------------------*/
  DBL Lat,f,Alb,Emissivite,Priestley_Taylor,Priestley_Taylor_lu;
  DBL zext,z0dext,z0d,d,z0t,dext,z0d_bat,Hmoy;
  /* Meteorologie */
  /*--------------*/
  int type_meteo;
  int type_mesure_vent;
  int type_haut,sequent,precipit;
  DBL gradText,gradthetaext,Po,Lmo_min;
  DBL U_min,sigmav_min,sigmaw_min;
  /* Dispersion */
  /*------------*/
  int calc_disp;
  int type_disp;
  DBL Di,sigmatheta;
  DBL sigma_y[N_Point_Disp],sigma_z[N_Point_Disp];
  DBL Pz_sol[N_Point_Disp],T_advect[N_Point_Disp];
  ParamNonGauss png[N_Point_Disp];
  /* Grille de travail */
  /*-------------------*/
  int N_Infl;
  Grid grd;
  Cell *cell;
  /* Chimie */
  /*--------*/
  int Type_Pol,Mod_Cste_Chim;
  DBL Taux_NO2,k1k3;
  /* Particules */
  /*------------*/
  DBL Diam_part,rho_part,Vit_T,Traj_Cross;
  /* Lessivage */
  /*-----------*/
  DBL lessiv_a,lessiv_b;
  /* Statistiques */
  /*--------------*/
  int N_Percentiles,N_Seuils;
  DBL *Percentiles,*Percentiles_NO,*Percentiles_NO2,*Percentiles_O3;
  DBL *Seuils,*Seuils_NO,*Seuils_NO2,*Seuils_O3;
  /* Affichage et sorties */
  /*----------------------*/
  int affich,calc_stat,ordre,champ,surfer,grille_horaire;
  int GrdRatio;
  Recept *Rec;
  /*------------------*/
} Donnees;

extern Donnees Don;
extern Meteo Met;
