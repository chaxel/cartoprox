/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Fonc.h --> Entetes des fonctions            */
/*                                             */
/***********************************************/
#ifndef _FONC_
#define _FONC_

/*----------*/
/* Bessel.c */
/*----------*/
DBL Jo(DBL x);
DBL J1(DBL x);
DBL Yo(DBL x);
DBL Y1(DBL x);

/*--------------------*/
/* Calcul-Conc-Fond.c */
/*--------------------*/
DBL Conc_Fond(DBL x,DBL y,int cell_i,Noeud *Nd,
	      Rue *R,Source_Ponct *Srce_Ponct);

/*-------------------*/
/* Calcul-Conc-Grd.c */
/*-------------------*/
void Calcul_grd(DBL **Conc,DBL Conc_fond,Rue *R,Noeud *N,
		Source_Ponct *Srce_Ponct,Grid Grd,DBL **IndRN);
void Calcul_grd_Chapman(DBL **C_NOx,DBL **C_NO,DBL **C_NO2,DBL **C_O3,
			DBL Cb_NOx,DBL Cb_NO,DBL Cb_NO2,DBL Cb_O3,
			DBL k1,DBL k3,Grid Grd);
DBL Conc_Recept(DBL x,DBL y,Rue *R,Noeud *N,
		Source_Ponct *Srce_Ponct);
int Inclu_Grd(DBL x,DBL y,Grid grd);

/*----------*/
/* Chimie.c */
/*----------*/
void Chapman(DBL Cd_NOx,DBL Cb_O3,DBL Cb_NO,DBL Cb_NO2,
	     DBL *C_NO,DBL *C_NO2,DBL *C_O3,DBL k1,DBL k3);
DBL Cste_k1(DBL elev_sol,DBL Cld);
DBL Cste_k3(DBL Tkelv,DBL Vo);

/*--------------*/
/* Fonct-Univ.c */
/*--------------*/
DBL Phim(DBL ksi);
DBL dPhim(DBL ksi);
DBL Psim(DBL ksi);
DBL Psih(DBL ksi);

/*---------*/
/* Gauss.c */
/*---------*/
DBL Gauss_Lineic(DBL x,DBL y,DBL Ly,DBL delta_x,
		 DBL dx,DBL dy,DBL L);
DBL Gauss(DBL x,DBL y,DBL Ly,DBL delta_x);
DBL Gaussz(DBL x,DBL y,DBL H);

/*---------------*/
/* Hauteur-CLA.c */
/*---------------*/
DBL Haut_CLA(Meteo met,Date date);
DBL Haut_CLA_stable(DBL us,DBL Lmo);
DBL Haut_CLA_instable(Meteo met,Date date);
DBL Integre_hCLA(DBL h0,DBL dt,DBL rho,DBL Fo,DBL us,DBL To);
DBL Fonc_CLA(DBL h0,DBL h,DBL dt,DBL x,DBL a1);
DBL lever_soleil(DBL jour);
DBL inversion_flux(Meteo met,DBL jour);

/*--------*/
/* Init.c */
/*--------*/
void Init(Noeud *Nd,Rue *R);
void Init_noeud(Noeud *Nd,Rue *R);
void Init_rue(Noeud *Nd,Rue *R);
void Init_grd(Noeud *Nd,Rue *R);
void rugo(DBL x,DBL *fn, DBL *df,DBL z0);
DBL rtnewt(void (*rugo)(DBL,DBL *,DBL *,DBL),
	   DBL x1,DBL x2,DBL xacc,DBL z0);

/*------------------------*/
/* Init-Disp-Similitude.c */
/*------------------------*/
void Sigma_simil(Meteo met);
DBL ZM(ParamNonGauss png,Meteo met,DBL sigma_z,DBL zs);
DBL UP(ParamNonGauss png,Meteo met,DBL sigma_z,DBL zs);
DBL pdfz(ParamNonGauss png,Meteo met,DBL sigmaz,DBL zs,DBL z);
DBL pw(ParamNonGauss png,DBL z,DBL zs);
void Coeff_pw(DBL sigmaw,DBL sigmaw_ic,DBL zm,DBL t,
	      ParamNonGauss *png,Meteo met);

/*--------------*/
/* Init-Meteo.c */
/*--------------*/
void initmeteo(DonIter *Iter);
DBL SigmaThetaPasquill(Meteo met);

/*----------------*/
/* Intersection.c */
/*----------------*/
void Intersect(Noeud *Nd,Rue *R);
void Flux_inters(Noeud Nd,int i,Rue *R,DBL **Flux);

/*------*/
/* Io.c */
/*------*/
void lecdon(char *fichier,Noeud **NN,Rue **RR,Source_Ponct **II,
	    DonIter **Iter,Grid *Grd);
void lecemis(Noeud **N,Rue **R,Source_Ponct **I,DonIter Iter);
void Test_Valid(DBL Value,int *Valid);

/*----------*/
/* Io-Don.c */
/*----------*/
void lecdongene(char *fichier,Grid *Grd,int affich);

/*------------*/
/* Io-Resul.c */
/*------------*/
void Init_resul(Rue *R);
void Suppr_tmp();
void Ecrire_resul_iter(Rue *R,DBL *C,DBL *C_NO,
		  DBL *C_NO2,DBL *C_O3,
		  DonIter Iter,int type,int pas_temps);
void Ecrire_resul_rue_recept(Rue *R,DonIter *Iter,int type);
void Ecrire_meteo(DonIter Iter,int pas_temps);

/*-------------*/
/* Io-Surfer.c */
/*-------------*/
void Ecrire_Rue_bln(Noeud *Nd,Rue *R);
void Ecrire_Forme_Rue_bln(Noeud *Nd,Rue *R);
void Ecrire_grd(DBL **Conc,Grid Grd,char *suffix,int num);
void Ecrire_grd_Surfer(DBL **Conc,Grid Grd,char *suffix,int num);
void Ecrire_grd_VerticalMapper(DBL **Conc,Grid Grd,char *suffix,int num);
int Lire_grd(DBL *Conc,char *suffix,int num,int n_debut,int N_Bloc);
int Lire_grd_Surfer(DBL *Conc,char *suffix,int num,int n_debut,int N_Bloc);
int Lire_grd_VerticalMapper(DBL *Conc,char *suffix,int num,int n_debut,int N_Bloc);

/*-------------*/
/* Particules.c */
/*-------------*/
DBL Vit_term(DBL d,DBL rho_p,DBL rho_f);

/*----------*/
/* Profil.c */
/*----------*/
DBL U(DBL z,DBL us,DBL z0d,DBL Lmo);
DBL T(DBL z,DBL z0t,Meteo met);

/*---------*/
/* Sigma.c */
/*---------*/
void Sigma();
void SigmaUnif();
void SigmaBriggsUrb();

/*--------------------*/
/* Sigma-Similitude.c */
/*--------------------*/
void sigmavwyz(Meteo met,DBL z,DBL t,DBL x,DBL *sigmav,DBL *sigmaw,
	       DBL *sigmaw_ic,DBL *sigmay,DBL *sigmaz);
DBL sigmavinstable(Meteo met,DBL z);
DBL sigmavstable(Meteo met,DBL z);
DBL sigmavneutre(Meteo met,DBL z);
DBL sigmawic(Meteo met,DBL z);
DBL sigmawinstable(Meteo met,DBL sigmaw_ic,DBL z);
DBL sigmawstable(Meteo met,DBL z);
DBL sigmawneutre(Meteo met,DBL z);
DBL sigmayinstable(Meteo met,DBL z,DBL t,DBL x);
DBL sigmaystable(Meteo met,DBL sigmav,DBL z,DBL t,DBL x);
DBL sigmayneutre(Meteo met,DBL sigmav,DBL t,DBL x);
DBL sigmazinstable(Meteo met,DBL sigmaw,DBL z,DBL t);
DBL sigmazstable(Meteo met,DBL sigmaw,DBL z,DBL t);
DBL sigmazneutre(DBL sigmaw,DBL t);
DBL freq_flot(Meteo met,DBL z);
DBL TL(Meteo met,DBL sigmaw,DBL z);

/*-------------*/
/* Solv-Conc.c */
/*-------------*/
void Solv_Conc(Noeud *Nd,Rue *R,Source_Ponct *Srce_Ponct);
DBL Convergence(int n,Noeud *Nd,Rue *R,DBL *Cprec);

/*-------------------*/
/* Solv-Conc-Noeud.c */
/*-------------------*/
void Solv_Noeud(Noeud *Nd,Rue *R);

/*-----------------*/
/* Solv-Conc-Rue.c */
/*-----------------*/
void Solv_Rue(Noeud *Nd,Rue *R);

/*--------------*/
/* Solv-Debit.c */
/*--------------*/
void Solv_Debit(Noeud *Nd,Rue *R,DBL Uix,DBL Uiy);

/*--------*/
/* Stat.c */
/*--------*/
void Stat(Rue *R,DonIter *Iter,Grid Grd);
void Stat_Point(DBL *C,Date *date,int N,
		DBL *Percentiles,DBL *Seuils,
		DBL *Cmoy,DBL *Cmax,DBL *Percent,
		int *Hrseuil,int *Jrseuil);

/*------------*/
/* Stat-Grd.c */
/*------------*/
void Stat_Grd(DBL *Percentiles,DBL *Seuils,Grid Grd,
	      DonIter *Iter,char *suffix);

/*-------------------*/
/* Stat-Rue-Recept.c */
/*-------------------*/
void Stat_Rue_Recept(DBL *Percentiles,DBL *Seuils,
		     Rue *R,DonIter *Iter,char *suffix,
		     int type);

/*--------------*/
/* Surhauteur.c */
/*--------------*/
DBL Hauteur_effective(Source_Ponct Srce_Ponct,Meteo met,DBL x);
DBL Surhauteur(DBL Rs,DBL Ws,DBL Ts,
	       DBL H,Meteo met,DBL x);

/*----------*/
/* Us-Lmo.c */
/*----------*/
void Us_Lmo(DBL jour,DBL heure,DBL Cld,DBL rho,DBL To,DBL Uext,DBL *Ug,
	    DBL *elev_sol,DBL *Fo,DBL *us,DBL *Lmo,DBL *thetas);
void UCalc2(DBL Fo,DBL Uext,DBL rho,DBL To,DBL *Ug,DBL *Lmo,DBL *us);
void UCalc3(DBL thetas,DBL thetas_ext,DBL Uext,DBL rho,DBL To,DBL *Ug,
	    DBL *Lmo,DBL *us,DBL *Fo);
DBL LongMonin(DBL us,DBL rho,DBL Fo,DBL To);
DBL Flux_jour(DBL elev_sol,DBL Cld,DBL To);
DBL Flux_nuit(DBL us,DBL rho,DBL thetas);
DBL Ug_us(DBL us,DBL Ug,DBL Fo,DBL Lmo,DBL z0d);
DBL elevation_solaire(DBL jour,DBL heure);

/*--------*/
/* Util.c */
/*--------*/
int ISQR(int sqrarg);
DBL DSQR(DBL sqrarg);
int IMAX(int maxarg1,int maxarg2);
DBL DMAX(DBL maxarg1,DBL maxarg2);
int IMIN(int minarg1,int minarg2);
DBL DMIN(DBL minarg1,DBL minarg2);
DBL DSIGN(DBL a,DBL b);
void nrerror(char error_text[]);
void Erreur(char error_text[],int type);
void BarreAvance(int indice,int N);
FILE *OuvreFichier(char *nom_fichier,char *mode);
void FermeFichier(FILE *fich);
void CreationRepertoire(char *path);

/*-------------*/
/* Util-Date.c */
/*-------------*/
void Jour_Annee(Date *date);
void Jour_Semaine(Date *date);

/*------------*/
/* Util-Mem.c */
/*------------*/
int **AllocMtrxI(int nl,int nc);
DBL **AllocMtrx(int nl,int nc);
int ***AllocBoiteI(int nl,int nc,int np);
DBL ***AllocBoite(int nl,int nc,int np);
void LibereMtrxI(int **md,int nl,int nc);
void LibereMtrx(DBL **md,int nl,int nc);
void LibereBoiteI(int ***md,int nl,int nc,int np);
void LibereBoite(DBL ***md,int nl,int nc,int np);
void LibereNoeud(Noeud *Nd);
void InitTabDBL(DBL *tab,int N,DBL val);

/*---------------------------------------------*/

#endif
