/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Us-Lmo.c --> Calcul des parametres de la    */
/*              theorie de similitude          */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"


/*---------------------------------------------*/


void Us_Lmo(DBL jour,DBL heure,DBL Cld,DBL rho,DBL To,DBL Uext,DBL *Ug,
	    DBL *elev_sol,DBL *Fo,DBL *us,DBL *Lmo,DBL *thetas)
/*---------------------------------*/	
/* Calcul de l'elevation solaire,  */
/* Fo, us, Lmo et thetas a partir  */
/* de l'heure, To, Cld, et Ug ou   */
/* Uext(zext)                      */
/*---------------------------------*/	
{
  DBL Fo_tmp,thetas_ext=0.0;

  /* Calcul de l'elevation solaire */
  *elev_sol=elevation_solaire(jour,heure);

  /*-------------------------------*/
  /* Cas d'une meteo neutre forcee */
  /*-------------------------------*/
  if(Don.type_meteo==0){
    /* Calcul de la vitesse de frottement */
    UCalc2(0.0,Uext,rho,To,Ug,Lmo,us);
  }

  /*------------------------------------------*/
  /* Cas d'une meteo thermiquement stratifiee */
  /*------------------------------------------*/
  else if(Don.type_meteo==1){
    /* Cas de la nuit */
    if(*elev_sol<=0.0){
      *thetas=0.09*(1.0-0.5*DSQR(Cld/8.0));
      if(Don.type_mesure_vent==1){
	thetas_ext=DMIN(0.9*kappa*DSQR(Uext)*To/
			(20.0*log(Don.zext/Don.z0dext)*Don.zext*g),
			0.09*(1.0-0.5*DSQR(Cld/8.0)));
      }
      UCalc3(*thetas,thetas_ext,Uext,rho,To,Ug,Lmo,us,Fo);
    }
    
    /* Cas du jour */
    else{
      Fo_tmp=Flux_jour(*elev_sol,Cld,To);
      if(Fo_tmp>=0.0){
	*Fo=Fo_tmp;
	UCalc2(*Fo,Uext,rho,To,Ug,Lmo,us);
      }
      else{
	*thetas=0.09*(1.0-0.5*DSQR(Cld/8.0));
	if(Don.type_mesure_vent==1){
	  thetas_ext=DMIN(0.9*kappa*DSQR(Uext)*To/
			  (20.0*log(Don.zext/Don.z0dext)*Don.zext*g),
			  0.09*(1.0-0.5*DSQR(Cld/8.0)));
	}
	UCalc3(*thetas,thetas_ext,Uext,rho,To,Ug,Lmo,us,Fo);      
	if(*Fo<Fo_tmp){
	  *Fo=Fo_tmp;
	  UCalc2(*Fo,Uext,rho,To,Ug,Lmo,us);
	}
      }
    }
  }

  /* Temperature de frottement */
  *thetas=-*Fo/(rho*Cp*(*us));
}


/*---------------------------------------------*/


void UCalc2(DBL Fo,DBL Uext,DBL rho,DBL To,DBL *Ug,DBL *Lmo,DBL *us)
/*---------------------------------*/	
/* Calcul us et Lmo a partir de Fo */
/* et Ug ou Uext                   */
/*                                 */
/* voir doc ADMS 3                 */
/*---------------------------------*/	
{
  int i;
  DBL us_prec,erreur=1.0;

/*   printf("\n-- UCalc2 --\n\n"); */

  /* Determination iterative de us a partir de Uext(zext) */
  if(Don.type_mesure_vent==1){
    *us=Uext/U(Don.zext-Don.dext,1.0,Don.z0dext,99999.0);
    us_prec=*us;
    for(i=0;i<500 && erreur>1.0e-5;i++){
      *Lmo=LongMonin(*us,rho,Fo,To);
      if(*Lmo>0.0) *Lmo=DMAX(*Lmo,Don.Lmo_min);
      *us=Uext/U(Don.zext-Don.dext,1.0,Don.z0dext,*Lmo);
      
      erreur=fabs(*us-us_prec)/(*us);
      *us=0.1*us_prec+0.9*(*us);
      us_prec=*us;
    }
    if(i==500){
      Erreur("Precision souhaitee non atteinte dans Ucalc2",1);
      printf("Erreur = %.2f %%\n",100.0*erreur);
    }
    *Ug=*us/Ug_us(*us,1.0,Fo,*Lmo,Don.z0dext);
  }

  /* Determination iterative de us a partir de Ug  */
  /* - Soit Ug est l'information fournie en entree */
  /* - Soit Ug a ete calcule ci-dessus a partir de */
  /*   au site de mesure (si z0d =/= z0dext)       */
  if(Don.type_mesure_vent==0 ||
     (Don.type_mesure_vent==1 && fabs(Don.z0d-Don.z0dext)>1.0e-5)){
    *us=(*Ug)/U(500.0,1.0,Don.z0d,99999.0);
    us_prec=*us;
    erreur=1.0;
    for(i=0;i<500 && erreur>1.0e-5;i++){
      *Lmo=LongMonin(*us,rho,Fo,To);
      if(*Lmo>0.0) *Lmo=DMAX(*Lmo,Don.Lmo_min);
      *us=Ug_us(*us,*Ug,Fo,*Lmo,Don.z0d);
      
      erreur=fabs(*us-us_prec)/(*us);
      *us=0.1*us_prec+0.9*(*us);
      us_prec=*us;
    }
    if(i==500){
      Erreur("Precision souhaitee non atteinte dans Ucalc2",1);
      printf("Erreur = %.2f %%\n",100.0*erreur);
    }
  }
}


/*---------------------------------------------*/


void UCalc3(DBL thetas,DBL thetas_ext,DBL Uext,DBL rho,DBL To,DBL *Ug,
	    DBL *Lmo,DBL *us,DBL *Fo)
/*----------------------------------*/	
/* Calcul us, Fo et Lmo a partir de */
/* thetas et Ug ou Uext             */
/*                                  */
/* voir doc ADMS 3                  */
/*----------------------------------*/	
{
  int i;
  DBL us_prec,erreur=1.0;

/*   printf("\n-- UCalc3 --\n\n"); */

  /* Determination iterative de us a partir de Uext(zext) */
  if(Don.type_mesure_vent==1){
    *us=Uext/U(Don.zext-Don.dext,1.0,Don.z0dext,99999.0);
    us_prec=*us;
    for(i=0;i<500 && erreur>1.0e-5;i++){
      *Fo=Flux_nuit(*us,rho,thetas_ext);
      *Lmo=DMAX(LongMonin(*us,rho,*Fo,To),Don.Lmo_min);
      *us=Uext/U(Don.zext-Don.dext,1.0,Don.z0dext,*Lmo);
      erreur=fabs(*us-us_prec)/(*us);
      *us=0.1*us_prec+0.9*(*us);
      us_prec=*us;
    }
    if(i==500) Erreur("Precision souhaitee non atteinte dans Ucalc3",1);
    *Ug=*us/Ug_us(*us,1.0,*Fo,*Lmo,Don.z0dext);
  }

  /* Determination iterative de us a partir de Ug  */
  /* - Soit Ug est l'information fournie en entree */
  /* - Soit Ug a ete calcule ci-dessus a partir de */
  /*   au site de mesure (si z0d =/= z0dext)       */
  if(Don.type_mesure_vent==0 ||
     (Don.type_mesure_vent==1 && fabs(Don.z0d-Don.z0dext)>1.0e-5)){
    *us=*Ug/U(500.0,1.0,Don.z0d,99999.0);
    us_prec=*us;
    erreur=1.0;
    for(i=0;i<500 && erreur>1.0e-5;i++){
      *Fo=DMAX(Flux_nuit(*us,rho,thetas),
	       -0.8*0.145*fabs(Don.f)*rho*Cp*To*DSQR(*Ug)/g);
      *Lmo=DMAX(LongMonin(*us,rho,*Fo,To),Don.Lmo_min);
      *us=Ug_us(*us,*Ug,*Fo,*Lmo,Don.z0d);
      
      erreur=fabs(*us-us_prec)/(*us);
      *us=0.1*us_prec+0.9*(*us);
      us_prec=*us;
    }
    if(i==500) Erreur("Precision souhaitee non atteinte dans Ucalc3",1);
  }
}


/*---------------------------------------------*/


DBL LongMonin(DBL us,DBL rho,DBL Fo,DBL To)
/*----------------------------------------*/
/* Calcul de la longueur de Monin-Obukhov */
/*----------------------------------------*/
{
  if(fabs(Fo)>1.0e-5)
    return -us*us*us*rho*Cp*To/(kappa*Fo*g);
  else
    return 99999.0;
}


/*---------------------------------------------*/


DBL Flux_jour(DBL elev_sol,DBL Cld,DBL To)
/*-------------------------------------*/
/* Calcul du flux de chaleur sensible  */
/* pour une situation de jour          */
/*                                     */
/* voir doc ADMS 3 et                  */
/* Holtslag et Van Ulden (1983)        */
/*-------------------------------------*/
{
  DBL Kp,Rn,S;

  /* Calcul du flux solaire incident */
  Kp=(990.0*elev_sol-30.0)*(1.0-0.75*pow(Cld/8.0,3.4));

  /* Calcul du flux radiatif net */
  Rn=((1.0-Don.Alb)*Kp+(5.31e-13)*pow(To,6.0)-
      Don.Emissivite*(5.67e-8)*pow(To,4.0)+60.0*Cld/8.0)/1.12;
  S=exp(0.055*(To-279.0));
  return ((1.0-Don.Priestley_Taylor)*S+1.0)/(S+1.0)*0.7*Rn-20.0*Don.Priestley_Taylor;
}


/*---------------------------------------------*/


DBL Flux_nuit(DBL us,DBL rho,DBL thetas)
/*-------------------------------------*/
/* Calcul du flux de chaleur sensible  */
/* pour une situation de nuit          */
/*                                     */
/* Formule de definition du flux       */
/*-------------------------------------*/
{
  return -rho*Cp*us*thetas;
}


/*---------------------------------------------*/


DBL Ug_us(DBL us,DBL Ug,DBL Fo,DBL Lmo,DBL z0d)
/*-------------------------------------*/
/* Calcul de ustar a partir de Ug, Lmo */
/* et us                               */
/*                                     */
/* voir doc ADMS 3, Arya (1975) et     */
/* Brost et Wyngaard (1978)            */
/*-------------------------------------*/
{
  DBL h,Tx,Ty,mu,A,B;

  if(Fo<=0.0){
    h=Haut_CLA_stable(us,Lmo);
    Tx=2.2*h/Lmo+log((h+30.0*z0d)/z0d)+0.19;
    Ty=-DMAX((3.55*h/Lmo+1.87),5.14)*Don.f/fabs(Don.f);
  }
  else{
    mu=kappa*us/(fabs(Don.f)*Lmo);
    if(mu>-50.0){
      A=1.01-0.105*mu-9.9e-4*mu*mu+8.1e-7*mu*mu*mu;
      B=5.14+0.142*mu+1.17e-3*mu*mu-3.3e-6*mu*mu*mu;
    }
    else{
      A=3.69;
      B=1.38;
    }
    Tx=log((us/fabs(Don.f)+100.0*z0d)/z0d)-A;
    Ty=-B*Don.f/fabs(Don.f);
  } 
  return sqrt(DSQR(kappa*Ug)/(DSQR(Tx)+DSQR(Ty)));
}


/*---------------------------------------------*/


DBL elevation_solaire(DBL jour,DBL heure)
/*-------------------------------*/
/* Calcul de l'elevation solaire */
/*-------------------------------*/
{
  DBL decl_sol;

  decl_sol=23.45*sin((jour+284.0)/365.0*2.0*pi);
  return sin(Don.Lat*pi/180.0)*sin(decl_sol*pi/180.0)+
    cos(Don.Lat*pi/180.0)*cos(decl_sol*pi/180.0)*
    cos((heure-12.0)/24.0*2.0*pi);
}


/*---------------------------------------------*/
