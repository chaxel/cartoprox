/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Sigma-Similitude.c --> Calcul des coeff. de */
/*     dispersion par la theorie de similitude */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"


/*---------------------------------------------*/


void sigmavwyz(Meteo met,DBL z,DBL t,DBL x,DBL *sigmav,DBL *sigmaw,
	       DBL *sigmaw_ic,DBL *sigmay,DBL *sigmaz)
/*------------------------------*/
/* Calcul de sigma v, w, y et z */
/*------------------------------*/
{
  /* Cas instable */
  if((met.h/met.Lmo)<-0.3){
    *sigmav=sigmavinstable(met,z);
    *sigmaw_ic=sigmawic(met,z);
    *sigmaw=sigmawinstable(met,*sigmaw_ic,z);
    *sigmay=sigmayinstable(met,z,t,x);
    *sigmaz=sigmazinstable(met,*sigmaw,z,t);
  }
  /* Cas stable */
  else if((met.h/met.Lmo)>1.0){
    *sigmav=sigmavstable(met,z);
    *sigmaw=sigmawstable(met,z);
    *sigmay=sigmaystable(met,*sigmav,z,t,x);
    *sigmaz=sigmazstable(met,*sigmaw,z,t);
  }
  /* Cas neutre */
  else{
    *sigmav=sigmavneutre(met,z);
    *sigmaw=sigmawneutre(met,z);
    *sigmay=sigmayneutre(met,*sigmav,t,x);
    *sigmaz=sigmazneutre(*sigmaw,t);
  }
}


/*---------------------------------------------*/


DBL sigmavinstable(Meteo met,DBL z)
/*------------------------------*/
/* Calcul de sigmav en instable */
/*------------------------------*/
{
  DBL sigmav_ic,sigmav_in;

  /* Terme convectif 0.547723 = sqrt(0.3) */
  sigmav_ic=0.547723*met.ws;

  /* Terme neutre */
  sigmav_in=2.0*(1.0-0.8*z/met.h)*met.us;
  return DMAX(sqrt(DSQR(sigmav_in)+DSQR(sigmav_ic)),Don.sigmav_min);
}


/*---------------------------------------------*/


DBL sigmavstable(Meteo met,DBL z)
/*----------------------------*/
/* Calcul de sigmav en stable */
/*----------------------------*/
{
  return DMAX(2.0*met.us*pow((1.0-0.5*z/met.h),0.75),Don.sigmav_min);
}


/*---------------------------------------------*/


DBL sigmavneutre(Meteo met,DBL z)
/*----------------------------*/
/* Calcul de sigmav en neutre */
/*----------------------------*/
{
  return DMAX(2.0*met.us*(1.0-0.8*z/met.h),Don.sigmav_min);
}


/*---------------------------------------------*/


DBL sigmawic(Meteo met,DBL z)
/*---------------------------------------*/
/* Calcul du terme sigmaw_ic en instable */
/*---------------------------------------*/
{
  /* 0.632455 = sqrt(0.4) */
  return 0.632455*met.ws*2.1*pow((z/met.h),0.333333)*(1.0-0.8*z/met.h);
}


/*---------------------------------------------*/


DBL sigmawinstable(Meteo met,DBL sigmaw_ic,DBL z)
/*------------------------------*/
/* Calcul de sigmaw en instable */
/*------------------------------*/
{
  DBL sigmaw_in;

  sigmaw_in=1.3*met.us*(1.0-0.8*z/met.h);
    return DMAX(sqrt(DSQR(sigmaw_ic)+DSQR(sigmaw_in)),Don.sigmaw_min);
}


/*---------------------------------------------*/


DBL sigmawstable(Meteo met,DBL z)
/*----------------------------*/
/* Calcul de sigmaw en stable */
/*----------------------------*/
{
    return DMAX(1.3*met.us*pow((1.0-0.5*z/met.h),0.75),Don.sigmaw_min);
}


/*---------------------------------------------*/


DBL sigmawneutre(Meteo met,DBL z)
/*----------------------------*/
/* Calcul de sigmaw en neutre */
/*----------------------------*/
{
    return DMAX(1.3*met.us*(1.0-0.8*z/met.h),Don.sigmaw_min);
}


/*---------------------------------------------*/


DBL sigmayinstable(Meteo met,DBL z,DBL t,DBL x)
/*------------------------------*/
/* Calcul de sigmay en instable */
/*------------------------------*/
{
  DBL sigmay_c,sigmay_n;

  /* Terme convectif 0.547723 = sqrt(0.3) */
  /* 0.90856 = 0.75^(1/3)                 */
  sigmay_c=0.547723*met.ws*t/sqrt(1.0+0.90856*t*met.ws/met.h);
  sigmay_n=2.0*(1.0-0.8*z/met.h)*met.us*t/sqrt(1.0+2.5*met.us*t/met.h);
  return sqrt(DSQR(sigmay_c)+DSQR(sigmay_n)+DSQR(met.sigmatheta*pi/180.0*x));
}


/*---------------------------------------------*/


DBL sigmaystable(Meteo met,DBL sigmav,DBL z,DBL t,DBL x)
/*----------------------------*/
/* Calcul de sigmay en stable */
/*----------------------------*/
{
  DBL sigmay_sa;

  sigmay_sa=sigmav*t/sqrt(1.0+2.5*met.us*t*met.Lmo/DSQR(met.h));
  return sqrt(DSQR(sigmay_sa)+DSQR(met.sigmatheta*pi/180.0*x));
}


/*---------------------------------------------*/


DBL sigmayneutre(Meteo met,DBL sigmav,DBL t,DBL x)
/*----------------------------*/
/* Calcul de sigmay en neutre */
/*----------------------------*/
{
  DBL sigmay_na;

  sigmay_na=sigmav*t/sqrt(1.0+2.5*met.us*t/met.h);
  return sqrt(DSQR(sigmay_na)+DSQR(met.sigmatheta*pi/180.0*x));
}


/*---------------------------------------------*/


DBL sigmazinstable(Meteo met,DBL sigmaw,DBL z,DBL t)
/*------------------------------*/
/* Calcul de sigmaz en instable */
/*------------------------------*/
{
  return sigmaw*t*pow(1.0+t/(2.0*TL(met,sigmaw,z)),-0.5);
}


/*---------------------------------------------*/


DBL sigmazstable(Meteo met,DBL sigmaw,DBL z,DBL t)
/*----------------------------*/
/* Calcul de sigmaz en stable */
/*----------------------------*/
{
  DBL N;

  N=freq_flot(met,z);
  /* Formule ADMS 3 pour zs=0 */
  return sigmaw*t/sqrt(6.25+DSQR(N*t)/(1.0+2.0*N*t));
}


/*---------------------------------------------*/


DBL sigmazneutre(DBL sigmaw,DBL t)
/*----------------------------*/
/* Calcul de sigmaz en neutre */
/*----------------------------*/
{
  /* Limite de l'expression en stable qd N -> 0 */
  return 0.4*sigmaw*t;
}


/*---------------------------------------------*/


DBL freq_flot(Meteo met,DBL z)
/*----------------------------------------*/
/* Calcul de la frequence de flottabilite */
/* de Brunt-Vaisala                       */
/*                                        */
/* voir doc ADMS 3                        */
/*----------------------------------------*/
{
  DBL zsu=DMIN(100.0,met.h);

  if(z<=zsu)
    return sqrt(DSQR(met.us)*Phim((z+Don.z0d)/met.Lmo)/(DSQR(kappa)*(z+Don.z0d)*met.Lmo));
  else if(z<=met.h)
    return sqrt(DSQR(met.us)*Phim((zsu+Don.z0d)/met.Lmo)/(DSQR(kappa)*(zsu+Don.z0d)*met.Lmo));
  else
    return met.Nu;
}


/*---------------------------------------------*/


DBL TL(Meteo met,DBL sigmaw,DBL z)
/*----------------------------*/
/* Calcul du temps lagrangien */
/*                            */
/* voir doc ADMS 3            */
/*----------------------------*/
{
  DBL zu,Lambda,gradu,N;

  /* On suppose Nu > 0 */  
  zu=DMAX(met.h-z,sigmaw/met.Nu);
    
  gradu=met.us/(kappa*(z+Don.z0d))*Phim(z/met.Lmo);

  /* Cas stable */
  if(met.Lmo>=0.0){
    N=freq_flot(met,z);    
    Lambda=1.0/(2.5/(z+Don.z0d)+N/sigmaw+4.0/met.h+1.0/zu);
    return Lambda/(1.3*sigmaw);
  }
  /* Cas instable */
  else{
    Lambda=1.0/(0.6/(z+Don.z0d)+gradu/sigmaw+2.0/met.h+1.0/zu);
    return (fabs(met.h/met.Lmo)+1.0/1.3)/
      (fabs(met.h/met.Lmo)+1.0)*Lambda/sigmaw;
  }
}


/*---------------------------------------------*/
