/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Profil.c --> Calcul des profils verticaux   */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"


/*---------------------------------------------*/


DBL U(DBL z,DBL us,DBL z0d,DBL Lmo)
/*----------------------------------*/
/* Calcul de la vitesse U           */
/*  Garratt, 1992                   */
/* "The atmospheric boundary layer" */
/*  page 53                         */
/*----------------------------------*/
{
  return (us/kappa)*(log((z+z0d)/z0d)-(Psim((z+z0d)/Lmo)-Psim(z0d/Lmo)));
}


/*---------------------------------------------*/


DBL T(DBL z,DBL z0t,Meteo met)
/*----------------------------------*/
/* Calcul de la temperature T       */
/*  Garratt, 1992                   */
/* "The atmospheric boundary layer" */
/*  page 54                         */
/*----------------------------------*/
{
  DBL zsu=DMIN(100.0,met.h),theta,theta_zsu,theta_h;

  /* Cas neutre et stable */
  if(met.Lmo>=0.0){
    if(z<=zsu)
      theta=met.To+(met.thetas/kappa)*(log((z+z0t)/z0t)-(Psih((z+z0t)/met.Lmo)-Psih(z0t/met.Lmo)));
    else if(z<=met.h){
      theta_zsu=met.To+(met.thetas/kappa)*(log((zsu+z0t)/z0t)-
					   (Psih((zsu+z0t)/met.Lmo)-Psih(z0t/met.Lmo)));
      theta=theta_zsu+met.To/g*DSQR(freq_flot(met,zsu))*(z-zsu);
    }
    else
      theta=theta_zsu+met.To/g*(DSQR(freq_flot(met,zsu))*(met.h-zsu)+
				DSQR(met.Nu)*(z-zsu));
  }
  /* Cas instable */
  else{
    if(z<=met.h)
      theta=met.To+(met.thetas/kappa)*(log((z+z0t)/z0t)-(Psih((z+z0t)/met.Lmo)-Psih(z0t/met.Lmo)));
    else{
      theta_h=met.To+(met.thetas/kappa)*(log((met.h+z0t)/z0t)-
					 (Psih((met.h+z0t)/met.Lmo)-Psih(z0t/met.Lmo)));
      theta=theta_h+met.To/g*DSQR(met.Nu)*(z-met.h);
    }
  }

  return theta-g/Cp*z;
}


/*---------------------------------------------*/
