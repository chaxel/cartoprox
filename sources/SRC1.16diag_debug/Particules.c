/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Particules.c -> Calcul des caracteristiques */
/*              de la dispersion de particules */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"


/*---------------------------------------------*/


DBL Vit_term(DBL d,DBL rho_p,DBL rho_f)
/*---------------------------------*/
/* Calcul de la vitesse terminale  */
/* d'une particule, par resolution */
/* de l'equation instationnaire    */
/*---------------------------------*/
{
  int n;
  DBL ReT,Up,Cd,gam,Up_prec=0.0;
  DBL residu=1.0,dt;

  gam=rho_p/rho_f;
  Up=DMIN(1.0e-3,gam*d*d*g/(18.0*nuair));
  for(n=0;n<1000 && residu>1.0e-6;n++){
    ReT=fabs(Up)*d/nuair;
    if(ReT<1000.0) Cd=24.0/ReT*(1.0+0.15*pow(ReT,0.687));
    else Cd=0.438;
    dt=0.5*Up/g;
    Up=Up+dt*((1.0-1.0/gam)*g-0.75*Cd*Up*fabs(Up)/(d*gam));
    residu=fabs(fabs(Up)-fabs(Up_prec))/fabs(Up);
    Up_prec=Up;
  }
  if(n==1000){
    Erreur("Precision souhaitee non atteinte dans Vit_term",1);
    printf("Erreur = %.2f %%\n",100.0*residu);
  }
  return Up;
}


/*---------------------------------------------*/
