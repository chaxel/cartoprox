/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Gauss.c --> Calcul des panaches gaussiens   */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"


/*---------------------------------------------*/


DBL Gauss_Lineic(DBL x,DBL y,DBL Ly,DBL delta_x_z,
		 DBL dx,DBL dy,DBL L)
/*------------------------------*/
/* Coefficient d'influence d'un */
/* panache gaussien a partir    */
/* d'une source lineique        */
/*------------------------------*/
{
  DBL si,sn,Larg;
  DBL Conc=0.0;
 
  /* Concentration nulle si :  */
  /* - recepteur sur la source */
  /* - recepteur trop loin     */
  /* - recepteur en amont      */
  if((x+delta_x_z)>=N_Point_Disp ||
     x<-0.5*L) return 0.0;

  /* Discretisation de la source lineique   */
  /* comme une serie de sources ponctuelles */
  /* a intervalles reguliers                */

  /* Calcul de la taille des intervalles */
  /* taille = L si x > 10 L              */
  /* taille = x/10 si 100 m < x < 10 L   */
  /* taille = 10 m si x < 100m           */
  if(x>100.0) sn=1.0/(int)(L/(x/10.0)+1.0);
  else sn=1.0/(int)(L/10.0+1.0);

  /* Calcul de la largeur de la source */
  if(fabs(dx)<1.0E-3) Larg=0.5*sn*L;
  else if(fabs(dy)<1.0E-3) Larg=Ly;
  else Larg=DMIN(fabs(0.5*sn*L*L/dy),fabs(Ly*L/dx));

  /* Sommation sur les intervalles */
  for(si=0.5*(-1.0+sn);si<0.5;si+=sn){
    Conc+=Gauss(x+si*dx,y+si*dy,Larg,delta_x_z);
  }
  return Conc*sn;
}


/*---------------------------------------------*/


DBL Gauss(DBL x,DBL y,DBL Ly,DBL delta_x_z)
/*----------------------------*/
/* Coefficient d'influence au */
/* sol d'un panache gaussien  */
/* emis au sol                */
/*----------------------------*/
{
  DBL sigma_y,Pz_sol,lessiv=1.0,facteur;

  /* Concentration nulle si :              */
  /* - recepteur sur la source ou en amont */
  /* - recepteur trop loin                 */
  if(x<1.0 ||
     (x+delta_x_z)>=N_Point_Disp) return 0.0;

  /* Coefficients de diffusion */
  sigma_y=Don.sigma_y[(int) x];
  Pz_sol=Don.Pz_sol[(int) (x+delta_x_z)];

  /* Concentration nulle si le recepteur */
  /* est a l'exterieur du panache        */
  if(fabs(y)>(3.0*sigma_y+Ly)) return 0.0;

  /* Prise en compte du lessivage */
  if(Don.precipit==1)
    lessiv=exp(-Met.Taux_Lessiv*Don.T_advect[(int) (x+delta_x_z)]);

  /* Concentration */
  facteur=1.0/DMAX(Sqrt2Pi*sigma_y,2.0*Ly);
  return lessiv*Pz_sol*facteur*
    exp(-0.5*(DSQR(DMAX(fabs(y)-DMAX(Ly-SqrtPi2*sigma_y,0.0),0.0)/sigma_y)));
}


/*---------------------------------------------*/


DBL Gaussz(DBL x,DBL y,DBL H)
/*----------------------------*/
/* Coefficient d'influence au */
/* sol d'un panache gaussien  */
/* emis a une hauteur H       */
/*----------------------------*/
{
  DBL sigma_y,sigma_z,lessiv=1.0,t;
  ParamNonGauss png;

  /* Concentration nulle si :              */
  /* - recepteur sur la source ou en amont */
  /* - recepteur trop loin                 */
  if(x<1.0E-5 ||
     x>=N_Point_Disp) return 0.0;

  /* Coefficients de diffusion */
  sigma_y=Don.sigma_y[(int) x];
  sigma_z=Don.sigma_z[(int) x];
  png=Don.png[(int) x];

  /* Concentration nulle si le recepteur */
  /* est a l'exterieur du panache        */
  if(fabs(y)>3.0*sigma_y) return 0.0;

  /* Temps de parcours */
  t=Don.T_advect[(int) x];

  /* Prise en compte du lessivage */
  if(Don.precipit==1)
    lessiv=exp(-Met.Taux_Lessiv*t);

  /* Calcul de la concentration en utilisant */
  /* les ecarts-types calcules au sol        */
  return lessiv*exp(-0.5*(DSQR(y/sigma_y)))*
    pdfz(png,Met,sigma_z,H-Don.Vit_T*t,Don.Hmoy)/
    (Sqrt2Pi*sigma_y*UP(png,Met,sigma_z,H-Don.Vit_T*t));
}


/*---------------------------------------------*/
