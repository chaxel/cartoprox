/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Sigma.c --> Calcul des coeff. de dispersion */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"


/*---------------------------------------------*/


void Sigma()
/*--------------------------------------*/
/* Calcul des coefficients de diffusion */
/*--------------------------------------*/
{

  printf("\n-- Calcul des parametres de dispersion --\n\n");

  /* Cas des particules */
  if(Don.Type_Pol==2){
    /* Vitesse terminale des particules */
    Don.Vit_T=Vit_term(Don.Diam_part,Don.rho_part,Met.rho);
    /* Trajectory-crossing effect */
    Don.Traj_Cross=pow(1.0+DSQR(Don.Vit_T/(1.3*Met.us)),-0.25);

    printf("\tVitesse de sedimentation\t= %.2e m/s\n",Don.Vit_T);
    printf("\tAttenuation de la dispersion\t= %.3e\n",Don.Traj_Cross);
  }
  else{
    Don.Vit_T=0.0;
    Don.Traj_Cross=1.0;
  }

  /* Cas du modele de dispersion uniforme */
  if(Don.type_disp==0) SigmaUnif();
  /* Cas du modele de dispersion base */
  /* sur les classes de Pasquill      */
  else if(Don.type_disp==1) SigmaBriggsUrb();
  /* Cas du modele de dispersion par */
  /* similitude                      */
  else if(Don.type_disp==2) Sigma_simil(Met);
}


/*---------------------------------------------*/


void SigmaUnif()
/*--------------------------------------------*/
/* Coefficients pour une diffusivite uniforme */
/*--------------------------------------------*/
{
  int i;
  DBL dx=1.0,x,Up;

  Don.sigma_y[0]=0.0;
  Don.sigma_z[0]=0.0;
  Up=UP(Don.png[0],Met,Don.sigma_z[0],Don.Hmoy);
  Don.T_advect[0]=0.0;
  Don.Pz_sol[0]=0.0;
  for(i=1;i<N_Point_Disp;i++){
    x=(DBL) i;
    /* Temps de parcours */
    if(Up>1.0e-2) Don.T_advect[i]=Don.T_advect[i-1]+dx/Up;
    else Don.T_advect[i]=Don.T_advect[i-1];
    /* Ecarts-types */
    Don.sigma_y[i]=Don.Traj_Cross*sqrt(2.0*Don.Di*x/Met.Uext);
    Don.sigma_z[i]=Don.Traj_Cross*sqrt(2.0*Don.Di*x/Met.Uext);
    /* Vitesse d'advection */
    Up=UP(Don.png[i],Met,Don.sigma_z[i],Don.Hmoy-Don.Vit_T*Don.T_advect[i]);
    /* PDF pour la concentration au sol */
    Don.Pz_sol[i]=pdfz(Don.png[i],Met,Don.sigma_z[i],
		       Don.Hmoy-Don.Vit_T*Don.T_advect[i],Don.Hmoy)/Up;
  }
}


/*---------------------------------------------*/


void SigmaBriggsUrb()
/*-------------------------------------*/
/* Coefficients de diffusion de Briggs */
/* pour terrain de type urbain         */
/*                                     */
/* Briggs (1973) ou Griffiths (1994)   */
/*-------------------------------------*/
{
  int i;
  DBL dx=1.0,x,Up;

  Don.sigma_y[0]=0.0;
  Don.sigma_z[0]=0.0;
  Up=UP(Don.png[0],Met,Don.sigma_z[0],Don.Hmoy);
  Don.T_advect[0]=0.0;
  Don.Pz_sol[0]=0.0;

  for(i=1;i<N_Point_Disp;i++){
    x=(DBL) i;
    /* Temps de parcours */
    if(Up>1.0e-2) Don.T_advect[i]=Don.T_advect[i-1]+dx/Up;
    else Don.T_advect[i]=Don.T_advect[i-1];
    /* Ecarts-types */
    if(x<100.0){
      /* Formule de Briggs adaptee pour x<100 m  */
      /* La valeur de diffusivite turbulente est */
      /* supposee constante entre 0 et 100 m     */
      if(Met.Pasq=='A'){
	Don.sigma_y[i]=31.4*sqrt(x/100.0);
	Don.sigma_z[i]=25.2*sqrt(x/100.0);
      }
      else if(Met.Pasq=='B'){
	Don.sigma_y[i]=31.4*sqrt(x/100.0);
	Don.sigma_z[i]=25.2*sqrt(x/100.0);
      }
      else if(Met.Pasq=='C'){
	Don.sigma_y[i]=21.6*sqrt(x/100.0);
	Don.sigma_z[i]=20.0*sqrt(x/100.0);
      }
      else if(Met.Pasq=='D'){
	Don.sigma_y[i]=15.7*sqrt(x/100.0);
	Don.sigma_z[i]=13.8*sqrt(x/100.0);
      }
      else if(Met.Pasq=='E'){
	Don.sigma_y[i]=10.8*sqrt(x/100.0);
	Don.sigma_z[i]=7.46*sqrt(x/100.0);
      }
      else if(Met.Pasq=='F'){
	Don.sigma_y[i]=10.8*sqrt(x/100.0);
	Don.sigma_z[i]=7.46*sqrt(x/100.0);
      }
    }
    else{
      /* 100 m < x < 10000 m */
      if(Met.Pasq=='A'){
	Don.sigma_y[i]=0.32*x/sqrt(1.0+0.0004*x);
	Don.sigma_z[i]=0.24*x*sqrt(1.0+0.001*x);
      }
      else if(Met.Pasq=='B'){
	Don.sigma_y[i]=0.32*x/sqrt(1.0+0.0004*x);
	Don.sigma_z[i]=0.24*x*sqrt(1.0+0.001*x);
      }
      else if(Met.Pasq=='C'){
	Don.sigma_y[i]=0.22*x/sqrt(1.0+0.0004*x);
	Don.sigma_z[i]=0.20*x;
      }
      else if(Met.Pasq=='D'){
	Don.sigma_y[i]=0.16*x/sqrt(1.0+0.0004*x);
	Don.sigma_z[i]=0.14*x/sqrt(1.0+0.0003*x);
      }
      else if(Met.Pasq=='E'){
	Don.sigma_y[i]=0.11*x/sqrt(1.0+0.0004*x);
	Don.sigma_z[i]=0.08*x/sqrt(1.0+0.0015*x);
      }
      else if(Met.Pasq=='F'){
	Don.sigma_y[i]=0.11*x/sqrt(1.0+0.0004*x);
	Don.sigma_z[i]=0.08*x/sqrt(1.0+0.0015*x);
      }
    }
    /* Correction pour les particules */
    Don.sigma_y[i]*=Don.Traj_Cross;
    Don.sigma_z[i]*=Don.Traj_Cross;
    /* Vitesse d'advection */
    Up=UP(Don.png[i],Met,Don.sigma_z[i],Don.Hmoy-Don.Vit_T*Don.T_advect[i]);
    /* PDF pour la concentration au sol */
    Don.Pz_sol[i]=pdfz(Don.png[i],Met,Don.sigma_z[i],
		       Don.Hmoy-Don.Vit_T*Don.T_advect[i],Don.Hmoy)/Up;
  }
}


/*---------------------------------------------*/
