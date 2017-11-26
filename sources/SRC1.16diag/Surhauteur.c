/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Surhauteur.c --> Calcul de surhauteur       */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"


/*---------------------------------------------*/


DBL Hauteur_effective(Source_Ponct Srce_Ponct,Meteo met,DBL x)
/*--------------------------------*/
/* Calcul de la hauteur effective */
/* d'un rejet ponctuel            */
/*--------------------------------*/
{
  DBL alpha=2.0,Delta_H,H_eff;

  /* Calcul de la surhauteur */
  if(x<=0) Delta_H=0.0;
  else {
    Delta_H=Surhauteur(Srce_Ponct.Rs,Srce_Ponct.Ws,Srce_Ponct.Ts,
		     Srce_Ponct.H-Srce_Ponct.H_sol,met,x);
   
  }
  /* Calcul de la hauteur effective */
  H_eff=Srce_Ponct.H+Delta_H;
  /* Modification de la hauteur pour tenir compte */
  /* des effets de deviation verticale du panache */
  /* en-dessous de alpha*Hmoy (hauteur au-dela de */
  /* laquelle il n'y a plus de deviation)         */
  if(H_eff<(alpha*Don.Hmoy)){
    H_eff=(H_eff-Srce_Ponct.H_sol)/
      (alpha*Don.Hmoy-Srce_Ponct.H_sol)*Don.Hmoy+Don.Hmoy;
  }
  return H_eff;
}


/*---------------------------------------------*/


DBL Surhauteur(DBL Rs,DBL Ws,DBL Ts,
	       DBL H,Meteo met,DBL x)
/*------------------------------*/
/* Calcul de surhauteur de      */
/* panache par les formules de  */
/* Briggs (1984).               */
/* Voir aussi le doc d'AERMOD   */
/* (Cimorelli, 1998)            */
/*------------------------------*/
{
  int i;
  DBL Up,Uph,Ta,Fm,Fb,beta1=0.6,beta2=0.4+1.2/Rs;
  DBL Fs,dHm,dHb;
  DBL dH=0.0,dHs,dHs_inf,dHn,dHsc,N,Nh,Np,Ln;
  DBL residu=1.0,dHprec=0.0;

  /* Calcul de la temperature et de la vitesse a la hauteur de la cheminee */
  Uph=U(H,met.us,Don.z0d,met.Lmo);
  Ta=T(H,Don.z0t,met);
  /* Flux de quantite de mouvement */
  Fm=Ta/Ts*DSQR(Ws*Rs);
  /* Flux de flottabilite */
  Fb=g*Ws*Rs*Rs*(Ts-Ta)/Ts;
  if(Fb<=0.0) return 0.0;

  /* Cas instable */
  if(met.Lmo<0){
    Fs=met.Fo*g/(met.rho*Cp*met.To);
    /* Methode iterative pour le calcul de Up */
    for(i=0;i<50 && residu>1.0e-5;i++){
      Up=0.5*(Uph+U(H+0.5*dH,met.us,Don.z0d,met.Lmo));
      Up=DMAX(Up,0.5);
      dH=pow(3.0*Fm*x/DSQR(beta2*Up)+
	     1.5*Fb*x*x/(beta1*beta1*Up*Up*Up),1.0/3.0);
      /* Valeur maxi de la surhauteur */
      dHm=1.3*pow((0.4+1.2)/Rs,-6.0/7.0)*pow(Fm/Up,3.0/7.0)*pow(Fs,-1.0/7.0);
      dHb=3.0*pow(Fb/Up,0.6)*pow(Fs,-0.4);
      dH=DMIN(dH,(dHm+dHb));
      /* Limitation due a la hauteur de la couche limite */
      dH=DMIN(dH,0.62*(met.h-H));
      residu=fabs(dH-dHprec)/dH;
      dHprec=dH;
    }
  }
  /* Cas stable */
  else{
    /* Methode iterative pour le calcul de N et Up */
    Nh=freq_flot(met,H);
    for(i=0;i<50 && residu>1.0e-5;i++){
      N=0.5*(Nh+freq_flot(met,H+0.5*dH));
      Np=0.7*N;
      Up=0.5*(Uph+U(H+0.5*dH,met.us,Don.z0d,met.Lmo));
      if(Up<0.05) dH=4.0*pow(Fb/(N*N*N),0.25);
      else{
	if(x<(Up/Np*atan(Fm*Np/Fb)))
	  dHs=2.66*pow(Fb/(N*N*Up)*(Np*Fm/Fb*sin(Np*x/Up)+
				    1.0-cos(Np*x/Up)),1.0/3.0);
	else dHs=2.66*pow(Fb/(N*N*Up),1.0/3.0);
	dHs_inf=2.66*pow(Fb/(N*N*Up),1.0/3.0);
	Ln=Fb/(Up*met.us);
	dHn=1.2*pow(Ln,0.6)*pow(H+1.2*Ln,0.4);
	dHsc=4.0*pow(Fb/(N*N*N),0.25);
	dH=DMIN(dHs,DMIN(dHs_inf,DMIN(dHn,dHsc)));
      }
      if(dH>0.0) residu=fabs(dH-dHprec)/dH;
      else residu=0.0;
      dHprec=dH;
    }
  }
  return dH;
}


/*---------------------------------------------*/
