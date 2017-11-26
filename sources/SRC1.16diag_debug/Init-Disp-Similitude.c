/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Init-Disp-Similitude.c --> Initialisations  */
/*                des parametres de dispersion */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"


/*---------------------------------------------*/


void Sigma_simil(Meteo met)
/*---------------------------------------*/
/* Calcul des coefficients de dispersion */
/* au niveau z=Hmoy par la theorie de    */
/* similitude                            */
/*---------------------------------------*/
{
  int i;
  DBL dx=1.0,Zm,Up,sigma_v,sigma_w,sigma_wic,t,x;
  
  /* Initialisations */
  t=0.0;
  x=0.0;
  Don.sigma_y[0]=0.0;
  Don.sigma_z[0]=0.0;
  Don.T_advect[0]=0.0;
  Zm=Don.Hmoy;
  Up=U(Don.Hmoy,met.us,Don.z0d,met.Lmo);
  Don.Pz_sol[0]=0.0;
  
  /* Calcul des parametres de dispersion */
  /* par intervalle de dx=1 metre        */
  for(i=1;i<N_Point_Disp;i++){
    x=(DBL) i;
    if(Up>1.0e-2)
      t+=dx/Up;
    else
      t+=1.0;
    Don.T_advect[i]=t;
    /* Calcul des ecarts-types */
    sigmavwyz(met,Zm,t,x,&sigma_v,&sigma_w,&sigma_wic,&(Don.sigma_y[i]),&(Don.sigma_z[i]));
    /* Correction pour les particules */
    Don.sigma_y[i]*=Don.Traj_Cross;
    Don.sigma_z[i]*=Don.Traj_Cross;
    
    /* Calcul des parametres de la pdf en instable */
    if((met.h/met.Lmo)<-0.3)
      Coeff_pw(sigma_w,sigma_wic,Zm,t,&(Don.png[i]),met);
    /* Calcul de la hauteur moyenne du panache */
    Zm=ZM(Don.png[i],met,Don.sigma_z[i],Don.Hmoy-Don.Vit_T*t);
    /* Calcul de la vitesse moyenne du panache */
    Up=UP(Don.png[i],met,Don.sigma_z[i],Don.Hmoy-Don.Vit_T*t);
    /* Calcul de la fonction de concentration */
    Don.Pz_sol[i]=pdfz(Don.png[i],met,Don.sigma_z[i],Don.Hmoy-Don.Vit_T*t,Don.Hmoy)/Up;
  }
  
}


/*---------------------------------------------*/


DBL ZM(ParamNonGauss png,Meteo met,DBL sigma_z,DBL zs)
/*-----------------------------------------*/
/* Calcul de la hauteur moyenne du panache */
/*-----------------------------------------*/
{
  int Nz=10;
  DBL dz=(3.0*sigma_z)/Nz;
  DBL z,c,Hmin,Hmax,numer=0.0,denom=0.0;

  if(fabs(sigma_z)<1.0e-6){
    return zs;
  }
  else{
    Hmax=DMIN(zs+3.0*sigma_z,met.h);
    Hmin=DMAX(zs-3.0*sigma_z,Don.Hmoy);
    for(z=Hmin;z<Hmax;z+=dz){
      c=pdfz(png,met,sigma_z,zs,z);
      numer+=z*c;
      denom+=c;
    }
    if(denom==0.0) return Don.Hmoy;
    else return numer/denom;
  }
}


/*---------------------------------------------*/


DBL UP(ParamNonGauss png,Meteo met,DBL sigma_z,DBL zs)
/*-----------------------------------------*/
/* Calcul de la vitesse moyenne du panache */
/*-----------------------------------------*/
{
  int Nz=10;
  DBL dz=(3.0*sigma_z)/Nz;
  DBL z,c,Hmin,Hmax,numer=0.0,denom=0.0;

  if(fabs(sigma_z)<1.0e-6){
    return U(zs-Don.d,met.us,Don.z0d,met.Lmo);
  }
  else{
    Hmax=DMIN(zs+3.0*sigma_z,met.h);
    Hmin=DMAX(zs-3.0*sigma_z,Don.Hmoy);
    for(z=Hmin;z<Hmax;z+=dz){
      c=pdfz(png,met,sigma_z,zs,z);
      numer+=U(z-Don.d,met.us,Don.z0d,met.Lmo)*c;
      denom+=c;
    }
    if(denom==0.0) return U(Don.Hmoy,met.us,Don.z0d,met.Lmo);
    return numer/denom;
  }
}


/*---------------------------------------------*/


DBL pdfz(ParamNonGauss png,Meteo met,DBL sigmaz,DBL zs,DBL z)
/*--------------------------------------------*/
/* Calcul de la fonction densite              */
/* de probabilite dans le cas general         */
/* avec reflexion par rapport au sol (z=Hmoy) */
/* et reflexion au sommet de la CLA (z=h)     */
/*--------------------------------------------*/
{
  /* Diffusion uniforme ----------> pdf gaussienne */
  /* Classes de Pasquill ---------> pdf gaussienne */
  /* Similitude stable ou neutre -> pdf gaussienne */
  if((met.h/met.Lmo)>=-0.3 || Don.type_disp==0 || Don.type_disp==1){
    /* Si zs au-dessus de la CLA -> pas de dispersion */
    if(zs>met.h) return 0.0;
    /* Si zs en-dessous de Hmoy (en raison de la sedimentation) */
    /* -> on remplace la reflexion au sol par un facteur 2      */
    else if(zs<Don.Hmoy) 
      return (2.0*exp(-DSQR(z-zs)/(2.0*DSQR(sigmaz)))+
	      exp(-DSQR(z-(2.0*met.h-zs))/(2.0*DSQR(sigmaz))))/(Sqrt2Pi*sigmaz);
    /* Sinon, 2 reflexions */
    else
      return (exp(-DSQR(z-zs)/(2.0*DSQR(sigmaz)))+
	      exp(-DSQR(z-(2.0*Don.Hmoy-zs))/(2.0*DSQR(sigmaz)))+
	      exp(-DSQR(z-(2.0*met.h-zs))/(2.0*DSQR(sigmaz))))/(Sqrt2Pi*sigmaz);
  }
  /* Similitude instable -> pdf non gaussienne */
  else{
    /* Si zs au-dessus de la CLA -> pas de dispersion */
    if(zs>met.h) return 0.0;
    /* Si zs en-dessous de Hmoy (en raison de la sedimentation) */
    /* -> on remplace la reflexion au sol par un facteur 2      */
    else if(zs<Don.Hmoy) 
      return (2.0*pw(png,z,zs)+pw(png,-z,-(2.0*met.h-zs)))/(Sqrt2Pi);
    /* Sinon, 2 reflexions */
    else
      return (pw(png,z,zs)+pw(png,-z,-(2.0*Don.Hmoy-zs))+pw(png,-z,-(2.0*met.h-zs)))/(Sqrt2Pi);
  }
}


/*---------------------------------------------*/


DBL pw(ParamNonGauss png,DBL z,DBL zs)
/*---------------------------------*/
/* Fonction densite de probabilite */
/* non gaussienne                  */
/*---------------------------------*/
{
  if((z-zs)-png.wchapt<=0){
    return png.am*exp(-DSQR(z-zs-png.wchapt)/(2.0*DSQR(png.sigmazm)))/png.sigmazm;
  }
  else{
    return png.ap*exp(-DSQR(z-zs-png.wchapt)/(2.0*DSQR(png.sigmazp)))/png.sigmazp;
  }
}


/*---------------------------------------------*/


void Coeff_pw(DBL sigmaw,DBL sigmaw_ic,DBL zm,DBL t,
	      ParamNonGauss *png,Meteo met)
/*---------------------------------------*/
/* Calcul des coefficients de la pdf     */
/* non gaussienne en atmosphere instable */
/*---------------------------------------*/
{
  DBL k,sigmawm,sigmawp,tl;

  k=pow(1.0+(1.0/4.0-3*pi/32.0)*pow(sigmaw_ic/sigmaw,2.0),-0.5);
  sigmawp=sigmaw/k+sqrt(pi/32.0)*sigmaw_ic;
  sigmawm=sigmaw/k-sqrt(pi/32.0)*sigmaw_ic;

  /* Coefficient de ponderation gaussienne */
  png->ap=k*sigmawp/sigmaw;
  png->am=k*sigmawm/sigmaw;

  /* Mode de la pfd */
  png->wchapt=-sigmaw_ic/2.0*t;

  /* Ecart-types verticaux */
  tl=TL(met,sigmaw,zm);
  png->sigmazp=sigmawp*t*pow(1.0+t/(2.0*tl),-0.5);
  png->sigmazm=sigmawm*t*pow(1.0+t/(2.0*tl),-0.5);
}


/*---------------------------------------------*/



