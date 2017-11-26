/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Hauteur-CLA.c --> Calcul de la hauteur de   */
/*              la couche limite ou de melange */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"


/*---------------------------------------------*/


DBL Haut_CLA(Meteo met,Date date)
/*---------------------------------*/
/* Calcul de la hauteur de melange */
/*---------------------------------*/
{
  DBL h;

  /* Valeur fournie en entree */
  /* On ne change rien */
  if(Don.type_haut==0){
    h=met.h;
  }
  /* Calcul par le modele */
  else if(Don.type_haut==1){
    /* Cas stable */
    if(met.Fo<=0.0){
      h=Haut_CLA_stable(met.us,met.Lmo);
    }
    /* Cas instable */
    else{
      h=Haut_CLA_instable(met,date);
    }
  }
  return DMIN(4000.0,DMAX(50.0,h));
}


/*---------------------------------------------*/


DBL Haut_CLA_stable(DBL us,DBL Lmo)
/*-------------------------------------------*/
/* Calcul de la hauteur de melange en stable */
/*                                           */
/* voir Nieuwstadt (1981)                    */
/*-------------------------------------------*/
{
  /* Cas stable */
  if(Lmo<10000)
    return (-1.0+sqrt(1.0+2.28*us/Lmo/Don.f))*Lmo/3.8;
  /* Cas neutre */
  else
    return 0.3*us/Don.f;
}


/*---------------------------------------------*/


DBL Haut_CLA_instable(Meteo met,Date date)
/*---------------------------------------------*/
/* Calcul de la hauteur de melange en instable */
/*---------------------------------------------*/
{
  DBL haut,haut_prec,heure,heure_prec,heure_inv;
  DBL elev_sol_h,Fo_h,us_h,Ug_h,Lmo_h,thetas_h;

  /* Initialisation de Ug_h */
  if(Don.type_mesure_vent==0) Ug_h=met.Ug;

  /* Calcul de l'heure de lever du soleil */
  heure_inv=inversion_flux(met,date.j);

  /* Cas de donnees sequentielles */
  if(Don.sequent==1){
    if(date.h<heure_inv+1.0)
      haut=Integre_hCLA(0.0,(date.h-heure_inv)*3600.0,met.rho,met.Fo,met.us,met.To);
    else
      haut=Integre_hCLA(met.h_prec,3600.0,met.rho,met.Fo,met.us,met.To);
  }

  /* Cas de donnees non sequentielles */
  else if(Don.sequent==0){
    heure_prec=heure_inv;
    haut_prec=0.0;
    for(heure=ceil(heure_inv);heure<=date.h;heure+=1.0){
      Us_Lmo(date.j,heure,met.Cld,met.rho,met.To,met.Uext,&Ug_h,
	     &elev_sol_h,&Fo_h,&us_h,&Lmo_h,&thetas_h);
      haut=Integre_hCLA(haut_prec,(heure-heure_prec)*3600.0,met.rho,Fo_h,us_h,met.To);
      heure_prec=heure;
      haut_prec=haut;
    }
  }

  /* Renvoie le max du calcul instable et du calcul neutre */
  return DMAX(haut,Haut_CLA_stable(met.us,99999.0));
}


/*---------------------------------------------*/


DBL Integre_hCLA(DBL h0,DBL dt,DBL rho,DBL Fo,DBL us,DBL To)
/*------------------------------------------*/
/* Integration de l'equation differentielle */
/* pour le calcul de h                      */
/*                                          */
/* dh   a1   a2                             */
/* -- = -- + ---                            */
/* dt   h    h*h                            */
/*                                          */
/* voir Batchvarova et Gryning (1991)       */
/*------------------------------------------*/
{
  int i;
  DBL a1,a2,x,f,fmid,hmin=0.0,hmax=4000.0,hmid;

  a1=1.4*Fo/Cp/rho/Don.gradthetaext;
  a2=5.0*us*us*us*To/Don.gradthetaext/g;
  x=a2/a1;

  /* Methode de dichotomie */
  f=Fonc_CLA(h0,hmin,dt,x,a1);
  fmid=Fonc_CLA(h0,hmax,dt,x,a1);
  if(f*fmid>=0) printf("Probleme\n");
  for(i=0;i<15;i++){
    hmid=hmin+0.5*(hmax-hmin);
    fmid=Fonc_CLA(h0,hmid,dt,x,a1);
    if(f*fmid>=0){
      hmin=hmid;
      f=fmid;
    }
    else hmax=hmid;
/*     printf("%d %f %f %f\n",i,hmid,f,fmid); */
  }
  return hmid;
}


/*---------------------------------------------*/


DBL Fonc_CLA(DBL h0,DBL h,DBL dt,DBL x,DBL a1)
{
  return (DSQR(h/x-1.0)+2.0*log(h/x+1.0)-1.0)-
    (DSQR(h0/x-1.0)+2.0*log(h0/x+1.0)-1.0)-2.0*a1*dt/DSQR(x);
}


/*---------------------------------------------*/


DBL lever_soleil(DBL jour)
/*--------------------------------------*/
/* Calcul de l'heure de lever du soleil */
/* du jour                              */
/*--------------------------------------*/
{
  DBL decl_sol;

  decl_sol=23.45*sin((jour+284.0)/365.0*2.0*pi);
  return 12.0-24.0/(2.0*pi)*
    acos(-sin(Don.Lat*pi/180.0)*sin(decl_sol*pi/180.0)/
	 (cos(Don.Lat*pi/180.0)*cos(decl_sol*pi/180.0)));
}


/*---------------------------------------------*/


DBL inversion_flux(Meteo met,DBL jour)
/*---------------------------------------*/
/* Calcul de l'heure d'inversion du flux */
/* de chaleur sensible (H > 0)           */
/*                                       */
/* Methode de dichotomie avec une        */
/* precision minimale de 40 sec.         */
/* (12*3600/2^10)                        */
/*---------------------------------------*/
{
  int i;
  DBL hmid,hmin,hmax,f,fmid;

  hmin=0.0;
  hmax=12.0;
  f=Flux_jour(elevation_solaire(jour,hmin),met.Cld,met.To);
  fmid=Flux_jour(elevation_solaire(jour,hmax),met.Cld,met.To);
  if(f*fmid>=0){
    Erreur("Appel au calcul de l'heure d'inversion du flux\npour une journee continuement stable",0);
  }
  for(i=0;i<10;i++){
    hmid=hmin+0.5*(hmax-hmin);
    fmid=Flux_jour(elevation_solaire(jour,hmid),met.Cld,met.To);
    if(f*fmid>=0){
      hmin=hmid;
      f=fmid;
    }
    else hmax=hmid;
  }
  return hmid;
  
}


/*---------------------------------------------*/
