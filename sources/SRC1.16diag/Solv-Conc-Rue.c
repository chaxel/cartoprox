/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Solv-Conc-Rue.c --> Bilan de concentration  */
/*                     dans chaque rue         */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"


/*---------------------------------------------*/


void Solv_Rue(Noeud *Nd,Rue *R)
/*-------------------------------*/
/*  Calcul de la concentration   */
/*    interne dans chaque rue    */
/*           a*C+b=0             */
/*       flux > 0 si entrant     */
/*-------------------------------*/
{
  int i;
  DBL a,b;

  for(i=0;i<Don.N_Rue;i++){
    if(R[i].Categ==0){
      a=0.0;
      b=0.0;
      /* Terme source */
      b+=R[i].Qsrce;
      
      /* Flux diffusif vertical */
      a-=R[i].Pdiff_v;
      b+=R[i].Pdiff_v*R[i].Cext;
      
      /* Flux convectif horizontal */
      a-=R[i].Pconv_h_evac;
      b+=R[i].Qconv_h;

      /* Lessivage */
      if(Don.precipit==1) a-=R[i].P_lessiv;

      /* Sedimentation des particules */
      if(Don.Type_Pol==2){ 
	a-=R[i].L*R[i].W*Don.Vit_T;
	b+=R[i].L*R[i].W*Don.Vit_T*R[i].Cext;
      }
      /* Resolution de l'equation */
      R[i].Cint=-b/a;

      /* Flux diffusif vertical */
      R[i].Qdiff_v=R[i].Pdiff_v*(R[i].Cint-R[i].Cext);
    }
  }

}


/*---------------------------------------------*/
