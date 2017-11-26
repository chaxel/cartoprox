/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Solv-Conc-Noeud.c --> Bilan de concentrat.  */
/*                       dans chaque noeud     */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"


/*---------------------------------------------*/


void Solv_Noeud(Noeud *Nd,Rue *R)
/*-------------------------------*/
/*  Calcul de la concentration   */
/*  interne dans chaque noeud    */
/*       flux > 0 si entrant     */
/*-------------------------------*/
{
  int i,k,ie,is;
  DBL Pconvtot,Qconvtot,Qtmp;
  Noeud Ndcour;

  for(i=0;i<Don.N_Rue;i++){
    R[i].Qconv_h=0.0;
  }

  for(i=0;i<Don.N_Noeud;i++){
    Ndcour=Nd[i];
    Ndcour.Qconv_v_entr=0.0;
    Ndcour.Qconv_v_sort=0.0;
    Ndcour.Pconv_v_entr=0.0;
    Ndcour.Pconv_v_sort=0.0;
    Pconvtot=0.0;
    Qconvtot=0.0;
    if(Ndcour.Categ==0){
      k=Ndcour.Num[0];
      /* Calcul du flux convectif de polluants */
      /* entrant dans chaque rue et provenant  */
      /* des autres rues ou de l'exterieur     */
      /*---------------------------------------*/
      for(is=0;is<Ndcour.N_Rue;is++){
	/* Flux provenant des autres rues */
	for(ie=0;ie<Ndcour.N_Rue;ie++){
	  Qtmp=R[Ndcour.Num[ie]].Cint*Ndcour.Flux[ie][is];
	  R[Ndcour.Num[is]].Qconv_h+=Qtmp;
	  Qconvtot+=Qtmp;
	  Pconvtot+=Ndcour.Flux[ie][is];
	}
      }
      
      for(is=0;is<Ndcour.N_Rue;is++){
	/* Flux vertical echange avec l'exterieur */
	/* Flux de l'exterieur vers les rues */
	Qtmp=Ndcour.Cext*Ndcour.Flux[Ndcour.N_Rue][is];
	R[Ndcour.Num[is]].Qconv_h+=Qtmp;
	Ndcour.Qconv_v_entr+=Qtmp;
	Ndcour.Pconv_v_entr+=Ndcour.Flux[Ndcour.N_Rue][is];
	Qconvtot+=Qtmp;
	Pconvtot+=Ndcour.Flux[Ndcour.N_Rue][is];
	/* Flux des rues vers l'exterieur */
	Qtmp=R[Ndcour.Num[is]].Cint*Ndcour.Flux[is][Ndcour.N_Rue];
	Ndcour.Qconv_v_sort+=Qtmp;
	Ndcour.Pconv_v_sort+=Ndcour.Flux[is][Ndcour.N_Rue];
	Qconvtot+=Qtmp;
	Pconvtot+=Ndcour.Flux[is][Ndcour.N_Rue];
      }
      
      /* Source dans l'intersection */
      for(is=0;is<Ndcour.N_Rue;is++){
	/* Transport d'une partie de la source */
	/* par le flux provenant des autres rues */
	for(ie=0;ie<Ndcour.N_Rue;ie++){
	  R[Ndcour.Num[is]].Qconv_h+=Ndcour.Qsrce*
	    Ndcour.Flux[ie][is]/Pconvtot;
	}
	/* Transport d'une partie de la source */
	/* par le flux provenant de l'exterieur */
	R[Ndcour.Num[is]].Qconv_h+=Ndcour.Qsrce*
	  Ndcour.Flux[Ndcour.N_Rue][is]/Pconvtot;
      }
      /* Transport d'une partie de la source par */
      /* le flux allant des rues vers l'exterieur */
      Ndcour.Qconv_v_sort+=Ndcour.Qsrce*
	Ndcour.Pconv_v_sort/Pconvtot;
      /* Ajout de la source au flux total */
      Qconvtot+=Ndcour.Qsrce;
      
      /* Calcul du flux convectif vertical */
      Ndcour.Qconv_v=Ndcour.Qconv_v_sort-Ndcour.Qconv_v_entr;
      
      /*------------------------------------*/
      /* Calcul de la concentration moyenne */
      /* dans l'intersection                */
      /*------------------------------------*/
      if(fabs(Pconvtot)>10){
	Ndcour.Cint=Qconvtot/Pconvtot;
      }
      else{
	Ndcour.Cint=Ndcour.Cext;	  
      }
      /*--------------------------------------*/
    }
    Nd[i]=Ndcour;
  }
}


/*---------------------------------------------*/
