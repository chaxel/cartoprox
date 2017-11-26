/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Solv-Debit.c --> Calcul de vitesse par rue  */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"


/*---------------------------------------------*/


void Solv_Debit(Noeud *Nd,Rue *R,DBL Uix,DBL Uiy)
/*-------------------------------*/
/* Determination des flux d'air  */
/* dans chaque rue du reseau     */
/* (sans gradient de pression    */
/* mais avec flux vertical aux   */
/* noeuds)                       */
/*-------------------------------*/
{
  int i,j,k;

  if(Don.affich>=0) printf("\n-- Determination des debits --\n");

  /*-----------------------------*/
  /* Determination de la vitesse */
  /*-----------------------------*/
  for(i=0;i<Don.N_Rue;i++){
    if(R[i].Categ==0){
      /* Vitesse de frottement locale */
      R[i].us_para=Met.us*(R[i].ix*Uix+
			   R[i].iy*Uiy);
      R[i].us_perp=Met.us*(R[i].ix*Uiy-
			   R[i].iy*Uix);

      /* Vitesse au niveau des toits */
      R[i].Uh=R[i].Kh*R[i].us_para;

      /* Vitesse moyenne */
      R[i].Umoy=R[i].Kmoy*R[i].Uh;

      /* Determination des noeuds d'entree */
      /* et de sortie (pour la rue)        */
      /* Rappel : la rue est orientee du   */
      /* noeud debut vers le noeud fin     */
      if(R[i].Umoy>1.0e-6){
	R[i].ndent=R[i].Deb;
	R[i].ndsort=R[i].Fin;
      }
      else if(R[i].Umoy<-1.0e-6){
	R[i].ndent=R[i].Fin;
	R[i].ndsort=R[i].Deb;
      }
      else{
	R[i].ndent=-1;
	R[i].ndsort=-1;
      }
    }
  }

  /*-------------------------------------------*/
  /* Determination du flux vertical aux noeuds */
  /* par difference entre la somme des flux    */
  /* horizontaux entrants et la somme des flux */
  /* horizontaux sortants                      */
  /*-------------------------------------------*/
  for(i=0;i<Don.N_Noeud;i++){
    /* Teste si c'est une rue (et non une route) */
    if(Nd[i].Categ==0){
      Nd[i].Pconv_v=0.0;
      Nd[i].Pconv_entr=0.0;
      Nd[i].Pconv_sort=0.0;
      /* Teste si le noeud est une intersection */
	for(j=0;j<Nd[i].N_Rue;j++){
	  k=Nd[i].Num[j];
	  if(R[k].Deb==i)
	    Nd[i].Pconv_v-=R[k].H*R[k].W*R[k].Umoy;
	  else if(R[k].Fin==i)
	    Nd[i].Pconv_v+=R[k].H*R[k].W*R[k].Umoy;
	  /* Calcul du flux convectif horizontal entrant */
	  /* et sortant dans l'intersection              */
	  if(R[k].ndsort==i)
	    Nd[i].Pconv_entr+=R[k].H*R[k].W*fabs(R[k].Umoy);
	  else if(R[k].ndent==i)
	    Nd[i].Pconv_sort+=R[k].H*R[k].W*fabs(R[k].Umoy);
	}
    }
  }

  /*---------------------------------*/
  /* Impression ecran de la solution */
  /*---------------------------------*/
  if(Don.affich>=1){
    printf("Solution :\n");
    for(i=0;i<Don.N_Rue;i++){
      if(R[i].Categ==0){
	printf("Rue %d : Vitesse=%f \n",i,
	       R[i].Umoy);
      }
    }
    printf("\n");
  }

}


/*---------------------------------------------*/
