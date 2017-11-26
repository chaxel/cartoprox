/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Intersection.c --> Modele d'echange dans    */
/*                    les intersections        */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"


/*---------------------------------------------*/


void Intersect(Noeud *Nd,Rue *R)
/*-------------------------------*/
/* Determination des flux dans   */
/*       l'intersection          */
/*-------------------------------*/
{
  int i,j,k,n,N=30,affich;
  DBL **Flux,Uix,Uiy,thetac,cste,poids;

  printf("\n-- Flux aux intersections --\n");

  /* Initialisation */
  /*----------------*/
  Flux=AllocMtrx(Nmax_Rue_Inters,Nmax_Rue_Inters);
  for(i=0;i<Don.N_Rue;i++){
    R[i].Pconv_h=0.0;
    R[i].Pconv_h_evac=0.0;
  }
  for(i=0;i<Don.N_Noeud;i++){
    if(Nd[i].Categ==0){
      for(j=0;j<Nd[i].N_Rue+1;j++){
	for(k=0;k<Nd[i].N_Rue+1;k++){
	  Nd[i].Flux[j][k]=0.0;
	}
      }
    }
  }
  cste=1.0/(Met.sigmatheta*sqrt(2.0*pi));

  /* Desactivation de l'affichage ecran */
  /* pour eviter les affichages repetes */
  /* lors de l'appel a Solv_debit       */
  affich=Don.affich;
  Don.affich=-1;

  /* Boucle sur les angles */
  /*-----------------------*/
  for(n=-N;n<=N;n++){
    /* Calcul de l'angle courant */
    thetac=Met.Angle+n*3.0*Met.sigmatheta/N;
    Uix=-sin(thetac*pi/180.0);
    Uiy=-cos(thetac*pi/180.0);
    /* Poids gaussien, corrige par la valeur de  */
    /* l'integrale discrete limitee a +/-3 sigma */
    poids=exp(-0.5*DSQR((thetac-Met.Angle)/Met.sigmatheta))*
      cste*3.0*Met.sigmatheta/N/0.997721;

    /* Calcul de l'ecoulement */
    Solv_Debit(Nd,R,Uix,Uiy);
    for(i=0;i<Don.N_Rue;i++){
      if(R[i].Categ==0){
	R[i].Pconv_h+=R[i].H*R[i].W*R[i].Umoy*poids;
	R[i].Pconv_h_evac+=R[i].H*R[i].W*fabs(R[i].Umoy*poids);
      }
    }

    /* Determination des flux */
    for(i=0;i<Don.N_Noeud;i++){
      if(Nd[i].Categ==0){
	Flux_inters(Nd[i],i,R,Flux);
	for(j=0;j<Nd[i].N_Rue+1;j++){
	  for(k=0;k<Nd[i].N_Rue+1;k++){
	    Nd[i].Flux[j][k]+=Flux[j][k]*poids;
	  }
	}
      }
    }
    /*------------------------*/
  }
  /* Calcul du flux convectif dans chaque rue */
  for(i=0;i<Don.N_Rue;i++){
    if(R[i].Categ==0){
      R[i].Pconv_h=fabs(R[i].Pconv_h);
    }
  }

  /* Reactivation de l'affichage ecran */
  Don.affich=affich;

  /* Affichage des matrices de flux */
  if(Don.affich>=2){
    for(i=0;i<Don.N_Noeud;i++){
      if(Nd[i].Categ==0){
	printf("Noeud numero %d\n",i);
	for(k=0;k<Nd[i].N_Rue;k++){
	  printf("Debit rue %d = %f %f\n",Nd[i].Num[k],
		 R[Nd[i].Num[k]].Pconv_h,R[Nd[i].Num[k]].Pconv_h_evac);
	}
	printf("\t");
	for(k=0;k<Nd[i].N_Rue;k++){
	  printf("%d\t",Nd[i].Num[k]);
	}
	printf("ext\n");
	for(j=0;j<Nd[i].N_Rue;j++){
	  printf("%d\t",Nd[i].Num[j]);
	  for(k=0;k<Nd[i].N_Rue+1;k++){
	    printf("%.3f\t",Nd[i].Flux[j][k]);
	  }
	  printf("\n");
	}
	printf("ext\t");
	for(k=0;k<Nd[i].N_Rue+1;k++){
	  printf("%.3f\t",Nd[i].Flux[Nd[i].N_Rue][k]);
	}
	printf("\n");
      }
    }
  }

  LibereMtrx(Flux,Nmax_Rue_Inters,Nmax_Rue_Inters);

}


/*---------------------------------------------*/


void Flux_inters(Noeud Nd,int i,Rue *R,DBL **Flux)
/*-------------------------------*/
/* Determination des flux dans   */
/* l'intersection numero i       */
/* Flux[i][j] = flux d'air de la */
/* rue i vers la rue j (en m3/s) */
/*-------------------------------*/
{
  int j,k,*Nume,*Nums,ie,is,Ne,Ns,num;
  DBL *Qe,*Qs,*thetae,*thetas,theta,Q;

  /*------------------------------------*/
  /* Comptage des rues situees en amont */
  /* et en aval de l'intersection       */
  Ne=0;
  Ns=0;
  for(j=0;j<Nd.N_Rue;j++){
    num=Nd.Num[j];
    /* Rues situees en amont */
    if(i==R[num].ndsort){
      Ne+=1;
    }
    /* Rues situees en aval */
    if(i==R[num].ndent){
      Ns+=1;
    }
  }
  /*-------------------------------*/
  /* Allocation */
  Nume=calloc(Ne,sizeof(int));
  Nums=calloc(Ns,sizeof(int));
  Qe=calloc(Ne,sizeof(DBL));
  Qs=calloc(Ns,sizeof(DBL));
  thetae=calloc(Ne,sizeof(DBL));
  thetas=calloc(Ns,sizeof(DBL));
  /*-------------------------------*/
  /* Determination de l'angle */
  ie=0;
  is=0;
  for(j=0;j<Nd.N_Rue;j++){
    num=Nd.Num[j];
    /* Rues situees en amont */
    if(i==R[num].ndsort){
      if(i==R[num].Deb) thetae[ie]=atan2(R[num].iy,R[num].ix);
      else thetae[ie]=atan2(-R[num].iy,-R[num].ix);
      Nume[ie]=j;
      Qe[ie]=R[num].H*R[num].W*fabs(R[num].Umoy);
      ie+=1;
    }
    /* Rues situees en aval */
    if(i==R[num].ndent){
      if(i==R[num].Deb) thetas[is]=atan2(R[num].iy,R[num].ix);
      else thetas[is]=atan2(-R[num].iy,-R[num].ix);
      Nums[is]=j;
      Qs[is]=R[num].H*R[num].W*fabs(R[num].Umoy);
      is+=1;
    }
  }
  /*-------------------------------*/
  /* Classement des rues en amont  */
  /* les rues sont ordonnees de la gauche vers la droite */
  /* lorsque l'on regarde dans la direction ou va le vent */
  for(ie=1;ie<Ne;ie++){
    num=Nume[ie];
    Q=Qe[ie];
    theta=thetae[ie];
    k=ie-1;
    /* Classement des angles dans l'ordre croissant */
    /* Tant que thetae[k] a gauche de theta ... */
    while(k>=0 && ((thetae[k]>theta && thetae[k]<(theta+pi)) || thetae[k]<(theta-pi))){
      Nume[k+1]=Nume[k];
      Qe[k+1]=Qe[k];
      thetae[k+1]=thetae[k];
      k--;
    }
    Nume[k+1]=num;
    Qe[k+1]=Q;
    thetae[k+1]=theta;
  }
  /* Classement des rues en aval */
  /* les rues sont ordonnees de la gauche vers la droite */
  /* lorsque l'on regarde dans la direction ou va le vent */
  for(is=1;is<Ns;is++){
    num=Nums[is];
    Q=Qs[is];
    theta=thetas[is];
    k=is-1;
    /* Classement des angles dans l'ordre decroissant */
    /* Tant que thetas[k] a droite de theta */
    while(k>=0 && ((thetas[k]>(theta-pi) && thetas[k]<theta) || thetas[k]>(theta+pi))){
      Nums[k+1]=Nums[k];
      Qs[k+1]=Qs[k];
      thetas[k+1]=thetas[k];
      k--;
    }
    Nums[k+1]=num;
    Qs[k+1]=Q;
    thetas[k+1]=theta;
  }
  /*-----------------------------------*/
  /* Determination des flux entre rues */
  for(j=0;j<Nmax_Rue_Inters;j++){
    for(k=0;k<Nmax_Rue_Inters;k++){
      Flux[j][k]=0.0;
    }      
  }
  /* Prise en compte du flux vertival      */
  /* On soustrait la part du flux vertical */
  /* correspondant a la rue courante, en   */
  /* proportion du debit de cette rue      */
  if(Nd.Pconv_v>0.0){
    for(ie=0;ie<Ne;ie++){
      /* Flux de la rue ie vers l'exterieur */
      Flux[Nume[ie]][Nd.N_Rue]=Nd.Pconv_v*Qe[ie]/Nd.Pconv_entr;
      /* Soustraction du flux sortant a la rue ie */
      Qe[ie]-=Nd.Pconv_v*Qe[ie]/Nd.Pconv_entr;
    }
  }
  else if(Nd.Pconv_v<0.0){
    for(is=0;is<Ns;is++){
      /* Flux de l'exterieur vers la rue is */
      Flux[Nd.N_Rue][Nums[is]]=-Nd.Pconv_v*Qs[is]/Nd.Pconv_sort;
      /* Soustraction du flux entrant dans la rue is */
      Qs[is]+=Nd.Pconv_v*Qs[is]/Nd.Pconv_sort;
    }
  }

  /* Repartition horizontale des flux */
  ie=0;
  is=0;
  while(ie<Ne && is<Ns){
    if(Qe[ie]<=Qs[is]){
      Flux[Nume[ie]][Nums[is]]=Qe[ie];
      Qs[is]-=Qe[ie];
      Qe[ie]=0.0;
    }
    else if(Qe[ie]>Qs[is]){
      Flux[Nume[ie]][Nums[is]]=Qs[is];
      Qe[ie]-=Qs[is];
      Qs[is]=0.0;
    }
    if(fabs(Qe[ie])<1.0e-6) ie+=1;
    if(fabs(Qs[is])<1.0e-6) is+=1;
  }
  /*------------*/
  /* Liberation */
  free(Nume);
  free(Nums);
  free(Qe);
  free(Qs);
  free(thetae);
  free(thetas);
  /*------------*/
}

/*---------------------------------------------*/
