/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Init.c --> Initialisations generales        */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"


/*---------------------------------------------*/


void Init(Noeud *Nd,Rue *R)
/*---------------------------*/
/* Initialisation des champs */
/*---------------------------*/
{
  
  printf("\n-- Initialisations --\n\n");

  /* Initialisation des constantes */
  /*-------------------------------*/
  Don.f=omega*2.0*sin(Don.Lat*pi/180.0);
  Don.gradthetaext=Don.gradText+GammaAdiab;

  /* Tests de compatibilite */
  /*------------------------*/
  if(Don.zext<=Don.dext) Erreur("Vent mesure a une hauteur < d",0);

  /* Initialisation des noeuds */
  /*---------------------------*/
  Init_noeud(Nd,R);

  /* Initialisation des rues */
  /*-------------------------*/
  Init_rue(Nd,R);

  /* Initialisation de la grille */
  /*-----------------------------*/
  Init_grd(Nd,R);
}


/*---------------------------------------------*/


void Init_noeud(Noeud *Nd,Rue *R)
/*---------------------------*/
/* Initialisation des noeuds */
/*---------------------------*/
{
  int i,j,k,nr;
  DBL numer,denom;

  for(i=0;i<Don.N_Noeud;i++){
    /* Comptage des rues (pas des routes) arrivant au noeud i */
    nr=0;
    for(j=0;j<Don.N_Rue;j++){
      if((R[j].Deb==i || R[j].Fin==i) &&
	 R[j].Categ==0)
	nr+=1;
    }
    Nd[i].N_Rue=nr;

    /* Determine le type de noeud et le type de voie */
    /* Type = 0 --> frontiere */
    /* Type = 1 --> interieur domaine */
    /* Categ = 0 --> rue */
    /* Categ = 1 --> route */
    if(Nd[i].N_Rue==0) Nd[i].Categ=1;
    else if(Nd[i].N_Rue==1){
      Nd[i].type=0;
      Nd[i].Categ=0;
    }
    else{
      Nd[i].type=1;
      Nd[i].Categ=0;
    }

    /* Calcul du rayon de l'intersection */
    /* = max des demi-largeurs des rues  */
    Nd[i].Rayon=0.0;
    for(j=0;j<Don.N_Rue;j++){
      /* Rue appartenant au bati     */
      /* -> recherche parmi les rues */
      if((R[j].Deb==i || R[j].Fin==i) &&
	 Nd[i].Categ==0 && R[j].Categ==0)
	Nd[i].Rayon=DMAX(Nd[i].Rayon,0.5*R[j].W);
      /* Rue n'appartenant pas au bati */
      /* -> recherche parmi les routes */
      else if((R[j].Deb==i || R[j].Fin==i) &&
	      Nd[i].Categ==1 && R[j].Categ==1)
	Nd[i].Rayon=DMAX(Nd[i].Rayon,0.5*R[j].W);
    }

    /* Calcul de la hauteur moyenne de l'intersection */
    /* ponderee par les largeurs des rues, pour les noeuds du bati  */
    numer=0.0;
    denom=0.0;
    if(Nd[i].Categ==0){
      for(j=0;j<Don.N_Rue;j++){
	/* Rue appartenant au bati     */
	/* -> recherche parmi les rues */
	if((R[j].Deb==i || R[j].Fin==i) && R[j].Categ==0)
	  numer+=R[j].W*R[j].H;
	  denom+=R[j].W;
      }
      Nd[i].H=numer/denom;
    }

    /* Allocation de la matrice des flux */
    if(Nd[i].Categ==0){
      Nd[i].Flux=AllocMtrx(Nd[i].N_Rue+1,Nd[i].N_Rue+1);
      for(j=0;j<Nd[i].N_Rue+1;j++){
	for(k=0;k<Nd[i].N_Rue+1;k++){
	  Nd[i].Flux[j][k]=0.0;
	}
      }
    }

    /* Stocke le numero des rues arrivant au noeud */
    if(Nd[i].N_Rue>0){
      Nd[i].Num=calloc(Nd[i].N_Rue,sizeof(DBL));
      k=0;
      for(j=0;j<Don.N_Rue;j++){
	if((R[j].Deb==i || R[j].Fin==i) &&
	   R[j].Categ==0){
	  Nd[i].Num[k]=j;
	  k++;
	}
      }
    }
  }
}


/*---------------------------------------------*/


void Init_rue(Noeud *Nd,Rue *R)
/*-------------------------*/
/* Initialisation des rues */
/*-------------------------*/
{
  int i;
  DBL C,D,alpha,beta;

  /*-------------- REMARQUE -------------*/
  /* Par convention, la rue est orientee */
  /* du noeud de debut vers le noeud de  */
  /* fin et la vitesse est comptee       */
  /* positivement dans ce sens           */
  /*-------------------------------------*/
  for(i=0;i<Don.N_Rue;i++){
    /* Determination du type de rue */
    /* en fonction du type de noeud */
    /* type = 1 --> entree */
    /* type = 2 --> sortie */
    /* type = 0 --> autre  */
    if(Nd[R[i].Deb].type==0)
      R[i].type=1;
    else if(Nd[R[i].Fin].type==0)
      R[i].type=2;
    else R[i].type=0;

    /* Centre de la rue */
    R[i].x=0.5*(Nd[R[i].Fin].x+
		Nd[R[i].Deb].x);
    R[i].y=0.5*(Nd[R[i].Fin].y+
		Nd[R[i].Deb].y);

    /* Longueur de la rue */
    R[i].L=sqrt(DSQR(Nd[R[i].Fin].x-
		     Nd[R[i].Deb].x)+
		DSQR(Nd[R[i].Fin].y-
		     Nd[R[i].Deb].y));

    /* Vecteur directeur de la rue */
    R[i].ix=(Nd[R[i].Fin].x-
	     Nd[R[i].Deb].x)/
      R[i].L;
    R[i].iy=(Nd[R[i].Fin].y-
	     Nd[R[i].Deb].y)/
      R[i].L;

    if(R[i].Categ==0){
      /* Constantes pour le calcul de la vitesse */
      /* voir These L. Soulhac (2000) p 167-168  */
      D=DMIN(R[i].H,0.5*R[i].W);
      C=rtnewt(rugo,0.0,2.0,1.0e-6,
	       Don.z0d_bat/D);
      alpha=log(D/Don.z0d_bat);
      beta=exp(C*(D-R[i].H)/sqrt(2.0)/D);
      /* Relation entre Uh et u* */
      R[i].Kh=sqrt(pi/(sqrt(2.0)*C*kappa*kappa)*
		   (Yo(C)-Y1(C)*Jo(C)/J1(C)));
      /* Relation entre Umoy et Uh */
      /* Rue etroite (H > W/2) */
      if(2.0*R[i].H>R[i].W){
	R[i].Kmoy=D*D/R[i].H/R[i].W*
	  ((1.0-C*C/3.0+C*C*C*C/45.0)*2.0*sqrt(2.0)/C*(1-beta)+
	   (2.0*alpha-3.0)/alpha*beta);
      }
      /* Rue large (H < W/2) */
      else{
	R[i].Kmoy=D*D/R[i].H/R[i].W*
	  ((2.0*alpha-3.0)/alpha*beta+(R[i].W/D-2.0)*(alpha-1.0)/alpha);
      }
    }
  }

  /* Ecriture sur fichier du reseau de rues */
  Ecrire_Rue_bln(Nd,R);
  Ecrire_Forme_Rue_bln(Nd,R);
}




/*---------------------------------------------*/


void Init_grd(Noeud *Nd,Rue *R)
/*----------------------------------------*/
/* Initialisation de la grille de travail */
/*----------------------------------------*/
{
  int i,j,k,l,m,ind_i,ind_j;
  int i_min,i_max,j_min,j_max;

  /* Pas du maillage */
  /*-----------------*/
  Don.grd.dx=(Don.grd.xmax-Don.grd.xmin)/((DBL) Don.grd.Nx);
  Don.grd.dy=(Don.grd.ymax-Don.grd.ymin)/((DBL) Don.grd.Ny);

  /* Allocation */
  /*------------*/
  Don.cell=calloc(Don.grd.Nx*Don.grd.Ny,sizeof(Cell));
  for(i=0;i<Don.grd.Nx;i++){
    for(j=0;j<Don.grd.Ny;j++){
      k=Don.grd.Nx*j+i;
      /* Position */
      Don.cell[k].x=Don.grd.xmin+Don.grd.dx*(0.5+(DBL) i);  
      Don.cell[k].y=Don.grd.ymin+Don.grd.dy*(0.5+(DBL) j);  
      /* Nombre de rues et de noeuds par maille */
      Don.cell[k].N_Rue=0;
      Don.cell[k].N_Noeud=0;
    }
  }

  /* Indice des mailles voisines */
  m=(2*Don.N_Infl+1)*(2*Don.N_Infl+1);
  for(k=0;k<Don.grd.Nx*Don.grd.Ny;k++)
    Don.cell[k].Num_Infl=calloc(m,sizeof(int));
  for(i=0;i<Don.grd.Nx;i++){
    for(j=0;j<Don.grd.Ny;j++){
      k=Don.grd.Nx*j+i;
      for(l=0;l<m;l++)
	Don.cell[k].Num_Infl[l]=-1;
      i_min=IMAX(i-Don.N_Infl,0);
      i_max=IMIN(i+Don.N_Infl,Don.grd.Nx-1);
      j_min=IMAX(j-Don.N_Infl,0);
      j_max=IMIN(j+Don.N_Infl,Don.grd.Ny-1);
      l=0;
      for(ind_i=i_min;ind_i<=i_max;ind_i++){
	for(ind_j=j_min;ind_j<=j_max;ind_j++){
	  Don.cell[k].Num_Infl[l]=Don.grd.Nx*ind_j+ind_i;
	  l+=1;
	}
      }
    }
  }
  /* Affectation des rues et des noeuds a chaque maille */
  /*----------------------------------------------------*/
  /* Recherche de la maille d'appartenance de chaque rue */
  /* Les mailles sont comptees avec l'indice Nx*j+i */
  for(i=0;i<Don.N_Rue;i++){
    ind_i=(int) floor((R[i].x-Don.grd.xmin)/Don.grd.dx);
    ind_j=(int) floor((R[i].y-Don.grd.ymin)/Don.grd.dy);
    if(ind_i>=0 && ind_i<Don.grd.Nx &&
       ind_j>=0 && ind_j<Don.grd.Ny){
      R[i].Cell_i=Don.grd.Nx*ind_j+ind_i;
      Don.cell[Don.grd.Nx*ind_j+ind_i].N_Rue+=1;
    }
    else{
      R[i].Cell_i=-1;
    }
  }
  /* Recherche de la maille d'appartenance de chaque noeud */
  /* Les mailles sont comptees avec l'indice Nx*j+i */
  for(i=0;i<Don.N_Noeud;i++){
    ind_i=(int) floor((Nd[i].x-Don.grd.xmin)/Don.grd.dx);
    ind_j=(int) floor((Nd[i].y-Don.grd.ymin)/Don.grd.dy);
    if(ind_i>=0 && ind_i<Don.grd.Nx &&
       ind_j>=0 && ind_j<Don.grd.Ny){
      Nd[i].Cell_i=Don.grd.Nx*ind_j+ind_i;
      Don.cell[Don.grd.Nx*ind_j+ind_i].N_Noeud+=1;
    }
    else{
      Nd[i].Cell_i=-1;
    }
  }
  /* Allocation des tableaux d'indice */
  for(k=0;k<Don.grd.Nx*Don.grd.Ny;k++){
    if(Don.cell[k].N_Rue>0)
      Don.cell[k].NumRue=calloc(Don.cell[k].N_Rue,sizeof(int));
    if(Don.cell[k].N_Noeud>0)
      Don.cell[k].NumNd=calloc(Don.cell[k].N_Noeud,sizeof(int));
  }
  /* Remplissage du tableau d'indices */
  for(k=0;k<Don.grd.Nx*Don.grd.Ny;k++){
    Don.cell[k].N_Rue=0;
    Don.cell[k].N_Noeud=0;
  }
  for(i=0;i<Don.N_Rue;i++){
    if(R[i].Cell_i!=-1){
      k=R[i].Cell_i;
      Don.cell[k].NumRue[Don.cell[k].N_Rue]=i;
      Don.cell[k].N_Rue+=1;
    }
  }
  for(i=0;i<Don.N_Noeud;i++){
    if(Nd[i].Cell_i!=-1){
      k=Nd[i].Cell_i;
      Don.cell[k].NumNd[Don.cell[k].N_Noeud]=i;
      Don.cell[k].N_Noeud+=1;
    }
  }

  /* Nombre de rues ou de noeuds influants */
  /*---------------------------------------*/
  for(i=0;i<Don.grd.Nx;i++){
    for(j=0;j<Don.grd.Ny;j++){
      k=Don.grd.Nx*j+i;
      Don.cell[k].Nrue_Infl=0;
      Don.cell[k].Nnoeud_Infl=0;
      i_min=IMAX(i-Don.N_Infl,0);
      i_max=IMIN(i+Don.N_Infl,Don.grd.Nx-1);
      j_min=IMAX(j-Don.N_Infl,0);
      j_max=IMIN(j+Don.N_Infl,Don.grd.Ny-1);
      for(ind_i=i_min;ind_i<=i_max;ind_i++){
	for(ind_j=j_min;ind_j<=j_max;ind_j++){
	  l=Don.grd.Nx*ind_j+ind_i;
	  Don.cell[k].Nrue_Infl+=Don.cell[l].N_Rue;
	  Don.cell[k].Nnoeud_Infl+=Don.cell[l].N_Noeud;
	}
      }
    }
  }

}


/*---------------------------------------------*/


void rugo(DBL x,DBL *fn, DBL *df,DBL z0)
/*-------------------------------------*/
/* Fonction definissant la constante C */
/* de facon implicite                  */
/*                                     */
/* voir These L. Soulhac (2000) p 167  */
/*-------------------------------------*/
{
  *fn=2.0/x*exp(pi/2.0*Y1(x)/J1(x)-gamma)-z0;
  *df=-exp(pi/2.0*Y1(x)/J1(x)-gamma)*
    (2.0*DSQR(J1(x))+
     pi*x*(Jo(x)*Y1(x)-J1(x)*Yo(x)))/
    DSQR(x*J1(x));
}


/*---------------------------------------------*/

#define JMAX 20

DBL rtnewt(void (*rugo)(DBL,DBL *,DBL *,DBL),
	   DBL x1,DBL x2,DBL xacc,DBL z0)
/*---------------------------*/
/* Resolution d'equation par */
/* la methode de Newton      */
/*                           */
/* Numerical Recipes in C    */
/*---------------------------*/
{
  int j;
  DBL df,dx,f,rtn;
  
  rtn=0.5*(x1+x2);
  for (j=1;j<=JMAX;j++) {
    (*rugo)(rtn,&f,&df,z0);
    dx=f/df;
    rtn -= dx;
/*     printf("%f %f %f\n",f,df,rtn); */
    if ((x1-rtn)*(rtn-x2) < 0.0)
      nrerror("Jumped out of brackets in rtnewt");
    if (fabs(dx) < xacc) return rtn;
  }
  nrerror("Maximum number of iterations exceeded in rtnewt");
  return 0.0;
}

#undef JMAX

/*---------------------------------------------*/
