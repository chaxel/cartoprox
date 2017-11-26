/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Solv-Conc.c --> Calcul des concentrations   */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"


/*---------------------------------------------*/


void Solv_Conc(Noeud *Nd,Rue *R,Source_Ponct *Srce_Ponct)
/*------------------------------*/
/*  Resolution du systeme des   */
/*  concentrations par methode  */
/*          iterative           */
/*------------------------------*/
{
  int i,j,k,l,n,m,cpt_r,cpt_n,trouve,ind_m,N_Rue,N_Cell;
  DBL sin_phi,Sig0,C_bord_int,C_bord_ext,dx,ecart,sig0;
  DBL *Cnd_fond,*Crue_fond,*Cprec,ConvMax;
  DBL *X,a,**Panache;
  int *Ordre,*Type;
  Cell cell_j,cell_m;

  printf("\n-- Determination des concentrations --\n");

  /*-----------------------*/
  /* Allocation de memoire */
  /*-----------------------*/
  Cnd_fond=calloc(Don.N_Noeud,sizeof(DBL));
  Crue_fond=calloc(Don.N_Rue,sizeof(DBL));
  Cprec=calloc(Don.N_Rue+Don.N_Noeud,sizeof(DBL));

  X=calloc(3*Don.N_Rue+Don.N_Noeud,sizeof(DBL));
  Ordre=calloc(3*Don.N_Rue+Don.N_Noeud,sizeof(int));
  Type=calloc(3*Don.N_Rue+Don.N_Noeud,sizeof(int));

  /*----------------*/
  /* Initialisation */
  /*----------------*/
  printf("\n-- Initialisation --\n");

  /* Calcul des reculs de sources virtuelles */
  /* pour les intersections du reseau */
  sig0=2.0;
  for(i=0;i<Don.N_Noeud;i++){
    if(Nd[i].Categ==0){
      j=0;
      while(0.5*Nd[i].H>Don.sigma_z[j] && j<N_Point_Disp){
	j+=1;
      }
      Nd[i].delta_x_z=(DBL) j;
    }
    else if(Nd[i].Categ==1){
      j=0;
      while(sig0>Don.sigma_z[j] && j<N_Point_Disp){
	j+=1;
      }
      Nd[i].delta_x_z=(DBL) j;
    }
  }

  /* Position des sources ponctuelles */
  /* Projection dans la direction du vent */
  for(i=0;i<Don.N_Srce_ponct;i++){
    Srce_Ponct[i].xv=Met.ix*Srce_Ponct[i].x+Met.iy*Srce_Ponct[i].y;
    Srce_Ponct[i].yv=Met.ix*Srce_Ponct[i].y-Met.iy*Srce_Ponct[i].x;
  }
  /* Position des noeuds */
  for(i=0;i<Don.N_Noeud;i++){
    /* Projection dans la direction du vent */
    Nd[i].xv=Met.ix*Nd[i].x+Met.iy*Nd[i].y;
    Nd[i].yv=Met.ix*Nd[i].y-Met.iy*Nd[i].x;

    /* Parametres de classement */
    X[3*Don.N_Rue+i]=Nd[i].xv;
    Ordre[3*Don.N_Rue+i]=i;
    Type[3*Don.N_Rue+i]=2;
  }
  /* Position des rues */
  for(i=0;i<Don.N_Rue;i++){
    /* Projection dans la direction du vent */
    R[i].xv=Met.ix*R[i].x+Met.iy*R[i].y;
    R[i].yv=Met.ix*R[i].y-Met.iy*R[i].x;
    /* Vecteur directeur de la rue dans un     */
    /* repere parallele a la direction du vent */
    R[i].ixv=Met.ix*R[i].ix+Met.iy*R[i].iy;
    R[i].iyv=Met.ix*R[i].iy-Met.iy*R[i].ix;

    /* Parametres de classement */
    X[i]=DMIN(Nd[R[i].Deb].xv,Nd[R[i].Fin].xv);
    X[Don.N_Rue+i]=R[i].xv;
    X[2*Don.N_Rue+i]=DMAX(Nd[R[i].Deb].xv,
			  Nd[R[i].Fin].xv);
    Ordre[i]=i;
    Ordre[Don.N_Rue+i]=i;
    Ordre[2*Don.N_Rue+i]=i;
    Type[i]=-1;
    Type[Don.N_Rue+i]=0;
    Type[2*Don.N_Rue+i]=1;
  }
  /* Classement de tous les points */
  /* dans l'ordre croissant dans   */
  /* la direction du vent          */
  /* Type = -1 --> noeud amont d'une rue */
  /* Type = 0  --> centre d'une rue      */
  /* Type = 1  --> noeud aval d'une rue  */
  /* Type = 2  --> noeud                 */
  /* Ordre[i] contient le numero de la   */
  /* rue ou du noeud                     */
  for(i=1;i<3*Don.N_Rue+Don.N_Noeud;i++){
    a=X[i];
    k=Ordre[i];
    l=Type[i];
    j=i-1;
    while(j>=0 && X[j]>a){
      X[j+1]=X[j];
      Ordre[j+1]=Ordre[j];
      Type[j+1]=Type[j];
      j--;
    }
    X[j+1]=a;
    Ordre[j+1]=k;
    Type[j+1]=l;
  }

  /* Initialisation de la dispersion */
  /* initiale au-dessus des routes,  */
  /* inspiree du modele CALINE 4 (Benson, 1984) */  
  for(i=0;i<Don.N_Rue;i++){
    if(R[i].Categ==1){
      /* Calcul de l'angle vent-rue (majore si = 0) */
      sin_phi=DMAX(fabs(Met.iy*R[i].ix-Met.ix*R[i].iy),sin(pi/24.0));
      /* Distance de transit */
      R[i].L0=DMIN((0.5*R[i].W)/sin_phi,0.5*R[i].L);
      /* Ajustement de la concentration en bord de route */
      Sig0=2.0;
      C_bord_int=sqrt(2.0/pi)/(Met.sigma_wH*R[i].L*R[i].W)*log(1.0+Met.sigma_wH*2.0*R[i].L0/(Sig0*Met.Uh));
      trouve=0;
      ecart=1.0E6;
      for(dx=0;dx<100.0 && trouve==0;dx+=1.0){
	C_bord_ext=Gauss_Lineic(R[i].L0,0.0,0.5*R[i].W,dx,
				R[i].ixv*R[i].L,
				R[i].iyv*R[i].L,
				R[i].L);
	if(fabs(C_bord_int-C_bord_ext)<ecart)
	  ecart=fabs(C_bord_int-C_bord_ext);
	else{
	  trouve=1;
	  R[i].delta_x=dx;
	}
      }
    }
  }

  /* Calcul de la concentration due aux routes a    */
  /* l'exterieur du bati et aux sources ponctuelles */
  for(i=0;i<Don.N_Noeud;i++){
    Cnd_fond[i]=Conc_Fond(Nd[i].xv,Nd[i].yv,Nd[i].Cell_i,Nd,
			  R,Srce_Ponct);
  }
  for(i=0;i<Don.N_Rue;i++){
    Crue_fond[i]=Conc_Fond(R[i].xv,R[i].yv,R[i].Cell_i,Nd,
			   R,Srce_Ponct);
  }

  /* Calcul des coefficients de flux pour les rues */
  for(i=0;i<Don.N_Rue;i++){
    if(R[i].Categ==0){
      R[i].Pdiff_v=R[i].L*R[i].W*Met.sigma_wH/sqrt(2.0)/pi;
      /* Pas de flux diffusif vertical si vent parallele */
      if(fabs(R[i].us_perp)<1.0e-6) R[i].Pdiff_v=0.0;
      /* Lessivage */
      R[i].P_lessiv=R[i].H*R[i].W*R[i].L*Met.Taux_Lessiv;
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
	/* Calcul du flux convectif horizontal entrant */
	/* et sortant dans l'intersection              */
	if(R[k].ndsort==i)
	  Nd[i].Pconv_entr+=R[k].Pconv_h;
	else if(R[k].ndent==i)
	  Nd[i].Pconv_sort+=R[k].Pconv_h;
      }
      Nd[i].Pconv_v=Nd[i].Pconv_entr-Nd[i].Pconv_sort;
    }
  }

  /* Initialisation des valeurs de concentration */
  for(i=0;i<Don.N_Rue;i++){
    R[i].Cint=0.0;
    R[i].Cext=0.0;
    R[i].Qdiff_v=0.0;
  }
  for(i=0;i<Don.N_Noeud;i++){
    Nd[i].Cint=0.0;
    Nd[i].Cext=0.0;
    Nd[i].Qconv_v=0.0;
  }

  /*-----------------------------------------------*/
  /* Calcul de la matrice de dispersion exterieure */
  /* Panache[i][j] = influence sur la rue i de la  */
  /* source j                                      */
  /*-----------------------------------------------*/
  Panache=calloc(Don.N_Rue+Don.N_Noeud,sizeof(DBL*));
  N_Cell=(2*Don.N_Infl+1)*(2*Don.N_Infl+1);
  /* Concentration au sommet des rues */
  for(j=0;j<Don.N_Rue;j++){
    if(R[j].Cell_i!=-1){
      cell_j=Don.cell[R[j].Cell_i];
      Panache[j]=calloc(cell_j.Nrue_Infl+cell_j.Nnoeud_Infl,sizeof(DBL));
      N_Rue=cell_j.Nrue_Infl;
      /* Boucle sur les mailles entourant la rue */
      cpt_r=0;
      cpt_n=0;
      for(m=0;m<N_Cell;m++){
	ind_m=cell_j.Num_Infl[m];
	if(ind_m!=-1){
	  cell_m=Don.cell[ind_m];
	  /* Concentration due aux rues */
	  for(n=0;n<cell_m.N_Rue;n++){
	    i=cell_m.NumRue[n];
	    if(R[i].Categ==0 && i!=j){
	      Panache[j][cpt_r]=Gauss_Lineic(R[j].xv-R[i].xv,R[j].yv-R[i].yv,0.5*R[i].W,0.0,
					 R[i].ixv*R[i].L,
					 R[i].iyv*R[i].L,
					 R[i].L);
	    }
	    else Panache[j][cpt_r]=0.0;
	    cpt_r+=1;
	  }
	  
	  /* Concentration due aux noeuds */
	  for(n=0;n<cell_m.N_Noeud;n++){
	    i=cell_m.NumNd[n];
	    if(Nd[i].Categ==0){
	      Panache[j][cpt_n+N_Rue]=
		Gauss(R[j].xv-Nd[i].xv,R[j].yv-Nd[i].yv,Nd[i].Rayon,Nd[i].delta_x_z);
	    }
	    else Panache[j][cpt_n+N_Rue]=0.0;
	    cpt_n+=1;
	  }
	}
      }
    }
  }
  /* Concentration au sommet des noeuds */
  for(j=0;j<Don.N_Noeud;j++){
    if(Nd[j].Cell_i!=-1){
      cell_j=Don.cell[Nd[j].Cell_i];
      Panache[j+Don.N_Rue]=calloc(cell_j.Nrue_Infl+cell_j.Nnoeud_Infl,sizeof(DBL));
      N_Rue=cell_j.Nrue_Infl;
      /* Boucle sur les mailles entourant le noeud */
      cpt_r=0;
      cpt_n=0;
      for(m=0;m<N_Cell;m++){
	ind_m=cell_j.Num_Infl[m];
	if(ind_m!=-1){
	  cell_m=Don.cell[ind_m];
	  /* Concentration due aux rues */
	  for(n=0;n<cell_m.N_Rue;n++){
	    i=cell_m.NumRue[n];
	    /* Pas d'influence d'une rue sur le noeud adjacent */
	    if(R[i].Categ==0 && j!=R[i].Deb && j!=R[i].Fin){
	      Panache[j+Don.N_Rue][cpt_r]=Gauss_Lineic(Nd[j].xv-R[i].xv,Nd[j].yv-R[i].yv,0.5*R[i].W,0.0,
						   R[i].ixv*R[i].L,
						   R[i].iyv*R[i].L,
						   R[i].L);
	    }
	    else Panache[j+Don.N_Rue][cpt_r]=0.0;
	    cpt_r+=1;
	  }
	  
	  /* Concentration due aux noeuds */
	  for(n=0;n<cell_m.N_Noeud;n++){
	    i=cell_m.NumNd[n];
	    if(Nd[i].Categ==0 && i!=j){
	      Panache[j+Don.N_Rue][cpt_n+N_Rue]=
		Gauss(Nd[j].xv-Nd[i].xv,Nd[j].yv-Nd[i].yv,Nd[i].Rayon,Nd[i].delta_x_z);
	    }
	    else Panache[j+Don.N_Rue][cpt_n+N_Rue]=0.0;
	    cpt_n+=1;
	  }
	}
      }
    }
  }

  /*------------------------------------------*/
  /* Resolution iterative de la concentration */
  /*------------------------------------------*/
  printf("\n-- Methode iterative --\n\n");

  ConvMax=1.0;
  for(n=0;n<100 && ConvMax>1.0e-5;n++){
    /* Calcul de la concentration interieure */
    for(m=0;m<5;m++){
      Solv_Noeud(Nd,R);
      Solv_Rue(Nd,R);
    }

    /* Calcul de la concentration exterieure */
    for(i=0;i<3*Don.N_Rue+Don.N_Noeud;i++){
      j=Ordre[i];
      /* Concentration exterieure aux rues */
      if(Type[i]<2 && R[j].Categ==0 && R[j].Cell_i!=-1){
	/* Calcul de Cext */	
	R[j].Cext=Crue_fond[j];

	cell_j=Don.cell[R[j].Cell_i];
	N_Rue=cell_j.Nrue_Infl;
	/* Boucle sur les mailles entourant la rue */
	cpt_r=0;
	cpt_n=0;
	for(m=0;m<N_Cell;m++){
	  ind_m=cell_j.Num_Infl[m];
	  if(ind_m!=-1){
	    cell_m=Don.cell[ind_m];
	    /* Concentration due aux rues */
	    for(l=0;l<cell_m.N_Rue;l++){
	      k=cell_m.NumRue[l];
	      R[j].Cext+=R[k].Qdiff_v*Panache[j][cpt_r];
	      cpt_r+=1;
	    }
	    /* Concentration due aux noeuds */
	    for(l=0;l<cell_m.N_Noeud;l++){
	      k=cell_m.NumNd[l];
	      if(Nd[k].Qconv_v>0.0){
		R[j].Cext+=DMIN(Nd[k].Cint,Nd[k].Qconv_v*Panache[j][cpt_n+N_Rue]);
	      }
	      else R[j].Cext+=Nd[k].Qconv_v*Panache[j][cpt_n+N_Rue];
	      cpt_n+=1;
	    }
	  }
	}
	/* La contribution du bati ne peut pas etre negative */
	R[j].Cext=DMAX(Crue_fond[j],R[j].Cext);
	/* Mise a jour du flux vertical */
	R[j].Qdiff_v=R[j].Pdiff_v*
	  (R[j].Cint-R[j].Cext);
      }
      /* Concentration exterieure aux noeuds */
      else if(Type[i]==2 && Nd[j].Categ==0 && Nd[j].Cell_i!=-1){
	/* Calcul de Cext */
	Nd[j].Cext=Cnd_fond[j];

	cell_j=Don.cell[Nd[j].Cell_i];
	N_Rue=cell_j.Nrue_Infl;
	/* Boucle sur les mailles entourant le noeud */
	cpt_r=0;
	cpt_n=0;
	for(m=0;m<N_Cell;m++){
	  ind_m=cell_j.Num_Infl[m];
	  if(ind_m!=-1){
	    cell_m=Don.cell[ind_m];
	    /* Concentration due aux rues */
	    for(l=0;l<cell_m.N_Rue;l++){
	      k=cell_m.NumRue[l];
	      Nd[j].Cext+=R[k].Qdiff_v*Panache[j+Don.N_Rue][cpt_r];
	      cpt_r+=1;
	    }
	    /* Concentration due aux noeuds */
	    for(l=0;l<cell_m.N_Noeud;l++){
	      k=cell_m.NumNd[l];
	      if(Nd[k].Qconv_v>0.0){
		Nd[j].Cext+=DMIN(Nd[k].Cint,Nd[k].Qconv_v*Panache[j+Don.N_Rue][cpt_n+N_Rue]);
	      }
	      else Nd[j].Cext+=Nd[k].Qconv_v*Panache[j+Don.N_Rue][cpt_n+N_Rue];
	      cpt_n+=1;
	    }
	  }
	}
	/* La contribution du bati ne peut pas etre negative */
	Nd[j].Cext=DMAX(Cnd_fond[j],Nd[j].Cext);
	/* Mise a jour du flux vertical */
	if(Nd[j].Pconv_v_entr>1.0e-3)
	  Nd[j].Qconv_v=Nd[j].Qconv_v_sort-Nd[j].Pconv_v_entr*Nd[j].Cext;
      }
      /* Mise a jour de la concentration interne   */
      /* tous les 10 calculs de concentration ext. */
      if(i%10==0){
	Solv_Noeud(Nd,R);
	Solv_Rue(Nd,R);
      }
    }

    /* Evaluation de la convergence */
    ConvMax=Convergence(n,Nd,R,Cprec);
  }

  /*-------------------------------------------*/
  /* Calcul de la concentration sur les routes */
  /*-------------------------------------------*/
  for(j=0;j<Don.N_Rue;j++){
    if(R[j].Categ==1 && R[j].Cell_i!=-1){
      R[j].Cext=Crue_fond[j];

      cell_j=Don.cell[R[j].Cell_i];
      N_Rue=cell_j.Nrue_Infl;
      /* Boucle sur les mailles entourant le noeud */
      cpt_r=0;
      cpt_n=0;
      for(m=0;m<N_Cell;m++){
	ind_m=cell_j.Num_Infl[m];
	if(ind_m!=-1){
	  cell_m=Don.cell[ind_m];
	  /* Concentration due aux rues */
	  for(l=0;l<cell_m.N_Rue;l++){
	    k=cell_m.NumRue[l];
	    if(R[k].Categ==0)
	      R[j].Cext+=R[k].Qdiff_v*Panache[j][cpt_r];
	    cpt_r+=1;
	  }
	  /* Concentration due aux noeuds */
	  for(l=0;l<cell_m.N_Noeud;l++){
	    k=cell_m.NumNd[l];
	    if(Nd[k].Categ==0){
	      if(Nd[k].Qconv_v>0.0){
		R[j].Cext+=DMIN(Nd[k].Cint,Nd[k].Qconv_v*Panache[j][cpt_n+N_Rue]);
	      }
	      else R[j].Cext+=Nd[k].Qconv_v*Panache[j][cpt_n+N_Rue];
	    }
	    cpt_n+=1;
	  }
	}
      }
      /* La contribution du bati ne peut pas etre negative */
      R[j].Cext=DMAX(Crue_fond[j],R[j].Cext);
      R[j].Cint=R[j].Cext;
    }
  }

  /*----------------------------------------------------------*/
  /* Concentration nulle a l'exterieur de la grille de calcul */
  /*----------------------------------------------------------*/
  for(j=0;j<Don.N_Rue;j++){
    if(R[j].Cell_i==-1){
      R[j].Cext=0.0;
      R[j].Cint=0.0;
    }
  }
  for(j=0;j<Don.N_Noeud;j++){
    if(Nd[j].Cell_i==-1){
      Nd[j].Cext=0.0;
      Nd[j].Cint=0.0;
    }
  }

  /*-------------------------------------*/
  /* Impression de la solution a l'ecran */
  /*-------------------------------------*/
  if(Don.affich>=1){
    printf("Solution :\n");
    for(i=0;i<Don.N_Rue;i++){
      if(R[i].Categ==0){
	printf("Rue %d : Conc int=%.3e\t Conc ext=%.3e\t Flux=%.3e\t \n",
	       i,R[i].Cint,R[i].Cext,R[i].Qdiff_v);
      }
    }
    for(i=0;i<Don.N_Noeud;i++){
      if(Nd[i].Categ==0){
	printf("Noeud %d : Conc int=%.3e\t Conc ext=%.3e\t Flux=%.3e\t \n",
	       i,Nd[i].Cint,Nd[i].Cext,Nd[i].Qconv_v);
      }
    }
    printf("\n");
  }

  /*-----------------------*/
  /* Liberation de memoire */
  /*-----------------------*/
  free(Cnd_fond);
  free(Crue_fond);
  free(Cprec);
  free(X);
  free(Ordre);
  free(Type);
  LibereMtrx(Panache,Don.N_Rue+Don.N_Noeud,Don.N_Rue+Don.N_Noeud);
}


/*---------------------------------------------*/


DBL Convergence(int n,Noeud *Nd,Rue *R,DBL *Cprec)
/*--------------------------*/
/* Calcul des parametres de */
/*   convergence du calcul  */
/*--------------------------*/
{
  int i,imax=0,typemax=0;
  DBL ConvMoy=0.0,ConvMax=0.0,residu;

  /*---------------------*/
  /* Boucle sur les rues */
  /*---------------------*/
  for(i=0;i<Don.N_Rue;i++){
    if(R[i].Cint!=0.0 && R[i].Categ==0){
      residu=fabs((R[i].Cint-Cprec[i])/
		  R[i].Cint);
      ConvMoy+=residu;
      if(ConvMax<residu){
	ConvMax=residu;
	imax=i;
	typemax=0;
      }      
    }
  }

  /*-----------------------*/
  /* Boucle sur les noeuds */
  /*-----------------------*/
  for(i=0;i<Don.N_Noeud;i++){
    if(Nd[i].Cint!=0.0 && Nd[i].Categ==0){
      residu=fabs((Nd[i].Cint-Cprec[i+Don.N_Rue])/
		  Nd[i].Cint);
      ConvMoy+=residu;
      if(ConvMax<residu){
	ConvMax=residu;
	imax=i;
	typemax=2;
      }
    }
  }

  /*------------------------------------*/
  /* Actualisation des valeurs de Cprec */
  /*------------------------------------*/
  for(i=0;i<Don.N_Rue;i++){
    Cprec[i]=R[i].Cint;
  }
  for(i=0;i<Don.N_Noeud;i++){
    Cprec[i+Don.N_Rue]=Nd[i].Cint;
  }

  /*------------------*/
  /* Impression ecran */
  /*------------------*/
  ConvMoy/=Don.N_Noeud+Don.N_Rue;
  if(typemax==0)
    printf("Iteration %d\tresidu moyen=%f\t max=%f (%d %d %f)\n",
	   n,ConvMoy,ConvMax,typemax,imax,R[imax].Cint*1e6);
  else if(typemax==2)
    printf("Iteration %d\tresidu moyen=%f\t max=%f (%d %d %f)\n",
	   n,ConvMoy,ConvMax,typemax,imax,Nd[imax].Cint*1e6);

  /*----------------------------*/
  /* Retourne la valeur ConvMax */
  /*----------------------------*/
  return ConvMax;
}


/*---------------------------------------------*/
