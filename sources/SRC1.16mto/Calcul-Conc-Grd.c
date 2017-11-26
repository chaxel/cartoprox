/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Calcul-Conc-Grd.c                           */
/* -> Calcul des concentrations sur une grille */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"


/*---------------------------------------------*/


void Calcul_grd(DBL **Conc,DBL Conc_fond,Rue *R,Noeud *N,
		Source_Ponct *Srce_Ponct,Grid Grd,DBL **IndRN)
/*--------------------------------------*/
/* Calcul de la concentration au niveau */
/* du sol sur une grille reguliere      */
/*--------------------------------------*/
{
  int ratio;
  int i,j,k,ind_i,ind_j,i_emis,j_emis,Nx,Ny;
  int i_l,j_l,i_g,j_g,Nx_g,Ny_g;
  DBL dx_g,dy_g,frac1,frac2,frac3,frac4,sn,si,W_Cell;
  DBL x,y,xv,yv,C_tmp,**Cunit,**Cemiss;
  DBL H_eff;
  char Message[100];

  /* Initialisations */
  ratio=Don.GrdRatio;
  Nx=Grd.Nx;
  Ny=Grd.Ny;
  if((Grd.Nx-1)%ratio!=0 || (Grd.Ny-1)%ratio!=0){
    sprintf(Message,"Nombre de point de grille n'est pas de la forme %d.i+1",ratio);
    Erreur(Message,1);
  }
  Nx_g=(Grd.Nx-1)/ratio+1;
  Ny_g=(Grd.Ny-1)/ratio+1;
  dx_g=ratio*Grd.dx;
  dy_g=ratio*Grd.dy;
  Cunit=AllocMtrx(2*Nx_g-1,2*Ny_g-1);
  Cemiss=AllocMtrx(Nx_g,Ny_g);
  /* Determination de la largeur des mailles dans la direction du vent */
  /* On projette les deux diagonales d'une maille dans la direction du vent */
  W_Cell=0.5*DMAX(fabs(Met.ix*dy_g-Met.iy*dx_g),fabs(-Met.ix*dy_g-Met.iy*dx_g));
  printf("%f %f %f\n",dx_g,dy_g,W_Cell);

  /* Calcul de la matrice unitaire */
  for(i=0;i<2*Nx_g-1;i++){
    for(j=0;j<2*Ny_g-1;j++){
      /* Determination des coordonnees du point */
      x=(i-Nx_g+1)*dx_g;
      y=(j-Ny_g+1)*dy_g;
      /* Changement de repere -> repere dans la direction du vent */
      xv=Met.ix*x+Met.iy*y;
      yv=Met.ix*y-Met.iy*x;
      /* Concentration au point de grille */
      Cunit[i][j]=Gauss(xv,yv,W_Cell,0.0);
    }
  }

  /* Affectation des emissions sur la grille */
  for(i=0;i<Nx_g;i++){
    for(j=0;j<Ny_g;j++){
      Cemiss[i][j]=0.0;
    }
  }
  for(k=0;k<Don.N_Rue;k++){
    /* Discretisation de la source lineique   */
    /* comme une serie de sources ponctuelles */
    /* a intervalles reguliers                */
    /* Calcul de la taille des intervalles */
    /* taille = 0.5*dx_g                   */
    sn=1.0/(int)(R[k].L/(0.5*dx_g)+1.0);

    for(si=0.5*(-1.0+sn);si<0.5;si+=sn){
      ind_i=(int) floor((R[k].x+si*R[k].ix*R[k].L-Grd.xmin)/dx_g+0.5);
      ind_j=(int) floor((R[k].y+si*R[k].iy*R[k].L-Grd.ymin)/dy_g+0.5);
      if(Inclu_Grd(R[k].x+si*R[k].ix*R[k].L,R[k].y+si*R[k].iy*R[k].L,Grd)==1){
	if(R[k].Categ==0)
	  Cemiss[ind_i][ind_j]+=R[k].Qdiff_v*sn;
	else if(R[k].Categ==1)
	  Cemiss[ind_i][ind_j]+=R[k].Qsrce*sn;
      }
    }
  }
  for(k=0;k<Don.N_Noeud;k++){
    ind_i=(int) floor((N[k].x-Grd.xmin)/dx_g+0.5);
    ind_j=(int) floor((N[k].y-Grd.ymin)/dy_g+0.5);
    if(Inclu_Grd(N[k].x,N[k].y,Grd)==1){
      if(N[k].Categ==0)
	Cemiss[ind_i][ind_j]+=N[k].Qconv_v;
      else if(N[k].Categ==1)
	Cemiss[ind_i][ind_j]+=N[k].Qsrce;
    }
  }

  /* Calcul pour chaque point de grille */
  for(i_g=0;i_g<Nx_g;i_g++){
    for(j_g=0;j_g<Ny_g;j_g++){
      /* Concentration au point de grille */
      C_tmp=0.0;
      i=ratio*i_g;
      j=ratio*j_g;
      for(i_emis=0;i_emis<Nx_g;i_emis++){
	for(j_emis=0;j_emis<Ny_g;j_emis++){
	  C_tmp+=Cemiss[i_emis][j_emis]*Cunit[i_g-i_emis+Nx_g-1][j_g-j_emis+Ny_g-1];
	}
      }
      Conc[i][j]=C_tmp;
    }
  }

  /* Test si des points de la grille grossiere sont dans des rues ou des intersections */
  for(i_g=0;i_g<Nx_g;i_g++){
    for(j_g=0;j_g<Ny_g;j_g++){
      i=ratio*i_g;
      j=ratio*j_g;
      k=IndRN[i][j];
      if(k!=-1){
	if(k<100000) Conc[i][j]=R[k].Cint;
	else Conc[i][j]=N[k-100000].Cint;
      }
    }
  }

  /* Ajout des sources ponctuelles */
  for(i_g=0;i_g<Nx_g;i_g++){
    for(j_g=0;j_g<Ny_g;j_g++){
      i=ratio*i_g;
      j=ratio*j_g;
      /* Determination des coordonnees du point */
      x=Grd.xmin+i*Grd.dx;
      y=Grd.ymin+j*Grd.dy;
      /* Changement de repere -> repere dans la direction du vent */
      xv=Met.ix*x+Met.iy*y;
      yv=Met.ix*y-Met.iy*x;
      /* Calcul de concentration pour toutes les sources ponctuelles */
      for(k=0;k<Don.N_Srce_ponct;k++){
	if(Srce_Ponct[k].H<Met.h){
	  H_eff=Hauteur_effective(Srce_Ponct[k],Met,x-Srce_Ponct[k].xv);
	  if(H_eff<Met.h)
	    Conc[i][j]+=Srce_Ponct[k].Qsrce*
	      Gaussz(xv-Srce_Ponct[k].xv,yv-Srce_Ponct[k].yv,H_eff);
	}
      }
    }
  }



  /* Interpolation de la grille grossiere a la grille fine */
  for(i=0;i<Nx;i++){
    for(j=0;j<Ny;j++){
      i_l=i%ratio;
      j_l=j%ratio;
      /* Calcul pour les points intermediaires a la grille grossiere */
      if(i_l!=0 || j_l!=0){
	/* Position des points grossiers */
	i_g=i-i_l;
	j_g=j-j_l;

	/* Cas ou le point est aligne avec la grille grossiere */
	if(i_l==0){
	  frac1=1.0/((DBL) j_l);
	  frac2=1.0/((DBL) ratio-j_l);
	  Conc[i][j]=(frac1*Conc[i_g][j_g]+frac2*Conc[i_g][j_g+ratio])/
	    (frac1+frac2);
	}
	else if(j_l==0){
	  frac1=1.0/((DBL) i_l);
	  frac2=1.0/((DBL) ratio-i_l);
	  Conc[i][j]=(frac1*Conc[i_g][j_g]+frac2*Conc[i_g+ratio][j_g])/
	    (frac1+frac2);
	}
	else{
	  frac1=1.0/((DBL) i_l*j_l);
	  frac2=1.0/((DBL) (ratio-i_l)*j_l);
	  frac3=1.0/((DBL) i_l*(ratio-j_l));
	  frac4=1.0/((DBL) (ratio-i_l)*(ratio-j_l));
	  Conc[i][j]=(frac1*Conc[i_g][j_g]+frac2*Conc[i_g+ratio][j_g]+
		      frac3*Conc[i_g][j_g+ratio]+frac4*Conc[i_g+ratio][j_g+ratio])/
	    (frac1+frac2+frac3+frac4);
	}
      }
    }
  }

  /* Test si des points dont dans des rues ou des intersections */
  for(i=0;i<Nx;i++){
    for(j=0;j<Ny;j++){
      k=IndRN[i][j];
      if(k!=-1){
	if(k<100000) Conc[i][j]=R[k].Cint;
	else Conc[i][j]=N[k-100000].Cint;
      }
    }
  }

  /* Passage en microgramme/m3 et ajout du fond */
  for(i=0;i<Nx;i++){
    for(j=0;j<Ny;j++){
      /* Minimum a zero */
      Conc[i][j]=DMAX(Conc[i][j],0.0);
      /* Conversion en micro-grammes/m3 */
      Conc[i][j]*=1.0e6;
      /* Ajout de la concentration de fond (en micro-grammes/m3) */
      Conc[i][j]+=Conc_fond;
    }
  }
}


/*---------------------------------------------*/


void Calcul_grd_Chapman(DBL **C_NOx,DBL **C_NO,DBL **C_NO2,DBL **C_O3,
			DBL Cb_NOx,DBL Cb_NO,DBL Cb_NO2,DBL Cb_O3,
			DBL k1,DBL k3,Grid Grd)
/*---------------------------------------------*/
/* Calcul de la concentration en NO, NO2 et 03 */
/* au niveau du sol sur une grille reguliere   */
/*---------------------------------------------*/
{
  int i,j;
  DBL Conc_NO,Conc_NO2,Conc_O3;

  /* Calcul pour chaque point de grille */
  for(i=0;i<Grd.Nx;i++){
    for(j=0;j<Grd.Ny;j++){
      /* Concentration au point de grille */
      Chapman(C_NOx[i][j]-Cb_NOx,Cb_O3,Cb_NO,Cb_NO2,
	      &Conc_NO,&Conc_NO2,&Conc_O3,k1,k3);
      C_NO[i][j]=Conc_NO;
      C_NO2[i][j]=Conc_NO2;
      C_O3[i][j]=Conc_O3;
    }
  }
}


/*---------------------------------------------*/


DBL Conc_Recept(DBL x,DBL y,Rue *R,Noeud *N,
		Source_Ponct *Srce_Ponct)
/*----------------------------------------*/
/* Calcul de la concentration au sol pour */
/* un recepteur quelconque                */
/* Si il est dans une rue                 */
/* -> C = concentration dans la rue       */
/* Sinon                                  */
/* -> C = concentration au niveau des     */
/* toits                                  */
/*----------------------------------------*/
{
  int k,ind_i,ind_j;
  DBL xv,yv,Conc_fond,Conc=0.0;

  /* Teste si le point est dans une rue du bati */
  for(k=0;k<Don.N_Rue;k++){
    if(R[k].Categ==0){
      /* Changement de repere -> repere dans la direction de la rue */
      xv=R[k].ix*(x-R[k].x)+R[k].iy*(y-R[k].y);
      yv=R[k].ix*(y-R[k].y)-R[k].iy*(x-R[k].x);
      if(xv>(-0.5*R[k].L) && xv<(0.5*R[k].L) &&
	 yv>(-0.5*R[k].W) && yv<(0.5*R[k].W))
	return R[k].Cint;
    }
  }

  /* Teste si le point est dans une intersection du bati */
  for(k=0;k<Don.N_Noeud;k++){
    if(N[k].Categ==0){
      if(DSQR(x-N[k].x)+DSQR(y-N[k].y)<DSQR(N[k].Rayon))
	return N[k].Cint;
    }
  }

  /* Changement de repere -> repere dans la direction du vent */
  xv=Met.ix*x+Met.iy*y;
  yv=Met.ix*y-Met.iy*x;

  /* Concentration due aux sources exterieures */
  ind_i=(int) floor((x-Don.grd.xmin)/Don.grd.dx);
  ind_j=(int) floor((y-Don.grd.ymin)/Don.grd.dy);
  if(Inclu_Grd(x,y,Don.grd)==1)
    Conc_fond=Conc_Fond(xv,yv,Don.grd.Nx*ind_j+ind_i,N,R,Srce_Ponct);
  else Conc_fond=0.0;
  Conc=Conc_fond;

  /* Concentration due aux rues du bati */
  for(k=0;k<Don.N_Rue;k++){
    if(R[k].Categ==0){
      /* Si Cint > Cext, on traite le flux   */
      /* sortant dans l'ecoulement exterieur */
      if(R[k].Qdiff_v>0.0){
	Conc+=DMIN((R[k].Cint-R[k].Cext)/2.0,R[k].Qdiff_v*
		   Gauss_Lineic(xv-R[k].xv,yv-R[k].yv,0.5*R[k].W,0.0,
				R[k].ixv*R[k].L,
				R[k].iyv*R[k].L,
				R[k].L));
      }
      /* Si Cint < Cext, on traite le flux */
      /* qui diffuse vers la canopee       */
      if(R[k].Qdiff_v<0.0){
	Conc+=DMAX((R[k].Cint-R[k].Cext)/2.0,R[k].Qdiff_v*
		   Gauss_Lineic(xv-R[k].xv,yv-R[k].yv,0.5*R[k].W,0.0,
				R[k].ixv*R[k].L,
				R[k].iyv*R[k].L,
				R[k].L));
      }
    }
  }
  
  /* Concentration due aux noeuds du bati */
  for(k=0;k<Don.N_Noeud;k++){
    if(N[k].Categ==0){
      /* S'il existe un flux vertical sortant,  */
      /* on traite le flux de polluants associe */
      if(N[k].Qconv_v>0.0){
	Conc+=DMIN(N[k].Cint,
		   N[k].Qconv_v*Gauss(xv-N[k].xv,
				      yv-N[k].yv,N[k].Rayon,N[k].delta_x_z));
      }
      else Conc+=N[k].Qconv_v*Gauss(xv-N[k].xv,
				    yv-N[k].yv,N[k].Rayon,N[k].delta_x_z);
    }
  }
  
  /* La contribution du bati ne peut pas etre negative */
  /* Renvoie une valeur > Conc_fond */
  return DMAX(Conc,Conc_fond);
}


/*---------------------------------------------*/


int Inclu_Grd(DBL x,DBL y,Grid grd)
/*---------------------------*/
/* Teste l'appartenance d'un */
/* point a une grille        */
/*---------------------------*/
{
  if(x>=Don.grd.xmin && x<Don.grd.xmax &&
     y>=Don.grd.ymin && y<Don.grd.ymax)
    return 1;
  else return 0;
}


/*---------------------------------------------*/
