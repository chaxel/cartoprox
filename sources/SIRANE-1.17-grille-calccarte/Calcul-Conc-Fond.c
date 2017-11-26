/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Calcul-Conc-Fond.c --> Calcul des           */
/* concentrations dues aux sources considerees */
/* comme hors du bati                          */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"


/*---------------------------------------------*/


DBL Conc_Fond(DBL x,DBL y,int cell_i,Noeud *Nd,
	      Rue *R,Source_Ponct *Srce_Ponct)
/*------------------------------*/
/*  Calcul de la concentration  */
/*  de fond, due aux sources a  */
/*  l'ext. du bati et aux       */
/*  sources ponct.              */
/*------------------------------*/
{
  int i,j,m,n,N_Cell,cell_m,trouve;
  DBL xv,yv;
  DBL xr,yr,xo,yo,Conc,H_eff,Lx,sig0;

  /* Cas d'un point en dehors de la grille de travail */
  if(cell_i==-1) return 0.0;

  /* Initialisation */
  Conc=0.0;
  N_Cell=(2*Don.N_Infl+1)*(2*Don.N_Infl+1);

  /* Concentration due aux points de grille */
  for(i=0;i<Don.grd.Nx;i++){
    for(j=0;j<Don.grd.Ny;j++){
      cell_m=Don.grd.Nx*j+i;
      /* Recherche si la cellule est au voisinage du point */
      trouve=0;
      for(m=0;m<N_Cell && trouve==0;m++){
	if(cell_m==Don.cell[cell_i].Num_Infl[m])
	  trouve=1;
      }
      /* Sinon, contribution de la cellule */
      if(trouve==0){
	xv=Met.ix*Don.cell[cell_m].x+Met.iy*Don.cell[cell_m].y;
	yv=Met.ix*Don.cell[cell_m].y-Met.iy*Don.cell[cell_m].x;
	Conc+=Don.cell[cell_m].Qrue_nd*Gauss(x-xv,y-yv,Don.grd.dx,0.0);
      }
    }
  }

  /* Boucle sur les mailles entourant le point */
  for(m=0;m<N_Cell;m++){
    cell_m=Don.cell[cell_i].Num_Infl[m];
    if(cell_m!=-1){
      /* Concentration due aux routes (ext. au bati) */
      for(n=0;n<Don.cell[cell_m].N_Rue;n++){
	i=Don.cell[cell_m].NumRue[n];
	if(R[i].Categ==1){
	  /* Changement de repere dans la direction du vent */
	  /* --------> repere dans la direction de la route */
	  xr=R[i].ixv*(x-R[i].xv)+R[i].iyv*(y-R[i].yv);
	  yr=R[i].ixv*(y-R[i].yv)-R[i].iyv*(x-R[i].xv);
	  /* Si point au-dessus de la route -> dilution initiale */
	  if(xr>(-0.49*R[i].L) && xr<(0.49*R[i].L) &&
	     yr>(-0.49*R[i].W) && yr<(0.49*R[i].W)){
	    /* Point de reference : point amont en longitudinal */
	    /* et transversal                                   */
	    /* x vers l'aval, y vers l'aval                     */
	    /* Rue en sens opposee au vent */
	    if(R[i].ixv<0.0)
	      xo=0.5*R[i].L-xr;
	    /* Rue dans le sens du vent */
	    else
	      xo=xr+0.5*R[i].L;
	    /* Vent de droite vers gauche */
	    if(R[i].iyv<0.0)
	      yo=yr+0.5*R[i].W;
	    /* Vent de gauche vers droite */
	    else
	      yo=0.5*R[i].W-yr;
	    
	    /* Distance depuis le bord amont de la route */
	    if(fabs(R[i].iyv)<1.0E-5) Lx=xo;
	    else if(fabs(R[i].ixv)<1.0E-5) Lx=yo;
	    else Lx=DMIN(fabs(yo/R[i].iyv),fabs(xo/R[i].ixv));
	    /* Concentration */
	    sig0=2.0;
	    Conc+=sqrt(2.0/pi)*R[i].Qsrce/(R[i].L*R[i].W*Met.sigma_wH)*
	      log(1.0+Met.sigma_wH*Lx/(sig0*Met.Uh));
	  }
	  /* Sinon calcul normal */
	  else{
	    Conc+=R[i].Qsrce*
	      Gauss_Lineic(x-R[i].xv,y-R[i].yv,0.5*R[i].W,R[i].delta_x,
			   R[i].ixv*R[i].L,
			   R[i].iyv*R[i].L,
			   R[i].L);
	  }
	}
      }
      
      /* Concentration due aux noeuds (ext. au bati) */
      for(n=0;n<Don.cell[cell_m].N_Noeud;n++){
	i=Don.cell[cell_m].NumNd[n];
	if(Nd[i].Categ==1){
	  /* Concentration limitee a la concentration */
	  /* a Rayon+3m en aval de la source          */
	  if(DSQR(x-Nd[i].x)+DSQR(y-Nd[i].y)<DSQR(Nd[i].Rayon))
	    Conc+=Nd[i].Qsrce*
	      Gauss(Nd[i].Rayon+3.0,0.0,Nd[i].Rayon,Nd[i].delta_x_z);
	  else
	    Conc+=Nd[i].Qsrce*
	      DMIN(Gauss(Nd[i].Rayon+3.0,0.0,Nd[i].Rayon,Nd[i].delta_x_z),
		   Gauss(x-Nd[i].xv,y-Nd[i].yv,Nd[i].Rayon,Nd[i].delta_x_z));
	}
      }
    }
  }

  /* Concentration due aux sources ponctuelles */
  for(i=0;i<Don.N_Srce_ponct;i++){
    if(Srce_Ponct[i].H<Met.h){
      H_eff=Hauteur_effective(Srce_Ponct[i],Met,x-Srce_Ponct[i].xv);
      if(H_eff<Met.h)
	Conc+=Srce_Ponct[i].Qsrce*
	  Gaussz(x-Srce_Ponct[i].xv,y-Srce_Ponct[i].yv,H_eff);
    }
  }

  return DMAX(Conc,0.0);
}


/*---------------------------------------------*/
