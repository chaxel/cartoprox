/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 22/03/2005    */
/*                                             */
/* Main.c --> Programme principal              */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"

Donnees Don;
Meteo Met;

/*---------------------------------------------*/


int main(int argc,char *argv[])
/*---------------------------------------------*/
/*         Modele SIRANE version 1.15          */
/*               26 octobre 2004               */
/*              Dr. Lionel SOULHAC             */
/*         LMFA/Ecole Centrale de Lyon         */
/*---------------------------------------------*/
{
  /*---------------------------*/
  /* Declaration des variables */
  /*---------------------------*/
  int i,j,k,sequent,prem_iter=0,trouve;
#ifdef DEC
  int tps_calcul;
#endif
  DBL *C_Rue,*C_Rue_NO,*C_Rue_NO2,*C_Rue_O3;
  DBL *C_Recept,*C_Recept_NO,*C_Recept_NO2,*C_Recept_O3;
  DBL **Csurf,**Csurf_NO,**Csurf_NO2,**Csurf_O3;
  DBL **Csurftot,**Csurftot_NO,**Csurftot_NO2,**Csurftot_O3;
  DBL x,y,xv,yv,**IndRN;
  Noeud *Nd;
  Rue *R;
  Source_Ponct *Srce_Ponct;
  DonIter *Iter;
  Grid Grd;

  /*---------------------*/
  /* Entete du programme */
  /*---------------------*/
  printf("************************************************\n");
  printf("**      _____ ________  ___    _   ________   **\n");
  printf("**     / ___//  _/ __ \\/   |  / | / / ____/   **\n");
  printf("**     \\__ \\ / // /_/ / /| | /  |/ / __/      **\n");
  printf("**    ___/ // // _, _/ ___ |/ /|  / /___      **\n");
  printf("**   /____/___/_/ |_/_/  |_/_/ |_/_____/      **\n");
  printf("**                                            **\n");
  printf("**         MODELE SIRANE VERSION 1.16 DIAG    **\n");
  printf("**              26 octobre 2004               **\n");
  printf("**             Dr. Lionel SOULHAC             **\n");
  printf("**        LMFA/Ecole Centrale de Lyon         **\n");
  printf("**                                            **\n");
  printf("************************************************\n");

#ifdef DEC
  tps_calcul=(int) time();
#endif
  /*---------------------*/
  /* Lecture des donnees */
  /*---------------------*/
  /* Verification de la ligne de commande */
#ifdef WIN
  if(argc!=2)
    Erreur("La syntaxe de la ligne de commande est = Sirane.exe [donnees.dat]",0);
#else
  if(argc!=2)
    Erreur("La syntaxe de la ligne de commande est = Sirane [donnees.dat]",0);
#endif
  lecdon(argv[1],&Nd,&R,&Srce_Ponct,&Iter,&Grd);

  /*----------------*/
  /* Ajout ATMO RA  */
  /*----------------*/
  /* Lire_meteo(Iter); io.c */    

  /*----------------*/
  /* Initialisation */
  /*----------------*/
  Init(Nd,R);

  /*------------------------------------*/
  /* Creation des fichiers de resultats */
  /*------------------------------------*/
  Init_resul(R);

  /*-----------------------*/
  /* Allocation de memoire */
  /*-----------------------*/
  if(Don.calc_disp==1){
    C_Rue=calloc(Don.N_Rue,sizeof(DBL));
    C_Recept=calloc(Don.N_Recept,sizeof(DBL));
    if(Don.Type_Pol==1){
      C_Rue_NO=calloc(Don.N_Rue,sizeof(DBL));
      C_Rue_NO2=calloc(Don.N_Rue,sizeof(DBL));
      C_Rue_O3=calloc(Don.N_Rue,sizeof(DBL));
      C_Recept_NO=calloc(Don.N_Recept,sizeof(DBL));
      C_Recept_NO2=calloc(Don.N_Recept,sizeof(DBL));
      C_Recept_O3=calloc(Don.N_Recept,sizeof(DBL));
    }
    if(Don.champ>=1){
      Csurf=AllocMtrx(Grd.Nx,Grd.Ny);
      Csurftot=AllocMtrx(Grd.Nx,Grd.Ny);
      IndRN=AllocMtrx(Grd.Nx,Grd.Ny);
      if(Don.Type_Pol==1){
	Csurf_NO=AllocMtrx(Grd.Nx,Grd.Ny);
	Csurf_NO2=AllocMtrx(Grd.Nx,Grd.Ny);
	Csurf_O3=AllocMtrx(Grd.Nx,Grd.Ny);
	Csurftot_NO=AllocMtrx(Grd.Nx,Grd.Ny);
	Csurftot_NO2=AllocMtrx(Grd.Nx,Grd.Ny);
	Csurftot_O3=AllocMtrx(Grd.Nx,Grd.Ny);
      }
    }
  }

  /*Construction du tableau IndRN pour le calcul sur la grille par interpolation */
  /* Test si des points dont dans des rues ou des intersections */
    if(Don.champ>=1){
  printf("\n-- Initialisation de la grille de sortie\n\n");
  for(i=0;i<Grd.Nx;i++){
    /* FT if(i%(Grd.Nx/10)==0) printf("%d%%\n",i/(Grd.Nx/10)*10);
    else if(i%(Grd.Nx/100)==0) printf("%d%%  ",i/(Grd.Nx/100));*/
    for(j=0;j<Grd.Ny;j++){
      IndRN[i][j]=-1;
      /* Determination des coordonnees du point */
      x=Grd.xmin+i*Grd.dx;
      y=Grd.ymin+j*Grd.dy;
      /* Teste si le point est dans une rue du bati */
      trouve=0;
      for(k=0;k<Don.N_Rue && trouve==0;k++){
	/* Changement de repere -> repere dans la direction de la rue */
	xv=R[k].ix*(x-R[k].x)+R[k].iy*(y-R[k].y);
	yv=R[k].ix*(y-R[k].y)-R[k].iy*(x-R[k].x);
	if(xv>(-0.5*R[k].L) && xv<(0.5*R[k].L) &&
	   yv>(-0.5*R[k].W) && yv<(0.5*R[k].W)){
	  IndRN[i][j]=k;
	  trouve=1;
	}
      }

      /* Teste si le point est dans une intersection du bati */
      trouve=0;
      for(k=0;k<Don.N_Noeud && trouve==0;k++){
	if(Nd[k].Categ==0){
	  if(DSQR(x-Nd[k].x)+DSQR(y-Nd[k].y)<DSQR(Nd[k].Rayon)){
	    IndRN[i][j]=100000+k;
	    trouve=1;
	  }
	}
      }
    }
  }
  }

  /*------------------*/
  /* Boucle de calcul */
  /*------------------*/
  printf("\n*************************************\n");
  printf("**                                 **\n");
  printf("**  DEBUT DE LA BOUCLE TEMPORELLE  **\n");
  printf("**                                 **\n");
  printf("*************************************\n");
  for(i=0;i<Don.N_Temps;i++){
    /* Cas d'un pas de temps hors de la plage d'etude */
    if(i+1<Don.iter_deb || i+1>Don.iter_fin){
      /* On considere que le pas de temps n'est pas valide */
      Iter[i].valid=0;
      /* On ne fait rien */
    }
    /* Cas d'un pas de temps non valide */
    else if(Iter[i].valid==0){
      Jour_Annee(&(Iter[i].date));
      Jour_Semaine(&(Iter[i].date));
      printf("\n#################################################################\n");
      printf("-- Iteration numero %d/%d\t %s %.2d/%.2d/%.4d %.2d:%.2d\tJour %d\n",
	     i-Don.iter_deb+2,Don.iter_fin-Don.iter_deb+1,Iter[i].date.NomJourSem,
	     (int) Iter[i].date.jm,(int) Iter[i].date.m,(int) Iter[i].date.a,
	     (int) Iter[i].date.h,(int) Iter[i].date.min,(int) Iter[i].date.j);
      printf("\n-- Pas de temps non valide - Donnees meteo manquantes\n\n");

      /*------------------------*/
      /* Ecriture des resultats */
      /*------------------------*/
      printf("\n-- Ecriture des resultats\n\n");
      Ecrire_meteo(Iter[i],i);
    }
    /* Cas d'un pas de temps valide */
    else if(Iter[i].valid==1){
      Jour_Annee(&(Iter[i].date));
      Jour_Semaine(&(Iter[i].date));
      printf("\n#################################################################\n");
      printf("-- Iteration numero %d/%d\t %s %.2d/%.2d/%.4d %.2d:%.2d\tJour %d\n",
	     i-Don.iter_deb+2,Don.iter_fin-Don.iter_deb+1,Iter[i].date.NomJourSem,
	     (int) Iter[i].date.jm,(int) Iter[i].date.m,(int) Iter[i].date.a,
	     (int) Iter[i].date.h,(int) Iter[i].date.min,(int) Iter[i].date.j);

      /*---------------------------------------*/
      /* Calcul des parametres meteorologiques */   
      /*---------------------------------------*/
      /* Desactivation de l'option sequentielle */
      /* pour le premier pas de temps valide ou */
      /* si le pas de temps precedent n'est pas */
      /* valide                                 */
      if(Don.sequent==1 && (prem_iter==0 || (i>0 && Iter[i-1].valid==0))){
	sequent=1;
	Don.sequent=0;
      }
      /* Initialisation des valeurs precedentes */
      else if(Don.sequent==1){
	Iter[i].met.h_prec=Iter[i-1].met.h;
	Iter[i].met.Precip_prec=Iter[i-1].met.Precip;
      }
      initmeteo(&Iter[i]);
      
      /* Reactivation de l'option sequentielle */
      if(sequent==1 && (prem_iter==0 || (i>0 && Iter[i-1].valid==0))){
	Don.sequent=sequent;
	prem_iter=1;
      }
      Met=Iter[i].met;

      /*------------------------------------*/
      /* Calcul des constantes de reactions */   
      /*------------------------------------*/
      if(Don.Type_Pol==1 && Don.Mod_Cste_Chim==0){
	Iter[i].k1=Don.k1k3;
	Iter[i].k3=1.0;
      }
      else if(Don.Type_Pol==1 && Don.Mod_Cste_Chim==1){
	Iter[i].k1=Cste_k1(Met.elev_sol,Met.Cld);
	Iter[i].k3=Cste_k3(Met.To,Met.Vo);
      }

      if(Don.calc_disp==1){
	/*-------------------------------------*/
	/* Calcul des parametres de dispersion */   
	/*-------------------------------------*/
	Sigma();
	
	/*---------------------------------------*/
	/* Determination des flux d'intersection */
	/*---------------------------------------*/
	Intersect(Nd,R);
	
	/*--------------------------*/
	/* Determination des debits */
	/*--------------------------*/
	Solv_Debit(Nd,R,Met.ix,Met.iy);
	
	/*-----------------------*/
	/* Lecture des emissions */
	/*-----------------------*/
	lecemis(&Nd,&R,&Srce_Ponct,Iter[i]);
	
	/*----------------------------------*/
	/* Determination des concentrations */
	/*----------------------------------*/
	Solv_Conc(Nd,R,Srce_Ponct);
	
	/*-----------------*/
	/* Calcul du champ */
	/*-----------------*/
	if(Don.champ>=1){
	  printf("\n-- Calcul du champ de concentration\n\n");
	  Calcul_grd(Csurf,Iter[i].Cext,R,Nd,Srce_Ponct,Grd,IndRN);
	  if(Don.Type_Pol!=1) Ecrire_grd(Csurf,Grd,"",i+1);
	  /* Calcul de la concentration en NO, NO2 et O3 */
	  if(Don.Type_Pol==1){
	/*    Ecrire_grd(Csurf,Grd,"NOx",i+1); */
	    Calcul_grd_Chapman(Csurf,Csurf_NO,Csurf_NO2,Csurf_O3,
			       Iter[i].Cext,Iter[i].Cb_NO,Iter[i].Cb_NO2,
			       Iter[i].Cb_O3,Iter[i].k1,Iter[i].k3,Grd);
  
	/*    Ecrire_grd(Csurf_NO,Grd,"NO",i+1); */
	/*    Ecrire_grd(Csurf_NO2,Grd,"NO2",i+1); */
	/*    Ecrire_grd(Csurf_O3,Grd,"O3",i+1);	*/
	  }
	  for(j=0;j<Grd.Nx;j++){
	    for(k=0;k<Grd.Ny;k++){
	      Csurftot[j][k]+=Csurf[j][k];
	      if(Don.Type_Pol==1){
		Csurftot_NO[j][k]+=Csurf_NO[j][k];
		Csurftot_NO2[j][k]+=Csurf_NO2[j][k];
		Csurftot_O3[j][k]+=Csurf_O3[j][k];
	      }
	    }
	  }	 
	}
	
	/*------------------------*/
	/* Stockage des resultats */
	/*------------------------*/
	for(j=0;j<Don.N_Rue;j++){
	  if(R[j].Cell_i!=-1){
	    /* Conversion kg/m3 -> microg/m3 */
	    C_Rue[j]=1.0e6*R[j].Cint;
	    /* Polluant passif ou particules */
	    if(Don.Type_Pol==0 || Don.Type_Pol==2)
	      C_Rue[j]+=Iter[i].Cext;
	    /* Calcul de la concentration en NO, NO2 et O3 */
	    else if(Don.Type_Pol==1){
	      Chapman(C_Rue[j],Iter[i].Cb_O3,Iter[i].Cb_NO,Iter[i].Cb_NO2,
		      &(C_Rue_NO[j]),&(C_Rue_NO2[j]),&(C_Rue_O3[j]),
		      Iter[i].k1,Iter[i].k3);
	      C_Rue[j]+=Iter[i].Cext;
	    }
	  }
	  else{
	    C_Rue[j]=0.0;
	    if(Don.Type_Pol==1){
	      C_Rue_NO[j]=0.0;
	      C_Rue_NO2[j]=0.0;
	      C_Rue_O3[j]=0.0;
	    }
	  }
	}
	/*------------------------------*/
	/* Concentration aux recepteurs */
	/*------------------------------*/
	for(j=0;j<Don.N_Recept;j++){
	  if(Inclu_Grd(Don.Rec[j].x,Don.Rec[j].y,Don.grd)==1){
	    /* Calcul de la concentration */
	    C_Recept[j]=1.0e6*Conc_Recept(Don.Rec[j].x,Don.Rec[j].y,
					  R,Nd,Srce_Ponct);
	    /* Polluant passif ou particules */
	    if(Don.Type_Pol==0 || Don.Type_Pol==2)
	      C_Recept[j]+=Iter[i].Cext;
	    /* Calcul de la concentration en NO, NO2 et O3 */
	    else if(Don.Type_Pol==1){
	      Chapman(C_Recept[j],Iter[i].Cb_O3,Iter[i].Cb_NO,Iter[i].Cb_NO2,
		      &(C_Recept_NO[j]),&(C_Recept_NO2[j]),&(C_Recept_O3[j]),
		      Iter[i].k1,Iter[i].k3);
	      C_Recept[j]+=Iter[i].Cext;
	    }
	  }
	  else{
	    C_Recept[j]=0.0;
	    if(Don.Type_Pol==1){
	      C_Recept_NO[j]=0.0;
	      C_Recept_NO2[j]=0.0;
	      C_Recept_O3[j]=0.0;
	    }
	  }
	}
      }

      /*------------------------*/
      /* Ecriture des resultats */
      /*------------------------*/
      printf("\n-- Ecriture des resultats\n\n");
      Ecrire_meteo(Iter[i],i+1);
      if(Don.calc_disp==1){
	Ecrire_resul_iter(R,C_Rue,C_Rue_NO,C_Rue_NO2,C_Rue_O3,Iter[i],0,i+1);
	Ecrire_resul_iter(R,C_Recept,C_Recept_NO,C_Recept_NO2,C_Recept_O3,Iter[i],1,i+1);
      }
    }
    /* Cas d'un pas de temps non valide ->  */
    /* les concentrations sont mises a zero */
    else{
      if(Don.calc_disp==1){
	InitTabDBL(C_Rue,Don.N_Rue,12.0);
	InitTabDBL(C_Recept,Don.N_Recept,0.0);
	if(Don.Type_Pol==1){
	  InitTabDBL(C_Rue_NO,Don.N_Rue,0.0);
	  InitTabDBL(C_Rue_NO2,Don.N_Rue,0.0);
	  InitTabDBL(C_Rue_O3,Don.N_Rue,0.0);
	  InitTabDBL(C_Recept_NO,Don.N_Rue,0.0);
	  InitTabDBL(C_Recept_NO2,Don.N_Rue,0.0);
	  InitTabDBL(C_Recept_O3,Don.N_Rue,0.0);
	}
      }
    }
  }
  printf("\n##########################################################\n\n");
  printf("***********************************\n");
  printf("**                               **\n");
  printf("**  FIN DE LA BOUCLE TEMPORELLE  **\n");
  printf("**                               **\n");
  printf("***********************************\n");
  
  if(Don.Type_Pol!=1) Ecrire_grd(Csurftot,Grd,"",0);
  /* Calcul de la concentration en NO, NO2 et O3 */
  else if(Don.Type_Pol==1){
    Ecrire_grd(Csurftot,Grd,"NOx",0);
    Ecrire_grd(Csurftot_NO,Grd,"NO",0);
    Ecrire_grd(Csurftot_NO2,Grd,"NO2",0);
    Ecrire_grd(Csurftot_O3,Grd,"O3",0);	
  }


  /*------------------------------------------*/
  /* Ecriture des resultats par rue/recepteur */
  /*------------------------------------------*/
  if(Don.ordre==1 && Don.calc_disp==1){
    Ecrire_resul_rue_recept(R,Iter,0);
    Ecrire_resul_rue_recept(R,Iter,1);
  }

  /*-------------------------*/
  /* Calcul des statistiques */
  /*-------------------------*/
  if(Don.calc_stat==1){
    Stat(R,Iter,Grd);
  }

  /*--------------------------------------*/
  /* Suppression des fichiers temporaires */
  /*--------------------------------------*/
   if(Don.calc_disp==1){
    Suppr_tmp();
  }

  /*-----------------------*/
  /* Liberation de memoire */
  /*-----------------------*/
  LibereNoeud(Nd);
  free(R);
  free(Srce_Ponct);
  free(Iter);
  if(Don.calc_disp==1){
    free(C_Rue);
    free(C_Recept);
    if(Don.Type_Pol==1){
      free(C_Rue_NO);
      free(C_Rue_NO2);
      free(C_Rue_O3);
      free(C_Recept_NO);
      free(C_Recept_NO2);
      free(C_Recept_O3);
    }
    if(Don.champ>=1){
      LibereMtrx(Csurf,Grd.Nx,Grd.Ny);
      LibereMtrx(Csurftot,Grd.Nx,Grd.Ny);
      if(Don.Type_Pol==1){
	LibereMtrx(Csurf_NO,Grd.Nx,Grd.Ny);	
	LibereMtrx(Csurf_NO2,Grd.Nx,Grd.Ny);	
	LibereMtrx(Csurf_O3,Grd.Nx,Grd.Ny);
	LibereMtrx(Csurftot_NO,Grd.Nx,Grd.Ny);	
	LibereMtrx(Csurftot_NO2,Grd.Nx,Grd.Ny);	
	LibereMtrx(Csurftot_O3,Grd.Nx,Grd.Ny);	
      }
    }
  }

  /*-----------------------------*/
  /* Calcul du temps d'execution */
  /*-----------------------------*/
#ifdef DEC
  printf("\n#############################\n");
  printf("-- Temps d'execution\n\n");
  tps_calcul=time()-tps_calcul;
  printf("Temps total d'execution\t\t\t\t= %d h %d min %d sec\n",
	 tps_calcul/3600,(tps_calcul/60)%60,tps_calcul%60);
  printf("Temps d'execution par pas de temps\t\t= %.2f sec\n",
	 (DBL) tps_calcul/Don.N_Temps);
  printf("Temps d'execution par rue et par pas de temps\t= %f sec\n",
	 (DBL) tps_calcul/Don.N_Temps/Don.N_Rue);
#endif

/*-----------------------------------*/
  printf("\n**************************\n");
  printf("**                      **\n");
  printf("**   FIN DU PROGRAMME   **\n");
  printf("**                      **\n");
  printf("**************************\n\n");
  return EXIT_SUCCESS;
}


/*---------------------------------------------*/
