/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Stat-Grd.c --> Statistiques de              */
/* concentration sur la grille de sortie       */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"


/*---------------------------------------------*/


void Stat_Grd(DBL *Percentiles,DBL *Seuils,Grid Grd,
	      DonIter *Iter,char *suffix)
/*------------------------------------------*/
/* Calcul des statistiques de concentration */
/* sur la grille de sortie :                */
/* - Concentration moyenne                  */
/* - Concentration maximale                 */
/* - Differents percentiles                 */
/* - Nombre d'heures de depassement de      */
/*   differents seuils                      */
/* - Nombre de jours de depassement de      */
/*   differents seuils                      */
/*------------------------------------------*/
{
  char nom_fichier[100];
  int i,j,k,l,m,n,N_Tps_Valid=0,N_Bloc,N_lu,Taille_Tab=1000000;
  int ***Hrseuil,***Jrseuil;
  DBL *C_Bloc,**C,**Ctmp,**Cmoy,**Cmax,***Percent;
  Date *date;

  printf("   Statistiques sur la grille de sortie %s\n\n",suffix);

  /*-----------------*/
  /* Initialisations */
  /*-----------------*/
  /* Comptage des pas de temps valides */
  /* et statistique uniquement sur ces */
  /* pas de temps                      */
  for(k=0;k<Don.N_Temps;k++){
    if(Iter[k].valid==1) N_Tps_Valid+=1;
  }

  /* Calcul de la taille d'un bloc pour que la */
  /* taille total du tableau C soit Taille_Tab */
  if(N_Tps_Valid!=0) N_Bloc=Taille_Tab/N_Tps_Valid;
  else N_Bloc=Grd.Nx*Grd.Ny;

  /* Allocation de memoire */
  /* Date */
  date=calloc(N_Tps_Valid,sizeof(Date));
  /* Concentration */
  C_Bloc=calloc(N_Bloc,sizeof(DBL));
  C=AllocMtrx(N_Bloc,N_Tps_Valid);
  /* Moyenne */
  Cmoy=AllocMtrx(Grd.Nx,Grd.Ny);
  /* Maximum */
  Cmax=AllocMtrx(Grd.Nx,Grd.Ny);
  /* Tableau des percentiles */
  Percent=AllocBoite(Grd.Nx,Grd.Ny,Don.N_Percentiles);
  /* Tableau des nombres d'heures de depassement */
  Hrseuil=AllocBoiteI(Grd.Nx,Grd.Ny,Don.N_Seuils);
  /* Tableau des nombres de jours de depassement */
  Jrseuil=AllocBoiteI(Grd.Nx,Grd.Ny,Don.N_Seuils);
  /* Tableau temporaire */
  Ctmp=AllocMtrx(Grd.Nx,Grd.Ny);

  /* Initialisation des dates */
  k=0;
  for(l=0;l<Don.N_Temps;l++){
    if(Iter[l].valid==1){
      /* Stockage de la date */
      date[k]=Iter[l].date;
      k+=1;
    }
  }

  /*-----------------------------------*/
  /* Lecture et statistiques par blocs */
  /*-----------------------------------*/
  n=0;
  while(n<(Grd.Nx*Grd.Ny)){
    /* Initialisations et ouvertures de fichiers */
    k=0;
    for(l=0;l<Don.N_Temps;l++){
      if(Iter[l].valid==1){
	/* Lecture du fichier */
	N_lu=Lire_grd(C_Bloc,suffix,l+1,n,N_Bloc);
	/* Transfert dans le tableau C */
	for(m=0;m<N_lu;m++)
	  C[m][k]=C_Bloc[m];
	k+=1;
      }
    }

    /* Calcul des statistiques pour tous les points du bloc */
    for(m=0;m<N_lu;m++){
      /* Calcul des positions i et j */
      if(Don.champ==1){
	j=(n+m)/Grd.Nx;
	i=(n+m)-j*Grd.Nx;
      }
      else if(Don.champ==2){
	j=Grd.Ny-(1+(n+m)/Grd.Nx);
	i=(n+m)-((n+m)/Grd.Nx)*Grd.Nx;
      }
      /* Calcul des parametres statistiques pour le point considere */
      Stat_Point(C[m],date,N_Tps_Valid,Percentiles,Seuils,
		 &(Cmoy[i][j]),&(Cmax[i][j]),Percent[i][j],
		 Hrseuil[i][j],Jrseuil[i][j]);
    }
    n+=N_lu;
    BarreAvance(n,Grd.Nx*Grd.Ny);
  }
  printf("\n\n");

  /*------------------------*/
  /* Ecriture des resultats */
  /*------------------------*/
  /* Concentration moyenne */
  sprintf(nom_fichier,"%s-moy",suffix);
  Ecrire_grd(Cmoy,Grd,nom_fichier,-1);
  /* Concentration moyenne */
  sprintf(nom_fichier,"%s-max",suffix);
  Ecrire_grd(Cmax,Grd,nom_fichier,-1);
  /* Nombre d'heures de depassement */
  for(k=0;k<Don.N_Seuils;k++){
    for(j=0;j<Grd.Ny;j++){
      for(i=0;i<Grd.Nx;i++){
	Ctmp[i][j]=Hrseuil[i][j][k];
      }
    }
    sprintf(nom_fichier,"%s-Hr_C%.0f",suffix,Seuils[k]);
    Ecrire_grd(Ctmp,Grd,nom_fichier,-1);
  }
  /* Nombre de jours de depassement */
  for(k=0;k<Don.N_Seuils;k++){
    for(j=0;j<Grd.Ny;j++){
      for(i=0;i<Grd.Nx;i++){
	Ctmp[i][j]=Jrseuil[i][j][k];
      }
    }
    sprintf(nom_fichier,"%s-Jr_C%.0f",suffix,Seuils[k]);
    Ecrire_grd(Ctmp,Grd,nom_fichier,-1);
  }
  /* Percentiles */
  for(k=0;k<Don.N_Percentiles;k++){
    for(j=0;j<Grd.Ny;j++){
      for(i=0;i<Grd.Nx;i++){
	Ctmp[i][j]=Percent[i][j][k];
      }
    }
    sprintf(nom_fichier,"%s-percent%.1f",suffix,Percentiles[k]);
    Ecrire_grd(Ctmp,Grd,nom_fichier,-1);
  }
  printf("\n");

  /*-----------------------*/
  /* Liberation de memoire */
  /*-----------------------*/
  free(date);
  free(C_Bloc);
  LibereMtrx(C,N_Bloc,N_Tps_Valid);
  LibereMtrx(Cmoy,Grd.Nx,Grd.Ny);
  LibereMtrx(Cmax,Grd.Nx,Grd.Ny);
  LibereBoite(Percent,Grd.Nx,Grd.Ny,Don.N_Percentiles);
  LibereBoiteI(Hrseuil,Grd.Nx,Grd.Ny,Don.N_Seuils);
  LibereBoiteI(Jrseuil,Grd.Nx,Grd.Ny,Don.N_Seuils);
  LibereMtrx(Ctmp,Grd.Nx,Grd.Ny);
}


/*---------------------------------------------*/
