/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Stat.c --> Statistiques de concentration    */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"


/*---------------------------------------------*/


void Stat(Rue *R,DonIter *Iter,Grid Grd)
/*------------------------------------------*/
/* Calcul des statistiques de concentration */
/*------------------------------------------*/
{
  /*----------------------*/
  /* Statistiques par rue */
  /*----------------------*/
  if(Don.calc_disp==1){
    printf("\n-- Calcul des statistiques par rue\n\n");
    if(Don.Type_Pol!=1){
      /* Calcul des statistiques du scalaire principal */
      Stat_Rue_Recept(Don.Percentiles,Don.Seuils,R,Iter,"",0);
    }
    else if(Don.Type_Pol==1){
      /* Calcul des statistiques des NOx */
      Stat_Rue_Recept(Don.Percentiles,Don.Seuils,R,Iter,"NOx",0);
      /* Calcul des statistiques de NO, NO2 et O3 */
      Stat_Rue_Recept(Don.Percentiles_NO,Don.Seuils_NO,R,Iter,"NO",0);
      Stat_Rue_Recept(Don.Percentiles_NO2,Don.Seuils_NO2,R,Iter,"NO2",0);
      Stat_Rue_Recept(Don.Percentiles_O3,Don.Seuils_O3,R,Iter,"O3",0);
    }
  }

  /*----------------------------*/
  /* Statistiques par recepteur */
  /*----------------------------*/
 if(Don.calc_disp==1){
   printf("\n-- Calcul des statistiques par recepteur\n\n");
   if(Don.Type_Pol!=1){
     /* Calcul des statistiques du scalaire principal */
     Stat_Rue_Recept(Don.Percentiles,Don.Seuils,R,Iter,"",1);
   }
   else if(Don.Type_Pol==1){
     /* Calcul des statistiques des NOx */
     Stat_Rue_Recept(Don.Percentiles,Don.Seuils,R,Iter,"NOx",1);
     /* Calcul des statistiques de NO, NO2 et O3 */
     Stat_Rue_Recept(Don.Percentiles_NO,Don.Seuils_NO,R,Iter,"NO",1);
     Stat_Rue_Recept(Don.Percentiles_NO2,Don.Seuils_NO2,R,Iter,"NO2",1);
     Stat_Rue_Recept(Don.Percentiles_O3,Don.Seuils_O3,R,Iter,"O3",1);
   }
 }

  /*--------------------------------------*/
  /* Statistiques sur la grille de sortie */
  /*--------------------------------------*/
  if(Don.champ>=1){
    printf("\n-- Calcul des statistiques sur la grille de sortie\n\n");
    if(Don.Type_Pol!=1){
      /* Calcul des statistiques du scalaire principal */
      Stat_Grd(Don.Percentiles,Don.Seuils,Grd,Iter,"");
    }
    else if(Don.Type_Pol==1){
      /* Calcul des statistiques des NOx */
      Stat_Grd(Don.Percentiles,Don.Seuils,Grd,Iter,"NOx");
      /* Calcul des statistiques de NO, NO2 et O3 */
      Stat_Grd(Don.Percentiles_NO,Don.Seuils_NO,Grd,Iter,"NO");
      Stat_Grd(Don.Percentiles_NO2,Don.Seuils_NO2,Grd,Iter,"NO2");
      Stat_Grd(Don.Percentiles_O3,Don.Seuils_O3,Grd,Iter,"O3");
    }
  }
}


/*---------------------------------------------*/


void Stat_Point(DBL *C,Date *date,int N,
		DBL *Percentiles,DBL *Seuils,
		DBL *Cmoy,DBL *Cmax,DBL *Percent,
		int *Hrseuil,int *Jrseuil)
/*------------------------------------------*/
/* Calcul des statistiques temporelles en   */
/* un point                                 */
/*                                          */
/* - C est le vecteur (de dimension N) de   */
/*   concentration sur lequel on fait des   */
/*   statistiques                           */
/* - Date est le vecteur (de dimension N)   */
/*   des dates correspondantes              */
/* - Cmoy et Cmax sont les concentrations   */
/*   moyenne et maximale retounees          */
/* - Percent, Hrseuil et Jrseuil sont les   */
/*   valeurs de retour des percentiles et   */
/*   des nombres de depassementss           */
/*------------------------------------------*/
{
  int i,k,l,trouve;
  int an,jour,**AnDepass,**JourDepass;
  DBL a,*Ctmp;

  /*-----------------------*/
  /* Allocation de memoire */
  /*-----------------------*/
  /* Indices des annees de depassement */
  AnDepass=AllocMtrxI(Don.N_Seuils,N);
  /* Indices des jours de depassement */
  JourDepass=AllocMtrxI(Don.N_Seuils,N);
  /* Tableau de travail */
  Ctmp=calloc(N,sizeof(DBL));
  for(i=0;i<N;i++)
    Ctmp[i]=C[i];

  /*-----------------------*/
  /* Calcul des parametres */
  /*-----------------------*/
  *Cmoy=0.0;
  *Cmax=0.0;
  for(k=0;k<Don.N_Seuils;k++){
    Hrseuil[k]=0;
    Jrseuil[k]=0;
  }
  for(i=0;i<N;i++){
    /* Concentration moyenne */
    *Cmoy+=Ctmp[i];
    /* Concentration maximale */
    *Cmax=DMAX(*Cmax,Ctmp[i]);
    /* Nombre d'heures de depassement */
    an=date[i].a;
    jour=date[i].j;
    for(k=0;k<Don.N_Seuils;k++){
      if(Ctmp[i]>=Seuils[k]){
	Hrseuil[k]+=1;
	/* Teste si on a deja un depassement le meme jour */
	trouve=0;
	for(l=0;l<Jrseuil[k] && trouve==0;l++){
	  if(an==AnDepass[k][l] && jour==JourDepass[k][l])
	    trouve=1;
	}
	/* Cas ou c'est la premiere fois qu'il y a un depassement ce jour */
	if(trouve==0){
	  Jrseuil[k]+=1;
	  AnDepass[k][Jrseuil[k]-1]=an;
	  JourDepass[k][Jrseuil[k]-1]=jour;
	}
      }
    }
  }
  /* Concentration moyenne */
  if(N>0) *Cmoy=*Cmoy/N;

  /* Classement des heures dans l'ordre croissant des */
  /* concentrations pour le calcul des percentiles    */
  for(i=1;i<N;i++){
    a=Ctmp[i];
    k=i-1;
    while(k>=0 && Ctmp[k]>a){
      Ctmp[k+1]=Ctmp[k];
      k--;
    }
    Ctmp[k+1]=a;
  }
  /* Calcul des percentiles */
  for(k=0;k<Don.N_Percentiles;k++){
    Percent[k]=Ctmp[(int) (N*Percentiles[k]/100.0)];
  }

  /*-----------------------*/
  /* Liberation de memoire */
  /*-----------------------*/
  LibereMtrxI(AnDepass,Don.N_Seuils,N);
  LibereMtrxI(JourDepass,Don.N_Seuils,N);
  free(Ctmp);
}


/*---------------------------------------------*/
