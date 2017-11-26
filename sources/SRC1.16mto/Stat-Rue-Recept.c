/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Stat-Rue-Recept.c --> Statistiques de       */
/* concentration sur les rues                  */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"


/*---------------------------------------------*/


void Stat_Rue_Recept(DBL *Percentiles,DBL *Seuils,
		     Rue *R,DonIter *Iter,char *suffix,
		     int type)
/*------------------------------------------*/
/* Calcul des statistiques de concentration */
/* rue par rue :                            */
/* - Concentration moyenne                  */
/* - Concentration maximale                 */
/* - Differents percentiles                 */
/* - Nombre d'heures de depassement de      */
/*   differents seuils                      */
/* - Nombre de jours de depassement de      */
/*   differents seuils                      */
/*------------------------------------------*/
{
  char nom_fichier[200],nom_fich_tmp[100],flag[50],**Id;
  int i,j,k,N,N_Tps_Valid=0;
  int **Hrseuil,**Jrseuil;
  DBL *X,*Y,bufferDBL;
  DBL *C,*Cmoy,*Cmax,**Percent;
  Date *date;
  FILE *fich;
  fpos_t **tab_pos_fich;

  /*--------------------------------------*/
  /* Distinction entre rues et recepteurs */
  /*--------------------------------------*/
  /* Cas des rues */
  if(type==0){
    printf("   Statistiques sur les rues %s\n",suffix);
    sprintf(flag,FLAG_RUE);
    N=Don.N_Rue;
    Id=calloc(N,sizeof(char*));
    sprintf(nom_fich_tmp,TMP_FICH_RUE);
    X=calloc(N,sizeof(DBL));
    Y=calloc(N,sizeof(DBL));
    for(j=0;j<N;j++){
      Id[j]=R[j].Id;
      X[j]=R[j].x;
      Y[j]=R[j].y;
    }
  }
  /* Cas des recepteurs */
  else if(type==1){
    printf("   Statistiques sur les recepteurs %s\n",suffix);
    sprintf(flag,FLAG_RECEPT);
    N=Don.N_Recept;
    Id=calloc(N,sizeof(char*));
    sprintf(nom_fich_tmp,TMP_FICH_RECEPT);
    X=calloc(N,sizeof(DBL));
    Y=calloc(N,sizeof(DBL));
    for(j=0;j<N;j++){
      Id[j]=Don.Rec[j].Id;
      X[j]=Don.Rec[j].x;
      Y[j]=Don.Rec[j].y;
    }
  }

  /*-----------------*/
  /* Initialisations */
  /*-----------------*/
  /* Comptage des pas de temps valides */
  /* et statistique uniquement sur ces */
  /* pas de temps                      */
  for(i=0;i<Don.N_Temps;i++){
    if(Iter[i].valid==1) N_Tps_Valid+=1;
  }

  /* Allocation de memoire */
  /* Pointeur sur fichier */
  tab_pos_fich=calloc(N_Tps_Valid,sizeof(fpos_t*));
  for(k=0;k<N_Tps_Valid;k++) tab_pos_fich[k]=calloc(1,sizeof(fpos_t));
  /* Date */
  date=calloc(N_Tps_Valid,sizeof(Date));
  /* Concentration */
  C=calloc(N_Tps_Valid,sizeof(DBL));
  /* Moyenne */
  Cmoy=calloc(N,sizeof(DBL));
  /* Maximum */
  Cmax=calloc(N,sizeof(DBL));
  /* Tableau des percentiles */
  Percent=AllocMtrx(N,Don.N_Percentiles);
  /* Tableau des nombres d'heures de depassement */
  Hrseuil=AllocMtrxI(N,Don.N_Seuils);
  /* Tableau des nombres de jours de depassement */
  Jrseuil=AllocMtrxI(N,Don.N_Seuils);

  /* Initialisations et ouvertures de fichiers */
  fich=OuvreFichier(nom_fich_tmp,"r");
  k=0;
  for(i=0;i<Don.N_Temps;i++){
    if(Iter[i].valid==1){ 
      /* Copie de la date */
      date[k]=Iter[i].date;
      /* Stockage de la position de la premiere */
      /* rue/recept dans le fichier temporaire */
      fgetpos(fich,tab_pos_fich[k]);
      /* Avance jusqu'a la rue/recept suivante */
      /* Polluant passif ou particules */
      if(Don.Type_Pol==0 || Don.Type_Pol==2){
	for(j=0;j<N;j++){	
	  fread(&bufferDBL,sizeof(DBL),1,fich);
	}
      }
      /* Chimie NO-NO2-O3 */
      else if(Don.Type_Pol==1){
	for(j=0;j<N;j++){
	  fread(&bufferDBL,sizeof(DBL),1,fich);
	  fread(&bufferDBL,sizeof(DBL),1,fich);
	  fread(&bufferDBL,sizeof(DBL),1,fich);
	  fread(&bufferDBL,sizeof(DBL),1,fich);
	}
      }
      k+=1;
    }
  }

  /*----------------------------------------*/
  /* Calcul des statistiques par rue/recept */
  /*----------------------------------------*/
  for(j=0;j<N;j++){
    /* Lecture des donnees pour une rue/recept */
    if(!strncmp(suffix,"",IMAX(strlen(suffix),0)) &&
       Don.Type_Pol!=1){
      for(k=0;k<N_Tps_Valid;k++){
	fsetpos(fich,tab_pos_fich[k]);
	fread(&(C[k]),sizeof(DBL),1,fich);
	fgetpos(fich,tab_pos_fich[k]);
      }
    }
    else if(!strncmp(suffix,"NOx",IMAX(strlen(suffix),3)) &&
	    Don.Type_Pol==1){
      for(k=0;k<N_Tps_Valid;k++){
	fsetpos(fich,tab_pos_fich[k]);
	fread(&(C[k]),sizeof(DBL),1,fich);
	fread(&bufferDBL,sizeof(DBL),1,fich);
	fread(&bufferDBL,sizeof(DBL),1,fich);
	fread(&bufferDBL,sizeof(DBL),1,fich);
	fgetpos(fich,tab_pos_fich[k]);
      }
    }
    else if(!strncmp(suffix,"NO",IMAX(strlen(suffix),2)) &&
	    Don.Type_Pol==1){
      for(k=0;k<N_Tps_Valid;k++){
	fsetpos(fich,tab_pos_fich[k]);
	fread(&bufferDBL,sizeof(DBL),1,fich);
	fread(&(C[k]),sizeof(DBL),1,fich);
	fread(&bufferDBL,sizeof(DBL),1,fich);
	fread(&bufferDBL,sizeof(DBL),1,fich);
	fgetpos(fich,tab_pos_fich[k]);
      }
    }
    else if(!strncmp(suffix,"NO2",IMAX(strlen(suffix),3)) &&
	    Don.Type_Pol==1){
      for(k=0;k<N_Tps_Valid;k++){
	fsetpos(fich,tab_pos_fich[k]);
	fread(&bufferDBL,sizeof(DBL),1,fich);
	fread(&bufferDBL,sizeof(DBL),1,fich);
	fread(&(C[k]),sizeof(DBL),1,fich);
	fread(&bufferDBL,sizeof(DBL),1,fich);
	fgetpos(fich,tab_pos_fich[k]);
      }
    }
    else if(!strncmp(suffix,"O3",IMAX(strlen(suffix),2)) &&
	    Don.Type_Pol==1){
      for(k=0;k<N_Tps_Valid;k++){
	fsetpos(fich,tab_pos_fich[k]);
	fread(&bufferDBL,sizeof(DBL),1,fich);
	fread(&bufferDBL,sizeof(DBL),1,fich);
	fread(&bufferDBL,sizeof(DBL),1,fich);
	fread(&(C[k]),sizeof(DBL),1,fich);
	fgetpos(fich,tab_pos_fich[k]);
      }
    }
    /* Calcul des parametres statistiques pour la rue/recept considere */
    Stat_Point(C,date,N_Tps_Valid,Percentiles,Seuils,
	       &(Cmoy[j]),&(Cmax[j]),Percent[j],
	       Hrseuil[j],Jrseuil[j]);
  }

  /*---------------------------------*/
  /* Fermeture du fichier temporaire */
  /*---------------------------------*/
  FermeFichier(fich);

  /*------------------------*/
  /* Ecriture des resultats */
  /*------------------------*/
  printf("   Ecriture des statistiques par %s %s -> ",flag,suffix);
  fflush(stdout);
  sprintf(nom_fichier,"%s/stat-%s-%s.dat",Don.name_dir_resul,flag,suffix);
  fich=OuvreFichier(nom_fichier,"w");
  fprintf(fich,"#Id\tX\tY\tCmoy\tCmax");
  for(k=0;k<Don.N_Seuils;k++){
    fprintf(fich,"\tHr_C>%.0f",Seuils[k]);
  }
  for(k=0;k<Don.N_Seuils;k++){
    fprintf(fich,"\tJr_C>%.0f",Seuils[k]);
  }
  for(k=0;k<Don.N_Percentiles;k++){
    fprintf(fich,"\tC%.1f",Percentiles[k]);
  }
  fprintf(fich,"\n");

  for(j=0;j<N;j++){
    fprintf(fich,"%s\t%.2f\t%.2f\t%f\t%f",
	    Id[j],X[j],Y[j],Cmoy[j],Cmax[j]);
    for(k=0;k<Don.N_Seuils;k++){
      fprintf(fich,"\t%d",Hrseuil[j][k]);
    }
    for(k=0;k<Don.N_Seuils;k++){
      fprintf(fich,"\t%d",Jrseuil[j][k]);
    }
    for(k=0;k<Don.N_Percentiles;k++){
      fprintf(fich,"\t%f",Percent[j][k]);
    }
    fprintf(fich,"\n");
  }
  FermeFichier(fich);

  printf("OK\n\n");

  /*-----------------------*/
  /* Liberation de memoire */
  /*-----------------------*/
  free(Id);
  free(X);
  free(Y);
  for(k=0;k<N_Tps_Valid;k++) free(tab_pos_fich[k]);
  free(tab_pos_fich);
  free(date);
  free(C);
  free(Cmoy);
  free(Cmax);
  LibereMtrx(Percent,N,Don.N_Percentiles);
  LibereMtrxI(Hrseuil,N,Don.N_Seuils);
  LibereMtrxI(Jrseuil,N,Don.N_Seuils);
}


/*---------------------------------------------*/
