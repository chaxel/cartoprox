/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Io-Resul.c --> Ecriture des resultats sur   */
/*                fichiers                     */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"


/*---------------------------------------------*/


void Init_resul(Rue *R)
/*----------------------------------------*/
/* Creation et initialisation des         */
/* fichiers pour l'ecriture des resultats */
/*----------------------------------------*/
{
  int j;
  FILE *fich;

  /* Creation des fichiers temporaires de resultats de concentration */
  if(Don.calc_disp==1){
    fich=OuvreFichier(TMP_FICH_RUE,"w");
    FermeFichier(fich);
    fich=OuvreFichier(TMP_FICH_RECEPT,"w");
    FermeFichier(fich);
  }

  /* Ouverture des fichiers de resultats de concentration */
  /* Classement par rue */
  if(Don.calc_disp==1 && Don.ordre==1){
    /* Ouverture des fichiers de rues */
    for(j=0;j<Don.N_Rue;j++){
      sprintf(R[j].name_res_rue,"%s/Evol-%s-%s.dat",
	      Don.name_dir_resul,FLAG_RUE,R[j].Id);
    }
    /* Ouverture des fichiers de recepteurs */
    for(j=0;j<Don.N_Recept;j++){
      sprintf(Don.Rec[j].name_res_recept,"%s/Evol-%s-%s.dat",
	      Don.name_dir_resul,FLAG_RECEPT,Don.Rec[j].Id);
    }
  }

  /* Ouverture du fichier meteorologique */
  sprintf(Don.name_res_met,"%s/Evol-meteo.dat",Don.name_dir_resul);
  fich=OuvreFichier(Don.name_res_met,"w");
  fprintf(fich,"i\tDate\tHeure");
  if(Don.type_mesure_vent==0) fprintf(fich,"\tUg");
  else fprintf(fich,"\tUext");
  fprintf(fich,"\tDirect\tUh\tTo\th\tus\tsigmatheta");
  if(Don.type_meteo==1) fprintf(fich,"\tCld\tFo\tLmo\tthetas");
  if(Don.type_disp==1) fprintf(fich,"\tPasq");
  if(Don.Type_Pol==1 && Don.Mod_Cste_Chim==1) fprintf(fich,"\tk1\tk3");
  fprintf(fich,"\n");
  FermeFichier(fich);
}


/*---------------------------------------------*/


void Suppr_tmp()
/*-----------------------------------------*/
/* Supprime les fichiers temporaires avant */
/* la sortie du programme                  */
/*-----------------------------------------*/
{
  printf("   Suppression des fichiers temporaires -> ");
  fflush(stdout);
  remove(TMP_FICH_RUE);
  remove(TMP_FICH_RECEPT);
  printf("OK\n");
}


/*---------------------------------------------*/

void Ecrire_resul_iter(Rue *R,DBL *C,DBL *C_NO,
		       DBL *C_NO2,DBL *C_O3,
		       DonIter Iter,int type,int pas_temps)
/*------------------------------*/
/* Ecriture des resultats par   */
/* pas de temps                 */
/*------------------------------*/
{
  int j,N;
  char nom_fichier[100],flag[50],**Id;
  char nom_fich_tmp[100];
  DBL *X,*Y;
  FILE *fich;

  /* Distinction entre rues et recepteurs */
  /* Cas des rues */
  if(type==0){
    printf("   Ecriture des concentrations dans les rues -> ");
    fflush(stdout);
    sprintf(flag,FLAG_RUE);
    N=Don.N_Rue;
    sprintf(nom_fich_tmp,TMP_FICH_RUE);
    if(Don.ordre==0){
      Id=calloc(N,sizeof(char*));
      X=calloc(N,sizeof(DBL));
      Y=calloc(N,sizeof(DBL));
      for(j=0;j<N;j++){
	Id[j]=R[j].Id;
	X[j]=R[j].x;
	Y[j]=R[j].y;
      }
    }
  }
  /* Cas des recepteurs */
  else if(type==1){
    printf("   Ecriture des concentrations aux recepteurs -> ");
    fflush(stdout);
    sprintf(flag,FLAG_RECEPT);
    N=Don.N_Recept;
    sprintf(nom_fich_tmp,TMP_FICH_RECEPT);
    if(Don.ordre==0){
      Id=calloc(N,sizeof(char*));
      X=calloc(N,sizeof(DBL));
      Y=calloc(N,sizeof(DBL));
      for(j=0;j<N;j++){
	Id[j]=Don.Rec[j].Id;
	X[j]=Don.Rec[j].x;
	Y[j]=Don.Rec[j].y;
      }
    }
  }

  /* Fichier temporaire */
  fich=OuvreFichier(nom_fich_tmp,"a");
  /* Polluant passif ou particules */
  if(Don.Type_Pol==0 || Don.Type_Pol==2){
    for(j=0;j<N;j++){	
      fwrite(&(C[j]),sizeof(DBL),1,fich);
    }
  }
  /* Chimie NO-NO2-O3 */
  else if(Don.Type_Pol==1){
    for(j=0;j<N;j++){
      fwrite(&(C[j]),sizeof(DBL),1,fich);
      fwrite(&(C_NO[j]),sizeof(DBL),1,fich);
      fwrite(&(C_NO2[j]),sizeof(DBL),1,fich);
      fwrite(&(C_O3[j]),sizeof(DBL),1,fich);
    }
  }
  FermeFichier(fich);

  /* Ecriture des fichiers du pas de temps */
  if(Don.ordre==0){
    sprintf(nom_fichier,"%s/Iteration-%s-%d.dat",
	    Don.name_dir_resul,flag,pas_temps);
    fich=OuvreFichier(nom_fichier,"w");
    /* Polluant passif ou particules */
    if(Don.Type_Pol==0 || Don.Type_Pol==2){
      fprintf(fich,"Id\tX\tY\tC\n");
      for(j=0;j<N;j++){
	fprintf(fich,"%s\t%.2f\t%.2f\t%f\n",
		Id[j],X[j],Y[j],C[j]);
      }
    }
    /* Chimie NO-NO2-O3 */
    else if(Don.Type_Pol==1){
      fprintf(fich,"Id\tX\tY\tC\tC_NO\tC_NO2\tC_O3\n");
      for(j=0;j<N;j++){
	fprintf(fich,"%s\t%.2f\t%.2f\t%f\t%f\t%f\t%f\n",
		Id[j],X[j],Y[j],C[j],C_NO[j],
		C_NO2[j],C_O3[j]);
      }
    }
    FermeFichier(fich);
  }
  printf("OK\n");

  /* Liberation de memoire */
  if(Don.ordre==0 && (type==0 || type==1)){
    free(Id);
    free(X);
    free(Y);
  }
}

/*---------------------------------------------*/


void Ecrire_resul_rue_recept(Rue *R,DonIter *Iter,int type)
/*------------------------------*/
/* Ecriture des resultats par   */
/* position                     */
/*------------------------------*/
{
  int i,j,k,N,N_Tps_Valid=0;
  char flag[50];
  char **tab_name_fich,nom_fich_tmp[100];
  DBL C,C_NO,C_NO2,C_O3,bufferDBL;
  FILE *fich_tmp,*fich;
  fpos_t **tab_pos_fich;

  /* Distinction entre rues et recepteurs */
  /* Cas des rues */
  if(type==0){
    printf("   Ecriture des concentrations dans les rues -> ");
    fflush(stdout);
    sprintf(flag,FLAG_RUE);
    N=Don.N_Rue;
    tab_name_fich=calloc(N,sizeof(char*));
    sprintf(nom_fich_tmp,TMP_FICH_RUE);
    for(j=0;j<N;j++){
      tab_name_fich[j]=R[j].name_res_rue;
    }
  }
  /* Cas des recepteurs */
  else if(type==1){
    printf("   Ecriture des concentrations aux recepteurs -> ");
    fflush(stdout);
    sprintf(flag,FLAG_RECEPT);
    N=Don.N_Recept;
    tab_name_fich=calloc(N,sizeof(char*));
    sprintf(nom_fich_tmp,TMP_FICH_RECEPT);
    for(j=0;j<N;j++){
      tab_name_fich[j]=Don.Rec[j].name_res_recept;
    }
  }

  /*-----------------*/
  /* Initialisations */
  /*-----------------*/
  /* Comptage des pas de temps valides */
  /* et statistique uniquement sur ces */
  /* pas de temps                      */
  N_Tps_Valid=0;
  for(i=0;i<Don.N_Temps;i++){
    if(Iter[i].valid==1) N_Tps_Valid+=1;
  }

  /* Allocation de memoire */
  /* Pointeur sur fichier */
  tab_pos_fich=calloc(N_Tps_Valid,sizeof(fpos_t*));
  for(k=0;k<N_Tps_Valid;k++) tab_pos_fich[k]=calloc(1,sizeof(fpos_t));

  /* Initialisations et ouvertures de fichiers */
  fich_tmp=OuvreFichier(nom_fich_tmp,"r");
  k=0;
  for(i=0;i<Don.N_Temps;i++){
    if(Iter[i].valid==1){ 
      /* Stockage de la position de la premiere */
      /* rue/recept dans le fichier temporaire */
      fgetpos(fich_tmp,tab_pos_fich[k]);
      /* Avance jusqu'a la rue/recept suivante */
      /* Polluant passif ou particules */
      if(Don.Type_Pol==0 || Don.Type_Pol==2){
	for(j=0;j<N;j++){	
	  fread(&bufferDBL,sizeof(DBL),1,fich_tmp);
	}
      }
      /* Chimie NO-NO2-O3 */
      else if(Don.Type_Pol==1){
	for(j=0;j<N;j++){
	  fread(&bufferDBL,sizeof(DBL),1,fich_tmp);
	  fread(&bufferDBL,sizeof(DBL),1,fich_tmp);
	  fread(&bufferDBL,sizeof(DBL),1,fich_tmp);
	  fread(&bufferDBL,sizeof(DBL),1,fich_tmp);
	}
      }
      k+=1;
    }
  }

  /* Classement par pas de temps */
  for(j=0;j<N;j++){
    fich=OuvreFichier(tab_name_fich[j],"w");
    /* Polluant passif ou particules */
    if(Don.Type_Pol==0 || Don.Type_Pol==2)
      fprintf(fich,"i\tDate\tHeure\tC\n");
    /* Chimie NO-NO2-O3 */
    else if(Don.Type_Pol==1)
      fprintf(fich,"i\tDate\tHeure\tC\tC_NO\tC_NO2\tC_O3\n");
    /* Lecture et des donnees pour une rue/recept */
    k=0;
    for(i=0;i<Don.N_Temps;i++){
      fprintf(fich,"%d\t%.2d/%.2d/%.4d\t%.2d:%.2d",i+1,
	      (int) Iter[i].date.jm,(int) Iter[i].date.m,(int) Iter[i].date.a,
	      (int) Iter[i].date.h,(int) Iter[i].date.min);
      if(Iter[i].valid==1){ 
	fsetpos(fich_tmp,tab_pos_fich[k]);
	if(Don.Type_Pol==0 || Don.Type_Pol==2){
	  fread(&(C),sizeof(DBL),1,fich_tmp);
	  fprintf(fich,"\t%f\n",C);
	}
	else if(Don.Type_Pol==1){
	  fread(&(C),sizeof(DBL),1,fich_tmp);
	  fread(&(C_NO),sizeof(DBL),1,fich_tmp);
	  fread(&(C_NO2),sizeof(DBL),1,fich_tmp);
	  fread(&(C_O3),sizeof(DBL),1,fich_tmp);
	  fprintf(fich,"\t%f\t%f\t%f\t%f\n",C,
		  C_NO,C_NO2,C_O3);
	}
	fgetpos(fich_tmp,tab_pos_fich[k]);
	k+=1;
      }
      else{
	if(Don.Type_Pol==0 || Don.Type_Pol==2){
	  fprintf(fich,"\t%f\n",0.0);
	}
	else if(Don.Type_Pol==1){
	  fprintf(fich,"\t%f\t%f\t%f\t%f\n",0.0,
		  0.0,0.0,0.0);
	}
      }
    }
    FermeFichier(fich);
  }
  FermeFichier(fich_tmp);
  printf("OK\n");

  /* Liberation de memoire */
  free(tab_name_fich);
  for(k=0;k<N_Tps_Valid;k++) free(tab_pos_fich[k]);
  free(tab_pos_fich);
}





/*---------------------------------------------*/


void Ecrire_meteo(DonIter Iter,int pas_temps)
/*-------------------------*/
/* Ecriture des parametres */
/* metea par pas de temps  */
/*-------------------------*/
{
  FILE *fich;

  printf("   Ecriture des donnees meteorologiques -> ");
  fflush(stdout);

  fich=OuvreFichier(Don.name_res_met,"a");
  if(Iter.valid==0){
    fprintf(fich,"%d\t%.2d/%.2d/%.4d\t%.2d:%.2d",pas_temps,
	    (int) Iter.date.jm,(int) Iter.date.m,(int) Iter.date.a,
	    (int) Iter.date.h,(int) Iter.date.min);
    if(Don.type_mesure_vent==0) fprintf(fich,"\t9999");
    else fprintf(fich,"\t9999");
    fprintf(fich,"\t9999\t9999\t9999\t9999\t9999\t9999");
    if(Don.type_meteo==1) fprintf(fich,"\t9999\t9999\t9999\t9999");
    if(Don.type_disp==1) fprintf(fich,"\t9999");
    if(Don.Type_Pol==1 && Don.Mod_Cste_Chim==1)
      fprintf(fich,"\t9999\t9999");
    fprintf(fich,"\n");
  }
  else if(Iter.valid==1){
    fprintf(fich,"%d\t%.2d/%.2d/%.4d\t%.2d:%.2d",pas_temps,
	    (int) Iter.date.jm,(int) Iter.date.m,(int) Iter.date.a,
	    (int) Iter.date.h,(int) Iter.date.min);
    if(Don.type_mesure_vent==0) fprintf(fich,"\t%.2f",Iter.met.Ug);
    else fprintf(fich,"\t%.2f",Iter.met.Uext);
    fprintf(fich,"\t%.1f\t%.2f\t%.2f\t%.1f\t%.2f\t%.2f",Iter.met.Angle,
	    Iter.met.Uh,Iter.met.To-T0abs,
	    Iter.met.h,Iter.met.us,Iter.met.sigmatheta);
    if(Don.type_meteo==1) fprintf(fich,"\t%.1f\t%.2f\t%.2f\t%.3f",Iter.met.Cld,
				  Iter.met.Fo,Iter.met.Lmo,Iter.met.thetas);
    if(Don.type_disp==1) fprintf(fich,"\t%c",Iter.met.Pasq);
    if(Don.Type_Pol==1 && Don.Mod_Cste_Chim==1)
      fprintf(fich,"\t%.2e\t%.2e",Iter.k1,Iter.k3);
    fprintf(fich,"\n");
  }
  FermeFichier(fich);

  printf("OK\n");
  

  
}

/*---------------------------------------------*/
