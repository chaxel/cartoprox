/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Io.c --> Gestion des entrees/sorties sur    */
/*          fichiers                           */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"


/*---------------------------------------------*/


void lecdon(char *fichier,Noeud **NN,Rue **RR,Source_Ponct **II,
	    DonIter **Iter,Grid *Grd)
/*---------------------------------*/
/* Lecture des donnees sur fichier */
/*---------------------------------*/
{
  int i,j,l;
  DBL jour,mois,annee,heure,minute;
  FILE *name;
    
  Noeud *N;
  Rue *R;
  Source_Ponct *I;
  DonIter *Ite;

  DBL pas_temps,cste_k1,cste_k3;   

  printf("\n-- Lecture des donnees --\n\n");

  /* Lecture des donnees */
  /*---------------------*/
  /* Lecture prealable des donnees par defaut */
  /* pour renseigner toutes les variables     */
  printf("   Lecture des donnees par defaut\n");
  lecdongene("./Don-defaut.dat",Grd,0);
  /* Lecture des donnees de l'utilisateur pour */
  /* modifier les valeurs par defaut           */
  /* + affichage des parametres a l'ecran      */
  printf("   Lecture des donnees de l'utilisateur\n");
  lecdongene(fichier,Grd,1);

  /* Initialisations */
  /*-----------------*/
  Grd->dx=(Grd->xmax-Grd->xmin)/(Grd->Nx-1);
  Grd->dy=(Grd->ymax-Grd->ymin)/(Grd->Ny-1);

  Ite=calloc(Don.N_Temps,sizeof(DonIter));
  /* Par defaut, tous les pas de temps sont valides */
  for(i=0;i<Don.N_Temps;i++) Ite[i].valid=1;

  /* Lecture des rues */
  /*------------------*/
  printf("\n   Lecture des donnees de rues -> ");
  fflush(stdout);
  name=OuvreFichier(Don.name_rue,"r");
  fscanf(name,"%d",&Don.N_Rue);
  R=calloc(Don.N_Rue,sizeof(Rue));
  for(i=0;i<Don.N_Rue;i++){
      fscanf(name,"%s %s %s %lf %lf %d",
	     R[i].Id,R[i].IdDeb,R[i].IdFin,
	     &R[i].H,&R[i].W,&R[i].Categ);
      /* Initialisation des indices de noeuds */
      R[i].Deb=-1;
      R[i].Fin=-1;
  }
  FermeFichier(name);
  printf("OK\n");
  printf("      Nombre de rues = %d\n",Don.N_Rue);

  /* Lecture des noeuds */
  /*--------------------*/
  printf("   Lecture des donnees de noeuds -> ");
  fflush(stdout);
  name=OuvreFichier(Don.name_noeud,"r");
  fscanf(name,"%d",&Don.N_Noeud);
  N=calloc(Don.N_Noeud,sizeof(Noeud));
  for(i=0;i<Don.N_Noeud;i++){
    fscanf(name,"%s %lf %lf",
	   N[i].Id,&N[i].x,&N[i].y);
    /* Renumerotation des noeuds auxquels les rues font reference */
    l=strlen(N[i].Id);
    for(j=0;j<Don.N_Rue;j++){
      if(!strncmp(R[j].IdDeb,N[i].Id,IMAX(strlen(R[j].IdDeb),l))) R[j].Deb=i;
      if(!strncmp(R[j].IdFin,N[i].Id,IMAX(strlen(R[j].IdFin),l))) R[j].Fin=i;
    }
  }
  FermeFichier(name);
  /* Test de la correspondance rues-noeuds */
  for(j=0;j<Don.N_Rue;j++){
    if(R[j].Deb==-1 || R[j].Fin==-1)
      Erreur("Probleme de correspondance entre les identifiants des rues et des noeuds",0);
  }
  printf("OK\n");
  printf("      Nombre de noeuds = %d\n",Don.N_Noeud);

  /* Lecture des sources ponctuelles */
  /*---------------------------------*/
  printf("   Lecture des donnees de sources ponctuelles -> ");
  fflush(stdout);
  name=OuvreFichier(Don.name_srce_ponct,"r");
  fscanf(name,"%d",&Don.N_Srce_ponct);
  I=calloc(Don.N_Srce_ponct,sizeof(Source_Ponct));
  for(i=0;i<Don.N_Srce_ponct;i++){
      fscanf(name,"%s %lf %lf %lf %lf %lf %lf %lf",I[i].Id,
	     &I[i].x,&I[i].y,&I[i].H,&I[i].H_sol,&I[i].Rs,&I[i].Ws,&I[i].Ts);
      I[i].Ts+=T0abs;
      /*DEBUG Atmo RA*/
      /*printf("%s %.2f %.2f %.2f %.2f %.2f %.2f %.2f\n",I[i].Id,
	     I[i].x,I[i].y,I[i].H,I[i].H_sol,I[i].Rs,I[i].Ws,I[i].Ts);*/
  }
  FermeFichier(name);
  printf("OK\n");
  printf("      Nombre de sources ponctuelles = %d\n",Don.N_Srce_ponct);

  /*---------------------*/
  /* Lecture de la meteo */
  /*---------------------*/
  printf("   Lecture des donnees meteorologiques -> ");
  fflush(stdout);

  name=OuvreFichier(Don.name_meteo,"r");
  for(i=0;i<Don.N_Temps;i++){
    /* Lecture de la date */
    fscanf(name,"%lf/%lf/%lf %lf:%lf",
	   &Ite[i].date.jm,&Ite[i].date.m,&Ite[i].date.a,
	   &Ite[i].date.h,&Ite[i].date.min);

    /* Lecture de la vitesse du vent */
    /* Vent geostrophique */
    if(Don.type_mesure_vent==0){
      fscanf(name,"%lf %lf %lf",&Ite[i].met.Ug,
	     &Ite[i].met.Angle,&Ite[i].met.To);
   
      Test_Valid(Ite[i].met.Ug,&Ite[i].valid);
      Test_Valid(Ite[i].met.Angle,&Ite[i].valid);
      Test_Valid(Ite[i].met.To,&Ite[i].valid);
    }
    /* Vent dans la CLS */
    else if(Don.type_mesure_vent==1){
      fscanf(name,"%lf %lf %lf",&Ite[i].met.Uext,
	     &Ite[i].met.Angle,&Ite[i].met.To);
     
      Test_Valid(Ite[i].met.Uext,&Ite[i].valid);
      Test_Valid(Ite[i].met.Angle,&Ite[i].valid);
      Test_Valid(Ite[i].met.To,&Ite[i].valid);
    } 
   /*  printf("%lf/%lf/%lf\n",Ite[i].met.Uext,Ite[i].met.Angle,Ite[i].met.To); */
    /* Lecture de la couverture nuageuse */
    if(Don.type_meteo==1){
      fscanf(name,"%lf",&Ite[i].met.Cld);
      Test_Valid(Ite[i].met.Cld,&Ite[i].valid);
    }
    /* Lecture des precipitations */
    if(Don.precipit==1){
      fscanf(name,"%lf",&Ite[i].met.Precip);
      Test_Valid(Ite[i].met.Precip,&Ite[i].valid);
    }
    /* Lecture de la hauteur de melange */
    if(Don.type_haut==0){
      fscanf(name,"%lf",&Ite[i].met.h);
      Test_Valid(Ite[i].met.h,&Ite[i].valid);
    }
    /* Lecture de la classe de Pasquill */
    if(Don.type_disp==1) fscanf(name,"%s",&Ite[i].met.Pasq);
  }
  FermeFichier(name);
  printf("OK\n");

  /*--------------------------*/
  /* Lecture de la meteo DIAG */
  /*--------------------------*/    
  printf("   Lecture des donnees meteorologiques diagnostiques -> ");
  fflush(stdout);
  name=OuvreFichier(Don.name_diag_mto,"r");

  for(i=0;i<Don.N_Temps;i++){

    /* Lecture de la date */
    fscanf(name,"%lf %lf/%lf/%lf %lf:%lf",&pas_temps,
                 &jour,&mois,&annee,&heure,&minute);
		 
    /* Lecture de la vitesse du vent */
    
        /* Vent geostrophique */
    if(Don.type_mesure_vent==0){
      fscanf(name,"%lf",&Ite[i].met.Ug);   
      Test_Valid(Ite[i].met.Ug,&Ite[i].valid);
    }    
    /* Vent dans la CLS */
    else if(Don.type_mesure_vent==1){
      fscanf(name,"%lf %lf %lf %lf %lf %lf %lf",
                   &Ite[i].met.Uext,
	           &Ite[i].met.Angle,
		   &Ite[i].met.Uh,
		   &Ite[i].met.To,
	           &Ite[i].met.h,
		   &Ite[i].met.us,
		   &Ite[i].met.sigmatheta);
     
      Test_Valid(Ite[i].met.Uext,&Ite[i].valid);
      Test_Valid(Ite[i].met.Angle,&Ite[i].valid);
      Test_Valid(Ite[i].met.Uh,&Ite[i].valid);      
      Test_Valid(Ite[i].met.To,&Ite[i].valid);
      Test_Valid(Ite[i].met.h,&Ite[i].valid);
      Test_Valid(Ite[i].met.us,&Ite[i].valid);
      Test_Valid(Ite[i].met.sigmatheta,&Ite[i].valid);            
    } 
     
    /* Lecture de la couverture nuageuse */
    if(Don.type_meteo==1){
      fscanf(name,"%lf %lf %lf %lf",
                   &Ite[i].met.Cld,
		   &Ite[i].met.Fo,
		   &Ite[i].met.Lmo,
		   &Ite[i].met.thetas);		   		   
      Test_Valid(Ite[i].met.Cld,&Ite[i].valid);
      Test_Valid(Ite[i].met.Fo,&Ite[i].valid);      
      Test_Valid(Ite[i].met.Lmo,&Ite[i].valid);      
      Test_Valid(Ite[i].met.thetas,&Ite[i].valid);      
    }

    /* Lecture de la classe de Pasquill */
    /*if(Don.type_disp==1) fscanf(name,"%s",&Ite[i].met.Pasq);*/        
    /*if(Don.type_disp==1) fprintf(fich,"\t%c",Ite[i].met.Pasq);*/
  
    /* Lecture des constantes chimiques  */  
    if(Don.Mod_Cste_Chim==1) {
    /* 05/10/2010: lit pour tous les polluants
    if(Don.Type_Pol==1 && Don.Mod_Cste_Chim==1) {  */  
      fscanf(name,"%lf %lf",&cste_k1,&cste_k3);		   		   
    /*Test_Valid(Ite[i].k1,&Ite[i].valid);
      Test_Valid(Ite[i].k3,&Ite[i].valid);      */
  }
  
  printf("%.2d/%.2d/%.4d %.2d:%.2d",(int) jour,(int) mois,(int) annee,(int) heure,(int) minute);           
  printf("\tTemp= %.2f degC",Ite[i].met.To);
  printf("\th= %.2f ",Ite[i].met.h);
  printf("\tLmo= %.2f \n",Ite[i].met.Lmo);
  
  }
  FermeFichier(name);
  printf("OK\n");  

  /* Lecture de la modulation temporelle de l'emission des rues */
  /*------------------------------------------------------------*/
  printf("   Lecture des donnees d'emission des rues -> ");
  fflush(stdout);
  name=OuvreFichier(Don.name_evol_emis_rue,"r");
  for(i=0;i<Don.N_Temps;i++){
    fscanf(name,"%lf/%lf/%lf %lf:%lf %lf %s",&jour,
	   &mois,&annee,&heure,&minute,&Ite[i].Mod_rue,Ite[i].name_emisrue);
	   
    /*  printf("%lf/%lf/%lf %lf:%lf %lf %s",&jour,
	   &mois,&annee,&heure,&minute,&Ite[i].Mod_rue,Ite[i].name_emisrue);
	      
      printf("%lf/%lf/%lf %lf:%lf %lf %s",&Ite[i].date.jm,
	   &Ite[i].date.m,&Ite[i].date.a,&Ite[i].date.h,&Ite[i].date.min,&Ite[i].Mod_rue,Ite[i].name_emisrue);
	   */   	   
	   
    if(fabs(Ite[i].date.jm-jour)>1.0e-5 || fabs(Ite[i].date.m-mois)>1.0e-5 ||
       fabs(Ite[i].date.a-annee)>1.0e-5 || fabs(Ite[i].date.h-heure)>1.0e-5 ||
       fabs(Ite[i].date.min-minute)>1.0e-5){
      printf("%d",i);
      Erreur("Date incoherente dans le fichier de modulation des emissions des rues ",0);
    }
  }
  FermeFichier(name);
  printf("OK\n");

  /* Lecture de la modulation temporelle de l'emission des noeuds */
  /*--------------------------------------------------------------*/
  printf("   Lecture des donnees d'emission des noeuds -> ");
  fflush(stdout);
  name=OuvreFichier(Don.name_evol_emis_noeud,"r");
  for(i=0;i<Don.N_Temps;i++){
/*     printf("%d\n",i); */
    fscanf(name,"%lf/%lf/%lf %lf:%lf %lf %s",&jour,
	   &mois,&annee,&heure,&minute,&Ite[i].Mod_noeud,Ite[i].name_emisnoeud);
    if(fabs(Ite[i].date.jm-jour)>1.0e-5 || fabs(Ite[i].date.m-mois)>1.0e-5 ||
       fabs(Ite[i].date.a-annee)>1.0e-5 || fabs(Ite[i].date.h-heure)>1.0e-5 ||
       fabs(Ite[i].date.min-minute)>1.0e-5) {
      Erreur("Date incoherente dans le fichier de modulation des emissions des noeuds",0);
      printf("%d",i); };
  }
  FermeFichier(name);
  printf("OK\n");

  /* Lecture de la modulation temporelle de l'emission des sources ponctuelles */
  /*---------------------------------------------------------------------------*/
  printf("   Lecture des donnees d'emission des sources ponctuelles -> ");
  fflush(stdout);
  name=OuvreFichier(Don.name_evol_emis_ponct,"r");
  for(i=0;i<Don.N_Temps;i++){
    fscanf(name,"%lf/%lf/%lf %lf:%lf %lf %s",&jour,
	   &mois,&annee,&heure,&minute,&Ite[i].Mod_ponct,Ite[i].name_emisponct);
    if(fabs(Ite[i].date.jm-jour)>1.0e-5 || fabs(Ite[i].date.m-mois)>1.0e-5 ||
       fabs(Ite[i].date.a-annee)>1.0e-5 || fabs(Ite[i].date.h-heure)>1.0e-5 ||
       fabs(Ite[i].date.min-minute)>1.0e-5)
      Erreur("Date incoherente dans le fichier de modulation des emissions ponctuelles",0);
  }
  FermeFichier(name);
  printf("OK\n");

  /* Lecture de la pollution de fond */
  /*---------------------------------*/
  printf("   Lecture des donnees de pollution de fond -> ");
  fflush(stdout);
  name=OuvreFichier(Don.name_pollu,"r");
  /* Polluant passif ou particules */
  if(Don.Type_Pol==0 || Don.Type_Pol==2){
    for(i=0;i<Don.N_Temps;i++){
      fscanf(name,"%lf/%lf/%lf %lf:%lf %lf",&jour,
	     &mois,&annee,&heure,&minute,&Ite[i].Cext);
      if(fabs(Ite[i].date.jm-jour)>1.0e-5 || fabs(Ite[i].date.m-mois)>1.0e-5 ||
	 fabs(Ite[i].date.a-annee)>1.0e-5 || fabs(Ite[i].date.h-heure)>1.0e-5 ||
	 fabs(Ite[i].date.min-minute)>1.0e-5)
	Erreur("Date incoherente dans le fichier de pollution de fond",0);
      /* Test de la validite des donnees  */
      if(Ite[i].Cext==9999)
	Ite[i].valid=0;
    }
  }
  /* Chimie activee */
  else if(Don.Type_Pol==1){
    for(i=0;i<Don.N_Temps;i++){
      fscanf(name,"%lf/%lf/%lf %lf:%lf %lf %lf %lf",&jour,
	     &mois,&annee,&heure,&minute,
	     &Ite[i].Cb_NO,&Ite[i].Cb_NO2,&Ite[i].Cb_O3);
      /* Calcul de la concentration en NOx */
      Ite[i].Cext=Ite[i].Cb_NO*M_NO2/M_NO+Ite[i].Cb_NO2;
      if(fabs(Ite[i].date.jm-jour)>1.0e-5 || fabs(Ite[i].date.m-mois)>1.0e-5 ||
	 fabs(Ite[i].date.a-annee)>1.0e-5 || fabs(Ite[i].date.h-heure)>1.0e-5 ||
	 fabs(Ite[i].date.min-minute)>1.0e-5)
	Erreur("Date incoherente dans le fichier de pollution de fond",0);
      /* Test de la validite des donnees  */
      if(Ite[i].Cb_NO==9999 || Ite[i].Cb_NO2==9999 || Ite[i].Cb_O3==9999)
	Ite[i].valid=0;
    }
  }
  FermeFichier(name);
  printf("OK\n");

  /* Lecture des positions des recepteurs ponctuels */
  /*------------------------------------------------*/
  printf("   Lecture des positions des recepteurs ponctuels -> ");
  fflush(stdout);
  name=OuvreFichier(Don.name_recept,"r");
    fscanf(name,"%d",&(Don.N_Recept));
    Don.Rec=calloc(Don.N_Recept,sizeof(Recept));
    for(i=0;i<Don.N_Recept;i++){
      fscanf(name,"%s %lf %lf",Don.Rec[i].Id,&(Don.Rec[i].x),&(Don.Rec[i].y));
    }
  FermeFichier(name);
  printf("OK\n");
  printf("      Nombre de recepteurs ponctuels = %d\n",Don.N_Recept);

  /* Lecture des parametres pour les calculs statistiques */
  /*------------------------------------------------------*/
  printf("   Lecture des parametres pour les calculs statistiques -> ");
  fflush(stdout);
  name=OuvreFichier(Don.name_stat,"r");
  /* Polluant passif ou particules */
  if(Don.Type_Pol==0 || Don.Type_Pol==2){
    fscanf(name,"%d",&(Don.N_Percentiles));
    Don.Percentiles=calloc(Don.N_Percentiles,sizeof(DBL));
    for(i=0;i<Don.N_Percentiles;i++){
      fscanf(name,"%lf",&(Don.Percentiles[i]));
    }
    fscanf(name,"%d",&(Don.N_Seuils));
    Don.Seuils=calloc(Don.N_Seuils,sizeof(DBL));
    for(i=0;i<Don.N_Seuils;i++){
      fscanf(name,"%lf",&(Don.Seuils[i]));
    }
  }
  /* Chimie activee */
  else if(Don.Type_Pol==1){
    fscanf(name,"%d",&(Don.N_Percentiles));
    Don.Percentiles=calloc(Don.N_Percentiles,sizeof(DBL));
    Don.Percentiles_NO=calloc(Don.N_Percentiles,sizeof(DBL));
    Don.Percentiles_NO2=calloc(Don.N_Percentiles,sizeof(DBL));
    Don.Percentiles_O3=calloc(Don.N_Percentiles,sizeof(DBL));
    for(i=0;i<Don.N_Percentiles;i++){
      fscanf(name,"%lf %lf %lf %lf",&(Don.Percentiles[i]),
	     &(Don.Percentiles_NO[i]),&(Don.Percentiles_NO2[i]),
	     &(Don.Percentiles_O3[i]));
    }
    fscanf(name,"%d",&(Don.N_Seuils));
    Don.Seuils=calloc(Don.N_Seuils,sizeof(DBL));
    Don.Seuils_NO=calloc(Don.N_Seuils,sizeof(DBL));
    Don.Seuils_NO2=calloc(Don.N_Seuils,sizeof(DBL));
    Don.Seuils_O3=calloc(Don.N_Seuils,sizeof(DBL));
    for(i=0;i<Don.N_Seuils;i++){
      fscanf(name,"%lf %lf %lf %lf",&(Don.Seuils[i]),&(Don.Seuils_NO[i]),
	     &(Don.Seuils_NO2[i]),&(Don.Seuils_O3[i]));
    }
  }
  FermeFichier(name);
  printf("OK\n");

  /* Creation des repertoires de sortie */
  /*------------------------------------*/
  printf("\n   Creation des repertoires\n");
  CreationRepertoire(Don.name_dir_resul);
  if(Don.champ>=1)
    CreationRepertoire(Don.name_dir_Surfer);

  /*----------------------------------*/
  *NN=N;
  *RR=R;
  *II=I;
  *Iter=Ite;

}


/*---------------------------------------------*/


void lecemis(Noeud **N,Rue **R,Source_Ponct **I,DonIter Iter)
/*----------------------------*/
/* Ecriture des resultats par */
/* pas de temps ou par rue    */
/*----------------------------*/
{
  int i,j,nr,l,trouve;
  DBL Qsrce;
  char buffer[10];
  FILE *name;

  /* Initialisation des emissions sur la grille */
  /*--------------------------------------------*/
  for(i=0;i<Don.grd.Nx*Don.grd.Ny;i++)
    Don.cell[i].Qrue_nd=0.0;

  /* Lecture des emissions des rues */
  /*--------------------------------*/
  /* Initialisation a zero */
  for(i=0;i<Don.N_Rue;i++)
    (*R)[i].Qsrce=0.0;
  /* Lecture des donnees */
  name=OuvreFichier(Iter.name_emisrue,"r");
  fscanf(name,"%d",&nr);
  for(i=0;i<nr;i++){
    fscanf(name,"%s %lf",buffer,&Qsrce);
    /* Recherche de la rue correspondante */
    trouve=0;
    l=strlen(buffer);
    for(j=0;j<Don.N_Rue && trouve==0;j++){
      if(!strncmp((*R)[j].Id,buffer,IMAX(strlen((*R)[j].Id),l))){
	(*R)[j].Qsrce+=Iter.Mod_rue*Qsrce;
	trouve=1;
	/* Affectation a la maille de grille correspondante */
	if((*R)[j].Cell_i!=-1)	
	  Don.cell[(*R)[j].Cell_i].Qrue_nd+=(*R)[j].Qsrce;
      }
    }
    if(trouve==0) Erreur("Emission d'une rue non affectee",1);
  }
  FermeFichier(name);

  /* Lecture des emissions des noeuds */
  /*----------------------------------*/
  /* Initialisation a zero */
  for(i=0;i<Don.N_Noeud;i++)
    (*N)[i].Qsrce=0.0;
  /* Lecture des donnees */
  name=OuvreFichier(Iter.name_emisnoeud,"r");
  fscanf(name,"%d",&nr);
  for(i=0;i<nr;i++){
    fscanf(name,"%s %lf",buffer,&Qsrce);
    /* Recherche du noeud correspondant */
    trouve=0;
    l=strlen(buffer);
    for(j=0;j<Don.N_Noeud && trouve==0;j++){
      if(!strncmp((*N)[j].Id,buffer,IMAX(strlen((*N)[j].Id),l))){
	(*N)[j].Qsrce+=Iter.Mod_noeud*Qsrce;
	trouve=1;
	/* Affectation a la maille de grille correspondante */
	if((*N)[j].Cell_i!=-1)	
	  Don.cell[(*N)[j].Cell_i].Qrue_nd+=(*N)[j].Qsrce;
      }
    }
    if(trouve==0) Erreur("Emission d'un noeud non affectee",1);
  }
  FermeFichier(name);

  /* Lecture des emissions des sources ponctuelles */
  /*-----------------------------------------------*/
  /* Initialisation a zero */
  for(i=0;i<Don.N_Srce_ponct;i++)
    (*I)[i].Qsrce=0.0;
  /* Lecture des donnees */
  name=OuvreFichier(Iter.name_emisponct,"r");
  fscanf(name,"%d",&nr);
  for(i=0;i<nr;i++){
    fscanf(name,"%s %lf",buffer,&Qsrce);
    /* Recherche de la source ponctuelle correspondante */
    trouve=0;
    l=strlen(buffer);
    for(j=0;j<Don.N_Srce_ponct && trouve==0;j++){
      if(!strncmp((*I)[j].Id,buffer,IMAX(strlen((*I)[j].Id),l))){
	(*I)[j].Qsrce+=Iter.Mod_ponct*Qsrce;
	trouve=1;
      }
    }
    if(trouve==0){
      /*DEBUG Atmo RA*/
      printf("buffer: %s\n",buffer);
      Erreur("Emission d'une source ponctuelle non affectee:",1);
    }
  }
  FermeFichier(name);
}


/*---------------------------------------------*/


void Test_Valid(DBL Value,int *Valid)
/*-------------------------------------------*/
/* Teste si un parametre meteo a une valeur  */
/* valide (differente de 9999). Si la valeur */
/* est non valide, on met Valid a 0.         */
/*-------------------------------------------*/
{
  if(Value>9998.99 && Value<9999.01) *Valid=0;
}


/*---------------------------------------------*/
