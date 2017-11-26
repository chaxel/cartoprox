/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Init-Meteo.c --> Initialisations des        */
/*                  parametres meteorologiques */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"


/*---------------------------------------------*/


void Lire_meteo(DonIter **Iter)
{
  DBL pas_temps;   
  DBL jour,mois,annee,heure,minute;       
  DBL cste_k1,cste_k3;    
  int i;

  FILE *name;
  DonIter *Ite;

  Ite=calloc(Don.N_Temps,sizeof(DonIter)); 
  
  for(i=0;i<Don.N_Temps;i++) Ite[i].valid=1;
    
  /*--------------------------*/
  /* Lecture de la meteo DIAG */
  /*--------------------------*/
  printf("   Lecture des donnees meteorologiques diagnostiques -> ");
  fflush(stdout);
  name=OuvreFichier(Don.name_diag_mto,"r");

      printf("Ouverture fichier %s OK\n",Don.name_diag_mto);

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
     
   /*  printf("%lf/%lf/%lf\n",Ite[i].met.Uext,Ite[i].met.Angle,Ite[i].met.To); */
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
    /* if(Don.type_disp==1) fprintf(fich,"\t%c",Ite[i].met.Pasq);*/
  
    /* Lecture des constantes chimiques */  
  if(Don.Type_Pol==1 && Don.Mod_Cste_Chim==1) {
      fscanf(name,"%lf %lf",&cste_k1,&cste_k3);		   		   
      /*Test_Valid(Ite[i].k1,&Ite[i].valid);
      Test_Valid(Ite[i].k3,&Ite[i].valid);      */
  }
  
  /**Iter.met=Ite.met;*/

  printf("%.2d/%.2d/%.4d %.2d:%.2d",(int) jour,(int) mois,(int) annee,(int) heure,(int) minute);           
  printf("\tTemp= %.2f K",Ite[i].met.To);
  printf("\th= %.2f ",Ite[i].met.h);   
  printf("\tLmo= %.2f \n",Ite[i].met.Lmo);  

  *Iter=Ite; 
  
  }
  FermeFichier(name);
  printf("OK\n");  
  
}
