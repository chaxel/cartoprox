Donnees.h

#define FICH_DIAG_METEO "Fichier meteo diagnostique"


Def.h

  /* Ajout ATMO RA */
  /*----------------------*/  
  char name_diag_mto[100];

Main.c

  /*----------------*/
  /* Ajout ATMO RA  */
  /*----------------*/
  Lire_meteo(&Iter);     

Init-Meteo-Diag.c
 On le creer

Io.c

Ajoute la boucle de lecture du fichier de meteo diagnostique


Io-Don.c

  /* Ajout ATMO RA */
  /*----------------------*/        
      else if(!strncmp(mot,FICH_DIAG_METEO,strlen(FICH_DIAG_METEO))){
	/* Fichier meteo diag Evol-meteo.dat */
	i=1;
	while(car[i]!='\n'&&car[i]!='\r'&&i<=100){
	  Don.name_diag_mto[i-1]=car[i];
	  i++;
	}
	Don.name_diag_mto[i-1]='\0';
      }   
  /* Ajout ATMO RA */
  /*----------------------*/     

  
Init-Meteo.c

On desactive le calcul des parametres lus dans Init-Meteo-Diag.c soit :
us, Lmo, h, Fo, 



Def.h 
  char name_emisrue[256];
  char name_emisnoeud[256];
  char name_emisponct[256];
  
  a la place de 
  
  char name_emisrue[100];
  char name_emisnoeud[100];
  char name_emisponct[100];

Probleme de longueur de caracteres 


Io-Resul.c

  /*----------------*/
  /* Ajout ATMO RA  */
  /*----------------*/  
  /* E. CHAXEL 2011 DESACTIVE L'ECRITURE 
  DANS LE FICHIER TEMPORAIRE 
  POUR ECRITURE DANS LE MODE ITERATION
  TROP DE I/O POUR DE GROS FICHIERS*/
  /* Fichier temporaire */
  /*fich=OuvreFichier(nom_fich_tmp,"a");
  /* Polluant passif ou particules */
  /*if(Don.Type_Pol==0 || Don.Type_Pol==2){
    for(j=0;j<N;j++){	
      fwrite(&(C[j]),sizeof(DBL),1,fich);
    }
  }
  /* Chimie NO-NO2-O3 */
  /*else if(Don.Type_Pol==1){
    for(j=0;j<N;j++){
      fwrite(&(C[j]),sizeof(DBL),1,fich);
      fwrite(&(C_NO[j]),sizeof(DBL),1,fich);
      fwrite(&(C_NO2[j]),sizeof(DBL),1,fich);
      fwrite(&(C_O3[j]),sizeof(DBL),1,fich);
    }
  }
  FermeFichier(fich);
  /* E. CHAXEL 2011 DESACTIVE L'ECRITURE 
  DANS LE FICHIER TEMPORAIRE 
  POUR ECRITURE DANS LE MODE ITERATION
  TROP DE I/O POUR DE GROS FICHIERS*/  
  /*----------------*/
  /* Ajout ATMO RA  */
  /*----------------*/
