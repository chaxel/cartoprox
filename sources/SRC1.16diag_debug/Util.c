/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Util.c --> Fonctions utilitaires diverses   */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"


/*---------------------------------------------*/


int ISQR(int sqrarg)
/*-----------------*/
/* Fonction carree */
/* pour entier     */
/*-----------------*/
{
  return sqrarg*sqrarg;
}


/*---------------------------------------------*/


DBL DSQR(DBL sqrarg)
/*-----------------*/
/* Fonction carree */
/* pour DBL        */
/*-----------------*/
{
  return sqrarg*sqrarg;
}


/*---------------------------------------------*/


int IMAX(int maxarg1,int maxarg2)
/*------------------*/
/* Fonction maximum */
/* pour entier      */
/*------------------*/
{
  return ((maxarg1) > (maxarg2) ? (maxarg1) : (maxarg2));
}


/*---------------------------------------------*/


DBL DMAX(DBL maxarg1,DBL maxarg2)
/*------------------*/
/* Fonction maximum */
/* pour DBL         */
/*------------------*/
{
  return ((maxarg1) > (maxarg2) ? (maxarg1) : (maxarg2));
}


/*---------------------------------------------*/


int IMIN(int minarg1,int minarg2)
/*------------------*/
/* Fonction minimum */
/* pour entier      */
/*------------------*/
{
  return ((minarg1) < (minarg2) ? (minarg1) : (minarg2));
}


/*---------------------------------------------*/


DBL DMIN(DBL minarg1,DBL minarg2)
/*------------------*/
/* Fonction minimum */
/* pour DBL         */
/*------------------*/
{
  return ((minarg1) < (minarg2) ? (minarg1) : (minarg2));
}


/*---------------------------------------------*/

DBL DSIGN(DBL a,DBL b)
/*----------------*/
/* Fonction signe */
/*----------------*/
{
  return ((b) >= 0.0 ? fabs(a) : -fabs(a));
}


/*---------------------------------------------*/


void nrerror(char error_text[])
/*------------------------------*/	
/* Message d'erreur standard    */
/* pour les routines de         */
/* Numerical Recipes            */
/* (conserve par compatibilite) */
/*------------------------------*/	
{
  Erreur(error_text,0);
}


/*---------------------------------------------*/


void Erreur(char error_text[],int type)
/*---------------------------*/	
/* Message d'erreur standard */
/*---------------------------*/	
{
  if(type==0){
    fflush(stdout);
    fprintf(stderr,"\n#################\n");
    fprintf(stderr,"# ERREUR FATALE #\n");
    fprintf(stderr,"#################\n");
    fprintf(stderr,"%s\n",error_text);
    fprintf(stderr,"### SORTIE DU PROGRAMME ###\n\n");
    exit(1);
  }
  else if(type==1){
    fflush(stdout);
    fprintf(stderr,"\n    #############\n");
    fprintf(stderr,"    # ATTENTION #\n");
    fprintf(stderr,"    #############\n\n");
    fprintf(stderr,"---> %s\n\n",error_text);
    fprintf(stderr,"### PROBLEME A CORRIGER ###\n\n\a");
  }
}


/*---------------------------------------------*/


void BarreAvance(int indice,int N)
/*-------------------------------*/	
/* Impression ercan d'une barre  */
/* de progression avec affichage */
/* des pourcentages              */
/*-------------------------------*/	
{
  int i,pc,N_barres;

  /* Calcul ces pourcentages par pas de 5% */
  pc=((indice*20)/N)*5;
  N_barres=pc/5;
  /* Affichage des barres */
  printf("\r<");
  for(i=0;i<N_barres;i++) printf("|");
  for(i=N_barres;i<20;i++) printf(" ");
  printf("> %2d%%",pc);
  fflush(stdout);
}


/*---------------------------------------------*/


FILE *OuvreFichier(char *nom_fichier,char *mode)
/*---------------------------------*/	
/* Implementation locale de fopen  */
/* avec test de la bonne ouverture */
/*---------------------------------*/	
{
  char message[200];
  FILE *fich;

  fich=fopen(nom_fichier,mode);
  if(fich==NULL){
    sprintf(message,"Probleme d'ouverture du fichier < %s >. Verifiez le nom !\n",nom_fichier);
    Erreur(message,0);
  }
  else return fich;

  /* Pour une compilation sans warning. Ne doit pas arriver */
  return NULL;
}


/*---------------------------------------------*/


void FermeFichier(FILE *fich)
/*---------------------------------*/	
/* Implementation locale de fclose */
/* avec test de la bonne fermeture */
/*---------------------------------*/	
{
  if(fich==NULL){
    Erreur("Probleme de fermeture d'un fichier.\n",0);
  }
  else fclose(fich);
}


/*---------------------------------------------*/


void CreationRepertoire(char *path)
/*--------------------------*/	
/* Creation d'un repertoire */
/*--------------------------*/	
{
  char message[200];

  if(mkdir(path,
	   S_IRUSR | S_IWUSR | S_IXUSR |
	   S_IRGRP | S_IWGRP | S_IXGRP |
	   S_IROTH | S_IWOTH | S_IXOTH)!=0){
    if(errno==EEXIST)
      printf("   - Le repertoire < %s > existe deja\n",path);
    else{
      sprintf(message,"Probleme de creation du repertoire < %s >. Verifiez le nom !\n",path);
      Erreur(message,0);
    }
  }
  else printf("   - Le repertoire < %s > a ete cree\n",path);
}


/*---------------------------------------------*/
