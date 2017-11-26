/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Util-Date.c --> Fonctions de traitement des */
/*                 dates et des heures         */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"


/*---------------------------------------------*/


void Jour_Annee(Date *date)
/*-----------------------------------*/	
/* Calcul du jour de l'annee (1-366) */
/* a partir d'une date JJ/MM/AAAA    */
/*-----------------------------------*/	
{
  int bis,i,annee,jour;
  int jour_par_mois[2][13]={{0,31,28,31,30,31,30,31,31,30,31,30,31},
			    {0,31,29,31,30,31,30,31,31,30,31,30,31}};
  
  /* Teste si l'annee est bissextile */
  /* Si oui, bis = 1 ; sinon bis = 0 */
  annee=(int) date->a;
  bis=(annee%4 == 0 && annee%100!=0) || annee%400==0;

  jour=(int) date->jm;
  for(i=1;i<((int) date->m);i++){
    jour+=jour_par_mois[bis][i];
  }
  date->j=(DBL) jour;
}


/*---------------------------------------------*/


void Jour_Semaine(Date *date)
/*-------------------------------------*/	
/* Calcul du jour de la semaine (1-7)  */
/* a partir de l'annee et du jour de   */
/* l'annee (valable pour le calendrier */
/* Gregorien, a partir de 1583)        */
/*-------------------------------------*/	
{
  int annee,jour_an,jour_sem;

  annee=(int) date->a;
  jour_an=(int) date->j;
  jour_sem=(annee+(annee-1)/4-(annee-1)/100+
	  (annee-1)/400+jour_an-2)%7;
  date->js=(DBL) jour_sem+1;

  switch(jour_sem){
  case 0:
    sprintf(date->NomJourSem,"Lundi");
    break;
  case 1:
    sprintf(date->NomJourSem,"Mardi");
    break;
  case 2:
    sprintf(date->NomJourSem,"Mercredi");
    break;
  case 3:
    sprintf(date->NomJourSem,"Jeudi");
    break;
  case 4:
    sprintf(date->NomJourSem,"Vendredi");
    break;
  case 5:
    sprintf(date->NomJourSem,"Samedi");
    break;
  case 6:
    sprintf(date->NomJourSem,"Dimanche");
    break;
  }
}


/*---------------------------------------------*/
