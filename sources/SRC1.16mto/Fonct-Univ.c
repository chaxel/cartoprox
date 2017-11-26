/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Fonct-Univ.c --> Fonctions universelles de  */
/*                 la theorie de Monin-Obhukov */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"


/*---------------------------------------------*/


DBL Phim(DBL ksi)
/*----------------------------------*/
/*  Garratt, 1992                   */
/* "The atmospheric boundary layer" */
/*  page 52                         */
/*----------------------------------*/
{
  /* Cas instable */
  if(ksi<0) return pow(1.0-16.0*ksi,-0.25);
  /* Cas stable */
  else return 1.0+5.0*ksi;
}


/*---------------------------------------------*/


DBL dPhim(DBL ksi)
/*----------------------------------*/
/*  d'apres Garratt, 1992           */
/* "The atmospheric boundary layer" */
/*  page 52                         */
/*----------------------------------*/
{
  /* Cas instable */
  if(ksi<0) return 4.0*pow(1.0-16.0*ksi,-1.25);
  /* Cas stable */
  else return 5.0;
}


/*---------------------------------------------*/


DBL Psim(DBL ksi)
/*----------------------------------*/
/*  Garratt, 1992                   */
/* "The atmospheric boundary layer" */
/*  page 53                         */
/*----------------------------------*/
{
  DBL x;

  /* Cas instable */
  if(ksi<0){
    x=pow(1.0-16.0*ksi,0.25);
    return 2.0*log((1.0+x)/2.0)+log((1.0+x*x)/2.0)-2.0*atan(x)+pi/2.0;
  }
  /* Cas stable */
  else return -5.0*ksi;
}


/*---------------------------------------------*/


DBL Psih(DBL ksi)
/*----------------------------------*/
/*  Garratt, 1992                   */
/* "The atmospheric boundary layer" */
/*  page 54                         */
/*----------------------------------*/
{
  DBL x;

  /* Cas instable */
  if(ksi<0){
    x=pow(1.0-16.0*ksi,0.5);
    return 2.0*log((1.0+x)/2.0);
  }
  /* Cas stable */
  else return -5.0*ksi;
}


/*---------------------------------------------*/
