/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Chimie.c --> Transformations chimiques      */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"


/*---------------------------------------------*/


void Chapman(DBL Cd_NOx,DBL Cb_O3,DBL Cb_NO,DBL Cb_NO2,
	     DBL *C_NO,DBL *C_NO2,DBL *C_O3,DBL k1,DBL k3)
/*--------------------------------------*/
/* Calcul de la concentration en NO2    */
/* a partir de la concentration en NOx. */
/* On suppose une concentration de fond */
/* en O3 de Cb_O3 et des concentrations */
/* de fond Cb_NO et Cb_NO2 en NO et NO2 */
/*                                      */
/* voir These L. Soulhac (2000) p 327   */
/*--------------------------------------*/
{
  DBL b,c,k1k3;

  /* Rapport k1/k3 */
  k1k3=DMAX(k1/k3,2.0);

  /* Passage en ppb */
  Cd_NOx=Cd_NOx*Met.Vo/M_NO2;
  Cb_NO=Cb_NO*Met.Vo/M_NO;
  Cb_NO2=Cb_NO2*Met.Vo/M_NO2;
  Cb_O3=Cb_O3*Met.Vo/M_O3;

  /* Calcul */
  b=k1k3+Cb_O3+Cb_NO+2.0*Cb_NO2+(1.0+Don.Taux_NO2)*Cd_NOx;
  c=(Cb_O3+Cb_NO2+Don.Taux_NO2*Cd_NOx)*(Cb_NO+Cb_NO2+Cd_NOx);
  *C_NO2=0.5*(b-sqrt(b*b-4.0*c));
  *C_NO=-(*C_NO2)+Cb_NO+Cb_NO2+Cd_NOx;
  *C_O3=Cb_O3-(*C_NO2)+Cb_NO2+Don.Taux_NO2*Cd_NOx;

  /* Passage en microg/m3 */
  *C_NO=*C_NO/Met.Vo*M_NO;
  *C_NO2=*C_NO2/Met.Vo*M_NO2;
  *C_O3=*C_O3/Met.Vo*M_O3;
}


/*---------------------------------------------*/


DBL Cste_k1(DBL elev_sol,DBL Cld)
/*--------------------------------------*/
/* Calcul du taux de photolyse du NO2   */
/* (en /s) a partir d'une methodologie  */
/* derivee d'UAM-V. L'attenuation en    */
/* presence de nuages est derivee du    */
/* modele de Kasten et Czeplak (1980)   */
/* pour le rayonnement direct.          */
/*--------------------------------------*/
{
  DBL k1_clear;

  /* Taux de photolyse pour un ciel clair */
  k1_clear=(0.5699-pow(9.056E-3*(90.0-180.0*asin(elev_sol)/pi),2.546))/60.0;

  /* Influence de la nebulosite */
  return DMAX(k1_clear,0.0)*(1-0.75*pow(Cld/8.0,3.4));

}


/*---------------------------------------------*/


DBL Cste_k3(DBL Tkelv,DBL Vo)
/*-----------------------------------------*/
/* Calcul de la constante de la reaction : */
/* NO + O3 --> NO2 + O2                    */
/* en /s/ppb (Seinfeld, 1986, page 125)    */
/*-----------------------------------------*/
{
  /* Nombre d'Avogadro */
  DBL Na=6.022E23;

  return 2.2E-12*exp(-1430.0/Tkelv)*Na/(Vo*1000.0*1.0E9);
}


/*---------------------------------------------*/
