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


void initmeteo(DonIter *Iter)
{
  DBL Re_s;

  printf("\n-- Calcul des conditions meteorologiques --\n\n");

  /*----------------*/
  /* Initialisation */
  /*----------------*/
  /* Calcul des temperatures */
  Iter->met.To+=T0abs;
  Iter->met.thetao=Iter->met.To;
  Iter->met.Nu=sqrt(g*Don.gradthetaext/Iter->met.To);

  /* Calcul de la masse volumique rho=P/(RT) */
  Iter->met.rho=Don.Po/(R_air_sec*Iter->met.To);

  /* Calcul du volume molaire Vo=M/rho en litres */
  Iter->met.Vo=1000.0*M_air_sec/Iter->met.rho;

  /* Valeur minimale de la vitesse du vent */
  /* Iter->met.Ug=DMAX(Iter->met.Ug,Don.U_min);*/
  /* Iter->met.Uext=DMAX(Iter->met.Uext,Don.U_min);*/

  /* Calcul du vecteur directeur du vent       */
  /* Angle = 0 deg -> vent provenant du nord   */
  /* Angle = 90 deg -> vent provenant de l'est */
  Iter->met.ix=-sin(Iter->met.Angle*pi/180.0);
  Iter->met.iy=-cos(Iter->met.Angle*pi/180.0);

  /* Calcul du taux de lessivage */
  if(Don.precipit==1)
    Iter->met.Taux_Lessiv=Don.lessiv_a*pow(Iter->met.Precip,Don.lessiv_b);
  else Iter->met.Taux_Lessiv=0.0;

  /* Modification du coefficient de Priestley-Taylor */
  Don.Priestley_Taylor=Don.Priestley_Taylor_lu;
  /* Cas de la pluie */
  if(Don.precipit==1 && Iter->met.Precip>0.01) Don.Priestley_Taylor=1.0;
  /* Cas de l'heure apres la pluie */
  if(Don.sequent==1 && Don.precipit==1 &&
     Iter->met.Precip<=0.01 && Iter->met.Precip_prec>0.01)
    Don.Priestley_Taylor=DMAX(Don.Priestley_Taylor_lu,0.45);

  /*---------------------------------*/
  /* Calcul de us, Lmo, thetas et Fo */
  /*---------------------------------*/
  /* Calcul desactivé dans la version DIAG, cependant elev_sol doit être calculé
  /* Us_Lmo(Iter->date.j,Iter->date.h,Iter->met.Cld,Iter->met.rho,Iter->met.To,Iter->met.Uext,
	 &(Iter->met.Ug),&(Iter->met.elev_sol),&(Iter->met.Fo),
	 &(Iter->met.us),&(Iter->met.Lmo),&(Iter->met.thetas));*/
	 
  /* Calcul de l'elevation solaire*/
  Iter->met.elev_sol=elevation_solaire(Iter->date.j,Iter->date.h);
	 
  Iter->met.ws=Iter->met.us*pow((Iter->met.h/(kappa*fabs(Iter->met.Lmo))),1.0/3.0);

  /*---------------------------------*/
  /* Calcul de la hauteur de melange */
  /*---------------------------------*/
 /* Iter->met.h=Haut_CLA(Iter->met,Iter->date);*/

  /*-------------------------------------*/
  /* Fluctuation de la direction du vent */
  /*-------------------------------------*/
  /* Cas du modele de dispersion uniforme */
  if(Don.type_disp==0) Iter->met.sigmatheta=Don.sigmatheta;
  /* Cas du modele de dispersion base */
  /* sur les classes de Pasquill      */
  else if(Don.type_disp==1) Iter->met.sigmatheta=SigmaThetaPasquill(Iter->met);
  /* Cas du modele de dispersion par */
  /* similitude (voir doc ADMS 3)    */
  else if(Don.type_disp==2) Iter->met.sigmatheta=0.065*
			 sqrt(7.0/U(10.0,Iter->met.us,Don.z0d,Iter->met.Lmo))*180.0/pi;

  /*-------------------------------------*/
  /* Vitesse moyenne au niveau des toits */
  /*-------------------------------------*/
  Iter->met.Uh=DMAX(U(Don.Hmoy-Don.d,Iter->met.us,Don.z0d,Iter->met.Lmo),Don.U_min);

  /*--------------------------------------------*/
  /* Fluctuation de vitesse au niveau des toits */
  /*--------------------------------------------*/
  /* Cas instable */
  if((Iter->met.h/Iter->met.Lmo)<-0.3){
    Iter->met.sigma_wH=sigmawinstable(Iter->met,sigmawic(Iter->met,Don.Hmoy-Don.d),Don.Hmoy-Don.d);
  }
  /* Cas stable */
  else if((Iter->met.h/Iter->met.Lmo)>1.0){
    Iter->met.sigma_wH=sigmawstable(Iter->met,Don.Hmoy-Don.d);
  }
  /* Cas neutre */
  else{
    Iter->met.sigma_wH=sigmawneutre(Iter->met,Don.Hmoy-Don.d);
  }

  /*---------------------------*/
  /* Rugosite thermique        */
  /* Voir Garratt (1992), p 93 */
  /*---------------------------*/
  Re_s=Iter->met.us*Don.z0d/nuair;
  Don.z0t=Don.z0d*exp(-kappa*(6.2*pow(Re_s,0.25)-5.0));

  /*-------------------------*/
  /* Affichage des resultats */
  /*-------------------------*/
  if(Iter->met.elev_sol<=0.0) printf("\tSituation de NUIT\t");
  else printf("\tSituation de JOUR\t");

  if(fabs(Iter->met.Lmo)>10000.0) printf("Atmosphere NEUTRE\n\n");
  else if(Iter->met.Lmo<0.0) printf("Atmosphere INSTABLE\n\n");
  else printf("Atmosphere STABLE\n");

  printf("\tTemperature\t\t\t= %.1f deg C\n",Iter->met.To-T0abs);
  printf("\tPression\t\t\t= %.0f Pa\n",Don.Po);
  printf("\tMasse volumique\t\t\t= %.2f kg/m3\n",Iter->met.rho);
  printf("\tVolume molaire\t\t\t= %.2f litres\n",Iter->met.Vo);
  printf("\tCouverture nuageuse\t\t= %.1f octas\n",Iter->met.Cld);
  printf("\tFlux de chaleur sensible\t= %.3f W/m2\n",Iter->met.Fo);
  if(fabs(Iter->met.Lmo)>10000.0)
    printf("\tLongueur de Monin-Obukhov\t= infinie\n");
  else
    printf("\tLongueur de Monin-Obukhov\t= %.2f m\n",Iter->met.Lmo);
  printf("\tVitesse de frottement\t\t= %.3f m/s\n",Iter->met.us);
  printf("\tTemperature de frottement\t= %.2f deg C\n",Iter->met.thetas);
  printf("\tVitesse au niveau des toits\t= %.1f m/s\n",Iter->met.Uh);
  printf("\tFluctuation de la direction\t= %.1f deg\n",
	 Iter->met.sigmatheta);

  if(Don.type_haut==0) printf("\tHauteur de CLA fournie\t\t= ");
  if(Don.type_haut==1) printf("\tHauteur de CLA calculee\t\t= ");
  printf("%.1f m\n",Iter->met.h);
}


/*---------------------------------------------*/


DBL SigmaThetaPasquill(Meteo met)
/*--------------------------------------*/
/* Ecarts-types de la direction du vent */
/* en fonction des classes de Pasquill  */
/*                                      */
/* Hanna, Briggs et Hosker (1982), p 28 */
/*--------------------------------------*/
{
  if(met.Pasq=='A'){
    return 25.0;
  }
  else if(met.Pasq=='B'){
    return 20.0;
  }
  else if(met.Pasq=='C'){
    return 15.0;
  }
  else if(met.Pasq=='D'){
    return 10.0;
  }
  else if(met.Pasq=='E'){
    return 5.0;
  }
  else if(met.Pasq=='F'){
    return 2.5;
  }
  else{
    Erreur("Classe de Pasquill differente de A, B, C, D, E ou F",0);
  }

  /* Pour une compilation sans warning. Ne doit pas arriver */
  return 0.0;
}


/*---------------------------------------------*/
