/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Io-Don.c --> Lecture du fichier de          */
/*              parametres                     */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"
#include "Donnees.h"


/*---------------------------------------------*/


void lecdongene(char *fichier,Grid *Grd,int affich)
/*--------------------------------------------*/
/* Lecture des donnees generales              */
/* Moteur de lecture traitant toutes ou une   */
/* partie des lignes dans un ordre quelconque */
/* Les lignes de commentaires sont possibles  */
/*--------------------------------------------*/
{
  int i=0;
  char ligne[100],mot[100];
  char *car=NULL;
  FILE *name;

  /* Ouverture du fichier */
  name=OuvreFichier(fichier,"r");

  /* Lecture du fichier ligne par ligne */
  if(affich==1){
    printf("\n\t----------------------------------------------------------\n");
    printf("\t|             Listing du fichier utilisateur             |\n");    
    printf("\t----------------------------------------------------------\n");
  }
  while(fgets(ligne,100,name)!=NULL){
    /* Impression ecran de la ligne */
    if(affich==1) printf("\t%s",ligne);
    /* Si la ligne commence par un / c'est un */
    /* commentaire donc a ne pas analyser     */
    if(ligne[0]!='/'){
      i=0;
      /* Constitution du mot-cle */
      /* "mot" contient le mot-cle a tester */
      /* "car" contient la valeur associee  */
      while(ligne[i]!='='&&i<99){
	mot[i]=ligne[i];
	car=ligne+i+2;
	i++;
      }
      mot[i]='\0';
      /* Comparaison du mots cle avec les mots      */
      /* contenus dans la base de donnees           */
      /* voir fichier Donnee.h pour leur definition */

      if(!strncmp(mot,FICH_RUE,strlen(FICH_RUE))){
	/* Fichier de rue */
	i=1;
	while(car[i]!='\n'&&car[i]!='\r'&&i<=100){
	  Don.name_rue[i-1]=car[i];
	  i++;
	}
	Don.name_rue[i-1]='\0';
      }
      else if(!strncmp(mot,FICH_NOEUD,strlen(FICH_NOEUD))){
	/* Fichier de noeud */
	i=1;
	while(car[i]!='\n'&&car[i]!='\r'&&i<=100){
	  Don.name_noeud[i-1]=car[i];
	  i++;
	}
	Don.name_noeud[i-1]='\0';
      }
      else if(!strncmp(mot,FICH_NOEUD,strlen(FICH_NOEUD))){
	/* Fichier de noeud */
	i=1;
	while(car[i]!='\n'&&car[i]!='\r'&&i<=100){
	  Don.name_noeud[i-1]=car[i];
	  i++;
	}
	Don.name_noeud[i-1]='\0';
      }
      else if(!strncmp(mot,FICH_SOURCES_PONCT,strlen(FICH_SOURCES_PONCT))){
	/* Fichier de sources ponctuelles  */
	i=1;
	while(car[i]!='\n'&&car[i]!='\r'&&i<=100){
	  Don.name_srce_ponct[i-1]=car[i];
	  i++;
	}
	Don.name_srce_ponct[i-1]='\0';
      }
      else if(!strncmp(mot,FICH_EVOL_EMIS_RUE,strlen(FICH_EVOL_EMIS_RUE))){
	/* Fichier d'emission des rues */
	i=1;
	while(car[i]!='\n'&&car[i]!='\r'&&i<=100){
	  Don.name_evol_emis_rue[i-1]=car[i];
	  i++;
	}
	Don.name_evol_emis_rue[i-1]='\0';
      }
      else if(!strncmp(mot,FICH_EVOL_EMIS_NOEUD,strlen(FICH_EVOL_EMIS_NOEUD))){
	/* Fichier d'emission des noeuds */
	i=1;
	while(car[i]!='\n'&&car[i]!='\r'&&i<=100){
	  Don.name_evol_emis_noeud[i-1]=car[i];
	  i++;
	}
	Don.name_evol_emis_noeud[i-1]='\0';
      }
      else if(!strncmp(mot,FICH_EVOL_EMIS_PONCT,strlen(FICH_EVOL_EMIS_PONCT))){
	/* Fichier d'emission ponctuelles */
	i=1;
	while(car[i]!='\n'&&car[i]!='\r'&&i<=100){
	  Don.name_evol_emis_ponct[i-1]=car[i];
	  i++;
	}
	Don.name_evol_emis_ponct[i-1]='\0';
      }
      else if(!strncmp(mot,FICH_METEO,strlen(FICH_METEO))){
	/* Fichier meteo */
	i=1;
	while(car[i]!='\n'&&car[i]!='\r'&&i<=100){
	  Don.name_meteo[i-1]=car[i];
	  i++;
	}
	Don.name_meteo[i-1]='\0';
      }
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
      else if(!strncmp(mot,FICH_POLLU,strlen(FICH_POLLU))){
	/* Fichier de pollution de fond */
	i=1;
	while(car[i]!='\n'&&car[i]!='\r'&&i<=100){
	  Don.name_pollu[i-1]=car[i];
	  i++;
	}
	Don.name_pollu[i-1]='\0';
      }
      else if(!strncmp(mot,FICH_DIR_SURFER,strlen(FICH_DIR_SURFER))){
	/* Repertoire d'ecriture des fichiers de champs */
	i=1;
	while(car[i]!='\n'&&car[i]!='\r'&&i<=100){
	  Don.name_dir_Surfer[i-1]=car[i];
	  i++;
	}
	Don.name_dir_Surfer[i-1]='\0';
      }
      else if(!strncmp(mot,FICH_DIR_RESUL,strlen(FICH_DIR_RESUL))){
	/* Repertoire d'ecriture des resultats */
	i=1;
	while(car[i]!='\n'&&car[i]!='\r'&&i<=100){
	  Don.name_dir_resul[i-1]=car[i];
	  i++;
	}
	Don.name_dir_resul[i-1]='\0';
      }
      else if(!strncmp(mot,FICH_RECEPT,strlen(FICH_RECEPT))){
	/* Fichier de position des recepteurs ponctuels */
	i=1;
	while(car[i]!='\n'&&car[i]!='\r'&&i<=100){
	  Don.name_recept[i-1]=car[i];
	  i++;
	}
	Don.name_recept[i-1]='\0';
      }
      else if(!strncmp(mot,FICH_STAT,strlen(FICH_STAT))){
	/* Fichier de definition des parametres statistiques */
	i=1;
	while(car[i]!='\n'&&car[i]!='\r'&&i<=100){
	  Don.name_stat[i-1]=car[i];
	  i++;
	}
	Don.name_stat[i-1]='\0';
      }
      else if(!strncmp(mot,N_TEMPS,strlen(N_TEMPS))){
	/* Nombre de pas de temps */
	Don.N_Temps=atoi(car);
      }
      else if(!strncmp(mot,ITER_DEB,strlen(ITER_DEB))){
	/* Pas de temps de debut */
	Don.iter_deb=atoi(car);
      }
      else if(!strncmp(mot,ITER_FIN,strlen(ITER_FIN))){
	/* Pas de temps de fin */
	Don.iter_fin=atoi(car);
      }
      else if(!strncmp(mot,SEQUENT,strlen(SEQUENT))){
	/* Donnees sequentielles */
	Don.sequent=atoi(car);
      }
      else if(!strncmp(mot,CALC_DISP,strlen(CALC_DISP))){
	/* Activation du modele de dispersion */
	Don.calc_disp=atoi(car);
      }
      else if(!strncmp(mot,TYPE_METEO,strlen(TYPE_METEO))){
	/* Conditions meteorologiques */
	Don.type_meteo=atoi(car);
      }
      else if(!strncmp(mot,TYPE_MES_VENT,strlen(TYPE_MES_VENT))){
	/* Type de mesure du vent */
	Don.type_mesure_vent=atoi(car);
      }
      else if(!strncmp(mot,PRECIPIT,strlen(PRECIPIT))){
	/* Prise en compte des precipitations */
	Don.precipit=atoi(car);
      }
      else if(!strncmp(mot,LAT,strlen(LAT))){
	/* Latitude */
	Don.Lat=atof(car);
      }
      else if(!strncmp(mot,ALB,strlen(ALB))){
	/* Albedo */
	Don.Alb=atof(car);
      }
      else if(!strncmp(mot,EMISSIVITE,strlen(EMISSIVITE))){
	/* Emissivite */
	Don.Emissivite=atof(car);
      }
      else if(!strncmp(mot,PRIESTLEY_TAYLOR,strlen(PRIESTLEY_TAYLOR))){
	/* Coefficient de Priestley-Taylor */
	Don.Priestley_Taylor_lu=atof(car);
      }
      else if(!strncmp(mot,TYPE_HAUT,strlen(TYPE_HAUT))){
	/* Calcul de la hauteur de la CLA */
	Don.type_haut=atoi(car);
      }
      else if(!strncmp(mot,GRADTEXT,strlen(GRADTEXT))){
	/* Gradient thermique dans l'atmosphere libre */
	Don.gradText=atof(car);
      }
      else if(!strncmp(mot,P0,strlen(P0))){
	/* Pression de reference */
	Don.Po=atof(car);
      }
      else if(!strncmp(mot,LMO_MIN,strlen(LMO_MIN))){
	/* Longueur de Monin-Obukhov minimale */
	Don.Lmo_min=atof(car);
      }
      else if(!strncmp(mot,U_MIN,strlen(U_MIN))){
	/* Vitesse du vent minimale */
	Don.U_min=atof(car);
      }
      else if(!strncmp(mot,SIGMAV_MIN,strlen(SIGMAV_MIN))){
	/* Ecart-type de vitesse sigmav minimal */
	Don.sigmav_min=atof(car);
      }
      else if(!strncmp(mot,SIGMAW_MIN,strlen(SIGMAW_MIN))){
	/* Ecart-type de vitesse sigmaw minimal */
	Don.sigmaw_min=atof(car);
      }
      else if(!strncmp(mot,ZEXT,strlen(ZEXT))){
	/* Altitude du vent mesure */
	Don.zext=atof(car);
      }
      else if(!strncmp(mot,Z0DEXT,strlen(Z0DEXT))){
	/* Rugosite aerodynamique du site de mesure */
	Don.z0dext=atof(car);
      }
      else if(!strncmp(mot,DEXT,strlen(DEXT))){
	/* Epaisseur de deplacement du site de mesure */
	Don.dext=atof(car);
      }
      else if(!strncmp(mot,Z0D,strlen(Z0D))){
	/* Rugosite aerodynamique du quartier */
	Don.z0d=atof(car);
      }
      else if(!strncmp(mot,ZD,strlen(ZD))){
	/* Epaisseur de deplacement du quartier */
	Don.d=atof(car);
      }
      else if(!strncmp(mot,HMOY,strlen(HMOY))){
	/* Hauteur moyenne des batiments */
	Don.Hmoy=atof(car);
      }
      else if(!strncmp(mot,TYPE_POL,strlen(TYPE_POL))){
	/* Type de polluant */
	Don.Type_Pol=atoi(car);
      }
      else if(!strncmp(mot,TAUX_NO2,strlen(TAUX_NO2))){
	/* Taux de NO2 a l'emission */
	Don.Taux_NO2=atof(car);
      }
      else if(!strncmp(mot,MOD_CSTE_CHIM,strlen(MOD_CSTE_CHIM))){
	/* Modele de constantes chimiques */
	Don.Mod_Cste_Chim=atoi(car);
      }
      else if(!strncmp(mot,K1K3,strlen(K1K3))){
	/* Rapport k1/k3 */
	Don.k1k3=atof(car);
      }
      else if(!strncmp(mot,DIAM_PART,strlen(DIAM_PART))){
	/* Diametre moyen des particules */
	Don.Diam_part=atof(car);
      }
      else if(!strncmp(mot,RHO_PART,strlen(RHO_PART))){
	/* Masse volumique des particules */
	Don.rho_part=atof(car);
      }
      else if(!strncmp(mot,LESSIV_A,strlen(LESSIV_A))){
	/* Constante a du modele de lessivage */
	Don.lessiv_a=atof(car);
      }
      else if(!strncmp(mot,LESSIV_B,strlen(LESSIV_B))){
	/* Constante b du modele de lessivage */
	Don.lessiv_b=atof(car);
      }
      else if(!strncmp(mot,TYPE_DISP,strlen(TYPE_DISP))){
	/* Modele de diffusion */
	Don.type_disp=atoi(car);
      }
      else if(!strncmp(mot,DI,strlen(DI))){
	/* Diffusivite turbulente */
	Don.Di=atof(car);
      }
      else if(!strncmp(mot,SIGMATHETA,strlen(SIGMATHETA))){
	/* Sigma theta */
	Don.sigmatheta=atof(car);
      }
      else if(!strncmp(mot,Z0D_BAT,strlen(Z0D_BAT))){
	/* Rugosite aerodynamique des batiments */
	Don.z0d_bat=atof(car);
      }
      else if(!strncmp(mot,AFFICH,strlen(AFFICH))){
	/* Niveau d'affichage */
	Don.affich=atoi(car);
      }
      else if(!strncmp(mot,CALCSTAT,strlen(CALCSTAT))){
	/* Calcul des statistiques */
	Don.calc_stat=atoi(car);
      }
      else if(!strncmp(mot,ORDRE,strlen(ORDRE))){
	/* Classement des resultats */
	Don.ordre=atoi(car);
      }
      else if(!strncmp(mot,CHAMP,strlen(CHAMP))){
	/* Sortie des champs */
	Don.champ=atoi(car);
      }
      else if(!strncmp(mot,NX_OUT,strlen(NX_OUT))){
	/* Nombre de point selon X du maillage de sortie */
	Grd->Nx=atoi(car);
      }
      else if(!strncmp(mot,NY_OUT,strlen(NY_OUT))){
	/* Nombre de point selon Y du maillage de sortie */
	Grd->Ny=atoi(car);
      }
      else if(!strncmp(mot,XMIN_OUT,strlen(XMIN_OUT))){
	/* Abscisse minimum du maillage de sortie */
	Grd->xmin=atof(car);
      }
      else if(!strncmp(mot,XMAX_OUT,strlen(XMAX_OUT))){
	/* Abscisse maximum du maillage de sortie */
	Grd->xmax=atof(car);
      }
      else if(!strncmp(mot,YMIN_OUT,strlen(YMIN_OUT))){
	/* Ordonnee minimum du maillage de sortie */
	Grd->ymin=atof(car);
      }
      else if(!strncmp(mot,YMAX_OUT,strlen(YMAX_OUT))){
	/* Ordonnee maximum du maillage de sortie */
	Grd->ymax=atof(car);
      }
      else if(!strncmp(mot,GRD_RATIO,strlen(GRD_RATIO))){
	/* Ratio d'interpolation de la grille de sortie */
	Don.GrdRatio=atoi(car);
      }
      else if(!strncmp(mot,NX_CALC,strlen(NX_CALC))){
	/* Nombre de cellules selon X du maillage de calcul */
	Don.grd.Nx=atoi(car);
      }
      else if(!strncmp(mot,NY_CALC,strlen(NY_CALC))){
	/* Nombre de cellules selon Y du maillage de calcul */
	Don.grd.Ny=atoi(car);
      }
      else if(!strncmp(mot,XMIN_CALC,strlen(XMIN_CALC))){
	/* Abscisse minimum du maillage de calcul */
	Don.grd.xmin=atof(car);
      }
      else if(!strncmp(mot,XMAX_CALC,strlen(XMAX_CALC))){
	/* Abscisse maximum du maillage de calcul */
	Don.grd.xmax=atof(car);
      }
      else if(!strncmp(mot,YMIN_CALC,strlen(YMIN_CALC))){
	/* Ordonnee minimum du maillage de calcul */
	Don.grd.ymin=atof(car);
      }
      else if(!strncmp(mot,YMAX_CALC,strlen(YMAX_CALC))){
	/* Ordonnee maximum du maillage de calcul */
	Don.grd.ymax=atof(car);
      }
      else if(!strncmp(mot,N_INFL,strlen(N_INFL))){
	/* Nombre de cellules d'influence du maillage de calcul */
	Don.N_Infl=atoi(car);
      }
    }
  }
  if(affich==1){
    printf("\t----------------------------------------------------------\n");
    printf("\t|         Fin du listing du fichier utilisateur          |\n");    
    printf("\t----------------------------------------------------------\n");
  }
  FermeFichier(name);
}


/*---------------------------------------------*/
