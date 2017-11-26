/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Io-Surfer.c --> Gestion des entrees/sorties */
/*                 vers le logiciel SURFER     */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"


/*---------------------------------------------*/


void Ecrire_Rue_bln(Noeud *Nd,Rue *R)
/*----------------------------------*/
/* Ecriture du reseau des rues sur  */
/* un fichier au format bln pour    */
/* SURFER                           */
/*----------------------------------*/
{
  int i;
  FILE *fich;
  char fichier[100];

  printf("   Ecriture du reseau de rues -> ");
  fflush(stdout);

  /* Ouverture du fichier */
  sprintf(fichier,"%s/reseau-rues.bln",Don.name_dir_resul);
  fich=OuvreFichier(fichier,"w");

  /* Ecriture des coordonnees des rues */
  for(i=0;i<Don.N_Rue;i++){
    fprintf(fich,"%d,%d\n",2,1);
    fprintf(fich,"%.2f,%.2f\n",Nd[R[i].Deb].x,Nd[R[i].Deb].y);
    fprintf(fich,"%.2f,%.2f\n",Nd[R[i].Fin].x,Nd[R[i].Fin].y);
  }

  /* Fermeture du fichier */
  FermeFichier(fich);

  printf("OK\n");
}


/*---------------------------------------------*/


void Ecrire_Forme_Rue_bln(Noeud *Nd,Rue *R)
/*----------------------------------*/
/* Ecriture du reseau des rues sous */
/* forme de rectangles sur un       */
/* fichier au format bln pour       */
/* SURFER                           */
/*----------------------------------*/
{
  int i;
  FILE *fich;
  char fichier[100];

  printf("   Ecriture de reseau des formes de rues -> ");
  fflush(stdout);

  /* Ouverture du fichier */
  sprintf(fichier,"%s/reseau-formes-rues.bln",Don.name_dir_resul);
  fich=OuvreFichier(fichier,"w");

  /* Ecriture des coordonnees des rues */
  for(i=0;i<Don.N_Rue;i++){
    fprintf(fich,"%d,%d\n",5,1);
    fprintf(fich,"%f,%f\n",Nd[R[i].Deb].x+0.5*R[i].iy*R[i].W,
	    Nd[R[i].Deb].y-0.5*R[i].ix*R[i].W);
    fprintf(fich,"%f,%f\n",Nd[R[i].Deb].x-0.5*R[i].iy*R[i].W,
	    Nd[R[i].Deb].y+0.5*R[i].ix*R[i].W);
    fprintf(fich,"%f,%f\n",Nd[R[i].Fin].x-0.5*R[i].iy*R[i].W,
	    Nd[R[i].Fin].y+0.5*R[i].ix*R[i].W);
    fprintf(fich,"%f,%f\n",Nd[R[i].Fin].x+0.5*R[i].iy*R[i].W,
	    Nd[R[i].Fin].y-0.5*R[i].ix*R[i].W);
    fprintf(fich,"%f,%f\n",Nd[R[i].Deb].x+0.5*R[i].iy*R[i].W,
	    Nd[R[i].Deb].y-0.5*R[i].ix*R[i].W);
  }

  /* Fermeture du fichier */
  FermeFichier(fich);

  printf("OK\n");
}


/*---------------------------------------------*/


void Ecrire_grd(DBL **Conc,Grid Grd,char *suffix,int num)
/*----------------------------------*/
/* Ecriture des donnees sur fichier */
/* grille                           */
/*----------------------------------*/
{
  if(Don.champ==1)
    Ecrire_grd_Surfer(Conc,Grd,suffix,num);
  else if(Don.champ==2)
    Ecrire_grd_VerticalMapper(Conc,Grd,suffix,num);
}


/*---------------------------------------------*/


void Ecrire_grd_Surfer(DBL **Conc,Grid Grd,char *suffix,int num)
/*----------------------------------*/
/* Ecriture des donnees sur fichier */
/* grille au format SURFER          */
/*----------------------------------*/
{
  int i,j,c;
  DBL zmin,zmax;
  FILE *name;
  char fichier[100];
  char CarTmp[4];
  short ShortTmp;
  float FloatTmp;
  double DoubleTmp;

  printf("   Ecriture du champ de concentration %s -> ",suffix);
  fflush(stdout);

  /* Calcul du minimum et du maximum */
  /* de concentration sur la grille  */
  zmin=1000000.0;
  zmax=-1000000.0;
  for(j=0;j<Grd.Ny;j++){
    for(i=0;i<Grd.Nx;i++){
      zmin=DMIN(zmin,Conc[i][j]);
      zmax=DMAX(zmax,Conc[i][j]);
    }
  }

  /* Ouverture du fichier */
  if(num<0) sprintf(fichier,"%s/concentr-%s.grd",Don.name_dir_Surfer,suffix);
  else sprintf(fichier,"%s/concentr-%s-%d.grd",Don.name_dir_Surfer,suffix,num);
  name=OuvreFichier(fichier,"w");

  /* ecriture au format ASCII */

  if(Don.surfer==0){
    
    /* Ecriture de l'entete */
    fprintf(name,"DSAA\n");
    fprintf(name,"%d %d\n",Grd.Nx,Grd.Ny);
    fprintf(name,"%f %f\n",Grd.xmin,Grd.xmax);
    fprintf(name,"%f %f\n",Grd.ymin,Grd.ymax);
    fprintf(name,"%f %f\n",zmin,zmax);
    
    /* Ecriture des donnees */
    for(j=0;j<Grd.Ny;j++){
      c=0;
      for(i=0;i<Grd.Nx;i++){
	fprintf(name,"%12.4e ",Conc[i][j]);
	c++;
	if(c==10){
	  fprintf(name,"\n");
	  c=0;
	}
      }
      if(c==0) fprintf(name,"\n");
      else fprintf(name,"\n\n");
    }
  }

  /* ecriture au format binaire */
  else if(Don.surfer==1){

    /* Ecriture de l'entete */
    sprintf(CarTmp,"DSBB");
    fwrite(CarTmp,sizeof(char),4,name);
    ShortTmp=(short)Grd.Nx;
    fwrite(&ShortTmp,sizeof(short),1,name);
    ShortTmp=(short)Grd.Ny;
    fwrite(&ShortTmp,sizeof(short),1,name);
    DoubleTmp=Grd.xmin;
    fwrite(&DoubleTmp,sizeof(double),1,name);
    DoubleTmp=Grd.xmax;
    fwrite(&DoubleTmp,sizeof(double),1,name);
    DoubleTmp=Grd.ymin;
    fwrite(&DoubleTmp,sizeof(double),1,name);
    DoubleTmp=Grd.ymax;
    fwrite(&DoubleTmp,sizeof(double),1,name);
    DoubleTmp=zmin;
    fwrite(&DoubleTmp,sizeof(double),1,name);
    DoubleTmp=zmax;
    fwrite(&DoubleTmp,sizeof(double),1,name);
    
    /* Ecriture des donnees */
    for(j=0;j<Grd.Ny;j++){
      for(i=0;i<Grd.Nx;i++){
	FloatTmp=Conc[i][j];
	fwrite(&FloatTmp,sizeof(float),1,name);
      }
    }
    

  }
  /* Fermeture du fichier */
  FermeFichier(name);

  printf("OK\n");
}




/*---------------------------------------------*/


void Ecrire_grd_VerticalMapper(DBL **Conc,Grid Grd,char *suffix,int num)
/*----------------------------------*/
/* Ecriture des donnees sur fichier */
/* grille au format Vertical Mapper */
/*----------------------------------*/
{
  int i,j;
  FILE *name;
  char fichier[100];

  printf("   Ecriture du champ de concentration %s -> ",suffix);
  fflush(stdout);

  /* Test du pas */
  if(fabs(Grd.dx-Grd.dy)>0.01)
    Erreur("Pas de grille incompatibles avec Vertical Mapper",1);
  else{
    /* Ouverture du fichier */
    if(num<0) sprintf(fichier,"%s/concentr-%s.grd",Don.name_dir_Surfer,suffix);
    else sprintf(fichier,"%s/concentr-%s-%d.grd",Don.name_dir_Surfer,suffix,num);
    name=OuvreFichier(fichier,"w");
    
    /* Ecriture de l'entete */
    fprintf(name,"ncols %d\n",Grd.Nx);
    fprintf(name,"nrows %d\n",Grd.Ny);
    fprintf(name,"xllcenter %.2f\n",Grd.xmin+0.5*Grd.dx);
    fprintf(name,"yllcenter %.2f\n",Grd.ymin+0.5*Grd.dx);
    fprintf(name,"cellsize %.2f\n",Grd.dx);
    
    /* Ecriture des donnees */
    for(j=Grd.Ny-1;j>=0;j--){
      for(i=0;i<Grd.Nx;i++){
	fprintf(name,"%12.4e ",Conc[i][j]);
      }
      fprintf(name,"\n");
    }
    
    /* Fermeture du fichier */
    FermeFichier(name);

    printf("OK\n");
  }
}


/*---------------------------------------------*/
int Lire_grd(DBL *Conc,char *suffix,int num,int n_debut,int N_Bloc)
/*----------------------------------*/
/* Lecture des donnees sur fichier  */
/* grille                           */
/*----------------------------------*/
{
  if(Don.champ==1)
    return Lire_grd_Surfer(Conc,suffix,num,n_debut,N_Bloc);
  else if(Don.champ==2)
    return Lire_grd_VerticalMapper(Conc,suffix,num,n_debut,N_Bloc);
  else{
    Erreur("Ne doit pas arriver...",0);
    return 0;
  }
}
/*---------------------------------------------*/

int Lire_grd_Surfer(DBL *Conc,char *suffix,int num,int n_debut,int N_Bloc)
/*----------------------------------*/
/* Lecture d'un fichier grille au   */
/* format SURFER et retourne le     */
/* nombre effectif de valeurs lues  */
/*----------------------------------*/
{
  int i,j,i_lu,n_lu;
  short Nx,Ny;
  DBL C_tmp;
  DBL DoubleTmp;
  float FloatTmp;
  FILE *name;
  char fichier[400],ligne[100],CarTmp[4];

  /* Ouverture du fichier */  
  if(num<0) sprintf(fichier,"%s/concentr-%s.grd",Don.name_dir_Surfer,suffix);
  else sprintf(fichier,"%s/concentr-%s-%d.grd",Don.name_dir_Surfer,suffix,num);
  name=OuvreFichier(fichier,"r");

  printf("lecture du fichier %s\n",fichier);
  /* Lecture de l'entete */
  fread(&CarTmp,sizeof(char),4,name);
 
  /* fichier au format ASCII */
  if(strncmp("DSAA",CarTmp,4)==0){

    fscanf(name,"%d %d\n",&Nx,&Ny);
    fgets(ligne,100,name);
    fgets(ligne,100,name);
    fgets(ligne,100,name);
    
    /* Lecture des donnees */
    i_lu=0;
    n_lu=0;
    for(j=0;j<Ny;j++){
      for(i=0;i<Nx;i++){
	fscanf(name,"%lf",&C_tmp);
	if(i_lu>=n_debut && i_lu<(n_debut+N_Bloc)){
	  Conc[n_lu]=C_tmp;
	  n_lu+=1;
	}
	i_lu+=1;      
      }
    }
  }

  /* fichier au format binaire */
  if(strncmp("DSBB",CarTmp,4)==0){
    fread(&Nx,sizeof(short),1,name);
    fread(&Ny,sizeof(short),1,name);
 
    fread(&DoubleTmp,sizeof(double),1,name); 
    fread(&DoubleTmp,sizeof(double),1,name);
    fread(&DoubleTmp,sizeof(double),1,name);
    fread(&DoubleTmp,sizeof(double),1,name);
    fread(&DoubleTmp,sizeof(double),1,name);
    fread(&DoubleTmp,sizeof(double),1,name);
    
   
    /* Lecture des donnees */
    i_lu=0;
    n_lu=0;
    for(j=0;j<Ny;j++){
      for(i=0;i<Nx;i++){
	fread(&FloatTmp,sizeof(float),1,name);
	
	if(i_lu>=n_debut && i_lu<(n_debut+N_Bloc)){
	  Conc[n_lu]=(double)FloatTmp;
	  n_lu+=1;
	}
	i_lu+=1;      
      }
    }
  }


  /* Fermeture du fichier */
  FermeFichier(name);

  /* Retour du nombre de valeurs lues */
  return n_lu;
}


/*---------------------------------------------*/



int Lire_grd_VerticalMapper(DBL *Conc,char *suffix,int num,int n_debut,int N_Bloc)
/*----------------------------------*/
/* Lecture d'un fichier grille au   */
/* format Vertical Mapper et        */
/* retourne le nombre effectif de   */
/* valeurs lues                     */
/*----------------------------------*/
{
  int i,j,Nx,Ny,i_lu,n_lu;
  DBL C_tmp;
  FILE *name;
  char fichier[100],buffer[100],ligne[100];

  /* Ouverture du fichier */
  if(num<0) sprintf(fichier,"%s/concentr-%s.grd",Don.name_dir_Surfer,suffix);
  else sprintf(fichier,"%s/concentr-%s-%d.grd",Don.name_dir_Surfer,suffix,num);
  name=OuvreFichier(fichier,"r");

  /* Lecture de l'entete */
  fscanf(name,"%s %d\n",buffer,&Nx);
  fscanf(name,"%s %d\n",buffer,&Ny);
  fgets(ligne,100,name);
  fgets(ligne,100,name);
  fgets(ligne,100,name);

  /* Lecture des donnees */
  i_lu=0;
  n_lu=0;
  for(j=Ny-1;j>=0;j--){
    for(i=0;i<Nx;i++){
      fscanf(name,"%lf",&C_tmp);
      if(i_lu>=n_debut && i_lu<(n_debut+N_Bloc)){
	Conc[n_lu]=C_tmp;
	n_lu+=1;
      }
      i_lu+=1;      
    }
  }

  /* Fermeture du fichier */
  FermeFichier(name);

  /* Retour du nombre de valeurs lues */
  return n_lu;
}


/*---------------------------------------------*/
