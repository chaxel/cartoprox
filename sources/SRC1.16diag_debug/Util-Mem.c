/***********************************************/
/*                                             */
/* Modele SIRANE version 1.15    LMFA/ECL 2004 */
/*                                             */
/* Auteur = Lionel SOULHAC                     */
/* Date de derniere modification 26/10/2004    */
/*                                             */
/* Util-Mem.c --> Fonctions utilitaires de     */
/*                gestion de la memoire        */
/*                                             */
/***********************************************/
#include "Def.h"
#include "Fonc.h"


/*---------------------------------------------*/


int **AllocMtrxI(int nl,int nc)
/*-------------------------*/	
/* Allocation d'un tableau */
/* 2D d'entiers            */
/*-------------------------*/	
{
  int **mi;
  int i;

  mi=(int **)calloc((unsigned)nl,sizeof(int*));
  if(!mi)
    Erreur("Erreur d'allocation d'une matrice 2D d'entiers",0);
  for(i=0;i<nl;i++) {
    mi[i]=(int *)calloc((unsigned)nc,sizeof(int));
    if(!mi[i])
      Erreur("Erreur d'allocation d'une matrice 2D d'entiers",0);
  }
  return mi;
}


/*---------------------------------------------*/


DBL **AllocMtrx(int nl,int nc)
/*-------------------------*/	
/* Allocation d'un tableau */
/* 2D de DBL               */
/*-------------------------*/	
{
  DBL **md;
  int i;

  md=(DBL **)calloc((unsigned)nl,sizeof(DBL*));
  if(!md)
    Erreur("Erreur d'allocation d'une matrice 2D de DBL",0);
  for(i=0;i<nl;i++){
    md[i]=(DBL *)calloc((unsigned)nc,sizeof(DBL));
    if(!md[i])
      Erreur("Erreur d'allocation d'une matrice 2D de DBL",0);
  }
  return md;
}


/*---------------------------------------------*/


int ***AllocBoiteI(int nl,int nc,int np)
/*-------------------------*/	
/* Allocation d'un tableau */
/* 3D d'entiers            */
/*-------------------------*/	
{
  int ***md;
  int i,j;

  md=(int ***)calloc((unsigned)nl,sizeof(int**));
  if(!md)
    Erreur("Erreur d'allocation d'une matrice 3D d'entiers",0);
  for(i=0;i<nl;i++){
    md[i]=(int **)calloc((unsigned)nc,sizeof(int*));
    if(!md[i])
      Erreur("Erreur d'allocation d'une matrice 3D d'entiers",0);
    for(j=0;j<nc;j++){
      md[i][j]=(int *)calloc((unsigned)np,sizeof(int));
      if(!md[i][j])
	Erreur("Erreur d'allocation d'une matrice 3D d'entiers",0);
    }
  }
  return md;
}


/*---------------------------------------------*/


DBL ***AllocBoite(int nl,int nc,int np)
/*-------------------------*/	
/* Allocation d'un tableau */
/* 3D de DBL               */
/*-------------------------*/	
{
  DBL ***md;
  int i,j;

  md=(DBL ***)calloc((unsigned)nl,sizeof(DBL**));
  if(!md)
    Erreur("Erreur d'allocation d'une matrice 3D de DBL",0);
  for(i=0;i<nl;i++){
    md[i]=(DBL **)calloc((unsigned)nc,sizeof(DBL*));
    if(!md[i])
      Erreur("Erreur d'allocation d'une matrice 3D de DBL",0);
    for(j=0;j<nc;j++){
      md[i][j]=(DBL *)calloc((unsigned)np,sizeof(DBL));
      if(!md[i][j])
	Erreur("Erreur d'allocation d'une matrice 3D de DBL",0);
    }
  }
  return md;
}


/*---------------------------------------------*/


void LibereMtrxI(int **md,int nl,int nc)
/*-------------------------*/	
/* Liberation d'un tableau */
/* 2D d'entiers            */
/*-------------------------*/	
{
  int i;

  for(i=0;i<nl;i++)
    free(md[i]);
  free(md);
}


/*---------------------------------------------*/


void LibereMtrx(DBL **md,int nl,int nc)
/*-------------------------*/	
/* Liberation d'un tableau */
/* 2D de DBL               */
/*-------------------------*/	
{
  int i;

  for(i=0;i<nl;i++)
    free(md[i]);
  free(md);
}


/*---------------------------------------------*/


void LibereBoiteI(int ***md,int nl,int nc,int np)
/*--------------------------*/	
/* Liberation d'une matrice */
/* 3D d'entiers             */
/*--------------------------*/	
{
  int i;

  for(i=0;i<nl;i++) LibereMtrxI(md[i],nc,np);
  free(md);
}


/*---------------------------------------------*/


void LibereBoite(DBL ***md,int nl,int nc,int np)
/*--------------------------*/	
/* Liberation d'une matrice */
/* 3D de DBL                */
/*--------------------------*/	
{
  int i;

  for(i=0;i<nl;i++) LibereMtrx(md[i],nc,np);
  free(md);
}


/*---------------------------------------------*/


void LibereNoeud(Noeud *Nd)
/*--------------------------*/	
/* Liberation d'une matrice */
/* 3D de DBL                */
/*--------------------------*/	
{
  int i;

  for(i=0;i<Don.N_Noeud;i++){
    if(Nd[i].Categ==0)
      LibereMtrx(Nd[i].Flux,Nd[i].N_Rue+1,Nd[i].N_Rue+1);
    if(Nd[i].N_Rue>0)
      free(Nd[i].Num);
  }
  free(Nd);
}


/*---------------------------------------------*/


void InitTabDBL(DBL *tab,int N,DBL val)
/*-----------------------------*/	
/* Initialisation d'un tableau */
/* de N DBL a la valeur val    */
/*-----------------------------*/	
{
  int i;

  for(i=0;i<N;i++) tab[i]=val;
}


/*---------------------------------------------*/
