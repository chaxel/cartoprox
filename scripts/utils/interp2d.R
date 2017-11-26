#Interpolation de données a des points X, Y, Z vers une grille 
#reguliere definie par :
#xmin,xmax,ymin,ymax,dx,dy,nx,ny

rm(list=ls())

#--------------------------------
# chargement des package utilises 
#--------------------------------
library(chron)
library(akima)
#library(geoR)

#--------
#polluant
#--------
pol <- "test"

#----------
#Parametres
#----------
interpolation2d <- T

#-----------------------------------------------------------------
#chargement des données aux points X,Y,Z (fichier)
#colonne 1 : X
#colonne 2 : Y
#colonne 3 : Z
#-----------------------------------------------------------------
data_points_dir <- "./"
data_points_fic <- paste(data_points_dir,"points.xyz",sep="/")

points <- read.csv(data_points_fic,sep=";",header=F)
xpoint<-points[,1]
ypoint<-points[,2]
zpoint<-points[,3]

#print(xpoint)
#print(ypoint)
#print(zpoint)
#-----------------------------------------------------------------
#chargement de la grille d'estimation (fichier)
#colonne 1 : X
#colonne 2 : Y
#-----------------------------------------------------------------
data_grille_dir <- "./"
data_grille_fic <- paste(data_grille_dir,"grille.xy",sep="/")

grille <- read.csv(data_grille_fic,sep=";",header=F)
xgrille<-grille[,1]
ygrille<-grille[,2]

xo<-xgrille
yo<-ygrille

#print(xo)
#print(yo)

#-----------------------------------------------------------------
#interpolation sur la grille d'estimation
#-----------------------------------------------------------------
zo<-interpp(xpoint,ypoint,zpoint,xo,yo,linear=T,duplicate="mean")$z

#-----------------------------------------------------
# les donnnees sont envoyees pour faire des cartes GMT
#-----------------------------------------------------
write.table(cbind(xo,yo,zo),file="./grille.xyz",sep=";",quote=F,row.names=F,col.names=F)
