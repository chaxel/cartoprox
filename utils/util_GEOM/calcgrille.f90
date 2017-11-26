!creation de la grille de sortie et de la grille de calcul à partir des coordonnes du centre (WRF)
!MLNG 2009/09/07
!.....................................................
PROGRAM calcgrille
 implicit none
 integer, parameter :: ifw = 11 
 real    :: xc, yc
 real    :: xmin, ymin, xmax, ymax
 integer    :: xout, yout, xcnpt, ycnpt
 integer :: dx, dy
 character*256 :: fout

!..............
 print*,'centre de la maille ? xc, yc LAMBERT'
 read*,xc, yc
 print*,"==>",xc, yc

 print*,'taille de la maille en metre ? dx, dy'
 read*,dx, dy
 print*,"==>",dx, dy

 print*,'nom du fichier de sortie ? fout'
 read*,fout
 print*,"==>",trim(fout)

 xcnpt=1 !par défaut car domaine petit
 ycnpt=1
 xout=(dx/10)+1 !juste pour information
 yout=(dy/10)+1
 xmin=(xc-dx/2)
 xmax=(xc+dx/2)
 ymin=(yc-dy/2)
 ymax=(yc+dy/2)


 open(unit=ifw,file=trim(fout))
 rewind(ifw)
 write(ifw,100)xout
 write(ifw,110)yout
 write(ifw,120)xmin
 write(ifw,130)xmax
 write(ifw,140)ymin
 write(ifw,150)ymax

 write(ifw,200)xcnpt
 write(ifw,210)ycnpt
 write(ifw,220)xmin
 write(ifw,230)xmax
 write(ifw,240)ymin
 write(ifw,250)ymax

!
 print*,'fichier ecrit : ', trim(fout)
 
 close(ifw) 

 stop 'fin normale du programme :-)))) '
  
!...........attention au format pas d'espace pour .sh
100 format(t1,'Xout=',i3)
110 format(t1,'Yout=',i3)
120 format(t1,'Xmin=',f8.1)
130 format(t1,'Xmax=',f8.1)
140 format(t1,'Ymin=',f9.1)
150 format(t1,'Ymax=',f9.1)
200 format(t1,'Xcnpt=',i1)
210 format(t1,'Ycnpt=',i1)
220 format(t1,'Xcmin=',f8.1)
230 format(t1,'Xcmax=',f8.1)
240 format(t1,'Ycmin=',f9.1)
250 format(t1,'Ycmax=',f9.1)

END



