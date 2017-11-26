! creation d"une grille cartesienne à partir d"un dx dy et xmin, ymin (coin SW)
! et allant jusqu'à xmax,ymax
! pour la compil : pgf90 -Mbyteswapio createnode.f90 -o createnode
! exec : createnode
! attention : fichier créé là où est exécuté le prog (sinon mettre le path dans le nom)...
!
! FTr 2009/08/10
!.....................................................
PROGRAM createnode
implicit none
integer, parameter :: nxmax=50000
integer, parameter :: nymax=50000
integer, parameter :: ifw=4

real    :: xmin, ymin,xmax,ymax
integer :: dx,dy,nx,ny
integer :: i,j,imax,jmax,jmaxwrite
integer :: ideb,ifin,jdeb,jfin
real    :: x(nxmax),y(nymax)
character*132 :: fout,nom
integer :: xcp,idom,xcp1,xcp2,ycp,ycp1,ycp2
character*2 :: c2

!..............
print*,'coin SW ? xmin, ymin LAMBERT'
read*,xmin, ymin
print*,"==>",xmin, ymin
!...tout RA
!xmin=705500
!ymin=1906500
!xmax=972200
!ymax=2172600
!......
!xmin=776200
!ymin=1981400

print*,'coin NE ? xmax, ymax LAMBERT'
read*,xmax, ymax
print*,"==>",xmax, ymax
print*,'quel dom ? a=1 b=2 c=3 d=4 e=5 ?'
read*,idom
print*,"==>",idom
 xcp=int(xmin+(xmax-xmin)/2)
 xcp1=int(xmin+(xmax-xmin)/3)
 xcp2=int(xmin+(xmax-xmin)*2/3)
 ycp=int(ymin+(ymax-ymin)/2)
 ycp1=int(ymin+(ymax-ymin)/3)
 ycp2=int(ymin+(ymax-ymin)*2/3)

print*,'cxp xcp1 cxp2',xcp,xcp1,xcp2
!...
select case (idom) 
 case(5) 
   print*,'tout de domaine entier',xmax,xmin
   c2='tt'
 case(1)
   print*,'coin SW'
   xmax=xcp
   ymax=ycp
   c2='SW'
 case(2)
   print*,'coin NE'
   xmin=xcp
   ymin=ycp
   xmax=xmax
   ymax=ymax
   c2='NE'
 case(3)
   print*,'coin SE'
   xmin=xcp
   ymin=ymin
   xmax=xmax
   ymax=ycp
   c2='SE'
 case(4)
   print*,'coin NW'
   xmin=xmin
   ymin=ycp
   xmax=xcp
   ymax=ymax
   c2='NW'
 case(6)
   xmin=xmin
   ymin=ymin
   xmax=xcp1
   ymax=ycp1
   c2='cf'
 case(7)
   xmin=xcp1
   ymin=ymin
   xmax=xcp2
   ymax=ycp1
   c2='cg'
 case(8)
   xmin=xcp2
   ymin=ymin
   xmax=xmax
   ymax=ycp1
   c2='ch'
 case(9)
   xmin=xmin
   ymin=ycp1
   xmax=xcp1
   ymax=ycp2
   c2='ci'
 case(10)
   xmin=xcp1
   ymin=ycp1
   xmax=xcp2
   ymax=ycp2
   c2='cj'
 case(11)
   xmin=xcp2
   ymin=ycp1
   xmax=xmax
   ymax=ycp2
   c2='ck'
 case(12)
   xmin=xmin
   ymin=ycp2
   xmax=xcp1
   ymax=ymax
   c2='cl'
 case(13)
   xmin=xcp1
   ymin=ycp2
   xmax=xcp2
   ymax=ymax
   c2='cm'
 case(14)
   xmin=xcp2
   ymin=ycp2
   xmax=xmax
   ymax=ymax
   c2='cn'

end select 
!xmax=972200
!ymax=2172600
!xmax=891896
!ymax=2106260

print*,'pas en x et en y ? dx, dy '
read*,dx, dy
!dx=50
!dy=dx

print*,'nom dans la colonne'
if(dx.eq.10.and.dy.eq.10) nom='g10m'//c2
if(dx.eq.30.and.dy.eq.30) nom='g30m'//c2
if(dx.eq.50.and.dy.eq.50) nom='g50m'//c2
if(dx.eq.100.and.dy.eq.100) nom='g100m'//c2
print*,' si dx non 10,30 , 50 ou 100 : PENSEZ A CHANGER nom EN DUR !!!!!!!'
print*,''

ideb=1
jdeb=1
!..................................
print*,',xmax ideb',xmax, ideb
do i =ideb,nxmax
 x(i)= xmin+(i-1)*dx
!print*,i,x(i),xmax
 if(x(i) > xmax) then 
  imax=i-1
  goto 500
 endif 
enddo
500   print*,'fin de la génération des x ',ideb,imax
  print*,' x :',x(1),x(imax)

do j =jdeb,nymax
 y(j)= ymin+(j-1)*dy
 if(y(j) > ymax) then
  jmax=j-1
  goto 600
 endif
enddo
600 print*,'fin de la génération des y',jdeb,jmax
print*,'y : ',y(1),y(jmax)
!..................................
!pour ecriture
ifin=imax
jfin=jmax
!..................................


print*,' fichier output ?'
read*,fout
!fout='noeuds.txt'

 open(ifw,file=trim(fout))
 rewind(ifw)
 print*,'fichier ouvert : ',trim(fout)
 do j=jdeb,jfin
  do i=ideb,ifin
    write(ifw,*)trim(nom),i,j,';',x(i),';',y(j)
  enddo !j
   print*,'i j',i,j
 enddo !i
 print*,'fichier ecrit imax , jmax',imax , jmax,imax*jmax
close(ifw)

fout=trim(fout)//".VM.txt"
open(unit=ifw,file=trim(fout))
write(ifw,100)ifin-ideb+1
write(ifw,110)jfin-jdeb+1
write(ifw,120)x(1)  
write(ifw,130)y(1)
write(ifw,142)int(x(1)) !int(y(1))
!
do j=jdeb,jfin
     write(ifw,*)(1,i=ideb,ifin)
   print*,'ici'
enddo

print*,'fichier ecrit : ', trim(fout)
!...........
100 format(t1,'ncols',i9)
110 format(t1,'nrows',i9)
120 format(t1,'xllcenter ',f9.1)
130 format(t1,'yllcenter ',f9.1,f9.1)
142 format(t1,'cellsize',i15)
close(ifw)


!..................
 stop 'fin normale du programme :-)))) '
!..................
END



