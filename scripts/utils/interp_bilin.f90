 program interp_bilin
 
 implicit none

!     arguments
 character(len=256) :: args(4)     
 character(len=256) :: fic
 real :: x, y 
 integer :: li, nli
 integer,parameter :: maxli=1e8
 real :: xc(maxli),yc(maxli),val(maxli)

 ! technique inverse de la distance 
 integer :: npts
 integer :: ip 
 integer,allocatable :: ix(:),wx(:)
 real,allocatable    :: distp(:)
 integer,allocatable :: indexp(:)
 real    :: valout
 real    :: sum
 real, parameter :: precision=1.E-6
  
 !local 
 real :: tmp
 logical :: idebug
 
 idebug=.false. 

 ! arguments: lit le nom du fichier
 call getarg(1,fic)       
 call getarg(2,args(2)) 
 call getarg(3,args(3))
 call getarg(4,args(4)) 
 
 read(args(2),*) x
 read(args(3),*) y
! read(args(4),*) npts
 npts = 1
 
 ! allocation
 allocate(ix(npts))
 allocate(wx(npts))
 allocate(distp(npts))
 allocate(indexp(npts))
 
 open(unit=10,file=trim(fic),status='old')
 
 do li=1, maxli
   read(10,*,end=99) xc(li),yc(li),val(li) 
 end do
 
99 continue 

 close(10) 
    
 nli=li-1
 
 if (idebug)write(*,*) nli,'lignes'
 
 distp(:) = 1.E9
 
 do li=1, nli 
   !calcul de la distance
   tmp = ((xc(li)-x)**2+(yc(li)-y)**2)**.5 
   !recherche des npts points les plus proches...
   do ip=1, npts
      ! trouve un point plus proche
      if (tmp.lt.distp(ip)) then
        distp(ip) = tmp
	indexp(ip) = li
	goto 8	
      end if      
   end do   
8 continue         
 end do
 
 ! calcul l'interpolation inverse des carrés
  valout = 0. 
  sum    = 0.
  do ip=1, npts
    if (idebug)write(*,*) ip, indexp(ip), distp(ip),val(indexp(ip)) 
    ! trouve un point plus proche
    valout = valout + ( val(indexp(ip)) * 1/(precision+distp(ip))**2 )
    sum = sum + 1/(precision+distp(ip))**2
  end do
  
  if (idebug)write(*,*) 'sum=',sum
  write(*,*) valout/sum
    

 end program
