 program genere_grille

! lit une grille et un fichier de points
! elimine les doublons en faisant la moyenne des points sur un même point de grille
! sort un fichier de point

 implicit none

 character(len=256) :: argstr
 character(len=256) :: fic
  
 integer, parameter :: npoint_max = 100000000
 real, parameter    :: distmin = 1. !metres
 real, parameter    :: distmax = 1.E20 !metres
 
 integer, parameter :: puissance = 2 !exposant de la distance -> DISTANCE = 1 AIRE = 2
 
 integer :: npoint
 real    :: xtmp,ytmp,ztmp
 real,allocatable :: x_point(:)
 real,allocatable :: y_point(:)
 real,allocatable :: z_point(:)
 real,allocatable :: d_point(:)
 
 integer :: iarg
 logical :: ixmin,iymin,idx,idy,inx,iny,idebug,ific
 real    :: xmin,ymin,dx,dy
 integer :: nx,ny
 integer :: ix,iy,ip,ip2
 
 real :: dist2
 
 real,allocatable :: xcentre(:,:)
 real,allocatable :: ycentre(:,:)
 real,allocatable :: zcentre(:,:)
 real,allocatable :: nmoyc(:,:) 
 
! lit l'argument
 iarg = 0    
 argstr='null'	
 ific=.false.  
 do while ( trim(adjustl(argstr)) .ne. '' )
 iarg = iarg + 1
 call getarg(iarg,argstr)
 if ( trim(adjustl(argstr)).eq. '-i' ) then
   call getarg(iarg+1,fic)
   ific = .true.
 else if ( trim(adjustl(argstr)).eq. '-xmin' ) then
   call getarg(iarg+1,argstr)
   read(argstr,*) xmin
   ixmin = .true.
 else if ( trim(adjustl(argstr)).eq. '-ymin' ) then
   call getarg(iarg+1,argstr)
   read(argstr,*) ymin
   iymin = .true.
 else if ( trim(adjustl(argstr)).eq. '-nx' ) then
   call getarg(iarg+1,argstr)
   read(argstr,*) nx
   inx = .true.
 else if ( trim(adjustl(argstr)).eq. '-ny' ) then
   call getarg(iarg+1,argstr)
   read(argstr,*) ny
   iny = .true.
 else if ( trim(adjustl(argstr)).eq. '-dx' ) then
   call getarg(iarg+1,argstr)
   read(argstr,*) dx
   idx = .true.
 else if ( trim(adjustl(argstr)).eq. '-dy' ) then
   call getarg(iarg+1,argstr)
   read(argstr,*) dy
   idy = .true.
 else if ( trim(adjustl(argstr)).eq. '-debug'.or.&
           trim(adjustl(argstr)).eq. '-d'           ) then
   idebug=.true.
 endif
 
 end do
 
 if (.not.ific) then
   stop 1
 end if
    
 allocate(xcentre(nx,ny))
 allocate(ycentre(nx,ny))
 allocate(zcentre(nx,ny)) 
 allocate(nmoyc(nx,ny)) 
 
 xcentre = 0. 
 ycentre = 0.
 zcentre = 0.
 nmoyc=0.
 
  ! DEBUG
  !write(*,*) 'nx=',nx
  !write(*,*) 'ny=',ny  

  !Lecture dans fichier
  open(unit=10,file=fic,status='old')
 
  !ETAPE 1 : Lecture  du fichier
  do ip = 1, npoint_max
    read(10,*,end=98,err=99)xtmp,ytmp,ztmp
  end do 
 
98 continue
   npoint = ip - 1
 
  allocate(x_point(npoint))
  allocate(y_point(npoint))
  allocate(z_point(npoint))
  allocate(d_point(npoint))
  close(10)

  open(unit=10,file=fic,status='old')
  do ip = 1, npoint
    read(10,*)x_point(ip),y_point(ip),z_point(ip)
  end do
  close(10)

  !ETAPE 2 : calcul la distance minimum inter-point    
  if ( npoint.lt.10000 ) then
    do ip = 1, npoint    
    d_point(ip) = distmax !metres
    do ip2 = 1, npoint    
      if (ip.ne.ip2) then
        dist2 = ((x_point(ip)-x_point(ip2))**2+(y_point(ip)-y_point(ip2))**2)**.5
        if (dist2.lt.d_point(ip))d_point(ip) = dist2
      end if    
    end do
    if (d_point(ip).lt.distmin)d_point(ip) = distmin     
    !Utilise un exposant (DISTANCE/AIRE)
    d_point(ip) = d_point(ip)**puissance
    end do    
  else  
    d_point(:) = distmin**puissance
  end if   
 
  !ETAPE 2 : recalcule la valeur Z averc une ponderation liée a la distance au point (** puissance)
  do ip = 1, npoint   
    ix = int((x_point(ip)-xmin)/dx) + 1
    iy = int((y_point(ip)-ymin)/dy) + 1
    !write(*,*) ix,iy,z_point
    if ((ix.ge.1).and.(ix.le.nx).and.(iy.ge.1).and.(iy.le.ny)) then
      xcentre(ix,iy) = xcentre(ix,iy) + ( x_point(ip) * d_point(ip) ) 
      ycentre(ix,iy) = ycentre(ix,iy) + ( y_point(ip) * d_point(ip) )
      zcentre(ix,iy) = zcentre(ix,iy) + ( z_point(ip) * d_point(ip) )	  
      nmoyc(ix,iy)   = nmoyc(ix,iy) + d_point(ip)
    end if     
  end do
 
  do ix = 1, nx
    do iy = 1, ny    
      if(nmoyc(ix,iy).ge.0.5*distmin**puissance) then 
        xcentre(ix,iy) = xcentre(ix,iy)/nmoyc(ix,iy)
        ycentre(ix,iy) = ycentre(ix,iy)/nmoyc(ix,iy)
        zcentre(ix,iy) = zcentre(ix,iy)/nmoyc(ix,iy)	       
        write(*,*) xcentre(ix,iy),ycentre(ix,iy),zcentre(ix,iy),nmoyc(ix,iy)
      end if
    end do   
  end do
  
  stop
  
99 continue  

   write(*,*)'ERREUR FORMAT X,Y,Z fic='//trim(fic)//' ligne:',ip

end program
