      program grille2VM
      
! Ecrit une grille X,Y,Z dans le format Vertial Mapper    
            
      implicit none
      
      character(len=256) :: fni
      character(len=256) :: polluant
      integer :: nx, ny, nrows, ncols
      integer :: i, j, ix, iy
      real, allocatable :: x(:,:),y(:,:),z(:,:)
      real    :: tmp_x, tmp_y, tmp_z   
      real    :: xmin,xmax,ymin,ymax
      real    :: dx, dy ! pas de la grille
      real,parameter :: nodata_value = -9999.
      
      character(len=18) :: periodestr
!------------------------------      
    ! arguments : lit le nom du fichier
          
    call getarg(1,fni)
    
    open(unit=10,file=fni,status='old')
    
    ! Lit le HEADER (MAJ 01/2011)
    read(10,*) periodestr, nx, ny, polluant, xmin, ymin, dx, dy  ! xmin, ymin -> coin inferieur gauche

    allocate(x(nx,ny))
    allocate(y(nx,ny))
    allocate(z(nx,ny))
    
    !VM    
    z(:,:) = nodata_value
    
    !Coordonnées des centres
    do j=1,ny 
      do i=1,nx       	 
	 x(i,j) =  xmin + ( i - .5) * dx
	 y(i,j) =  ymin + ( j - .5) * dy	 
      end do
    end do

    !Lit le fichier de données
    do j=1,ny 
      do i=1,nx
	 read(10,*,err=88,end=89) tmp_x, tmp_y, tmp_z 	   	 
	 ix = int( (tmp_x - xmin) / dx ) + 1
	 iy = int( (tmp_y - ymin) / dy ) + 1	 	 
	 if ( (ix.ge.1).and.(iy.ge.1).and.(ix.le.nx).and.(iy.le.ny) ) then	    
	    z(ix,iy) = tmp_z
	 end if	 		 	     
      end do
    end do

89 continue

    !format VM
    ncols = nx    
    nrows = ny

    if ( dx .ne. dy ) then
      write(*,*) 'Conversion VM impossible: dx differe de dy'
      stop 1
    end if
    
    if ( ncols .gt. 30000 ) then
      write(*,*) 'Erreur dans grille2VM.f90: ncols limite a 3000'
      stop 1
    end if    
    
    xmin=9999999.
    xmax=-99999999.    
    ymin=9999999.
    ymax=-99999999.  

    ! Lit le CORPS dans le format VM             
    do j=nrows,1,-1
      do i=1,ncols
	 if ( z(i,j).lt.-999.) then
	   z(i,j) = nodata_value
	 end if	 
         !if ( z(i,j).lt.9999.) then
         !  z(i,j) = 9999
         !end if
	 if (x(i,j).lt.xmin)xmin=x(i,j)
	 if (y(i,j).lt.ymin)ymin=y(i,j)
      end do
    end do  

99  continue
    close(10) 
    
    ! Format VERTICAL MAPPER
!   write(*,*) 'ncols',ncols
!   write(*,*) 'nrows',nrows
!   write(*,*) 'xllcenter',xmin
!   write(*,*) 'yllcenter',ymin
!   write(*,*) 'cellsize',dx
!   write(*,*) 'NODATA_value',nodata_value  
!   do j=nrows,1,-1
!write(*,'(3000F10.2)') (z(i,j),i=1,ncols)   
!   end do     



write(*,100)ncols
write(*,110)nrows
write(*,120)xmin 
write(*,130)ymin        
write(*,140)int(dx)
!
do j=nrows,1,-1
     write(*,*)(z(i,j),i=1,ncols)
enddo

!...........
100 format(t1,'ncols',i9)
110 format(t1,'nrows',i9)
120 format(t1,'xllcenter ',f9.1)
130 format(t1,'yllcenter ',f9.1)
140 format(t1,'cellsize',i6)


    stop 0
    
88  continue   
    
    write(*,*) 'Erreur dans lecture des données' 
    
    stop 1	  

    end
