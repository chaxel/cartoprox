      program grille2grd
      
! Ecrit une grille X,Y,Z dans le format SURFER GRID ASCII     
            
      implicit none
      
      character(len=256) :: fni
      character(len=256) :: polluant
      integer :: nx, ny
      integer :: i, j, ix, iy
      real, allocatable :: x(:,:),y(:,:),z(:,:)
      real    :: tmp_x, tmp_y, tmp_z      
      real    :: xmin,xmax,ymin,ymax,zmin,zmax
      real    :: dx, dy ! pas de la grille
      real,parameter :: nodata_value = -9999.      
      
      character(len=18) :: periodestr
!------------------------------      
    ! arguments : lit le nom du fichier
    
    zmin=9999999.
    zmax=-99999999.
    xmin=9999999.
    xmax=-99999999.    
    ymin=9999999.
    ymax=-99999999.    
    
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
    
    ! Lit le CORPS                
    do j=1,ny 
      do i=1,nx
	 if ( z(i,j).gt.-999.) then
	   if (z(i,j).lt.zmin)zmin=z(i,j)
	   if (z(i,j).gt.zmax)zmax=z(i,j)
	 end if	 
	 if (x(i,j).lt.xmin)xmin=x(i,j)
	 if (x(i,j).gt.xmax)xmax=x(i,j)	 	 
	 if (y(i,j).lt.ymin)ymin=y(i,j)
	 if (y(i,j).gt.ymax)ymax=y(i,j)	 	 	 	 
      end do
    end do  

99  continue
    close(10) 

    !write(*,*) nx,ny
        
    write(*,'(A4)') 'DSAA'
    write(*,*) nx,ny
    write(*,*) xmin,xmax
    write(*,*) ymin,ymax
    write(*,*) zmin,zmax   
    do j=1,ny 
      do i=1,int(nx/10.)
        write(*,*) z((i-1)*10+1:(i-1)*10+10,j)
      end do      
      if (nx-int(nx/10.)*10.ge.1)write(*,*) z( int(nx/10.)*10+1:nx, j )
      write(*,*) ''
    end do     
          
    stop
    
88  continue   
    
    write(*,*) 'Erreur dans lecture des données' 	  

    end
