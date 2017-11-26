      program calc_cartoprox_nc
!****************************************************
!*     PREVALP/CARTOPROX                            *
!*     Calcul des concentration CARTPROX            *
!*     date : decembre 2010                         *
!*          auteur: E. Chaxel                       *
!*		       LEGI/GIERSA	            *
!*			chaxeleric@yahoo.fr         *
!****************************************************

! version 3  : calcule l'aire représentative de chaque récepteur
! version 31 : calcule l'aire représentative de chaque récepteur sur un voisinage

      use netcdf
      use typesizes

      implicit none 

      character(len=256),parameter :: program_exe='calc_suremis_nc.exe'
      character(len=256) :: fic_sirane
      
      integer :: pointDimID, frTimeDimID, xVarID
      integer :: ncSiraneID
      integer :: numPoints, numFrTimes      
      
      integer,parameter :: nvar = 5      
      character(len=20),parameter :: varstr(nvar) = (/ 'NO', 'NO2', 'O3', 'PM10', 'PM25'/)
      real,parameter :: ppb2microg(nvar) = (/ 1.21, 1.91, 1.99, 1., 1./)      
           
      character(len=20) :: varstr_sirane
      character(len=20) :: varstr_fond
            
      character(len=19) :: datestr       
      
      real, allocatable :: conc_sirane(:,:)                 
      real, allocatable :: xpt(:)
      real, allocatable :: ypt(:)
      real, allocatable :: dist(:)       
      real, allocatable :: area(:)        
      real              :: conc_fond(nvar,1)
      real              :: conc_prox(nvar,1)           
      
      integer :: start1d(1)
      integer :: start2d(2)   
      
      integer :: it, iv, ip, ip2, i, j
      
      real :: x, y, dist2, ind
      
      logical :: idisp_legende !flags pour affichage de la legende
            
      integer, parameter :: nprint = 100 ! nombre de point à afficher
      
      !DEBUG
      logical :: idebug 
      
      !SUREMISSIONS
      character(len=256):: fic_suremis 
      real              :: suremission_dx !metres taille du domaine
      character(len=20) :: xmin_str, ymin_str, suremission_dx_str   
      character(len=10) :: nx_str, ny_str   
      real              :: xmin, ymin
      real              :: dx, dy
      integer           :: nx, ny
      integer           :: ncGridID
      real, allocatable :: easting(:,:), northing(:,:), suremis(:,:,:)
      real, allocatable :: aire_suremis(:,:) 
         
!--------------------------------------------------------------------------------------------      
      logical :: isirane, ifond 

      !idebug = .true.

! ARGS  ---------------------------------------------------------------------------------             
      call getarg(1,nx_str)
      call getarg(2,ny_str)      
      call getarg(3,xmin_str)
      call getarg(4,ymin_str)
      call getarg(5,suremission_dx_str)      
      call getarg(6,fic_sirane)
      call getarg(7,fic_suremis)
      
! SUREMISSION  ---------------------------------------------------------------------------------     
      read(nx_str,*)nx
      read(ny_str,*)ny
      read(xmin_str,*)xmin
      read(ymin_str,*)ymin
      read(suremission_dx_str,*)suremission_dx      

      write(*,*) 'nx ',nx
      write(*,*) 'ny ',ny      
      write(*,*) 'xmin ',xmin
      write(*,*) 'ymin ',ymin   
      dx = suremission_dx
      dy = suremission_dx
      allocate  ( easting(nx,ny) )
      allocate  ( northing(nx,ny) )
      allocate  ( aire_suremis(nx,ny) )
      allocate  ( suremis(nvar,nx,ny) )                        
      do i = 1 , nx
        do j = 1 , ny
          easting(i,j)  = xmin +  dx * ( i - 1)
          northing(i,j) = ymin +  dy * ( j - 1)
        end do 
      end do         
      
      call create_nc(fic_suremis,nx,ny) 
      call check(nf90_open(fic_suremis, nf90_write, ncGridID) )
      if ( nf90_inq_varid(ncGridID, 'easting', xVarID) .eq. nf90_noerr ) &  
	   call check(nf90_put_var(ncGridID, xVarID, easting ) )         
      if ( nf90_inq_varid(ncGridID, 'northing', xVarID) .eq. nf90_noerr ) &  
	   call check(nf90_put_var(ncGridID, xVarID, northing ) )
      call check(nf90_close(ncGridID) ) 
      write(*,*) 'Creation '//trim(fic_suremis)//' OK'    
	    
!----------------------------------------------------------------------------------
      write(*,*) 'Lit '//trim(fic_sirane) 	                     
      !Fichier ENTREES : lit les dimensions     
      call check(nf90_open(path = trim(fic_sirane), mode = nf90_nowrite, ncid = ncSiraneID))     
      
      call check(nf90_inq_dimid(ncSiraneID,'Point', pointDimID))     
      call check(nf90_inq_dimid(ncSiraneID, 'Time', frTimeDimID))                      
      call check(nf90_Inquire_Dimension(ncSiraneID, pointDimID , len= numPoints))             
      call check(nf90_Inquire_Dimension(ncSiraneID, frTimeDimID, len= numFrTimes))
      
      write(*,*)'numPoints=',numPoints
      write(*,*)'numFrTimes=',numFrTimes      
      
      ! Allocation      
      allocate(conc_sirane(nvar,numPoints))    
                 
!-------------------------------------------------------------------------------------------
!version 3 : calcul des aires caractéristiques 
!-------------------------------------------------------------------------------------------
! HYPOTHESE : Pour ce calcul, on assume que le maillage est localement un maillage 
! de triangles equilateraux.
! ATTENTION : on utilise les coordonnées métriques. L'aire minimum est fixée à 1 m^2

      allocate(xpt(numPoints))
      allocate(ypt(numPoints))
      allocate(area(numPoints))
      allocate(dist(numPoints))                 
                  
      call check(nf90_inq_varid(ncSiraneID, 'easting_pts', xVarID) )
      call check(nf90_get_var(ncSiraneID, xVarID, xpt,count=(/numPoints/), start=(/1/)) )	

      call check(nf90_inq_varid(ncSiraneID, 'northing_pts', xVarID) )
      call check(nf90_get_var(ncSiraneID, xVarID, ypt,count=(/numPoints/), start=(/1/) ) )

      call check(nf90_close(ncSiraneID))

!recherche du point le plus proche
      write(*,'("PT ",A6,A9,A10,A10,A10)') 'num', 'easting','northing','dist(m)','area(m2)'
      do ip = 1, numPoints
	x = xpt(ip)
	y = ypt(ip) 
	dist(ip) = 1.E18        
	do ip2 = 1, numPoints
	  if (ip2.ne.ip) then
	     dist2 = ( ( x - xpt(ip2) )**2 +  ( y - ypt(ip2) )**2 )**0.5
	     if ( dist2 .lt. dist(ip) )dist(ip) = dist2	     
	  end if
        end do
	
	if ( dist(ip) .gt. 1.E17 ) then	
	  write(*,*)'Erreur dans le calcul de la distance interrecepteur: dist=1.E18'	
	end if
	
	!Aire d'un triangle equilateral de cote a A = sqrt(3)/2*a*a/2
	area(ip) = ((3**.5/2*dist(ip))*dist(ip)) !/2	
	
	if ( area(ip).lt.1.)area(ip) = 1.
        
	if (ip.lt.nprint) write(*,'("PT ",I6,F9.1,F10.1,F10.1,F10.1)') ip, xpt(ip), ypt(ip), dist(ip), area(ip)  
      end do
      write(*,*) '----- Infos domaine -----------------------------------'
      write(*,*) 'Nombre de points:',numPoints
      write(*,*) 'Distance max. entre 2 points (m)=', maxval( dist )    
      write(*,*) 'Aire max. en un point (m2)=', maxval( area ) 
      write(*,*) '-------------------------------------------------------'             
            
      !Ecrit dans le fichier d'entree
      call check(nf90_open(fic_sirane, nf90_write  , ncSiraneID))          
      if ( nf90_inq_varid(ncSiraneID, 'area_pts', xVarID) .eq. nf90_noerr ) &
        call check(nf90_put_var(ncSiraneID, xVarID, area, count=(/numPoints/), start=(/1/) ) )      
      call check(nf90_close(ncSiraneID))
            
!-------------------------------------------------------------------------------------------     
!Verifie la presence des varaibles
!-------------------------------------------------------------------------------------------
      if (idebug)write(*,*)'Ouvre '//trim(fic_sirane)
      if (nf90_open(fic_sirane, nf90_nowrite  ,ncSiraneID).ne.nf90_noerr)stop 'STOP: Erreur ouverture fichier SIRANE'       
      if (idebug)write(*,*)'Ouvre '//trim(fic_suremis)      
      if (nf90_open(fic_suremis,  nf90_write  ,ncGridID).ne.nf90_noerr)stop 'STOP: Erreur ouverture fichier SUREMIS'     
!-------------------------------------------------------------------------------------------     
!Ouvre les fichiers
!-------------------------------------------------------------------------------------------      
      do iv = 1, nvar      
        !Defini les variables
        varstr_sirane	 = trim(adjustl(varstr(iv)))
        varstr_fond	 = trim(adjustl(varstr(iv)))//'_fond'	
        isirane=.false.	
        if ( nf90_inq_varid(ncSiraneID, varstr_fond,   xVarID) .eq. nf90_noerr)ifond=.true.
	
        if ( nf90_inq_varid(ncSiraneID, varstr_sirane, xVarID) .eq. nf90_noerr) then
	  isirane=.true.	   
          if (.not.(ifond.and.isirane)) then        
            write(*,*) 'Manque une variable'
            write(*,*) 'sirane ('//trim(varstr_sirane)//')=', isirane
            write(*,*) 'fond ('//trim(varstr_fond)//')='  , ifond
            stop 'STOP: calc_cartoproc_nc.f90'
          end if
	end if
      end do

!-------------------------------------------------------------------------------------------
!Calcul CARTOPROX horaire
!-------------------------------------------------------------------------------------------
      do it = 1, numFrTimes
            
        start2d=(/  1,  it/)
        start1d=(/	it/)
	
	!Ecrit les pas de temps
	call check(nf90_inq_varid(ncSiraneID, 'Times', xVarID) )   
        call check(nf90_get_var(ncSiraneID, xVarID, datestr, start=start2d ) )
		
	if(idebug)write(*,*)  	datestr  
      
        do iv = 1, nvar
	
	  !Nom des varaiables
          varstr_sirane	 = trim(adjustl(varstr(iv)))
          varstr_fond	 = trim(adjustl(varstr(iv)))//'_fond'		  
	
	  idisp_legende=.true.
        	           
          !la variable SIRANE est dans le fichier ? isirane = .true.
	  if ( nf90_inq_varid(ncSiraneID, varstr_sirane, xVarID) .eq. nf90_noerr) then
		
             !Lit dans le fichier SIRANE
             if (idebug)write(*,*)'Lecture des concentrations'
	     if (idebug)write(*,*)'Lit '//trim(varstr_sirane)//' dans '//trim(fic_sirane)
	     call check(nf90_inq_varid(ncSiraneID, varstr_sirane, xVarID) )
             call check(nf90_get_var(ncSiraneID, xVarID, conc_sirane(iv,:),count=(/numPoints/), start=start2d ) )	     
	     if (idebug)write(*,*)'Lit '//trim(varstr_fond)//' dans '//trim(fic_sirane)
	     call check(nf90_inq_varid(ncSiraneID, varstr_fond, xVarID) )   
             call check(nf90_get_var(ncSiraneID, xVarID, conc_fond(iv,:), count=(/1/), start=start1d ) ) 
             if (idebug)write(*,*)'Fin de lecture'

! SUREMISSION  ---------------------------------------------------------------------------------   
  	     suremis (iv,:,:) = 0.
	     aire_suremis(:,:) = 0. 
	     do ip = 1, numPoints
	       i = int( ( xpt(ip) - xmin ) / dx + 1 )
               j = int( ( ypt(ip) - ymin ) / dy + 1 )
	       if  ( (i .ge. 1) .and.  (i .le. nx) .and. (j .ge. 1) .and.  (j .le. ny) ) &
	         suremis (iv,i,j) = suremis (iv,i,j) + ( conc_sirane(iv,ip) - conc_fond(iv,1) )	*  area(ip)
		 aire_suremis(i,j) =  aire_suremis(i,j) + area(ip)          
             end do
	     
             !normalise avec l'aire totale
	     do i = 1 , nx
               do j = 1 , ny	     
	         suremis(iv,i,j) = suremis(iv,i,j) / aire_suremis(i,j)
	       end do
	     end do	
	          
             if (idebug)write(*,*)'Ecrit '//trim(varstr_sirane)//' dans '//trim(fic_suremis)
	     
             if (it.eq.1) then
	       call create_var2d_nc(ncGridID,'aire',xVarID)	     
	       call check(nf90_put_var(ncGridID, xVarID, aire_suremis(:,:),count=(/nx,ny/), start=(/1,1/)))
	     end if 	     
	     
             call create_var_nc(ncGridID,trim(varstr_sirane),xVarID)	     
	     call check(nf90_put_var(ncGridID, xVarID, suremis (iv,:,:),count=(/nx,ny/), start=(/1,1,1,it/)) )
	       
	     call check(nf90_inq_varid(ncGridID, 'Times', xVarID) )   
             call check(nf90_put_var(ncGridID, xVarID, datestr, start=start2d ) )	     
	     
             if (idebug)write(*,*)'Raster '//trim(varstr_sirane)//' OK'	         
!-------------------------------------------------------------------------------------------------	     
	     end if ! var existe ? 

          end do !ivar

      end do 
                     
      call check(nf90_close(ncSiraneID)) 
      call check(nf90_close(ncGridID))             

      write(*,*) '-> '//trim(fic_suremis)
      write(*,*) 'Fin du programme '//trim(program_exe)
                    
      end program
!*****************************************************************************      
subroutine check(status)
  ! Internal subroutine - checks error status after each netcdf, prints out text message each time
  !   an error code is returned. 

  use netcdf
  use typesizes

  implicit none

  integer, intent ( in) :: status
   
  if(status .ne. nf90_noerr) then 
    print*, 'Error occured'
    print *, 'status=',status
    print*, trim(nf90_strerror(status))
    stop    
  end if
end subroutine check  

