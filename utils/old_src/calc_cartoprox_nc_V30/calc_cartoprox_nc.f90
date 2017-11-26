      program calc_cartoprox_nc
!****************************************************
!*     PREVALP/CARTOPROX                            *
!*     Calcul des concentration CARTPROX            *
!*     date : decembre 2010                         *
!*          auteur: E. Chaxel                       *
!*		       LEGI/GIERSA	            *
!*			chaxeleric@yahoo.fr         *
!****************************************************

! version 3: calcule l'aire représentative de chaque récepteur


      use netcdf
      use typesizes

      implicit none 

      character(len=256),parameter :: program_exe='calc_cartoprox_nc.exe'
      character(len=256) :: fileIn, fileOut     
      
      integer :: pointDimID, frTimeDimID, xVarID, ncFileOutID, ncFileID    
      integer :: numPoints, numFrTimes      
      
      integer,parameter :: nvar = 5      
      character(len=20),parameter :: varstr(nvar) = (/ 'NO', 'NO2', 'O3', 'PM10', 'PM25'/)
      real,parameter :: ppb2microg(nvar) = (/ 1.21, 1.91, 1.99, 1., 1./)      
           
      character(len=20) :: varstr_sirane
      character(len=20) :: varstr_grille
      character(len=20) :: varstr_cartoprox            
      character(len=20) :: varstr_fond         
      character(len=20) :: varstr_prox 

      character(len=19) :: datestr       
      
      real, allocatable :: conc_sirane(:)
      real, allocatable :: conc_grille(:)
      real, allocatable :: conc_cartoprox(:)            
      real, allocatable :: xpt(:)
      real, allocatable :: ypt(:)
      real, allocatable :: area(:)        
      real              :: conc_fond(1)
      real              :: conc_prox(1)           
      
      integer :: start1d(1)
      integer :: start2d(2)   
      
      integer :: it, iv, ip, ip2
      
      real :: x, y, a, dist2, ind, total
      
      logical :: idisp_legende !flags pour affichage de la legende

!--------------------------------------------------------------------------------------------      
      logical :: isirane, ifond, igrille, iprox 

      call getarg(1,fileIn)
      call getarg(2,fileOut)      

      write(*,*) 'Lit '//trim(fileIn)      
      
      !Fichier ENTREES : lit les dimensions     
      call check(nf90_open(path = trim(fileIn), mode = nf90_nowrite, ncid = ncFileID))       
      
      call check(nf90_inq_dimid(ncFileID,'Point', pointDimID))     
      call check(nf90_inq_dimid(ncFileID, 'Time', frTimeDimID))                      
      call check(nf90_Inquire_Dimension(ncFileID, pointDimID , len= numPoints))             
      call check(nf90_Inquire_Dimension(ncFileID, frTimeDimID, len= numFrTimes))
      
      write(*,*)'numPoints=',numPoints
      write(*,*)'numFrTimes=',numFrTimes      
      
      ! Allocation
      allocate(conc_sirane(numPoints))
      allocate(conc_grille(numPoints)) 
      allocate(conc_cartoprox(numPoints))          

!-------------------------------------------------------------------------------------------
!version 3 : calcul des aires caractéristiques 
!-------------------------------------------------------------------------------------------
! HYPOTHESE : Pour ce calcul, on assume que le maillage est localement un maillage 
! de triangles equilateraux.
! ATTENTION : on utilise les coordonnées métriques. L'aire minimum est fixée à 1 m^2

      allocate(xpt(numPoints))
      allocate(ypt(numPoints))
      allocate(area(numPoints))      
                  
      call check(nf90_inq_varid(ncFileID, 'easting_pts', xVarID) )
      call check(nf90_get_var(ncFileID, xVarID, xpt,count=(/numPoints/), start=(/1/)) )	

      call check(nf90_inq_varid(ncFileID, 'northing_pts', xVarID) )
      call check(nf90_get_var(ncFileID, xVarID, ypt,count=(/numPoints/), start=(/1/) ) )

!recherche du point le plus proche
      write(*,'("PT ",A6,A9,A10,A10)') 'num', 'easting','northing','area(m2)'
      do ip = 1, numPoints
	x = xpt(ip)
	y = ypt(ip) 
	a = 1.E18        
	do ip2 = 1, numPoints
	  if (ip2.ne.ip) then
	     dist2 = ( ( x - xpt(ip2) )**2 +  ( y - ypt(ip2) )**2 )**0.5
	     if ( dist2 .lt. a )a = dist2	     
	  end if
        end do
	
	if ( a .gt. 1.E17 ) then	
	  write(*,*)'Erreur dans le calcul de la distance interrecepteur: dist=1.E18'	
	end if
	
	!Aire d'un triangle equilateral de cote a A = sqrt(3)/2*a*a/2
	area(ip) = ((3**.5/2*a)*a) !/2
	
	if ( area(ip).lt.1.)area(ip) = 1.
        
	write(*,'("PT ",I6,F9.1,F10.1,F10.1)') ip, xpt(ip), ypt(ip), area(ip)
      
      end do
      
      call check(nf90_close(ncFileID))       
      
            
      !Ecrit dans le fichier d'entree
      call check(nf90_open(fileIn, nf90_write  , ncFileID))          
      if ( nf90_inq_varid(ncFileID, 'area_pts', xVarID) .eq. nf90_noerr ) &
        call check(nf90_put_var(ncFileID, xVarID, area, count=(/numPoints/), start=(/1/) ) )      
      call check(nf90_close(ncFileID))        
      
      !Ecrit dans le fichier de sortie
      call check(nf90_open(fileOut, nf90_write  , ncFileOutID))          
      if ( nf90_inq_varid(ncFileOutID, 'area_pts', xVarID) .eq. nf90_noerr ) &
        call check(nf90_put_var(ncFileOutID, xVarID, area, count=(/numPoints/), start=(/1/) ) )      
      call check(nf90_close(ncFileOutID))   
      
!-------------------------------------------------------------------------------------------     
!Calcul CARTOPROX horaire
!-------------------------------------------------------------------------------------------
      call check(nf90_open(fileIn,  nf90_nowrite  , ncFileID))      
      call check(nf90_open(fileOut, nf90_write    , ncFileOutID))          
                                    	  	           
      do it = 1, numFrTimes
      
        start2d=(/  1,  it/)
        start1d=(/	it/)
	
	!Ecrit les pas de temps
	call check(nf90_inq_varid(ncFileID, 'Times', xVarID) )   
        call check(nf90_get_var(ncFileID, xVarID, datestr, start=start2d ) )
	
	call check(nf90_inq_varid(ncFileOutID, 'Times', xVarID) )   
        call check(nf90_put_var(ncFileOutID, xVarID, datestr, start=start2d ) )
	
	!write(*,*)  	datestr  
      
        do iv = 1, nvar
	
	 idisp_legende=.true.
        
         !Defini les variables
         varstr_sirane    = trim(adjustl(varstr(iv)))
	 varstr_cartoprox = varstr_sirane
         varstr_grille    = trim(adjustl(varstr(iv)))//'_grille'	
         varstr_fond      = trim(adjustl(varstr(iv)))//'_fond'
         varstr_prox      = trim(adjustl(varstr(iv)))//'_prox'

         isirane=.false.
         igrille=.false.
         ifond=.false.
         		           
         !la variable SIRANE est dans le fichier ?
	 if ( nf90_inq_varid(ncFileID, varstr_sirane, xVarID).eq.nf90_noerr) then  
	   
           isirane=.true.
           if ( nf90_inq_varid(ncFileID, varstr_grille, xVarID) .eq. nf90_noerr)igrille=.true.
           if ( nf90_inq_varid(ncFileID, varstr_fond,   xVarID) .eq. nf90_noerr)ifond=.true.
           if ( nf90_inq_varid(ncFileID, varstr_prox,   xVarID) .eq. nf90_noerr)iprox=.true.	   
          
           if (ifond.and.igrille.and.isirane) then
        		
             !Lit dans le fichier SIRANE
	     call check(nf90_inq_varid(ncFileID, varstr_fond, xVarID) )   
             call check(nf90_get_var(ncFileID, xVarID, conc_fond, count=(/1/), start=start1d ) ) 

             call check(nf90_inq_varid(ncFileID, varstr_grille, xVarID) )
             call check(nf90_get_var(ncFileID, xVarID, conc_grille(:),count=(/numPoints/), start=start2d) )	
        	   
             call check(nf90_inq_varid(ncFileID, varstr_sirane, xVarID) )
             call check(nf90_get_var(ncFileID, xVarID, conc_sirane(:),count=(/numPoints/), start=start2d ) )

             !Calcul de la suremissions prox de SIRANE
	     total = 0.
	     conc_prox(1) = 0.
             do ip = 1, numPoints
               conc_prox(1) = conc_prox(1) + ( conc_sirane(ip) - conc_fond(1) ) * area(ip) 
	       total = total + area(ip)
             end do	   
             conc_prox(1) = conc_prox(1) / total
        
             !Formule CARTOPROX (2011, VERSION 3)
	     !CARTOPROX = SIRANE - fond_SIRANE + fond_PREVALP - prox_SIRANE
             do ip = 1, numPoints
               conc_cartoprox(ip) = conc_sirane(ip) - conc_fond(1) + ( conc_grille(ip) * ppb2microg(iv) - conc_prox(1) )
             end do
	   
	     if(it.eq.1)write(*,*)trim(varstr_cartoprox)//'-> '//trim(fileOut)
	     if(idisp_legende.and.(it.eq.1)) then
	       idisp_legende=.false.
	       write(*,'(A19,1X,A8,1X,4A10)  ') 'date(unit:ug/m3)','var','sirane(ip)','fond(1)','grille(ip)' ,'prox(1)'
	     end if
	     !if(it.eq.1)&
	     write(*,'(A19,1X,A8,1X,4F10.2)') datestr,trim(adjustl(varstr(iv))),maxval(conc_sirane(:)),conc_fond(1),maxval(conc_grille(:) * ppb2microg(iv)),conc_prox(1)
	     
	     !Ecrit dans le fichier CARTOPROX
             if ( nf90_inq_varid(ncFileOutID, varstr_cartoprox, xVarID) .eq. nf90_noerr ) then
	       if(it.eq.1)&
	       write(*,*)'Ecrit  '//trim(varstr_cartoprox)
	       call check(nf90_put_var(ncFileOutID, xVarID, conc_cartoprox(:), count=(/numPoints/), start=start2d ) )
             end if
             if ( nf90_inq_varid(ncFileOutID, varstr_grille, xVarID) .eq. nf90_noerr ) then
	       if(it.eq.1)&
	       write(*,*)'Ecrit  '//trim(varstr_grille) 
	       call check(nf90_put_var(ncFileOutID, xVarID, conc_grille(:), count=(/numPoints/), start=start2d ) )
             end if
             if ( nf90_inq_varid(ncFileOutID, varstr_prox, xVarID) .eq. nf90_noerr ) then   
	       if(it.eq.1)&
	       write(*,*)'Ecrit  '//trim(varstr_prox)
	       call check(nf90_put_var(ncFileOutID, xVarID, conc_prox(:), count=(/1/), start=start1d ) )
             end if
             if ( nf90_inq_varid(ncFileOutID, varstr_fond, xVarID) .eq. nf90_noerr ) then  
	       if(it.eq.1)&
	       write(*,*)'Ecrit  '//trim(varstr_fond)
	       call check(nf90_put_var(ncFileOutID, xVarID, conc_fond(:), count=(/1/), start=start1d ) )	      
             end if
	     
	   else

             write(*,*) 'Manque une variable'
	     write(*,*) 'sirane=', isirane
	     write(*,*) 'grille=', igrille
	     write(*,*) 'fond='  , ifond
	     write(*,*) 'prox='  , iprox
	     
           end if	      
           
         end if !Variable dans le fichier
             
        end do !iv = 1, nvar
   
      end do 
      
      write(*,*)'Aire totale (m2)=',total      
            
      call check(nf90_close(ncFileID)) 
      call check(nf90_close(ncFileOutID))       

      write(*,*) '-> '//trim(fileOut)
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

