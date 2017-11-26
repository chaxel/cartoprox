      program calc_cartoprox_nc
!****************************************************
!*     PREVALP/CARTOPROX                            *
!*     Calcul des concentration CARTPROX            *
!*     date : decembre 2010                         *
!*          auteur: E. Chaxel                       *
!*		       LEGI/GIERSA	            *
!*			chaxeleric@yahoo.fr         *
!****************************************************

!3.2 toutes les variaables ont le même nom
      use netcdf
      use typesizes

      implicit none 

      integer, parameter :: version=32
      
      character(len=256),parameter :: program_exe='calc_cartoprox_nc.exe'
      character(len=256) :: fic_sirane, fic_carto, fic_reg, fic_sur
      
      integer :: pointDimID, frTimeDimID, xVarID
      integer :: ncSiraneID, ncRegID, ncSurID  , ncOutID    
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
      
      real, allocatable :: conc_sirane(:,:)
      real, allocatable :: conc_grille(:,:)
      real, allocatable :: conc_cartoprox(:,:)
      real, allocatable :: conc_suremis(:,:)                  
      real, allocatable :: xpt(:)
      real, allocatable :: ypt(:)
      real, allocatable :: dist(:)       
      real, allocatable :: area(:)        
      real              :: conc_fond(nvar,1)         
      
      integer :: start1d(1)
      integer :: start2d(2)   
      
      integer :: it, iv, ip, ip2, i, j
      
      real :: x, y, dist2, ind, aire_totale
      
      logical :: idisp_legende !flags pour affichage de la legende
                   
      integer, parameter :: nprint = 100 ! nombre de point à afficher
      
      !DEBUG
      logical :: idebug 
            
!--------------------------------------------------------------------------------------------      
      logical :: isirane, ifond, igrille, iprox 

! ARGS  ---------------------------------------------------------------------------------             
      call getarg(1,fic_sirane)
      call getarg(2,fic_reg)
      call getarg(3,fic_sur)           
      call getarg(4,fic_carto)
      	    
!----------------------------------------------------------------------------------
      !idebug = .true.
      
      write(*,*)'calc_cartoprox_nc.f90 version:',version
       	                     
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
      allocate(conc_grille(nvar,numPoints)) 
      allocate(conc_cartoprox(nvar,numPoints)) 
      allocate(conc_suremis(nvar,numPoints))    
             
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
      call check(nf90_close(ncSiraneID))             
            
      !Ecrit dans le fichier d'entree
      call check(nf90_open(fic_sirane, nf90_write  , ncSiraneID))          
      if ( nf90_inq_varid(ncSiraneID, 'area_pts', xVarID) .eq. nf90_noerr ) &
        call check(nf90_put_var(ncSiraneID, xVarID, area, count=(/numPoints/), start=(/1/) ) )      
      call check(nf90_close(ncSiraneID))
      
      !Ecrit dans le fichier de sortie
      call check(nf90_open(fic_carto, nf90_write  , ncOutID))          
      if ( nf90_inq_varid(ncOutID, 'area_pts', xVarID) .eq. nf90_noerr ) &
        call check(nf90_put_var(ncOutID, xVarID, area, count=(/numPoints/), start=(/1/) ) )      
      call check(nf90_close(ncOutID))
      
!-------------------------------------------------------------------------------------------     
!Verifie la presence des varaibles
!-------------------------------------------------------------------------------------------
      if (idebug)write(*,*)'Ouvre '//trim(fic_sirane)
      if (nf90_open(fic_sirane, nf90_nowrite  ,ncSiraneID).ne.nf90_noerr)stop 'STOP: Erreur ouverture fichier SIRANE'  
      if (idebug)write(*,*)'Ouvre '//trim(fic_reg)
      if (nf90_open(fic_reg,    nf90_nowrite  ,ncRegID).ne.nf90_noerr)stop 'STOP: Erreur ouverture fichier REGIONAL'
      if (idebug)write(*,*)'Ouvre '//trim(fic_sur)
      if (nf90_open(fic_sur,    nf90_nowrite  ,ncSurID).ne.nf90_noerr)stop 'STOP: Erreur ouverture fichier SUREMIS'            
      if (idebug)write(*,*)'Ouvre '//trim(fic_carto)
      if (nf90_open(fic_carto,  nf90_write    ,ncOutID).ne.nf90_noerr)stop 'STOP: Erreur ouverture fichier CARTOPROX'         
!-------------------------------------------------------------------------------------------     
!Ouvre les fichiers
!-------------------------------------------------------------------------------------------      
      do iv = 1, nvar      
        !Defini les variables
        varstr_sirane	 = trim(adjustl(varstr(iv)))
	varstr_cartoprox = trim(adjustl(varstr(iv)))
        varstr_grille	 = trim(adjustl(varstr(iv)))
        varstr_prox      = trim(adjustl(varstr(iv)))
        varstr_fond	 = trim(adjustl(varstr(iv)))//'_fond'	
        isirane=.false.
        igrille=.false.
	iprox=.false.
        ifond=.false.	
        if ( nf90_inq_varid(ncRegID,    varstr_grille, xVarID) .eq. nf90_noerr)igrille=.true.
        if ( nf90_inq_varid(ncSurID,    varstr_prox,   xVarID) .eq. nf90_noerr)iprox=.true.  
        if ( nf90_inq_varid(ncSiraneID, varstr_fond,   xVarID) .eq. nf90_noerr)ifond=.true.
	
        if ( nf90_inq_varid(ncSiraneID, varstr_sirane, xVarID) .eq. nf90_noerr) then
	  isirane=.true.	   
          if (.not.(ifond.and.igrille.and.iprox.and.isirane)) then        
            write(*,*) 'Manque une variable'
            write(*,*) 'sirane ('//trim(varstr_sirane)//')=', isirane
            write(*,*) 'grille ('//trim(varstr_grille)//')=', igrille
            write(*,*) 'suremission ('//trim(varstr_prox)//')=', iprox	    
            write(*,*) 'fond ('//trim(varstr_fond)//')='  , ifond
            stop 'STOP: calc_cartoproc_nc.f90'
          end if
	end if
      end do

!-------------------------------------------------------------------------------------------     
!Calcul CARTOPROX horaire
!-------------------------------------------------------------------------------------------                                   	  	           
      !idebug = .true.

      do it = 1, numFrTimes
      
        start2d=(/  1,  it/)
        start1d=(/	it/)
	
	!Ecrit les pas de temps
	call check(nf90_inq_varid(ncSiraneID, 'Times', xVarID) )   
        call check(nf90_get_var(ncSiraneID, xVarID, datestr, start=start2d ) )
	
	call check(nf90_inq_varid(ncOutID, 'Times', xVarID) )   
        call check(nf90_put_var(ncOutID, xVarID, datestr, start=start2d ) )
	
	if(idebug)write(*,*)  	datestr  
      
        do iv = 1, nvar
	
	  !Nom des varaiables
          varstr_sirane	 = trim(adjustl(varstr(iv)))
	  varstr_cartoprox = trim(adjustl(varstr(iv)))
          varstr_grille	 = trim(adjustl(varstr(iv)))
          varstr_prox      = trim(adjustl(varstr(iv)))
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
           
	   if (idebug)write(*,*)'Lit '//trim(varstr_grille)//' dans '//trim(fic_reg)
           call check(nf90_inq_varid(ncRegID, varstr_grille, xVarID) )
           call check(nf90_get_var(ncRegID, xVarID, conc_grille(iv,:),count=(/numPoints/), start=start2d) )   
           
	   if (idebug)write(*,*)'Lit '//trim(varstr_prox)//' dans '//trim(fic_sur)
           call check(nf90_inq_varid(ncSurID, varstr_prox, xVarID) )
           call check(nf90_get_var(ncSurID, xVarID, conc_suremis(iv,:),count=(/numPoints/), start=start2d) )			   
	   
	   if (idebug)write(*,*)'Fin de lecture'
	   
	  end if ! var existe ? 

        end do !ivar
		
        !Formule CARTOPROX (2011, VERSION 3)
	!CARTOPROX = SIRANE - fond_SIRANE + fond_PREVALP - prox_SIRANE
        if (idebug)write(*,*) 'Calcul de la concentration CARTOPROX'
	conc_cartoprox(iv,:) = 0.
	do ip = 1, numPoints
          !VERSION 31
	  do iv = 1, nvar
	    conc_grille(iv,ip) = max ( -9999., conc_grille(iv,ip) * ppb2microg(iv) ) ! conversion ppbV -> ug/m3
	    conc_cartoprox(iv,ip) = max (0. , conc_sirane(iv,ip) - conc_fond(iv,1) + ( conc_grille(iv,ip) - conc_suremis(iv,ip) ) )
	  end do
	end do     
			
	!if (idebug)write(*,*) 'Ecriture dans fichier'
	if(it.eq.1)write(*,*)'-> '//trim(fic_carto)

	do iv = 1, nvar
		      
	   !Nom des varaiables
           varstr_sirane    = trim(adjustl(varstr(iv)))
	   varstr_cartoprox = trim(adjustl(varstr(iv)))
           varstr_grille    = trim(adjustl(varstr(iv)))
           varstr_prox      = trim(adjustl(varstr(iv)))
           varstr_fond	    = trim(adjustl(varstr(iv)))//'_fond'		   
   	 
	    !la variable SIRANE est dans le fichier ? isirane = .true.
	   if ( nf90_inq_varid(ncSiraneID, varstr_sirane, xVarID) .eq. nf90_noerr) then

           if (idebug)write(*,'(A19,1X,A8,1X,5F10.2)') &
	     datestr,trim(adjustl(varstr(iv))),conc_sirane(iv,1),conc_fond(iv,1),conc_grille(iv,1),conc_suremis(iv,1)	   
				 
	   !Affichage
	   if(idisp_legende.and.(it.eq.1)) then
	     idisp_legende=.false.
	     write(*,'(A19,1X,A8,1X,5A10)  ') 'date(unit:ug/m3)','var','sirane(ip)','fond(1)','grille(ip)','suremis(ip)'
	   end if
	   !if(it.eq.1)&
	   if (mod(it-1,24).eq.0) &
	   write(*,'(A19,1X,A8,1X,5F10.2)') &
	     datestr,trim(adjustl(varstr(iv))),conc_sirane(iv,1),conc_fond(iv,1),&
	     conc_grille(iv,1),conc_suremis(iv,1)


	   !Creer les variables de sortie
	   if (it.eq.1) then	 
	  
	   if ( index(varstr_sirane,'NO')+index(varstr_sirane,'O3').ne.0) then
             write(*,*)'Creer NO, NO2, O3, PM10 dans '//trim(varstr_sirane)
	     call create_var_pt_nc(ncOutID,'NO',xVarID)
	     call create_var_pt_nc(ncOutID,'NO2',xVarID)
	     call create_var_pt_nc(ncOutID,'O3',xVarID)
	   end if
	   if ( index(varstr_sirane,'PM').ne.0) then
	     write(*,*)'Creer '//trim(varstr_sirane)//' dans '//trim(fic_carto)
	     call create_var_pt_nc(ncOutID,varstr_sirane,xVarID)
	   end if
	   
	   end if
	   !fin de creation de variable 	 
	   
	   !Ecrit dans le fichier CARTOPROX	
           if ( nf90_inq_varid(ncOutID, varstr_cartoprox, xVarID) .eq. nf90_noerr ) then
	     if(idebug)&
	     write(*,*)'Ecrit  '//trim(varstr_cartoprox)
	     call check(nf90_put_var(ncOutID, xVarID, conc_cartoprox(iv,:), count=(/numPoints/), start=start2d ) )
           end if	   
	   
	 end if ! var existe ?				       
           
       end do !iv = 1, nvar
   
      end do !it
                   
      call check(nf90_close(ncSiraneID)) 
      call check(nf90_close(ncRegID))
      call check(nf90_close(ncSurID))
      call check(nf90_close(ncOutID))            

      write(*,*) '-> '//trim(fic_carto)
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

