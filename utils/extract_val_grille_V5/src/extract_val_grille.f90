      program extract_val_grille
! Ecrit les donnees a des points fournits dans un fichier X,Y dans le NetCDF CHIMERE 
! VERSION 3 : optimisation en ne lisant qu'un voisinage
! VERSION 4 : variable de sortie du NetCDF identique aux varaibles d'entrée

      use netcdf
      use typesizes       
      use params      

      implicit none
      
      real, allocatable :: conc_tmp(:)
      
      integer :: nhourperstep
      
      logical :: itime ! le temps est dans le fichier ?
      
      integer :: methode_interp
      
      !optimisation
      integer, allocatable :: is_valid(:)
      integer :: is2, nstat_valid            
       
      ! Lit les arguments
      call read_args 
!--------------------------------------------------------------------

      call check(nf90_open(fic_grille_nc, nf90_nowrite, ncFileID))

      if(idebug)write(*,*) 'Get dimensions...' ! assume 1 seul niveau     
      if (nf90_inq_dimid(ncFileID, 'west_east'   , lonDimID).ne.nf90_noerr)&
          call check(nf90_inq_dimid(ncFileID, 'X'   , lonDimID))       
      if (nf90_inq_dimid(ncFileID, 'south_north' , latDimID).ne.nf90_noerr)&
          call check(nf90_inq_dimid(ncFileID, 'Y'   , latDimID))
	  
      itime=.false.  
      if (nf90_inq_dimid(ncFileID, 'Time', frTimeDimID).eq.nf90_noerr)itime=.true.
               
      call check(nf90_Inquire_Dimension(ncFileID, latDimID   , len=nyy))
      call check(nf90_Inquire_Dimension(ncFileID, lonDimID   , len=nxx))             
      if (itime) then
        call check(nf90_Inquire_Dimension(ncFileID, frTimeDimID, len= numSteps))
	if (numSteps.eq.0)itime=.false.
      end if
      
      !Gere un fichier grille avec des données journalieres (pour PM10 par exemple)
      !ijour=.false.
      nhourperstep=1
      !Cette condition est un peu tordue:
      !Il faudrait lire les dates dans les fichiers
      if ((numSteps.eq.365).or.(numSteps.eq.366))ijour=.true.
      if (ijour)nhourperstep=24   
           		      
      TimeStringLen=60.
      if(idebug)write(*,*) 'Dimensions',nxx,nyy
      if(itime.and.idebug)write(*,*) 'Pas de temps dispo. ',numSteps
      

!--------------------------------------------------------------------
! Defini la grille CHIMERE X/Y
      allocate(xg (nxx,nyy))
      allocate(yg (nxx,nyy))     
      do j= 1, nyy
        do i= 1, nxx
	  xg(i,j) = xmin + (i-1+0.5) * dx
	  yg(i,j) = ymin + (j-1+0.5) * dy
	end do
      end do
      
      nstat = 1
      
      if(ific_stations) then
        
	if (.not.ific_stations_nc) then
	     
        !coordonnes des points (stations)     
        allocate(xp(smax))
	allocate(yp(smax))
	allocate(zp(smax))

        nstat = smax           
        open (unit=10,file=fic_stations,status='old')
        do is =1, nstat
	  read(10,*,end=9) xp(is), yp(is), zp(is)	
	end do	
9       continue
        nstat = is - 1	
        if ( nstat .ge. smax-1 ) then
          write(*,*) 'Augmente la valeur de smax dans params.f90'   
	  stop 1   
        end if	
	
       else if (ific_stations_nc) then
       	 
	 !lit les infos dans le netCDF	 
         call get_nc_info(fic_stations)
       	 
       end if ! ific_stations_nc

      end if                  
      
      if(idebug)write(*,*)'Stations lues:',nstat

      ! calcul de l'interpolation
      allocate(conc_tmp(nstat))      
      allocate(output(nstat))
      allocate(is_valid(nstat))
      allocate(ix(nstat))
      allocate(iy(nstat))
      allocate(wx(nstat))
      allocate(wy(nstat))
      
!Calcule les facteurs pour l'interpolation 
      if (index(var_nom,'GroundClass').ne.0) then           
        methode_interp = 1
      else
        methode_interp = 2
      end if
           
      if(ismooth) methode_interp=3

! ------------------------------------------------------------------- 
! Affiche la methode d'interpolation
!--------------------------------------------------------------------
      if (methode_interp.eq.1) then
      if(idebug)write(*,*) '->VALEUR AU POINT DE GRILLE'
      end if 
      
      if (methode_interp.eq.2) then
      if(idebug)write(*,*) '->INTERPOLATION BILINEAIRE conc ='     
      if(idebug)write(*,*) '(  (1.-wx)*(1.-wy)*concs(ix  ,iy  ) &'
      if(idebug)write(*,*) ' +     wx *(1.-wy)*concs(ix+1,iy  ) &'
      if(idebug)write(*,*) ' + (1.-wx)*    wy *concs(ix  ,iy+1) &' 
      if(idebug)write(*,*) ' +     wx *    wy *concs(ix+1,iy+1) )'
      end if
      
      if (methode_interp.eq.3) then
      if(idebug)write(*,*) '->SMOOTHING',nsmooth,'POINTS'
      end if              

! ------------------------------------------------------------------ 
! Calcul la correspondance des grilles
!--------------------------------------------------------------------      
      do is = 1, nstat
      ! ATTENTION : formules valables pour une grille reguliere      
      if ((methode_interp.eq.1).or.(methode_interp.eq.3)) then
        ix(is) = int ((xp(is) - xmin ) / dx + 1)
        iy(is) = int ((yp(is) - ymin ) / dy + 1)
        if(idebug)write(*,'("station ",I7," ix=",I4," iy=",I4)')is,ix(is),iy(is)  	
      else      
        ix(is) = int ((xp(is) - xmin - dx/2.) / dx) + 1      
        iy(is) = int ((yp(is) - ymin - dy/2.) / dy) + 1
	if ((ix(is).ge.1).and.(ix(is).le.nxx).and.(iy(is).ge.1).and.(iy(is).le.nyy)) then        	
	wx(is) = ( xp(is) - xg(ix(is),iy(is)) ) / dx
        wy(is) = ( yp(is) - yg(ix(is),iy(is)) ) / dx  
	else
	ix(is) = 0
        iy(is) = 0
	wx(is) = 0.
        wy(is) = 0.
        end if
	if(idebug)write(*,'("station ",I7," ix=",I4," iy=",I4," wx=",F8.2," wy=",F8.2)')& 
	            is,ix(is),iy(is),wx(is),wy(is)
      end if
      end do
      
! ------------------------------------------------------------------- 
! Calcul la taille du voisinage (OPTIMISATION VERSION 3)
!--------------------------------------------------------------------
      ix_voisinage(1) = nxx
      ix_voisinage(2) = 1
      iy_voisinage(1) = nyy
      iy_voisinage(2) = 1
      
      do is = 1, nstat
      ! ATTENTION : formules valables pour une grille reguliere      
        if ( ix(is) .lt. ix_voisinage(1) )ix_voisinage(1) = ix(is)
	if ( ix(is) .gt. ix_voisinage(2) )ix_voisinage(2) = ix(is)
        if ( iy(is) .lt. iy_voisinage(1) )iy_voisinage(1) = iy(is)
	if ( iy(is) .gt. iy_voisinage(2) )iy_voisinage(2) = iy(is)			
      end do   
                   
      ! Modifie le voisinage si besoin
      if (methode_interp.eq.2) then
        ix_voisinage(2) = ix_voisinage(2) + 1
        iy_voisinage(2) = iy_voisinage(2) + 1
      end if
      
      ! Modifie le voisinage si besoin
      if (methode_interp.eq.3) then
	ix_voisinage(1) = ix_voisinage(1) - nsmooth
        iy_voisinage(1) = iy_voisinage(1) - nsmooth        	
	ix_voisinage(2) = ix_voisinage(2) + nsmooth
        iy_voisinage(2) = iy_voisinage(2) + nsmooth
      end if 
           
      if ( ix_voisinage(1).lt.1   )ix_voisinage(1) = 1
      if ( iy_voisinage(1).lt.1   )iy_voisinage(1) = 1
      if ( ix_voisinage(2).gt.nxx )ix_voisinage(2) = nxx
      if ( iy_voisinage(2).gt.nyy )iy_voisinage(2) = nyy
   
! ------------------------------------------------------------------- 
! Modifie les dimensions et les indices 
!--------------------------------------------------------------------
      nstat_valid = 0
      do is = 1, nstat 
        if  ((ix(is).ge.1).and.(ix(is).le.nxx).and.(iy(is).ge.1).and.(iy(is).le.nyy)) then 
          !stations valides
	  nstat_valid = nstat_valid + 1
	  is_valid(nstat_valid) = is
	  ix(is) = ix(is) - ix_voisinage(1) + 1 
	  iy(is) = iy(is) - iy_voisinage(1) + 1
	end if		
      end do 
      
      nxx = ix_voisinage(2) - ix_voisinage(1) + 1
      nyy = iy_voisinage(2) - iy_voisinage(1) + 1       
                
      if (idebug)write(*,*) '********************Voisinage******************'
      if (idebug)write(*,*) 'i',ix_voisinage(:)
      if (idebug)write(*,*) 'j',iy_voisinage(:)
      if (idebug)write(*,*) 'nxx',nxx
      if (idebug)write(*,*) 'nyy',nyy 
      if (idebug)write(*,*) '***********************************************'		 

! ------------------------------------------------------------------- 
! Calcul la correspondance des grilles
!--------------------------------------------------------------------                 

! Allocations
      allocate(var_data(nxx,nyy,1))
      
      itstart=1
      itstop=numsteps
      
      if (istep) itstart=it1
      if (istep) itstop= it2
      
      if (.not.itime) itstart = 1
      if (.not.itime) itstop  = 1
      
      if (itstop.eq.0)itstop=itstart

      ! Si fichier de stations NetCDF -> determine les pas de temps de debut/fin
      if (ific_stations_nc) then       
        do it = 1, numsteps
         call check(nf90_inq_varid(ncFileID, 'Times', xVarID))
         call check(nf90_get_var(ncFileID, xVarID, datestr, start = (/1,it/) ))          
         if (datestr.eq.datestr_stations) then 
	   itstart = it
	   go to 97
	  end if  	
        end do	
97      continue		
	itstop = itstart + numsteps_stations/nhourperstep - 1
	it_stations = 0	
      end if            
       
      if(idebug)write(*,*)'Lit iterations du fichier grille :',itstart,'->',itstop
        
      !Quelques tests 
      if (nf90_inq_varid(ncFileID, trim(var_nom), xVarID).eq.0) then
        if (idebug.and.(it.eq.1)) write(*,*) 'Lit '//trim(var_nom)	
	call check(nf90_inquire_variable(ncFileID, xVarID, ndims = var_dim ))	      
        if (var_dim.eq.1) then
          write(*,*) 'Variable a 1 dimension non support: choisir une variable de dimension 2(X,Y) à 4(X,Y,Z,T)'
          stop 1
        end if
      end if
      
      nlev = 1
      
      !Ouvre un fichier existant
      !Format : Point, Time, easting_pts, northing_pts, x_pts, y_pts
      if (ihour.and.ific_stations_nc)call check(nf90_open(fic_stations, nf90_write, ncStatID))
      
! >>>>>>>>>>>>>>>>>> ITERATIONS >>>>>>>>>>>>>>>>>>>>
      do it=itstart,itstop
! >>>>>>>>>>>>>>>>>> ITERATIONS >>>>>>>>>>>>>>>>>>>>

      ! date             	      
       if (itime) then
        if ( nf90_inq_varid(ncFileID, 'Times', xVarID).eq.nf90_noerr) then    
        call check(nf90_get_var(ncFileID, xVarID, datestr, start = (/1,it/) ))    	      
	  read(datestr,'(I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,I2)',err=99)&
     	       Year,Mnth,Day,Hour,Minu,Sec 	  
	  goto 98     	       
99        continue ! en cas d'erreur
          itime=.false.	  
98        continue	       	       
	end if    
       end if
                   
      ! SURFACE  
       if (nf90_inq_varid(ncFileID, trim(var_nom), xVarID).eq.0) then
       
	 call check(nf90_inquire_variable(ncFileID, xVarID, ndims = var_dim ))		  	  	  
	 if (itime) then
           if (var_dim.eq.4)call check(nf90_get_var(ncFileID, xVarID, var_data(:,:,1), start=(/ix_voisinage(1),iy_voisinage(1),1,it/),count=(/nxx,nyy,nlev/))) 
           if (var_dim.eq.3)call check(nf90_get_var(ncFileID, xVarID, var_data(:,:,1), start=(/ix_voisinage(1),iy_voisinage(1),  it/),count=(/nxx,nyy,   1/)))
           if (var_dim.eq.2)call check(nf90_get_var(ncFileID, xVarID, var_data(:,:,1), start=(/ix_voisinage(1),iy_voisinage(1)     /),count=(/nxx,nyy,   1/))) 		   
	 else
           if (var_dim.eq.3)call check(nf90_get_var(ncFileID, xVarID, var_data(:,:,1), start=(/ix_voisinage(1),iy_voisinage(1),1   /),count=(/nxx,nyy,nlev/))) 
           if (var_dim.eq.2)call check(nf90_get_var(ncFileID, xVarID, var_data(:,:,1), start=(/ix_voisinage(1),iy_voisinage(1)     /),count=(/nxx,nyy,   1/))) 	
	 end if
       else
          if(idebug)write(*,*) '***ERREUR : Variable inconnue '//trim(var_nom)
	  var_data(:,:,1) = -9999.
       end if
         	 
       conc_tmp(:) = -9999.

       !VALEUR AU POINT
       if (methode_interp.eq.1) then

        do is2 = 1 , nstat_valid
          is = is_valid(is2)
          conc_tmp(is) = var_data(ix(is),iy(is),1)							
        end do  
       
       !INTERPOLATION LINEAIRE
       else if (methode_interp.eq.2) then
        
        do is2 = 1 , nstat_valid
          is = is_valid(is2)	
          conc_tmp(is) =  &
         (  (1.-wx(is))*(1.-wy(is))*   var_data(ix(is)  ,iy(is)  ,1)	&
          +	wx(is) *(1.-wy(is))*   var_data(ix(is)+1,iy(is)  ,1)	&
          + (1.-wx(is))*    wy(is) *   var_data(ix(is)  ,iy(is)+1,1)	&
          +	wx(is) *    wy(is) *   var_data(ix(is)+1,iy(is)+1,1)	)				
        end do

       !SMOOTHING 
       else if (methode_interp.eq.3) then
        	
        do is2 = 1 , nstat_valid
          is = is_valid(is2)
          if(idebug)write(*,*)'->SMOOTHING 2D',        nsmooth  	  
          if(idebug)write(*,*)ix(is), iy(is)		
          call smoothing2d(nxx,nyy,var_data(:,:,1),ix(is),iy(is),conc_tmp(is),nsmooth) 
        end do
        	
       end if

       !--------------Calcul des sorties       
       if ( agregation.eq.'moy') then
           output(:) = output(:) + conc_tmp(:)
       end if
         
       if (agregation.eq.'max') then
         do is =1, nstat
           if(conc_tmp(is).ge.output(is))output(is) = conc_tmp(is) 
         end do
       end if
          
       if(ihour)output(:) = conc_tmp(:) 	       
       
       if(idebug) then
         do is =1, nstat
           write(*,'("station ",I7,1X,A19,F10.2, " ppb")') is,datestr, output(is)	
         end do
       end if

       if(idebug)write(*,*)'ihour=',ihour
      ! EN HORAIRE : ECRIT LES RESULTATS HEURE PAR HEURE        
       if(ihour)then      
         if (.not.ific_stations_nc) then            
           write(*,'(A19,F10.2)') datestr,conc_tmp(is)      
         else if (ific_stations_nc) then ! par defaut en horaire         	 	 
	   !call check(nf90_open(fic_stations, nf90_write, ncStatID)) !Optimisation
	   call create_nc_var(trim(adjustl(var_nom)))
	   !Optimisation dans le cas NOX+
	   call check(nf90_inq_varid(ncStatID,trim(var_nom), xVarID))
	   if (mod(it_stations,24).eq.0)&
	     write(*,'("Date:",A19," var:",A5," steps:",I5," ->",I5," val max:",F10.2)') datestr,trim(var_nom),it_stations+1,it_stations+nhourperstep,maxval(output(1:nstat))
	   do ih=1,nhourperstep
	     it_stations = it_stations + 1	     
             call check(nf90_put_var(ncStatID, xVarID, output(1:nstat),  start = (/1,it_stations/) ))	       
	   end do
	   !call check(nf90_close(ncStatID)) !Optimisation
        end if  !ihour
      end if                  
! >>>>>>>>>>>>>>>>>> ITERATIONS >>>>>>>>>>>>>>>>>>>>
      end do
! >>>>>>>>>>>>>>>>>> ITERATIONS >>>>>>>>>>>>>>>>>>>>
      if (ihour.and.ific_stations_nc)call check(nf90_close(ncStatID))
      call check(nf90_close(ncFileID))  
      
!---------------------------------------------------------------------------  
      ! resultats
      if (.not.ihour) then
      do is =1, nstat
        if (agregation.eq.'moy')output(is)= output(is)/ (itstop - itstart + 1 )         
        write(*,*) xp(is), yp(is), zp(is), output(is) 
	output(is) = 0.
      end do
      end if               
!---------------------------------------------------------------------------                   
      if(idebug)write(*,*) 'Normal terminason of extract_val_grille' 
        
      end program

!-------------------------------------------------------------
      subroutine smoothing2d(nx,ny,in2d,i1,j1,out1p,ratio)
      
      !moyenne glissante d'une grille haute resolution vers une grille haute resolution
      
      implicit none
      
      integer :: i1, j1
      integer :: nx,ny
      integer :: ix(nx,ny),iy(nx,ny)     
      real    :: in2d (nx,ny)
      real    :: out1p
      
      integer :: imin, jmin
      integer :: imax, jmax          
      
      integer :: i2, j2
      
      integer :: ratio
      
      integer :: radius
            
      real :: nval
      
      out1p = 0.
      
      radius = int((ratio-1.+.1)/2.)
                           
      imin = i1 - radius
      jmin = j1 - radius
      imax = i1 + radius
      jmax = j1 + radius

      if (imin.lt. 1)imin=1
      if (jmin.lt. 1)jmin=1
      if (imax.gt.nx)imin=nx
      if (jmax.gt.ny)jmin=ny  

      nval = 0.

      do i2 = imin, imax
      do j2 = jmin, jmax	      
        nval = nval + 1.
        out1p = out1p + in2d(i2 ,j2 )	      
      end do
      end do
              
      out1p = out1p / nval
	 	 
	 
      end subroutine smoothing2d 
