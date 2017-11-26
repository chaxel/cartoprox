      program extract_val_grille
! Ecrit les donnees a des points fournits dans un fichier X,Y dans le NetCDF CHIMERE 

      use netcdf
      use typesizes       
      use params      

      implicit none
      
      real :: conc_tmp
      
      logical :: itime ! le temps est dans le fichier ?
      
      integer :: methode_interp            
       
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
      if (itime)call check(nf90_Inquire_Dimension(ncFileID, frTimeDimID, len= numSteps))    
    		      
      TimeStringLen=60.
      if(idebug)write(*,*) 'Dimensions',nxx,nyy
      if(idebug)write(*,*) 'Pas de temps dispo. ',numSteps      

!--------------------------------------------------------------------                                                        
!      write(*,*) 'Lit longitude/latitude de CHIMERE...'
!      allocate(lon (nxx,nyy))
!      allocate(lat (nxx,nyy))            
!      call check(nf90_inq_varid(ncFileID, 'lon' , lonVarID)) 
!      call check(nf90_inq_varid(ncFileID, 'lat' , latVarID))     
    
!      call check(nf90_get_var(ncFileID, lonVarID, lon,  start=(/1,1/)))
!      call check(nf90_get_var(ncFileID, latVarID, lat,  start=(/1,1/)))
!      call check(nf90_close(ncFileID))
      
!      exit
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
       	  	 
         call get_nc_info(fic_stations)
	 
       end if ! ific_stations_nc


      end if                  
      
      if(idebug)write(*,*)'Stations lues:',nstat

      
!Calcule les facteurs pour l'interpolation 
      if (index(var_nom,'GroundClass').ne.0) then           
        methode_interp = 1
      else
        methode_interp = 2
      end if
      
      if(ismooth) methode_interp=3

!AFFICHAGE
      if (methode_interp.eq.2) then
      if(idebug)write(*,*) '->INTERPOLATION BILINEAIRE conc ='     
      if(idebug)write(*,*) '(  (1.-wx)*(1.-wy)*concs(ix  ,iy  ) &'
      if(idebug)write(*,*) ' +     wx *(1.-wy)*concs(ix+1,iy  ) &'
      if(idebug)write(*,*) ' + (1.-wx)*    wy *concs(ix  ,iy+1) &' 
      if(idebug)write(*,*) ' +     wx *    wy *concs(ix+1,iy+1) )'
      end if      
      
      if (methode_interp.eq.1) then
      if(idebug)write(*,*) '->VALEUR AU POINT DE GRILLE'
      end if 
      
      if (methode_interp.eq.3) then
      if(idebug)write(*,*) '->SMOOTHING',nsmooth,'POINTS'
      end if              
      
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
	wx(is) = 0.
        wy(is) = 0.
        end if
	if(idebug)write(*,'("station ",I7," ix=",I4," iy=",I4," wx=",F8.2," wy=",F8.2)')& 
	            is,ix(is),iy(is),wx(is),wy(is)
      end if
      end do	 
		 
! ------------------------------------
        !-------------------------------------------------------------------- 
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
      
      output = 0.

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
	itstop = itstart + numsteps_stations - 1
	it_stations = 0	
       end if
        
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
          if (idebug.and.(it.eq.1)) write(*,*) 'Lit '//trim(var_nom)
	  
	  call check(nf90_inquire_variable(ncFileID, xVarID, ndims = var_dim ))		  
	  	  
	  nlev = 1
	  
	  if (var_dim.eq.1) then
	    write(*,*) 'Variable a 1 dimension non support: choisir une variable de dimension 2(X,Y) à 4(X,Y,Z,T)'
	    stop 1
	  end if
	  	  
	  if (itime) then
            if (var_dim.eq.4)call check(nf90_get_var(ncFileID, xVarID, var_data(:,:,1) ,  start=(/1,1,1,it/),count=(/nxx,nyy,nlev/))) 
            if (var_dim.eq.3)call check(nf90_get_var(ncFileID, xVarID, var_data(:,:,1) ,  start=(/1,1,  it/),count=(/nxx,nyy,   1/)))
            if (var_dim.eq.2)call check(nf90_get_var(ncFileID, xVarID, var_data(:,:,1) ,  start=(/1,1     /),count=(/nxx,nyy,   1/)))	    	     
	  else
            if (var_dim.eq.3)call check(nf90_get_var(ncFileID, xVarID, var_data(:,:,1) ,  start=(/1,1,1   /),count=(/nxx,nyy,nlev/))) 
            if (var_dim.eq.2)call check(nf90_get_var(ncFileID, xVarID, var_data(:,:,1) ,  start=(/1,1     /),count=(/nxx,nyy,   1/)))	  
	  end if
	else
          if(idebug)write(*,*) '***ERREUR : Variable inconnue '//trim(var_nom)
	  var_data(:,:,1) = -999.
	end if

        if (methode_interp.eq.1) then

	 do is =1, nstat
	 if ((ix(is).ge.1).and.(ix(is).le.nxx).and.(iy(is).ge.1).and.(iy(is).le.nyy)) then	 
	   conc_tmp = var_data(ix(is),iy(is),1)
	 else
	   conc_tmp = -999.	 
	 end if  	 	 	          
	 output(is)= output(is) +  conc_tmp
         if(ihour)output(is)= conc_tmp 	 
         if(idebug.and.      itime )write(*,'("station ",I7,1X,A19,F10.2, " ppb")') is,datestr, output(is)  / (it - itstart + 1 )
         if(idebug.and.(.not.itime))write(*,'("station ",I7,1X    ,F10.2, " ppb")') is,         output(is)  / (it - itstart + 1 )	 
         end do	 

	else if (methode_interp.eq.2) then
	 
	 do is =1, nstat	     	 	 
	 if ((ix(is).ge.1).and.(ix(is).le.nxx).and.(iy(is).ge.1).and.(iy(is).le.nyy)) then
	   conc_tmp =  &
	  (  (1.-wx(is))*(1.-wy(is))*   var_data(ix(is)  ,iy(is)  ,1) 	 &
           +     wx(is) *(1.-wy(is))*   var_data(ix(is)+1,iy(is)  ,1) 	 &
           + (1.-wx(is))*    wy(is) *   var_data(ix(is)  ,iy(is)+1,1) 	 &
           +     wx(is) *    wy(is) *   var_data(ix(is)+1,iy(is)+1,1) 	 )
         else
	   conc_tmp = -999.	 
	 end if 	  
	 if ( agregation.eq.'moy') output(is)= output(is) + conc_tmp 
	 if ((agregation.eq.'max').and.(conc_tmp.ge.output(is)))output(is)= conc_tmp 	
         if(ihour)output(is)= conc_tmp 	  	 
         if(idebug)write(*,'("station ",I7,1X,A19,F10.2, " ppb")') is,datestr, output(is)	 
         end do
	 
	else if (methode_interp.eq.3) then
		 
	 do is =1, nstat
	   if ((ix(is).ge.1).and.(ix(is).le.nxx).and.(iy(is).ge.1).and.(iy(is).le.nyy)) then
	     if(idebug)write(*,*)'->SMOOTHING 2D',	nsmooth	 	   
	     if(idebug)write(*,*)ix(is), iy(is)	   	 
	     call smoothing2d(nxx,nyy,var_data(:,:,1),ix(is),iy(is),conc_tmp,nsmooth)	
           else
	     conc_tmp = -999.	 
	   end if 	      	  
	   if ( agregation.eq.'moy') output(is)= output(is) + conc_tmp 
	   if ((agregation.eq.'max').and.(conc_tmp.ge.output(is)))output(is)= conc_tmp 	 
           if(ihour)output(is)= conc_tmp 	   
           if(idebug)write(*,'("station ",I7,1X,A19,F10.2, " ppb")') is,datestr, output(is)	   
         end do
	 	 
	end if
      ! EN HORAIRE : ECRIT LES RESULTATS HEURE PAR HEURE 
      if(ihour)then      
      
      if (.not.ific_stations_nc) then            
         write(*,'(A19,F10.2)') datestr,conc_tmp      
      else if (ific_stations_nc) then ! par defaut en horaire         	 
	 it_stations = it_stations + 1
	 call check(nf90_open(fic_stations, nf90_write, ncStatID))	 	 
         call create_nc_var(trim(adjustl(var_nom))//'_grille')	 	        
         call check(nf90_inq_varid(ncStatID,trim(var_nom)//'_grille', xVarID))    
         call check(nf90_put_var(ncStatID, xVarID, output(1:nstat),  start = (/1,it_stations/) ))	 
         call check(nf90_close(ncStatID))	 
      end if
      
      end if
      
            
! >>>>>>>>>>>>>>>>>> ITERATIONS >>>>>>>>>>>>>>>>>>>>
      end do
! >>>>>>>>>>>>>>>>>> ITERATIONS >>>>>>>>>>>>>>>>>>>>

      call check(nf90_close(ncFileID))  
      
!---------------------------------------------------------------------------  
      ! resultats
      if (.not.ihour) then
      do is =1, nstat
        if (agregation.eq.'moy')output(is)= output(is)/ (itstop - itstart + 1 )         
        write(*,*) xp(is), yp(is), zp(is), output(is) 
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
