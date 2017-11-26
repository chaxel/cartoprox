      program mosaique_grille

!Lit une grille CHIMERE d'une taille nx,ny,nz,xmin,ymin
!et ecrit dans une grille xminc,yminc,nxc,nyc,dxc,dyc

      use netcdf
      use typesizes       
      use params      

      implicit none      
      real, allocatable :: dataf(:,:)
      real, allocatable :: datac(:,:)      
      integer :: nhourperstep      
      logical :: itime ! le temps est dans le fichier ?      
      integer :: methode_interp         
       
      ! Lit les arguments
      call read_args 
      
!--------------------------------------------------------------------
! Grille FINE
!--------------------------------------------------------------------
      call get_nc_info(fic_fine,nxf,nyf,nzf,ntf)   
      
      !Gere un fichier grille avec des données journalieres (pour PM10 par exemple)    
      if(idebug)write(*,*) 'Dimensions',nxf,nyf,nzf
      if(idebug)write(*,*) 'Pas de temps dispo. ',ntf   
      allocate(xf (nxf,nyf))
      allocate(yf (nxf,nyf)) 
      allocate(dataf(nxf,nyf))             
      
      !Centre des mailles
      do j= 1, nyf
        do i= 1, nxf
	  xf(i,j) = xminf + (i - 0.5) * dxf
	  yf(i,j) = yminf + (j - 0.5) * dyf
	end do
      end do
      
!--------------------------------------------------------------------
! Grille COARSE
!--------------------------------------------------------------------
      call get_nc_info(fic_coarse,nxc,nyc,nzc,ntc)  
      
      !Gere un fichier grille avec des données journalieres (pour PM10 par exemple)    
      if(idebug)write(*,*) 'Dimensions',nxc,nyc,nzc
      if(idebug)write(*,*) 'Pas de temps dispo. ',ntc                  
      allocate(xc (nxc,nyc))
      allocate(yc (nxc,nyc)) 
      allocate(datac (nxc,nyc))       
      
      !Centre des mailles
      do j= 1, nyc
        do i= 1, nxc
	  xc(i,j) = xminc + (i - 0.5) * dxc
	  yc(i,j) = yminc + (j - 0.5) * dyc
	end do
      end do  
          
!--------------------------------------------------------------------
! Defini la grille CHIMERE X/Y   
!--------------------------------------------------------------------
      allocate(ix(nxf,nyf))
      allocate(iy(nxf,nyf))
      allocate(wx(nxf,nyf))
      allocate(wy(nxf,nyf))
      
! ------------------------------------------------------------------- 
! Affiche la methode d'interpolation
!--------------------------------------------------------------------
      !if(idebug)write(*,*) '->INTERPOLATION BILINEAIRE conc ='     
      !if(idebug)write(*,*) '(  (1.-wx)*(1.-wy)*concs(ix  ,iy  ) &'
      !if(idebug)write(*,*) ' +     wx *(1.-wy)*concs(ix+1,iy  ) &'
      !if(idebug)write(*,*) ' + (1.-wx)*    wy *concs(ix  ,iy+1) &' 
      !if(idebug)write(*,*) ' +     wx *    wy *concs(ix+1,iy+1) )'
           
! ------------------------------------------------------------------- 
! Calcul la correspondance des grilles (GRILLE REGULIERE)
!--------------------------------------------------------------------      
      do i = 1, nxf
        do j = 1, nyf     
          ix(i,j) = int ((xf(i,j) - xminc ) / dxc + 1)
          iy(i,j) = int ((yf(i,j) - yminc ) / dyc + 1)
	  if(idebug)write(*,*) i,j,'->',ix(i,j),iy(i,j)
	  if ( ix(i,j).lt.1 ) write(*,*) 'Warning mosaique_grille.f90: ix(i,j)<1'
	  if ( iy(i,j).lt.1 ) write(*,*) 'Warning mosaique_grille.f90: iy(i,j)<1'
	  if ( ix(i,j).gt.nxc ) write(*,*) 'Warning mosaique_grille.f90: ix(i,j)>',nxc
	  if ( iy(i,j).gt.nyc ) write(*,*) 'Warning mosaique_grille.f90: iy(i,j)>',nyc
	end do
      end do      

! ------------------------------------------------------------------- 
! Calcul la correspondance des grilles
!--------------------------------------------------------------------                 

! Allocations      
      itstart=1
      itstop=ntf

      call check(nf90_open(fic_fine  , nf90_nowrite, fFileID))
      call check(nf90_open(fic_coarse, nf90_write  , cFileID))
                        
! >>>>>>>>>>>>>>>>>> ITERATIONS >>>>>>>>>>>>>>>>>>>>
      do it=itstart,itstop
! >>>>>>>>>>>>>>>>>> ITERATIONS >>>>>>>>>>>>>>>>>>>>            	      
        if ( nf90_inq_varid(fFileID, 'Times', xVarID).eq.nf90_noerr) then    
          call check(nf90_get_var(fFileID, xVarID, datestr, start = (/1,it/) ))    	      
	  read(datestr,'(I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,I2)')Year,Mnth,Day,Hour,Minu,Sec 	   	              	         
       end if
        if (idebug)write(*,*) 'Date '//trim(datestr)	  
                   
      ! SURFACE  
        if (nf90_inq_varid(fFileID, trim(var_nom), xVarID).eq.0) then
          if (idebug) write(*,*) 'Lit '//trim(var_nom)	  
	  call check(nf90_inquire_variable(fFileID, xVarID, ndims = var_dim ))
	  if (var_dim.eq.1) then
	    write(*,*) 'Variable a 1 dimension non support: choisir une variable de dimension 2(X,Y) à 4(X,Y,Z,T)'
	    stop 1
	  else
	    if (idebug)write(*,*)'dims=',var_dim
	  end if
	    	           
	  if(var_dim.eq.3)call check(nf90_get_var(fFileID, xVarID, dataf(:,:) ,  start=(/1,1,it/),count=(/nxf,nyf/)))
	  if(var_dim.eq.4)call check(nf90_get_var(fFileID, xVarID, dataf(:,:) ,  start=(/1,1,1,it/),count=(/nxf,nyf/)))
	  
	  if(idebug)write(*,*) 'maxval:',maxval(dataf)
	  
        else
		 	 
	  dataf(:,:) = 0.
	  
	end if
	
	!Ecrit

	call create_nc_var(cFileID, trim(var_nom))
	if (nf90_inq_varid(cFileID, trim(var_nom), xVarID).eq.0) then

	  !time
	  if (idebug) write(*,*) 'Ecrit date COARSE '//trim(datestr)
	  call check(nf90_inq_varid(cFileID, 'Times', xVarID))
	  call check(nf90_put_var(cFileID, xVarID, datestr, start = (/1,it/) ))	
	
	  call check(nf90_inq_varid(cFileID, trim(var_nom), xVarID))
	  call check(nf90_inquire_variable(cFileID, xVarID, ndims = var_dim ))
	  if (idebug) write(*,*) 'Lit COARSE '//trim(var_nom)		  
	  if (idebug)write(*,*)'dims=',var_dim
	  
	  if(var_dim.eq.3)call check(nf90_get_var(cFileID, xVarID, datac(:,:) ,  start=(/1,1,it/),count=(/nxc,nyc/)))
	  if(var_dim.eq.4)call check(nf90_get_var(cFileID, xVarID, datac(:,:) ,  start=(/1,1,1,it/),count=(/nxc,nyc/)))
	  
	  !sum les contribution
	  do i = 1, nxf
            do j = 1, nyf
	      datac(ix(i,j),iy(i,j)) = dataf(i,j)           
	    end do
          end do
	  
	  if (idebug) write(*,*) 'Ecrit COARSE '//trim(var_nom)	
	  if(var_dim.eq.3)call check(nf90_put_var(cFileID, xVarID, datac(:,:) ,start=(/1,1,it/),count=(/nxc,nyc/) ) )
          if(var_dim.eq.4)call check(nf90_put_var(cFileID, xVarID, datac(:,:) ,start=(/1,1,1,it/),count=(/nxc,nyc/) ) )
	  
	
	end if
                
! >>>>>>>>>>>>>>>>>> ITERATIONS >>>>>>>>>>>>>>>>>>>>
      end do
! >>>>>>>>>>>>>>>>>> ITERATIONS >>>>>>>>>>>>>>>>>>>>

      call check(nf90_close(cFileID))  
      call check(nf90_close(fFileID))      
!---------------------------------------------------------------------------  
      if(idebug)write(*,*) '->'//trim(fic_coarse)
      if(idebug)write(*,*) 'Normal terminason of mosaique_grille' 
        
      end program
