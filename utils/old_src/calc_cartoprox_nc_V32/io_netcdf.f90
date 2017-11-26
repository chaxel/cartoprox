! ---------------------------------------------------------------------------------------------------
    subroutine create_nc(fic,nx,ny)

    use netcdf
    use typesizes	
     
    implicit none


    integer :: i, j
    
    integer :: ncFileID, lonDimID, latDimID, zDimID, timeDimID, timeStrDimID
    integer :: lonVarID, latVarID, timeVarID, xVarID
    
    integer :: nx,ny
    
    character(len=*) :: fic     
	 
    call check(nf90_create(path = fic, cmode = nf90_clobber, ncid = ncFileID))
    
    call check(nf90_def_dim(ncid = ncFileID, name = 'west_east'  ,len = nx, dimid = lonDimID))        
    call check(nf90_def_dim(ncid = ncFileID, name = 'south_north',len = ny, dimid = latDimId  ))     
    call check(nf90_def_dim(ncid = ncFileID, name = 'bottom_top' ,len = 1, dimid = zDimId  ))    
    call check(nf90_def_dim(ncid = ncFileID, name = 'DateStrLen',len = 19, dimid = timeStrDimID))       
    call check(nf90_def_dim(ncid = ncFileID, name = 'Time',len = nf90_unlimited, dimid = timeDimID)) 	

    call check(nf90_def_var(ncFileID,'easting',nf90_float,dimids=(/lonDimID,latDimID/),varID=XVarID))    
    call check(nf90_put_att(ncFileID, XVarID, 'long_name','easting'))
    call check(nf90_put_att(ncFileID, XVarID, 'units','m'))
    call check(nf90_put_att(ncFileID, XVarID,  "_FillValue", -9999.0 ) )   

    call check(nf90_def_var(ncFileID,'northing',nf90_float,dimids=(/lonDimID,latDimID/),varID=XVarID))    
    call check(nf90_put_att(ncFileID, XVarID, 'long_name','northing'))
    call check(nf90_put_att(ncFileID, XVarID, 'units','m'))
    call check(nf90_put_att(ncFileID, XVarID,  "_FillValue", -9999.0 ) )

    call check(nf90_def_var(ncFileID, 'Times', nf90_char, (/timeStrDimID,timeDimID/), XVarID) )
    call check(nf90_put_att(ncFileID, XVarID, 'long_name','forecast time'))
	     
    call check(nf90_close(ncFileID))   
	 
    end subroutine
! ---------------------------------------------------------------------------------------------------
    subroutine create_var2d_nc(ncFileID,varname,xVarID)

    use netcdf
    use typesizes	
     
    implicit none

    integer :: ncFileID, lonDimID, latDimID, zDimID, timeDimID
    integer :: lonVarID, latVarID, timeVarID, xVarID

    character(len=*) :: varname
    				    
    !call check(nf90_open(fic, nf90_write, ncFileID) )

    if ( nf90_inq_varid(ncFileID, trim(varname), xVarID) .ne. nf90_noerr ) then
!----------------------------------------------------	 	 
      call check(nf90_inq_dimid(ncFileID,'west_east',lonDimID))  
      call check(nf90_inq_dimid(ncFileID,'south_north' ,latDimID)) 
        			       
      call check(nf90_redef(ncFileID))  					      
      write(*,*) 'Creation de la variable '//trim(varname)//' en cours...'	       
      call check(nf90_def_var(ncFileID,trim(varname),nf90_float,dimids=(/lonDimID,latDimID/),varID=XVarID))    
      call check(nf90_put_att(ncFileID, XVarID, 'long_name',trim(varname)))
      call check(nf90_put_att(ncFileID, XVarID, 'units','m2'))
      call check(nf90_put_att(ncFileID, XVarID,  "_FillValue", -9999.0 ) )			      
      call check(nf90_enddef(ncFileID))
      write(*,*) 'Creation raster '//trim(varname)//' OK'
!----------------------------------------------------	 	 		
    end if  
	  	 
    end subroutine
! ---------------------------------------------------------------------------------------------------
    subroutine create_var_nc(ncFileID,varname,xVarID)

    use netcdf
    use typesizes	
     
    implicit none

    integer :: ncFileID, lonDimID, latDimID, zDimID, timeDimID
    integer :: xVarID

    character(len=*) :: varname
    				    
    !call check(nf90_open(fic, nf90_write, ncFileID) )

    if ( nf90_inq_varid(ncFileID, trim(varname), xVarID) .ne. nf90_noerr ) then
!----------------------------------------------------	 	 
      call check(nf90_inq_dimid(ncFileID,'west_east',lonDimID))  
      call check(nf90_inq_dimid(ncFileID,'south_north' ,latDimID))
      call check(nf90_inq_dimid(ncFileID,'bottom_top' ,zDimID))      
      call check(nf90_inq_dimid(ncFileID,'Time' ,timeDimID))  
        			       
      call check(nf90_redef(ncFileID))  					      
      write(*,*) 'Creation de la variable '//trim(varname)//' en cours...'	       
      call check(nf90_def_var(ncFileID,trim(varname),nf90_float,dimids=(/lonDimID,latDimID,zDimID,timeDimID/),varID=XVarID))	
      call check(nf90_put_att(ncFileID, XVarID, 'long_name',trim(varname)))
      call check(nf90_put_att(ncFileID, XVarID, 'units','microg/m3'))
      call check(nf90_put_att(ncFileID, XVarID,  "_FillValue", -9999.0 ) )			      
      call check(nf90_enddef(ncFileID))
      write(*,*) 'Creation raster '//trim(varname)//' OK'
!----------------------------------------------------	 	 		
    end if  

    end subroutine
! ---------------------------------------------------------------------------------------------------
    subroutine create_var_pt_nc(ncFileID,varname,xVarID)

    use netcdf
    use typesizes	
     
    implicit none

    integer :: ncFileID, ptsDimID,  timeDimID
    integer :: xVarID

    character(len=*) :: varname
    				    
    !call check(nf90_open(fic, nf90_write, ncFileID) )

    if ( nf90_inq_varid(ncFileID, trim(varname), xVarID) .ne. nf90_noerr ) then
!----------------------------------------------------	 	 
      !write(*,*)'Lit Point & Time'
      call check(nf90_inq_dimid(ncFileID,'Point',ptsDimID))	     
      call check(nf90_inq_dimid(ncFileID,'Time' ,timeDimID))  
        			       
      call check(nf90_redef(ncFileID))  					      
      write(*,*) 'Creation de la variable '//trim(varname)//' en cours...'	       
      call check(nf90_def_var(ncFileID,trim(varname)	,nf90_float,(/ptsDimID,timeDimID/),XVarID))    
      call check(nf90_put_att(ncFileID, XVarID, 'long_name','Concentration '//trim(varname)))
      call check(nf90_put_att(ncFileID, XVarID, 'units','microg/m3'))
      call check(nf90_put_att(ncFileID, XVarID,  '_FillValue', 0.0 ) )  
      call check(nf90_put_att(ncFileID, XVarID,  'missing_value', -9999.0 ) )		      
      call check(nf90_enddef(ncFileID))
      write(*,*) 'Creation raster '//trim(varname)//' OK'
!----------------------------------------------------	 	 		
    end if  
	 
    end subroutine	 
	 
