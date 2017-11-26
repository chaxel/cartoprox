         subroutine get_nc_info(fic_nc,nx,ny,nz,nt)
	 
         use netcdf
         use typesizes  
	 	     	  
         implicit none	
	 
	 integer ::  ncFileID, xDimID, yDimID, zDimID, frTimeDimID
	 
	 integer :: nx, ny, nz, nt
	 
	 character(len=256) :: fic_nc

         call check(nf90_open(fic_nc, nf90_nowrite, ncFileID))
	  	 
         call check(nf90_inq_dimid(ncFileID,'west_east', xDimID))
         call check(nf90_Inquire_Dimension(ncFileID, xDimID, len=nx))

         call check(nf90_inq_dimid(ncFileID,'south_north', yDimID))
         call check(nf90_Inquire_Dimension(ncFileID, yDimID, len=ny))
	 
         if (nf90_inq_dimid(ncFileID,'bottom_top', zDimID).eq.nf90_noerr) &
           call check(nf90_Inquire_Dimension(ncFileID, zDimID, len=nz))	 	 

         call check(nf90_inq_dimid(ncFileID,'Time', frTimeDimID))
         call check(nf90_Inquire_Dimension(ncFileID, frTimeDimID, len=nt))
	 
         call check(nf90_close(ncFileID))	 
	  	 
	 end subroutine get_nc_info
! ---------------------------------------------------------------------------------------------------
         subroutine create_nc_var(ncFileID, varname)

         use netcdf
         use typesizes       
	  
         implicit none
	 
	 integer ::  ncFileID, xDimID, yDimID, zDimID, frTimeDimID, xVarID
	 logical :: ivert
	 character(len=*) :: varname
	 
	 ivert = .false.
	 
	 if ( nf90_inq_varid(ncFileID, trim(varname), xVarID) .ne. nf90_noerr ) then
         call check(nf90_inq_dimid(ncFileID,'west_east', xDimID))
         call check(nf90_inq_dimid(ncFileID,'south_north', yDimID))
         if (nf90_inq_dimid(ncFileID,'bottom_top', zDimID).eq.nf90_noerr)ivert=.true.
         call check(nf90_inq_dimid(ncFileID,'Time', frTimeDimID))
	 call check(nf90_redef(ncFileID))		        
	 	 
         write(*,*) 'Creation de la variable '//trim(varname)//' en cours...'
         
	 if (ivert)then
	 call check(nf90_def_var(ncFileID,trim(varname),nf90_float,dimids=(/xDimID,yDimID,zDimID,frTimeDimID/),varID=XVarID))             
	 else
	 call check(nf90_def_var(ncFileID,trim(varname),nf90_float,dimids=(/xDimID,yDimID,frTimeDimID/),varID=XVarID))
	 end if
	 call check(nf90_put_att(ncFileID, XVarID, 'long_name',trim(varname)))
         call check(nf90_put_att(ncFileID, XVarID, 'units','ppbV'))
         call check(nf90_put_att(ncFileID, XVarID,  "_FillValue", 0.0 ) )

	 call check(nf90_enddef(ncFileID))
         write(*,*) 'Creation OK'		 	 
		 
	 end if
	 	 	 		 
	 end subroutine
! ---------------------------------------------------------------------------------------------------
