         subroutine get_nc_info(fic_nc)
	 
         use netcdf
         use typesizes       
         use params
	  
         implicit none	 
	 
	 character(len=256) :: fic_nc

         call check(nf90_open(fic_nc, nf90_nowrite, ncStatID))
	  	 
         call check(nf90_inq_dimid(ncStatID,'Point', pointDimID))
         call check(nf90_Inquire_Dimension(ncStatID, pointDimID, len=nstat))

         call check(nf90_inq_dimid(ncStatID,'Time', frTimeDimID))
         call check(nf90_Inquire_Dimension(ncStatID, frTimeDimID, len=numsteps_stations))
	 
         call check(nf90_inq_varid(ncStatID, 'Times', xVarID))             
	 call check(nf90_get_var(ncStatID, xVarID, datestr_stations, start = (/1,1/) )) 	 	 	 
	       
         call check(nf90_inq_varid(ncStatID, 'easting_pts', xVarID))    
         call check(nf90_get_var(ncStatID, xVarID, xp(1:nstat),  start = (/1/) )) ! dans le systeme de coordonnées de la grille
      
         call check(nf90_inq_varid(ncStatID, 'northing_pts', xVarID))    
         call check(nf90_get_var(ncStatID, xVarID, yp(1:nstat),  start = (/1/) )) ! dans le systeme de coordonnées de la grille
	
         call check(nf90_close(ncStatID))
	 
	 end subroutine get_nc_info
! ---------------------------------------------------------------------------------------------------
         subroutine create_nc_var(varname)

         use netcdf
         use typesizes       
         use params
	  
         implicit none
	 
	 character(len=*) :: varname
	 
	 if ( nf90_inq_varid(ncStatID, trim(varname), xVarID) .ne. nf90_noerr ) then
         call check(nf90_inq_dimid(ncStatID,'Point',pointDimID))  
         call check(nf90_inq_dimid(ncStatID,'Time' ,frTimeDimID))	                  
	 write(*,*) 'Dimensions OK'
         write(*,*) 'Creation de la variable '//trim(varname)//' en cours...'	 	 
	 call check(nf90_redef(ncStatID))	       
         call check(nf90_def_var(ncStatID,trim(varname),nf90_float,dimids=(/pointDimID,frTimeDimID/),varID=XVarID))    
         call check(nf90_put_att(ncStatID, XVarID, 'long_name',trim(varname)))
         call check(nf90_put_att(ncStatID, XVarID, 'units','ppbV'))
         call check(nf90_put_att(ncStatID, XVarID,  "_FillValue", -9999.0 ) )      
	 call check(nf90_enddef(ncStatID))
         write(*,*) 'Redef OK'		
         end if  
	 
	 end subroutine
! ---------------------------------------------------------------------------------------------------
