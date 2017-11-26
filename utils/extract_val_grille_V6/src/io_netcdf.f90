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
	 
         allocate(xp(nstat))
	 allocate(yp(nstat))
	 allocate(zp(nstat))	 	 	 	 
	       
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
	 call check(nf90_redef(ncStatID))		        	 	 
         if ( (trim(varname).eq.'NO2').or.(trim(varname).eq.'NO').or.(trim(varname).eq.'O3')) then	 
	   write(*,*) 'Creation des variables NO, NO2, O3  en cours...'	 	 	 
	   call check(nf90_def_var(ncStatID,'NO',nf90_float,dimids=(/pointDimID,frTimeDimID/),varID=XVarID))    
           call check(nf90_put_att(ncStatID, XVarID, 'long_name','Concentration NO'))
           call check(nf90_put_att(ncStatID, XVarID, 'units','ppbV'))
           call check(nf90_put_att(ncStatID, XVarID,  "_FillValue", -9999.0 ) )	 
           call check(nf90_def_var(ncStatID,'NO2',nf90_float,dimids=(/pointDimID,frTimeDimID/),varID=XVarID))    
           call check(nf90_put_att(ncStatID, XVarID, 'long_name','Concentration NO2'))
           call check(nf90_put_att(ncStatID, XVarID, 'units','ppbV'))
           call check(nf90_put_att(ncStatID, XVarID,  "_FillValue", -9999.0 ) )	 	 	 
	   call check(nf90_def_var(ncStatID,'O3',nf90_float,dimids=(/pointDimID,frTimeDimID/),varID=XVarID))    
           call check(nf90_put_att(ncStatID, XVarID, 'long_name','Concentration O3'))
           call check(nf90_put_att(ncStatID, XVarID, 'units','ppbV'))
           call check(nf90_put_att(ncStatID, XVarID,  "_FillValue", -9999.0 ) )	 	 	 	 
	 else	 
           write(*,*) 'Creation de la variable '//trim(varname)//' en cours...'		  
           call check(nf90_def_var(ncStatID,trim(varname),nf90_float,dimids=(/pointDimID,frTimeDimID/),varID=XVarID))    
           call check(nf90_put_att(ncStatID, XVarID, 'long_name',trim(varname)))
           call check(nf90_put_att(ncStatID, XVarID, 'units','ppbV'))
           call check(nf90_put_att(ncStatID, XVarID,  "_FillValue", -9999.0 ) )	 	 
	 end if	 	 	 
	 call check(nf90_enddef(ncStatID))
         write(*,*) 'Creation OK'		
         end if  
	 
	 end subroutine
! ---------------------------------------------------------------------------------------------------
