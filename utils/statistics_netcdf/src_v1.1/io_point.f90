   
!---------------------------------------------------------------------------

      subroutine get_dim_ncout_point(fileName,numPoints,numFrTimes,&
                      startYear,startMnth,startDay,startHour,startMin,startSec)

      use netcdf
      use typesizes
		
      implicit none
      		      
      ! General  
      character(len=256):: fileName   !Name of the input file              
      integer :: numPoints	     !Dimension of grid in Y direction
      integer :: numFrTimes	     !Numbers of time steps
      integer :: startYear	     !Four-digit year of start time
      integer :: startMnth	     !Month of start time
      integer :: startDay	     !Day of start time
      integer :: startHour	     !Hour of start time
      integer :: startMin	     !Minute of start time
      integer :: startSec	     !Second of start time
      character(len=33) :: tunits
      		      
      !netcdf related
      integer :: ncFileID, &
                 pointDimID, varDimID, frTimeDimID, eastingVarID, northingVarID, frTimeVarID, xVarID		           	 		 		 
      integer :: n      
      character(len=19) :: datestr       		      
      write(*,*) 'Read existing CDF '//trim(fileName)      
      call check(nf90_open(path = trim(fileName), mode = nf90_nowrite, ncid = ncFileID))       
      call check(nf90_inq_dimid(ncFileID, 'Point'      , pointDimID))     
!      call check(nf90_inq_dimid(ncFileID, 'Variable'   , varDimID)) 
      call check(nf90_inq_dimid(ncFileID, 'Time', frTimeDimID))                      
      call check(nf90_Inquire_Dimension(ncFileID, pointDimID   , len=numPoints))             
      call check(nf90_Inquire_Dimension(ncFileID, frTimeDimID, len= numFrTimes))    
! nombre de pas de temps
      call check(nf90_inq_dimid(ncFileID, 'Time', frTimeDimID)) 
      call check(nf90_Inquire_Dimension(ncFileID, frTimeDimID, len= numFrTimes))
      write(*,*) 'Point : ',numPoints 
      write(*,*) 'Trouve pas de temps : ',numFrTimes           
      ! date
      write(*,*) 'Lit date de CHIMERE'             	      
      call check(nf90_inq_varid(ncFileID, "Times", xVarID))    
      call check(nf90_get_var(ncFileID, xVarID, datestr, start = (/1,1/) ))     	      
      read(datestr,'(I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,I2)')&
     	     startYear,startMnth,startDay,startHour,startMin,startSec
      write(*,*) 'Trouve date initiale : '//datestr        
      call check(nf90_close(ncFileID)) 
               
      end subroutine get_dim_ncout_point
      
!---------------------------------------------------------------------------

   subroutine get_ncout_point_coord
   
      use netcdf
      use typesizes 
      use params
      
      implicit none          
   
      call check(nf90_open(fout1, nf90_nowrite, out1FileID)) 
       
      call check(nf90_inq_varid(out1FileID, 'x_pts', xVarID))    
      call check(nf90_get_var(out1FileID, xVarID, lon0(:,1),  start = (/1,1/) ))
      
      call check(nf90_inq_varid(out1FileID, 'y_pts', xVarID))    
      call check(nf90_get_var(out1FileID, xVarID, lat0(:,1),  start = (/1,1/) ))
      
      call check(nf90_inq_varid(out1FileID, 'easting_pts', xVarID))    
      call check(nf90_get_var(out1FileID, xVarID, easting0(:,1),  start = (/1,1/) ))
      
      call check(nf90_inq_varid(out1FileID, 'northing_pts', xVarID))    
      call check(nf90_get_var(out1FileID, xVarID, northing0(:,1),  start = (/1,1/) ))      
      
      call check(nf90_close(out1FileID))       

   end subroutine
!-------------------------------------------------------------------------------------
   subroutine create_ncstat_point
   
    use netcdf
    use typesizes 
    use params
    
    implicit none  

    call check(nf90_create(path = fstat, cmode = nf90_clobber, ncid = statfileID))

    call check(nf90_def_dim(ncid = statfileID, name = 'Point'  ,len = nx1, dimid = pointDimID))              
    call check(nf90_def_dim(ncid = statfileID, name = 'Time',len = nf90_unlimited, dimid = jourDimID))   
    call check(nf90_def_dim(ncid = statfileID, name = 'Mois',len = 12, dimid = moisDimID))          
    call check(nf90_def_dim(ncid = statfileID, name = 'DateStrLen',len = 19, dimid = frTimeStrDimID))    
          
    write(*,*) 'Dimensions OK'  
    call check(nf90_def_var(statfileID, 'Time', nf90_float, jourDimID, XVarID) )
    call check(nf90_put_att(statfileID, XVarID, 'long_name','jour'))
    call check(nf90_put_att(statfileID, XVarID, 'units',tunits))     
  
    call check(nf90_def_var(statfileID,'x_pts'    ,nf90_float,dimids=(/pointDimID/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Longitude centre mailles'))
    call check(nf90_put_att(statfileID, XVarID, 'units','degrees'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )    
       
    call check(nf90_def_var(statfileID,'y_pts'    ,nf90_float,dimids=(/pointDimID/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Latitude centre mailles'))
    call check(nf90_put_att(statfileID, XVarID, 'units','degrees'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )   
    
    call check(nf90_def_var(statfileID,'easting_pts'    ,nf90_float,dimids=(/pointDimID/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Longitude centre mailles'))
    call check(nf90_put_att(statfileID, XVarID, 'units','degrees'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )    
       
    call check(nf90_def_var(statfileID,'northing_pts'    ,nf90_float,dimids=(/pointDimID/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Latitude centre mailles'))
    call check(nf90_put_att(statfileID, XVarID, 'units','degrees'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )         
    
    call check(nf90_def_var(statfileID,'o3_moy_an'  ,nf90_float,dimids=(/pointDimID/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Moyenne annuelle O3'))
    call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )     
            
    call check(nf90_def_var(statfileID,'no2_moy_an'  ,nf90_float,dimids=(/pointDimID/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Moyenne annuelle NO2'))
    call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )
           
    call check(nf90_def_var(statfileID,'no_moy_an'  ,nf90_float,dimids=(/pointDimID/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Moyenne annuelle NO'))
    call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )      
    
    call check(nf90_def_var(statfileID,'o3_moy_mois'  ,nf90_float,dimids=(/pointDimID,moisDimId/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Moyenne mensuelle O3'))
    call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )     
            
    call check(nf90_def_var(statfileID,'no2_moy_mois'  ,nf90_float,dimids=(/pointDimID,moisDimId/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Moyenne mensuelle NO2'))
    call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )
    
    call check(nf90_def_var(statfileID,'no_moy_mois'  ,nf90_float,dimids=(/pointDimID,moisDimId/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Moyenne mensuelle NO'))
    call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )                               
     
    call check(nf90_def_var(statfileID,'aot80'  ,nf90_float,dimids=(/pointDimID/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','AOT 40 protection foret'))
    call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3.h'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) ) 

    call check(nf90_def_var(statfileID,'aot40'  ,nf90_float,dimids=(/pointDimID/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','AOT40 protection vegetation'))
    call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3.h'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )           
  
    call check(nf90_def_var(statfileID,'nb_dep_120_moy8h'  ,nf90_float,dimids=(/pointDimID/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Nb heures de depassements seuil 120 moyenne 8 h'))
    call check(nf90_put_att(statfileID, XVarID, 'units',''))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )  

    call check(nf90_def_var(statfileID,'nb_dep_180_1h'  ,nf90_float,dimids=(/pointDimID/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Nb heures de depassements seuil 180'))
    call check(nf90_put_att(statfileID, XVarID, 'units',''))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )  
    
    call check(nf90_def_var(statfileID,'nb_dep_240_1h'  ,nf90_float,dimids=(/pointDimID/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Nb heures de > seuil 240'))
    call check(nf90_put_att(statfileID, XVarID, 'units',''))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )      

    call check(nf90_def_var(statfileID,'nb_dep_120_jour'  ,nf90_float,dimids=(/pointDimID/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Nb de jours > seuil 120 moyenne 8 h'))
    call check(nf90_put_att(statfileID, XVarID, 'units',''))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )  

    call check(nf90_def_var(statfileID,'nb_dep_180_jour'  ,nf90_float,dimids=(/pointDimID/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Nb de jours > seuil 180 pour O3'))
    call check(nf90_put_att(statfileID, XVarID, 'units',''))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )  
    
    call check(nf90_def_var(statfileID,'nb_dep_240_jour'  ,nf90_float,dimids=(/pointDimID/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Nombre de jours > seuil 240 pour O3'))
    call check(nf90_put_att(statfileID, XVarID, 'units',''))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )      

    call check(nf90_def_var(statfileID,'nb_dep_140_jour'  ,nf90_float,dimids=(/pointDimID/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Nb de jours > seuil 140 pour NO2'))
    call check(nf90_put_att(statfileID, XVarID, 'units',''))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )  

    call check(nf90_def_var(statfileID,'nb_dep_200_jour'  ,nf90_float,dimids=(/pointDimID/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Nb de jours > seuil 200 pour NO2'))
    call check(nf90_put_att(statfileID, XVarID, 'units',''))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )  
    
    if (ihour) then        
      call check(nf90_def_var(statfileID,'o3_moy8h'  ,nf90_float,dimids=(/pointDimID,frTimeDimID/),varID=XVarID))    
      call check(nf90_put_att(statfileID, XVarID, 'long_name','Moyenne glissante O3 sur 8 heures'))
      call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3'))
      call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )   
    end if   
    
    if (ijour) then    
      call check(nf90_def_var(statfileID,'o3_max_1hr_jour'  ,nf90_float,dimids=(/pointDimID,jourDimID/),varID=XVarID))    
      call check(nf90_put_att(statfileID, XVarID, 'long_name','Maximum jour O3'))
      call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3'))
      call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) ) 
      
      call check(nf90_def_var(statfileID,'o3_max_8hr_jour'  ,nf90_float,dimids=(/pointDimID,jourDimID/),varID=XVarID))    
      call check(nf90_put_att(statfileID, XVarID, 'long_name','Maximum jour moyenne-8h  O3'))
      call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3'))
      call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )         
      
      call check(nf90_def_var(statfileID,'no2_max_1hr_jour'  ,nf90_float,dimids=(/pointDimID,jourDimID/),varID=XVarID))    
      call check(nf90_put_att(statfileID, XVarID, 'long_name','Maximum jour NO2'))
      call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3'))
      call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )                  
    
      call check(nf90_def_var(statfileID,'pm10_moy_jour'  ,nf90_float,dimids=(/pointDimID,jourDimID/),varID=XVarID))    
      call check(nf90_put_att(statfileID, XVarID, 'long_name','Moyenne jour'))
      call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3'))
      call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )     
    end if  
    
    call check(nf90_def_var(statfileID,'pm10_moy_an'  ,nf90_float,dimids=(/pointDimID/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Moyenne annuelle PM10'))
    call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )
    
    call check(nf90_def_var(statfileID,'pm10_moy_mois'  ,nf90_float,dimids=(/pointDimID,moisDimID/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Moyenne mensuelle PM10'))
    call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )              
   
    do is=1,nseuils_moy_jour    
      call check(nf90_def_var(statfileID, trim(seuils_moy_jour_str(is)),nf90_float,dimids=(/pointDimID/),varID=XVarID))    
      call check(nf90_put_att(statfileID, XVarID, 'long_name',trim(seuils_moy_jour_str(is))))
      call check(nf90_put_att(statfileID, XVarID, 'units',''))
      call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )      
    end do    
          
    ! Global attributes
    call check(nf90_put_att(statfileID, nf90_global, 'title'  , 'Statistiques annuelles SIRANE v'//trim(adjustl(version_str))//' - (c) AtmoRA 2010' ))      
    call check(nf90_put_att(statfileID, nf90_global, 'version', trim(adjustl(version_str))))   

    call check(nf90_close(statfileID))        
   
   
   end subroutine create_ncstat_point
!-----------------------------------------------------------------
   subroutine write_ncstat_point_coord
   
      use netcdf
      use typesizes 
      use params
      
      implicit none          
   
      call check(nf90_open(fstat, nf90_write, statfileID)) 

      write(*,*) 'Ecrit longitude x_pts'
      call check(nf90_inq_varid(statfileID, 'x_pts', xVarID))	      
      call check(nf90_put_var(statfileID, xVarID, lon0(:,1), start = (/ 1/) )) 
      
      write(*,*) 'Ecrit latitude y_pts'      
      call check(nf90_inq_varid(statfileID, 'y_pts', xVarID))   
      call check(nf90_put_var(statfileID, xVarID, lat0(:,1), start = (/ 1/) ))
      
      write(*,*) 'Ecrit longitude easting_pts'
      call check(nf90_inq_varid(statfileID, 'easting_pts', xVarID))	      
      call check(nf90_put_var(statfileID, xVarID, easting0(:,1), start = (/ 1/) )) 
      
      write(*,*) 'Ecrit latitude northing_pts'      
      call check(nf90_inq_varid(statfileID, 'northing_pts', xVarID))   
      call check(nf90_put_var(statfileID, xVarID, northing0(:,1), start = (/ 1/) ))       
                          
      call check(nf90_close(statfileID)) 
      
    end subroutine
!-------------------------------------------------------------------------------------
   subroutine ecrit_an_statnc_point
   
      use netcdf
      use typesizes 
      use params
      
      implicit none   
      
      call check(nf90_open(fstat, nf90_write, statfileID)) 
      
      if (igaz) then !GAZ 
	    
      write(*,*) 'o3_moy_an'
      call check(nf90_inq_varid(statfileID, 'o3_moy_an', xVarID))
      call check(nf90_put_var(statfileID, XVarID, o3_moy_an, start = (/ 1/) ))

      write(*,*) 'no2_moy_an'
      call check(nf90_inq_varid(statfileID, 'no2_moy_an', xVarID))
      call check(nf90_put_var(statfileID, XVarID, nox_moy_an(1,:,:), start = (/ 1/) ))

      write(*,*) 'no_moy_an'
      call check(nf90_inq_varid(statfileID, 'no_moy_an', xVarID))
      call check(nf90_put_var(statfileID, XVarID, nox_moy_an(2,:,:), start = (/ 1/) ))

      write(*,*) 'aot80'
      call check(nf90_inq_varid(statfileID, 'aot80', xVarID))
      call check(nf90_put_var(statfileID, XVarID, aot80, start = (/ 1/) ))

      write(*,*) 'aot40'
      call check(nf90_inq_varid(statfileID, 'aot40', xVarID))
      call check(nf90_put_var(statfileID, XVarID, aot40, start = (/ 1/) ))

      write(*,*) 'nb_dep_120_moy8h'
      call check(nf90_inq_varid(statfileID, 'nb_dep_120_moy8h', xVarID))
      call check(nf90_put_var(statfileID, XVarID, nb_dep_120_moy8h, start = (/ 1/) ))

      write(*,*) 'nb_dep_180_1h'
      call check(nf90_inq_varid(statfileID, 'nb_dep_180_1h', xVarID))
      call check(nf90_put_var(statfileID, XVarID, nb_dep_180_1h, start = (/ 1/) ))

      write(*,*) 'nb_dep_240_1h'
      call check(nf90_inq_varid(statfileID, 'nb_dep_240_1h', xVarID))
      call check(nf90_put_var(statfileID, XVarID, nb_dep_240_1h, start = (/ 1/) ))      
      
      write(*,*) 'nb_dep_120_jour'
      call check(nf90_inq_varid(statfileID, 'nb_dep_120_jour', xVarID))      
      call check(nf90_put_var(statfileID, XVarID, nb_dep_120_jour, start = (/ 1/) ))

      write(*,*) 'nb_dep_180_jour'          
      call check(nf90_inq_varid(statfileID, 'nb_dep_180_jour', xVarID))                 
      call check(nf90_put_var(statfileID, XVarID, nb_dep_180_jour, start = (/ 1/) ))
           
      write(*,*) 'nb_dep_240_jour'          
      call check(nf90_inq_varid(statfileID, 'nb_dep_240_jour', xVarID))                 
      call check(nf90_put_var(statfileID, XVarID, nb_dep_240_jour, start = (/ 1/) ))            

      write(*,*) 'nb_dep_140_jour'          
      call check(nf90_inq_varid(statfileID, 'nb_dep_140_jour', xVarID))                 
      call check(nf90_put_var(statfileID, XVarID, nb_dep_140_jour, start = (/ 1/) ))  
      
      write(*,*) 'nb_dep_200_jour'          
      call check(nf90_inq_varid(statfileID, 'nb_dep_200_jour', xVarID))                 
      call check(nf90_put_var(statfileID, XVarID, nb_dep_200_jour, start = (/ 1/) ))  

      end if !GAZ        

      if (ipm) then !PM
      
      write(*,*) 'pm10_moy_an'
      call check(nf90_inq_varid(statfileID, 'pm10_moy_an', xVarID))
      call check(nf90_put_var(statfileID, XVarID, pm10_moy_an, start = (/ 1/) ))
    
      do is= 1, nseuils_moy_jour      
        write(*,*) trim(seuils_moy_jour_str(is))
        call check(nf90_inq_varid(statfileID, trim(seuils_moy_jour_str(is)), xVarID))	   
        call check(nf90_put_var(statfileID, XVarID, nb_dep_x_jour(is,:,:), start = (/ 1/) ))
      end do
  
      end if !PM
      
      call check(nf90_close(statfileID)) 

   end subroutine ecrit_an_statnc_point  
!-------------------------------------------------------------------------------------     
   subroutine ecrit_mois_ncstat_point
   
      use netcdf
      use typesizes 
      use params
      
      implicit none   
             
      call check(nf90_open(fstat, nf90_write, statfileID))  
      
         ! calcul des moyennes mensuelles  
	 o3_moy_mois         = o3_moy_mois         / n_o3_moy_mois
 	 nox_moy_mois(1,:,:) = nox_moy_mois(1,:,:) / n_nox_moy_mois(1,:,:)
	 nox_moy_mois(2,:,:) = nox_moy_mois(2,:,:) / n_nox_moy_mois(2,:,:)
 	 pm10_moy_mois       = pm10_moy_mois       / n_pm10_moy_mois

         write(*,*) '-> Ecrit moyennes mensuelles pour le mois',mois_en_cours	 
	 
	 if (igaz) then !GAZ 
	   call check(nf90_inq_varid(statfileID, 'o3_moy_mois', xVarID))      
           call check(nf90_put_var(statfileID, XVarID,  o3_moy_mois, start = (/ 1,mois_en_cours/) ))
	   call check(nf90_inq_varid(statfileID, 'no2_moy_mois', xVarID))      
           call check(nf90_put_var(statfileID, XVarID, nox_moy_mois(1,:,:), start = (/ 1,mois_en_cours/) )) 
	   call check(nf90_inq_varid(statfileID, 'no_moy_mois', xVarID))      
           call check(nf90_put_var(statfileID, XVarID, nox_moy_mois(2,:,:), start = (/ 1,mois_en_cours/) )) 	   	     
	 end if
	  
	 if (ipm) then !PM
	   call check(nf90_inq_varid(statfileID, 'pm10_moy_mois', xVarID))	
           call check(nf90_put_var(statfileID, XVarID, pm10_moy_mois, start = (/ 1,mois_en_cours/) ))
         end if	             
      
      call check(nf90_close(statfileID)) 
            
   end subroutine ecrit_mois_ncstat_point     
!-------------------------------------------------------------------------------------
   subroutine ecrit_jour_ncstat_point
   
      use netcdf
      use typesizes 
      use params
      
      implicit none   
      
      !---------------------------------------------------------------       
      ! calcul des donnees journalieres
      !---------------------------------------------------------------
      if ( ijour_new ) then
              		
        ! --- PM10: jour de depassement du x ug/m3
        if (ipm) then
	
	do i1=1,nx1
	do j1=1,ny1	
	
	pm10_moy_jour(i1,j1) = pm10_moy_jour(i1,j1) / n_pm10_moy_jour(i1,j1)
			
        do is= 1, nseuils_moy_jour
        if ( nint(pm10_moy_jour(i1,j1)) .ge. seuils_moy_jour(is) ) &
          nb_dep_x_jour(is,i1,j1) =  nb_dep_x_jour(is,i1,j1) + 1     
        end do
	
	end do
	end do
	end if
	
	! --- O3/NO2: jour de depassement du x ug/m3
        if (igaz) then	
	
	do i1=1,nx1
	do j1=1,ny1
             
        ! --- O3: jour de depassement du 180 ug/m3 
        if ( nint(o3_max_1hr_jour(i1,j1)) .ge. 180 ) &
          nb_dep_180_jour(i1,j1) =  nb_dep_180_jour(i1,j1) + 1
        		  
        ! --- O3: jour de depassement du 240 ug/m3 
        if ( nint(o3_max_1hr_jour(i1,j1)) .ge. 240 ) &
          nb_dep_240_jour(i1,j1) =  nb_dep_240_jour(i1,j1) + 1  	  
          
        ! --- O3: jour de depassement du 120 ug/m3 
        if ( nint(o3_max_8hr_jour(i1,j1)) .ge. 120 ) &
          nb_dep_120_jour(i1,j1) =  nb_dep_120_jour(i1,j1) + 1  		 

        ! --- NO2: jour de depassement du 200 ug/m3 
        if ( nint(nox_max_1hr_jour(1,i1,j1)) .ge. 140 ) &
          nb_dep_140_jour(i1,j1) =  nb_dep_140_jour(i1,j1) + 1
          
        ! --- NO2: jour de depassement du 200 ug/m3 
        if ( nint(nox_max_1hr_jour(1,i1,j1)) .ge. 200 ) &
          nb_dep_200_jour(i1,j1) =  nb_dep_200_jour(i1,j1) + 1
	  
	end do
	end do
	
	end if	
           
      end if

      !---------------------------------------------------------------       
      ! calcul des degres jour
      !--------------------------------------------------------------- 
      if ( ijour_new ) then      
	do i1=1,nx1
	do j1=1,ny1	      		 
         dju_16 = (273.15 + 16) - ( tem2_max_1hr_jour(i1,j1) + tem2_min_1hr_jour(i1,j1) ) / 2.
         dju_18 = (273.15 + 18) - ( tem2_max_1hr_jour(i1,j1) + tem2_min_1hr_jour(i1,j1) ) / 2.         
         if ( dju_18.ge.0. ) then 
          degres_18_jour(i1,j1) = degres_18_jour(i1,j1) + dju_18
          degres_18_an(i1,j1)	= degres_18_an(i1,j1)	+ dju_18
         end if
         if ( dju_16.ge.0. ) then 
          degres_16_jour(i1,j1) = degres_16_jour(i1,j1) + dju_16
          degres_16_an(i1,j1)	= degres_16_an(i1,j1)	+ dju_16
         end if
	end do
	end do
      end if   
      
      !---------------------------------------------------------------       
      ! ecrit dans NetCDF
      !---------------------------------------------------------------          	      
             
      call check(nf90_open(fstat, nf90_write, statfileID))  
      
          it_jour = (it_en_cours + 1) / nheures_par_jour
	  
	  if(it_jour.eq.1)write(*,*) '-> Ecrit donnees pour it=',it_jour,'nheures=',nheures_par_jour
	  if (ipm) then		    
	    call check(nf90_inq_varid(statfileID, 'pm10_moy_jour', xVarID))      
            call check(nf90_put_var(statfileID, XVarID, pm10_moy_jour, start = (/ 1,it_jour/) ))	    
	  end if	  
	  
	  if (igaz) then !GAZ 
	    call check(nf90_inq_varid(statfileID, 'o3_max_1hr_jour', xVarID))      
            call check(nf90_put_var(statfileID, XVarID, o3_max_1hr_jour, start = (/ 1,it_jour/) ))

	    call check(nf90_inq_varid(statfileID, 'o3_max_8hr_jour', xVarID))      
            call check(nf90_put_var(statfileID, XVarID, o3_max_8hr_jour, start = (/ 1,it_jour/) ))
	  	  
	    call check(nf90_inq_varid(statfileID, 'no2_max_1hr_jour', xVarID))      
            call check(nf90_put_var(statfileID, XVarID, nox_max_1hr_jour(1,:,:), start = (/ 1,it_jour/) ))	    	    	  	
	  end if 
	    	    	  	  
          call check(nf90_inq_varid(statfileID, 'Time', xVarID))
          call check(nf90_put_var(statfileID, XVarID, it_jour-1, start = (/it_jour/) ))          
      
      call check(nf90_close(statfileID)) 
            
   end subroutine ecrit_jour_ncstat_point     
!-------------------------------------------------------------------------------------
   subroutine lit_step_ncstat_point
   
      use netcdf
      use typesizes 
      use params
      
      implicit none   
                 
      start2d=(/  1,  it/)
              
      o3_var='O3'   
      nox_var(1)='NO2' 
      nox_var(2)='NO'           
      pm10_var='PM10'      
                                
      call check(nf90_open(fout1, nf90_write, out1fileID))        

      if (igaz) then  
        ! O3            
        call check(nf90_inq_varid(out1fileID, o3_var, xVarID))   
        call check(nf90_get_var(out1fileID, xVarID, o3_0,  start=start2d ))           
        ! NO2 
        call check(nf90_inq_varid(out1fileID, nox_var(1), xVarID))
        call check(nf90_get_var(out1fileID, xVarID, nox_0(1,:,:),  start=start2d ))
        ! NO2
	call check(nf90_inq_varid(out1fileID, nox_var(2), xVarID))
        call check(nf90_get_var(out1fileID, xVarID, nox_0(2,:,:),  start=start2d ))		   
      end if !GAZ
      
      if (ipm) then                        
	! PM10
        call check(nf90_inq_varid(out1fileID, pm10_var, xVarID))	
        call check(nf90_get_var(out1fileID, xVarID, pm10_0,  start=start2d ))	 
      end if ! ipm      
      call check(nf90_close(out1fileID))                      
            
   end subroutine lit_step_ncstat_point
!-------------------------------------------------------------------------------------
