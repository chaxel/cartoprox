      subroutine get_dim_ncout_grille(fileName,numLons,numLats,numLays,&
                      numFrTimes,&
                      distGrid,startYear,startMnth,startDay,&
		      startHour,startMin,startSec)

      use netcdf
      use typesizes
		
      implicit none
      		      
      ! General  
      character(len=256):: fileName   !Name of the input file              
      integer :: numLats	     !Dimension of grid in Y direction
      integer :: numLons	     !Dimension of grid in X direction
      integer :: numLays	     !Number of sigma-P levels
      integer :: numSteps	     !Step number
      integer :: numFrTimes	     !Numbers of time steps
      real    :: distGrid	     !Distance of grid cells (in meters)
      integer :: startYear	     !Four-digit year of start time
      integer :: startMnth	     !Month of start time
      integer :: startDay	     !Day of start time
      integer :: startHour	     !Hour of start time
      integer :: startMin	     !Minute of start time
      integer :: startSec	     !Second of start time
      character(len=33) :: tunits
      		      
      !netcdf related
      integer :: ncFileID, &
                 latDimID, lonDimID, zDimID, frTimeDimID, timeDimID, &
                 latVarID, lonVarID, zVarID,frTimeVarID, scalarVarID, xvarID 
		 
      real,allocatable :: xCoord(:,:) ! X coordinates of the grid 
      real,allocatable :: tCoord(:)           	 		 
		 
      integer :: n
      
      character(len=19) :: datestr       		      

    write(*,*) 'Read existing CDF '//trim(fileName) 
     
    call check(nf90_open(path = trim(fileName), mode = nf90_nowrite, ncid = ncFileID))       

    call check(nf90_inq_dimid(ncFileID, 'west_east'  , lonDimID))     
    call check(nf90_inq_dimid(ncFileID, 'south_north'  , latDimID))
    call check(nf90_inq_dimid(ncFileID, 'bottom_top'  , zDimID))  
    call check(nf90_inq_dimid(ncFileID, 'Time', frTimeDimID))  
                   
    call check(nf90_Inquire_Dimension(ncFileID, latDimID   , len=numLats))
    call check(nf90_Inquire_Dimension(ncFileID, lonDimID   , len=numLons))    
    call check(nf90_Inquire_Dimension(ncFileID, zDimID     , len=numLays))            
    call check(nf90_Inquire_Dimension(ncFileID, frTimeDimID, len= numFrTimes))    

! nombre de pas de temps
      call check(nf90_inq_dimid(ncFileID, 'Time', frTimeDimID)) 
      call check(nf90_Inquire_Dimension(ncFileID, frTimeDimID, len= numFrTimes))
      write(*,*) 'Trouve pas de temps : ',numFrTimes   

      ! date
      write(*,*) 'Lit date de CHIMERE'             	      
      call check(nf90_inq_varid(ncFileID, 'Times', xVarID))    
      call check(nf90_get_var(ncFileID, xVarID, datestr, start = (/1,1/) ))
     	      
      read(datestr,'(I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,I2)')&
     	     startYear,startMnth,startDay,startHour,startMin,startSec
      write(*,*) 'Trouve date initiale : '//datestr

    allocate(xcoord(numLons,numLats))
    
    call check(nf90_inq_varid(ncFileID, 'lon', lonVarID))
    call check(nf90_get_var(ncFileID, lonVarID, xcoord, start = (/ 1,1/) ))
    distgrid= xcoord(2,1)-xcoord(1,1)
    
    write(*,*) 'distgrid(degrees)=',distgrid

    deallocate(xcoord)
        
    call check(nf90_close(ncFileID)) 
               
    end subroutine get_dim_ncout_grille
!-----------------------------------------------------------------
   subroutine get_ncout_grille_coord
   
      use netcdf
      use typesizes 
      use params
      
      implicit none          
   
      call check(nf90_open(fout1, nf90_nowrite, out1FileID)) 
       
      call check(nf90_inq_varid(out1FileID, 'lon', xVarID))    
      call check(nf90_get_var(out1FileID, xVarID, lon0,  start = (/1,1/) ))
      
      call check(nf90_inq_varid(out1FileID, 'lat', xVarID))    
      call check(nf90_get_var(out1FileID, xVarID, lat0,  start = (/1,1/) ))
      
      call check(nf90_close(out1FileID))       

   end subroutine
!-----------------------------------------------------------------
   subroutine write_ncstat_grille_coord
   
      use netcdf
      use typesizes 
      use params
      
      implicit none          
   
      call check(nf90_open(fstat, nf90_write, statfileID)) 

      write(*,*) 'Ecrit longitude lon'
      call check(nf90_inq_varid(statfileID, 'lon', xVarID))	      
      call check(nf90_put_var(statfileID, xVarID, lon0, start = (/ 1, 1/) )) 
      
      write(*,*) 'Ecrit latitude lat'      
      call check(nf90_inq_varid(statfileID, 'lat', xVarID))   
      call check(nf90_put_var(statfileID, xVarID, lat0, start = (/ 1, 1/) )) 
      
      write(*,*) 'Ecrit id_maille'      
      call check(nf90_inq_varid(statfileID, 'id_maille', xVarID))   
      call check(nf90_put_var(statfileID, xVarID, id_maille, start = (/ 1, 1/) )) 
      
      write(*,*) 'Ecrit id_maille_vm'      
      call check(nf90_inq_varid(statfileID, 'id_maille_vm', xVarID))   
      call check(nf90_put_var(statfileID, xVarID, id_maille_vm, start = (/ 1, 1/) ))        
              
      if ( region1km )write(*,*) 'Ecrit infos region'   
      if ( region1km )call check(nf90_inq_varid(statfileID, 'contour_region', xVarID))        
      if ( region1km )call check(nf90_put_var(statfileID, xVarID, rregion, start = (/ 1, 1/) )) 
      if ( region1km )call check(nf90_inq_varid(statfileID, 'dept', xVarID))  
      if ( region1km )call check(nf90_put_var(statfileID, xVarID, idept_region, start = (/ 1, 1, 1/) )) 
      if ( region1km )call check(nf90_inq_varid(statfileID, 'zone', xVarID))  
      if ( region1km )call check(nf90_put_var(statfileID, xVarID, izone_region, start = (/ 1, 1, 1/) )) 
      if ( region1km )call check(nf90_inq_varid(statfileID, 'pop99', xVarID)) 
      if ( region1km )call check(nf90_put_var(statfileID, xVarID, pop_region, start = (/ 1, 1/) )) 

      call check(nf90_close(statfileID)) 
      
    end subroutine

!-------------------------------------------------------------------------------------
   subroutine create_ncstat_grille
   
    use netcdf
    use typesizes 
    use params
    
    implicit none  

    call check(nf90_create(path = fstat, cmode = nf90_clobber, ncid = statfileID))

    call check(nf90_def_dim(ncid = statfileID, name = 'west_east'  ,len = nx1, dimid = lonDimID))        
    call check(nf90_def_dim(ncid = statfileID, name = 'south_north',len = ny1, dimid = latDimId  )) 
!    call check(nf90_def_dim(ncid = statfileID, name = 'bottom_top' ,len = 1, dimid = zDimId  ))      
    call check(nf90_def_dim(ncid = statfileID, name = 'Time',len = nf90_unlimited, dimid = jourDimID))   
    call check(nf90_def_dim(ncid = statfileID, name = 'Mois',len = 12, dimid = moisDimID))          
    call check(nf90_def_dim(ncid = statfileID, name = 'DateStrLen',len = 19, dimid = frTimeStrDimID))    
    call check(nf90_def_dim(ncid = statfileID, name = 'departements',len = 99, dimid = deptDimId  ))  
    call check(nf90_def_dim(ncid = statfileID, name = 'zones',len = 13, dimid = zoneDimId  ))  !deptDimId,zoneDimId
          
    write(*,*) 'Dimensions OK'  
    call check(nf90_def_var(statfileID, 'Time', nf90_float, jourDimID, XVarID) )
    call check(nf90_put_att(statfileID, XVarID, 'long_name','jour'))
    call check(nf90_put_att(statfileID, XVarID, 'units',tunits))     

    !call check(nf90_def_var(statfileID, 'Times', nf90_char, (/frTimeStrDimID,jourDimID/), XVarID) )
    !call check(nf90_put_att(statfileID, XVarID, 'long_name','forecast time'))
  
    call check(nf90_def_var(statfileID,'lon'    ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Longitude centre mailles'))
    call check(nf90_put_att(statfileID, XVarID, 'units','degrees'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )    
       
    call check(nf90_def_var(statfileID,'lat'    ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Latitude centre mailles'))
    call check(nf90_put_att(statfileID, XVarID, 'units','degrees'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) ) 
    
    call check(nf90_def_var(statfileID,'id_maille',nf90_int,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','ID de la maille'))
    call check(nf90_put_att(statfileID, XVarID, 'units','degrees'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999 ) )  
    
    call check(nf90_def_var(statfileID,'id_maille_vm',nf90_int,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','ID de la maille Vertical Mapper'))
    call check(nf90_put_att(statfileID, XVarID, 'units','degrees'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999 ) )  
    
    call check(nf90_def_var(statfileID,'o3_moy_an'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Moyenne annuelle O3'))
    call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )     
            
    call check(nf90_def_var(statfileID,'no2_moy_an'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Moyenne annuelle NO2'))
    call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) ) 

    call check(nf90_def_var(statfileID,'no_moy_an'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Moyenne annuelle NO'))
    call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )      
    
    call check(nf90_def_var(statfileID,'o3_moy_mois'  ,nf90_float,dimids=(/lonDimID,latDimId,moisDimId/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Moyenne mensuelle O3'))
    call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )     
            
    call check(nf90_def_var(statfileID,'no2_moy_mois'  ,nf90_float,dimids=(/lonDimID,latDimId,moisDimId/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Moyenne mensuelle NO2'))
    call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) ) 

    call check(nf90_def_var(statfileID,'no_moy_mois'  ,nf90_float,dimids=(/lonDimID,latDimId,moisDimId/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Moyenne mensuelle NO'))
    call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )                               
     
    call check(nf90_def_var(statfileID,'aot80'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','AOT 40 protection foret'))
    call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3.h'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) ) 

    call check(nf90_def_var(statfileID,'aot40'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','AOT40 protection vegetation'))
    call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3.h'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )           
  
    call check(nf90_def_var(statfileID,'nb_dep_120_moy8h'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Nombre heures > seuil 120 moyenne 8 h'))
    call check(nf90_put_att(statfileID, XVarID, 'units',''))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )  

    call check(nf90_def_var(statfileID,'nb_dep_180_1h'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Nombre heures > seuil 180'))
    call check(nf90_put_att(statfileID, XVarID, 'units',''))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )  
    
    call check(nf90_def_var(statfileID,'nb_dep_240_1h'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Nombre heures > seuil 240'))
    call check(nf90_put_att(statfileID, XVarID, 'units',''))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )      

    call check(nf90_def_var(statfileID,'nb_dep_120_jour'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Nombre de jours > seuil 120 moyenne 8 h'))
    call check(nf90_put_att(statfileID, XVarID, 'units',''))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )  

    call check(nf90_def_var(statfileID,'nb_dep_180_jour'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Nombre de jours > seuil 180 pour O3'))
    call check(nf90_put_att(statfileID, XVarID, 'units',''))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )  
    
    call check(nf90_def_var(statfileID,'nb_dep_240_jour'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Nombre de jours > seuil 240 pour O3'))
    call check(nf90_put_att(statfileID, XVarID, 'units',''))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )      

    call check(nf90_def_var(statfileID,'nb_dep_140_jour'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Nombre de jours > seuil 140 pour NO2'))
    call check(nf90_put_att(statfileID, XVarID, 'units',''))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )  

    call check(nf90_def_var(statfileID,'nb_dep_200_jour'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Nombre de jours > seuil 200 pour NO2'))
    call check(nf90_put_att(statfileID, XVarID, 'units',''))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )  
    
    if (ihour) then        
      call check(nf90_def_var(statfileID,'o3_moy8h'  ,nf90_float,dimids=(/lonDimID,latDimId,frTimeDimID/),varID=XVarID))    
      call check(nf90_put_att(statfileID, XVarID, 'long_name','Moyenne glissante O3 sur 8 heures'))
      call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3'))
      call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )   
    end if   
    
    if (ijour) then    
      call check(nf90_def_var(statfileID,'o3_max_1hr_jour'  ,nf90_float,dimids=(/lonDimID,latDimId,jourDimID/),varID=XVarID))    
      call check(nf90_put_att(statfileID, XVarID, 'long_name','Maximum jour O3'))
      call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3'))
      call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )
      
      call check(nf90_def_var(statfileID,'o3_max_8hr_jour'  ,nf90_float,dimids=(/lonDimID,latDimId,jourDimID/),varID=XVarID))    
      call check(nf90_put_att(statfileID, XVarID, 'long_name','Maximum jour moyenne-8h  O3'))
      call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3'))
      call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )

      call check(nf90_def_var(statfileID,'no2_max_1hr_jour'  ,nf90_float,dimids=(/lonDimID,latDimId,jourDimID/),varID=XVarID))    
      call check(nf90_put_att(statfileID, XVarID, 'long_name','Maximum jour NO2'))
      call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3'))
      call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )
    end if

    !if (ipm) then

    if (ijour) then
      call check(nf90_def_var(statfileID,'pm10_moy_jour'  ,nf90_float,dimids=(/lonDimID,latDimId,jourDimID/),varID=XVarID))    
      call check(nf90_put_att(statfileID, XVarID, 'long_name','Moyenne jour'))
      call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3'))
      call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) ) 
      if (iecart) then   
        call check(nf90_def_var(statfileID,'pm10_ecart_moy_jour'  ,nf90_float,dimids=(/lonDimID,latDimId,jourDimID/),varID=XVarID))    
        call check(nf90_put_att(statfileID, XVarID, 'long_name','Moyenne jour'))
        call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3'))
        call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )                
      end if

    end if
    
    !end if
    
    !if (ipm25) then
      call check(nf90_def_var(statfileID,'pm25_moy_an'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
      call check(nf90_put_att(statfileID, XVarID, 'long_name','Moyenne annuelle PM2.5'))
      call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3'))
      call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )       
    !end if    
    
    call check(nf90_def_var(statfileID,'pm10_moy_an'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Moyenne annuelle PM10'))
    call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )

    if (iecart) then    
      call check(nf90_def_var(statfileID,'pm10_ecart_moy_an'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
      call check(nf90_put_att(statfileID, XVarID, 'long_name','Moyenne annuelle ecart PM10'))
      call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3'))
      call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )  
    end if
    
    call check(nf90_def_var(statfileID,'pm10_moy_mois'  ,nf90_float,dimids=(/lonDimID,latDimId,moisDimID/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Moyenne mensuelle PM10'))
    call check(nf90_put_att(statfileID, XVarID, 'units','microg/m3'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )              
   
    do is=1,nseuils_moy_jour    
      call check(nf90_def_var(statfileID, trim(seuils_moy_jour_str(is)),nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
      call check(nf90_put_att(statfileID, XVarID, 'long_name','Nombre de depassements PM10 '//trim(seuils_moy_jour_str(is))))
      call check(nf90_put_att(statfileID, XVarID, 'units',''))
      call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )      
    end do    
        
    if (item2) then

    call check(nf90_def_var(statfileID,'degres_18_jour'  ,nf90_float,dimids=(/lonDimID,latDimId,jourDimID/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Degres jour 18 degresC'))
    call check(nf90_put_att(statfileID, XVarID, 'units','K'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )  

    call check(nf90_def_var(statfileID,'degres_16_jour'  ,nf90_float,dimids=(/lonDimID,latDimId,jourDimID/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Degres jour 16 degresC'))
    call check(nf90_put_att(statfileID, XVarID, 'units','K'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )  

    call check(nf90_def_var(statfileID,'degres_18_an'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Degres jour 18 degresC'))
    call check(nf90_put_att(statfileID, XVarID, 'units','K'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )  

    call check(nf90_def_var(statfileID,'degres_16_an'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Degres jour 16 degresC'))
    call check(nf90_put_att(statfileID, XVarID, 'units','K'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )  
    
    end if    

    if (region1km) then
    call check(nf90_def_var(statfileID,'contour_region'    ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Contour de la regoin Rhone-Alpes'))
    call check(nf90_put_att(statfileID, XVarID, 'units','0/1'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) ) 
           
    call check(nf90_def_var(statfileID,'dept'    ,nf90_float,dimids=(/lonDimID,latDimId,deptDimId/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Contour departements francais'))
    call check(nf90_put_att(statfileID, XVarID, 'units','0/1'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )                   

    call check(nf90_def_var(statfileID,'zone'    ,nf90_float,dimids=(/lonDimID,latDimId,zoneDimId/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Contour departements francais'))
    call check(nf90_put_att(statfileID, XVarID, 'units','0/1'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )   

    call check(nf90_def_var(statfileID,'pop99'   ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(statfileID, XVarID, 'long_name','Population au km carre'))
    call check(nf90_put_att(statfileID, XVarID, 'units','0/1'))
    call check(nf90_put_att(statfileID, XVarID,  "_FillValue", -9999.0 ) )            
    end if

    ! Global attributes
    call check(nf90_put_att(statfileID, nf90_global, 'title'  , 'Statistiques annuelles CHIMERE v'//trim(adjustl(version_str))//' - (c) AtmoRA 2010' ))      
    call check(nf90_put_att(statfileID, nf90_global, 'version', trim(adjustl(version_str))))   

    call check(nf90_close(statfileID))           
   
   end subroutine create_ncstat_grille

!-------------------------------------------------------------------------------------
   subroutine ecrit_an_statnc_grille
   
      use netcdf
      use typesizes 
      use params
      
      implicit none   
      
      call check(nf90_open(fstat, nf90_write, statfileID)) 

      if (ino) then !GAZ 

      write(*,*) 'no_moy_an'
      call check(nf90_inq_varid(statfileID, 'no_moy_an', xVarID))
      call check(nf90_put_var(statfileID, XVarID, nox_moy_an(2,:,:), start = (/ 1,1/) ))  

      end if      
      
      if (igaz) then !GAZ 
      
      write(*,*) 'o3_moy_an'
      call check(nf90_inq_varid(statfileID, 'o3_moy_an', xVarID))
      call check(nf90_put_var(statfileID, XVarID, o3_moy_an, start = (/ 1,1/) ))

      write(*,*) 'no2_moy_an'
      call check(nf90_inq_varid(statfileID, 'no2_moy_an', xVarID))
      call check(nf90_put_var(statfileID, XVarID, nox_moy_an(1,:,:), start = (/ 1,1/) ))

      write(*,*) 'aot80'
      call check(nf90_inq_varid(statfileID, 'aot80', xVarID))
      call check(nf90_put_var(statfileID, XVarID, aot80, start = (/ 1,1/) ))

      write(*,*) 'aot40'
      call check(nf90_inq_varid(statfileID, 'aot40', xVarID))
      call check(nf90_put_var(statfileID, XVarID, aot40, start = (/ 1,1/) ))

      write(*,*) 'nb_dep_120_moy8h'
      call check(nf90_inq_varid(statfileID, 'nb_dep_120_moy8h', xVarID))
      call check(nf90_put_var(statfileID, XVarID, nb_dep_120_moy8h, start = (/ 1,1/) ))

      write(*,*) 'nb_dep_180_1h'
      call check(nf90_inq_varid(statfileID, 'nb_dep_180_1h', xVarID))
      call check(nf90_put_var(statfileID, XVarID, nb_dep_180_1h, start = (/ 1,1/) ))

      write(*,*) 'nb_dep_240_1h'
      call check(nf90_inq_varid(statfileID, 'nb_dep_240_1h', xVarID))
      call check(nf90_put_var(statfileID, XVarID, nb_dep_240_1h, start = (/ 1,1/) ))      
      
      write(*,*) 'nb_dep_120_jour'
      call check(nf90_inq_varid(statfileID, 'nb_dep_120_jour', xVarID))      
      call check(nf90_put_var(statfileID, XVarID, nb_dep_120_jour, start = (/ 1,1/) ))

      write(*,*) 'nb_dep_180_jour'          
      call check(nf90_inq_varid(statfileID, 'nb_dep_180_jour', xVarID))                 
      call check(nf90_put_var(statfileID, XVarID, nb_dep_180_jour, start = (/ 1,1/) ))
           
      write(*,*) 'nb_dep_240_jour'          
      call check(nf90_inq_varid(statfileID, 'nb_dep_240_jour', xVarID))                 
      call check(nf90_put_var(statfileID, XVarID, nb_dep_240_jour, start = (/ 1,1/) ))            

      write(*,*) 'nb_dep_140_jour'          
      call check(nf90_inq_varid(statfileID, 'nb_dep_140_jour', xVarID))                 
      call check(nf90_put_var(statfileID, XVarID, nb_dep_140_jour, start = (/ 1,1/) ))  
      
      write(*,*) 'nb_dep_200_jour'          
      call check(nf90_inq_varid(statfileID, 'nb_dep_200_jour', xVarID))                 
      call check(nf90_put_var(statfileID, XVarID, nb_dep_200_jour, start = (/ 1,1/) ))  

      end if

      if (ipm) then
      
        write(*,*) 'pm10_moy_an'
        call check(nf90_inq_varid(statfileID, 'pm10_moy_an', xVarID))
        call check(nf90_put_var(statfileID, XVarID, pm10_moy_an, start = (/ 1,1/) ))

        if (iecart) then
         write(*,*) 'pm10_ecart_moy_an'
         call check(nf90_inq_varid(statfileID, 'pm10_ecart_moy_an', xVarID))
         call check(nf90_put_var(statfileID, XVarID, pm10_ecart_moy_an, start = (/ 1,1/) ))
        end if      

        do is= 1, nseuils_moy_jour      
          write(*,*) trim(seuils_moy_jour_str(is))
          call check(nf90_inq_varid(statfileID, trim(seuils_moy_jour_str(is)), xVarID))      
          call check(nf90_put_var(statfileID, XVarID, nb_dep_x_jour(is,:,:), start = (/ 1,1/) ))
        end do
  
      end if
      
      if (ipm25) then      
        write(*,*) 'pm25_moy_an'
        call check(nf90_inq_varid(statfileID, 'pm25_moy_an', xVarID))
	call check(nf90_put_var(statfileID, XVarID, pm25_moy_an, start = (/ 1,1/) ))
      end if      
      
      if (item2) then

      write(*,*) 'degres_18_an'          
      call check(nf90_inq_varid(statfileID, 'degres_18_an', xVarID))      
      call check(nf90_put_var(statfileID, XVarID, degres_18_an, start = (/ 1,1/) ))

      write(*,*) 'degres_16_an'          
      call check(nf90_inq_varid(statfileID, 'degres_16_an', xVarID))      
      call check(nf90_put_var(statfileID, XVarID, degres_16_an, start = (/ 1,1/) ))
      
      end if      
      
      call check(nf90_close(statfileID)) 

   end subroutine ecrit_an_statnc_grille
!-------------------------------------------------------------------------------------

   subroutine ecrit_mois_ncstat_grille
   
      use netcdf
      use typesizes 
      use params
      
      implicit none   
             
      call check(nf90_open(fstat, nf90_write, statfileID))  
      
         ! calcul des moyennes mensuelles  
	 o3_moy_mois     = o3_moy_mois  / n_o3_moy_mois
 	 nox_moy_mois    = nox_moy_mois / n_nox_moy_mois	 
 	 pm10_moy_mois   = pm10_moy_mois/ n_pm10_moy_mois

         write(*,*) '-> Ecrit moyennes mensuelles pour le mois',mois_en_cours	
	 
	 if (igaz) then !GAZ	  
	   call check(nf90_inq_varid(statfileID, 'o3_moy_mois', xVarID))      
           call check(nf90_put_var(statfileID, XVarID,  o3_moy_mois, start = (/ 1,1,mois_en_cours/) ))
	   call check(nf90_inq_varid(statfileID, 'no2_moy_mois', xVarID))      
           call check(nf90_put_var(statfileID, XVarID, nox_moy_mois(1,:,:), start = (/ 1,1,mois_en_cours/) ))
	 end if	   
	   
	 if (ino) then !NO	
	   call check(nf90_inq_varid(statfileID, 'no_moy_mois', xVarID))      
           call check(nf90_put_var(statfileID, XVarID, nox_moy_mois(2,:,:), start = (/ 1,1,mois_en_cours/) ))  	  
	 end if
	 
	 if (ipm) then
	   call check(nf90_inq_varid(statfileID, 'pm10_moy_mois', xVarID))	
           call check(nf90_put_var(statfileID, XVarID, pm10_moy_mois, start = (/ 1,1,mois_en_cours/) ))
         end if	             
      
      call check(nf90_close(statfileID)) 
            
   end subroutine ecrit_mois_ncstat_grille     
 
 !-------------------------------------------------------------------------------------
   subroutine ecrit_jour_ncstat_grille
   
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

        ! --- PM10: jour de depassement du x ug/m3
        if (ipm25) then
	
	do i1=1,nx1
	do j1=1,ny1		
	pm25_moy_jour(i1,j1) = pm25_moy_jour(i1,j1) / n_pm25_moy_jour(i1,j1)	
	end do
	end do
	
	end if
	
	! --- O3/NO2: jour de depassement du x ug/m3
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
      
          !it_jour = (it_en_cours + 1) / nheures_par_jour
	  it_jour = it_en_cours / nheures_par_jour
	  
	  !if(it_jour.eq.1)&
	  write(*,*) '-> Ecrit donnees pour it=',it_jour,'nheures=',nheures_par_jour
	  !DEBUGwrite(*,*) '-> Ecrit donnees pour le jour ',it_jour,'nheures=',nheures_par_jour		    
	  
	  if (ipm) then		    
	    call check(nf90_inq_varid(statfileID, 'pm10_moy_jour', xVarID))      
            call check(nf90_put_var(statfileID, XVarID, pm10_moy_jour, start = (/ 1,1,it_jour/) ))
	    
	    if (iecart) then    
	      call check(nf90_inq_varid(statfileID, 'pm10_ecart_moy_jour', xVarID))
              call check(nf90_put_var(statfileID, XVarID, pm10_ecart_moy_jour, start = (/ 1,1,it_jour/) ))
            end if
	  end if
	  	  
	  call check(nf90_inq_varid(statfileID, 'o3_max_1hr_jour', xVarID))      
          call check(nf90_put_var(statfileID, XVarID, o3_max_1hr_jour, start = (/ 1,1,it_jour/) ))
	  
	  call check(nf90_inq_varid(statfileID, 'o3_max_8hr_jour', xVarID))      
          call check(nf90_put_var(statfileID, XVarID, o3_max_8hr_jour, start = (/ 1,1,it_jour/) ))	  
	  	  
	  call check(nf90_inq_varid(statfileID, 'no2_max_1hr_jour', xVarID))      
          call check(nf90_put_var(statfileID, XVarID, nox_max_1hr_jour(1,:,:), start = (/ 1,1,it_jour/) ))	  	
	    	  
	  if (item2) then
	  call check(nf90_inq_varid(statfileID, 'degres_16_jour', xVarID))      
          call check(nf90_put_var(statfileID, XVarID, degres_16_jour, start = (/ 1,1,it_jour/) ))	  
	  
	  call check(nf90_inq_varid(statfileID, 'degres_18_jour', xVarID))      
          call check(nf90_put_var(statfileID, XVarID, degres_18_jour, start = (/ 1,1,it_jour/) ))	
	  end if  	  	  

          call check(nf90_inq_varid(statfileID, 'Time', xVarID))
          call check(nf90_put_var(statfileID, XVarID, it_jour-1, start = (/it_jour/) )) 
	  !write(*,*) 'OK'           
      
      call check(nf90_close(statfileID)) 
            
   end subroutine ecrit_jour_ncstat_grille  
!-------------------------------------------------------------------------------------
   subroutine lit_step_ncstat_grille
   
      use netcdf
      use typesizes 
      use params
      
      implicit none   
             
      count3d=(/nx1,ny1,  1     /)     
      start3d=(/  1,  1, it     /)
      start4d=(/  1,  1,  1, it /)           
      ! out.*.nc
      !write(*,*) 'Lit les variables chimie pour date '//trim(datestr)//' it=',it      
      o3_var='O3'
      no_var='NO'      
      nox_var(1)='NO2'
      nox_var(2)='NO'  
      pm10_var='PM10'
      pm25_var='PM25'      
             
      call check(nf90_open(fout1, nf90_write, out1fileID))        

      ! O3 --> si pas de sorties O3 : fixe le O3 à 0
      if (nf90_inq_varid(out1fileID, o3_var, xVarID).eq.nf90_noerr) then      
        call check(nf90_inquire_variable(out1fileID,  xVarID, ndims = dimensions ))     
        if (it .eq. 1)  write(*,*) '... '//trim(o3_var)//' ndims=', dimensions
        if ( dimensions .eq. 3 ) then
          call check(nf90_get_var(out1fileID, xVarID, o3_0,  start=start3d ))
        else if ( dimensions .eq. 4 ) then
          call check(nf90_get_var(out1fileID, xVarID, o3_0,  start=start4d ))      
        end if      
      else
        !write(*,*) 'Warning: pas de O3 disponible dans les sorties !'
        o3_0 = 0.
      end if
      
      ! NO2 --> si pas de sorties NO2 : fixe le NO2 à 0
      if (nf90_inq_varid(out1fileID, nox_var(1), xVarID) .eq. nf90_noerr ) then
        call check(nf90_inq_varid(out1fileID, nox_var(1), xVarID))
        call check(nf90_inquire_variable(out1fileID,  xVarID, ndims = dimensions ))  
        if (it .eq. 1)  write(*,*) '... '//trim(nox_var(1))//' ndims=', dimensions
        if ( dimensions .eq. 3 ) then
          call check(nf90_get_var(out1fileID, xVarID, nox_0(1,:,:),  start=start3d ))
        else if ( dimensions .eq. 4 ) then
          call check(nf90_get_var(out1fileID, xVarID, nox_0(1,:,:),  start=start4d ))      
        end if
      else
        !write(*,*) 'Warning: pas de NO2 disponible dans les sorties !'
        nox_0(1,:,:) = 0.
      end if
           
      ! NO --> si pas de sorties NO : fixe le NO à 0
      if (nf90_inq_varid(out1fileID, nox_var(2), xVarID) .eq. nf90_noerr ) then
        call check(nf90_inquire_variable(out1fileID,  xVarID, ndims = dimensions ))  
        if (it .eq. 1)  write(*,*) '... '//trim(nox_var(2))//' ndims=', dimensions
        if ( dimensions .eq. 3 ) then
          call check(nf90_get_var(out1fileID, xVarID, nox_0(2,:,:),  start=start3d ))
        else if ( dimensions .eq. 4 ) then
          call check(nf90_get_var(out1fileID, xVarID, nox_0(2,:,:),  start=start4d ))      
        end if
      else
        !write(*,*) 'Warning: pas de NO disponible dans les sorties !'
        nox_0(2,:,:) = 0.
      end if    

      if (item2) then
        call check(nf90_inq_varid(out1fileID, 'tem2', xVarID))
        call check(nf90_inquire_variable(out1fileID,  xVarID, ndims = dimensions ))  	   
        if (it .eq. 1)  write(*,*) '... tem2 ndims=', dimensions	
        if ( dimensions .eq. 3 ) then
          call check(nf90_get_var(out1fileID, xVarID, tem2_0,  start=start3d ))
        else if ( dimensions .eq. 4 ) then
          call check(nf90_get_var(out1fileID, xVarID, tem2_0,  start=start4d ))      
        end if	
      end if 
   
      if (ipm) then
                        
	! PM10 (TEOM ou FDMS)
        call check(nf90_inq_varid(out1fileID, pm10_var, xVarID))	
	call check(nf90_inquire_variable(out1fileID,  xVarID, ndims = dimensions ))   
        if (it .eq. 1)  write(*,*) '... '//trim(pm10_var)//' ndims=', dimensions
        if ( dimensions .eq. 3 ) then
          call check(nf90_get_var(out1fileID, xVarID, pm10k_0,  start=start3d ))
        else if ( dimensions .eq. 4 ) then
          call check(nf90_get_var(out1fileID, xVarID, pm10k_0,  start=start4d ))      
        end if	
		
	! nitrate
	if (iphno3) then
        call check(nf90_inq_varid(out1fileID, 'pHNO3', xVarID))
        if (it .eq. 1) write(*,*) '... pHNO3', dimensions
        call check(nf90_inquire_variable(out1fileID,  xVarID, ndims = dimensions ))     
        if ( dimensions .eq. 3 ) then
          call check(nf90_get_var(out1fileID, xVarID, phno3_0,  start=start3d ))
        else if ( dimensions .eq. 4 ) then
          call check(nf90_get_var(out1fileID, xVarID, phno3_0,  start=start4d ))      
        end if	
	end if
	
	! ECART_FDMS as dispo -> ECART FDMS = 0 		
	pm10k_ecart_0 = 0.
	! PM10 et pHNO3 	
	pm10_ecart_0  = phno3_0
	pm10_0 =  pm10k_0
	                    
      end if ! ipm  
      
      if (ipm25) then
                        
	! PM25 (TEOM ou FDMS)
        call check(nf90_inq_varid(out1fileID, pm25_var, xVarID))	
	call check(nf90_inquire_variable(out1fileID,  xVarID, ndims = dimensions ))   
        if (it .eq. 1)  write(*,*) '... '//trim(pm25_var)//' ndims=', dimensions
        if ( dimensions .eq. 3 ) then
          call check(nf90_get_var(out1fileID, xVarID, pm25_0,  start=start3d ))
        else if ( dimensions .eq. 4 ) then
          call check(nf90_get_var(out1fileID, xVarID, pm25_0,  start=start4d ))      
        end if	
	
      end if ! ipm        
      
      call check(nf90_close(out1fileID))                
            
   end subroutine lit_step_ncstat_grille       
!-------------------------------------------------------------------------------------
