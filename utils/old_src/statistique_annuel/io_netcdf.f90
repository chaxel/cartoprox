   subroutine create_cdf
   
    use netcdf
    use typesizes 
    use params
    
    implicit none  

    call check(nf90_create(path = fout2, cmode = nf90_clobber, ncid = out2fileID))

    call check(nf90_def_dim(ncid = out2fileID, name = 'west_east'  ,len = nx1, dimid = lonDimID))        
    call check(nf90_def_dim(ncid = out2fileID, name = 'south_north',len = ny1, dimid = latDimId  )) 
!    call check(nf90_def_dim(ncid = out2fileID, name = 'bottom_top' ,len = 1, dimid = zDimId  ))      
    call check(nf90_def_dim(ncid = out2fileID, name = 'Time',len = nf90_unlimited, dimid = jourDimID))   
    call check(nf90_def_dim(ncid = out2fileID, name = 'Mois',len = 12, dimid = moisDimID))          
    call check(nf90_def_dim(ncid = out2fileID, name = 'DateStrLen',len = 19, dimid = frTimeStrDimID))    
    call check(nf90_def_dim(ncid = out2fileID, name = 'departements',len = 99, dimid = deptDimId  ))  
    call check(nf90_def_dim(ncid = out2fileID, name = 'zones',len = 13, dimid = zoneDimId  ))  !deptDimId,zoneDimId
          
    write(*,*) 'Dimensions OK'  
    call check(nf90_def_var(out2fileID, 'Time', nf90_float, jourDimID, XVarID) )
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','jour'))
    call check(nf90_put_att(out2fileID, XVarID, 'units',tunits))     

    !call check(nf90_def_var(out2fileID, 'Times', nf90_char, (/frTimeStrDimID,jourDimID/), XVarID) )
    !call check(nf90_put_att(out2fileID, XVarID, 'long_name','forecast time'))
  
    call check(nf90_def_var(out2fileID,'lon'    ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','Longitude centre mailles'))
    call check(nf90_put_att(out2fileID, XVarID, 'units','degrees'))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) )    
       
    call check(nf90_def_var(out2fileID,'lat'    ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','Latitude centre mailles'))
    call check(nf90_put_att(out2fileID, XVarID, 'units','degrees'))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) ) 
    
    call check(nf90_def_var(out2fileID,'id_maille',nf90_int,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','ID de la maille'))
    call check(nf90_put_att(out2fileID, XVarID, 'units','degrees'))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999 ) )  
    
    call check(nf90_def_var(out2fileID,'id_maille_vm',nf90_int,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','ID de la maille Vertical Mapper'))
    call check(nf90_put_att(out2fileID, XVarID, 'units','degrees'))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999 ) )  
    
    call check(nf90_def_var(out2fileID,'o3_moy_an'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','Moyenne annuelle O3'))
    call check(nf90_put_att(out2fileID, XVarID, 'units','microg/m3'))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) )     
            
    call check(nf90_def_var(out2fileID,'no2_moy_an'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','Moyenne annuelle NO2'))
    call check(nf90_put_att(out2fileID, XVarID, 'units','microg/m3'))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) )  
    
    call check(nf90_def_var(out2fileID,'o3_moy_mois'  ,nf90_float,dimids=(/lonDimID,latDimId,moisDimId/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','Moyenne mensuelle O3'))
    call check(nf90_put_att(out2fileID, XVarID, 'units','microg/m3'))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) )     
            
    call check(nf90_def_var(out2fileID,'no2_moy_mois'  ,nf90_float,dimids=(/lonDimID,latDimId,moisDimId/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','Moyenne mensuelle NO2'))
    call check(nf90_put_att(out2fileID, XVarID, 'units','microg/m3'))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) )                           
     
    call check(nf90_def_var(out2fileID,'aot80'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','AOT 40 protection foret'))
    call check(nf90_put_att(out2fileID, XVarID, 'units','microg/m3.h'))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) ) 

    call check(nf90_def_var(out2fileID,'aot40'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','AOT40 protection vegetation'))
    call check(nf90_put_att(out2fileID, XVarID, 'units','microg/m3.h'))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) )           
  
    call check(nf90_def_var(out2fileID,'nb_dep_120_moy8h'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','Nombre heures de depassements seuil 120 moyenne 8 h'))
    call check(nf90_put_att(out2fileID, XVarID, 'units',''))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) )  

    call check(nf90_def_var(out2fileID,'nb_dep_180_1h'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','Nombre heures de depassements seuil 180'))
    call check(nf90_put_att(out2fileID, XVarID, 'units',''))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) )  
    
    call check(nf90_def_var(out2fileID,'nb_dep_240_1h'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','Nombre heures de depassements seuil 240'))
    call check(nf90_put_att(out2fileID, XVarID, 'units',''))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) )      

    call check(nf90_def_var(out2fileID,'nb_dep_120_jour'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','Nombre de jours de depassements seuil 120 moyenne 8 h'))
    call check(nf90_put_att(out2fileID, XVarID, 'units',''))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) )  

    call check(nf90_def_var(out2fileID,'nb_dep_180_jour'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','Nombre de jours de depassements seuil 180 pour O3'))
    call check(nf90_put_att(out2fileID, XVarID, 'units',''))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) )  
    
    call check(nf90_def_var(out2fileID,'nb_dep_240_jour'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','Nombre de jours de depassements seuil 240 pour O3'))
    call check(nf90_put_att(out2fileID, XVarID, 'units',''))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) )      

    call check(nf90_def_var(out2fileID,'nb_dep_140_jour'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','Nombre de jours de depassements seuil 140 pour NO2'))
    call check(nf90_put_att(out2fileID, XVarID, 'units',''))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) )  

    call check(nf90_def_var(out2fileID,'nb_dep_200_jour'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','Nombre de jours de depassements seuil 200 pour NO2'))
    call check(nf90_put_att(out2fileID, XVarID, 'units',''))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) )  
    
    if (ihour) then        
    call check(nf90_def_var(out2fileID,'o3_moy8h'  ,nf90_float,dimids=(/lonDimID,latDimId,frTimeDimID/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','Moyenne glissante O3 sur 8 heures'))
    call check(nf90_put_att(out2fileID, XVarID, 'units','microg/m3'))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) )   
    end if   
    
    if (ijour) then    
    call check(nf90_def_var(out2fileID,'o3_max_1hr_jour'  ,nf90_float,dimids=(/lonDimID,latDimId,jourDimID/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','Maximum jour O3'))
    call check(nf90_put_att(out2fileID, XVarID, 'units','microg/m3'))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) ) 

    call check(nf90_def_var(out2fileID,'no2_max_1hr_jour'  ,nf90_float,dimids=(/lonDimID,latDimId,jourDimID/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','Maximum jour NO2'))
    call check(nf90_put_att(out2fileID, XVarID, 'units','microg/m3'))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) )                  
    end if

    if (ipm) then

    if (ijour) then
      call check(nf90_def_var(out2fileID,'pm10_moy_jour'  ,nf90_float,dimids=(/lonDimID,latDimId,jourDimID/),varID=XVarID))    
      call check(nf90_put_att(out2fileID, XVarID, 'long_name','Moyenne jour'))
      call check(nf90_put_att(out2fileID, XVarID, 'units','microg/m3'))
      call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) ) 
      if (iecart) then   
        call check(nf90_def_var(out2fileID,'pm10_ecart_moy_jour'  ,nf90_float,dimids=(/lonDimID,latDimId,jourDimID/),varID=XVarID))    
        call check(nf90_put_att(out2fileID, XVarID, 'long_name','Moyenne jour'))
        call check(nf90_put_att(out2fileID, XVarID, 'units','microg/m3'))
        call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) )                
      end if
    end if
    
    call check(nf90_def_var(out2fileID,'pm10_moy_an'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','Moyenne annuelle PM10'))
    call check(nf90_put_att(out2fileID, XVarID, 'units','microg/m3'))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) )

    if (iecart) then    
      call check(nf90_def_var(out2fileID,'pm10_ecart_moy_an'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
      call check(nf90_put_att(out2fileID, XVarID, 'long_name','Moyenne annuelle ecart PM10'))
      call check(nf90_put_att(out2fileID, XVarID, 'units','microg/m3'))
      call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) )  
    end if
    
    call check(nf90_def_var(out2fileID,'pm10_moy_mois'  ,nf90_float,dimids=(/lonDimID,latDimId,moisDimID/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','Moyenne mensuelle PM10'))
    call check(nf90_put_att(out2fileID, XVarID, 'units','microg/m3'))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) )              
   
    do is=1,nseuils_moy_jour    
      call check(nf90_def_var(out2fileID, trim(seuils_moy_jour_str(is)),nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
      call check(nf90_put_att(out2fileID, XVarID, 'long_name','Nombre de depassements PM10 '//trim(seuils_moy_jour_str(is))))
      call check(nf90_put_att(out2fileID, XVarID, 'units',''))
      call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) )      
    end do    
    
    end if
    
    if (item2) then

    call check(nf90_def_var(out2fileID,'degres_18_jour'  ,nf90_float,dimids=(/lonDimID,latDimId,jourDimID/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','Degres jour 18 degresC'))
    call check(nf90_put_att(out2fileID, XVarID, 'units','K'))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) )  

    call check(nf90_def_var(out2fileID,'degres_16_jour'  ,nf90_float,dimids=(/lonDimID,latDimId,jourDimID/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','Degres jour 16 degresC'))
    call check(nf90_put_att(out2fileID, XVarID, 'units','K'))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) )  

    call check(nf90_def_var(out2fileID,'degres_18_an'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','Degres jour 18 degresC'))
    call check(nf90_put_att(out2fileID, XVarID, 'units','K'))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) )  

    call check(nf90_def_var(out2fileID,'degres_16_an'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','Degres jour 16 degresC'))
    call check(nf90_put_att(out2fileID, XVarID, 'units','K'))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) )  
    
    end if    

    if (region1km) then
    call check(nf90_def_var(out2fileID,'contour_region'    ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','Contour de la regoin Rhone-Alpes'))
    call check(nf90_put_att(out2fileID, XVarID, 'units','0/1'))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) ) 
           
    call check(nf90_def_var(out2fileID,'dept'    ,nf90_float,dimids=(/lonDimID,latDimId,deptDimId/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','Contour departements francais'))
    call check(nf90_put_att(out2fileID, XVarID, 'units','0/1'))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) )                   

    call check(nf90_def_var(out2fileID,'zone'    ,nf90_float,dimids=(/lonDimID,latDimId,zoneDimId/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','Contour departements francais'))
    call check(nf90_put_att(out2fileID, XVarID, 'units','0/1'))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) )   

    call check(nf90_def_var(out2fileID,'pop99'   ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','Population au km carre'))
    call check(nf90_put_att(out2fileID, XVarID, 'units','0/1'))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) )            
    end if

    ! Global attributes
    call check(nf90_put_att(out2fileID, nf90_global, 'title'  , 'Statistiques annuelles CHIMERE v'//trim(adjustl(version_str))//' - (c) AtmoRA 2010' ))      
    call check(nf90_put_att(out2fileID, nf90_global, 'version', trim(adjustl(version_str))))   

    call check(nf90_close(out2fileID))        
   
   
   end subroutine create_cdf
!-------------------------------------------------------------------------------------
   subroutine ecrit_an_cdf
   
      use netcdf
      use typesizes 
      use params
      
      implicit none   
      
      call check(nf90_open(fout2, nf90_write, out2FileID)) 
      
      write(*,*) 'o3_moy_an'
      call check(nf90_inq_varid(out2fileID, 'o3_moy_an', xVarID))
      call check(nf90_put_var(out2fileID, XVarID, o3_moy_an, start = (/ 1,1/) ))

      write(*,*) 'no2_moy_an'
      call check(nf90_inq_varid(out2fileID, 'no2_moy_an', xVarID))
      call check(nf90_put_var(out2fileID, XVarID, no2_moy_an, start = (/ 1,1/) ))

      write(*,*) 'aot80'
      call check(nf90_inq_varid(out2fileID, 'aot80', xVarID))
      call check(nf90_put_var(out2fileID, XVarID, aot80, start = (/ 1,1/) ))

      write(*,*) 'aot40'
      call check(nf90_inq_varid(out2fileID, 'aot40', xVarID))
      call check(nf90_put_var(out2fileID, XVarID, aot40, start = (/ 1,1/) ))

      write(*,*) 'nb_dep_120_moy8h'
      call check(nf90_inq_varid(out2fileID, 'nb_dep_120_moy8h', xVarID))
      call check(nf90_put_var(out2fileID, XVarID, nb_dep_120_moy8h, start = (/ 1,1/) ))

      write(*,*) 'nb_dep_180_1h'
      call check(nf90_inq_varid(out2fileID, 'nb_dep_180_1h', xVarID))
      call check(nf90_put_var(out2fileID, XVarID, nb_dep_180_1h, start = (/ 1,1/) ))

      write(*,*) 'nb_dep_240_1h'
      call check(nf90_inq_varid(out2fileID, 'nb_dep_240_1h', xVarID))
      call check(nf90_put_var(out2fileID, XVarID, nb_dep_240_1h, start = (/ 1,1/) ))      
      
      write(*,*) 'nb_dep_120_jour'
      call check(nf90_inq_varid(out2fileID, 'nb_dep_120_jour', xVarID))      
      call check(nf90_put_var(out2fileID, XVarID, nb_dep_120_jour, start = (/ 1,1/) ))

      write(*,*) 'nb_dep_180_jour'          
      call check(nf90_inq_varid(out2fileID, 'nb_dep_180_jour', xVarID))                 
      call check(nf90_put_var(out2fileID, XVarID, nb_dep_180_jour, start = (/ 1,1/) ))
           
      write(*,*) 'nb_dep_240_jour'          
      call check(nf90_inq_varid(out2fileID, 'nb_dep_240_jour', xVarID))                 
      call check(nf90_put_var(out2fileID, XVarID, nb_dep_240_jour, start = (/ 1,1/) ))            

      write(*,*) 'nb_dep_140_jour'          
      call check(nf90_inq_varid(out2fileID, 'nb_dep_140_jour', xVarID))                 
      call check(nf90_put_var(out2fileID, XVarID, nb_dep_140_jour, start = (/ 1,1/) ))  
      
      write(*,*) 'nb_dep_200_jour'          
      call check(nf90_inq_varid(out2fileID, 'nb_dep_200_jour', xVarID))                 
      call check(nf90_put_var(out2fileID, XVarID, nb_dep_200_jour, start = (/ 1,1/) ))  

      if (ipm) then
      
        write(*,*) 'pm10_moy_an'
        call check(nf90_inq_varid(out2fileID, 'pm10_moy_an', xVarID))
        call check(nf90_put_var(out2fileID, XVarID, pm10_moy_an, start = (/ 1,1/) ))

        if (iecart) then
         write(*,*) 'pm10_ecart_moy_an'
         call check(nf90_inq_varid(out2fileID, 'pm10_ecart_moy_an', xVarID))
         call check(nf90_put_var(out2fileID, XVarID, pm10_ecart_moy_an, start = (/ 1,1/) ))
        end if      

        do is= 1, nseuils_moy_jour      
          write(*,*) trim(seuils_moy_jour_str(is))
          call check(nf90_inq_varid(out2fileID, trim(seuils_moy_jour_str(is)), xVarID))      
          call check(nf90_put_var(out2fileID, XVarID, nb_dep_x_jour(is,:,:), start = (/ 1,1/) ))
        end do
  
      end if
      
      if (item2) then

      write(*,*) 'degres_18_an'          
      call check(nf90_inq_varid(out2fileID, 'degres_18_an', xVarID))      
      call check(nf90_put_var(out2fileID, XVarID, degres_18_an, start = (/ 1,1/) ))

      write(*,*) 'degres_16_an'          
      call check(nf90_inq_varid(out2fileID, 'degres_16_an', xVarID))      
      call check(nf90_put_var(out2fileID, XVarID, degres_16_an, start = (/ 1,1/) ))
      
      end if      
      
      call check(nf90_close(out2fileID)) 

   end subroutine ecrit_an_cdf      
