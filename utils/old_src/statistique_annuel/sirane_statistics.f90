      program sirane_statistics
!********************************************************************
!*         CARTOPROX                               		    *
!*	   date: octobre 2010                                       *
!*         auteur: E. Chaxel                                        *
!********************************************************************
! 06/10/2010 : reprend le programme en bouclant sur les points recepteur
!              de SIRANE a la place des point de grille 

      use netcdf
      use typesizes 
      use params
      
      implicit none         
                             
!--------------------------------------------------------------  
      write(version_str,'(F10.2)') version  
      write(*,*) '##########################################' 
      write(*,*) '# CARTOPROX - Statistiques SIRANE'
      write(*,*) '# (c)Atmo Rhone-Alpes 2010'
      write(*,*) '# version '//trim(version_str)
      write(*,*) '# contact: echaxel@atmo-rhonealpes.org'                                                                                
      write(*,*) '##########################################'
!--------------------------------------------------------------               
      call getarg(1,fout1)	
      call getarg(2,fout2)
      
      if ( trim(fout2) == '' ) then
        fout2='stat.nc'
      end if      
      
      if ( trim(fout1) == '' ) then
        write(*,*) 'warning : Ne trouve pas d argument'
	write(*,*) 'indiquer : fichier chimere, [fichier de statistiques]'
	stop
      end if
          
!--------------------------------------------------------------------
      write(*,*) 'Lit la date dans '//trim(fout1)
     
! completer avec fill_cfd de sirane2recept    
     
!--------------------------------------------------------------------        
! Ouvre les fichiers de chimie et verifie la presence de PM10
!--------------------------------------------------------------------  
      write(*,*) 'Ouvre les fichiers NetCDF'
    ! grille CHIMERE
! aerosols Y/N   
      call check(nf90_open(fout1, nf90_nowrite, out1FileID))
      ! check le nombre de recepteurs
      call check(nf90_close( out1FileID)) 

!--------------------------------------------------------------------                   
! Recepteurs POINTS
      allocate( lon0(nx1,ny1) )    
      allocate( lat0(nx1,ny1) )       
    
! supprime les id_maille et id_maille_vm

!-------- 
! Allocations pour la suite
!--------------------------------------------------------------------      
! Grille CHIMERE coarse
      write(*,*) 'Allocations...'
      call allocation       
!--------------------------------------------------------------------        
! Read Model outputs for temperature at 2 m, q, u and v and 
!-------------------------------------------------------------------- 

! Creer un NetCDF de sorties
    write(*,*) 'Creer le NetCDF de sortie'    
    call create_cdf !reprendre create_cdf de sirane2recept
    
!--------------------------------------------------------------------        
! Ouvre les fichiers en lecture ou lecture/ecriture
!--------------------------------------------------------------------  
      write(*,*) 'Ouvre les fichiers NetCDF'
    ! grille CHIMERE
      write(*,*) 'Process SIRANE'    
      call check(nf90_open(fout1, nf90_nowrite, out1FileID)) 
       
      call check(nf90_inq_varid(out1FileID, 'lon', xVarID))    
      call check(nf90_get_var(out1FileID, xVarID, lon0,  start = (/1,1/) ))
      
      call check(nf90_inq_varid(out1FileID, 'lat', xVarID))    
      call check(nf90_get_var(out1FileID, xVarID, lat0,  start = (/1,1/) ))             

! Ecrit les coordonnees lon,lat
      call check(nf90_open(fout2, nf90_write, out2FileID)) 

      write(*,*) 'Ecrit longitude lon'
      call check(nf90_inq_varid(out2fileID, 'lon', xVarID))	      
      call check(nf90_put_var(out2fileID, xVarID, lon0, start = (/ 1, 1/) )) 
      
      write(*,*) 'Ecrit latitude lat'      
      call check(nf90_inq_varid(out2fileID, 'lat', xVarID))   
      call check(nf90_put_var(out2fileID, xVarID, lat0, start = (/ 1, 1/) )) 
      
      call check(nf90_close(out2fileID))          
           
!--------------------------------------------------------------------       
!       LOOP OVER HOURS 
!--------------------------------------------------------------------      
      write(*,*) numFrTimes
      
      ntimes=numFrTimes-mod(numFrTimes-1,24)
      
      do it=1,ntimes

      call check(nf90_open(fout2, nf90_write, out2FileID)) 

      !if ( it .eq. 1 ) write(*,*) 'Lit la date'
      ! date             	      
      call check(nf90_inq_varid(out1FileID, 'Times', xVarID))    
      call check(nf90_get_var(out1FileID, xVarID, datestr, start = (/1,it/) ))
      
      if (it.eq.1) then
        mois_1 = 1
      else
        mois_1 = Mnth
      end if
      
      read(datestr,'(I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,I2)')&
     	     Year,Mnth,Day,Hour,Min,Sec
       
      count3d=(/nx1,ny1,  1     /)     
      start3d=(/  1,  1, it     /)
      start4d=(/  1,  1,  1, it /)     
      
      ! out.*.nc
      if (it .eq. 1) write(*,*) 'Lit les variables chimie'      

      o3_var='O3'
      no_var='NO'      
      no2_var='NO2'      
      pm10_var='PM10'      

      ! PM10_nonvolat      
      if ( nf90_inq_varid(out1fileID, 'PM10_nonvolat', xVarID) .eq. nf90_noerr ) pm10_var ='PM10_nonvolat'  

      ! O3 KRIG
      call check(nf90_inq_varid(out1fileID, o3_var, xVarID))
      call check(nf90_inquire_variable(out1fileID,  xVarID, ndims = dimensions ))     
      call check(nf90_get_var(out1fileID, xVarID, o3_0,  start=start4d ))      
      
      ! NO2 KRIG
      call check(nf90_inq_varid(out1fileID, no2_var, xVarID))
      call check(nf90_inquire_variable(out1fileID,  xVarID, ndims = dimensions ))  
      call check(nf90_get_var(out1fileID, xVarID, no2_0,  start=start3d ))
       	  
      ! calcule le PM10 corrige = TEOM + ecart de correction FDMS
      pm10_0 = pm10k_0 + pm10_ecart_0

      do i1=1,nx1
        do j1=1,ny1

          ! conversion ppb -> microg/m3
          o3_0(i1,j1,1)  = o3_0(i1,j1,1)  * 2.00
          no2_0(i1,j1,1) = no2_0(i1,j1,1) * 1.91	  
	  
	  ! contrainte sur heure 8 h à 20 h heure HL hiver -> 7 h à 19 h TU du 1er mai au 31 juillet
	  ! polair extrait de 07 TU (06 TU BDQA)
	  if ( iaot_veg ) then
	  !!!!!!!!!!!!!!!!!!!!!!!!!!
      !---------------------------------------------------------------       
      ! calcul de l'AOT 40 VEGETATION
      !--------------------------------------------------------------- 
           if (o3_0(i1,j1,1).gt.80.) then
	     aot40(i1,j1) = aot40(i1,j1) + ( o3_0(i1,j1,1) - 80. )	 
	   end if 
	!!!!!!!!!!!!!!!!!!!!!!! 
	 end if	 
	 if ( iaot_foret ) then
      !---------------------------------------------------------------       
      ! calcul de l'AOT 40 FORET
      !--------------------------------------------------------------- 
           if (o3_0(i1,j1,1).gt.80.) then
	     aot80(i1,j1) = aot80(i1,j1) + ( o3_0(i1,j1,1) - 80. )  
	   end if 
	!!!!!!!!!!!!!!!!!!!!!!! 
	 end if	 
      !---------------------------------------------------------------       
      ! calcul des depâssements jour pour PM10/O3/NO2
      ! pour maximiser les depassements, on utilise un arrondi
      !--------------------------------------------------------------- 
         !if ( (i1*j1.eq.1).and.( mod(it-1,24) .eq. 0) ) write(*,*) 'Trouve date : '//datestr

         if ( (it.gt.1).and.( mod(it-1,24) .eq. 0) ) then 
	  	   	   
	   ! --- PM10: jour de depassement du x ug/m3
	   if (ipm) then	   
	   do is= 1, nseuils_moy_jour
	   if ( nint(pm10_moy_jour(i1,j1)) .ge. seuils_moy_jour(is) ) &
	     nb_dep_x_jour(is,i1,j1) =  nb_dep_x_jour(is,i1,j1) + 1	
	   end do  
	   end if 
	        
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
	   if ( nint(no2_max_1hr_jour(i1,j1)) .ge. 140 ) &
	     nb_dep_140_jour(i1,j1) =  nb_dep_140_jour(i1,j1) + 1
	     
	   ! --- NO2: jour de depassement du 200 ug/m3 
	   if ( nint(no2_max_1hr_jour(i1,j1)) .ge. 200 ) &
	     nb_dep_200_jour(i1,j1) =  nb_dep_200_jour(i1,j1) + 1
	      
	 end if 
	 	  	   
         if (pm10_0(i1,j1,1).ge.0) then	 

           ! moyennes jour
	   pm10_moy_jour(i1,j1)      = pm10_moy_jour(i1,j1)       + pm10_0(i1,j1,1)       / 24.
	   pm10_ecart_moy_jour(i1,j1)= pm10_ecart_moy_jour(i1,j1) + pm10_ecart_0(i1,j1,1) / 24.	   	   

           ! moyennes an/mois
 	   pm10_moy_an(i1,j1)        = pm10_moy_an(i1,j1)         + pm10_0(i1,j1,1)	   
 	   pm10_moy_mois(i1,j1)      = pm10_moy_mois(i1,j1)       + pm10_0(i1,j1,1)	   	    	   
	   pm10_ecart_moy_an(i1,j1)  = pm10_ecart_moy_an(i1,j1)   + pm10_ecart_0(i1,j1,1) 	   	
	   
           ! compteurs
	   n_pm10_moy_an(i1,j1)  = n_pm10_moy_an(i1,j1) + 1
	   n_pm10_moy_mois(i1,j1)= n_pm10_moy_mois(i1,j1) + 1	   
	   	   
	 end if
	 
      !---------------------------------------------------------------       
      ! calcul des moyenne annuelles
      !---------------------------------------------------------------

         if (o3_0(i1,j1,1).ge.0) then
           ! moyennes
 	   o3_moy_an(i1,j1)   = o3_moy_an(i1,j1)   + o3_0(i1,j1,1)
 	   o3_moy_mois(i1,j1) = o3_moy_mois(i1,j1)   + o3_0(i1,j1,1)
           ! compteurs
	   n_o3_moy_an(i1,j1)  = n_o3_moy_an(i1,j1) + 1.
	   n_o3_moy_mois(i1,j1)= n_o3_moy_mois(i1,j1) + 1.	   
	 end if   
	 
	    	 
         if (no2_0(i1,j1,1).ge.0) then
           ! moyennes	   
           no2_moy_an(i1,j1)   = no2_moy_an(i1,j1)   + no2_0(i1,j1,1)
           no2_moy_mois(i1,j1) = no2_moy_mois(i1,j1) + no2_0(i1,j1,1)	   
           ! compteurs	   	   
	   n_no2_moy_an(i1,j1)  = n_no2_moy_an(i1,j1) + 1.
	   n_no2_moy_mois(i1,j1)= n_no2_moy_mois(i1,j1) + 1.	   
	 end if    
	 
      !---------------------------------------------------------------       
      ! calcul de la moyenne glissante 120 
      !--------------------------------------------------------------- 
       if (it.ge.8) then
         do ix=1,7
	   o3_8h(i1,j1,ix) = o3_8h(i1,j1,ix+1) 
	 end do
	 o3_8h(i1,j1,8) = o3_0(i1,j1,1)
       
         moy8h(i1,j1) = 0.
       
         do ix=1,8
	   moy8h(i1,j1) = moy8h(i1,j1) + o3_8h(i1,j1,ix) / 8. 
	 end do	 
	 
	 if (moy8h(i1,j1).lt.0.) then
	   moy8h(i1,j1) = -9999.
	 end if
	 
         if (moy8h(i1,j1).ge.120.) then
	   nb_dep_120_moy8h(i1,j1) = nb_dep_120_moy8h(i1,j1) + 1.
	 end if	 
	 

      !---------------------------------------------------------------       
      ! calcul des max 8hr O3 
      !--------------------------------------------------------------- 
       if (moy8h(i1,j1).ge.o3_max_8hr_jour(i1,j1) ) &	 
	   o3_max_8hr_jour(i1,j1) = moy8h(i1,j1)

       end if
       
      !---------------------------------------------------------------       
      ! calcul des max 1hr/8hr O3 NO2 T2 jour
      !--------------------------------------------------------------- 
	 
       if (o3_0(i1,j1,1).ge.o3_max_1hr_jour(i1,j1) ) &	 
	   o3_max_1hr_jour(i1,j1) = o3_0(i1,j1,1)
	   	   	 	   
       if (no2_0(i1,j1,1).ge.no2_max_1hr_jour(i1,j1) ) &	 
	   no2_max_1hr_jour(i1,j1) = no2_0(i1,j1,1)

       if (tem2_0(i1,j1).ge.tem2_max_1hr_jour(i1,j1) ) &	 
	   tem2_max_1hr_jour(i1,j1) = tem2_0(i1,j1)
	   
       if (tem2_0(i1,j1).le.tem2_min_1hr_jour(i1,j1) ) &	 
	   tem2_min_1hr_jour(i1,j1) = tem2_0(i1,j1)
	   	          
      !---------------------------------------------------------------       
      ! calcul du nombre de depassements du 180
      !--------------------------------------------------------------- 
         if (o3_0(i1,j1,1).ge.180.) then
	   nb_dep_180_1h(i1,j1) = nb_dep_180_1h(i1,j1) + 1.
	 end if
	 
         if (o3_0(i1,j1,1).ge.240.) then
	   nb_dep_240_1h(i1,j1) = nb_dep_240_1h(i1,j1) + 1.	   	   	   
	 end if	 
	 
      !---------------------------------------------------------------       
      ! calcul des degres jour
      !--------------------------------------------------------------- 
       if ( (it.gt.1).and.( mod(it-1,24) .eq. 0) ) then 
	  
	  dju_16 = (273.15 + 16) - ( tem2_max_1hr_jour(i1,j1) + tem2_min_1hr_jour(i1,j1) ) / 2.
	  dju_18 = (273.15 + 18) - ( tem2_max_1hr_jour(i1,j1) + tem2_min_1hr_jour(i1,j1) ) / 2.
	  
	  if ( dju_18.ge.0. ) then 
	   degres_18_jour(i1,j1) = degres_18_jour(i1,j1) + dju_18
	   degres_18_an(i1,j1)   = degres_18_an(i1,j1)   + dju_18
	  end if
          if ( dju_16.ge.0. ) then 
	   degres_16_jour(i1,j1) = degres_16_jour(i1,j1) + dju_16
	   degres_16_an(i1,j1)   = degres_16_an(i1,j1)   + dju_16
	  end if
	  
	  tem2_max_1hr_jour(i1,j1) = -9999.
          tem2_min_1hr_jour(i1,j1) =  9999.

      end if
      !----------------------------------------------------------------

        end do
      end do

      if ((it.ge.8).and.(ihour)) then
        call check(nf90_inq_varid(out2fileID, 'o3_moy8h', xVarID))      
        call check(nf90_put_var(out2fileID, XVarID, moy8h, start = (/ 1,1,it/) ))
      end if

      !---------------------------------------------------------------       
      ! calcul des donnees mensuelles
      !---------------------------------------------------------------
      if ( Mnth .ne. mois_1 ) then
         
	 if ( mois_1.ne.0) then
 	 ! calcul des moyennes mensuelles  
	 o3_moy_mois     = o3_moy_mois  / n_o3_moy_mois
 	 no2_moy_mois    = no2_moy_mois / n_no2_moy_mois
 	 pm10_moy_mois   = pm10_moy_mois/ n_pm10_moy_mois

         write(*,*) '-> Ecrit moyennes mensuelles pour le mois',mois_1	 
	 call check(nf90_inq_varid(out2fileID, 'o3_moy_mois', xVarID))      
         call check(nf90_put_var(out2fileID, XVarID,  o3_moy_mois, start = (/ 1,1,mois_1/) ))
	 call check(nf90_inq_varid(out2fileID, 'no2_moy_mois', xVarID))      
         call check(nf90_put_var(out2fileID, XVarID, no2_moy_mois, start = (/ 1,1,mois_1/) ))   
	 if (ipm) then
	   call check(nf90_inq_varid(out2fileID, 'pm10_moy_mois', xVarID))	
           call check(nf90_put_var(out2fileID, XVarID, pm10_moy_mois, start = (/ 1,1,mois_1/) ))
         end if	  
	 
	 ! remet les moyennes et compteurs à 0
	 o3_moy_mois     = 0.
         no2_moy_mois    = 0.
         pm10_moy_mois   = 0.
	 n_o3_moy_mois   = 0.
         n_no2_moy_mois  = 0.
         n_pm10_moy_mois = 0.	
	 else
	   write(*,*) 'ERREUR dans le traitement des mois au mois_1=',mois_1
	 end if 
	 	 
      end if	

      !---------------------------------------------------------------       
      ! ecritures des donnees jour
      !---------------------------------------------------------------                  
      if ( (it.ge.24) .and. (mod(it-1,24).eq.0).and.(ijour) ) then

          it1 = it/24
	  
	  if (ipm) then
	  
	    call check(nf90_inq_varid(out2fileID, 'pm10_moy_jour', xVarID))      
            call check(nf90_put_var(out2fileID, XVarID, pm10_moy_jour, start = (/ 1,1,it1/) ))
	    
	    if (iecart) then
	      call check(nf90_inq_varid(out2fileID, 'pm10_ecart_moy_jour', xVarID))
              call check(nf90_put_var(out2fileID, XVarID, pm10_ecart_moy_jour, start = (/ 1,1,it1/) ))
            end if
	  end if
	  
	  call check(nf90_inq_varid(out2fileID, 'o3_max_1hr_jour', xVarID))      
          call check(nf90_put_var(out2fileID, XVarID, o3_max_1hr_jour, start = (/ 1,1,it1/) ))
	  
	  call check(nf90_inq_varid(out2fileID, 'no2_max_1hr_jour', xVarID))      
          call check(nf90_put_var(out2fileID, XVarID, no2_max_1hr_jour, start = (/ 1,1,it1/) ))	  	
	    	  
	  if (item2) then
	  call check(nf90_inq_varid(out2fileID, 'degres_16_jour', xVarID))      
          call check(nf90_put_var(out2fileID, XVarID, degres_16_jour, start = (/ 1,1,it1/) ))	  
	  
	  call check(nf90_inq_varid(out2fileID, 'degres_18_jour', xVarID))      
          call check(nf90_put_var(out2fileID, XVarID, degres_18_jour, start = (/ 1,1,it1/) ))	
	  end if  	  	  

          if ( it .eq. 1 ) write(*,*) '... Time' 
          call check(nf90_inq_varid(out2fileID, 'Time', xVarID))
          call check(nf90_put_var(out2fileID, XVarID, it1-1, start = (/it1/) )) 
	
      end if 
            
      if ( mod(it-1,24).eq.0 )   then
      
	 degres_16_jour = 0.
	 degres_18_jour = 0.	  

        ! remet les valeurs moyenne/max à zero pour une nouvelle journée
	 pm10_moy_jour       = 0.
	 pm10_ecart_moy_jour = 0.	 
         o3_max_1hr_jour = 0.
	 o3_max_8hr_jour = 0.
	 no2_max_1hr_jour = 0.	 	
      end if	     
                                      
      call check(nf90_close(out2FileID)) 

      end do ! ntimes
      
      !---------------------------------------------------------------       
      ! calcul des moyenne annuelles
      !---------------------------------------------------------------
	do i1=1,nx1
	do j1=1,ny1
         o3_moy_an(i1,j1)          = o3_moy_an(i1,j1)        / n_o3_moy_an(i1,j1)
 	 no2_moy_an(i1,j1)        = no2_moy_an(i1,j1)        / n_no2_moy_an(i1,j1)
 	 pm10_moy_an(i1,j1)       = pm10_moy_an(i1,j1)       / n_pm10_moy_an(i1,j1) 
 	 pm10_ecart_moy_an(i1,j1) = pm10_ecart_moy_an(i1,j1) / n_pm10_moy_an(i1,j1) 	 
        end do
	end do      
      !---------------------------------------------------------------       
      ! ecrit dans un NetCDF pour le pas de temps it
      !---------------------------------------------------------------  
       call ecrit_an_cdf
      !---------------------------------------------------------------
      ! Ferme les NetCDF
      !---------------------------------------------------------------
      ! CHIMERE       
      call check(nf90_close(out1fileID))
!      call check(nf90_close(met1fileID))          
      ! grille fine                  
!      call check(nf90_close(out2fileID))       
      
99    continue      

   
      end program
   
