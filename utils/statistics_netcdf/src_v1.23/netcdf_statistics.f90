      program netcdf_statistics
!****************************************************
!*     PREVALP/CARTOPROX                            *
!*     Calcul des statistiques CHIMERE/CARTPROX     *
!*     date : decembre 2010                         *
!*          auteur: E. Chaxel                       *
!*		       LEGI/GIERSA	            *
!*			chaxeleric@yahoo.fr         *
!****************************************************
!chimere_staistics
! Lit les fichiers horaires ou jours out.*.nc, assim.*.nc et sirane.*.nc
! calcule les statistiques
! Pour ozone
! AOT 80
! moyenne glissante 8 heures
! 06/12/2010 : [version 1.0] version identique a chimere_statistics version 1.11

      use netcdf
      use typesizes 
      use params
      
      implicit none         
                             
!--------------------------------------------------------------  
      write(version_str,'(F10.2)') version  
      write(*,*) '******************************************' 
      write(*,*) '* PREVALP - STATISTIQUES CHIMERE        '
      write(*,*) '* (c)Atmo Rhone-Alpes 2011      '
      write(*,*) '* version '//trim(version_str)
      write(*,*) '* contact: echaxel@atmo-rhonealpes.org'                                                                                
      write(*,*) '******************************************'
!--------------------------------------------------------------               
      call getarg(1,fout1)	
      call getarg(2,fstat)
      call getarg(3,fic_correction_pm10)      
      
      if ( trim(fstat) == '' ) then
        fstat='stat.nc'
      end if 
                      
      if ( trim(fout1) == '' ) then
        write(*,*) 'warning : Ne trouve pas d argument'
	write(*,*) 'indiquer : fichier chimere, [fichier de statistiques]'
	stop
      end if
      
      correction_pm10 = 0.
      if ( trim(fic_correction_pm10) .ne. '' ) then
        write(*,*) 'info : lit '//trim(fic_correction_pm10)
	open(unit=10,file=fic_correction_pm10,status='old')
	do nd=1, 366
          read(10,*,end=9)correction_pm10(nd) 
        end do
9       continue        
	close(10)
      end if      

!--------------------------------------------------------------------
      write(*,*) 'Determine le type de fichier '//trim(fout1)      
      call check( nf90_open(fout1, nf90_nowrite, out1FileID) )      
      if ( nf90_inq_dimid(out1FileID,'Point', pointDimID).eq.nf90_noerr ) then
        write(*,*) '-> Point'
	igrille=.false.
	ipoint=.true.	
      else
        write(*,*) '-> Grille'
        igrille=.true.
	ipoint=.false.
      end if   
      call check(nf90_close(out1FileID))  
      
!--------------------------------------------------------------------
! Pour simplifier les calculs et utiliser des tableaux identiques, si on a un point
!  numLons=numPoints
!  numLats=1                      
!--------------------------------------------------------------------      
      if(igrille)call get_dim_ncout_grille(fout1,nx1,ny1,nz1,numFrTimes,distGrid,Year,Mnth,Day,Hour,Min,Sec)
      if(.not.igrille) then
        call get_dim_ncout_point (fout1,nx1,numFrTimes,Year,Mnth,Day,Hour,Min,Sec)
	ny1 = 1
	nz1 = 1
	distGrid = 0.
      end if
		      
      TimeStringLen=60.    
          
!--------------------------------------------------------------------
      write(*,*) 'Lit la date dans '//trim(fout1)    		      
      TimeStringLen=60.           
      
!--------------------------------------------------------------------        
! Ouvre les fichiers de chimie et verifie la presence de PM10
!--------------------------------------------------------------------  
      write(*,*) 'Ouvre les fichiers NetCDF'
      call check(nf90_open(fout1, nf90_nowrite, out1FileID))       
    ! grille CHIMERE
! aerosols Y/N
      write(*,*) 'Verifie la presence de PM10'    
      if (nf90_inq_varid(out1fileID, 'PM10', xVarID).eq.nf90_noerr ) then
        write(*,*) '> AEROSOLS TOTAUX DISPONIBLES'
	ipm=.true.
      else if (nf90_inq_varid(out1fileID, 'PM10_nonvolat', xVarID).eq.nf90_noerr ) then
        write(*,*) '> AEROSOLS NON VOLATILES DISPONIBLES'
	ipm=.true.      
      else
        write(*,*) '> AEROSOLS PM10 INDISPONIBLE'      
        ipm=.false.      
      end if
      
! aerosols PM2.5
      write(*,*) 'Verifie la presence de PM25'    
      if (nf90_inq_varid(out1fileID, 'PM25', xVarID).eq.nf90_noerr ) then
        write(*,*) '> AEROSOLS PM2.5 DISPONIBLES'
	ipm25=.true.     
      else      
        write(*,*) '> AEROSOLS PM25 INDISPONIBLES'
        ipm25=.false. 	   
      end if            

! gaz Y/N
      write(*,*) 'Verifie la presence de O3/NO2'    
      if ( (nf90_inq_varid(out1fileID, 'O3' , xVarID).eq.nf90_noerr).and. &
           (nf90_inq_varid(out1fileID, 'NO2', xVarID).eq.nf90_noerr)) then
        write(*,*) '> GAZ O3/NO2 DISPONIBLES'
	igaz=.true.
      else
        write(*,*) '> GAZ O3/NO2 INDISPONIBLES'      
        igaz=.false.      
      end if 
      
! NO Y/N      
      write(*,*) 'Verifie la presence de NO'    
      if  (nf90_inq_varid(out1fileID, 'NO' , xVarID).eq.nf90_noerr) then
        write(*,*) '> GAZ NO DISPONIBLE'
	ino=.true.
      else
        write(*,*) '> GAZ NO INDISPONIBLE'      
        ino=.false.      
      end if       
               
! temperature Y/N
      write(*,*) 'Verifie la presence de tem2'    
      call check(nf90_open(fout1, nf90_nowrite, out1FileID)) 
      if (nf90_inq_varid(out1fileID, 'tem2', xVarID).eq.nf90_noerr ) then
        write(*,*) '> TEMPERATURE 2M DISPONIBLE'
	item2=.true.
      else
        write(*,*) '> TEMPERATURE 2M INDISPONIBLE'      
        item2=.false.      
      end if
            
! nitrates Y/N
      write(*,*) 'Verifie la presence de pHNO3'    
      call check(nf90_open(fout1, nf90_nowrite, out1FileID)) 
      if (nf90_inq_varid(out1fileID, 'pHNO3', xVarID).eq.nf90_noerr ) then
        write(*,*) '> pHNO3 DISPONIBLE'
	iphno3=.true.
	iecart=.true.
      else
        write(*,*) '> pHNO3 INDISPONIBLE'      
        iphno3=.false.      
      end if    
        
      call check(nf90_close( out1FileID)) 

 ! Sort les resultats sur une grille regionale
      write(*,*) 'Dimensions', nx1, ny1, nz1

!---------------------------------
! Mailles region
!---------------------------------
      if (igrille)call include_grille_region
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
      write(*,*) 'Creer le NetCDF de sortie ? -> seulement si n existe pas'
      icreate=.true.
      if(nf90_open(fstat, nf90_nowrite, statFileID).eq.nf90_noerr)icreate=.false.
      if(.not.icreate)call check(nf90_close(statFileID))
      write(*,*) icreate
            
      if(igrille)call create_ncstat_grille             ! GRILLE -> le créer dans tous les cas
      if(ipoint.and.icreate)call create_ncstat_point   ! POINT  -> le créer s'il n'existe pas
!--------------------------------------------------------------------        
! Ouvre les fichiers en lecture ou lecture/ecriture
!--------------------------------------------------------------------  
    ! grille CHIMERE
      write(*,*) 'Lit les coordonnees'    
! Ecrit les coordonnees lon,lat
      if(igrille)call get_ncout_grille_coord
      if(ipoint)call get_ncout_point_coord            

      write(*,*) 'Ecrit les coordonnees lon,lat ou x_pts, y_pts'    
! Ecrit les coordonnees lon,lat
      if(igrille)call write_ncstat_grille_coord
      if(ipoint)call write_ncstat_point_coord     
           
!--------------------------------------------------------------------       
!       LOOP OVER HOURS 
! version 1.1 : se base sur la date
!--------------------------------------------------------------------            
      ntimes = numFrTimes      
      write(*,*) 'Nombre de pas de temps pour une année=',numFrTimes            
      nheures_par_jour_tmp = 0
      it_jour=0
      
      do it=1,ntimes !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

      !if ( it .eq. 1 ) write(*,*) 'Lit la date'
      ! date                        	      
      call lit_date_cdf
      
      !write(*,*) 'Date '//datestr
      
      if (it.ne.1) then
        mois_en_cours = Mnth
	jour_en_cours = Day
	it_en_cours=it-1
      end if
      
      read(datestr,'(I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,I2)')&
     	     Year,Mnth,Day,Hour,Min,Sec
      !write(*,*) datestr
	     
      if (it.eq.1) then
        mois_en_cours = Mnth
	jour_en_cours = Day
	it_en_cours=0
      end if

      ! a-t-on change de mois ?      
      if (mois_en_cours.eq.Mnth) then
        imois_new = .false.
      else
        imois_new = .true.
      end if
      
      ! a-t-on change de jour ?       
      if (jour_en_cours.eq.Day) then
        ijour_new = .false.
      else
        ijour_new = .true.
      end if      
		    
      !---------------------------------------------------------------       
      ! ecritures des donnees horaires (moyennes ozone 8h)
      !---------------------------------------------------------------
      !if (ihour.and.(it.ne.it_en_cours)) then
      !  call check(nf90_inq_varid(statfileID, 'o3_moy8h', xVarID))      
      !  call check(nf90_put_var(statfileID, XVarID, moy8h, start = (/ 1,1,it_en_cours/) ))
      !end if

      !---------------------------------------------------------------       
      ! ecritures des donnees jour
      !---------------------------------------------------------------                        
      if ( ijour_new .and. (nheures_par_jour.eq.0) ) then 
        nheures_par_jour = nheures_par_jour_tmp
	write(*,*) 'Pas de temps dans entree par jour:',nheures_par_jour
      end if
      !remet a 0 en cas de nouvelle journée
      if ( ijour_new ) then
        nheures_par_jour_tmp = 0
      end if      	
      
      if ( ijour_new ) then
        !write(*,*) 'ecrit jour...'
        if(igrille)call ecrit_jour_ncstat_grille
        if(ipoint )call ecrit_jour_ncstat_point
      end if
           
      !---------------------------------------------------------------       
      ! Ecritures des donnees mensuelles
      !---------------------------------------------------------------
      if ( imois_new ) then
        !write(*,*) 'ecrit mois...'
        if(igrille)call ecrit_mois_ncstat_grille
        if(ipoint )call ecrit_mois_ncstat_point	
      end if  
      
      !---------------------------------------------------------------       
      ! remet les valeurs moyenne/max à zero pour une nouvelle journée
      !---------------------------------------------------------------          
      if ( ijour_new .or. (it.eq.1) ) then 	  
	tem2_max_1hr_jour = -9999.
        tem2_min_1hr_jour =  9999.	  
      end if
      
      if ( ijour_new .or. (it.eq.1) ) then
	 degres_16_jour = 0.
	 degres_18_jour = 0.		
	 pm10_moy_jour = 0.
	 pm25_moy_jour = 0.
	 n_pm10_moy_jour = 0.
	 n_pm25_moy_jour = 0.
	 pm10_ecart_moy_jour = 0.	 
         o3_max_1hr_jour  = 0.
	 o3_max_8hr_jour  = 0.
	 nox_max_1hr_jour = 0.
      end if
	      
      if ( imois_new .or. (it.eq.1) ) then 
	 o3_moy_mois     = 0.
         nox_moy_mois    = 0.
         pm10_moy_mois   = 0.
	 n_o3_moy_mois   = 0.
         n_nox_moy_mois  = 0.
         n_pm10_moy_mois = 0.	
      end if 

      !affichage nombre d'heures par jour
      if(ijour_new.and.(it_jour.eq.1))write(*,*) 'Heures par jour=',nheures_par_jour
      	 	 
      !---------------------------------------------------------------       
      ! Compte le nombre d'heure par jour
      !---------------------------------------------------------------         
      if ( it.eq.1 ) nheures_par_jour_tmp = 0
      nheures_par_jour_tmp = nheures_par_jour_tmp + 1

      !---------------------------------------------------------------       
      ! Lecture du fichier NetCDF (io_netcdf)
      !---------------------------------------------------------------         
      if(igrille)call lit_step_ncstat_grille
      if(ipoint)call lit_step_ncstat_point      

      !---------------------------------------------------------------       
      ! Affichage
      !---------------------------------------------------------------             
       ! DEFINITION DES AOT FORET ET VEGETATION
      if ( (Hour.ge.6) .and. (Hour.le.17).and.(Mnth.ge.4).and.(Mnth.le.9) ) then
        iaot_foret=.true.
      else
        iaot_foret=.false.        	
      end if
      
      if ( (Hour.ge.6) .and. (Hour.le.17).and.(Mnth.ge.5).and.(Mnth.le.7) ) then      
        iaot_veg=.true.
      else
        iaot_veg=.false.        	
      end if

      if ( it .eq. 1 ) then
        if (ipm.or.ipm25) write(*,'(A10,2A10,4A10)') 'Date' ,'AOT_foret','AOT_veget','O3 max','NO2 max','PM10 max','PM2.5 max'
	if ((.not.ipm).and.(.not.ipm25)) write(*,'(A10,2A10,2A10)') 'Date' ,'AOT_foret','AOT_veget','O3 max','NO2 max'
      end if

      !--------------------------------------------------------------- 
      !Fixe la correction PM10
      !---------------------------------------------------------------       
      correction_pm10_jour = correction_pm10(it_jour+1)   

      if (  ijour_new.or.(it.eq.1) ) then
        if (iaot_foret.and.(.not.iaot_veg)) then
           if (ipm.or.ipm25)               write(*,'(A13,2(5X,A1,4X),4F10.2)') datestr(1:13),'X',' ',maxval(o3_0(:,:)),maxval(nox_0(1,:,:)),maxval(pm10_0(:,:)),maxval(pm25_0(:,:))
	   if ((.not.ipm).and.(.not.ipm25))write(*,'(A13,2(5X,A1,4X),2F10.2)') datestr(1:13),'X',' ',maxval(o3_0(:,:)),maxval(nox_0(1,:,:))
	else if (iaot_foret.and.iaot_veg) then
           if (ipm.or.ipm25)               write(*,'(A13,2(5X,A1,4X),4F10.2)') datestr(1:13),'X','X',maxval(o3_0(:,:)),maxval(nox_0(1,:,:)),maxval(pm10_0(:,:)),maxval(pm25_0(:,:))
	   if ((.not.ipm).and.(.not.ipm25))write(*,'(A13,2(5X,A1,4X),2F10.2)') datestr(1:13),'X','X',maxval(o3_0(:,:)),maxval(nox_0(1,:,:))
        else 	
	   if (ipm.or.ipm25)               write(*,'(A13,2(5X,A1,4X),4F10.2)') datestr(1:13),' ',' ',maxval(o3_0(:,:)),maxval(nox_0(1,:,:)),maxval(pm10_0(:,:)),maxval(pm25_0(:,:))
	   if ((.not.ipm).and.(.not.ipm25))write(*,'(A13,2(5X,A1,4X),2F10.2)') datestr(1:13),' ',' ',maxval(o3_0(:,:)),maxval(nox_0(1,:,:))
        end if
      end if               

      do i1=1,nx1
        do j1=1,ny1
      !---------------------------------------------------------------       
      ! conversion ppb -> microg/m3 (uniquement sur une grille CHIMERE)
      !--------------------------------------------------------------- 
          if (igrille) then
	    o3_0(   i1,j1) = o3_0(   i1,j1) * 2.00
            nox_0(1,i1,j1) = nox_0(1,i1,j1) * 1.91
            nox_0(2,i1,j1) = nox_0(2,i1,j1) * 1.25	    
	  end if	  
	  
	  ! contrainte sur heure 8 h à 20 h heure HL hiver -> 7 h à 19 h TU du 1er mai au 31 juillet
	  ! polair extrait de 07 TU (06 TU BDQA)
	  if ( iaot_veg ) then
	  !!!!!!!!!!!!!!!!!!!!!!!!!!
      !---------------------------------------------------------------       
      ! calcul de l'AOT 40 VEGETATION
      !--------------------------------------------------------------- 
           if (o3_0(i1,j1).gt.80.) then
	     aot40(i1,j1) = aot40(i1,j1) + ( o3_0(i1,j1) - 80. )	 
	   end if 
	!!!!!!!!!!!!!!!!!!!!!!! 
	 end if	 
	 if ( iaot_foret ) then
      !---------------------------------------------------------------       
      ! calcul de l'AOT 40 FORET
      !--------------------------------------------------------------- 
           if (o3_0(i1,j1).gt.80.) then
	     aot80(i1,j1) = aot80(i1,j1) + ( o3_0(i1,j1) - 80. )  
	   end if 
	!!!!!!!!!!!!!!!!!!!!!!! 
	 end if	 
      !---------------------------------------------------------------       
      ! calcul des depâssements jour pour PM10/O3/NO2
      ! pour maximiser les depassements, on utilise un arrondi
      !--------------------------------------------------------------- 
         !if ( (i1*j1.eq.1).and.( mod(it-1,24) .eq. 0) ) write(*,*) 'Trouve date : '//datestr	 	  	   	 
	 if (ipm) then
           
	   !correction pm10 (SCENARIO)
	   !if (i1*j1.eq.1)write(*,*) 'Applique la correcion PM10 :',correction_pm10_jour
	   pm10_0(i1,j1) = pm10_0(i1,j1)  + correction_pm10_jour
	   
	   if (pm10_0(i1,j1).lt.0.)pm10_0(i1,j1)=0.
	   
	   ! moyennes jour
	   pm10_moy_jour(i1,j1)      = pm10_moy_jour(i1,j1)       + pm10_0(i1,j1)
	   pm10_ecart_moy_jour(i1,j1)= pm10_ecart_moy_jour(i1,j1) + pm10_ecart_0(i1,j1)
           ! moyennes an/mois
 	   pm10_moy_an(i1,j1)        = pm10_moy_an(i1,j1)         + pm10_0(i1,j1) 
 	   pm10_moy_mois(i1,j1)      = pm10_moy_mois(i1,j1)       + pm10_0(i1,j1)	   	    	   
	   pm10_ecart_moy_an(i1,j1)  = pm10_ecart_moy_an(i1,j1)   + pm10_ecart_0(i1,j1) 	   		   
           ! compteurs
	   n_pm10_moy_an(i1,j1)      = n_pm10_moy_an(i1,j1)   + 1
	   n_pm10_moy_mois(i1,j1)    = n_pm10_moy_mois(i1,j1) + 1
	   n_pm10_moy_jour(i1,j1)    = n_pm10_moy_jour(i1,j1) + 1	   	   	   	   	   
	 end if
	 
	 if (ipm25) then
           
	   if (pm25_0(i1,j1).lt.0.)pm25_0(i1,j1)=0.	   
	   ! moyennes jour
	   pm25_moy_jour(i1,j1)      = pm25_moy_jour(i1,j1)       + pm25_0(i1,j1)
           ! moyennes an/mois
	   pm25_moy_an(i1,j1)        = pm25_moy_an(i1,j1)         + pm25_0(i1,j1)   	   		   
           ! compteurs
	   n_pm25_moy_jour(i1,j1)    = n_pm25_moy_jour(i1,j1)     + 1	   	   	
	   n_pm25_moy_an(i1,j1)      = n_pm25_moy_an(i1,j1)       + 1	
	         	   	   
	 end if	 
	 
      !---------------------------------------------------------------       
      ! calcul des moyenne annuelles
      !---------------------------------------------------------------
         if (o3_0(i1,j1).ge.0) then
           ! moyennes
 	   o3_moy_an(i1,j1)   = o3_moy_an(i1,j1)   + o3_0(i1,j1)
 	   o3_moy_mois(i1,j1) = o3_moy_mois(i1,j1)   + o3_0(i1,j1)
           ! compteurs
	   n_o3_moy_an(i1,j1)  = n_o3_moy_an(i1,j1) + 1.
	   n_o3_moy_mois(i1,j1)= n_o3_moy_mois(i1,j1) + 1.	   
	 end if   
	 	    	 
         do is = 1, 2 
	   if (nox_0(is,i1,j1).ge.0) then
             ! moyennes	   
             nox_moy_an(is,i1,j1)   = nox_moy_an(is,i1,j1)   + nox_0(is,i1,j1)
             nox_moy_mois(is,i1,j1) = nox_moy_mois(is,i1,j1) + nox_0(is,i1,j1)	   
             ! compteurs	   	   
	     n_nox_moy_an(is,i1,j1)  = n_nox_moy_an(is,i1,j1) + 1.
	     n_nox_moy_mois(is,i1,j1)= n_nox_moy_mois(is,i1,j1) + 1.	   
	   end if
	 end do
	 	 
      !---------------------------------------------------------------       
      ! calcul de la moyenne glissante 120 
      !--------------------------------------------------------------- 
       if (it.ge.8) then
         do ix=1,7
	   o3_8h(i1,j1,ix) = o3_8h(i1,j1,ix+1) 
	 end do
	 o3_8h(i1,j1,8) = o3_0(i1,j1)
       
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
       if (o3_0(i1,j1).ge.o3_max_1hr_jour(i1,j1) ) &	 
	   o3_max_1hr_jour(i1,j1) = o3_0(i1,j1)
	   	   	 	   
       do is = 1, 2
         if (nox_0(is,i1,j1).ge.nox_max_1hr_jour(is,i1,j1) ) &	 
	     nox_max_1hr_jour(is,i1,j1) = nox_0(is,i1,j1)
       end do

       if (tem2_0(i1,j1).ge.tem2_max_1hr_jour(i1,j1) ) &	 
	   tem2_max_1hr_jour(i1,j1) = tem2_0(i1,j1)
	   
       if (tem2_0(i1,j1).le.tem2_min_1hr_jour(i1,j1) ) &	 
	   tem2_min_1hr_jour(i1,j1) = tem2_0(i1,j1)	   	          
      !---------------------------------------------------------------       
      ! calcul du nombre de depassements du 180
      !--------------------------------------------------------------- 
         if (o3_0(i1,j1).ge.180.) then
	   nb_dep_180_1h(i1,j1) = nb_dep_180_1h(i1,j1) + 1.
	 end if
	 
         if (o3_0(i1,j1).ge.240.) then
	   nb_dep_240_1h(i1,j1) = nb_dep_240_1h(i1,j1) + 1.	   	   	   
	 end if	 	
      !----------------------------------------------------------------

        end do
      end do
	            	                                          
      end do ! ntimes !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                          
      ! A-t-on le bon nombre d'heures ?
      it_en_cours = it_en_cours + 1      
      write(*,*) 'nheures_par_jour_tmp=',nheures_par_jour_tmp
      if ( nheures_par_jour.eq.nheures_par_jour_tmp) then        
        !---------------------------------------------------------------       
        ! ecritures des donnees jour
        !---------------------------------------------------------------                 
        if(igrille)call ecrit_jour_ncstat_grille
        if(ipoint )call ecrit_jour_ncstat_point    
        !---------------------------------------------------------------       
        ! Ecritures des donnees mensuelles
        !---------------------------------------------------------------
        if(igrille)call ecrit_mois_ncstat_grille
        if(ipoint) call ecrit_mois_ncstat_point               
      end if      
        
      !---------------------------------------------------------------       
      ! calcul des moyenne annuelles
      !---------------------------------------------------------------
	do i1=1,nx1
	do j1=1,ny1
         o3_moy_an(i1,j1)         = o3_moy_an(i1,j1)         / n_o3_moy_an(i1,j1)
 	 nox_moy_an(1,i1,j1)      = nox_moy_an(1,i1,j1)      / n_nox_moy_an(1,i1,j1)
 	 nox_moy_an(2,i1,j1)      = nox_moy_an(2,i1,j1)      / n_nox_moy_an(2,i1,j1)
 	 pm10_moy_an(i1,j1)       = pm10_moy_an(i1,j1)       / n_pm10_moy_an(i1,j1)
	 pm25_moy_an(i1,j1)       = pm25_moy_an(i1,j1)       / n_pm25_moy_an(i1,j1)
 	 pm10_ecart_moy_an(i1,j1) = pm10_ecart_moy_an(i1,j1) / n_pm10_moy_an(i1,j1)
        end do
	end do      
      !---------------------------------------------------------------       
      ! ecrit dans un NetCDF pour le pas de temps it
      !---------------------------------------------------------------                
       if(igrille)call ecrit_an_statnc_grille  
       if(ipoint)call ecrit_an_statnc_point
            
      !---------------------------------------------------------------
      ! Ferme les NetCDF
      !---------------------------------------------------------------
      write(*,*) trim(fstat)//' OK'
      ! CHIMERE       
      !call check(nf90_close(out1fileID))
!      call check(nf90_close(met1fileID))          
      ! grille fine                  
!      call check(nf90_close(statfileID))       
      
99    continue      

   
      end program
   
