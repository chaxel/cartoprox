   subroutine allocation
   
      use netcdf
      use typesizes 
      use params
      
      implicit none   

      ! Grille CHIMERE
      allocate( lon0(nx1,ny1) )    
      allocate( lat0(nx1,ny1) )   
      allocate( easting0(nx1,ny1) )    
      allocate( northing0(nx1,ny1) )  
      
      ! out.*.nc        
      allocate(o3_0  (nx1,ny1))     
      allocate(nox_0 (2,nx1,ny1)) 
      allocate(pm10_0 (nx1,ny1))
      allocate(pm25_0 (nx1,ny1))      
      allocate(pm10k_0 (nx1,ny1))  
      allocate(pm10_ecart_0 (nx1,ny1))
      allocate(phno3_0 (nx1,ny1))      

      !       
      allocate(pm10k_ecart_0 (nx1,ny1))      
                                
      ! met.*.nc     
      allocate(tem2_0(nx1,ny1))                  
      
      ! variables de sorties

      ! moyennes annuelles
      allocate(o3_moy_an(nx1,ny1))
      allocate(nox_moy_an(2,nx1,ny1))
      allocate(pm10_moy_an(nx1,ny1))
      allocate(pm25_moy_an(nx1,ny1))      
      allocate(pm10_ecart_moy_an(nx1,ny1))      
      
      allocate(n_o3_moy_an(nx1,ny1))
      allocate(n_nox_moy_an(2,nx1,ny1))
      allocate(n_pm10_moy_an(nx1,ny1))
      allocate(n_pm25_moy_an(nx1,ny1))      
      
      ! moyennes mensuelles
      allocate(o3_moy_mois(nx1,ny1))
      allocate(nox_moy_mois(2,nx1,ny1))
      allocate(pm10_moy_mois(nx1,ny1))   
      
      allocate(n_o3_moy_mois(nx1,ny1))
      allocate(n_nox_moy_mois(2,nx1,ny1))
      allocate(n_pm10_moy_mois(nx1,ny1)) 
      
      ! moyennes jour
      allocate(pm10_moy_jour(nx1,ny1))
      allocate(pm10_ecart_moy_jour(nx1,ny1))      
      allocate(pm25_moy_jour(nx1,ny1))      
      
      allocate(n_pm10_moy_jour(nx1,ny1))
      allocate(n_pm25_moy_jour(nx1,ny1))
      
      ! x depassements moyennes jour
      allocate(nb_dep_x_jour(nseuils_moy_jour,nx1,ny1))      	     

      ! O3
      allocate(aot80(nx1,ny1))
      allocate(aot40(nx1,ny1))
      allocate(moy8h(nx1,ny1))
      allocate(nb_dep_120_moy8h(nx1,ny1))
      allocate(nb_dep_180_1h(nx1,ny1))
      allocate(nb_dep_240_1h(nx1,ny1))
      allocate(nb_dep_120_jour(nx1,ny1))
      allocate(nb_dep_180_jour(nx1,ny1))      
      allocate(nb_dep_240_jour(nx1,ny1))
      allocate(o3_8h(nx1,ny1,8))
      
      allocate(o3_max_8hr_jour(nx1,ny1))
      allocate(o3_max_1hr_jour(nx1,ny1))
            
      ! NO2
      allocate(nox_max_1hr_jour(2,nx1,ny1))      
      allocate(nb_dep_200_jour(nx1,ny1))  
      allocate(nb_dep_140_jour(nx1,ny1))            
         
      ! tem2
      !if (item2) then      
        allocate(tem2_min_1hr_jour(nx1,ny1))
        allocate(tem2_max_1hr_jour(nx1,ny1))      
        allocate(tem2_avg_1hr_jour(nx1,ny1))      
        allocate(degres_16_jour(nx1,ny1))
        allocate(degres_18_jour(nx1,ny1))
        allocate(degres_16_an(nx1,ny1))
        allocate(degres_18_an(nx1,ny1))
      !end if

! initialisations
       ! concentration
       o3_0  = 0.
       nox_0 = 0.
       pm10_0 = 0.
       pm25_0 = 0.       
       pm10k_0 = 0.
       pm10_ecart_0 = 0.
       phno3_0 = 0.

       ! moyennes annuelles
       o3_moy_an=0.
       nox_moy_an=0.         
       n_o3_moy_an=0.
       n_nox_moy_an=0.
       
       !if (ipm) then       
       pm10_moy_an=0. 
       pm25_moy_an=0.        
       pm10_ecart_moy_an=0.        
       n_pm10_moy_an=0.
       n_pm25_moy_an=0.       
       !end if
       
       ! moyennes mensuelles       
       o3_moy_mois=0.
       nox_moy_mois=0.          
       n_o3_moy_mois=0.
       n_nox_moy_mois=0.
       
       !if (ipm) then
       pm10_moy_mois=0.       
       n_pm10_moy_mois=0.  
       !end if
                         
       aot80 = 0.
       aot40 = 0.
       !if (item2) then
       tem2_max_1hr_jour = -9999.
       tem2_min_1hr_jour =  9999.
       tem2_avg_1hr_jour =  0.
       !end if
       nb_dep_120_moy8h = 0.
       nb_dep_180_1h = 0.
       nb_dep_240_1h = 0.
       nb_dep_120_jour = 0.
       nb_dep_180_jour = 0.
       nb_dep_240_jour = 0.
       !if (ipm)&
       nb_dep_x_jour = 0.
       
   end subroutine allocation
