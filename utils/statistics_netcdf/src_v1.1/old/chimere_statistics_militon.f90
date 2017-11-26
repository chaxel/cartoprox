      program chimere_statistics
!********************************************************************
!*         PREVALP                               		    *
!*     Plateforme Regionale de Previsions de qualite de l'air       *
!*                           					    *
!*				  date: novembre 2007               *
!*          auteur: E. Chaxel                                       *
!*		       LEGI/GIERSA	                            *
!*			chaxeleric@yahoo.fr                         *
!********************************************************************
!chimere_staistics
! Lit les fichiers meteo.*dom.nc, dep.*dom.ncv et out.*dom.nc de CHIMERE 
! et calcule les statistiques
! Pour ozone
! AOT 80
! moyenne glissante 8 heures
! ...

! Pour PM10
! dépassements du 50 en moyenne jour 

      use netcdf
      use typesizes 
      
      implicit none         
      
      character*10 ::idatestr             
      
! Inputs                       
      real :: dx1,dx2,dx                                  

! Mesh  
      real :: dist       
               
      real, allocatable :: tcoord(:) 
      real, allocatable :: xcoord(:) 

      !rh calculation from INTERPF/MM5
      REAL :: es , qs
      REAL , PARAMETER :: svp1        =     0.6112
      REAL , PARAMETER :: eps         =     0.622      
      REAL , PARAMETER :: svp2        =    17.67
      REAL , PARAMETER :: svp3        =    29.65
      REAL , PARAMETER :: svpt0       =   273.15                         		    

      integer :: i1,j1,k1,ix,iy
      integer :: ns, np
      real    :: rsearch
      real    :: rtemp    

      ! General inputs     
      integer :: ntimes
      integer :: numSteps	     !Step number
      integer :: numFrTimes	     !Numbers of time steps
      integer :: timeStringLen       !Duration of one time step (in minutes)
      real    :: distGrid	     !Distance of grid cells (in meters)
       
      integer :: Year	     !Four-digit year of obs time
      integer :: Mnth	     !Month of obs time
      integer :: Day	     !Day of obs time
      integer :: Hour	     !Hour of obs time
      integer :: Min	     !Minute of obs time
      integer :: Sec	     !Second of obs time 
      integer :: nday        !number of day in a month
      integer :: it,it1      !timestep of observations in mmoutcdf file
      
      integer :: lonDimID,latDimID,zDimID, frTimeDimID, jourDimID, moisDimID
      integer :: lonVarID,latVarID,xVarID, frTimeStrDimID, zoneDimID, deptDimID
      
      integer :: out1fileID,out2fileID      
      
! index 
      integer :: dimensions
      integer :: start3d(3)
      integer :: start4d(4)
      integer :: count3d(3)
      integer :: count4d(4) 
    
! especes gazeuses (pour l'instant O3 seulement)
      real,allocatable :: o3_0 (:,:,:)      
      real,allocatable :: no2_0(:,:,:)
      real,allocatable :: pm10_0(:,:,:) 
      real,allocatable :: pm10k_0(:,:,:)        
      real,allocatable :: ecart_pm10_0(:,:,:) 
      
      real,allocatable :: ecart_pm10k_0(:,:,:)       

      real,allocatable :: hght0(:,:)
      real,allocatable :: tem2_0(:,:)
     
! variables emissions 1=NOX, 2=VOC, 3=PM10  
      real,allocatable :: emis0(:,:,:)        ! (id_polluant,x,y)
      real,allocatable :: emis2(:,:,:)        ! (id_polluant,x,y)
      real,allocatable :: ratio_emis2(:,:,:)  ! rapport des emissions maille fine/maille chimere
                                              ! (id_polluant,x,y)
      real,allocatable :: ratio_emis0(:,:,:)  ! rapport des emissions maille fine/maille chimere
                                              ! (id_polluant,x,y)     
      real,allocatable :: nratio_emis0(:,:,:) ! rapport des emissions maille fine/maille chimere
                                              ! (id_polluant,x,y)   					       
! Grille CHIMERE 
      character(len=256) :: fout1
      character(len=256) :: fout2      
      integer :: nx1,ny1,nz1
      integer :: nx2,ny2,nz2      
      real,allocatable :: lon0(:,:)
      real,allocatable :: lat0(:,:)
      real,allocatable :: topo0(:,:)
      real,allocatable :: land0(:,:)   

! id_maille
      integer, allocatable :: id_maille(:,:)  
      integer, allocatable :: id_maille_vm(:,:)              
                   
! Variables speciales
      real,allocatable :: o3_1D_1(:) ! Profil d'ozone moyen
      real,allocatable :: alti_1D_1(:) ! Profil d'altitude moyen      
      
! Varaibles locale
      integer :: npol
      real    :: topo,dzo3,nval,fac,dno,vapp1,qsbt1,vapp2,qsbt2,pres1,pres2
      real    :: ecart_krig
      real, parameter :: t0k = 273.15
      real    :: tem2,sreh,w10m
      
! suite           
      integer :: nd, ech
      character(len=10) :: pol  
! ------ UTM
      double precision  :: h1, h2, lon2r1, lat2r1, lon2r2, lat2r2, x2, y2, projunits
      integer           :: projtype=2 ! UTM
      double precision  :: utmzone=31
      integer :: gs1, gs2
      double precision :: d2r	 ! Degrees   ->Radians	 
      double precision :: r2d	 ! Radians   ->Degrees	 	     
      parameter( d2r=.0174532925199	   )
      parameter( r2d=57.2957795131	   )   
      
!      integer :: rundate   
      
      character(len=19) :: datestr
      
      integer,parameter :: nzone = 13
      integer :: nz
      real :: temp(3,nzone)     
      
! Grillle de  sorties
      character(len=39):: tunits
      character(len=19):: timeStr !YYYY-MM-DD HH:MM:SS
      character(len=4) :: yearStr
      character(len=2) :: mnthStr
      character(len=2) :: dayStr
      character(len=2) :: hourStr
      character(len=2) :: minStr
      character(len=2) :: secStr
               
! Variables de sorties
!!!!!!!!! Moyennes !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! annuelles
      real,allocatable :: o3_moy_an(:,:)
      real,allocatable :: no2_moy_an(:,:)        
      real,allocatable :: pm10_moy_an(:,:)
      real,allocatable :: pm10_ecart_moy_an(:,:)

      real,allocatable :: n_o3_moy_an(:,:)
      real,allocatable :: n_no2_moy_an(:,:)
      real,allocatable :: n_pm10_moy_an(:,:)
      
      ! mensuelles
      integer          :: mois_1
      real,allocatable :: o3_moy_mois(:,:)
      real,allocatable :: no2_moy_mois(:,:)      
      real,allocatable :: pm10_moy_mois(:,:)      
                 
      real,allocatable :: n_o3_moy_mois(:,:)
      real,allocatable :: n_no2_moy_mois(:,:)       
      real,allocatable :: n_pm10_moy_mois(:,:)           

!!!!!!!!! O3 ozone !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      real,allocatable :: aot40(:,:)
      real,allocatable :: aot80(:,:)      
!      real,allocatable :: aot45(:,:)      
      real,allocatable :: moy8h(:,:)

      real,allocatable :: o3_8h(:,:,:)       
           
      real,allocatable :: nb_dep_120_moy8h(:,:)
      real,allocatable :: nb_dep_180_1h(:,:)
      real,allocatable :: nb_dep_240_1h(:,:)      

      ! maximum 120/180 ug/m3 jour
      real,allocatable :: o3_max_8hr_jour(:,:)
      real,allocatable :: o3_max_1hr_jour(:,:)

      ! nombre de jours depasssement de jour du 180 pour l'ozone
      real,allocatable :: nb_dep_180_jour(:,:)  
      ! nombre de jours depasssement de jour du 120 moyenne 8 h
      real,allocatable :: nb_dep_120_jour(:,:)   
      ! nombre de jours depasssement de jour du 240 
      real,allocatable :: nb_dep_240_jour(:,:)                 

!!!!!!!!! NO2 dioxyde d azote !!!!!!!!!!!!!!!!!!
      real,allocatable :: no2_max_1hr_jour(:,:)
      ! nombre de jours depasssement de jour du 200 moyenne 1 h
      real,allocatable :: nb_dep_200_jour(:,:) 
      ! nombre de jours depasssement de jour du 180 pour l'ozone
      real,allocatable :: nb_dep_140_jour(:,:)        
            
      
!!!!!!!! PM10!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Corrections      
      logical :: ipm 

      real,allocatable   :: pm10_moy_jour(:,:) 
    
      ! nombre de jours de depasssement du x moyenne jour PM10
      integer, parameter  :: nseuils_moy_jour=8      
      real, parameter     :: seuils_moy_jour(nseuils_moy_jour)=(/25.,30.,35.,40.,45.,50.,80.,125./)
      character(len=256), parameter    :: seuils_moy_jour_str(nseuils_moy_jour)= &
      (/'nb_dep_25_jour','nb_dep_30_jour','nb_dep_35_jour','nb_dep_40_jour','nb_dep_45_jour','nb_dep_50_jour','nb_dep_80_jour','nb_dep_125_jour'/)
      real, allocatable  :: nb_dep_x_jour(:,:,:)    

!!! temperature !!!
      logical :: it2m
      real,allocatable :: tem2_min_1hr_jour(:,:)
      real,allocatable :: tem2_max_1hr_jour(:,:)
      real,allocatable :: tem2_avg_1hr_jour(:,:)                  
      real,allocatable :: degres_18_jour(:,:) ! degres jour a 18°C
      real,allocatable :: degres_16_jour(:,:) ! degres jour a 18°C    
      real,allocatable :: degres_18_an(:,:) ! degres jour a 18°C
      real,allocatable :: degres_16_an(:,:) ! degres jour a 18°C   
      real :: dju_16, dju_18         

!!! nitrate !!!
      logical :: iphno3
      real,allocatable :: phno3_0(:,:,:) 
      
! sorties
      logical :: ijour=.true. ! sorties horaires
      logical :: ihour=.false. ! sortie jour    

! region
      logical :: region1km   
      integer :: i, j, is     
      real                :: dept_region, zone_region,pop
      logical,allocatable :: iregion(:,:)
      real,allocatable    :: rregion(:,:)        
      real,allocatable    :: izone_region(:,:,:) 
      real,allocatable    :: idept_region(:,:,:) !1,7,26,38,73,74,69,42           
      real,allocatable    :: pop_region(:,:) 
      integer,allocatable :: tzone_region(:,:) 
      integer,allocatable :: tdept_region(:,:) !1,7,26,38,73,74,69,42         
      
! test
      character(len=10) :: o3_var
      character(len=10) :: no2_var  
      character(len=10) :: pm10_var
       
       
!--------------------------------------------------------------         
      
      fout1='out.nc'            
      fout2='stat.nc' 
          
!--------------------------------------------------------------------
      write(*,*) 'Lit la date dans '//trim(fout1)
     
      call getdimensions(fout1,nx1,ny1,nz1,&
                      numFrTimes,timeStringLen,&
                      distGrid,Year,Mnth,Day,&
		      Hour,Min,Sec)
		      
      TimeStringLen=60.           
      
!--------------------------------------------------------------------        
! Ouvre les fichiers de chimie et verifie la presence de PM10
!--------------------------------------------------------------------  
      write(*,*) 'Ouvre les fichiers NetCDF'
    ! grille CHIMERE
! aerosols Y/N
      write(*,*) 'Verifie la presence de PM10 dans CHIMERE'    
      call check(nf90_open(fout1, nf90_nowrite, out1FileID)) 
      if (nf90_inq_varid(out1fileID, 'PM10', xVarID).eq.0) then
        write(*,*) '> AEROSOLS DISPONIBLES'
	ipm=.true.
      else
        write(*,*) '> AEROSOLS INDISPONIBLES'      
        ipm=.false.      
      end if
! temperature Y/N
      write(*,*) 'Verifie la presence de tem2 dans CHIMERE'    
      call check(nf90_open(fout1, nf90_nowrite, out1FileID)) 
      if (nf90_inq_varid(out1fileID, 'tem2', xVarID).eq.0) then
        write(*,*) '> TEMPERATURE 2M DISPONIBLE'
	it2m=.true.
      else
        write(*,*) '> TEMPERATURE 2M INDISPONIBLE'      
        it2m=.false.      
      end if
            
! nitrates Y/N
      write(*,*) 'Verifie la presence de pHNO3 dans CHIMERE'    
      call check(nf90_open(fout1, nf90_nowrite, out1FileID)) 
      if (nf90_inq_varid(out1fileID, 'pHNO3', xVarID).eq.0) then
        write(*,*) '> pHNO3 DISPONIBLE'
	iphno3=.true.
      else
        write(*,*) '> pHNO3 INDISPONIBLE'      
        iphno3=.false.      
      end if      
      call check(nf90_close( out1FileID)) 


!--------------------------------------------------------------------                   
! Grille CHIMERE
      allocate( lon0(nx1,ny1) )    
      allocate( lat0(nx1,ny1) ) 
      allocate( land0(nx1,ny1) )    
      allocate( topo0(nx1,ny1) )       

! Ecrite les id_maille
      allocate( id_maille(nx1,ny1) ) 
      do j1=1,ny1
        do i1=1,nx1  
          id_maille(i1,j1) = ( j1 - 1 ) * nx1 + i1
        end do
      end do

! Ecrite les id_maille Vertical Mapper      
      allocate( id_maille_vm(nx1,ny1) ) 
      do j1=1,ny1
        do i1=1,nx1  
          id_maille_vm(i1,j1) = ( ny1 - j1 ) * nx1 + i1
        end do
      end do      
       
 ! Sort les resultats sur une grille regionale
      write(*,*) 'Dimensions', nx1, ny1, nz1
!---------------------------------
!---------------------------------
! Mailles region
      if ( (nx1 .eq. 275) .and. (ny1.eq.268) ) then
         region1km=.true.
         write(*,*) 'Lit mailles region'
         
	 allocate(iregion(nx1,ny1))
	 allocate(rregion(nx1,ny1))	
	 allocate(idept_region(nx1,ny1,99))		 
	 allocate(izone_region(nx1,ny1,13))
	 allocate(tdept_region(nx1,ny1))		 
	 allocate(tzone_region(nx1,ny1))	 
	 	 	 	 	  	 
	 allocate(pop_region(nx1,ny1))	 
	 	 
         do i1=1,nx1
           do j1=1,ny1	  	
	      iregion(i1,j1) = .false.
	      rregion(i1,j1) = 0.
	      pop_region(i1,j1) = 0.	      
	      idept_region(i1,j1,:) = 0.
	      izone_region(i1,j1,:) = 0.
	      tdept_region(i1,j1) = 0
	      tzone_region(i1,j1) = 0	      	      	      	      	      
	    end do
          end do 	 
	 
	 open( unit=10,file='MAILLES_CADASTRE_CADASTRE',status='old' )
	 open( unit=11,file='mailles_region1km.csv',status='unknown' )	 
	 

         do i1=1,nx1
           do j1=1,ny1	  
	      read(10,*,end=999) i, j, pop, zone_region, dept_region
	      ix =  ( i- 553500 )/1000 + 1
	      iy =  ( j-4887500 )/1000 + 1
	      if ( (ix .ge.1).and.(iy .ge.1).and.(ix .le.nx1).and.(iy .le.ny1) ) then 
	        iregion(ix,iy) = .true.
	        rregion(ix,iy) = 1.
		i = dept_region
		j = zone_region - 100
		tdept_region(ix, iy) = dept_region
                tzone_region(ix, iy) = zone_region - 100
		pop_region(ix, iy) = pop
		!write(*,*) ix, iy, i1, j1, pop	
		if ((i.ge.1).and.(i.le.99)) idept_region(ix, iy, i) = 1.
		if ((j.ge.1).and.(j.le.13)) izone_region(ix, iy, j) = 1.

	      end if        
	    end do
          end do   
	  
999      continue	  	 

	! ecrit dans un fichier csv
	 !ns = 0
         do ix=1,nx1
           do iy=1,ny1	  
	      if ( iregion(ix,iy) ) then 
	    !    ns = ns + 1	     
		write(11,'(I10,";",I5,";",I5,";",F12.5)') id_maille(ix,iy),tdept_region(ix, iy),tzone_region(ix, iy),pop_region(ix, iy)	
	      end if        
	    end do
          end do 


	 
	 close(10)
	 close(11)
	  
      else
        region1km=.false.
      end if
!-------- 
! Allocations pour la suite
!--------------------------------------------------------------------      
! Grille CHIMERE coarse
      ! out.*.nc        
      allocate(o3_0  (nx1,ny1,1))     
      allocate(no2_0 (nx1,ny1,1)) 
      allocate(pm10_0 (nx1,ny1,1))
      allocate(pm10k_0 (nx1,ny1,1))  
      allocate(ecart_pm10_0 (nx1,ny1,1))
      allocate(phno3_0 (nx1,ny1,1))      

      !       
      allocate(ecart_pm10k_0 (nx1,ny1,1))      
                                
      ! met.*.nc     
      allocate(tem2_0(nx1,ny1))                  
      
      ! variables de sorties

      ! moyennes annuelles
      allocate(o3_moy_an(nx1,ny1))
      allocate(no2_moy_an(nx1,ny1))
      allocate(pm10_moy_an(nx1,ny1))
      allocate(pm10_ecart_moy_an(nx1,ny1))      
      
      allocate(n_o3_moy_an(nx1,ny1))
      allocate(n_no2_moy_an(nx1,ny1))
      allocate(n_pm10_moy_an(nx1,ny1))
      
      ! moyennes mensuelles
      allocate(o3_moy_mois(nx1,ny1))
      allocate(no2_moy_mois(nx1,ny1))
      allocate(pm10_moy_mois(nx1,ny1))   
      
      allocate(n_o3_moy_mois(nx1,ny1))
      allocate(n_no2_moy_mois(nx1,ny1))
      allocate(n_pm10_moy_mois(nx1,ny1))      

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
      allocate(no2_max_1hr_jour(nx1,ny1))      
      allocate(nb_dep_200_jour(nx1,ny1))  
      allocate(nb_dep_140_jour(nx1,ny1))            

      ! PM10
      allocate(pm10_moy_jour(nx1,ny1))
      allocate(nb_dep_x_jour(nseuils_moy_jour,nx1,ny1))        

      ! tem2
      allocate(tem2_min_1hr_jour(nx1,ny1))
      allocate(tem2_max_1hr_jour(nx1,ny1))      
      allocate(tem2_avg_1hr_jour(nx1,ny1))      
      allocate(degres_16_jour(nx1,ny1))
      allocate(degres_18_jour(nx1,ny1))
      allocate(degres_16_an(nx1,ny1))
      allocate(degres_18_an(nx1,ny1))

! initialisations
       ! moyennes annuelles
       o3_moy_an=0.
       no2_moy_an=0.
       pm10_moy_an=0. 
       pm10_ecart_moy_an=0.          
       n_o3_moy_an=0.
       n_no2_moy_an=0.
       n_pm10_moy_an=0.
       
       ! moyennes mensuelles       
       o3_moy_mois=0.
       no2_moy_mois=0.
       pm10_moy_mois=0.          
       n_o3_moy_mois=0.
       n_no2_moy_mois=0.
       n_pm10_moy_mois=0.  
                                 
       aot80 = 0.
       aot40 = 0.
       tem2_max_1hr_jour = -9999.
       tem2_min_1hr_jour =  9999.
       tem2_avg_1hr_jour =  0.
       nb_dep_120_moy8h = 0.
       nb_dep_180_1h = 0.
       nb_dep_240_1h = 0.
       nb_dep_120_jour = 0.
       nb_dep_180_jour = 0.
       nb_dep_240_jour = 0.
       nb_dep_x_jour = 0.         
!--------------------------------------------------------------------        
! Read Model outputs for temperature at 2 m, q, u and v and 
!-------------------------------------------------------------------- 

! Creer un NetCDF de sorties
      write(*,*) 'Creer le NetCDF de sortie'
!!!!!!!!!!!!!!
  !Define timeString
      write(yearStr,'(I4)') Year
      if (Mnth.LT.10) then
        write(mnthStr,'(I1)') Mnth
      else
        write(mnthStr,'(I2)') Mnth
      end if
 
      if (Day.LT.10) then
        write(dayStr,'(I1)') Day
      else
        write(dayStr,'(I2)') Day
      end if
 
      if (Hour.LT.10) then
        write(hourStr,'(I1)') Hour
      else
        write(hourStr,'(I2)') Hour
      end if
 
      if (Min.LT.10) then
        write(minStr,'(I1)') Min
      else
        write(minStr,'(I2)') Min
      end if
 
      secStr='00'

    tunits = 'days since '//yearStr//'-'//trim(mnthStr)//'-'//trim(dayStr)! //' '&
      !//trim(hourStr)//':'//trim(minStr)//':'//trim(secStr)
      
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
    end if
    
    call check(nf90_def_var(out2fileID,'pm10_moy_an'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','Moyenne annuelle PM10'))
    call check(nf90_put_att(out2fileID, XVarID, 'units','microg/m3'))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) )
    
    call check(nf90_def_var(out2fileID,'pm10_ecart_moy_an'  ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(out2fileID, XVarID, 'long_name','Moyenne annuelle ecart PM10'))
    call check(nf90_put_att(out2fileID, XVarID, 'units','microg/m3'))
    call check(nf90_put_att(out2fileID, XVarID,  "_FillValue", -9999.0 ) )  
    
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
    
    if (it2m) then

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

    call check(nf90_close(out2fileID))
!--------------------------------------------------------------------        
! Ouvre les fichiers en lecture ou lecture/ecriture
!--------------------------------------------------------------------  
      write(*,*) 'Ouvre les fichiers NetCDF'
    ! grille CHIMERE
      write(*,*) 'Process CHIMERE chimie'    
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
      
      write(*,*) 'Ecrit id_maille'      
      call check(nf90_inq_varid(out2fileID, 'id_maille', xVarID))   
      call check(nf90_put_var(out2fileID, xVarID, id_maille, start = (/ 1, 1/) )) 
      
      write(*,*) 'Ecrit id_maille_vm'      
      call check(nf90_inq_varid(out2fileID, 'id_maille_vm', xVarID))   
      call check(nf90_put_var(out2fileID, xVarID, id_maille_vm, start = (/ 1, 1/) ))        
              
      if ( region1km )write(*,*) 'Ecrit infos region'   
      if ( region1km )call check(nf90_inq_varid(out2fileID, 'contour_region', xVarID))        
      if ( region1km )call check(nf90_put_var(out2fileID, xVarID, rregion, start = (/ 1, 1/) )) 
      if ( region1km )call check(nf90_inq_varid(out2fileID, 'dept', xVarID))  
      if ( region1km )call check(nf90_put_var(out2fileID, xVarID, idept_region, start = (/ 1, 1, 1/) )) 
      if ( region1km )call check(nf90_inq_varid(out2fileID, 'zone', xVarID))  
      if ( region1km )call check(nf90_put_var(out2fileID, xVarID, izone_region, start = (/ 1, 1, 1/) )) 
      if ( region1km )call check(nf90_inq_varid(out2fileID, 'pop99', xVarID)) 
      if ( region1km )call check(nf90_put_var(out2fileID, xVarID, pop_region, start = (/ 1, 1/) )) 

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
      no2_var='NO2'      
      pm10_var='PM10'      
  
      ! O3 KRIG
      if ( nf90_inq_varid(out1fileID, 'O3_KRIG', xVarID)  .eq.0 ) o3_var='O3_KRIG'
      if ( nf90_inq_varid(out1fileID, 'NO2_KRIG', xVarID) .eq.0 ) no2_var='NO2_KRIG'    
  
      ! O3 KRIG
      call check(nf90_inq_varid(out1fileID, o3_var, xVarID))
      call check(nf90_inquire_variable(out1fileID,  xVarID, ndims = dimensions ))     
      if (it .eq. 1)  write(*,*) '... '//trim(o3_var)//' ndims=', dimensions
      if ( dimensions .eq. 3 ) then
        call check(nf90_get_var(out1fileID, xVarID, o3_0,  start=start3d ))
      else if ( dimensions .eq. 4 ) then
        call check(nf90_get_var(out1fileID, xVarID, o3_0,  start=start4d ))      
      end if
      
      ! NO2 KRIG
      call check(nf90_inq_varid(out1fileID, no2_var, xVarID))
      call check(nf90_inquire_variable(out1fileID,  xVarID, ndims = dimensions ))  
      if (it .eq. 1)  write(*,*) '... '//trim(no2_var)//' ndims=', dimensions
      if ( dimensions .eq. 3 ) then
        call check(nf90_get_var(out1fileID, xVarID, no2_0,  start=start3d ))
      else if ( dimensions .eq. 4 ) then
        call check(nf90_get_var(out1fileID, xVarID, no2_0,  start=start4d ))      
      end if
   
      if (ipm) then
      
        
      if ( nf90_inq_varid(out1fileID, 'PM10_KRIG', xVarID) .eq.0 ) then  
          
	! PM10 TEOM KRIG       
	call check(nf90_inquire_variable(out1fileID,  xVarID, ndims = dimensions ))   
        if (it .eq. 1)  write(*,*) '... PM10_KRIG ndims=', dimensions
        if ( dimensions .eq. 3 ) then
          call check(nf90_get_var(out1fileID, xVarID, pm10k_0,  start=start3d ))
        else if ( dimensions .eq. 4 ) then
          call check(nf90_get_var(out1fileID, xVarID, pm10k_0,  start=start4d ))      
        end if
	
	! PM10 TEOM        
	call check(nf90_inq_varid(out1fileID, 'PM10', xVarID))
        call check(nf90_inquire_variable(out1fileID,  xVarID, ndims = dimensions )) 
        if (it .eq. 1)  write(*,*) '... PM10 non volat ndims=', dimensions	    
        if ( dimensions .eq. 3 ) then
          call check(nf90_get_var(out1fileID, xVarID, pm10_0,  start=start3d ))
        else if ( dimensions .eq. 4 ) then
          call check(nf90_get_var(out1fileID, xVarID, pm10_0,  start=start4d ))      
        end if

        if (it2m) then
        call check(nf90_inq_varid(out1fileID, 'tem2', xVarID))
        call check(nf90_inquire_variable(out1fileID,  xVarID, ndims = dimensions ))  	   
        if (it .eq. 1)  write(*,*) '... tem2 ndims=', dimensions	
        if ( dimensions .eq. 3 ) then
          call check(nf90_get_var(out1fileID, xVarID, tem2_0,  start=start3d ))
        else if ( dimensions .eq. 4 ) then
          call check(nf90_get_var(out1fileID, xVarID, tem2_0,  start=start4d ))      
        end if	
        end if    
	
	! ECART_FDMS
        call check(nf90_inq_varid(out1fileID, 'ECART_FDMS', xVarID))
        if (it .eq. 1) write(*,*) '... ECART_FDMS', dimensions
        call check(nf90_inquire_variable(out1fileID,  xVarID, ndims = dimensions ))     
        if ( dimensions .eq. 3 ) then
          call check(nf90_get_var(out1fileID, xVarID, ecart_pm10_0,  start=start3d ))
        else if ( dimensions .eq. 4 ) then
          call check(nf90_get_var(out1fileID, xVarID, ecart_pm10_0,  start=start4d ))      
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

        ! version du calcul FDMS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        if ((Year.eq.2007).and.(.not.iphno3)) then
          if (it .eq. 1) write(*,*) '> CALCUL FDMS : METHODE MICALLEF 2008'
        else if (iphno3) then
          if (it .eq. 1) write(*,*) '> CALCUL FDMS : METHODE MILITON 2009'		
        else
          write(*,*) '> CALCUL FDMS : ANNEE INCONUE',Year
	  stop
        end if
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        ! test sur ecart FDMS entrant
	  do i1=1,nx1
	  do j1=1,ny1	   
	   if ((ecart_pm10_0(i1,j1,1).le.-5.).and.(iregion(i1,j1)))&
	   write(*,'("WARNING ECART FDMS <-5 (ecart,it,i,j)=",F10.2,I6,2I4)')ecart_pm10_0(i1,j1,1),it,i1,j1
	  end do
	  end do		   
	   	
	
	if ((Year.eq.2007).and.(.not.iphno3)) then	
	   
	  do i1=1,nx1
	  do j1=1,ny1	   
	   ecart_pm10k_0(i1,j1,1)= ecart_pm10_0(i1,j1,1) + (pm10k_0(i1,j1,1) - pm10_0(i1,j1,1)  ) * .2259
	  end do
	  end do		   
	   	
	end if
	
	if (iphno3) then
	  
	  ! facteur (Militon, 2009) sur la temperature horaire 
	  do i1=1,nx1
	   do j1=1,ny1	
	    
	    tem2 = tem2_0(i1,j1) - 273.15 ! K -> degC
	    
	    if ( (tem2.lt.-50).or.(tem2.gt.50)) write(*,*) 'WARNING TEMPERATURE TEM2 (degC)=',tem2		    
	    
	    ! facteur multiplicatif TEOM
	    fac = 0.1 + 0.2993 * exp( -0.09 * tem2 )	
   
	    ! recalcul de l'ecart_fdms ecart_pm10k_0 (version Militon, 2009)
	    ecart_pm10k_0(i1,j1,1)= 1.647 * phno3_0(i1,j1,1) + fac * pm10k_0(i1,j1,1)	    	
	   end do
	  end do
	  
	end if

	! test sur l'ecart FDMS > 0
	do i1=1,nx1
        do j1=1,ny1		
 
	 if ( pm10k_0(i1,j1,1).ge.0. ) then 
	 
	  if ( ecart_pm10k_0(i1,j1,1).le.0 ) then
	   if ((ecart_pm10k_0(i1,j1,1).le.-5.).and.(iregion(i1,j1))) then
	    write(*,'("WARNING ECART FDMS KRIG <-5 (ecart,it,i,j)=",F10.2,I6,2I4)') ecart_pm10k_0(i1,j1,1),it,i1,j1
	    write(*,'("ecart_fdms,tem2,fac,pm10_k,pm10_nonvolat",5F10.2)') ecart_pm10_0(i1,j1,1),tem2,fac ,pm10k_0(i1,j1,1), pm10_0(i1,j1,1)
	   end if
	   ecart_pm10k_0(i1,j1,1) = ecart_pm10_0(i1,j1,1)
	  end if	 

	  pm10_0(i1,j1,1) = pm10k_0(i1,j1,1) + ecart_pm10k_0(i1,j1,1)
	  
	 else
	 
	  !write(*,'("WARNING PM10 KRIG < 0 (PM10,it,i,j)=",F10.2,I6,2I4)') pm10k_0(i1,j1,1),it,i1,j1	 
	  pm10_0(i1,j1,1) = pm10_0(i1,j1,1) + ecart_pm10_0(i1,j1,1)
	   
	 end if
	 	 	 
	end do
	end do	
	        
      else
      
        if (it .eq. 1)  write(*,*) '... PM10'
	call check(nf90_inq_varid(out1fileID, 'PM10', xVarID) )
        call check(nf90_get_var(out1fileID, xVarID, pm10_0,  start=start4d )) 
	     
      end if       
      end if    
      
  
      !if ( it .eq. 1 ) write(*,*) '... Times'       
      !call check(nf90_inq_varid(out2fileID, 'Times', xVarID))
      !call check(nf90_put_var(out2fileID, XVarID, datestr, start = (/1,it/) )) 

      if (  Hour.eq.0 ) then
        if ((Mnth.ge.5).and.(Mnth.le.7)) then
           write(*,*) 'Trouve date : '//datestr//' (dans AOT 40)'
        else		
           write(*,*) 'Trouve date : '//datestr//' (hors AOT 40)'
        end if
      end if

      do i1=1,nx1
        do j1=1,ny1

          ! conversion ppb -> microg/m3
          o3_0(i1,j1,1)  = o3_0(i1,j1,1)  * 2.00
          no2_0(i1,j1,1) = no2_0(i1,j1,1) * 1.91	  
	  
	  ! contrainte sur heure 8 h à 20 h heure HL hiver -> 7 h à 19 h TU du 1er mai au 31 juillet
	  ! polair extrait de 07 TU (06 TU BDQA)
	  if ( (Hour.ge.6) .and. (Hour.le.17).and.(Mnth.ge.5).and.(Mnth.le.7) ) then
	  !!!!!!!!!!!!!!!!!!!!!!!!!!
      !---------------------------------------------------------------       
      ! calcul de l'AOT 80
      !--------------------------------------------------------------- 
           if (o3_0(i1,j1,1).gt.80.) then
	     aot40(i1,j1) = aot40(i1,j1) + ( o3_0(i1,j1,1) - 80. )	 
	   end if 
	!!!!!!!!!!!!!!!!!!!!!!! 
	 end if	 
	 if ( (Hour.ge.6) .and. (Hour.le.17).and.(Mnth.ge.4).and.(Mnth.le.9) ) then
      !---------------------------------------------------------------       
      ! calcul de l'AOT 80
      !--------------------------------------------------------------- 
           if (o3_0(i1,j1,1).gt.80.) then
	     aot80(i1,j1) = aot80(i1,j1) + ( o3_0(i1,j1,1) - 80. )  
	   end if 
	!!!!!!!!!!!!!!!!!!!!!!! 
	 end if	 
      !---------------------------------------------------------------       
      ! calcul des depâssements jour pour PM10/O3
      !--------------------------------------------------------------- 
         !if ( (i1*j1.eq.1).and.( mod(it-1,24) .eq. 0) ) write(*,*) 'Trouve date : '//datestr

         if ( (it.gt.1).and.( mod(it-1,24) .eq. 0) ) then 
	  	   	   
	   ! PM10: jour de depassement du x ug/m3 
	   do is= 1, nseuils_moy_jour
	   if ( pm10_moy_jour(i1,j1) .ge. seuils_moy_jour(is) ) &
	     nb_dep_x_jour(is,i1,j1) =  nb_dep_x_jour(is,i1,j1) + 1	
	   end do   
	        
	   ! O3: jour de depassement du 180 ug/m3 
	   if ( o3_max_1hr_jour(i1,j1) .ge. 180. ) &
	     nb_dep_180_jour(i1,j1) =  nb_dep_180_jour(i1,j1) + 1
	     	     	     
	   ! O3: jour de depassement du 240 ug/m3 
	   if ( o3_max_1hr_jour(i1,j1) .ge. 240. ) &
	     nb_dep_240_jour(i1,j1) =  nb_dep_240_jour(i1,j1) + 1	     
	     
	   ! O3: jour de depassement du 120 ug/m3 
	   if ( o3_max_8hr_jour(i1,j1) .ge. 120. ) &
	     nb_dep_120_jour(i1,j1) =  nb_dep_120_jour(i1,j1) + 1	     	    

	   ! NO2: jour de depassement du 200 ug/m3 
	   if ( no2_max_1hr_jour(i1,j1) .ge. 140. ) &
	     nb_dep_140_jour(i1,j1) =  nb_dep_140_jour(i1,j1) + 1
	     
	   ! NO2: jour de depassement du 200 ug/m3 
	   if ( no2_max_1hr_jour(i1,j1) .ge. 200. ) &
	     nb_dep_200_jour(i1,j1) =  nb_dep_200_jour(i1,j1) + 1
	      
	 end if 
	 	  	   
         if (pm10_0(i1,j1,1).ge.0) then	 

           ! moyennes
	   pm10_moy_jour(i1,j1)     = pm10_moy_jour(i1,j1) + pm10_0(i1,j1,1) / 24.	   
 	   pm10_moy_an(i1,j1)       = pm10_moy_an(i1,j1)   + pm10_0(i1,j1,1)	   
 	   pm10_moy_mois(i1,j1)     = pm10_moy_mois(i1,j1) + pm10_0(i1,j1,1)	   	   
 	   pm10_ecart_moy_an(i1,j1) = pm10_ecart_moy_an(i1,j1) + ecart_pm10k_0(i1,j1,1) 	   	
	   
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
	 	 
      end if	

      !---------------------------------------------------------------       
      ! ecritures des donnees jour
      !---------------------------------------------------------------                  
      if ( (it.ge.24) .and. (mod(it-1,24).eq.0).and.(ijour) ) then

          it1 = it/24
	  if (ipm) then
	    call check(nf90_inq_varid(out2fileID, 'pm10_moy_jour', xVarID))      
            call check(nf90_put_var(out2fileID, XVarID, pm10_moy_jour, start = (/ 1,1,it1/) ))
          end if
	  call check(nf90_inq_varid(out2fileID, 'o3_max_1hr_jour', xVarID))      
          call check(nf90_put_var(out2fileID, XVarID, o3_max_1hr_jour, start = (/ 1,1,it1/) ))
	  
	  call check(nf90_inq_varid(out2fileID, 'no2_max_1hr_jour', xVarID))      
          call check(nf90_put_var(out2fileID, XVarID, no2_max_1hr_jour, start = (/ 1,1,it1/) ))	  	
	    	  
	  if (it2m) then
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
	 pm10_moy_jour   = 0.
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

      call check(nf90_open(fout2, nf90_write, out2FileID)) 
      
      write(*,*) 'o3_moy_an'
      call check(nf90_inq_varid(out2fileID, 'o3_moy_an', xVarID))
      call check(nf90_put_var(out2fileID, XVarID, o3_moy_an, start = (/ 1,1/) ))

      write(*,*) 'no2_moy_an'
      call check(nf90_inq_varid(out2fileID, 'no2_moy_an', xVarID))
      call check(nf90_put_var(out2fileID, XVarID, no2_moy_an, start = (/ 1,1/) ))

      write(*,*) 'pm10_moy_an'
      call check(nf90_inq_varid(out2fileID, 'pm10_moy_an', xVarID))
      call check(nf90_put_var(out2fileID, XVarID, pm10_moy_an, start = (/ 1,1/) ))

      write(*,*) 'pm10_ecart_moy_an'
      call check(nf90_inq_varid(out2fileID, 'pm10_ecart_moy_an', xVarID))
      call check(nf90_put_var(out2fileID, XVarID, pm10_ecart_moy_an, start = (/ 1,1/) ))

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

      do is= 1, nseuils_moy_jour      
        write(*,*) trim(seuils_moy_jour_str(is))
        call check(nf90_inq_varid(out2fileID, trim(seuils_moy_jour_str(is)), xVarID))      
        call check(nf90_put_var(out2fileID, XVarID, nb_dep_x_jour(is,:,:), start = (/ 1,1/) ))
      end do

      
      end if
      
      if (it2m) then

      write(*,*) 'degres_18_an'          
      call check(nf90_inq_varid(out2fileID, 'degres_18_an', xVarID))      
      call check(nf90_put_var(out2fileID, XVarID, degres_18_an, start = (/ 1,1/) ))

      write(*,*) 'degres_16_an'          
      call check(nf90_inq_varid(out2fileID, 'degres_16_an', xVarID))      
      call check(nf90_put_var(out2fileID, XVarID, degres_16_an, start = (/ 1,1/) ))
      
      end if      
      
      call check(nf90_close(out2fileID)) 
      
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
   
