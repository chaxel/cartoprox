module params

! Version
      character(len=20), parameter :: exe_str='statistics.exe'
      real             , parameter :: version = 1.00
      character(len=16) :: version_str	

! Inputs                       
      real :: dx1,dx2,dx   
      
! Logical  
      logical :: igrille !fichier grille ?
      logical :: ipoint !fichier point ?      
      
! Mesh  
      real    :: dist                                         		    
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
      integer :: it,it_jour,it_en_cours      !timestep of observations in mmoutcdf file
      integer :: nheures_par_jour     !nb d'heures par jour 1 ou 24
      integer :: nheures_par_jour_tmp !compteur dans la journée 1      
      
      integer :: lonDimID,latDimID,zDimID, frTimeDimID, jourDimID, moisDimID, pointDimID
      integer :: lonVarID,latVarID,xVarID, frTimeStrDimID, zoneDimID, deptDimID
      
      integer :: out1fileID,statfileID  
      
      integer :: iostatus    
      
! index 
      integer :: dimensions
      integer :: start2d(2)
      integer :: count2d(2)      
      integer :: start3d(3)
      integer :: start4d(4)
      integer :: count3d(3)
      integer :: count4d(4) 

! long/lat    
      real,allocatable :: lon0(:,:)
      real,allocatable :: lat0(:,:)
! easting/northing          
      real,allocatable :: easting0(:,:) 
      real,allocatable :: northing0(:,:)           
! especes gazeuses (pour l'instant O3 seulement)
      real,allocatable :: o3_0 (:,:,:)      
      real,allocatable :: no2_0(:,:,:)
      real,allocatable :: pm10_0(:,:,:) 
      real,allocatable :: pm10k_0(:,:,:)        
      real,allocatable :: pm10_ecart_0(:,:,:)       
      real,allocatable :: pm10k_ecart_0(:,:,:)       
      real,allocatable :: tem2_0(:,:)
      real,allocatable :: phno3_0(:,:,:) 

! id_maille
      integer, allocatable :: id_maille(:,:)  
      integer, allocatable :: id_maille_vm(:,:)              
                              				       
! Grille CHIMERE 
      character(len=256) :: fout1
      character(len=256) :: fstat      
      integer          :: nx1,ny1,nz1    

! Varaibles locale
      real    :: tem2,sreh,w10m,fac,fac_teom,fac_hno3
      
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
      integer          :: mois_en_cours
      logical          :: imois_new      
      real,allocatable :: o3_moy_mois(:,:)
      real,allocatable :: no2_moy_mois(:,:)      
      real,allocatable :: pm10_moy_mois(:,:)      
                 
      real,allocatable :: n_o3_moy_mois(:,:)
      real,allocatable :: n_no2_moy_mois(:,:)       
      real,allocatable :: n_pm10_moy_mois(:,:)
      
      ! journalieres
      integer          :: jour_en_cours
      logical          :: ijour_new
      real,allocatable :: pm10_moy_jour(:,:)
      real,allocatable :: n_pm10_moy_jour(:,:)       
      real,allocatable :: pm10_ecart_moy_jour(:,:)
                           
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
! switch
      logical :: ipm, igaz, iphno3, iecart, iaot_foret, iaot_veg, icreate

! sorties
      logical :: ijour=.true. ! sorties horaires
      logical :: ihour=.false. ! sortie jour    
    
      ! nombre de jours de depasssement du x moyenne jour PM10
      integer, parameter  :: nseuils_moy_jour=8      
      integer, parameter  :: seuils_moy_jour(nseuils_moy_jour)=(/25,30,35,40,45,50,80,125/)
      character(len=256), parameter    :: seuils_moy_jour_str(nseuils_moy_jour)= &
      (/'nb_dep_25_jour','nb_dep_30_jour','nb_dep_35_jour','nb_dep_40_jour','nb_dep_45_jour','nb_dep_50_jour','nb_dep_80_jour','nb_dep_125_jour'/)
      real, allocatable  :: nb_dep_x_jour(:,:,:)    

!!! temperature !!!
      logical :: item2
      real,allocatable :: tem2_min_1hr_jour(:,:)
      real,allocatable :: tem2_max_1hr_jour(:,:)
      real,allocatable :: tem2_avg_1hr_jour(:,:)                  
      real,allocatable :: degres_18_jour(:,:) ! degres jour a 18°C
      real,allocatable :: degres_16_jour(:,:) ! degres jour a 18°C    
      real,allocatable :: degres_18_an(:,:) ! degres jour a 18°C
      real,allocatable :: degres_16_an(:,:) ! degres jour a 18°C   
      real :: dju_16, dju_18         
      
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
      character(len=256) :: o3_var
      character(len=256) :: no_var      
      character(len=256) :: no2_var  
      character(len=256) :: pm10_var  
      
! correction modele PM10 (bidouille)
      character(len=256) :: fic_correction_pm10  
      real               :: correction_pm10_jour,correction_pm10(366)

        
      
end module params        
