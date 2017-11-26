      module params
      
      !version
      character(len=5),parameter :: versionstr='3'
      
      !integer
      character(len=3) :: agregation
     
      ! entrees
      logical :: ixmin, iymin, ixp, iyp, idx, idy, ific, ivar, istep, idebug, ific_stations, idatestr
      logical :: ismooth
      logical :: ihour
      logical :: ijour
      real    :: xmin, ymin, dx, dy
      integer :: step, it1, it2
      
      ! smoothing
      integer :: nsmooth
      
      character(len=256) :: fic_grille_nc, fic_stations
      character(len=256) :: var_nom
      
      ! nombre de points (lecture d'un fichier ASCII)
      integer, parameter :: smax = 10000000  
      integer :: nstat
      
      !sorties      
      real,allocatable    :: output(:)
     
      !coordonnes des points (stations)     
      real,allocatable     :: xp(:), yp(:), zp(:)
      
      ! calcul de l'interpolation
      integer,allocatable  :: ix(:), iy(:)
      real,allocatable     :: wx(:), wy(:)
      
 !--------Version 3 Voisinage
      integer :: ix_voisinage(2) ! i min et i max du voisinage
      integer :: iy_voisinage(2) ! j min et j max du voisinage
	   
      ! netCDF
      integer :: ncFileID,xVarID,lonDimId,latDimId,frTimeDimId,zDimID
      
      ! netCDF stations
      logical           :: ific_stations_nc ! format netcdf ?
      integer           :: ncStatID,pointDimId,eastingVarID,northingVarID
      character(len=19) :: datestr_stations 
      integer           :: numsteps_stations 
      integer           :: it_stations   

      ! parametres de grille 
      integer :: nxx, nyy
      real, allocatable :: xg(:,:), yg(:,:)       
            
      ! donnees 3D    
      real,allocatable :: var_data(:,:,:)                                                 
      integer          :: var_dim
      real             :: var_fac              
                        
      integer :: n,nvar
      integer :: date, idate
      integer :: ih      

      real :: jdate
      real :: ppb2ug

      integer :: i,j, is
      integer :: it, itstart, itstop

      character(len=3):: istr

      
      integer :: nlev
      
!--------NetCDF
      integer :: numSteps	     !Step number
      integer :: timeStringLen       !Duration of one time step (in minutes)
      real    :: distGrid	     !Distance of grid cells (in meters)
      
      integer :: startYear	     !Four-digit year of start time
      integer :: startMnth	     !Month of start time
      integer :: startDay	     !Day of start time
      integer :: startHour	     !Hour of start time
      integer :: startMin	     !Minute of start time
      integer :: startSec	     !Second of start time  
      
      integer :: Year,Mnth,Day,Hour,Minu,Sec
      
      character(len=19) :: datestr

      character(len=19) :: datestr1, datestr2
      
      character(len=10) :: pol
      
      integer :: julian
     

     
      end module
