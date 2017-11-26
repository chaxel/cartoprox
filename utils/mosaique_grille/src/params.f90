      module params
      
      !version
      character(len=5),parameter :: versionstr='3'
      
      !integer
      character(len=3) :: agregation
     
      ! entrees
      logical :: idebug, ihelp, ivar
      logical :: ifine,   ixminf, iyminf, idxf, idyf
      logical :: icoarse, ixminc, iyminc, idxc, idyc
      
      real    :: xminf, yminf, dxf, dyf ! grille fine
      real    :: xminc, yminc, dxc, dyc ! grille coarse  
      
      integer :: nxf, nyf, nzf, ntf
      integer :: nxc, nyc, nzc, ntc     
      
      integer :: it1, it2
             
      character(len=256) :: fic_coarse
      character(len=256) :: fic_fine    
      character(len=256) :: var_nom
    
      !coordonnes des points (stations)     
      real,allocatable     :: xc(:,:), yc(:,:)
      real,allocatable     :: xf(:,:), yf(:,:)      
      
      ! calcul de l'interpolation
      integer,allocatable  :: ix(:,:), iy(:,:)
      real,allocatable     :: wx(:,:), wy(:,:)
      	   
      ! netCDF
      integer :: cFileID,fFileID,xVarID,xDimId,yDimId,frTimeDimId,zDimID
                     
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
      
      character(len=10) :: pol
      
      integer :: julian
     

     
      end module
