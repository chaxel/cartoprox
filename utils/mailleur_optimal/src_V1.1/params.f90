      module params
      
      !version
      character(len=5),parameter :: versionstr='1.10'
      
      ! noms des fichiers
      character(len=256) :: fichier_mif
      character(len=256) :: fichier_recept
      character(len=256) :: fichier_brin
      character(len=256) :: fichier_grille
      
      !!!! OPERATIONNEL !!!!
      logical :: maillage_optimal
      logical :: maillage_virtuel

      ! ENTREES      
      logical :: imif, iout, igrille
      logical :: igrille_secours ! version 1.03
      logical :: igrilleuser, ixmin, ixmax, iymin, iymax, ixc, iyc, idx, idy      
      logical :: ibrin_dx, irecept_dx, icadre_dx
      logical :: iutm, il2, igeo, igeoid, iutm_zone
            
      ! PROJECTION
      character(len=5)   :: geoidstr_i         ! nom de la geoid d'entree
      character(len=5)   :: geoidstr_o         ! nom de la geoid d'entree
      integer            :: geoid_i            ! geoid d'entree (1=WGS84, 2=ED50, 3=NTF, 4=GRS80)
      integer            :: geoid_o            ! geoid d'entree (1=WGS84, 2=ED50, 3=NTF, 4=GRS80)          
      integer            :: projection_i       ! projection entree (0=geo, 1=L2E, 2=UTM31)
      integer ,parameter :: projection_o=1     ! projection entree (0=geo, 1=L2E, 2=UTM31)      
      integer            :: zoneutm_i          ! zone UTM entre entree (0=geo, 1=L2E, 2=UTM31)
      integer ,parameter :: zoneutm_o=31       ! projection sortie (non utile)
      double precision   :: units=1.           ! unites (1.=metres, 1000.=kms)

      ! parametres de decoupage des brins : chaque brin est discretise en n pts tous les x m
      real :: brin_dx != 10. ! m  
      ! espacement de la grille de recepteurs (PARAMETRE DETERMINANT EN TEMPS)
      real :: dx_recept_ini != 5.      
      ! cadre autour du domaine
      real :: cadre_dx != 1000. 
      ! resolution maxi du maillage optimal
      real, parameter :: dx_recept_max = 1000.             
      ! cadre de passage cartesien -> coord lamb/geo
      real, parameter :: cadre_dx_defaut = 100. !metres assure que les recepteurs en projection UTM sont dans le domaine Lambert 2         
      !!!!!!!!!!!!!!!!!!!!!!
      
      !!!! DEBUG !!!!
      ! espacement de la grille de recepteurs
      !real, parameter :: dx_recept_ini = 50.      
      ! parametres de decoupage des brins : cahque brin est discretise en n pts tous les x m
      !real, parameter   :: brin_dx = 30. !m         
      !!!!!!!!!!!!!!!!!!!!!!

      ! parametres (classes d'ecartement des recepteurs), on peut utiliser une focntion...
  
       ! distance inter-recepteur
      integer, parameter :: nclass_recept = 10
      real, parameter    :: facteur_geom = 2.
                    
      real :: dy_recept(nclass_recept) ! calcule a partir de dx_recept_ini suivant une suite geometrique de raison r_dy_recept
      real :: r_dy_recept(nclass_recept)
      ! data dy_recept /  10., 30., 90., 270.,  810.,  2430. /

      ! distance à la voie
      real :: dx_recept(nclass_recept) ! calcule a partir de dy_recept
                    
      ! parametres des brins
      ! chaque brin est defini par un segment 1-----2, une longueur et un nb de points discretise
      integer :: nbrins
      real,    allocatable :: brin_x1(:)
      real,    allocatable :: brin_y1(:)      
      real,    allocatable :: brin_x2(:)
      real,    allocatable :: brin_y2(:) 
      real,    allocatable :: brin_long(:) 
      integer, allocatable :: brin_npts(:) ! discretisation      
         
      ! parametres de decoupage des brins : cahque brin est discretise en n pts tous les x m
      real    :: long_max       
      integer :: npts_max           
      real, allocatable :: brin_xd(:,:)  ! indices brin, pt
      real, allocatable :: brin_yd(:,:)  ! indices brin, pt

      real, allocatable :: brin_xd_cart(:,:)  ! UTM
      real, allocatable :: brin_yd_cart(:,:)  ! UTM
      
      integer :: npts     ! nombre de pts  
            
      ! parametres du domaine Lambert 2
      real :: xmin, xmax, ymin, ymax 
      
      ! parametres du domaine UTM (cartesien)
      real :: xmin_cart, xmax_cart, ymin_cart, ymax_cart       
      
      ! centre du domaine Lambert 2
      real :: xc, yc

      ! centre du domaine UTM
      real :: xc1, yc1      

      real :: dx, dy
      
      ! parametres du maillage de récepteurs
      integer :: nx, ny  
      
      ! parametres des recepteurs
      ! chaque recepteurs est defini par ses coordonnees 
      integer :: nrecept
      
      ! recepteurs UTM
      real, allocatable   :: recept_x1(:,:)
      real, allocatable   :: recept_y1(:,:)

      ! recepteurs Lambert 2
      real, allocatable   :: recept_x(:,:)
      real, allocatable   :: recept_y(:,:)      
      logical,allocatable :: recept_valid(:,:) 
      real,allocatable    :: recept_dist_recept(:,:)   
      real,allocatable    :: recept_dist_pt(:,:) 
      integer :: recept_valid_n                   
      
                   
      end module
