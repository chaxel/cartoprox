      module params
      
      !version
      character(len=5),parameter :: versionstr='1.03'
      
      ! noms des fichiers
      character(len=256) :: fichier_ascii
      character(len=256) :: fichier_recept
      character(len=256) :: fichier_grille
      
      !!!! OPERATIONNEL !!!!
      logical :: maillage_optimal
      logical :: maillage_virtuel

      ! ENTREES      
      logical :: iascii, iout, igrille
      logical :: igrilleuser, ixmin, ixmax, iymin, iymax, ixc, iyc, idx, idy, iverbose, idebug
      logical :: ibrin_dx, irecept_dx, icadre_dx
      logical :: iutm_i, il2_i, igeo_i, igeoid_i, iutm_zone_i
      logical :: iutm_o, il2_o, igeo_o, igeoid_o, iutm_zone_o
      logical :: ipolair ! sortie format polair
      
      ! PROJECTION
      character(len=5)   :: geoidstr_i         ! nom de la geoid d'entree
      character(len=5)   :: geoidstr_o         ! nom de la geoid d'entree
      integer            :: geoid_i                    ! geoid d'entree (1=WGS84, 2=ED50, 3=NTF, 4=GRS80)
      integer            :: geoid_o                    ! geoid d'entree (1=WGS84, 2=ED50, 3=NTF, 4=GRS80)          
      integer            :: projection_i               ! projection entree (0=geo, 1=L2E, 2=UTM31)
      integer            :: projection_o               ! projection entree (0=geo, 1=L2E, 2=UTM31)      
      integer            :: zoneutm_i                  ! zone UTM entre entree (0=geo, 1=L2E, 2=UTM31)
      integer            :: zoneutm_o                  ! projection sortie (non utile)
      double precision   :: units=1.                   ! unites (1.=metres, 1000.=kms)
!!!!!!!!!!!

      ! parametres (classes d'ecartement des recepteurs), on peut utiliser une focntion...                      
      integer :: npts     ! nombre de pts  
                    
      ! coordonnees du point en entree
      double precision :: xc, yc

      ! parametres des recepteurs
      ! chaque recepteurs est defini par ses coordonnees 
      integer :: nrecept
      
      ! recepteurs UTM
      double precision, allocatable   :: recept_x1(:)
      double precision, allocatable   :: recept_y1(:)

      ! recepteurs Lambert 2
      double precision, allocatable   :: recept_x(:)
      double precision, allocatable   :: recept_y(:)                      
                         
      end module
