      module params
      
      ! noms des fichiers
      character(len=256) :: fichier_mif      
      character(len=256) :: fichier_recept
      
      !!!! OPERATIONNEL !!!!
      logical :: maillage_optimal
      logical :: maillage_virtuel

      ! ENTREES      
      logical :: imif, iout                    
      logical :: ixmin, ixmax, iymin, iymax, ixc, iyc, idx, idy      
      logical :: ibrin_dx, irecept_dx, icadre_dx      

      ! parametres de decoupage des brins : chaque brin est discretise en n pts tous les x m
      real :: brin_dx != 10. ! m  
      ! espacement de la grille de recepteurs (PARAMETRE DETERMINANT EN TEMPS)
      real :: dx_recept_ini != 5.      
      ! cadre autour du domaine
      real :: cadre_dx != 1000.          
      !!!!!!!!!!!!!!!!!!!!!!
      
      !!!! DEBUG !!!!
      ! espacement de la grille de recepteurs
      !real, parameter :: dx_recept_ini = 50.      
      ! parametres de decoupage des brins : cahque brin est discretise en n pts tous les x m
      !real, parameter   :: brin_dx = 30. !m         
      !!!!!!!!!!!!!!!!!!!!!!

      ! parametres (classes d'ecartement des recepteurs), on peut utiliser une focntion...
  
       ! distance inter-recepteur
      integer, parameter :: nclass_recept = 8 
      real :: facteur_geom = 3.
                    
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
      integer :: npts     ! nombre de pts  
            
      ! parametres du domaine
      real :: xmin, xmax, ymin, ymax 
      
      ! parametres du domaine
      real :: xc, yc, dx, dy
             
      end module
