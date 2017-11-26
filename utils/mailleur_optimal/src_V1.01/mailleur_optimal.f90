      program mailleur_optimal
      
! Atmo Rhone-Alpes 2009
! Description : le program lit un fichier .MIF et crée un maillage intelligent
!               pour un modèle de rue type SIRANE
!              le maillage intelligent est défini par une distance à l'axe et
!              une distance inter-point (ex: à 50 m de l'axe, un point tous les 
!              20 m, à 1000 m de l'axe, un point tous les 1000 m)
! Principe : 1. lecture du réseau dans le fichier .MIF
!            2. calcul des paramètres des brins : étendue du domaine, longueurs
!            2. discrétisation des brins en points (parametres : dl_brin = 10 m)
!            3. définition d'un maillage initial
!            4. filtrage des points utilisés

      use params

      implicit none
      
      ! ON A POSE LES BASES C'EST PARTI !!!!

      ! lit le fichier
      call read_args
      
      ! lit le fichier
      call read_mif   
      
      ! calcul les parametres des brins  
      call calcul_brins
      
      ! calcul les parametres des recepteurs  
      call calcul_recept
      
      end program
      
          
