      subroutine read_args

      use params

      implicit none
      
!     arguments
      character(len=256) :: args(4)      
      
      maillage_optimal=.false.
      maillage_virtuel=.false.      

      ! arguments: lit le nom du fichier
      call getarg(1,args(1))          
            
      if      ( index(args(1),'-optimal') .ne. 0 ) then 
        maillage_optimal = .true.	
        call getarg(2,fichier_mif)
        call getarg(3,args(2))
        call getarg(4,args(3))  
        call getarg(5,args(4))		
        goto 11
      else if ( index(args(1),'-virtuel') .ne. 0 ) then 
        maillage_virtuel = .true.
        call getarg(2,fichier_mif)
        call getarg(3,args(2))  
        call getarg(4,args(3))
        call getarg(5,args(4))	
        goto 11
      else if ( index(args(1),'-h'      ) .ne. 0 ) then 
        goto 77
      else
        goto 88      
      end if   	
      
11    continue
      if ( trim(args(4)) .eq. "" ) then 
        goto 88	
      end if
   
      read(args(2),*) brin_dx
      read(args(3),*) dx_recept_ini
      read(args(4),*) cadre_dx       
      
      write(*,'("Largeur de discretisation des brins (m)=",F5.0)') brin_dx
      write(*,'("Pas de la grille de depart (m)=",F5.0)') dx_recept_ini  
      write(*,'("Largeur du cadre (m)=",F6.0)') cadre_dx
      
      go to 99

77    continue

      write(*,*) '#######################################################################'
      write(*,*) '# PREVALP - Generateur de maillage optimal'
      write(*,*) '# Atmo Rhone-Alpes 2009'
      write(*,*) '# version 1.0'
      write(*,*) '# contact: echaxel@atmo-rhonealpes.org'								       
      write(*,*) '#######################################################################'
      write(*,*) ''
      write(*,*) 'BUT     : A partir d un fichier au format MIF, genere un maillage optimal' 
      write(*,*) '          pour un modele de rue de type SIRANE'
      write(*,*) ''      
      write(*,*) 'PRINCIPE: a partir d un grille cartesienne en X/Y a un pas fin determine' 
      write(*,*) '          par l utilisateur dx_recept_ini, '
      write(*,*) '          on elimine les points a distance des brins de circulation' 
      write(*,*) '          pour ne conserver que les points a proximite des axes.'
      write(*,*) '          La loi de progression des distances inter-recepteurs est geometrique de raison 3'            
      write(*,*) ''      
      write(*,*) 'SYNTAXE : mailleur.e -optimal[-virtuel] fichier.MIF brin_dx dx_recept_ini cadre_dx'  
      write(*,*) ''	   
      write(*,*) 'ENTREE  : nom.MIF       = fichier de brins au format MIF'
      write(*,*) '          brin_dx       = longueur de discretisation des brins (m)'
      write(*,*) '          dx_recept_ini = pas de la grille de depart (m)'
      write(*,*) '          cadre_dx      = largeur du cadre autour du domaine (m)'      
      write(*,*) ''
      write(*,*) 'SORTIES : 2 fichiers'
      write(*,*) '          - recept_pts.txt : un fichier X Y Z1 Z2 de maillage optimal '
      write(*,*) '          - brin_pts.txt   : un fichier X Y Z1 Z2 avec la discretisation '
      write(*,*) '                             des axes en points '  
      write(*,*) 'OPTIONS : -virtuel : utilise un seul intervalle de recepteur qui est dx_recept_ini'      
                   
      stop

88    continue

      write(*,*) 'Syntaxe -optimal[-virtuel] nom.MIF brin_dx dx_recept_ini cadre_dx'  
      write(*,*) 'Argument -h/--help pour obtenir de l aide'        
      
      
      stop       
      
99    continue
          
      end subroutine  
