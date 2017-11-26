      subroutine read_ascii

      use params

      implicit none
      
      ! local
      integer :: ios      
      integer, parameter :: npts_tmp = 10000000
      double precision :: x1_tmp(npts_tmp)
      double precision :: y1_tmp(npts_tmp)             
      integer :: ipt
                  
      ! corps de la routine
      open(unit=10, file= fichier_ascii, status='old',iostat=ios)      
      if (ios.eq.0) then      
        write(*,*) 'Ouverture de '//trim(fichier_ascii)//' OK'    
      end if
      
      ! c'est parti pour la lecture
      ! boucle une premiere fois pour connaitre le nombre de brins
      
      ipt=0
      
      do ipt = 1, npts_tmp      
        read(10,*,end=99) x1_tmp(ipt), y1_tmp(ipt)
      end do
99    continue

      if ( ipt .ge. npts_tmp) then
         write(*,*) 'Augmente la valeur de npts_tmp dansread_ascii.f90'   
	 stop 1   
      end if

      npts = ipt - 1
      
      write(*,'("Trouve points:",I8)') npts
                 
      allocate (recept_x1(npts)   )
      allocate (recept_y1(npts)   ) 

      allocate (recept_x(npts)   )
      allocate (recept_y(npts)   )        
      
      recept_x1 = x1_tmp(1:npts)
      recept_y1 = y1_tmp(1:npts)
      recept_x  = x1_tmp(1:npts)
      recept_y  = y1_tmp(1:npts)      
            
      close(10)
            
      end subroutine read_ascii
