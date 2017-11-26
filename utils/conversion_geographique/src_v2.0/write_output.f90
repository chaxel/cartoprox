!-------------------------------------------------------------      
      subroutine write_output
      
      use params

      implicit none 
      
      integer :: ipt     
      
      ! ecrit les parametre de grille 
      write(*,*) '-> Ecrit '//trim(fichier_recept)  
      open(unit=10,file=trim(fichier_recept)  ,status='unknown')
      do ipt=1, npts   
        write(10,*) recept_x(ipt),recept_y(ipt)
      end do				 
      close(10) 
                       
      end subroutine write_output         
