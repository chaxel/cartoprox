      subroutine write_output
      
      use params

      implicit none 

      ! local      
      integer :: ibrin, ipt, ic, iter, ix, iy           

      write(*,*) 'Ecrit '//trim(fichier_recept)//' avec nrecept=',recept_valid_n
      
      ! ecrit le resultats X_lamb2, Y_lamb2, X_UTM31, Y_UTM31,distance_l_axe,distance_point
      open(unit=10,file=trim(fichier_recept),status='unknown')
      ipt = 0
      do iy=1, ny
       do ix=1, nx
        if ( recept_valid(ix,iy) ) then
	ipt = ipt + 1
        write(10,'(I8,4F12.1,2F12.2)') &
	  (iy-1)*nx+ix,recept_x(ix,iy),recept_y(ix,iy),recept_x1(ix,iy),recept_y1(ix,iy),recept_dist_pt(ix,iy),recept_dist_recept(ix,iy)
	end if
       end do
      end do
      close(10) 
        
      ! ecrit les resultats grille si dx_recept_ini>50 m (DEBUG)
      if ( dx_recept_ini.ge.50. ) then     
      write(*,*) 'Ecrit recept_pts_all.txt avec nrecept=',nx*ny
   
      open(unit=10,file='recept_pts_all.txt',status='unknown')
      do iy=1, ny
       do ix=1, nx
        write(10,'(I8,4F12.1,2F12.2)') &
	  (iy-1)*nx+ix,recept_x(ix,iy),recept_y(ix,iy),recept_x1(ix,iy),recept_y1(ix,iy),recept_dist_pt(ix,iy),recept_dist_recept(ix,iy)
       end do
      end do
      close(10)    
      end if
                 
      end subroutine
!-------------------------------------------------------------      
      subroutine write_domaine
      
      use params

      implicit none       
      
      ! ecrit les parametre de grille 
      write(*,*) '-> Ecrit '//trim(fichier_grille)  
      open(unit=10,file=trim(fichier_grille)  ,status='unknown')
        write(10,'("xc  ",F12.2)') xc
        write(10,'("yc  ",F12.2)') yc
        write(10,'("dx  ",F12.2)') dx+cadre_dx*2
        write(10,'("dy  ",F12.2)') dy+cadre_dx*2
        write(10,'("xmin",F12.2)') xmin
        write(10,'("xmax",F12.2)') xmax
        write(10,'("ymin",F12.2)') ymin
        write(10,'("ymax",F12.2)') ymax 
	
        write(10,'("xc1  ",F12.2)') xc1
        write(10,'("yc1  ",F12.2)') yc1
        write(10,'("dx   ",F12.2)') dx
        write(10,'("dy   ",F12.2)') dy	
        write(10,'("xmin1",F12.2)') xmin_cart
        write(10,'("xmax1",F12.2)') xmax_cart
        write(10,'("ymin1",F12.2)') ymin_cart
        write(10,'("ymax1",F12.2)') ymax_cart	
				 
      close(10) 
                       
      end subroutine write_domaine
      
         
