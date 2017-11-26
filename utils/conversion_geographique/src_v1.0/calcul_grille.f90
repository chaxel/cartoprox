      subroutine print_domaine
      
      use params

      implicit none

      write(*,'("Parametres du domaine MAILLEUR")') 
      write(*,'("Xmin,Xmax(m)=",2F10.1)') xmin,xmax
      write(*,'("Ymin,Ymax(m)=",2F10.1)') ymin,ymax
      write(*,'("Xc,Yc(m)=",2F10.1)') xc,yc
      write(*,'("largeur x hauteur(m)=",2F10.1)') dx,dy        	
     	          
      end subroutine print_domaine
!----------------------------------------------------------------------- 
      subroutine user_domaine
      
      use params

      implicit none

      ! definition du domaine avec les centres
      if ( ixc.and.iyc.and.(idx.or.idy)) then
        if (idx.and.(.not.idy)) dy = dx
        if (idy.and.(.not.idx)) dx = dy	
      end if
      
      call print_domaine      
      
      end subroutine user_domaine
!-----------------------------------------------------------------------            
      subroutine auto_domaine
      
      use params

      implicit none

      ! local
      integer :: ibrin
      
      double precision    :: x1,x2,y1,y2
      
      write(*,*) '-> Grille AUTO active sur nbrins=',nbrins
       
      if(.not. ixmin)xmin = 1.E10  
      if(.not. iymin)ymin = 1.E10	 
      if(.not. ixmax)xmax = -1.E10  
      if(.not. iymax)ymax = -1.E10         
	     	    
      ! calcul les bornes du domaines si elles ne sont pas fournies par l'utilisateur
      do ibrin=1, nbrins
      
        x1 = brin_x1(ibrin)
	x2 = brin_x2(ibrin)
	
	y1 = brin_y1(ibrin)
	y2 = brin_y2(ibrin)
			
	if ( x1.lt. xmin .and. (.not. ixmin)) xmin = x1
	if ( x2.lt. xmin .and. (.not. ixmin)) xmin = x2
	if ( x1.gt. xmax .and. (.not. ixmax)) xmax = x1
	if ( x2.gt. xmax .and. (.not. ixmax)) xmax = x2
	
	if ( y1.lt. ymin .and. (.not. iymin)) ymin = y1
	if ( y2.lt. ymin .and. (.not. iymin)) ymin = y2
	if ( y1.gt. ymax .and. (.not. iymax)) ymax = y1
	if ( y2.gt. ymax .and. (.not. iymax)) ymax = y2
					
      end do
      
      if (.not.igrilleuser) xc = ( xmax + xmin ) / 2.
      if (.not.igrilleuser) yc = ( ymin + ymax ) / 2.
      
      if(.not. ixmin)xmin = xmin
      if(.not. iymin)xmax = xmax
      if(.not. ixmax)ymin = ymin
      if(.not. iymax)ymax = ymax
      
      if(.not. ixmin)xmin1 = xmin
      if(.not. iymin)xmax1 = xmax
      if(.not. ixmax)ymin1 = ymin
      if(.not. iymax)ymax1 = ymax 
      
      
      dx =  ymax - ymin
      dy =  ymax - ymin
                             
      end subroutine auto_domaine
!-----------------------------------------------------------------------            
      subroutine calcul_grille
      
      use params

      implicit none 
      
      ! local
      integer :: ix, iy           
      
      ! cree la grille de sortie a partir de l'ecart minimum entre 2 recepteur
      nx = (xmax-xmin) / dx
      ny = (ymax-ymin) / dx

      ! defini le nombre de points initials
      nrecept = nx * ny
            
      ! initialisation des recepteurs en coordonnées UTM/geo/lambert 2
      write(*,'("Grille ",I3, " m dimensions",I6," x ",I6," soit ",I8, " m x",I8," m")') &
      int(dx_recept_ini),nx, ny,int(nx*dx_recept_ini),int(ny*dx_recept_ini)
      write(*,'("Points grille avant optimisation ",I12)')   nrecept
      do iy=1,ny      
        do ix=1,nx
	  recept_x1(ix,iy) = xmin1 + (ix-1) * dx
	  recept_y1(ix,iy) = ymin1 + (iy-1) * dx
	  recept_x(ix,iy)  = recept_x1(ix,iy)
	  recept_y(ix,iy)  = recept_y1(ix,iy)	  	  	  	  	  
	end do
      end do
      
      ! projection des recepteurs en coordonnées UTM/geo/lambert 2 -> Lambert 2
      if (projection_o.ne.projection_i) then
        write(*,*) '-> Projection '//trim(geoidstr_i)//' -> '//trim(geoidstr_o)	
        do iy=1,ny      
          do ix=1,nx            
             call conversion_geo(recept_x(ix,iy),recept_y(ix,iy))
	  end do
        end do
      else
        write(*,*) '*** info : grille entree/sortie en '//trim(geoidstr_i)//'-> pas de projection'	
      end if

      end subroutine calcul_grille 
!-----------------------------------------------------------------------            
