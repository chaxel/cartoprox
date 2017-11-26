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

      ! local
      
      ! utilise un cadre de 3 km
      if(ixmin)xmin = xmin - cadre_dx
      if(ixmax)xmax = xmax + cadre_dx
      if(iymin)ymin = ymin - cadre_dx
      if(iymax)ymax = ymax + cadre_dx
      
      ! definition du domaine avec les centres
      if ( ixc.and.iyc.and.(idx.or.idy)) then
        if (idx.and.(.not.idy)) dy = dx
        if (idy.and.(.not.idx)) dx = dy
	
	! UTM
	xmin_cart = xc1 - dx/2.
	xmax_cart = xc1 + dx/2. 
	ymin_cart = yc1 - dx/2.
	ymax_cart = yc1 + dx/2.

        ! Lambert 2 etendu = domaine calcul SIRANE
	! le cadre_dx sert a inclure tous les recepteurs dans le domaine Lambert 2 SIRANE
	xmin = xc - dx/2. - cadre_dx
	xmax = xc + dx/2. + cadre_dx	
	ymin = yc - dx/2. - cadre_dx
	ymax = yc + dx/2. + cadre_dx
      end if
      
      call print_domaine      
      
      end subroutine user_domaine
!-----------------------------------------------------------------------            
      subroutine auto_domaine
      
      use params

      implicit none

      ! local
      integer :: ibrin
      
      real    :: x1,x2,y1,y2
      
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
      
      if(.not. ixmin)xmin = xmin - cadre_dx
      if(.not. iymin)xmax = xmax + cadre_dx    
      if(.not. ixmax)ymin = ymin - cadre_dx
      if(.not. iymax)ymax = ymax + cadre_dx      
      
      if(.not. ixmin)xmin_cart = xmin
      if(.not. iymin)xmax_cart = xmax
      if(.not. ixmax)ymin_cart = ymin
      if(.not. iymax)ymax_cart = ymax 
      
      
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
      nx = (xmax_cart-xmin_cart) / dx_recept_ini
      ny = (ymax_cart-ymin_cart) / dx_recept_ini   

      ! defini le nombre de points initials
      nrecept = nx * ny
      
      allocate (recept_x          (nx,ny))
      allocate (recept_y          (nx,ny))

      allocate (recept_x1         (nx,ny))
      allocate (recept_y1         (nx,ny))      
      
      allocate (recept_valid      (nx,ny)) 
      allocate (recept_dist_pt    (nx,ny))
      allocate (recept_dist_recept(nx,ny))
      
      !Initialisations
      recept_dist_pt = 0.
      recept_dist_recept = 0.
      recept_valid = .true.
            
      ! initialisation des recepteurs en coordonnées UTM/geo/lambert 2
      write(*,'("Grille ",I4, " m dimensions",I6," x ",I6," soit ",I8, " m x",I8," m")') &
      int(dx_recept_ini),nx, ny,int(nx*dx_recept_ini),int(ny*dx_recept_ini)
      write(*,'("Points grille avant optimisation ",I12)')   nrecept
      do iy=1,ny      
        do ix=1,nx
	  recept_x1(ix,iy) = xmin_cart + (ix-1) * dx_recept_ini + 0.5 * dx_recept_ini
	  recept_y1(ix,iy) = ymin_cart + (iy-1) * dx_recept_ini + 0.5 * dx_recept_ini
	  recept_x(ix,iy)  = recept_x1(ix,iy)
	  recept_y(ix,iy)  = recept_y1(ix,iy)	  	  	  	  	  
	end do
      end do
      
      ! projection des recepteurs en coordonnées UTM/geo/lambert 2 -> Lambert 2
      if (projection_o.ne.projection_i) then
        write(*,*) '-> Projection '//trim(geoidstr_i)//' -> Lambert '//trim(geoidstr_o)	
        do iy=1,ny      
          do ix=1,nx            
             call conversion_geo(recept_x(ix,iy),recept_y(ix,iy))
	  end do
        end do
      else
        write(*,*) '*** info : grille entree/sortie en '//trim(geoidstr_i)//'-> pas de projection'	
      end if

      end subroutine calcul_grille 
