      subroutine calcul_recept
      
      !version 1.2: boucle sur les classes de recepteurs

      use params

      implicit none
      
      ! local
      integer :: ibrin, ipt, ic, iter, ix, iy
      integer :: i, j
      integer :: i1vois, j1vois, i2vois, j2vois         
      
      real :: x1,y1,x2,y2,x3,y3,x_cart,y_cart
      real :: a_reg,b_reg
      real :: d, d2p
      real :: dist_recept_recept
      real :: dist_recept_pt
      real :: a, b
      
      integer :: irecept
      
      integer :: niter
      
      !real, allocatable :: recept_y_temp(:)
      !real, allocatable :: recept_x_temp(:)      
      !integer :: nrecept_tmp
            
      integer :: recept_suppr_n 
      integer :: y_suppr_n       
      integer :: iter_suppr_n 
      
      real    :: dist_recept_pt_max 
      integer :: nbpts 
      !integer :: iter_ntest, iter_ntest_max
      
      !calcul de la distance max � l'axe
      ! calcule la distance du recepteur au brin (ibrin,ipt) le plus proche 
      write(*,*)'Calcul distance aux brins max...'
      dist_recept_pt_max = 0.  
      do ix=1, nx 
      do iy=1, ny
      x1 = recept_x(ix,iy)
      y1 = recept_y(ix,iy)
      dist_recept_pt = 1.E12	
      do ibrin= 1, nbrins
        !methode rapide: utilise le centre du brin 
        !do ipt= 1, brin_npts(ibrin)
          nbpts = brin_npts(ibrin)
	  !x2 = brin_xd(ibrin,ipt)
          !y2 = brin_yd(ibrin,ipt)
	  x2 = (brin_xd(ibrin,1)+brin_xd(ibrin,nbpts))/2.
          y2 = (brin_yd(ibrin,1)+brin_yd(ibrin,nbpts))/2.
          d  = d2p(x1,y1,x2,y2)
          if (d.lt.dist_recept_pt )dist_recept_pt = d
        !end do
      end do
      if(dist_recept_pt.gt.dist_recept_pt_max)dist_recept_pt_max = dist_recept_pt
      end do
      end do
      write(*,*)'dist_recept_pt_max(m)=',dist_recept_pt_max 
      
      !initialise le nombre de classes
      nclass_recept = nclass_recept_defaut
      
      allocate( dy_recept(nclass_recept))
      allocate( dx_recept(nclass_recept))
      allocate( r_dy_recept(nclass_recept))
      
      write(*,*) 'Calcul des distances inter-recepteur'
      ! genere les distances inter-recepteurs
      ! la plus petite correspond � l'espacement de la grille
      dy_recept(1) = dx_recept_ini
      dx_recept(1) = dx_recept_ini * (sqrt(3.)/2)
      if  (maillage_virtuel) then
        r_dy_recept(1) = 1.
      else
        r_dy_recept(1) = facteur_geom
      end if
      
      if (dy_recept_max.lt.0.)dy_recept_max = dy_recept_max_defaut
      
      !defini la distance inter-recepteurs dy_recept
      ! ensuite suit geometrique de raison 2^n
      do ic = 2, nclass_recept_defaut
	if  (maillage_virtuel) then
	  r_dy_recept(ic) = 1.
	else
          r_dy_recept(ic) = facteur_geom !r_dy_recept(ic-1) * 2.
	end if
	dy_recept(ic) = dy_recept(ic-1) * r_dy_recept(ic)
	!Borne la valeur de l'ecart avec la diagonale d'une maille de dx = dy_recept_max
	!if (dy_recept(ic).gt.(2**.5)*dy_recept_max)dy_recept(ic)=(2**.5)*dy_recept_max
	if (dy_recept(ic).gt.dy_recept_max) then
	  dy_recept(ic) = dy_recept_max
	  go to 888
	end if
	
      end do            
      go to 777

888   continue
      nclass_recept = ic
      
777   continue      
                
      !defini la relation distance au brin dx_recept
      !dx_recept(1) =  dy_recept(1) !* sqrt(3.)/2.
      do ic = 2, nclass_recept
        dx_recept(ic) = dx_recept(ic-1) + dy_recept(ic)/(sqrt(3.)/2.)/2. + dy_recept(ic-1)/(sqrt(3.)/2.)/2.
	if (dx_recept(ic).gt.dist_recept_pt_max) then
	  go to 8888
	end if  	
      end do
      go to 7777

8888  continue
      nclass_recept = ic

7777  continue  
      
      ! affichage
      do ic = 1, nclass_recept
	  write(*,'(I3," Distance voie dx_recept(m)=",F10.2," distance inter-recepteur dy_recept(m)=",F10.2)') ic,dx_recept(ic),dy_recept(ic)
      end do
	     
      ! c'est parti pour l'optimisation
      
      ! 1. regarde a quel distance se trouve un point des autres et de la chaussee   
      ! 2. supprime tous les points d'un voisinage qui sont plus pres que la distance admise (dy_recept)  
      
      ! avec la methode de la suppression par voisinage, bouble sur les N classes de recepteurs (version 1.2)
      niter = nclass_recept
      
      ! initialisation
      recept_valid   = .true.
      recept_valid_n = nrecept
      recept_suppr_n = 0      
     
      ! boucle du plus grand au plus petit
      do iter = 1, niter  
      
      iter_suppr_n = 0
      
      if (iter.ge.2) &
        write(*,'("Iteration ",I2," sur ",I2," pts (K) ",I10," pts suppr ",I9,F8.2," %" )') &
	  iter,niter,recept_valid_n,recept_suppr_n,1.0*recept_valid_n/nrecept*100.
    
      do ix=1, nx 
      do iy=1, ny
       
      irecept = (ix-1)*ny   + iy
      !irecept = (iy-1)*nx   + ix            
      
      if (mod(irecept,int(nx*ny/10.)).eq. 1 ) then  ! tous les 10%    
        write(*,'("Iteration ",I2," sur ",I2," pts=",I10," pts suppr=",I9,I4," %")') &
	    iter,niter,recept_valid_n,recept_suppr_n,int(100.*irecept/(nx*ny))
	recept_suppr_n = 0
      end if    
      
      if ( recept_valid(ix,iy) ) then

	! on se place a un point, on va enlever tous les points qui se trouvent � une distance inferieure a ce point	
	
	! coordonnees non cartesiennes
	x1 = recept_x(ix,iy)
	y1 = recept_y(ix,iy)  
	
	! coordonnees cartesiennes	
	x_cart = recept_x1(ix,iy)
	y_cart = recept_y1(ix,iy)   
	
	! calcule la distance du recepteur au brin (ibrin,ipt) le plus proche 
	dist_recept_pt = 1.E12	
	do ibrin= 1, nbrins     
	 do ipt= 1, brin_npts(ibrin)
	  x2 = brin_xd(ibrin,ipt)
	  y2 = brin_yd(ibrin,ipt)
	  d = d2p(x1,y1,x2,y2)	
	  !write(*,*) d  
	  if ( d .lt. dist_recept_pt ) dist_recept_pt = d
	 end do
	end do
	
	!version 1.2
	!si distance < dx_recept(ic) -> SKIP
	if(dist_recept_pt.lt.dx_recept(nclass_recept-iter+1))go to 99
	
	recept_dist_pt(ix,iy) = dist_recept_pt
	
	if ( dist_recept_pt.gt.1.E11) then
	  write(*,*) 'Erreur dans calcul de dist_recept_pt'
	  stop  
	end if
		
	! MAILLAGE VIRTUEL
	! point a proximite directe de l'axe < dy_recept(1) / 4
	if  ( dist_recept_pt .lt. dy_recept(1) / 4. ) then	
	  recept_valid(ix,iy) = .false.
	  recept_valid_n = recept_valid_n - 1
          recept_suppr_n = recept_suppr_n + 1	  
	  go to 99
	end if
	
	! calcul de la distance inter-recepteurs			
	! plus loin que x km
	if ( dist_recept_pt.ge.dx_recept(nclass_recept) ) then 
	  dist_recept_recept = dy_recept(nclass_recept)
	else if ( dist_recept_pt.le.dx_recept(1) ) then        
	  dist_recept_recept = dy_recept(1)
	else  	
	  ! dans intervalle
	  do ic = 1, nclass_recept-1
	    if ( ( dist_recept_pt.gt.dx_recept(ic)) .and.( dist_recept_pt.le.dx_recept(ic+1)     )) then
	      !a = dist_recept_pt  -  dx_recept(ic  )
	      !b = dx_recept(ic+1) -  dist_recept_pt 
	      !dist_recept_recept =  min( a + dy_recept(ic),  ( a * dy_recept(ic+1) + b * dy_recept(ic) ) / ( a + b ))
	      dist_recept_recept =  dy_recept(ic)
	      go to 77
	    end if
          end do		
	end if

77      continue

	recept_dist_recept(ix,iy) = dist_recept_recept

	if ( (dist_recept_recept.lt.0).or.(dist_recept_recept.gt.dx_recept(nclass_recept))) then
	  write(*,*) 'Erreur dans calcul de dist_recept_recept'
	  stop  
	end if

	! si un pt recepteur est plus proche que dist_recept_recept, on le supprime...
	
	! Attention pour le calcul du voisinage : se place maillage cartesien
	
	! travaille sur un voisinage pour la recherche des points a supprimer
	i1vois = max( 1,int((x_cart - 1.01 * dist_recept_recept - xmin_cart) /  dx_recept_ini) + 1)
	i2vois = min(nx,int((x_cart + 1.01 * dist_recept_recept - xmin_cart) /  dx_recept_ini) + 1)
	
	j1vois = max( 1,int((y_cart - 1.01 * dist_recept_recept - ymin_cart) /  dx_recept_ini) + 1)
	j2vois = min(ny,int((y_cart + 1.01 * dist_recept_recept - ymin_cart) /  dx_recept_ini) + 1)
	
	do i= i1vois, i2vois
	  do j= j1vois, j2vois
	   if ( recept_valid(i,j) ) then
	    x3 = recept_x(i,j)
	    y3 = recept_y(i,j)
	    d = d2p(x1,y1,x3,y3)
	    if ( (( i.ne.ix).or.(j.ne.iy)).and.( d.lt.dist_recept_recept ) ) then
	      ! supprime ce point
	      recept_valid_n = recept_valid_n - 1
              recept_suppr_n = recept_suppr_n + 1 ! compte le nb de suppression sur le domaine   
	      recept_valid(i,j) = .false.
	      !write(*,'("(ix,iy)=",2I6," (x,y)=",2F9.0," Supprime (i,j)=",2I6," d=",F8.1," dist_recept_pt=",F8.1," dist_recept_recept=",F8.1)')&
	      !   ix,iy,x1,y1,i,j,d,dist_recept_pt,dist_recept_recept
	     end if	   
	   end if
	  end do ! i
      end do !j     

      end if ! valid  
      
99    continue	
                
      end do !ix
      end do !iy
      
      ! on regarde si l'iteration a supprim� des points
!      iter_suppr_n = 0
      
      !if (iter_suppr_n.eq.0) goto 999

98    continue
                         
      end do ! iter
      
999   continue

      write(*,*) 'Conserve recepteurs:', recept_valid_n
                                   
      end subroutine
      
!----------------------
      real function d2p(x1,y1,x2,y2)      
      implicit none      
      real :: x1, y1, x2, y2      
      d2p = ( ( x2 - x1 )**2 +  ( y2 - y1 )**2 ) **0.5            
      end function
