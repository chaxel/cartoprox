      subroutine calcul_brins

      use params

      implicit none
      
      ! local
      integer :: ibrin, ipt, nbrins_selection
      real :: x1,x2,y1,y2
      real :: a_reg,b_reg
            
      ! corps de la routine
      allocate (brin_long(nbrins)   )
      allocate (brin_npts(nbrins)   )      
       
      xmin = 1.E10  
      ymin = 1.E10        
      xmax = -1.E10  
      ymax = -1.E10         
	     	    
      ! calcul la longueur d'un brin ezt les bornes du domaines
      do ibrin=1, nbrins
        x1 = brin_x1(ibrin)
	x2 = brin_x2(ibrin)
	
	y1 = brin_y1(ibrin)
	y2 = brin_y2(ibrin)
	
        brin_long(ibrin) = ( (x2 - x1)**2 +  ( y2 - y1 )**2 )**0.5
	
	if (brin_long(ibrin).gt.10000) write(*,'("Warning: brin > 10km"," brin",I5," x1=",F10.1," x2=",F10.1," y1=",F10.1," y2=",F10.1)') &
	   ibrin,x1,x2,y1,y2
	
	if ( x1.lt. xmin .and. .not. ixmin) xmin = x1
	if ( x2.lt. xmin .and. .not. ixmin) xmin = x2
	if ( x1.gt. xmax .and. .not. ixmax) xmax = x1
	if ( x2.gt. xmax .and. .not. ixmax) xmax = x2
	if ( y1.lt. ymin .and. .not. iymin) ymin = y1
	if ( y2.lt. ymin .and. .not. iymin) ymin = y2
	if ( y1.gt. ymax .and. .not. iymax) ymax = y1
	if ( y2.gt. ymax .and. .not. iymax) ymax = y2
					
      end do
      
      ! utilise un cadre de 3 km
      if(.not.ixmin)xmin = int(xmin) - cadre_dx
      if(.not.ixmax)xmax = int(xmax) + cadre_dx
      if(.not.iymin)ymin = int(ymin) - cadre_dx
      if(.not.iymax)ymax = int(ymax) + cadre_dx
      
      ! definition du domaine avec les centres
      if ( ixc.and.iyc.and.(idx.or.idy)) then
        if (idx.and.(.not.idy)) dy = dx
        if (idy.and.(.not.idx)) dx = dy
	xmin = xc - dx/2.
	xmax = xc + dx/2.	
	ymin = yc - dx/2.
	ymax = yc + dx/2.		
      end if	
                     
      long_max = maxval( brin_long )
      npts_max = int( long_max / brin_dx) + 1 
           	          
      write(*,'("Parametres du domaine")') 
      write(*,'("X(m)=",2F10.1)') xmin,xmax
      write(*,'("Y(m)=",2F10.1)') ymin,ymax
      write(*,'("largeur(m)=",2F10.1)') ymin,ymax      
      write(*,'("longueur_max=",F10.1)') long_max  
      write(*,'("pts par brin max=",I5)') npts_max            
      
      ! calcule la discretisation des brins en points
      ! pour chaque brin on calcule les pts discretises
      allocate (brin_xd(nbrins,npts_max) )
      allocate (brin_yd(nbrins,npts_max) )
        
      open(unit=10,file='brin_pts.txt',status='unknown')
      
      npts = 0
      nbrins_selection=0
      
      do ibrin=1, nbrins
      
        x1 = brin_x1(ibrin)
	x2 = brin_x2(ibrin)
	
	y1 = brin_y1(ibrin)
	y2 = brin_y2(ibrin)
	
	if (((x1.ge.xmin).and.(x1.le.xmax).and.(y1.ge.ymin).and.(y1.le.ymax)).or.&
	((x2.ge.xmin).and.(x2.le.xmax).and.(y2.ge.ymin).and.(y2.le.ymax))) then
	
	nbrins_selection = nbrins_selection + 1
        
	! redefinition des brins
        brin_x1(nbrins_selection) = x1
	brin_x2(nbrins_selection) = x2
	
	brin_y1(nbrins_selection) = y1
	brin_y2(nbrins_selection) = y2
	
	brin_long(nbrins_selection) = brin_long(ibrin)
		    
	! calcul du nombre de points sur le brin	     
        brin_npts(nbrins_selection) = int( brin_long(nbrins_selection) / brin_dx ) + 1
	
	! regression lineaire
	a_reg=(y2-y1)/(x2-x1)
	b_reg = y1 - a_reg * x1
	
	do ipt=1, brin_npts(nbrins_selection)
	   
	   brin_xd(nbrins_selection,ipt) = x1 + (x2-x1)/(brin_npts(ibrin)-1)*(ipt-1)
           brin_yd(nbrins_selection,ipt) = a_reg * brin_xd(ibrin,ipt) + b_reg
           
	   npts = npts + 1	   	   
	   write(10,'(3I8,2F10.1)') npts, ibrin, ipt, brin_xd(nbrins_selection,ipt),brin_yd(nbrins_selection,ipt)	
	
	end do
	
	end if

      end do
      
      close(10)
      
      nbrins = nbrins_selection
      
      write(*,*) 'Selectionne nbrins=',nbrins            
      write(*,*) 'Ecrit brin_pts.txt avec npts=',npts
            
            !en      
      !real, parameter   :: brin_dx = 10. !m 
      !integer  :: (brin_npts(nbrins)   )  
            
      end subroutine
