      subroutine calcul_brins

      use params

      implicit none
      
      ! local
      integer :: ibrin, ipt
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
	
	if ( x1.lt. xmin ) xmin = x1
	if ( x2.lt. xmin ) xmin = x2
	if ( x1.gt. xmax ) xmax = x1
	if ( x2.gt. xmax ) xmax = x2
	if ( y1.lt. ymin ) ymin = y1
	if ( y2.lt. ymin ) ymin = y2
	if ( y1.gt. ymax ) ymax = y1
	if ( y2.gt. ymax ) ymax = y2
					
      end do
      
      ! utilsie un cadre de 3 km
      xmin = int(xmin) - cadre_dx
      xmax = int(xmax) + cadre_dx
      ymin = int(ymin) - cadre_dx
      ymax = int(ymax) + cadre_dx
       
      long_max = maxval( brin_long )
      npts_max = int( long_max / brin_dx) + 1 
           	          
      write(*,'("Parametres du domaine")') 
      write(*,'("X=",2F10.1)') xmin,xmax
      write(*,'("Y=",2F10.1)') ymin,ymax
      write(*,'("longueur_max=",F10.1)') long_max  
      write(*,'("pts par brin max=",I5)') npts_max            
      
      ! calcule la discretisation des brins en points
      ! pour chaque brin on calcule les pts discretises
      allocate (brin_xd(nbrins,npts_max) )
      allocate (brin_yd(nbrins,npts_max) )
        
      open(unit=10,file='brin_pts.txt',status='unknown')
      
      npts = 0
      
      do ibrin=1, nbrins
      
        x1 = brin_x1(ibrin)
	x2 = brin_x2(ibrin)
	
	y1 = brin_y1(ibrin)
	y2 = brin_y2(ibrin)    
      
        brin_npts(ibrin) = int( brin_long(ibrin) / brin_dx ) + 1
	
	! regression lineaire
	a_reg=(y2-y1)/(x2-x1)
	b_reg = y1 - a_reg * x1
	
	do ipt=1, brin_npts(ibrin)
	   
	   brin_xd(ibrin,ipt) = x1 + (x2-x1)/(brin_npts(ibrin)-1)*(ipt-1)
           brin_yd(ibrin,ipt) = a_reg * brin_xd(ibrin,ipt) + b_reg
         
	   npts = npts + 1	   
	   
	   write(10,'(3I8,2F10.1)') npts, ibrin,ipt,brin_xd(ibrin,ipt),brin_yd(ibrin,ipt)	
	
	end do

      end do
      
      close(10)
      
      write(*,*) 'Ecrit brin_pts.txt avec npts=',npts
            
            !en      
      !real, parameter   :: brin_dx = 10. !m 
      !integer  :: (brin_npts(nbrins)   )  
            
      end subroutine
