      subroutine calcul_brins

      use params

      implicit none
      
      ! local
      integer :: ibrin, ipt, nbrins_selection
      real    :: x1,x2,y1,y2
      real    :: a_reg,b_reg 
      
      ! calcule la discretisation des brins en points
      ! pour chaque brin on calcule les pts discretises
      allocate (brin_npts(nbrins)   )        

      ! corps de la routine
      allocate (brin_long(nbrins)   )    

      ! calcul de la longueur des brins
      do ibrin=1, nbrins  

        x1 = brin_x1(ibrin)
	x2 = brin_x2(ibrin)
	
	y1 = brin_y1(ibrin)
	y2 = brin_y2(ibrin)      
          
        brin_long(ibrin) = ( (x2 - x1)**2 +  ( y2 - y1 )**2 )**0.5      
	if (brin_long(ibrin).gt.10000) write(*,'("Warning: brin > 10km"," brin",I5," x1=",F10.1," x2=",F10.1," y1=",F10.1," y2=",F10.1)') &
	   ibrin,x1,x2,y1,y2
      end do	
               
      long_max = maxval( brin_long )
      npts_max = int( long_max / brin_dx) + 1 
      write(*,'("Parametres des brins")')       
      write(*,'("longueur_max(m)=",F10.1)') long_max  
      write(*,'("pts par brin max=",I5)') npts_max               

      ! allocations
      allocate (brin_xd       (nbrins,npts_max) )
      allocate (brin_yd       (nbrins,npts_max) )
      allocate (brin_xd_cart  (nbrins,npts_max) )
      allocate (brin_yd_cart  (nbrins,npts_max) )      
      
      ! selection des brins dans le domaine  
      !open(unit=10,file='brin_pts.txt',status='unknown') ! dans write_output.f90 (v1.03)  
      
      npts = 0
      nbrins_selection=0

      write(*,*) 'Conversion geographique en cours...' 
      write(*,*) projection_i, geoid_i
      write(*,*) projection_o, geoid_o 
      
      do ibrin=1, nbrins
      
        x1 = brin_x1(ibrin)
	x2 = brin_x2(ibrin)
	
	y1 = brin_y1(ibrin)
	y2 = brin_y2(ibrin)
	
	if (((x1.ge.xmin).and.(x1.le.xmax).and.(y1.ge.ymin).and.(y1.le.ymax)).or.&
	((x2.ge.xmin).and.(x2.le.xmax).and.(y2.ge.ymin).and.(y2.le.ymax)).or.&
	((x1.lt.0.).and.(x2.lt.0.).and.(y1.lt.0.).and.(y2.lt.0.).and.(nbrins.eq.1)) &  ! aucun brin
	  ) then
	
	nbrins_selection = nbrins_selection + 1
        
	! redefinition des brins
        brin_x1(nbrins_selection) = x1
	brin_x2(nbrins_selection) = x2
	
	brin_y1(nbrins_selection) = y1
	brin_y2(nbrins_selection) = y2
	
	brin_long(nbrins_selection) = brin_long(ibrin)
		    
	! calcul du nombre de points sur le brin DISCRETISATION	     
        brin_npts(nbrins_selection) = int( brin_long(nbrins_selection) / brin_dx ) + 1
	
	! regression lineaire
	!if ( x1 .eq. x2 ) then
	!  a_reg=-99999999
	!else
	!  a_reg=(y2-y1)/(x2-x1)
	!end if	
	!b_reg = y1 - a_reg * x1
	
	!write(*,*) 'reg',	a_reg, b_reg	
	
	do ipt=1, brin_npts(nbrins_selection)
	   
	   !Simplifie le calcul (version 1.03)
	   if ( brin_npts(nbrins_selection).eq.1) then ! brins courts (v1.11)
	     brin_xd(nbrins_selection,ipt) = (x1 + x2) / 2.
             brin_yd(nbrins_selection,ipt) = (y1 + y2) / 2.
	   else
	     brin_xd(nbrins_selection,ipt) = x1 + (x2-x1)/(brin_npts(nbrins_selection)-1)*(ipt-1)
	     brin_yd(nbrins_selection,ipt) = y1 + (y2-y1)/(brin_npts(nbrins_selection)-1)*(ipt-1)
           end if
	   !if ( a_reg.lt.-9999999999) then
	   !  brin_yd(nbrins_selection,ipt) = a_reg * brin_xd(nbrins_selection,ipt) + b_reg
	   !else
	   !  brin_yd(nbrins_selection,ipt) = a_reg * brin_xd(nbrins_selection,ipt) + b_reg
	   !end if             	   
	   npts = npts + 1
	   
	   ! Conversion Lambert -> UTM (version 1.03)
	   brin_xd_cart(nbrins_selection,ipt) = brin_xd(nbrins_selection,ipt)
           brin_yd_cart(nbrins_selection,ipt) = brin_yd(nbrins_selection,ipt)	   	   
	  
	   !write(*,*)brin_xd_cart(nbrins_selection,ipt),brin_yd_cart(nbrins_selection,ipt)
	   !call conversion_geo(brin_xd(nbrins_selection,ipt),brin_yd(nbrins_selection,ipt)) !Version 1.11
	   call conversion_geo_inv(brin_xd_cart(nbrins_selection,ipt),brin_yd_cart(nbrins_selection,ipt))
           
	   ! dans write_output.f90 (v1.03)   	   
	   !write(10,'(3I8,2F10.1)') npts, ibrin, ipt, brin_xd(nbrins_selection,ipt),brin_yd(nbrins_selection,ipt)	
	
	end do
	
	end if

      end do
      
      !close(10) ! dans write_output.f90 (v1.03)  
      
      nbrins = nbrins_selection
      
      write(*,*) 'Selection dans le domaine de calcul nbrins=',nbrins            
      write(*,*) 'Ecrit brin_pts.txt avec npts=',npts
      
      ! VERSION 1.2
      if (nbrins.eq.0) then
        write(*,*) '***INFO: Aucun brin dans le domaine de calcul -> MODE SECOURS'
	write(*,*) '***INFO: dx_recept_ini(m)=',dx_recept_max
        dx_recept_ini = dx_recept_max
	igrille_secours=.true.
      else
        igrille_secours=.false.
      end if
            
            !en      
      !real, parameter   :: brin_dx = 10. !m 
      !integer  :: (brin_npts(nbrins)   )  
            
      end subroutine
