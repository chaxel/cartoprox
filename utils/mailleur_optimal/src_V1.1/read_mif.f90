      subroutine read_mif

      use params

      implicit none
      
      ! local
      integer :: ios      
      integer, parameter :: nbrins_tmp = 1000000
      real :: x1_tmp(nbrins_tmp)
      real :: y1_tmp(nbrins_tmp) 
      real :: x2_tmp(nbrins_tmp)
      real :: y2_tmp(nbrins_tmp) 
      integer :: nl,np                
      character(len=100) :: ligne  
      integer :: ibrin
      integer :: pline_npoints
      
      integer :: count 
                  
      ! corps de la routine
      open(unit=10, file= fichier_mif, status='old',iostat=ios)      
      if (ios.eq.0) then      
        write(*,*) 'Ouverture de '//trim(fichier_mif)//' OK'    
      end if
      
      ! c'est parti pour la lecture
      ! boucle une premiere fois pour connaitre le nombre de brins
      
      ibrin=0
      
      count = 0
      
      do nl = 1, 100000000
        !read(10,'(A20)',end=99,err=98) ligne(1:20)
	!write(*,*)ligne
        count = count + 1
	read(10,'(A100)',end=99,err=98) ligne(1:100)
	
	if ( index(ligne(1:5),'Pline').ne.0) then
	
	  !write(*,*) index(ligne,'Pline'),ligne
	  read(ligne(6:20),*) pline_npoints
	  
	  if ( pline_npoints.eq.2 ) then
	  do np=1,pline_npoints-1
	    ibrin = ibrin + 1
	    count = count + 1
	    read(10,*,end=99,err=98) x1_tmp(ibrin), y1_tmp(ibrin)
	    count = count + 1
	    read(10,*,end=99,err=98) x2_tmp(ibrin), y2_tmp(ibrin)
	    !write(*,'("Segment ",I5," y1=",F10.1," y1=",F10.1," x2=",F10.1," y2=",F10.1)') &
	    !ibrin, x1_tmp(ibrin), y1_tmp(ibrin)	, x2_tmp(ibrin), y2_tmp(ibrin) 	    
	  end do
	  else
	    do np=1,pline_npoints-1
	      ibrin = ibrin + 1
	      count = count + 1
	      read(10,*,end=99,err=98) x1_tmp(ibrin), y1_tmp(ibrin)
	      read(10,*,end=99,err=98) x2_tmp(ibrin), y2_tmp(ibrin)
	      if(np.ne.pline_npoints-1)backspace(10)
	    end do
	    count = count + 1
	  end if	  

	else if ( index(ligne,'Line').ne.0) then
	
	  pline_npoints = 2
	  ibrin = ibrin + 1	 
	  read(ligne(6:100),*) x1_tmp(ibrin), y1_tmp(ibrin), x2_tmp(ibrin), y2_tmp(ibrin)
	  !  write(*,'("Segment ",I5," y1=",F10.1," y1=",F10.1," x2=",F10.1," y2=",F10.1)') &
	  !  ibrin, x1_tmp(ibrin), y1_tmp(ibrin)	, x2_tmp(ibrin), y2_tmp(ibrin) 
	end if   
      
      end do
      
98    continue
      write(*,*) 'Erreur de lecture de '//trim(fichier_mif)//' ligne',count
      write(*,*) 'ligne='//trim(ligne)
      stop

99   continue

      nbrins = ibrin
      
      write(*,'("Trouve brins dans fichier MIF:",I10)') nbrins
      
      if (nbrins.eq.0)   then
        nbrins = 1
        x1_tmp(nbrins)=-9.999E3
        y1_tmp(nbrins)=-9.999E3
        x2_tmp(nbrins)=-9.999E3+1
        y2_tmp(nbrins)=-9.999E3+1
      end if
            
      allocate (brin_x1(nbrins)   )
      allocate (brin_y1(nbrins)   )  
      allocate (brin_x2(nbrins)   )
      allocate (brin_y2(nbrins)   )
      
      brin_x1 = x1_tmp(1:nbrins)
      brin_y1 = y1_tmp(1:nbrins)
      brin_x2 = x2_tmp(1:nbrins)
      brin_y2 = y2_tmp(1:nbrins)
            
      close(10)
            
      end subroutine
