   subroutine include_grille_region

      use netcdf
      use typesizes 
      use params
      
      implicit none   

!--------------------------------------------------------------------                       
! Ecrite les id_maille
      allocate( id_maille(nx1,ny1) ) 
      do j1=1,ny1
        do i1=1,nx1  
          id_maille(i1,j1) = ( j1 - 1 ) * nx1 + i1
        end do
      end do

! Ecrite les id_maille Vertical Mapper      
      allocate( id_maille_vm(nx1,ny1) ) 
      do j1=1,ny1
        do i1=1,nx1  
          id_maille_vm(i1,j1) = ( ny1 - j1 ) * nx1 + i1
        end do
      end do   

      open( unit=10,file='MAILLES_CADASTRE_CADASTRE',status='old',iostat=iostatus )  

      if ( (nx1 .eq. 275) .and. (ny1.eq.268) .and. ( iostatus.eq.0)  ) then
         region1km=.true.
         write(*,*) 'Lit mailles region'
         
	 allocate(iregion(nx1,ny1))
	 allocate(rregion(nx1,ny1))	
	 allocate(idept_region(nx1,ny1,99))		 
	 allocate(izone_region(nx1,ny1,13))
	 allocate(tdept_region(nx1,ny1))		 
	 allocate(tzone_region(nx1,ny1))	 
	 	 	 	 	  	 
	 allocate(pop_region(nx1,ny1))	 
	 	 
         do i1=1,nx1
           do j1=1,ny1	  	
	      iregion(i1,j1) = .false.
	      rregion(i1,j1) = 0.
	      pop_region(i1,j1) = 0.	      
	      idept_region(i1,j1,:) = 0.
	      izone_region(i1,j1,:) = 0.
	      tdept_region(i1,j1) = 0
	      tzone_region(i1,j1) = 0	      	      	      	      	      
	    end do
          end do 	 
	 
	 open( unit=10,file='MAILLES_CADASTRE_CADASTRE',status='old' )
	 
	 open( unit=11,file='mailles_region1km.csv',status='unknown' )	 
	 
         do i1=1,nx1
           do j1=1,ny1	  
	      read(10,*,end=999) i, j, pop, zone_region, dept_region
	      ix =  ( i- 553500 )/1000 + 1
	      iy =  ( j-4887500 )/1000 + 1
	      if ( (ix .ge.1).and.(iy .ge.1).and.(ix .le.nx1).and.(iy .le.ny1) ) then 
	        iregion(ix,iy) = .true.
	        rregion(ix,iy) = 1.
		i = dept_region
		j = zone_region - 100
		tdept_region(ix, iy) = dept_region
                tzone_region(ix, iy) = zone_region - 100
		pop_region(ix, iy) = pop
		!write(*,*) ix, iy, i1, j1, pop	
		if ((i.ge.1).and.(i.le.99)) idept_region(ix, iy, i) = 1.
		if ((j.ge.1).and.(j.le.13)) izone_region(ix, iy, j) = 1.

	      end if        
	    end do
          end do   
	  
999      continue	  	 

	! ecrit dans un fichier csv
	 !ns = 0
         do ix=1,nx1
           do iy=1,ny1	  
	      if ( iregion(ix,iy) ) then 
	    !    ns = ns + 1	     
		write(11,'(I10,";",I5,";",I5,";",F12.5)') id_maille(ix,iy),tdept_region(ix, iy),tzone_region(ix, iy),pop_region(ix, iy)	
	      end if        
	    end do
          end do 
	 
	 close(10)
	 close(11)
	  
      else
        region1km=.false.
      end if
      
   end subroutine include_grille_region      
