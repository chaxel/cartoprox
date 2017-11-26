      program conversion
      
! Atmo Rhone-Alpes 2010
! Description : Realise des conversions entre systeme de coordonnées

      use params

      implicit none
      
      integer :: ipt
      
      double precision :: xo,yo
      
      ! ON A POSE LES BASES C'EST PARTI !!!!

      ! lit le fichier
      call read_args
           
      ! lit le fichier
      if (iascii)call read_ascii
      
      if (ipolair)write(*,'("Conversion POLAIR -> verifier que le resultat est -5 : int(-5.5)=",I3)') int(-5.5) 

      if (ixc.and.iyc) then 
        if (iverbose)write(*,*) 'Projection xc et yc...'    
        call conversion_geo(xc,yc,geoid_i,projection_i,zoneutm_i,&
	                    xo,yo,geoid_o,projection_o,zoneutm_o,&
		            ipolair,idebug)
	write(*,*) xo,yo 	  
      end if
           
      if (iascii) then
       do ipt=1, npts   
        call conversion_geo(recept_x(ipt),recept_y(ipt),geoid_i,projection_i,zoneutm_i,&
	                    recept_x(ipt),recept_y(ipt),geoid_o,projection_o,zoneutm_o,&
		            ipolair,idebug)
       end do      
      end if   
           
      ! ecrit les sorties  
      if (iascii)call write_output
                 
      end program
      
          
