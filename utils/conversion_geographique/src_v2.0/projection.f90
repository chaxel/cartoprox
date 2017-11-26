      module constantes_projection
      
      double precision :: pi	 ! Pi number
      double precision :: r2d	 ! Radians   ->Degrees
      double precision :: d2r	 ! Degrees   ->Radians

      parameter( pi=3.141592653589793238   )
      parameter( r2d=57.2957795131	   )
      parameter( d2r=.0174532925199	   ) 
      
      end module constantes_projection
		 
!------------------------------------------------
      subroutine conversion_geo(xi,yi,geoid1,projection1,zoneutm1,&
                                xo,yo,geoid2,projection2,zoneutm2,&
				ipolair,idebug)
      
      !use params
      use constantes_projection
      
      implicit none
                  
      ! entrees
      double precision :: xi,yi,xo,yo      
      real :: dd,mm,ss
      
      logical :: idebug, ipolair
      
      integer :: geoid1, geoid2
      integer :: zoneutm1, zoneutm2
      integer :: projection1, projection2     

      ! local
      double precision :: x1
      double precision :: y1     
      double precision :: x2
      double precision :: y2
      double precision :: h2      
      double precision :: x3
      double precision :: y3
      double precision :: h3      
      double precision :: x4
      double precision :: y4          
      double precision :: zone_utm
      double precision :: units
      
      units = 1.     
      
      x1 = xi
      y1 = yi
            
      ! Converti au format POLAIR
      if (ipolair.and.(projection1.eq.0)) then  

        ! DDMMSS.SS -> DD.DDDDDD
        dd=int(x1/10000.)
	mm=int((x1-dd*10000.)/100.)
	ss=x1-dd*10000.-mm*100.
	x1= dd + mm/60. + ss/3600.

	! DDMMSS.SS -> DD.DDDDDD
        dd=int(y1/10000.)
	mm=int((y1-dd*10000.)/100.)
	ss=y1-dd*10000.-mm*100.
	y1= dd + mm/60. + ss/3600.
	
	write(*,'("Conversion POLAIR")')
	write(*,'("lon=",F8.4,1X,"lat=",F8.4)')	 x1, 	y1
      end if 
          
      ! X,Y -> GEOGRAPHIQUES
      if ( projection1.ne.0) then   
        zone_utm = zoneutm1
	x2 = 0.
        y2 = 0.
        h2 = 0.	
	if(idebug)&
	  write(*,*)     geoid1,x1,y1,zone_utm,x2,y2,projection1,units
        call projectinv2(geoid1,x1,y1,zone_utm,x2,y2,projection1,units)
      else
        x2 = x1 * d2r
        y2 = y1 * d2r
        h2 = 0.		
      end if  

      if(idebug)&
        write(*,*)x1,y1,'->',x2*r2d,y2*r2d       
    
      !!!!!!!!!!!!!!!!!!! sans cette ligne le programme BUG !!!!!!!!!!!!!!!!!!!
      !write(*,*)
      !!!!!!!!!!!!!!!!!!! sans cette ligne le programme BUG !!!!!!!!!!!!!!!!!!!      

      ! GEOID 1 - GEOID 2 (converti latlong en cartesienne puis realise le changement de GEOID)
      if (geoid1.ne.geoid2) then 

        x3 = 0.
	y3 = 0.
	h3 = 0.
	if(idebug)&
	  write(*,*)geoid1,x2,y2,h2,geoid2,x3,y3,h3
	call georef(geoid1,x2,y2,h2,geoid2,x3,y3,h3)	
      else
        x3 = x2
	y3 = y2
      end if
                  
      if(idebug)&
	write(*,*)x2*r2d,y2*r2d,'->',x3*r2d,y3*r2d  	    

      ! GEOGRAPHIQUES -> X,Y
      if ( projection2.ne.0) then
        x4 = 0.
	y4 = 0.
	zone_utm = zoneutm2
	if(idebug)&
	write(*,*)       geoid2,x3,y3,x4,y4,zone_utm,projection2,units
        call projectfor2(geoid2,x3,y3,x4,y4,zone_utm,projection2,units) 
      else
        x4 = x3 * r2d
        y4 = y3 * r2d	
      end if   
      
      if(idebug)&
	write(*,*)x3*r2d,y3*r2d,'->',x4,y4                
            
      ! Converti au format POLAIR
      if (ipolair.and.(projection2.eq.0).and.(projection1.ne.0)) then  
        ! X DDMMSS.SS
        dd=int(x4)
	mm=int((x4-dd)*60.)
	ss=((x4-dd)*60.-mm)*60.
	xo=dd*10000.+mm*100.+ss

	! Y DDMMSS.SS
        dd=int(y4)
	mm=int((y4-dd)*60.)
	ss=((y4-dd)*60.-mm)*60.
	yo=dd*10000.+mm*100.+ss			
      else      
        if (projection2.eq.0) then 
	  ! projection geo -> pas d'arrondi
	  xo = x4
          yo = y4
	else
	! projection plane -> arrondi au m
	  xo = nint(x4)
          yo = nint(y4)	
	end if          
      end if        
      
      end subroutine conversion_geo
      
      
