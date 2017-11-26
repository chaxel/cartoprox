      module constantes_projection
      
      double precision :: pi	 ! Pi number
      double precision :: r2d	 ! Radians   ->Degrees
      double precision :: d2r	 ! Degrees   ->Radians

      parameter( pi=3.141592653589793238   )
      parameter( r2d=57.2957795131	   )
      parameter( d2r=.0174532925199	   ) 
      
      end module constantes_projection

!------------------------------------------------
      subroutine projection
      
      use params
      use constantes_projection
      
      implicit none
      
      if (projection_o.ne.projection_i) then
      
      if (ixc.and.iyc) then 
        write(*,*) 'Projection xc et yc...'
	xc1 = xc
	yc1 = yc      
        call conversion_geo(xc,yc)
	write(*,*) 'xc=',xc
	write(*,*) 'yc=',yc	
      end if
      
      if (ixmin.and.iymin)call conversion_geo(xmax,ymax)      
     
      if (ixmax.and.iymax)call conversion_geo(xmin,ymin)      
     
      end if
      
      end subroutine
		 
!------------------------------------------------
      subroutine conversion_geo(xi,yi)
      
      use params
      use constantes_projection
      
      implicit none
                  
      ! entrees
      real :: xi,yi 

      ! local             
      double precision :: x1
      double precision :: y1
      double precision :: h1
      double precision :: h2
      double precision :: x2
      double precision :: y2
      double precision :: x3
      double precision :: y3
      double precision :: zone_utm
      
      x1 = xi
      y1 = yi
            
      h1 = 0.
      h2 = 0.
      zone_utm = zoneutm_i
          
      ! X,Y -> GEOGRAPHIQUES
      if ( projection_i.ne.0) then
        !write(*,*)geoid_i,x1,y1,zone_utm,x2,y2,projection_i,units      
        call projectinv2(geoid_i,x1,y1,zone_utm,x2,y2,projection_i,units)
      else
        x2 = x1 * d2r
        y2 = y1 * d2r	
      end if   

      ! GEOID 1 - GEOID 2       
      if (geoid_i.ne.geoid_o) then 
        !write(*,*) geoid_i,x2,y2,h1,geoid_o,x2,y2,h2
	call georef(geoid_i,x2,y2,h1,geoid_o,x2,y2,h2)
      end if

      ! GEOGRAPHIQUES -> X,Y
      if ( projection_o.ne.0) then
        !write(*,*)geoid_o,x2,y2,x3,y3,zone_utm,projection_o,units    
        call projectfor2(geoid_o,x2,y2,x3,y3,zone_utm,projection_o,units) 
      else
        x3 = x2 * d2r
        y3 = y2 * d2r	
      end if             
      
      xi = x3
      yi = y3            
      
      end subroutine conversion_geo
      
      
