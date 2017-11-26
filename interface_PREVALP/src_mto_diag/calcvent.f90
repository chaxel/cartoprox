      subroutine calcvent(numLons,numLats,numLays,ua,va,dd,ff)
      
      implicit none
      
      integer :: numLons
      integer :: numLats      
      integer :: numLays
      
      real :: ua(numlons,numLats,numLays)
      real :: va(numlons,numLats,numLays)
      real :: dd(numlons,numLats,numLays)
      real :: ff(numlons,numLats,numLays)
      
      real :: u
      real :: v
      real :: d
      real :: f
      
      real,parameter :: pi=3.1415926     
      
      integer :: i, j, k
      
      do i=1,numLons
      do j=1,numLats
      do k=1,numLays
      
      u = ua(i,j,k)
      v = va(i,j,k)
      
      f = ( u*u + v*v ) ** 0.5 
      
      if (u.gt.0) then
        
        d = 90 - 180/pi * atan(v/u) ! pour ou va le vent
	
	d= d + 180        ! pour d'ou vient le vent
      
      else if (u.lt.0) then
      
        d = 180 + 90 - 180/pi * atan(v/u) ! pour ou va le vent    
	
	d = d + 180  ! pour d'ou vient le vent  
      
      else if (u.eq.0) then
      
        if (v.gt.0) then
	
	  d = 360
	
        else if (v.lt.0) then    

	  d = 180	
	
	else 
	 
	  d=0
	
	end if  
      
      end if
       
      dd(i,j,k) = mod ( d , 360. ) 
      
      ff(i,j,k) = f
      
      
      end do
      end do
      end do
      
       
            
      
      end subroutine


!!!!!!!
      subroutine calcvent_station(ua,va,dd,ff)
      
      implicit none
      
      real :: ua
      real :: va
      real :: dd
      real :: ff
      
      real :: u
      real :: v
      real :: d
      real :: f
      
      real,parameter :: pi=3.1415926     
      
      u = ua
      v = va
      
      f = ( u*u + v*v ) ** 0.5 
      
      if (u.gt.0) then
        
        d = 90 - 180/pi * atan(v/u) ! pour ou va le vent
	
	d= d + 180        ! pour d'ou vient le vent
      
      else if (u.lt.0) then
      
        d = 180 + 90 - 180/pi * atan(v/u) ! pour ou va le vent    
	
	d = d + 180  ! pour d'ou vient le vent  
      
      else if (u.eq.0) then
      
        if (v.gt.0) then
	
	  d = 360
	
        else if (v.lt.0) then    

	  d = 180	
	
	else 
	 
	  d=0
	
	end if  
      
      end if
       
      dd = mod ( d , 360. ) 
      
      ff = f
      
      end subroutine
