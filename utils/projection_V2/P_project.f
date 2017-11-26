         PROGRAM projectest
C      ******************************************************************
C      *             TRANSFORMATION DE SYSTEMES DE COORDONNEES          *
C      *    REFERENCE:         WWW.IGN.FR WWW.USGS.GOV                  *
C      *    AUTHOR:            E. CHAXEL                                *
C      *                       LABORATOIRE DES ECOULEMENTS GEOPHYSIQUES *
C      *                       ET INDUSTRIELS                           *
C      *		       BP49 - 38041 GRENOBLE                              *
C      *    LAST MODIFICATION: 4 OCTOBER 2003                           *
C      ******************************************************************
C Small program used for performing coordinate transformations
C Used to validate the tools GEOREF, PROJECTFOR2 and PROJECTINV2
C GEOREF performs changes of geodesic system for geographic coordinates.
C It uses IGN algorithms and parameters for WGS84, ED50, NTF
C PROJECTFOR2 performs conversion geographic -> (UTM, LAMBERTCC)
C in one specific geodesic system. It uses USGS algorithms.
C PROJECTINV2 performs conversion (UTM, LAMBERTCC) -> geographic 
C in one specific geodesic system. It uses USGS algorithms.
	implicit none
	include 'constantes.ext'
	integer  gs1,gs2          ! Geodesic systems	
	double precision  x1      ! (I) X projection coordinate  
	double precision  y1      ! (I) Y projection coordinate 	 
	double precision  h1,h2   ! Unused in UTM2LL			 
	double precision  x2      ! temp X projection coordinate  
	double precision  y2      ! temp Y projection coordinate 
	double precision  x3      ! (O) X projection coordinate  
	double precision  y3      ! (O) Y projection coordinate 
	double precision  zone    ! UTM zone  
	double precision  div     ! Units factor (div=1 for m, div=1000 for km)
	integer  proj1,proj2 ! Conversion (1=LAMBERTCC, 2=UTM, 0=GEOGRAPHIC)
	integer  transfo          ! Code of the geodesic transformation	
	character(len=20) projection(4) !Projections
	character(len=10) units(1000)	!Units     
	character(len=5)  ref(10)       !Geodesic systems
	integer  i, j, n, forinv
	character  yn !YES/NO
C-------Some arrays
        ref(1)='WGS84'
	ref(2)='ED50'
	ref(3)='NTF'
	ref(4)='GRS80'	
	ref(5)='USER'
	projection(1)='Geographic'
	projection(2)='Lambert 2 etendu'
	projection(3)='Transverse Mercator (UTM)'
	projection(4)='Lambert 93'
	units(1)='metres    '
	units(1000)='kilometres'		
	div=1  !metres
C-----****PARAMETERS****
      write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      write(*,*) '     PARAMETERS FOR LAMBERT ARE IN'
	write(*,*) '            parameters.include '
      write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'

C-----****User interface**** 
1       continue
	write(*,*) 'Transformation:'
	n=1
	do i=1,4
	  do j=1,4
	    if (i.ne.j) then
	      write(*,*) n,'-',trim(projection(i)),' to ',trim(projection(j))
	      n=n+1
	    end if
	  end do
	end do
	write(*,*) n,'-','Geoid transformation NO projection'
	write(*,*) 'Choice ?'
	read(*,*) forinv 
	
	if (forinv.eq.n) then 
	  proj1=1
	  proj2=proj1
	else if ((forinv.ge.1).and.(forinv.lt.n)) then
	  n=1
	  do i=1,4
	    do j=1,4
	    
	      if (i.ne.j) then		
		if (n.eq.forinv) then
	          proj1 = i 
	          proj2 = j        
		end if	       
	        n=n+1		
	      end if
	      
	    end do	    
	  end do
	  
	else
	  write(*,*) 'Quit'
	  stop
      end if
      write(*,*) projection(proj1),' -> ',projection(proj2)
      
      proj1 = proj1 - 1
      proj2 = proj2 - 1
      
      write(*,*)'proj1=',proj1
      write(*,*)'proj2=',proj2     
      !proj1=0 !PROJ=0 FOR GEOGRAPHIC
      
	!if (proj2.eq.3) proj2=0 !PROJ=0 FOR GEOGRAPHIC
        gs1 = 0
	gs2 = 0        
	
	if(proj1.eq.0)gs1=1
	if(proj1.eq.1)gs1=3
	if(proj1.eq.2)gs1=1		
	if(proj1.eq.3)gs1=4
	
	if(proj2.eq.0)gs2=1
	if(proj2.eq.1)gs2=3
	if(proj2.eq.2)gs2=1		
	if(proj2.eq.3)gs2=4	
	
	if (gs1*gs2.eq.0) then	
	write(*,*) 'Geodesic systems:'
        write(*,*) '1 : WGS84 -> GRS80'
        write(*,*) '2 : GRS80 -> WGS84'
        write(*,*) '3 : WGS84 -> NTF'
        write(*,*) '4 : NTF   -> WGS84'
        write(*,*) '7 : No transformation (WGS84)'
        write(*,*) '8 : No transformation (ED50)'
        write(*,*) '9 : No transformation (NTF)'
        write(*,*) 'Choix ?'
        read(*,*) transfo
	if (transfo.EQ.1) then
         gs1=1 !WGS84
	 gs2=4 !GRS80
	else if (transfo.EQ.2) then
         gs1=4 !GRS80
	 gs2=1 !WGS84
	else if (transfo.EQ.3) then
         gs1=1 !WGS84
	 gs2=3 !NTF
	else if (transfo.EQ.4) then
	 gs1=3 !NTF
	 gs2=1 !WGS84
	else if (transfo.EQ.7) then
	 gs1=1 !WGS84
	 gs2=1 !WGS84
	else if (transfo.EQ.8) then	
         gs1=2 !ED50
	 gs2=2 !ED50
	else if (transfo.EQ.9) then	
         gs1=3 !NTF
	 gs2=3 !NTF
	else
	  write(*,*) 'Quit'	
	  stop
	end if	
	end if

      write(*,*)'gs1='//trim(ref(gs1))
      write(*,*)'gs2='//trim(ref(gs2))

	if (proj1.eq.0) then
	  write(*,*) 'Enter LONGITUDE '//ref(gs1)//' (in degrees)'
	  read(*,*) x1
	  write(*,*) 'Enter LATITUDE '//ref(gs1)//' (in degrees):'
	  read(*,*) y1
c     ****Degrees -> Radians***
	  x1=x1*D2R
	  y1=y1*D2R
	else
	  write(*,*) 'projection ',projection(proj1)
	  write(*,*) 'Enter X_'//ref(gs1)//
     &           ' (in '//trim(units(int(div)))//'):'
	  read(*,*) x1
	  write(*,*) 'Enter Y_'//ref(gs1)//
     &           ' (in '//trim(units(int(div)))//'):'
	  read(*,*) y1
	end if
c     ****Is it UTM ?***
	if ((proj1.eq.2).or.(proj2.eq.2)) then
	  write(*,*) 'Enter UTM zone:'
	  read(*,*) zone
	else 
	  zone =-99
      end if
	h1=0

c     ****DISPLAY RESULTS ON INITIAL PROJECTION****
      write(*,*) '***********RESULT************'
	write(*,*) 'System= '//ref(gs1)
      if (proj1.eq.0) then
	  write(*,'(" LON=",f11.6," degrees")') x1*R2D
	  write(*,'(" LAT=",f11.6," degrees")') y1*R2D
	else
	 write(*,*) 'projection ',projection(proj1+1),int(zone)
	 write(*,'(" X=",f13.3,1X,A10)') x1,units(int(div))
	 write(*,'(" Y=",f13.3,1X,A10)') y1,units(int(div))       
	end if

c     ****COMPUTE TRANSFORMATION****
c     PROJ1
	write(*,*)'projectinv2(gs1,x1,y1,zone,x2,y2,proj1,div)'
	call projectinv2(gs1,x1,y1,zone,x2,y2,proj1,div)
	write(*,*) gs1,x1,y1,zone,x2,y2,proj1,div
c     CHANGE GEOID
        write(*,*)'georef(gs1,x2,y2,h1,gs2,x2,y2,h2)'
	call georef(gs1,x2,y2,h1,gs2,x2,y2,h2)
	write(*,*) gs1,x2,y2,h1,gs2,x2,y2,h2
c     PROJ2
c      write(*,*) 'zone ',zone
        write(*,*)'projectfor2(gs2,x2,y2,x3,y3,zone,proj2,div)'
	call projectfor2(gs2,x2,y2,x3,y3,zone,proj2,div)
	write(*,*) gs2,x2,y2,x3,y3,zone,proj2,div

c     ****DISPLAY RESULTS OF 2ND TRANSFORMATION****
	write(*,*) '      ----->'
	write(*,*) 'System= '//ref(gs2)
      if (proj2.eq.0) then
	  write(*,'(" LON=",f11.6," degrees")') x3*R2D
	  write(*,'(" LAT=",f11.6," degrees")') y3*R2D
	else
	 write(*,*) 'projection ',projection(proj2+1),int(zone)
	 write(*,'(" X=",f13.3,1X,A10)') x3,units(int(div))
	 write(*,'(" Y=",f13.3,1X,A10)') y3,units(int(div))       
	end if		    	    
      write(*,*) '***********RESULT************'
	write(*,*) 'Another transformation [Y/N]'
	read(*,*) yn
	if ((yn.EQ.'y').OR.(yn.EQ.'Y')) then
	  goto 1
	else
	  write(*,*) 'Quit'
	  stop
	end if
       END PROGRAM projectest
