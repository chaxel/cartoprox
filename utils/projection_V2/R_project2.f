      
C************************************************************
      SUBROUTINE projectfor2(gs,lon_,lat_,x,y,zone,proj,div)
C************************************************************      	     
	implicit none
	
	include 'projection.ext'
	include 'constantes.ext'	

C-------Inputs in metres and radians
	double precision  lon_,lat_ !radians
	double precision  lon,lat,x,y,zone,calc_utm_zone
	double precision  div ! metres: div=1 ; kilometres: div=1000
	integer  gs !Syteme géodésique(1=WGS84,2=ED50,3=NTF,4=GRS80,5=USER)	
	integer  proj !1=LAMBERTCC ; 2=UTM ; 0=NO PROJECTION
	
C-------Paramètres de l'ellipsoide		
	double precision  a !Demi-grand axe de l'ellipsoide (en mètres)
	double precision  b !Demi-petit axe de l'ellipsoide (en mètres)	
	double precision  e !Première excentricité de l'ellipsoide
	double precision  f !Applatissement
C-------Paramètres des projections UTM et LAMBERTCC	
        double precision  lon_center
c        double precision  lat_origin    !Origin latitude
c        double precision  false_east    !False easting
c        double precision  false_north   !False northing
c        double precision  scale_fact    !Scale factor
C-------Paramètres des projections UTM et LAMBERTCC
c        double precision  c_lat_D       !center latitude in degree
c        double precision  c_lon_D       !center longitude in degree
c        double precision  lat1_D        !first standard parallel in degree
c        double precision  lat2_D        !second standard parallel in degree
c        double precision  false_east_D  !x offset in meters
c        double precision  false_north_D ! y offset in meters
c        parameter ( lat_origin=0      )
c        parameter ( false_east=500000 )
c        parameter ( false_north=0     )
c        parameter ( scale_fact=0.9996 )
c        parameter ( c_lat_D=40	    )
c        parameter ( c_lon_D=-90	    )
c        parameter ( lat1_D=30	    )
c        parameter ( lat2_D=60	    )
c        parameter ( false_east_D=0    )
c        parameter ( false_north_D=0   )		
	
C-------Get parameters of the ellipsoid
        call ellipsoide(gs,a,b,e,f)
	
C-------Get parameters of the projection (typ_proj and others)
        call projection_params(proj,typ_proj,
     &     lat_origin,false_east,false_north,scale_fact,
     &     c_lat_D,c_lon_D,lat1_D,lat2_D,false_east_D,false_north_D)
		
	lon=lon_
	lat=lat_
	IF (typ_proj.eq.1) THEN
	  CALL lamccfor(a,b,lat1_D*D2R,lat2_D*D2R,
     &               c_lon_D*D2R,c_lat_D*D2R,
     &               false_east_D,false_north_D,scale_fact,lon,lat,x,y)
c	  zone=-99
	ELSE IF (typ_proj.eq.2) THEN
	  if (zone.NE.calc_utm_zone(lon)) then
C	    write(*,*) 'Attention zone incorrecte: deformation importante'
	  end if
	  lon_center = ((6 * dabs(zone)) - 183) * D2R
          CALL utmfor(a,b,lon_center,lat_origin,false_east,false_north,
     &                scale_fact,zone,lon,lat,x,y)
	ELSE IF (typ_proj.eq.0) THEN
	  x=lon_*div
	  y=lat_*div
c	  zone=-99		
	END IF
		
	x=x/div
	
	y=y/div
	
      END SUBROUTINE projectfor2
      
C************************************************************
      SUBROUTINE projectinv2(gs,x_,y_,zone,lon,lat,proj,div)
C************************************************************
        implicit none
	
	include 'projection.ext'
	include 'constantes.ext'	
	
C-------Inputs in metres and radians
	double precision  lon,lat,x,y,zone,x_,y_ 
	double precision  div ! metres: div=1 ; kilometres: div=1000
	integer  gs !Syteme géodésique(1=WGS84,2=ED50,3=NTF,4=GRS80,5=USER)	
	integer  proj !1=LAMBERTCC ; 2=UTM ; 0=NO PROJECTION
		
C-------Paramètres de l'ellipsoide		
	double precision  a    !Demi-grand axe de l'ellipsoide (en mètres)
	double precision  b    !Demi-petit axe de l'ellipsoide (en mètres)	
	double precision  e    !Première excentricité de l'ellipsoide
	double precision  f    !Applatissement	
	
        double precision  lon_center
c        double precision  lat_origin    !Origin latitude
c        double precision  false_east    !False easting
c        double precision  false_north   !False northing
c        double precision  scale_fact    !Scale factor
	
C-------Paramètres des projections UTM et LAMBERTCC
c        double precision  c_lat_D       !center latitude in degree
c        double precision  c_lon_D       !center longitude in degree
c        double precision  lat1_D        !first standard parallel in degree
c        double precision  lat2_D        !second standard parallel in degree
c        double precision  false_east_D  !x offset in meters
c        double precision  false_north_D ! y offset in meters
c        parameter ( lat_origin=0      )
c        parameter ( false_east=500000 )
c        parameter ( false_north=0     )
c        parameter ( scale_fact=0.9996 )
c        parameter ( c_lat_D=40	    )
c        parameter ( c_lon_D=-90	    )
c        parameter ( lat1_D=30	    )
c        parameter ( lat2_D=60	    )
c        parameter ( false_east_D=0    )
c        parameter ( false_north_D=0   )			

C-------Get parameters of the ellipsoid
        call ellipsoide(gs,a,b,e,f)
	
C-------Get parameters of the projection (typ_proj and others)
        call projection_params(proj,typ_proj,
     &     lat_origin,false_east,false_north,scale_fact,
     &     c_lat_D,c_lon_D,lat1_D,lat2_D,false_east_D, false_north_D)

c       write(*,*)'projectinv2: typ_proj=',typ_proj
	
	IF (typ_proj.eq.1) THEN
	  x=x_*div	
	  y=y_*div		
	  CALL lamccinv(a,b,lat1_D*D2R,lat2_D*D2R,
     &                  c_lon_D*D2R,c_lat_D*D2R,
     &		        false_east_D,false_north_D,scale_fact,x,y,lon,lat)
c		zone=-99
	ELSE IF (typ_proj.eq.2) THEN	
	  x=x_*div	
	  y=y_*div							
	  lon_center = ((6 * dabs(zone)) - 183) * D2R
	  CALL utminv(a,b,lon_center,lat_origin,false_east,false_north,
     &		      scale_fact,zone,x,y,lon,lat)
	ELSE IF (typ_proj.eq.0) THEN
	  lon=x_
	  lat=y_
c	  zone=-99
        ELSE
	      write(*,*) 'Bad Projection !'
	END IF	
	
	lon=lon	
	lat=lat
	
      END SUBROUTINE projectinv2
