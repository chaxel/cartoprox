      SUBROUTINE lamccfor(r_maj,r_min,lat1,lat2,c_lon,c_lat,&
                          false_east,false_north,lon_1,lat_1,x_,y_)

!Modified by E. Chaxel fron C to F90 - June 2002
!chaxeleric@yahoo.fr
!Original header
!!******************************************************************************
!NAME                            LAMBERT CONFORMAL CONIC
!
!PURPOSE:	Transforms input longitude and latitude to Easting and
!		Northing for the Lambert Conformal Conic projection.  The
!		longitude and latitude must be in degrees.  The Easting
!		and Northing values will be returned in meters.
!
!PROGRAMMER              DATE
!----------              ----
!T. Mittan               2-26-93
!
!
!ALGORITHM REFERENCES
!
!1.  Snyder, John P., "Map Projections--A Working Manual", U.S. Geological
!    Survey Professional Paper 1395 (Supersedes USGS Bulletin 1532), United
!    State Government Printing Office, Washington D.C., 1987.
!
!2.  Snyder, John P. and Voxland, Philip M., "An Album of Map Projections",
!    U.S. Geological Survey Professional Paper 1453 , United State Government
!*******************************************************************************/
!
! include	module_projection.f90
!			functions_proj.f90


	! E. Chaxel June 2002
	! chaxeleric@yahoo.fr
	! DRP version 1.0
	IMPLICIT NONE
	include 'constantes.ext'
	! INPUT FOR INIT
	DOUBLE PRECISION ::r_major                ! major axis 
	DOUBLE PRECISION ::r_minor                ! minor axis   
	DOUBLE PRECISION ::es                     ! eccentricity squared  
	DOUBLE PRECISION ::e                      ! eccentricity   
	DOUBLE PRECISION ::center_lon             ! center longitute in degrees 
	DOUBLE PRECISION ::center_lat             ! cetner latitude in degrees    
	DOUBLE PRECISION ::ns                     ! ratio of angle between meridian
	DOUBLE PRECISION ::f0                     ! flattening of ellipsoid      
	DOUBLE PRECISION ::rh                     ! height above ellipsoid    
	DOUBLE PRECISION ::false_easting          ! x offset in meters      
	DOUBLE PRECISION ::false_northing         ! y offset in meters  
	! INPUT FOR CALCULATION
	DOUBLE PRECISION :: lon_1			! Longitude in rad
	DOUBLE PRECISION :: lat_1			! Latitude in rad
	DOUBLE PRECISION :: lon_			! Longitude in rad
	DOUBLE PRECISION :: lat_			! Latitude in rad
	DOUBLE PRECISION :: x_				! X projection coordinate 
	DOUBLE PRECISION :: y_				! Y projection coordinate 
	! INITIALIZATION
	DOUBLE PRECISION   ::c_lat			! center latitude  in rad
	DOUBLE PRECISION   ::c_lon			! center longitude  in rad               
	DOUBLE PRECISION   ::lat1			! first standard parallel  in rad         
	DOUBLE PRECISION   ::lat2			! second standard parallel  in rad    
	DOUBLE PRECISION   ::r_maj			! major axis   
	DOUBLE PRECISION   ::r_min			! minor axis                           
	DOUBLE PRECISION   ::false_east     ! x offset in meters                   */
	DOUBLE PRECISION   ::false_north    ! y offset in meters                   */
	DOUBLE PRECISION  ::sin_po                  ! sin value                            */
	DOUBLE PRECISION  ::cos_po                  ! cos value                            */
	DOUBLE PRECISION  ::con                     ! temporary variable                   */
	DOUBLE PRECISION  ::ms1                     ! small m 1                            */
	DOUBLE PRECISION  ::ms2                     ! small m 2                            */
	DOUBLE PRECISION  ::temp                    ! temporary variable                   */
	DOUBLE PRECISION  ::ts0                     ! small t 0                            */
	DOUBLE PRECISION  ::ts1                     ! small t 1                            */
	DOUBLE PRECISION  ::ts2                     ! small t 2             */
	DOUBLE PRECISION  ::rh1						! height above ellipsoid               */
	DOUBLE PRECISION  ::sinphi					! sin value                            */
	DOUBLE PRECISION  ::theta					! angle                                */
	DOUBLE PRECISION  ::ts  	                         
	DOUBLE PRECISION, EXTERNAL  ::msfnz         ! function to compute small m          */
	DOUBLE PRECISION, EXTERNAL	:: tsfnz		! function to compute small t          */
	DOUBLE PRECISION, EXTERNAL	:: adjust_lon	! function to adust the longitude      */ 
	DOUBLE PRECISION, EXTERNAL	:: fabs
	DOUBLE PRECISION, EXTERNAL	:: square
	DOUBLE PRECISION, EXTERNAL	:: pow
	INTEGER :: ios
	

	lon_=lon_1
	lat_=lat_1

	!! Initialize the Lambert Conformal conic projection
	!  ------------------------------------------------*/
	r_major = r_maj
	r_minor = r_min
	false_northing = false_north
	false_easting = false_east

	! Standard Parallels cannot be equal and on opposite sides of the equator
	!------------------------------------------------------------------------*/
	IF (fabs(lat1+lat2) < EPSLN) THEN
	   PRINT*, "Equal Latitiudes for St. Parallels on opposite sides of equator ","lamcc-for"
	END IF
	temp = r_minor / r_major
	es = 1.0 - SQUARE(temp) 
	e = SQRT(es)
	center_lon = c_lon
	center_lat = c_lat
	CALL SINCOS(lat1,sin_po,cos_po)
	con=sin_po
	ms1=MSFNZ(e,sin_po,cos_po)
	ts1=TSFNZ(e,lat1,sin_po)
	CALL SINCOS(lat2,sin_po,cos_po)
	ms2=MSFNZ(e,sin_po,cos_po)
	ts2=TSFNZ(e,lat2,sin_po)
	sin_po= DSIN(center_lat)
	ts0=TSFNZ(e,center_lat,sin_po)
	IF (FABS(lat1 - lat2)>EPSLN) THEN
		ns = LOG(ms1/ms2)/LOG(ts1/ts2)
	ELSE
		ns = con
	END IF
	f0 = ms1/(ns*(pow(ts1,ns)))
	rh = r_major*f0*(pow(ts0,ns))
	!/* Lambert Conformal conic forward equations--mapping lat,long to x,y
	!-----------------------------------------------------------------*/
	con =fabs(fabs(lat_)-HALF_PI)
	IF (con>EPSLN) THEN
		sinphi=DSIN(lat_)
		ts=TSFNZ(e,lat_,sinphi)
		rh1=r_major*f0*pow(ts,ns)
	ELSE
		con=lat_*ns
		IF (con<=0) THEN
			PRINT*, "Point can not be projected"
		END IF
		rh1=0
	END IF
	theta=ns*ADJUST_LON(lon_-center_lon)
	x_=(rh1*DSIN(theta)+false_easting)
	y_=(rh-rh1*DCOS(theta)+false_northing)
	! Report parameters to the user
	! -----------------------------*/
	!	PRINT*,"LAMBERT CONFORMAL CONIC"
	!	CALL radius2(r_major, r_minor)
	!	CALL stanparl(lat1,lat2)
	!	CALL cenlonmer(center_lon)
	!	CALL origin(c_lat)
	!	CALL offsetp(false_easting,false_northing)
END SUBROUTINE lamccfor



!********************************************************************************
!Modified by E. Chaxel fron C to F90 - June 2002
!chaxeleric@yahoo.fr
!Original header
!/*******************************************************************************
!NAME                            LAMBERT CONFORMAL CONIC 
!
!PURPOSE:	Transforms input Easting and Northing to longitude and
!		latitude for the Lambert Conformal Conic projection.  The
!		Easting and Northing must be in meters.  The longitude
!		and latitude values will be returned in radians.
!
!PROGRAMMER              DATE
!----------              ----
!T. Mittan		2-26-93
!
!ALGORITHM REFERENCES
!
!1.  Snyder, John P., "Map Projections--A Working Manual", U.S. Geological
!   Survey Professional Paper 1395 (Supersedes USGS Bulletin 1532), United
!    State Government Printing Office, Washington D.C., 1987.
!
!2.  Snyder, John P. and Voxland, Philip M., "An Album of Map Projections",
!    U.S. Geological Survey Professional Paper 1453 , United State Government
!    Printing Office, Washington D.C., 1989.
!*******************************************************************************/
!#include "cproj.h"
!
      SUBROUTINE lamccinv(r_maj,r_min,lat1,lat2,c_lon,c_lat,&
                          false_east,false_north,x_1 , y_1, lon_, lat_)
	! E. Chaxel June 2002
	! chaxeleric@yahoo.fr
	! DRP version 1.0
	IMPLICIT NONE
	include 'constantes.ext'	
	! INPUT FOR INIT
	DOUBLE PRECISION	::r_major          ! major axis 
	DOUBLE PRECISION	::r_minor          ! minor axis   
	DOUBLE PRECISION	::es               ! eccentricity squared  
	DOUBLE PRECISION	::e                ! eccentricity   
	DOUBLE PRECISION	::center_lon       ! center longituted 
	DOUBLE PRECISION	::center_lat       ! cetner latitude    
	DOUBLE PRECISION	::ns               ! ratio of angle between meridian
	DOUBLE PRECISION	::f0               ! flattening of ellipsoid      
	DOUBLE PRECISION	::rh               ! height above ellipsoid    
	DOUBLE PRECISION	::false_easting    ! x offset in meters      
	DOUBLE PRECISION	::false_northing   ! y offset in meters  
	! INPUT FOR CALCULATION
	DOUBLE PRECISION	::x_,x_1		! X projection coordinate 
	DOUBLE PRECISION	::y_,y_1		! Y projection coordinate 
	DOUBLE PRECISION	::lon_			! Longitude in radians
	DOUBLE PRECISION	::lat_			! Latitude in radians 
	! INITIALIZATION
	DOUBLE PRECISION   ::c_lat			! center latitude  in rad
	DOUBLE PRECISION   ::c_lon			! center longitude  in rad               
	DOUBLE PRECISION   ::lat1			! first standard parallel  in rad         
	DOUBLE PRECISION   ::lat2			! second standard parallel  in rad   
	DOUBLE PRECISION	::r_maj			! major axis   
	DOUBLE PRECISION	::r_min			! minor axis                           
	DOUBLE PRECISION	::false_east    ! x offset in meters  
	DOUBLE PRECISION	::false_north   ! y offset in meters  
	DOUBLE PRECISION	::sin_po        ! sin value         
	DOUBLE PRECISION	::cos_po        ! cos value          
	DOUBLE PRECISION	::con           ! temporary variable
	DOUBLE PRECISION	::ms1           ! small m 1         
	DOUBLE PRECISION	::ms2           ! small m 2 
	DOUBLE PRECISION	::temp          ! temporary variable  
	DOUBLE PRECISION	::ts0			! small t 0
	DOUBLE PRECISION	::ts1           ! small t 1 
	DOUBLE PRECISION	::ts2           ! small t 2 
	DOUBLE PRECISION	::rh1			! height above ellipsoid  
	DOUBLE PRECISION	::sinphi		! sin value                 
	DOUBLE PRECISION	::theta			! angle               
	DOUBLE PRECISION	::ts  	                         
	DOUBLE PRECISION, EXTERNAL  ::	msfnz       ! function to compute small m 
	DOUBLE PRECISION, EXTERNAL	::	tsfnz		! function to compute small t
	DOUBLE PRECISION, EXTERNAL	::	adjust_lon	! function to adust the longitude
	DOUBLE PRECISION, EXTERNAL	::	fabs
	DOUBLE PRECISION, EXTERNAL	::	square
	DOUBLE PRECISION, EXTERNAL	::	pow
	DOUBLE PRECISION, EXTERNAL	::	phi2z
	INTEGER :: ios, flag
	
	x_=x_1
	y_=y_1
	r_major = r_maj
	r_minor = r_min
	false_easting = false_east
	false_northing = false_north
	!/* Standard Parallels cannot be equal and on opposite sides of the equator
	!------------------------------------------------------------------------*/
	IF (fabs(lat1+lat2) < EPSLN) THEN
		PRINT*, "Equal Latitiudes for St. Parallels on opposite sides of equator",&
		"lamcc-inv"
	END IF

	temp = r_minor / r_major
	es = 1.0 - SQUARE(temp)
	e = SQRT(es)
	center_lon = c_lon
	center_lat = c_lat
	CALL sincos(lat1,sin_po,cos_po)
	con = sin_po
	ms1 = MSFNZ(e,sin_po,cos_po)
	ts1 = TSFNZ(e,lat1,sin_po)
	CALL sincos(lat2,sin_po,cos_po)
	ms2 = MSFNZ(e,sin_po,cos_po)
	ts2 = TSFNZ(e,lat2,sin_po)
	sin_po = sin(center_lat)
	ts0 = TSFNZ(e,center_lat,sin_po)
	IF (fabs(lat1 - lat2) > EPSLN) THEN
		ns = LOG(ms1/ms2)/ LOG(ts1/ts2);
	ELSE
		ns = con
	END IF
	f0 = ms1 / (ns * pow(ts1,ns))
	rh = r_major * f0 * pow(ts0,ns)

	!/* Report parameters to the user
	!  -----------------------------*/
	!ptitle("LAMBERT CONFORMAL CONIC"); 
	!radius2(r_major, r_minor);
	!stanparl(lat1,lat2);
	!cenlonmer(center_lon);
	!origin(c_lat);
	!offsetp(false_easting,false_northing);

	!/* Lambert Conformal Conic inverse equations--mapping x,y to lat/long
	!-----------------------------------------------------------------*/
	flag = 0	
	x_ = x_-false_easting
	y_ = rh - y_ + false_northing
	IF (ns > 0) THEN
		rh1 = SQRT(x_ * x_ + y_ * y_)
		con = 1.0
	ELSE
		rh1 = -SQRT(x_ * x_ + y_ * y_)
		con = -1.0
	END IF
	theta = 0.0
	IF (rh1 /= 0) THEN
		theta = DATAN2((con * x_),(con * y_))
	END IF
	IF ((rh1 /= 0) .OR. (ns > 0.0)) THEN
		con = 1.0/ns;
		ts = pow((rh1/(r_major * f0)),con);
		lat_ = phi2z(e,ts,flag);
	ELSE
		lat_ = -HALF_PI;
	END IF
	lon_ = adjust_lon(theta/ns + center_lon)

END SUBROUTINE lamccinv

!******************************************************************************* 
! Algorithm modified by E. Chaxel
! October 2002
! chaxeleric@yahoo.fr
! include functions_proj.f90
!         module_projection.f90
!
! ******************************************************************************
! NAME                            UNIVERSAL TRANSVERSE MERCATOR
!
! PURPOSE:	Transforms input longitude and latitude to Easting and
!		Northing for the Universal Transverse Mercator projection.
!		The longitude and latitude must be in radians.  The Easting
!		and Northing values will be returned in meters.
!
! PROGRAMMER              DATE		REASON
! ----------              ----		------
! D. Steinwand, EROS      Nov, 1991
! T. Mittan		Mar, 1993
! S. Nelson		Feb, 1995	Divided tmfor.c into two files, one
!					for UTM (utmfor.c) and one for 
!					TM (tmfor.c).  This was a
!					necessary change to run forward
!					projection conversions for both
!					UTM and TM in the same process.
!
! ALGORITHM REFERENCES
!
! 1.  Snyder, John P., "Map Projections--A Working Manual", U.S. Geological
!     Survey Professional Paper 1395 (Supersedes USGS Bulletin 1532), United
!     State Government Printing Office, Washington D.C., 1987.
!
! 2.  Snyder, John P. and Voxland, Philip M., "An Album of Map Projections",
!     U.S. Geological Survey Professional Paper 1453 , United State Government
!     Printing Office, Washington D.C., 1989.
! ******************************************************************************

! Universal Transverse Mercator forward equations--mapping lat,long to x,y
! Note:  The algorithm for UTM is exactly the same as TM and therefore
!	     if a change is implemented, also make the change to TMFOR.c
! -----------------------------------------------------------------------
      subroutine utmfor(r_maj,r_min,lon_center,lat_origin,false_east,false_north,&
                        scale_fact,zone,lon, lat, x, y)
		

	implicit none
	include 'constantes.ext'
	double precision ::  r_major		! major axis 				
	double precision ::  r_minor		! minor axis 				
	double precision ::  scale_factor	! scale factor				
	double precision ::  lon_center		! Center longitude (projection center) 
	double precision ::  lat_origin		! center latitude			
	double precision ::  e0,e1,e2,e3	! eccentricity constants		
	double precision ::  e,es,esp		! eccentricity constants		
	double precision ::  ml0			! small value m			
	double precision ::  false_northing	! y offset in meters			
	double precision ::  false_easting	! x offset in meters
	double precision ::  false_north	! y offset in meters			
	double precision ::  false_east		! x offset in meters		
	double precision ::  ind		! spherical flag			

	double precision :: lon			! (I) Longitude 		
	double precision :: lat			! (I) Latitude 		
	double precision :: x			! (O) X projection coordinate 	
	double precision :: y			! (O) Y projection coordinate 	

	double precision :: adjust_lon		! Function to adjust longitude to -180 - 180 	
	double precision :: delta_lon		! Delta longitude (Given longitude - center 	
	double precision :: theta			! angle					
	double precision :: delta_theta		! adjusted longitude				
	double precision :: mlfn			! function to compute small m			
	double precision :: sin_phi			! sin value				
	double precision :: cos_phi			! cos value				
	double precision :: al, als			! temporary values				
	double precision :: b				! temporary values				
	double precision :: c, t, tq		! temporary values				
	double precision :: con, n, ml		! cone constant, small m			

	double precision ::  r_maj			! major axis
	double precision ::  r_min			! minor axis	
	double precision ::  scale_fact		! scale factor
	double precision :: zone					! zone number
	double precision :: temp			!temporary variable
	double precision :: e0fn,e1fn,e2fn,e3fn,square,fabs	!functions

	! Initialize the Universal Transverse Mercator (UTM) projection
	!  -------------------------------------------------------------
	if ((abs(zone) < 1) .OR. (abs(zone) > 60)) then
	   print*,"Illegal zone number in utm-forint"
	endif
	r_major = r_maj
	r_minor = r_min
	scale_factor = scale_fact
	false_northing=false_north
	false_easting=false_east
	temp = r_minor / r_major
	es = 1.0 - SQUARE(temp)
	e = dsqrt(es)
	e0 = e0fn(es)
	e1 = e1fn(es)
	e2 = e2fn(es)
	e3 = e3fn(es)
	ml0 = r_major * mlfn(e0, e1, e2, e3, lat_origin)
	esp = es / (1.0 - es);
	if (es < .00001) then
	   ind = 1
	end if

	! Forward equations
	!  -----------------
	delta_lon = adjust_lon(lon - lon_center)
	call sincos(lat, sin_phi, cos_phi)

	! This part was in the fortran code and is for the spherical form 
	!----------------------------------------------------------------
	if (ind /= 0) then
		b = cos_phi * dsin(delta_lon);
		if ((fabs(fabs(b) - 1.0)) < .0000000001) then
			print*,"Point projects into infinity"
		end if
	else
		x = .5 * r_major * scale_factor * dlog((1.0 + b)/(1.0 - b))
		con = dacos(cos_phi * dcos(delta_lon)/dsqrt(1.0 - b*b))
		if (lat < 0) then
			con = - con
		y = r_major * scale_factor * (con - lat_origin)
		end if
	end if

	al  = cos_phi * delta_lon
	als = SQUARE(al)
	c   = esp * SQUARE(cos_phi)
	tq  = dtan(lat)
	t   = SQUARE(tq)
	con = 1.0 - es * SQUARE(sin_phi)
	n   = r_major / dsqrt(con)
	ml  = r_major * mlfn(e0, e1, e2, e3, lat)

	x  = scale_factor * n * al * (1.0 + als / 6.0 * (1.0 - t + c + als / 20.0 * &
	(5.0 - 18.0 * t + SQUARE(t) + 72.0 * c - 58.0 * esp))) + false_easting

	y  = scale_factor * (ml - ml0 + n * tq * (als * (0.5 + als / 24.0 * &
	(5.0 - t + 9.0 * c + 4.0 * SQUARE(c) + als / 30.0 * (61.0 - 58.0 * t &
	+ SQUARE(t) + 600.0 * c - 330.0 * esp))))) + false_northing

end subroutine utmfor
!**************************************************************************************

!/*******************************************************************************
!NAME                            UNIVERSAL TRANSVERSE MERCATOR
!
!PURPOSE:	Transforms input Easting and Northing to longitude and
!		latitude for the Universal Transverse Mercator projection.
!		The Easting and Northing must be in meters.  The longitude
!		and latitude values will be returned in radians.
!
!PROGRAMMER              DATE		REASON
!----------              ----		------
!D. Steinwand, EROS      Nov, 1991
!T. Mittan		Mar, 1993
!S. Nelson		Feb, 1995	Divided tminv.c into two files, one
!					for UTM (utminv.c) and one for
!					TM (tminv.c).  This was a necessary
!					change to run inverse projection
!					conversions for both UTM and TM
!					in the same process.
!
!ALGORITHM REFERENCES
!
!1.  Snyder, John P., "Map Projections--A Working Manual", U.S. Geological
!    Survey Professional Paper 1395 (Supersedes USGS Bulletin 1532), United
!    State Government Printing Office, Washington D.C., 1987.
!
!2.  Snyder, John P. and Voxland, Philip M., "An Album of Map Projections",
!    U.S. Geological Survey Professional Paper 1453 , United State Government
!    Printing Office, Washington D.C., 1989.
!*******************************************************************************/
!
      subroutine utminv(r_maj,r_min,lon_center,lat_origin,false_east,false_north,&
                        scale_fact,zone,x, y, lon, lat)
	

	implicit none
	include 'constantes.ext'
	double precision ::  r_major		! major axis 				
	double precision ::  r_minor		! minor axis 				
	double precision ::  scale_factor	! scale factor				
	double precision ::  lon_center		! Center longitude (projection center) 
	double precision ::  lat_origin		! center latitude			
	double precision ::  e0,e1,e2,e3	! eccentricity constants		
	double precision ::  e,es,esp		! eccentricity constants		
	double precision ::  ml0			! small value m			
	double precision ::  false_northing	! y offset in meters			
	double precision ::  false_easting	! x offset in meters
	double precision ::  false_north	! y offset in meters			
	double precision ::  false_east		! x offset in meters			
	double precision ::  ind			! spherical flag			

	double precision :: lon				! (I) Longitude 		
	double precision :: lat				! (I) Latitude 		
	double precision :: x				! (O) X projection coordinate 	
	double precision :: y				! (O) Y projection coordinate 	

	double precision :: adjust_lon		! Function to adjust longitude to -180 - 180 	
	double precision :: delta_lon		! Delta longitude (Given longitude - center 	
	double precision :: theta			! angle					
	double precision :: delta_theta		! adjusted longitude				
	double precision :: mlfn			! function to compute small m			
	double precision :: sin_phi			! sin value				
	double precision ::	cos_phi			! cos value				
	double precision :: al, als			! temporary values				
	double precision :: b				! temporary values				
	double precision :: c, t, tq		! temporary values				
	double precision :: con, n, ml		! cone constant, small m			

	double precision ::  r_maj			! major axis
	double precision ::  r_min			! minor axis	
	double precision ::  scale_fact		! scale factor
	double precision :: zone					! zone number
	double precision :: temp			!temporary variable
	double precision :: e0fn,e1fn,e2fn,e3fn,square,fabs, sign_	!functions

	double precision :: phi		!/* temporary angles				*/
	double precision :: delta_phi	!/* difference between longitudes		*/
	integer :: i			!/* counter variable				*/
	double precision ::  tan_phi	!/* sin cos and tangent values	*/
	double precision ::  cs, ts, r, d, ds !	/* temporary variables		*/
	double precision ::  f, h, g	!		/* temporary variables		*/
	integer ::  max_iter = 20		!	/* maximun number of iterations	*/
	double precision ::  asinz

	!/* Initialize the Universal Transverse Mercator (UTM) projection
	!  -------------------------------------------------------------*/

	if ((abs(zone) < 1) .or. (abs(zone) > 60)) then
	   print*, "Illegal zone number"
	end if
	r_major = r_maj
	r_minor = r_min
	scale_factor = scale_fact

	!false_easting = 500000.0
	!if (zone < 0) then
	!	false_northing = 10000000.0
	!else
	!	false_northing = 0.0
	!end if

	false_northing=false_north
	false_easting=false_east

	temp = r_minor / r_major
	es = 1.0 - SQUARE(temp)
	e = dsqrt(es)
	e0 = e0fn(es)
	e1 = e1fn(es)
	e2 = e2fn(es)
	e3 = e3fn(es)
	ml0 = r_major * mlfn(e0, e1, e2, e3, lat_origin)
	esp = es / (1.0 - es)

	if (es < .00001) then
	   ind = 1
	else 
	   ind = 0
	end if

	!/* Universal Transverse Mercator inverse equations--mapping x,y to lat,long 
	!   Note:  The algorithm for UTM is exactly the same as TM and therefore
	!	  if a change is implemented, also make the change to TMINV.c
	!  -----------------------------------------------------------------------*/


	!/* fortran code for spherical form 
	!--------------------------------*/
	if (ind/= 0) then
		f = dexp(x/(r_major * scale_factor))
		g = .5 * (f - 1/f)
		temp = lat_origin + y/(r_major * scale_factor)
		h = dcos(temp)
		con = dsqrt((1.0 - h * h)/(1.0 + g * g));
		lat = asinz(con)
		if (temp < 0) then
			lat = -lat
		end if
		if ((g == 0) .and. (h == 0)) then
			lon = lon_center
			!return(OK)
		else
			lon = adjust_lon(datan2(g,h) + lon_center)
			!return(OK);
		end if
	end if

!/* Inverse equations
!  -----------------*/
	x = x - false_easting;
	y = y - false_northing;
	con = (ml0 + y / scale_factor) / r_major;
	phi = con;
	i=0
	do while (i<=max_iter)
		i=i+1
		delta_phi=((con + e1 * dsin(2.0*phi) - e2 * dsin(4.0*phi) + e3 * dsin(6.0*phi))/ e0) - phi;
		phi = phi + delta_phi
		if (fabs(delta_phi) <= EPSLN) then
			goto 9
		end if
		if (i == max_iter) then
			print *, "Latitude failed to converge in UTM-INVERSE"
			read (*,*)
		end if
	end do

9	if (fabs(phi) < HALF_PI) then
		call sincos(phi, sin_phi, cos_phi)
		tan_phi = dtan(phi)
		c    = esp * SQUARE(cos_phi)
		cs   = SQUARE(c)
		t    = SQUARE(tan_phi)
		ts   = SQUARE(t)
		con  = 1.0 - es * SQUARE(sin_phi);
		n    = r_major / dsqrt(con)
		r    = n * (1.0 - es) / con
		d    = x / (n * scale_factor)
		ds   = SQUARE(d)
		lat = phi - (n * tan_phi * ds / r) * (0.5 - ds / 24.0 * (5.0 + 3.0 * t + &
          10.0 * c - 4.0 * cs - 9.0 * esp - ds / 30.0 * (61.0 + 90.0 * t + &
          298.0 * c + 45.0 * ts - 252.0 * esp - 3.0 * cs)))
		lon = adjust_lon(lon_center + (d * (1.0 - ds / 6.0 * (1.0 + 2.0 * t + &
          c - ds / 20.0 * (5.0 - 2.0 * c + 28.0 * t - 3.0 * cs + 8.0 * esp + &
          24.0 * ts))) / cos_phi))
	else
		lat = HALF_PI * sign_(y)
		lon = lon_center
	end if
end subroutine utminv
!**********************************************************************************
