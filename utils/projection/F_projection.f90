 !  Function POW
!  --------------------------------------- 
DOUBLE PRECISION FUNCTION pow(a,b)
	! E. Chaxel June 2002
	! chaxeleric@yahoo.fr
	! DRP version 1.0
	IMPLICIT NONE
	DOUBLE PRECISION :: a,b
	pow=a**b
END FUNCTION pow


double precision FUNCTION square(x)
	IMPLICIT NONE
	DOUBLE PRECISION :: x
	square=x*x
END FUNCTION SQUARE

!  Function FABS
!  ---------------------------------------
DOUBLE PRECISION FUNCTION fabs(x)
	IMPLICIT NONE
	DOUBLE PRECISION :: x
	fabs=ABS(x)
END FUNCTION fabs

!  Function SIGN_ to return the sign of an argument
!  ---------------------------------------
DOUBLE PRECISION FUNCTION sign_(x) 
	IMPLICIT NONE
	DOUBLE PRECISION :: x
	IF (x < 0) THEN
		sign_=-1
	ELSE
		sign_=1
	END IF
END FUNCTION sign_

!  Function to calculate the sine and cosine in one call.  Some computer
!  systems have implemented this function, resulting in a faster implementation
!  than calling each function separately.  It is provided here for those
!  computer systems which don`t implement this function
!  ----------------------------------------------------
SUBROUTINE sincos(val, sin_val, cos_val) 
	IMPLICIT NONE
	DOUBLE PRECISION  :: val
	DOUBLE PRECISION, INTENT(OUT) :: sin_val 
	DOUBLE PRECISION, INTENT(OUT)  :: cos_val
	sin_val = DSIN(val)
	cos_val = DCOS(val)
END SUBROUTINE

!  Function to adjust a longitude angle to range from -180 to 180 radians
!  added if statments
!  -----------------------------------------------------------------------
DOUBLE PRECISION FUNCTION adjust_lon(x) 

	IMPLICIT NONE	
	include 'constantes.ext'
	DOUBLE PRECISION  :: x !	/* Angle in radians
	DOUBLE PRECISION, EXTERNAL :: fabs, sign_
	 
	INTEGER :: count
	count=0
	DO WHILE (count<=MAX_VAL)
		IF (fabs(x)<=PI) THEN
			GO TO 5
		ELSE
			IF (INT(fabs(x / PI))< 2) THEN
				x = x-(SIGN_(x) *TWO_PI)
			ELSE
				IF (INT(fabs(x / TWO_PI)) < MAXLONG) THEN
					x = x-(INT((x / TWO_PI))*TWO_PI)
				ELSE
					IF (INT(fabs(x / (MAXLONG * TWO_PI)))<MAXLONG) THEN
						x = x-(INT((x / (MAXLONG * TWO_PI))) * (TWO_PI * MAXLONG))
					ELSE
						IF (INT(fabs(x / (DBLLONG * TWO_PI)))<MAXLONG) THEN
							x = x-(INT((x / (DBLLONG * TWO_PI))) * (TWO_PI * DBLLONG))
						ELSE
							x = x-(SIGN_(x) *TWO_PI)
						END IF
					ENDIF
				END IF
			END IF
		END IF
		count=count+1
	END DO
5	adjust_lon=x

END FUNCTION adjust_lon

!  Function to compute the constant small t for use in the forward
!  computations in the Lambert Conformal Conic and the Polar
!  Stereographic projections.
!  -------------------------------------------------------------
DOUBLE PRECISION FUNCTION tsfnz(eccent_,phi_,sinphi_)
	IMPLICIT NONE
	include 'constantes.ext'
	DOUBLE PRECISION :: con_
	DOUBLE PRECISION :: eccent_ !	/* Eccentricity of the spheroid
	DOUBLE PRECISION :: phi_		!/* Latitude phi
	DOUBLE PRECISION :: sinphi_	!/* Sine of the latitude
	DOUBLE PRECISION :: com_
	con_ = eccent_ * sinphi_
	com_ = .5 * eccent_
	con_ = ((1.0 - con_) / (1.0 + con_))**com_
	tsfnz=DTAN(.5 * (HALF_PI - phi_))/con_
END FUNCTION tsfnz


!  Function to compute the constant small m which is the radius of
!  a parallel of latitude, phi, divided by the semimajor axis.
!  ---------------------------------------------------------------
DOUBLE PRECISION FUNCTION msfnz (eccent_,sinphi_,cosphi_)
	IMPLICIT NONE
	DOUBLE PRECISION  :: eccent_
	DOUBLE PRECISION  :: sinphi_
	DOUBLE PRECISION  :: cosphi_
	DOUBLE PRECISION  :: con_
	con_ = eccent_ * sinphi_
	msfnz= cosphi_ / DSQRT(1.0 - con_**2)
END FUNCTION msfnz

!  Function to compute the latitude angle, phi2, for the inverse of the
!  Lambert Conformal Conic and Polar Stereographic projections.
!  ----------------------------------------------------------------
DOUBLE PRECISION FUNCTION phi2z(eccent,ts,flag)
	IMPLICIT NONE
	include 'constantes.ext'
	DOUBLE PRECISION :: eccent		!/* Spheroid eccentricity
	DOUBLE PRECISION :: ts		!/* Constant value t
	INTEGER :: flag,i !		/* Error flag number
	DOUBLE PRECISION :: eccnth
	DOUBLE PRECISION :: phi
	DOUBLE PRECISION :: con
	DOUBLE PRECISION :: dphi
	DOUBLE PRECISION :: sinpi
	double precision :: fabs
	flag = 0
	eccnth = .5 * eccent
	phi = HALF_PI - 2 * DATAN(ts)
	DO i=0,15
		sinpi = DSIN(phi)
		con = eccent * sinpi;
		dphi = HALF_PI - 2 * DATAN(ts *((1.0 - con)/(1.0 + con))**eccnth)-phi
		phi = phi+dphi
		IF (fabs(dphi) <=.0000000001) THEN
			phi2z=phi
			flag=1
		END IF
	END DO
	IF (flag==0) THEN
		PRINT*, "Convergence error ","phi2z"
	END IF

END FUNCTION phi2z


! FUNCTIONS FOR UTM TRANSFORMATION


!/* Functions to compute the constants e0, e1, e2, and e3 which are used
!   in a series for calculating the distance along a meridian.  The
!   input x represents the eccentricity squared.
!----------------------------------------------------------------*/
double precision function e0fn(x) 
	implicit none
	double precision :: x 
	e0fn=1.0-0.25*x*(1.0+x/16.0*(3.0+1.25*x))
end function

double precision function e1fn(x) 
	implicit none
	double precision :: x
	e1fn=0.375*x*(1.0+0.25*x*(1.0+0.46875*x))
end function

double precision function e2fn(x) 
	implicit none
	double precision :: x
	e2fn=0.05859375*x*x*(1.0+0.75*x)
end function

double precision function  e3fn(x) 
	implicit none
	double precision :: x
	e3fn=x*x*x*(35.0/3072.0)
end function

!/* Function to compute the constant e4 from the input of the eccentricity
!   of the spheroid, x.  This constant is used in the Polar Stereographic
!   projection.
!--------------------------------------------------------------------*/
double precision function e4fn(x)
	implicit none
	double precision :: x
	double precision :: con
	double precision :: com
	double precision :: pow
	con = 1.0 + x;
	com = 1.0 - x;
	e4fn=dsqrt(pow(con,con)*pow(com,com))
end function

!/* Function computes the value of M which is the distance along a meridian
!   from the Equator to latitude phi.
!------------------------------------------------*/
double precision function mlfn(e0,e1,e2,e3,phi)
	implicit none
	double precision :: e0,e1,e2,e3,phi
	mlfn=e0*phi-e1*dsin(2.0*phi)+e2*dsin(4.0*phi)-e3*dsin(6.0*phi)
end function



!/* Function to eliminate roundoff errors in asin
!----------------------------------------------*/
double precision function asinz (con)
	implicit none
	double precision :: con
	double precision :: fabs
	if (fabs(con) > 1.0) then
		if (con > 1.0) then
			con = 1.0
		else
			con = -1.0
		end if
	end if
	asinz=dasin(con)
end function asinz


!/* Function to calculate UTM zone number--NOTE Longitude entered in DEGREES!!!
!  ---------------------------------------------------------------------------*/
double precision function calc_utm_zone(lon)
	implicit none
	include 'constantes.ext'
	double precision :: lon !en radians
	calc_utm_zone=dint(((lon*R2D + 180.0) / 6.0) + 1.0)
end function
