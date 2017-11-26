C************************************************************
       subroutine projection_params(proj,typ_proj,
     &     lat_origin,false_east,false_north,scale_fact,
     &     c_lat_D, c_lon_D, lat1_D, lat2_D,false_east_D, false_north_D)
C*********************************************************************
C*	       PARAMETRES DES PROJECTIONS	                     *
C*********************************************************************
                
       implicit none

!      inputs
       integer :: proj

!      outputs
!------Paramètres des projections UTM
       double precision  lat_origin    !Origin latitude
       double precision  false_east    !False easting
       double precision  false_north   !False northing
       double precision  scale_fact    !Scale factor
!------Paramètres des projections LAMBERT CC
       double precision  c_lat_D       !center latitude in degree
       double precision  c_lon_D       !center longitude in degree
       double precision  lat1_D        !first standard parallel in degree
       double precision  lat2_D        !second standard parallel in degree
       double precision  false_east_D  !x offset in meters
       double precision  false_north_D ! y offset in meters
       integer :: typ_proj	  
       
       typ_proj = 0
       
       if(proj.eq.2) then !UTM
	
	 typ_proj = 2
	 lat_origin=0
         false_east=500000
         false_north=0
         scale_fact=0.9996

       else if(proj.eq.1) then !LAMBCC LAMB2 ETENDU       

	typ_proj = 1
	c_lat_D= 46.8	
        c_lon_D=  2.33722917
        lat1_D=  45.898919
        lat2_D=  47.696014
        false_east_D=600000
        false_north_D=2200000
c	scale_fact=0.99987742 !inutile

       else if(proj.eq.3) then !LAMBCC LAMB-93

        typ_proj = 1
	c_lat_D= 46.5
        c_lon_D=  3.0
        lat1_D=  44.0
        lat2_D=  49.0
        false_east_D=700000
	false_north_D=6600000
c	scale_fact=0.99905103 !inutile

       end if
       
c       write(*,*)'projection_params: typ_proj=',typ_proj
              
       end subroutine

C************************************************************
       SUBROUTINE ellipsoide(gs,a,b,e,f)
C*********************************************************************
C*	       TRANSFORMATION DE SYSTEMES GEODESIQUES PAR LA	     *
C*   METHODE DES 7 PARAMETRES ET DU PASSAGE CARTESIEN/GEOGRAPHIQUE   *
C*    REFERENCE:	 WWW.IGN.FR				     *
C*    AUTHOR:		 E. CHAXEL				     *
C*			 LABORATOIRE DES ECOULEMENTS GEOPHYSIQUES    *
C*			 ET INDUSTRIELS 			     *
C*			 BP49 - 38041 GRENOBLE  		     *
C*    LAST MODIFICATION: 10 APRIL 2003  			     *
C*********************************************************************
        implicit none
C-------Entrée
        integer gs !Syteme geodesique 1=WGS84,2=ED50,3=NTF,4=GRS80,5=USER             
C-------Sortie
        double precision  a !Demi-grand axe de l'ellipsoide (en metres)
        double precision  b !Demi-petit axe de l'ellipsoide (en meres)      
        double precision  e !Premiere excentricite de l'ellipsoide
        double precision  f !Applatissement  
C-------Parametres de l'ellipsoide	
        double precision aref(10) !Demi-grands axes des l'ellipsoides (en metres)   
        double precision fref(10) !Applatissement (flatening) des l'ellipsoides
C----Données des ellipsoides (ATTENTION: NE PAS MODIFIER)
C------DEFINITIONS
C       e=first excentricity of the ellipsoid
C       f=flattening
C       f=(a-b)/a
C       e**2=(a**2-b**2)/a**2=1-b**2/a**2
C-------WGS84
        aref(1) = 6378137.000
	fref(1) = 1/298.257223563
C-------ED50
	aref(2) = 6378388.000
	fref(2) = 1/297.
C-------NTF
        aref(3) = 6378249.2 
	fref(3) = 1/293.466021
C-------GRS80
        aref(4) = 6378137.000
	fref(4) = 1/298.257222101	
C-------USER DEFINED
        aref(5) = 6370000.
	fref(5) = 0
C-------Definition des variables b, f et e	
        a = aref(gs)
	f = fref(gs)
	b = (1-f)*a
	e = dsqrt(1-b**2/a**2)				
      END 
