      subroutine georef(gs1,lon1,lat1,h1,gs2,lon2,lat2,h2)	
C      *********************************************************************
C      *             TRANSFORMATION DE SYSTEMES GEODESIQUES PAR LA         *
C      *   METHODE DES 7 PARAMETRES ET DU PASSAGE CARTESIEN/GEOGRAPHIQUE   *
C      *    REFERENCE:         WWW.IGN.FR                                  *
C      *    AUTHOR:            E. CHAXEL                                   *
C      *                       LABORATOIRE DES ECOULEMENTS GEOPHYSIQUES    *
C      *                       ET INDUSTRIELS                              *
C      *		       BP49 - 38041 GRENOBLE                       *
C      *		       CHAXEL@HMG.INPG.FR                          *
C      *    LAST MODIFICATION: 10 APRIL 2003                               *
C      *********************************************************************

C----Cette routine transforme les coordonnées géographique d'un système vers un 
C----autre. Les données en entrées sont notées 1 et les données en sortie 2.
C----Il faut fournir obligatoirement a2 et e2. Pour l'instant les systèmes supportés sont
C----WGS84, ED50 et NTF. On les définit par : 1 = WGS84, 2 = ED50, 3 = NTF, 4=GRS80
C----Les vecteurs rotation et translation sont définis avec les valeurs de l'IGN pour la France
C----Rapports techniques TR/G n°7 et n°14
C----Pour l'instant le système GRS80 n'est pas supporté mais il est définit dans la routine
C----Routine body
        implicit none
C----Input
	integer gs1,gs2            !Systèmes géodésiques (1=WGS84,2=ED50,3=NTF,4=GRS80,5=USER)	
        double precision lon1,lat1 !longitude, latitude d'entrée en radians
	double precision h1        !hauteur au dessus de l'ellipsoide gs1 (m)
	double precision lon2,lat2 !longitude, latitude de sortie en radians
	double precision h2	   !hauteur au dessus de l'ellipsoide gs2 (m)		
	double precision X1,Y1,Z1  !coordonnées cartésiennes d'entrée
	double precision X2,Y2,Z2  !coordonnées cartésiennes de sortie
	double precision X,Y,Z     !coordonnées cartésiennes temporaires	
	double precision a1,a2     !Demi-grands axes des l'ellipsoides (en mètres)
	double precision b1,b2     !Demi-petits axes des l'ellipsoides (en mètres)	
	double precision e1,e2     !Première excentricité de l'ellipsoide
	double precision f1,f2     !Flattening
	
C-------Paramètres des ellipsoides	
	double precision aref      !Demi-grands axes des l'ellipsoides (en mètres)   
	double precision fref      !Applatissement (flatening) des l'ellipsoides
	dimension aref(10)         !maximum d'ellipsoides=10
	dimension fref(10)	   !maximum d'ellipsoides=10
C----Variables utilisées dans les routines	
	double precision Tx,Ty,Tz  !Translation
	double precision D         !Facteur d'échelle
	double precision Ddef      !Facteur d'échelle par défaut	
	double precision Rx,Ry,Rz  !angles de rotation autour des 3 axes (rad)
	double precision Ux,Uy,Uz  !Input de la transformation à 7 paramètres (coord. cartésiennes)
	double precision Vx,Vy,Vz  !Output de la transformation à 7 paramètres (coord. cartésiennes)  
	double precision epsilon   !precision	  
        double precision lambda    !longitude par rapport au méridien d'origine
        double precision phi       !latitude
	double precision he        !hauteur au-dessus de l'ellipsoide (en mètres)
C----External	
	double precision grandeN   !grande normale de l'ellipsoide (en mètres)


	

        call ellipsoide(gs1,a1,b1,e1,f1)        
	call ellipsoide(gs2,a2,b2,e2,f2)

      if ((gs1.eq.5).or.(gs2.eq.5)) then  
        lon2=lon1
	lat2=lat1
	h2=h1
	goto 99
      end if 
	
	
C----Transformation de lon1,lat1,h1 -> X1,Y1,Z1 dans le système gs1   
        call geo2cart(lon1,lat1,h1,a1,e1,X1,Y1,Z1)
C	write(*,*) 'X1=',X1
C	write(*,*) 'Y1=',Y1
C	write(*,*) 'Z1=',Z1

C----Définit les paramètres de transformation cartésienne à 7 variables  
C----et applique la transformation    
	if ((gs1.EQ.1).AND.(gs2.EQ.2)) then !WGS84->ED50
	  Tx =  -84. !données IGN ED50->WGS84 (utilise par7inv)
	  Ty =  -97. !données IGN ED50->WGS84 (utilise par7inv)
	  Tz = -117. !données IGN ED50->WGS84 (utilise par7inv)
C	  Rx = -0.48488/3600. !DK 
C	  Ry = -0.02436/3600. !DK
C	  Rz = -0.41321/3600. !DK
	  Rx =    0.
	  Ry =    0.
	  Rz =    0.
	  D =    Ddef
        call par7inv(Tx,Ty,Tz,D,Rx,Ry,Rz,X1,Y1,Z1,X2,Y2,Z2)
	
	else if ((gs1.EQ.2).AND.(gs2.EQ.1)) then !ED50->WGS84
	  Tx =  -84. !données IGN ED50->WGS84 (utilise par7for)
	  Ty =  -97. !données IGN ED50->WGS84 (utilise par7inv)
	  Tz = -117. !données IGN ED50->WGS84 (utilise par7inv)
C	  Rx = -0.48488/3600. !DK 
C	  Ry = -0.02436/3600. !DK
C	  Rz = -0.41321/3600. !DK
	  Rx =    0.
	  Ry =    0.
	  Rz =    0.
	  D =    Ddef
          call par7for(Tx,Ty,Tz,D,Rx,Ry,Rz,X1,Y1,Z1,X2,Y2,Z2)
	  
      else if ((gs1.EQ.1).AND.(gs2.EQ.3)) then !WGS84->NTF
	  Tx = -168.  !données IGN NTF->WGS84 
	  Ty =  -60.  !données IGN NTF->WGS84 
	  Tz =  320.  !données IGN NTF->WGS84 
	  Rx =    0.
	  Ry =    0.
	  Rz =    0.
	  D =    Ddef
          call par7inv(Tx,Ty,Tz,D,Rx,Ry,Rz,X1,Y1,Z1,X2,Y2,Z2)
	  
      else if ((gs1.EQ.3).AND.(gs2.EQ.1)) then !NTF->WGS84
	  Tx = -168.  !données IGN NTF->WGS84 
	  Ty =  -60.  !données IGN NTF->WGS84 
	  Tz =  320.  !données IGN NTF->WGS84 
	  Rx =    0.
	  Ry =    0.
	  Rz =    0.
	  D =    Ddef
          call par7for(Tx,Ty,Tz,D,Rx,Ry,Rz,X1,Y1,Z1,X2,Y2,Z2)  
	      
       else if ((gs1.EQ.2).AND.(gs2.EQ.3)) then !ED50->NTF
	  Tx =  -84.  !données IGN NTF->ED50
	  Ty =   37.  !données IGN NTF->ED50
	  Tz =  437.  !données IGN NTF->ED50
	  Rx =    0.
	  Ry =    0.
	  Rz =    0.
	  D =    Ddef
          call par7inv(Tx,Ty,Tz,D,Rx,Ry,Rz,X1,Y1,Z1,X2,Y2,Z2)
	  
      else if ((gs1.EQ.3).AND.(gs2.EQ.2)) then !NTF->ED50
	  Tx = -84.  !données IGN NTF->ED50
	  Ty = +37.  !données IGN NTF->ED50
	  Tz = 437.  !données IGN NTF->ED50
	  Rx =   0.
	  Ry =   0.
	  Rz =   0.
	  D =   Ddef
          call par7for(Tx,Ty,Tz,D,Rx,Ry,Rz,X1,Y1,Z1,X2,Y2,Z2)    
	  
      else if (gs1.EQ.gs2) then
          X2=X1
	  Y2=Y1
	  Z2=Z1	  	   	           	    
      else if ((gs1.eq.5).or.(gs2.eq.5)) then  
          X2=X1
	  Y2=Y1
	  Z2=Z1	                 
      else
	  write(*,*) 'Translation not supported'
	  stop
      end if
C	write(*,*) 'X2=',X2
C	write(*,*) 'Y2=',Y2
C	write(*,*) 'Z2=',Z2
C----Transformation cartésienne->géographiques
	epsilon = 1.0D-11
      call cart2geo(a2,e2,X2,Y2,Z2,epsilon,lon2,lat2,h2)
99    continue      
      
	end


C Compute ED50 WGS84 transformation
C************************************************************** 
      subroutine par7for(Tx,Ty,Tz,D,Rx,Ry,Rz,Ux,Uy,Uz,Vx,Vy,Vz)
C**************************************************************
	implicit none
	double precision Tx,Ty,Tz,D,Rx,Ry,Rz,Ux,Uy,Uz !Input
	double precision Vx,Vy,Vz !Output
          Vx = Tx + Ux * (1+D) + Uz*Ry - Uy*Rz
          Vy = Ty + Uy * (1+D) + Ux*Rz - Uz*Rx
          Vz = Tz + Uz * (1+D) + Uy*Rx - Ux*Ry
	end 
C************************************************************** 
      subroutine par7inv(Tx,Ty,Tz,D,Rx,Ry,Rz,Ux,Uy,Uz,Vx,Vy,Vz)
C**************************************************************
	implicit none
	double precision Tx,Ty,Tz !Translation
	double precision D        !Facteur d'échelle
	double precision Rx,Ry,Rz !angles de rotation autour des 3 axes (rad)
	double precision Ux,Uy,Uz !Input  (coord. cartésiennes)
	double precision Vx,Vy,Vz !Output (coord. cartésiennes)
          Vx = (Tx-Ux) * (D-1) + (Tz-Uz) * Ry - (Ty-Uy) * Rz
          Vy = (Ty-Uy) * (D-1) + (Tx-Ux) * Rz - (Tz-Uz) * Rx
          Vz = (Tz-Uz) * (D-1) + (Ty-Uy) * Rx - (Tx-Ux) * Ry
	end 

C************************************************************** 
      double precision function grandeN(phi,a,e)
C************************************************************** 
C----Calcule la grande normale de l'ellipsoide
 	implicit none     
        double precision phi !latitude
        double precision a   !Demi-grand axe de l'ellipsoide
	double precision e   !Première excentricité de l'ellipsoide
	  grandeN = a / dsqrt(1 - ( e * dsin(phi) )**2)
	end
C************************************************************** 
      double precision function signe(r)
C************************************************************** 
C----Calcule le signe du reel DP r
        implicit none
        double precision r   !réel
        if (r.GE.0) then
          signe=+1
      	else
      	  signe=-1
      	end if
      	end
C**************************************************************
      subroutine geo2cart(lambda,phi,he,a,e,X,Y,Z)
C**************************************************************
C----Transformation de coordonnées géographiques ellipsoidales
C----en coordonnes cartesiennes
        implicit none     
        double precision lambda  !longitude par rapport au méridien d'origine
        double precision phi     !latitude
	double precision he      !hauteur au-dessus de l'ellipsoide
        double precision a       !Demi-grand axe de l'ellipsoide
	double precision e       !Première excentricité de l'ellipsoide
	double precision X,Y,Z   !coordonnées cartésiennes
C----External	
	double precision grandeN !grande normale de l'ellipsoide
	  X = ( grandeN(phi,a,e) + he ) * dcos(phi) * dcos(lambda)
	  Y = ( grandeN(phi,a,e) + he ) * dcos(phi) * dsin(lambda)
	  Z = ( grandeN(phi,a,e) * (1 - e**2) + he ) * dsin(phi)
	end
C**************************************************************
      subroutine cart2geo(a,e,X,Y,Z,epsilon,lambda,phi,he)
C**************************************************************
C----Transformation de coordonnées cartésiennes
C----en coordonnes géographiques ellipsoidales
         implicit none   
        double precision a       !Demi-grand axe de l'ellipsoide
	double precision e       !Première excentricité de l'ellipsoide
	double precision X,Y,Z   !coordonnées cartésiennes
	double precision epsilon !precision	  
        double precision lambda  !longitude par rapport au méridien d'origine
        double precision phi     !latitude
	double precision he      !hauteur au-dessus de l'ellipsoide
C----External	
	double precision grandeN !grande normale de l'ellipsoide
C----Local
        double precision phi0, phi1, phi2
C----Program body
      lambda = 2 * datan( Y / ( X +dsqrt( X**2 + Y**2) ) )
	
      phi0 = datan( Z / 
     &(dsqrt(X**2+Y**2) * (1 - (a * e**2) / dsqrt(X**2 + Y**2 + Z**2))))

      phi1 = datan(  Z / 
     &(dsqrt( X**2 + Y**2 ) * 
     &( 1 - a * e**2 * dcos(phi0) /
     &(dsqrt( X**2 + Y**2 ) * dsqrt(1 - (e*dsin(phi0))**2)
     &))))
     
      if (dabs(phi1-phi0).LT.epsilon) then
        phi=phi1
	go to 88
      else
77      phi2 = datan( Z /
     &         (dsqrt( X**2 + Y**2 ) * 
     &         (1 - a * e**2 * dcos(phi1) / 
     &         (dsqrt( X**2 + Y**2 ) * 
     &          dsqrt(1 - (e*dsin(phi1))**2)))))
        if (dabs(phi2-phi1).LT.epsilon) then 
	  phi=phi2
	  go to 88
	else
	  phi1=phi2
	  go to 77
	end if 
      end if
88    continue

	he = dsqrt(X**2 + Y**2) / dcos(phi) 
     &     - a / dsqrt(1 - (e*dsin(phi))**2)

	end