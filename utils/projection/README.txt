Coordinate system conversion package
REFERENCE:
WWW.IGN.FR and WWW.USGS.GOV
AUTHOR:
E. CHAXEL - LABORATOIRE DES ECOULEMENTS GEOPHYSIQUES ET INDUSTRIELS 
BP49 - 38041 GRENOBLE 
LAST MODIFICATION: 4 OCTOBER 2003

Summary

A.	PURPOSE	1
B.	INSTALLATION	1
C.	ROUTINES	2
1.	Ellipsoid	2
2.	Georef	2
3.	Projectfor2	2
4.	Projectinv2	2
D.	INCLUDE: PARAMETERS.INCLUDE	3

A. Purpose
This package is an ensemble of FORTRAN library for UNIX and executables for windows that perform coordinate transformations: based on USGS Coordinates Transformation package and on IGN algorithms, these tools perform ellipsoid transformation (such as ED50 to WGS84) and projections between UTM, Lambert Conical Conformal and Geographic projections. These tools are suitable on Windows and Unix environments.
B. Installation
1. Download the archive project.zip.
2. Create a directory where you unzip the archive. You can define a system variable under UNIX by export $PROJ=path or setenv $PROJ path.
3. Unzip the archive in this directory.
4. This package is composed of source files written in FORTRAN 90:
* P_project.f: program to test the package
* R_project2.f: routines ellipsoid, projectinv2 and projectfor2
* F_projection.f90: Ensemble of functions used by routines 
* R_georef.f: Routines used to change ellipsoid and to perform conversion from geographic to Cartesian coordinates. Adapted from IGN algorithms.
* R_usgs.f90: routines converted to FORTRAN 90 from the USGS Coordinate Transformation package of USGS.
* parameters.include: parameters for Lambert and UTM transformation.
* libproj.a: library with the compiled objects. Use with –L/$PROJ –lproj.
5. To use the test program in P_project.f just type under UNIX: 
f90 –o project *.f* in the directory where you placed the source code.
6. To use with other UNIX FORTRAN programs add in the compilation options –L/$PROJ –lproj with $PROJ the path of the directory where you unzipped the archive.
7. Under windows you can use the compiled executable PROJECT.exe.
C. Routines
The three main routines are ellipsoid, georef, projectfor2 and project2inv.
1. Ellipsoid
* Where: in R_project2.f
* Routine ellipsoid gets parameters of the ellipsoids WGS84, ED50, NTF and user-defined ellipsoid.
* Usage: CALL ellipsoide(gs,a,b,e,f)

integer gs !Syteme geodesique 1=WGS84,2=ED50,3=NTF,4=GRS80,5=USER             
C-------Sortie
double precision  a !Demi-grand axe de l'ellipsoide (en metres)
double precision  b !Demi-petit axe de l'ellipsoide (en meres)      
double precision  e !Premiere excentricite de l'ellipsoide
double precision  f !Applatissement  
C-------Parametres de l'ellipsoide	
double precision aref(10) !Demi-grands axes des l'ellipsoides (en metres)   
double precision fref(10) !Applatissement (flatening) des l'ellipsoides
2. Georef
* Where in R_georef.f
* Routine georef performs ellipsoid transformation with the method of 7 parameters and Cartesian/geographic conversion
* Usage: CALL georef(gs1,lon1,lat1,h1,gs2,lon2,lat2,h2)

integer gs1,gs2            !Systèmes géodésiques (1=WGS84,2=ED50,3=NTF,4=GRS80,5=USER)	
double precision lon1,lat1 !longitude, latitude d'entrée en radians
double precision h1        !hauteur au dessus de l'ellipsoide gs1
double precision lon2,lat2 !longitude, latitude de sortie en radians
double precision h2	   !hauteur au dessus de l'ellipsoide gs2		
double precision X1,Y1,Z1  !coordonnées cartésiennes d'entrée
double precision X2,Y2,Z2  !coordonnées cartésiennes de sortie
double precision X,Y,Z     !coordonnées cartésiennes temporaires	
double precision a1,a2     !Demi-grands axes des l'ellipsoides (en mètres)
double precision b1,b2     !Demi-petits axes des l'ellipsoides (en mètres)
double precision e1,e2     !Première excentricité de l'ellipsoide
3. Projectfor2
* Where: in R_project2.f
* Routine projectfor2 performs conversion from geographic to UTM or Lambert coordinate system.
* Usage: CALL projectfor2(gs,lon_,lat_,x,y,zone,proj,div)

double precision  lon_, lat_ !Input coordinates in radians
double precision  lon, lat, 
double precision  x, y, zone !Output coordinates
double precision  calc_utm_zone
double precision  div ! meters: div=1 ; kilometers: div=1000
integer  gs !geodesic system(1=WGS84,2=ED50,3=NTF,4=GRS80,5=USER)	
integer   proj !1=LAMBERTCC ; 2=UTM ; 0=NO PROJECTION
4. Projectinv2
* Where: in R_project2.f
* Routine projectinv2 performs conversion from UTM or Lambert to geographic coordinate system.
* Usage: CALL projectinv2(gs,x_,y_,zone,lon,lat,proj,div)

double precision  x, y 
double precision  lon, lat   !Output coordinates in radians
double precision  x_, y_     !Input in m(div=1) or km(div=1000)
double precision  zone       !UTM zone
double precision  div        !meters: div=1 ; kilometers: div=1000
integer  gs                  !geodesic system
                             !1=WGS84, 2=ED50, 3=NTF, 4=GRS80, 5=USER
integer  proj                !1=LAMBERTCC ; 2=UTM ; 3=NO PROJECTION
D. Include: parameters.include
Parameters for Lambert Conic Conformal projection are contained in the file parameters.include. Modify this file if necessary and compile the program with: f90 –o project *.f*.

For all remarks: eric.chaxel@hmg.inpg.fr.
