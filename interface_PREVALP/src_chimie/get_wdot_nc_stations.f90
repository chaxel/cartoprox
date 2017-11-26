      program get_wdot_nc
! Ecrit les donnees a des points fournits dans un fichier X,Y dans le NetCDF CHIMERE 
! version 2

      use netcdf
      use typesizes       
      

      implicit none
      
      !real,allocatable :: smax(:,:),savg(:,:)
      real,allocatable :: lon(:,:),lat(:,:)
      real,allocatable :: var_data(:,:,:,:) 
      real,allocatable :: phno3(:,:,:)   
      real,allocatable :: pnh3(:,:,:)                   
      real,allocatable :: hlay(:,:,:)      
      
      character(10),allocatable :: varname(:)
      integer,allocatable :: vardim(:)
      real,allocatable    :: varfac(:)            
      real,allocatable :: vars(:,:)      
      
      
      integer :: n,nvar
      integer :: date, idate
      integer :: ihour      
      real :: jdate
      character(len=256) :: fni,fns
      real :: ppb2ug
      integer :: ix,iy,iz,ns,nstat
      integer :: i,j,i2,j2
      integer :: it, itstart
      character(len=4):: istr
      integer, parameter :: nstatmax=10000
      ! donnees aux stations ou sur la grille de krigeage
      ! donnees a interpoler vers la grille de CHIMERE
      !real :: lons(nstatmax),lats(nstatmax),concs(nstatmax)  
      !real :: xs(nstatmax)  ,ys(nstatmax)
      integer :: nx0,ny0
      real, allocatable :: lons(:),lats(:),concs(:)
      real, allocatable :: station_X(:), station_Y(:), station_Z(:)
      real :: pluie0
      integer,allocatable :: ix1(:),iy1(:)
      real,allocatable    :: wx1(:),wy1(:) 
      
!  ----- interpolation verticalz
      integer,allocatable :: iz1(:)
      real,allocatable    :: wz1(:) 
            
      
      integer :: ndays,level
      integer :: nxx,nyy,nlev
      integer*4 :: deb
      real rtemp,rsearch
      
      integer :: nx(nstatmax),ny(nstatmax)      

      real :: dist
      
!--------NetCDF
      integer :: numSteps	     !Step number
      integer :: timeStringLen       !Duration of one time step (in minutes)
      real    :: distGrid	     !Distance of grid cells (in meters)
      
      integer :: startYear	     !Four-digit year of start time
      integer :: startMnth	     !Month of start time
      integer :: startDay	     !Day of start time
      integer :: startHour	     !Hour of start time
      integer :: startMin	     !Minute of start time
      integer :: startSec	     !Second of start time  
      
      integer :: Year,Mnth,Day,Hour,Minu,Sec
      
      character(len=19) :: datestr
      character(len=10) :: datestr2

! Date de debut et de fin d'extraction      
      character(len=19) :: deb_str
      character(len=19) :: fin_str      
      logical :: iextract ! extrait l'heure ?

      integer :: ncFileID
      integer :: lonVarID,latVarID,xVarID,lonDimId,latDimId,frTimeDimId,zDimID
      
      character(len=10) :: pol
      
      integer :: julian
      
      integer :: ios
      
! ------ UTM
      double precision  :: h1, h2, lon2r1, lat2r1, lon2r2, lat2r2, x2, y2, projunits
      integer           :: projtype=2 ! UTM
      double precision  :: utmzone=31
      integer :: gs1, gs2
      double precision :: d2r	 ! Degrees   ->Radians	 
      double precision :: r2d	 ! Radians   ->Degrees	 	     
      parameter( d2r=.0174532925199	   )
      parameter( r2d=57.2957795131	   )   

! VERSION 2
      integer :: option_chimie
      integer :: option_chimie_n
      integer :: n1,n2

!--------------------------------- especes gazeuses  
      real,allocatable :: conc(:,:,:)         
!------------------------------      

      allocate (lons(nstatmax))
      allocate (lats(nstatmax)) 
      
      allocate (station_X(nstatmax))
      allocate (station_Y(nstatmax))
      allocate (station_Z(nstatmax))
            
      allocate (ix1(nstatmax))
      allocate (iy1(nstatmax))
      allocate (wx1(nstatmax))
      allocate (wy1(nstatmax))
      allocate (iz1(nstatmax))
      allocate (wz1(nstatmax))     

      open(unit=10,file='OUT_CHIMERE.txt',status='old') 
      read(10,*) deb_str, fin_str
      read(10,*) fni
      read(10,*) option_chimie,option_chimie_n  
      close(10)

      write(*,*) 'CHIMERE METEO : Extraction des dates '//deb_str//' --> '//fin_str
      
      ! stations
      fns='coordonnees_stations.txt'              
      level=1

!--------------------------------------------------------------------
      write(*,*) 'Fichier CHIMERE NetCDF '//trim(fni)
      
      call check(nf90_open(fni, nf90_nowrite, ncFileID))

      write(*,*) 'Get dimensions...'      
      call check(nf90_inq_dimid(ncFileID, 'west_east'    , lonDimID))     
      call check(nf90_inq_dimid(ncFileID, 'south_north'  , latDimID))
      !call check(nf90_inq_dimid(ncFileID, 'bottom_top'  , zDimID))  
      call check(nf90_inq_dimid(ncFileID, 'Time'        , frTimeDimID))                     
      call check(nf90_Inquire_Dimension(ncFileID, latDimID   , len=nyy))
      call check(nf90_Inquire_Dimension(ncFileID, lonDimID   , len=nxx))    
      !call check(nf90_Inquire_Dimension(ncFileID, zDimID     , len=nlev))
      nlev = 1 ! utilise uniquement le niveau 1
      call check(nf90_Inquire_Dimension(ncFileID, frTimeDimID, len= numSteps))    
      		      
      TimeStringLen=60.
      write(*,*) 'Dimensions',nxx,nyy,nlev
      write(*,*) 'Grille (m)',distgrid
      write(*,*) 'Pas de temps dispo. ',numSteps      

!--------------------------------------------------------------------      
!     Find date of observations
      startYear  = int(deb/1000000)
      startMnth  = int((deb-startYear*1000000)/10000)
      startDay   = int((deb-startYear*1000000-startMnth*10000)/100)
      startHour  = int((deb-startYear*1000000-startMnth*10000-startDay*100))   
       
!--------------------------------------------------------------------  
      allocate(lon (nxx,nyy))
      allocate(lat (nxx,nyy))
      allocate(hlay(nxx,nyy,nlev))      
                             
      ! Nombre de variables
      !!!!!!!!!! A CHANGER !!!!!
      nvar = 4
      !!!!!!!!!!!!!!!!!!!!!!!!!!            
      allocate(varname(nvar))
      allocate(vardim(nvar))  
      allocate(varfac(nvar))   
      
      varname( 1)= 'NO'
      varname( 2)= 'NO2'
      varname( 3)= 'O3'      
      varname( 4)= 'PM10'
      ! toutes variables dimension 3D
      vardim( :)= 3

      varfac( :)= 1     
      varfac( 1)= 1.25
      varfac( 2)= 1.91
      varfac( 3)= 2.      
      
      allocate(var_data(nxx,nyy,nlev,nvar))
      allocate(phno3(nxx,nyy,nlev))
      allocate(pnh3 (nxx,nyy,nlev))
      allocate(vars(nstatmax,nvar))

      vars = 0.        
!--------------------------------------------------
      write(*,*) 'Get coordinates...'
      call check(nf90_inq_varid(ncFileID, 'lon' , lonVarID)) 
      call check(nf90_inq_varid(ncFileID, 'lat' , latVarID))     
    
      call check(nf90_get_var(ncFileID, lonVarID, lon,  start=(/1,1/)))
      call check(nf90_get_var(ncFileID, latVarID, lat,  start=(/1,1/)))
      call check(nf90_close(ncFileID))
!--------------------------------------------------------------------        
! Read Model outputs for temperature at 2 m, q, u and v and 
!-------------------------------------------------------
! Lit les stations
      nstat= nstatmax     
      write(*,*) 'Trouve points:'
!            
      open(unit=10,file=fns,status='old') 
      do i=1,nstat 
         read(10,*,end=20) station_X(i),station_Y(i),station_Z(i)    	 	
         x2 = station_X(i) 
         y2 = station_Y(i)
	 gs1 = 2
	 gs2 = 2
	 h1 = 0.
	 h2 = 0.
	 projunits=1.	       				       
         lons(i) = x2 !lon2r2 * r2d
	 lats(i) = y2 !lat2r2 * r2d 
	 write(*,*) lons(i), lats(i),station_Z(i)  	 
      end do
            
10    continue
      write(*,*) 'ERROR: increase reading input'
      stop
20    continue
      nstat=i-1      
      close(10)
            
! ------------------------------------
! Calcul la correspondance des grilles
!--------------------------------------------------------------------                   
      open(unit=10,file='WDOT',status='unknown')  

      if (option_chimie.eq.1) then

      write(*,*) 'OPTION 1 : Utilise une interpolation bilineaire'
      
      write(*,*) 'Debut du calcul des poids'	
      do i=1,nstat    
        write(*,*) 'Station ',i
        call wcal1(nxx,nyy,ix1(i),iy1(i),wx1(i),wy1(i),lon,lat,lons(i),lats(i))
        write(10,*) i,ix1(i),iy1(i),wx1(i),wy1(i)
        if (ix1(i)*iy1(i).eq.0) then
          write(*,*) 'ERREUR: Point en dehors du domaine - STOP',i,lons(i),lats(i)   
	  stop	
        end if            
      end do  
      
      write(*,*) 'Fin du calcul des poids'      
      
      write(*,*) ' conc ='     
      write(*,*) '(  (1.-wx1)*(1.-wy1)*concs(ix1  ,iy1  ) &'
      write(*,*) ' +     wx1 *(1.-wy1)*concs(ix1+1,iy1  ) &'
      write(*,*) ' + (1.-wx1)*    wy1 *concs(ix1  ,iy1+1) &' 
      write(*,*) ' +     wx1 *    wy1 *concs(ix1+1,iy1+1) )'

      else if (option_chimie.eq.2) then

      write(*,*) 'OPTION 2 : Utilise un voisinage de',3*option_chimie_n,'maille(s)'

      write(*,*) 'Debut du calcul de la correspondance'            
      do i=1,nstat  
        ix1(i) = 0
        iy1(i) = 0
        wx1(i) = 0.
        wy1(i) = 0.		
	rsearch=10000000
        do ix=1,nxx
          do iy=1,nyy
	    rtemp = ((lon(ix,iy)-lons(i))**2+(lat(ix,iy)-lats(i))**2)**.5
	    if (rtemp.lt.rsearch) then
              ix1(i) = ix
              iy1(i) = iy
	      rsearch=rtemp		
	    end if	
	  end do
        end do      
        if (ix1(i)*iy1(i).eq.0) then
          write(*,*) 'Point en dehors du domaine [lon lat]:',lons(i),lats(i)
          stop
	else
	  write(10,*) i,ix1(i),iy1(i),wx1(i),wy1(i)
        end if  	      
      end do                  
      else
      write(*,*) 'Option inconnue:',option_chimie
      end if      
      close(10)  
      
      write(*,*) nstat,'domaines SIRANE -> WDOT'                     
      do i=1,nstat
        write(istr,'("000",I1)') i
        if (i.gt.9)   write(istr,'("00" ,I2)') i
        if (i.gt.99)  write(istr,'("0"  ,I3)') i
        if (i.gt.999) write(istr,'(      I4)') i	      
        open(unit=11,file='./resultats_stations/OUT.STATION.'//istr//'.csv',status='unknown')
        write(11,'(A19,";",20(A10,";"))') 'date',varname(:)			
        close(11)
      end do

      iextract = .false.
      call check(nf90_open(fni, nf90_nowrite, ncFileID))      
! >>>>>>>>>>>>>>>>>> ITERATIONS >>>>>>>>>>>>>>>>>>>>  
      do it=1,numSteps
! >>>>>>>>>>>>>>>>>> ITERATIONS >>>>>>>>>>>>>>>>>>>>                      
      ! date             	      
      call check(nf90_inq_varid(ncFileID, 'Times', xVarID))    
      call check(nf90_get_var(ncFileID, xVarID, datestr, start = (/1,it/) ))
     	      
      read(datestr,'(I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,I2)')&
     	     Year,Mnth,Day,Hour,Minu,Sec               
      !write(*,*) 'Trouve date '//datestr   

      !Extraction d'une periode (l ordre des lignes est IMPORTANT)
      if (datestr.eq.deb_str)iextract = .true. !debut de l'extraction      
      if (.not.iextract)goto 999
      if (datestr.eq.fin_str)iextract = .false.                   
      if(mod(it-1,24).eq.0)write(*,*) 'Extraction de la date '//datestr//' en cours' 
      
      if( mod(it,168).eq.1) write(*,*) datestr,maxval(var_data(:,:,:,1)),maxval(var_data(:,:,:,2)),maxval(var_data(:,:,:,3))
 
      ! SURFACE  
      do n = 1, nvar   
        if (nf90_inq_varid(ncFileID, trim(varname(n)), xVarID).eq.0) then     
          if (it.eq.1) write(*,*) 'Lit '//trim(varname(n))	     
          if (vardim(n).eq.3)call check(nf90_get_var(ncFileID, xVarID, var_data(:,:,:,n) ,  start=(/1,1,1,it/),count=(/nxx,nyy,nlev/))) 
          if (vardim(n).eq.2)call check(nf90_get_var(ncFileID, xVarID, var_data(:,:,:,n) ,  start=(/1,1,it/)  ,count=(/nxx,nyy,1/))) 	  
	else      
	  var_data(:,:,1,n) = -999.
	end if  
      end do           
      
      ! Calcul a la station        
      do i = 1, nstat
          
        ! fichier sortie 
                      write(istr,'("000",I1)') i
        if (i.gt.9)   write(istr,'("00" ,I2)') i
        if (i.gt.99)  write(istr,'("0"  ,I3)') i
        if (i.gt.999) write(istr,'(      I4)') i
      
        open(unit=11,file='./resultats_stations/OUT.STATION.'//istr//'.csv',access='append')  
        open(unit=12,file='./resultats_stations/maxjour.STATION.'//istr//'.csv',access='append')  
        open(unit=13,file='./resultats_stations/moyjour.STATION.'//istr//'.csv',access='append')  		    
        
        ! niveau verticaux	
        iz1(i) = 2
	wz1(i) = 1.0
       
        do n = 1, nvar              
    
	 if (option_chimie.eq.1) then
	
	  if ((vardim(n).eq.3).and.(nlev.gt.1)) then	 
	   vars(i,n)=  &  
           (  (1.-wx1(i))*(1.-wy1(i))*(  wz1(i) * var_data(ix1(i)  ,iy1(i)  ,iz1(i)-1,n) + (1- wz1(i)) *  var_data(ix1(i)    ,iy1(i)    ,iz1(i),n) )     &
           +     wx1(i) *(1.-wy1(i))*(  wz1(i) * var_data(ix1(i)+1,iy1(i)  ,iz1(i)-1,n) + (1- wz1(i)) *  var_data(ix1(i)+1  ,iy1(i)    ,iz1(i),n) )     &
           + (1.-wx1(i))*	   wy1(i) *(  wz1(i) * var_data(ix1(i)  ,iy1(i)+1,iz1(i)-1,n) + (1- wz1(i)) *  var_data(ix1(i)    ,iy1(i)+1  ,iz1(i),n) )     &
           +     wx1(i) *	   wy1(i) *(  wz1(i) * var_data(ix1(i)+1,iy1(i)+1,iz1(i)-1,n) + (1- wz1(i)) *  var_data(ix1(i)+1  ,iy1(i)+1  ,iz1(i),n) )     ) &
	   * varfac(n)	 
	   else	 
	   vars(i,n)=  &  
           (  (1.-wx1(i))*(1.-wy1(i))*   var_data(ix1(i)  ,iy1(i)  ,iz1(i)-1,n)  	&
           +     wx1(i) *(1.-wy1(i))*   var_data(ix1(i)+1,iy1(i)  ,iz1(i)-1,n)  	&
           + (1.-wx1(i))*	   wy1(i) *   var_data(ix1(i)  ,iy1(i)+1,iz1(i)-1,n)  	&
           +     wx1(i) *	   wy1(i) *   var_data(ix1(i)+1,iy1(i)+1,iz1(i)-1,n)  	) &
	   * varfac(n)	 	 
	   end if
	 
	  else if (option_chimie.eq.2) then
	 
	   vars(i,n)=0	 
	   do n1 = -1*option_chimie_n, option_chimie_n
	     do n2 = -1*option_chimie_n, option_chimie_n
	       ix = max(1,ix1(i)+n1)
	       iy = max(1,iy1(i)+n2)
	       if (ix.gt.nxx) ix=nxx
	       if (iy.gt.nyy) iy=nyy	       
	       vars(i,n) = vars(i,n) + (var_data(ix,iy,1,n)* varfac(n))/((option_chimie_n*2+1)**2)	 
	     end do
	   end do	 	 
	 
	 end if
	 	 	 
	end do     
        write(11,'(I4,";",A19,";",20(F10.2,";"))') it,datestr,vars(i, :)
        close(11)	
      
      end do
      
999   continue      
! >>>>>>>>>>>>>>>>>> ITERATIONS >>>>>>>>>>>>>>>>>>>>        
      end do
! >>>>>>>>>>>>>>>>>> ITERATIONS >>>>>>>>>>>>>>>>>>>>           
      call check(nf90_close(ncFileID)) 
!---------------------------------------------------------------------------                  
 
      write(*,*) 'Normal terminason of get_wdot_nc'   
      write(*,*) 'Sorties :'       
      write(*,*) '* fichier WDOT'
      write(*,*) '* fichier OUT.STATION.*.txt'
      end
