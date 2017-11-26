      program get_wdot_nc
! Ecrit les donnees a des points fournits dans un fichier X,Y dans le NetCDF CHIMERE 

      use netcdf
      use typesizes       
      

      implicit none
      
      real,allocatable :: dmax(:,:),davg(:,:)
      real,allocatable :: lon(:,:),lat(:,:)
      real,allocatable :: var(:,:,:,:) !u10(:,:), v10(:,:),t2(:,:),q2(:,:),psfc(:,:),rainc(:,:),rainnc(:,:),swdown(:,:)
      real,allocatable :: hlay(:,:,:)      
      
      character(10),allocatable :: varname(:)
      character(10) :: long_name,lat_name      
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
      real :: rtemp,rsearch
      
      integer :: nx(nstatmax),ny(nstatmax)      

      real :: dist, atte
      
!--------NetCDF
      integer :: numSteps	     !Step number
      integer :: timeStringLen       !Duration of one time step (in minutes)
      
      integer :: startYear	     !Four-digit year of start time
      integer :: startMnth	     !Month of start time
      integer :: startDay	     !Day of start time
      integer :: startHour	     !Hour of start time
      integer :: startMin	     !Minute of start time
      integer :: startSec	     !Second of start time  
      
      integer :: Year,Mnth,Day,Hour,Minu,Sec,jDay
      
      character(len=19) :: datestr

! Date de debut et de fin d'extraction      
      character(len=19) :: deb_str
      character(len=19) :: fin_str      
      logical :: iextract ! extrait l'heure ?

      integer :: ncFileID
      integer :: lonVarID,latVarID,xVarID,lonDimId,latDimId,frTimeDimId,zDimID
      
      character(len=10) :: pol
      
      integer :: julian
      
      integer :: ios
      
      integer :: indice_u
      integer :: indice_v
      integer :: indice_dd
      integer :: indice_vv           
      integer :: indice_rainc
      integer :: indice_rainnc        
      
      
! ------ UTM
      double precision  :: h1, h2, lon2r1, lat2r1, lon2r2, lat2r2, x2, y2, projunits
      integer           :: projtype=2 ! UTM
      double precision  :: utmzone=31
      integer :: gs1, gs2
      double precision :: d2r	 ! Degrees   ->Radians	 
      double precision :: r2d	 ! Radians   ->Degrees	 	     
      parameter( d2r=.0174532925199	   )
      parameter( r2d=57.2957795131	   )   
!-------- constantes solaires
      real :: decl_sol, elev_sol, k1_clear, lat_sol
      real, parameter :: na = 6.0221367e23
      real, parameter :: pi = 3.1415926   
      real, parameter :: v0 = 22.4 

!--------------------------------- especes gazeuses  
      real,allocatable :: conc(:,:,:)         
!------------------------------      
      integer :: dimensions

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

      open(unit=10,file='OUT_DIAG.txt',status='old') 
      read(10,*) deb_str, fin_str
      read(10,*) fni
      close(10)

      write(*,*) 'CHIMERE CHIMIE : Extraction des dates '//deb_str//' --> '//fin_str

      ! stations
      fns='coordonnees_stations.txt'   
           
      level=1

!--------------------------------------------------------------------

      call check(nf90_open(fni, nf90_nowrite, ncFileID))

      write(*,*) 'Get dimensions...'      
      call check(nf90_inq_dimid(ncFileID, 'west_east'    , lonDimID))     
      call check(nf90_inq_dimid(ncFileID, 'south_north'  , latDimID))
      call check(nf90_inq_dimid(ncFileID, 'Time'         , frTimeDimID)) 
      
      call check(nf90_Inquire_Dimension(ncFileID, latDimID   , len=nyy))
      call check(nf90_Inquire_Dimension(ncFileID, lonDimID   , len=nxx))          
      call check(nf90_Inquire_Dimension(ncFileID, frTimeDimID, len= numSteps)) 
                        
      if ( nf90_inq_dimid(ncFileID, 'bottom_top'   , zDimID) .eq.0 ) then 
        call check(nf90_Inquire_Dimension(ncFileID, zDimID     , len=nlev))        
      else
        nlev = 1
      end if    

      TimeStringLen=60.
      write(*,*) nxx,nyy,nlev

!--------------------------------------------------------------------      
!     Find date of observations
      startYear  = int(deb/1000000)
      startMnth  = int((deb-startYear*1000000)/10000)
      startDay   = int((deb-startYear*1000000-startMnth*10000)/100)
      startHour  = int((deb-startYear*1000000-startMnth*10000-startDay*100))   
       
!--------------------------------------------------------------------  
      allocate(dmax(nxx,nyy))
      allocate(davg(nxx,nyy))



      allocate(lon (nxx,nyy))
      allocate(lat (nxx,nyy))    
      
      ! Nombre de variables
      nvar = 19
      nlev = 1

      allocate(hlay(nxx,nyy,nlev))        
      allocate(var(nxx,nyy,nlev,nvar))
      allocate(vars(nstatmax,nvar))      
      allocate(varname(nvar))

      indice_u=4
      indice_v=5
      indice_dd=6
      indice_vv=7
      indice_rainc=8
      indice_rainnc=9

      !ATTENTION : ne pas changer les indices 4,5,12 et 13
      varname( 1)='tem2'
      varname( 2)='sreh'
      varname( 3)='w10m'
      varname( 4)='winz'
      varname( 5)='winm'
      varname( 6)='dd10'
      varname( 7)='ff1z'      
      varname( 8)='topc'
      varname( 9)='topc'
      varname(10)='obuk'
      varname(11)='hght'
      varname(12)='usta'	
      varname(13)='topc'    
      varname(14)='atte'
      
      ! variables calculées
      varname(15)='uhto' ! vitesse hauteur des toits	
      varname(16)='sthe' ! sigma theta
      varname(17)='cldo' ! nebulosité en octas    
      varname(18)='k1'   ! constante reaction k1
      varname(19)='k3'   ! constante reaction k3
           
      long_name='lon'
      lat_name= 'lat'
                        
      do iy=1,nyy
        do ix=1,nxx
	  davg(ix,iy) = 0.
          dmax(ix,iy) = 0.  	  
        end do
      end do	

!--------------------------------------------------
      write(*,*) 'Get coordinates...'    
      call check(nf90_inq_varid(ncFileID, long_name , lonVarID)) 
      call check(nf90_inq_varid(ncFileID, lat_name , latVarID))     
    
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
!         read(10,*,end=20) lons(i),lats(i)!,concs(i,j)    
         read(10,*,end=20) station_X(i),station_Y(i),station_Z(i)    	 	
         x2 = station_X(i) 
         y2 = station_Y(i)
	 gs1 = 2
	 gs2 = 2
	 h1 = 0.
	 h2 = 0.
	 projunits=1.	       
	 call projectinv2(gs2,x2,y2,utmzone,lon2r2,lat2r2,projtype,projunits)					       
         lons(i) = lon2r2 * r2d
	 lats(i) = lat2r2 * r2d 
         lons(i) = station_X(i)
	 lats(i) = station_Y(i)
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
      write(*,*) 'Debut du calcul des poids'	
      open(unit=10,file='WDOT',status='unknown')  
      do i=1,nstat    
        write(*,*) 'Station ',i
        call wcal1(nxx,nyy,ix1(i),iy1(i),wx1(i),wy1(i),lon,lat,lons(i),lats(i))
        write(10,*) i,ix1(i),iy1(i),wx1(i),wy1(i)           
        if (ix1(i)*iy1(i).eq.0) then
          write(*,*) 'ERREUR: Point en dehors du domaine - STOP',i,lons(i),lats(i)   
	  stop	
        end if            
      end do  
      close(10)       
      write(*,*) 'Fin du calcul des poids'
      
      write(*,*) nstat,'domaines SIRANE -> WDOT'       

      write(*,*) ' conc ='     
      write(*,*) '(  (1.-wx1)*(1.-wy1)*concs(ix1  ,iy1  ) &'
      write(*,*) ' +     wx1 *(1.-wy1)*concs(ix1+1,iy1  ) &'
      write(*,*) ' + (1.-wx1)*    wy1 *concs(ix1  ,iy1+1) &' 
      write(*,*) ' +     wx1 *    wy1 *concs(ix1+1,iy1+1) )'

      
      do i=1,nstat
                      write(istr,'("000",I1)') i
        if (i.gt.9)   write(istr,'("00" ,I2)') i
        if (i.gt.99)  write(istr,'("0"  ,I3)') i
        if (i.gt.999) write(istr,'(      I4)') i
      
        open(unit=11,file='./resultats_stations/OUT.STATION.'//istr//'.csv',status='unknown')
	write(11,'(A200)') 'i;date_heure;t(deg);sreh(%);dd10;ff10(m/s);obuk;hght(m);usta(m/s);topc(mm);atte;uh(m/s);sigmatheta;cld;k1(s/ppb);k3(s/ppb)'	      
        !write(11,'(A19,1X,10A10)') 'date',varname(:)
        close(11)
      
      end do

      pluie0 = 0. 
      iextract = .false.    
      call check(nf90_open(fni, nf90_nowrite, ncFileID))               
! >>>>>>>>>>>>>>>>>> ITERATIONS >>>>>>>>>>>>>>>>>>>>       
      do it=1,numSteps
! >>>>>>>>>>>>>>>>>> ITERATIONS >>>>>>>>>>>>>>>>>>>>             
      
      ! date             	      
      call check(nf90_inq_varid(ncFileID, 'Times', xVarID))    
      call check(nf90_get_var(ncFileID, xVarID, datestr, start = (/1,it/) ))
     	           
      !Extraction d'une periode (l ordre des lignes est IMPORTANT)
      if (datestr.eq.deb_str)iextract = .true. !debut de l'extraction      
      if (.not.iextract)goto 999
      if (datestr.eq.fin_str)iextract = .false.     
      if(mod(it-1,24).eq.0)write(*,*) 'Extraction de la date '//datestr//' en cours'
      !write(*,*) 'Trouve date '//datestr  

      read(datestr,'(I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,I2)')&
     	     Year,Mnth,Day,Hour,Minu,Sec     
      ! jour julien
      jDay=Julian(Year, Mnth, Day)      
  
      ! 3D
      if ( nlev.gt.1) then
       call check(nf90_inq_varid(ncFileID, 'hlay', xVarID))         
       call check(nf90_get_var(ncFileID, xVarID, hlay(:,:,:) ,  start=(/1,1,1,it/)))           
       hlay(:,:,:) = hlay(:,:,:) - 8.
      else
       hlay(:,:,:) = 0.
      end if
     
      ! SURFACE  
      do n = 1, nvar     
        if (nf90_inq_varid(ncFileID, trim(varname(n)), xVarID).eq.0) then
	if (it.eq.1) write(*,*) 'Lit '//trim(varname(n)) 
	
        call check(nf90_inquire_variable(ncFileID,  xVarID, ndims = dimensions ))     
 
        if ( dimensions .eq. 3 ) then
	  call check(nf90_get_var(ncFileID, xVarID, var(:,:,1:nlev,n) ,  start=(/1,1,it/)))    
        else if ( dimensions .eq. 4 ) then
	  call check(nf90_get_var(ncFileID, xVarID, var(:,:,1:nlev,n) ,  start=(/1,1,1,it/)))     
        end if	
 
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

        ! niveau verticaux	
	if (it.eq.1) then
        
	do iz= 1,nlev
	  if (station_Z(i).gt.hlay(ix1(i),iy1(i),iz) ) iz1(i) = iz + 1
        end do

        if ( iz1(i) .eq. 0 ) then 
          iz1(i) = 2
	  wz1(i) = 1.0
	else
	  wz1(i) = (hlay(ix1(i),iy1(i),iz1(i)) - station_Z(i)) / (hlay(ix1(i),iy1(i),iz1(i)) - hlay(ix1(i),iy1(i),iz1(i)-1))
          write(*,'(I3,F6.0,2(F10.3,A1,F5.3))')  i, station_Z(i) , hlay(ix1(i),iy1(i),iz1(i)-1),  '*', wz1(i),  hlay(ix1(i),iy1(i),iz1(i)), '*', 1-wz1(i)        
	end if
	
        end if
	
	if (nlev.eq.1) iz1(i)=1

        pluie0 = (vars(i,indice_rainc)+vars(i,indice_rainnc))
       
       
         if (nlev.gt.1) then
       
           do n = 1, nvar              
         vars(i,n)=   & 
        (  (1.-wx1(i))*(1.-wy1(i))*(  wz1(i) * var(ix1(i)  ,iy1(i)  ,iz1(i)-1,n) + (1- wz1(i)) *  var(ix1(i)    ,iy1(i)    ,iz1(i),n) )     &
         +     wx1(i) *(1.-wy1(i))*(  wz1(i) * var(ix1(i)+1,iy1(i)  ,iz1(i)-1,n) + (1- wz1(i)) *  var(ix1(i)+1  ,iy1(i)    ,iz1(i),n) )     &
         + (1.-wx1(i))*	   wy1(i) *(  wz1(i) * var(ix1(i)  ,iy1(i)+1,iz1(i)-1,n) + (1- wz1(i)) *  var(ix1(i)    ,iy1(i)+1  ,iz1(i),n) )     &
         +     wx1(i) *	   wy1(i) *(  wz1(i) * var(ix1(i)+1,iy1(i)+1,iz1(i)-1,n) + (1- wz1(i)) *  var(ix1(i)+1  ,iy1(i)+1  ,iz1(i),n) )     ) 
	 
           end do
	 
	 else

           do n = 1, nvar              
         vars(i,n)=   & 
        (  (1.-wx1(i))*(1.-wy1(i))*  var(ix1(i)    ,iy1(i)    ,iz1(i),n) 	 &
         +     wx1(i) *(1.-wy1(i))*  var(ix1(i)+1  ,iy1(i)    ,iz1(i),n) 	 &
         + (1.-wx1(i))*	   wy1(i) *  var(ix1(i)    ,iy1(i)+1  ,iz1(i),n) 	 &
         +     wx1(i) *	   wy1(i) *  var(ix1(i)+1  ,iy1(i)+1  ,iz1(i),n) 	 ) 
	 
           end do	 
	 
	 end if !nlev>1
     
         ! Calcule la direction et force du vent avec U10 et V10 interpolés
         call calcvent_station(vars(i,indice_u),vars(i,indice_v),vars(i,indice_dd),vars(i,indice_vv))      
         
         if ( vars(i,indice_rainc) + vars(i,indice_rainnc) - pluie0 .lt.0. ) pluie0 = vars(i,indice_rainc) + vars(i,indice_rainnc)
     
        ! variables calculées
        !varname(15)='uhto' ! vitesse hauteur des toits	
        !varname(16)='sthe' ! sigma theta
        !varname(17)='cldo' ! nebulosité en octas    
        !varname(18)='k1'   ! constante reaction k1
        !varname(19)='k3'   ! constante reaction k3
        
	! attenuation photolyse (attenuation est egale a 1 en ciel clair)
	atte = vars(i,14)
	
	! uhto : fourni une valeur mais est recalcule par SIRANE
	vars(i,15) = vars(i,3)
	
	! sigma theta : fourni une valeur mais est recalcule par SIRANE
        vars(i,16) = 0.065*sqrt(7/vars(i,3))*180/pi
        
	! calcul de la couverture nuageuse (ciel clair: atte = 1)
	!vars(i,17)=8*(atte/0.75)**(1/3.4) BUG : attenuation est egal a 1 en ciel clair	
	vars(i,17) = 8*( ( 1 - atte ) /0.75)**(1/3.4)  ! formule correcte

	decl_sol = 23.45*sin((jday+284.0)/365.0*2.0*pi)*pi/180.0
	lat_sol  = lat(ix1(i),iy1(i))*pi/180.0
        elev_sol = sin(lat_sol)*sin(decl_sol)+ &
                   cos(lat_sol)*cos(decl_sol)* &
                   cos((Hour-12.0)/24.0*2.0*pi)
        k1_clear=(0.5699-((9.056E-3*(90.0-180.0*asin(elev_sol)/pi))**2.546))/60.0
			
	vars(i,18)=max(0.0, k1_clear *   atte  ) ! k1 * atte	
	 
	vars(i,19)=2*1.e-12*exp(-1430/vars(i, 1))*na/(v0*1000*1.e9) ! k3

        write(11,'(I4,";",A19,12(";",F10.2),2(";",E10.2))') it,datestr,&
        vars(i, 1)-273.15,vars(i, 2)*100.,&                              ! tem2,sreh
        vars(i,indice_dd),vars(i,3),  &                                  ! DD10,FF10
        vars(i, 10),vars(i,11),vars(i,12),vars(i,13),vars(i,14), & ! obuk,hght,usta,topc,atte
        vars(i, 15),vars(i,16),real(int(vars(i,17))),vars(i,18),vars(i,19)   ! uhto,sthe,cldo,k1,k3      
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
      write(*,*) '* fichier OUT.STATION.*.csv'
      end
