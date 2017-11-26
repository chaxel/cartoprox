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
      integer :: it, itstart, data_step
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

! -------- SIRANE
      real , parameter :: rho=1.0    ! kg m-3
      real , parameter :: Cp=1004.67 !J kg-1 K-1
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

      open(unit=10,file='OUT_MTO.txt',status='old') 
      read(10,*) deb_str, fin_str
      read(10,*) fni
      close(10)

      write(*,*) 'WRF : Extraction des dates '//deb_str//' --> '//fin_str

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
      write(*,*) nxx,nyy,nlev,distgrid

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
      nvar = 18
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
      varname( 1)='T2'
      varname( 2)='Q2'
      varname( 3)='PSFC'
      varname( 4)='U10'
      varname( 5)='V10'
      varname( 6)='DD10'
      varname( 7)='FF10'      
      varname( 8)='RAINNC'
      varname( 9)='RAINC'
      varname(10)='SNOW'
      varname(11)='SWDOWN'
      varname(12)='GLW'	
      varname(13)='UST'
      varname(14)='PBLH'
      varname(15)='HFX'      
      varname(16)='QFX'
      varname(17)='LH' 
      
      ! calculé
      varname(18)='thetas'            
      
      long_name='XLONG'
      lat_name= 'XLAT'
                        
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
	write(11,'(A200)') 'i;date_heure;t(deg);q(g/kg);p(hPa);pluie(mm);dd;ff;snow(kg/m2);sw(W/m2);lw(W/m2);u*(m/s);Hcl(m);hfx(W/m2);qfx(kg/m2/s);lh(W/m2);thetas'	      
        !write(11,'(A19,1X,10A10)') 'date',varname(:)
        close(11)
      
      end do

 
      pluie0 = 0.
      iextract = .false.      
      call check(nf90_open(fni, nf90_nowrite, ncFileID))  
! >>>>>>>>>>>>>>>>>> ITERATIONS >>>>>>>>>>>>>>>>>>>>           
      do it=1,numSteps
! >>>>>>>>>>>>>>>>>> ITERATIONS >>>>>>>>>>>>>>>>>>>>      
            
      data_step=it ! it pour les données
      if (data_step.eq.1) data_step = 2 ! passe le premier pas de temps ou les valeurs DIAG sont indisponibles
      
      ! date             	      
      call check(nf90_inq_varid(ncFileID, 'Times', xVarID))    
      call check(nf90_get_var(ncFileID, xVarID, datestr, start = (/1,it/) ))
      !write(*,*) 'Trouve date '//datestr     	      
      
      read(datestr,'(I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,I2)')&
     	     Year,Mnth,Day,Hour,Minu,Sec     
      
      !Extraction d'une periode (l ordre des lignes est IMPORTANT)
      if (datestr.eq.deb_str)iextract = .true. !debut de l'extraction      
      if (.not.iextract)goto 999
      if (datestr.eq.fin_str)iextract = .false.      
      if (mod(it-1,24).eq.0)write(*,*) 'Extraction de la date '//datestr//' en cours'
  
      ! 3D
      if ( nlev.gt.1) then
       call check(nf90_inq_varid(ncFileID, 'hlay', xVarID))         
       call check(nf90_get_var(ncFileID, xVarID, hlay(:,:,:) ,  start=(/1,1,1,data_step/)))           
       hlay(:,:,:) = hlay(:,:,:) - 8.
      else
       hlay(:,:,:) = 0.
      end if
     
      ! SURFACE  
      do n = 1, nvar  
        if (nf90_inq_varid(ncFileID, trim(varname(n)), xVarID).eq.0) then
	if (it.eq.1) write(*,*) 'Lit '//trim(varname(n)) 
	if ( nlev.eq.1) then
	  call check(nf90_get_var(ncFileID, xVarID, var(:,:,1,n) ,  start=(/1,1,data_step/)))    
	else
          call check(nf90_get_var(ncFileID, xVarID, var(:,:,:,n) ,  start=(/1,1,1,data_step/)))   
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
     
         ! calcul : thetas : - hfx*/(rho*cp*u*)
	 vars(i,18) = -1. * vars(i,15)/(rho*Cp*vars(i,13))		 
    
       !write(11,'(A19,1X,10F10.6)') datestr,vars(i, :)

        write(11,'(I4,";",A19,4(";",F10.2),";",F10.0,";",F10.2,9(";",F10.2))') it,datestr,&
        vars(i, 1)-273.15,vars(i, 2)*1000.,vars(i,3)/100.,( vars(i,indice_rainc) + vars(i,indice_rainnc) - pluie0 )*10.,& ! T2,Q2,PSFC,RAIN
        vars(i,indice_dd),vars(i,indice_vv),  &                   ! DD10,FF10
        vars(i, 10),vars(i,11),vars(i,12),vars(i,13),vars(i,14),vars(i,15),vars(i,16),vars(i,17),           &  ! SWDOWN,UST,PBLH
	vars(i, 18)
!        if (i*it.eq.1)write(*,'(A19,1X,4A10,A10,A10,3A10)') 'date','t(deg)','q(g/kg)','p(hPa)','pluie(mm)','dd','ff','sw(W/m2)','u*(m/s)','Hcl(m)'
!        if (i.eq.1)write( *,'(A19,1X,4F10.2,F10.0,F10.2,F10.0,F10.2,F10.0)') datestr,&
!                    vars(i, 1)-273.15,vars(i, 2)*1000.,vars(i,3)/100.,( vars(i,6)+vars(i,7)- pluie0 )*10.,& ! T2,Q2,PSFC,RAIN
!                    vars(i,12),vars(i,13),  &                   ! DD10,FF10
!                    vars(i, 9),vars(i,10),vars(i,11)           ! SWDOWN,UST,PBLH
!      
        close(11)
      
      end do

999   continue !SKIP extraction               
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
