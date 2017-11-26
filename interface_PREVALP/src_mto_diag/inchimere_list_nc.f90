      program exchimere_list_nc
! Ecrit les donnees a des points fournits dans un fichier X,Y dans le NetCDF CHIMERE 

      use netcdf
      use typesizes       
      

      implicit none
      
      real,allocatable :: dmax(:,:),davg(:,:),lon(:,:),lat(:,:)
      integer :: date, idate
      integer :: ihour      
      real :: jdate
      character(len=256) :: fni,fns
      real :: ppb2ug
      integer :: ix,iy,ns,nstat
      integer :: i,j,i2,j2
      integer :: it, itstart
      integer, parameter :: nstatmax=300*300
      ! donnees aux stations ou sur la grille de krigeage
      ! donnees a interpoler vers la grille de CHIMERE
      !real :: lons(nstatmax),lats(nstatmax),concs(nstatmax)  
      !real :: xs(nstatmax)  ,ys(nstatmax)
      integer :: nx0,ny0
      real, allocatable :: lons(:,:),lats(:,:),concs(:,:)
      integer,allocatable :: ix1(:,:),iy1(:,:)
      real,allocatable    :: wx1(:,:),wy1(:,:)      
      
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

      integer :: ncFileID
      integer :: lonVarID,latVarID,xVarID,lonDimId,latDimId,frTimeDimId
      
      character(len=10) :: pol
      
      integer :: julian
      
      integer :: ios
      
!--------------------------------- especes gazeuses  
      real,allocatable :: conc(:,:,:)         
!------------------------------      
      read *,fni
      read *,deb
      read *,ndays
      read *,idate
      read *,ihour           
      read *,nx0    ! nx de gkrig
      read *,ny0    ! ny de gkrig
      read *,fns
      read *,pol
      
      level=1

!--------------------------------------------------------------------
      call getdimensions(fni,nxx,nyy,nlev,&
                      numSteps,timeStringLen,&
                      distGrid,startYear,startMnth,startDay,&
		      startHour,startMin,startSec)
		      
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

      allocate(lon(nxx,nyy))
      allocate(lat(nxx,nyy))

      do iy=1,nyy
        do ix=1,nxx
	  davg(ix,iy) = 0.
          dmax(ix,iy) = 0.  	  
        end do
      end do	

!--------------------------------------------------
      write(*,*) 'Get coordinates...'
      call check(nf90_open(fni, nf90_nowrite, ncFileID))
      call check(nf90_inq_varid(ncFileID, 'lon' , lonVarID)) 
      call check(nf90_inq_varid(ncFileID, 'lat' , latVarID))     
    
      call check(nf90_get_var(ncFileID, lonVarID, lon,  start=(/1,1/)))
      call check(nf90_get_var(ncFileID, latVarID, lat,  start=(/1,1/)))
      call check(nf90_close(ncFileID))
!--------------------------------------------------------------------        
! Read Model outputs for temperature at 2 m, q, u and v and 
    
!-------------------------------------------------------
      nstat= nstatmax
      
      allocate (lons(nx0,ny0))
      allocate (lats(nx0,ny0)) 
      allocate (concs(nx0,ny0))
      
      open(unit=10,file=fns,status='old')
      do j=1,ny0  
        do i=1,nx0
           read(10,*,end=10) lons(i,j),lats(i,j),concs(i,j)      
        end do
      end do
      
      go to 20
      
10    continue

      write(*,*) 'ERROR: increase reading input'
      stop

20    continue
      
      close(10)
                 
! Conversion ppcb -> ug/m3
      ppb2ug = 1.     
      if ( trim(pol).eq.'O3' ) then          
      ppb2ug = 2. !O3
      else if ( trim(pol).eq.'O3_KRIG' ) then          
      ppb2ug = 2. !O3       
      else if ( trim(pol).eq.'O3_INNO' ) then          
      ppb2ug = 2. !O3      
      else if ( trim(pol).eq.'NO2' ) then        
      ppb2ug = 1.91 !NO2 
      else if ( trim(pol).eq.'NO' ) then         
      ppb2ug = 1.25 !NO
      else if ( trim(pol).eq.'SO2' ) then         
      ppb2ug = 2.66 !SO2
      else if ( trim(pol).eq.'CO' ) then         	
      ppb2ug = 1.2  !CO   
      else
      ppb2ug = 1.     
      end if
           
!  Initialisations

      jdate = julian (startYear,startMnth,startDay) + startHour/24  

! ------------------------------------
        !-------------------------------------------------------------------- 
! Calcul la correspondance des grilles
!-------------------------------------------------------------------- 
       

      allocate (ix1(nxx,nyy))
      allocate (iy1(nxx,nyy)) 
      allocate (wx1(nxx,nyy))
      allocate (wy1(nxx,nyy))            
      
      open(unit=10,file='WDOT',status='old',iostat=ios)
      
      if ( ios .eq. 0 ) then
        write(*,*) 'Lit les poids dans fichier'      
        do i2=1,nxx
          do j2=1,nyy    
	    read(10,*) ix1(i2,j2),iy1(i2,j2),wx1(i2,j2),wy1(i2,j2)
	  end do
         end do 
         close(10)	      
      else  
        write(*,*) 'Debut du calcul des poids'        
        call wcal1(nx0,ny0,ix1,iy1,wx1,wy1,lons,lats,nxx,nyy,lon,lat)
        open(unit=10,file='WDOT',status='unknown')
        do i2=1,nxx
          do j2=1,nyy    
	    write(10,*) ix1(i2,j2),iy1(i2,j2),wx1(i2,j2),wy1(i2,j2)
	  end do
        end do		
        close(10)
      end if
 
      write(*,*) 'Fin du calcul des poids'

!-------------------------------------------------------------------- 
! Interpolation des données de grille
!--------------------------------------------------------------------
      allocate(conc(nxx,nyy,1))

      do i2=1,nxx
        do j2=1,nyy
	  if (     ( ix1(i2,j2) * iy1(i2,j2) .ne. 0) &
	       .or.( wx1(i2,j2) .lt. 0.).or.( wx1(i2,j2) .gt. 1.) &
	       .or.( wy1(i2,j2) .lt. 0.).or.( wy1(i2,j2) .gt. 1.) &
	    ) then
	    
	    if ( max( -1*concs(ix1(i2,j2),iy1(i2,j2))    ,-1*concs(ix1(i2,j2)+1,iy1(i2,j2)  ), &
	              -1*concs(ix1(i2,j2)  ,iy1(i2,j2)+1),-1*concs(ix1(i2,j2)+1,iy1(i2,j2)+1) ) .gt. 1000. ) then
		     conc(i2,j2,1) = -9999. 
	    else      
	    
	  conc(i2,j2,1) = (  (1.-wx1(i2,j2))*(1.-wy1(i2,j2))*concs(ix1(i2,j2)  ,iy1(i2,j2)  ) &
                           +     wx1(i2,j2) *(1.-wy1(i2,j2))*concs(ix1(i2,j2)+1,iy1(i2,j2)  ) &
                           + (1.-wx1(i2,j2))*    wy1(i2,j2) *concs(ix1(i2,j2)  ,iy1(i2,j2)+1) &
                           +     wx1(i2,j2) *    wy1(i2,j2) *concs(ix1(i2,j2)+1,iy1(i2,j2)+1) ) &
			   / ppb2ug	
	 
	    end if
	 
	  else
	    conc(i2,j2,1) = -9999.
	  end if
	  	  	  
	end do 
      end do 

! --------------Find iteration
      call check(nf90_open(fni, nf90_write, ncFileID))      

      if ( nf90_inq_varid(ncFileID, pol, xVarID) .ne. 0 ) then
! Defini une nouvelle variable: pol 

        call check(nf90_inq_dimid(ncFileID, 'west_east'  , lonDimID))     
        call check(nf90_inq_dimid(ncFileID, 'south_north'  , latDimID))
        !call check(nf90_inq_dimid(ncFileID, 'bottom_top'  , zDimID))  
        call check(nf90_inq_dimid(ncFileID, 'Time', frTimeDimID))          
        write(*,*) 'Dimensions OK'
        write(*,*) 'Redefinition en cours...'	
 	call check(nf90_redef(ncFileID))
        call check(nf90_def_var(ncFileID,pol,nf90_float,dimids=(/lonDimID,latDimId,frTimeDimID/),varID=XVarID))    
        call check(nf90_put_att(ncFileID, XVarID, 'long_name',pol))
        call check(nf90_put_att(ncFileID, XVarID, 'units','ppb'))
        call check(nf90_put_att(ncFileID, XVarID,  "_FillValue", -9999.0 ) )        
	call check(nf90_enddef(ncFileID))
        write(*,*) 'Redef OK'	
		
      end if


      do it=1,ndays*24
      
      ! date             	      
      call check(nf90_inq_varid(ncFileID, 'Times', xVarID))    
      call check(nf90_get_var(ncFileID, xVarID, datestr, start = (/1,it/) ))
     	      
      read(datestr,'(I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,I2)')&
     	     Year,Mnth,Day,Hour,Minu,Sec
     
      date = Year*10000 +  Mnth*100 + Day 
      
      if (( date .eq.idate ).and. (hour .eq.ihour)) then
         write(*,*) 'Trouve date : '//datestr
         itstart=it
      end if
      
      end do
      
      it=itstart
      
      if ( itstart.eq.0 ) then
        write(*,*) 'ERREUR : pas de temps non trouve',startYear,startMnth,startDay
	stop
      end if
            
      write(*,*) 'IT:',it 
      
!---------------------------------------------------------------------------                  
      ! SURFACE  
            
      call check(nf90_inq_varid(ncFileID, pol, xVarID))         
      call check(nf90_put_var(ncFileID, xVarID, conc,  start=(/1,1,it/))) 	                  

!------------- end LOOP over the hours
      call check(nf90_close(ncFileID))
    
      open(unit=10,file='grid.out.inexchimere.txt',status='unknown')
    
      do i2=1,nxx
        do j2=1,nyy
	  write(10,*) lon(i2,j2),lat(i2,j2),conc(i2,j2,1)
	  	  	  
	end do 
      end do     
      
      close(10)
    
      write(*,*) 'Normal terminason of inchimere_list_nc'   

      end
