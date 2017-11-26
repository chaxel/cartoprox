      program cartes_to_nc

      use netcdf
      use typesizes       
      
      implicit none
      
      character(len=256) :: fni,fno,var_str     
      integer :: nx, ny      
      real    :: xmin, ymin, dx, dy
      integer :: i, j, ix, iy
      real    :: tmp_x, tmp_y, tmp_z, tmp_lon, tmp_lat      
      real, allocatable :: easting(:,:),northing(:,:),var1(:,:),lon(:,:),lat(:,:)
      integer :: ncFileID
      integer :: lonVarID,latVarID,xVarID,lonDimId,latDimId
      
      !projection
      logical                      :: igeo
      character(len=20)            :: option
      double precision             :: easting2, northing2, lon2, lat2, units, utmzone  
      integer                      :: geoid, projection
      double precision, parameter  :: pi = 3.141592653589793238
      
      logical :: icreate
      
      character(len=18) :: periodestr
            
!------------------------------      
    !initialisations
    icreate = .true. !creer le NetCDF ?
    igeo=.false.     !ecrit les lon/lat ?

    ! arguments : lit le nom du fichier
    fno='carte.nc'
    
    call getarg(1,fni)
    call getarg(2,fno)    
    call getarg(3,option)
    
    !Ecrit les données lat/long

    if (index(option,'geo').ne.0)igeo=.true.
    
    open(unit=10,file=fni,status='old')
    
    ! Lit le HEADER
    read(10,*) periodestr, nx, ny, var_str, xmin, ymin , dx, dy
                  	        
    allocate(easting(nx,ny))
    allocate(northing(nx,ny))
    allocate(var1(nx,ny))
    
    if (igeo) then    
      allocate(lon(nx,ny))
      allocate(lat(nx,ny))       
    end if
    
    if ( nf90_open(fno, nf90_write, ncFileID) .eq. nf90_noerr ) then ! Fichier existe
      icreate=.false.
      if ( nf90_inq_varid(ncFileID, 'lon', xVarID).eq. nf90_noerr )igeo=.true.
      call check(nf90_close(ncFileID))
    end if
    
    ! Creer le NetCDF s'il n'existe pas...  
    if (icreate) then
    
    call check(nf90_create(path = fno, cmode = nf90_clobber, ncid = ncFileID))
    
    write(*,*) 'Dimensions'
    call check(nf90_def_dim(ncid = ncFileID, name = 'west_east'  ,len = nx, dimid = lonDimID))        
    call check(nf90_def_dim(ncid = ncFileID, name = 'south_north',len = ny, dimid = latDimId  ))  

    write(*,*) 'Data'
    
    call check(nf90_def_var(ncFileID,'easting'    ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(ncFileID, XVarID, 'long_name','Coord centre mailles'))
    call check(nf90_put_att(ncFileID, XVarID, 'units','m'))
    call check(nf90_put_att(ncFileID, XVarID,  "_FillValue", -9999.0 ) )    
       
    call check(nf90_def_var(ncFileID,'northing'    ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(ncFileID, XVarID, 'long_name','Coord centre mailles'))
    call check(nf90_put_att(ncFileID, XVarID, 'units','m'))
    call check(nf90_put_att(ncFileID, XVarID,  "_FillValue", -9999.0 ) ) 
    
    if (igeo) then

    call check(nf90_def_var(ncFileID,'lon'    ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(ncFileID, XVarID, 'long_name','Longitude WGS84 centre mailles'))
    call check(nf90_put_att(ncFileID, XVarID, 'units','m'))
    call check(nf90_put_att(ncFileID, XVarID,  "_FillValue", -9999.0 ) )    
       
    call check(nf90_def_var(ncFileID,'lat'    ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(ncFileID, XVarID, 'long_name','Latitude WGS84 centre mailles'))
    call check(nf90_put_att(ncFileID, XVarID, 'units','m'))
    call check(nf90_put_att(ncFileID, XVarID,  "_FillValue", -9999.0 ) ) 
    
    end if         
    
    call check(nf90_put_att(ncFileID, nf90_global, 'title'  , 'Cartoprox '//trim(periodestr)//' - (c) AtmoRA 2011' ))
                           
    call check(nf90_close(ncFileID))  
    
    end if
    
    !CREER LA VARIABLE SI BESOIN    
    call check(nf90_open(fno, nf90_nowrite, ncFileID))
    if ( nf90_inq_varid(ncFileID, trim(var_str), xVarID) .ne. nf90_noerr ) then
      call check(nf90_close(ncFileID))
      call check(nf90_open(fno, nf90_write, ncFileID))
      call check(nf90_inq_dimid(ncFileID, 'west_east'	 , lonDimID))	  
      call check(nf90_inq_dimid(ncFileID, 'south_north'  , latDimID))	 
      write(*,*) 'Dimensions OK'
      write(*,*) 'Redefinition en cours pour creer '//trim(var_str)//'...'   
      call check(nf90_redef(ncFileID))
      call check(nf90_def_var(ncFileID, trim(var_str),nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
      call check(nf90_put_att(ncFileID, XVarID, 'long_name','Estimation variable'))
      call check(nf90_put_att(ncFileID, XVarID, 'units','none'))
      call check(nf90_put_att(ncFileID, XVarID,  "_FillValue", -9999.0 ) )
      call check(nf90_enddef(ncFileID))
      call check(nf90_close(ncFileID)) 
    end if
    
    
    open(unit=11,file='cartes_to_nc.dat',status='unknown')    
    
    var1(:,:) = -9999.

    !Calcule les coordonnées
    do j=1,ny 
      do i=1,nx       	 
	 easting(i,j)  =  xmin + ( i - .5) * dx
	 northing(i,j) =  ymin + ( j - .5) * dy	 
	 if (igeo) then	 
	 !Transformation de coordonnées UTM31 -> geo
	 geoid      =  1
	 units      =  1.
	 utmzone    = 31
	 projection = 2
	 easting2  = easting(i,j)
	 northing2 = northing(i,j)
	 call projectinv2(geoid,easting2,northing2,utmzone,lon2,lat2,projection,units)
	 lon(i,j) = lon2 * 180. / pi
	 lat(i,j) = lat2 * 180. / pi	 	 
	 end if	     
      end do
    end do        

    !Lit le fichier de données
    do j=1,ny 
      do i=1,nx
	 read(10,*,err=88,end=89) tmp_x, tmp_y, tmp_z 	   	 
	 ix = int( (tmp_x - xmin) / dx ) + 1
	 iy = int( (tmp_y - ymin) / dy ) + 1	 	 
	 if ( (ix.ge.1).and.(iy.ge.1).and.(ix.le.nx).and.(iy.le.ny) ) then
	    !easting(ix,iy) = tmp_x
	    !northing(ix,iy) = tmp_y
	    var1(ix,iy) = tmp_z	     		
	    write(11,'(2F12.3,2F10.3)') easting(ix,iy),northing(ix,iy),var1(ix,iy)		 	 
	 end if
	 		 	     
      end do
    end do
    
    go to 99 ! Pas de probleme
    
89  continue   
    
    write(*,*) 'WARNING: dans grille2nc.f90, grille incomplete' 	

99  continue
              
    close(10)
    close(11)    

    write(*,*) i,j,(j-1)*nx +i
    write(*,*) nx,ny
 
    call check(nf90_open(fno, nf90_write, ncFileID))
    
    call check(nf90_inq_varid(ncFileID, 'easting', xVarID)) 	     
    call check(nf90_put_var(ncFileID, xVarID, easting, start = (/ 1, 1/) ))      

    call check(nf90_inq_varid(ncFileID, 'northing', xVarID)) 	 
    call check(nf90_put_var(ncFileID, xVarID, northing, start = (/ 1, 1/) )) 
    
    if (igeo) then
        
    call check(nf90_inq_varid(ncFileID, 'lon', xVarID)) 	     
    call check(nf90_put_var(ncFileID, xVarID, lon, start = (/ 1, 1/) ))      

    call check(nf90_inq_varid(ncFileID, 'lat', xVarID)) 	 
    call check(nf90_put_var(ncFileID, xVarID, lat, start = (/ 1, 1/) ))        
    
    write(*,*) 'Creation de la grille LAT/LONG OK' 
    
    end if
      
    call check(nf90_inq_varid(ncFileID, trim(var_str), xVarID))    
    call check(nf90_put_var(ncFileID, xVarID, var1, start = (/ 1, 1/) ))	   	   
        
    call check(nf90_close(ncFileID))
    
    stop
    
88  continue   
    
    write(*,*) 'Erreur dans lecture des données a la ligne:',i+(j-1)*nx

    end
!-----------------------------------------------------------------------
      subroutine check(status)
  ! Internal subroutine - checks error status after each netcdf, prints out text message each time
  !   an error code is returned. 
  
      use netcdf
      use typesizes  
  
      integer, intent ( in) :: status
      if(status /= nf90_noerr) then 
      print*, trim(nf90_strerror(status))
      print *, 'status=',status
      stop    
      end if
      end subroutine check      
