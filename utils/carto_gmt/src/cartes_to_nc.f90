      program cartes_to_nc

      use netcdf
      use typesizes       
      
      implicit none
      
      character(len=256) :: fni,fno,nx_str,ny_str      
      integer :: nx, ny
      integer :: i, j
      real, allocatable :: lon(:,:),lat(:,:),var1(:,:),var2(:,:)
      integer :: ncFileID
      integer :: lonVarID,latVarID,xVarID,lonDimId,latDimId       
!------------------------------      
    ! arguments : lit le nom du fichier
    fno='carte.nc'
    
    call getarg(1,fni)
    call getarg(2,nx_str)
    call getarg(3,ny_str)
    
    read(nx_str,'(I4)') nx
    read(ny_str,'(I4)') ny   
         	        
    allocate(lon(nx,ny))
    allocate(lat(nx,ny))
    allocate(var1(nx,ny))
    allocate(var2(nx,ny))      
      
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
    
    call check(nf90_def_var(ncFileID,'var1'    ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(ncFileID, XVarID, 'long_name','Estimation variable'))
    call check(nf90_put_att(ncFileID, XVarID, 'units','none'))
    call check(nf90_put_att(ncFileID, XVarID,  "_FillValue", -9999.0 ) )    
             
    call check(nf90_def_var(ncFileID,'var2'    ,nf90_float,dimids=(/lonDimID,latDimId/),varID=XVarID))    
    call check(nf90_put_att(ncFileID, XVarID, 'long_name','Variance krigeage'))
    call check(nf90_put_att(ncFileID, XVarID, 'units','none'))
    call check(nf90_put_att(ncFileID, XVarID,  "_FillValue", -9999.0 ) )    
   
    call check(nf90_close(ncFileID))  
    
    open(unit=10,file=fni,status='old')
    open(unit=11,file='cartes_to_nc.dat',status='unknown')    
    do j=1,ny 
      do i=1,nx
    	 read(10,'(2F11.3,2F9.3)',err=88,end=88) lon(i,j),lat(i,j),var1(i,j),var2(i,j)  
    	 write(11,'(2F12.3,2F10.3)') lon(i,j),lat(i,j),var1(i,j),var2(i,j)  	    
      end do
    end do
    close(10)
    close(11)    

99  continue

    write(*,*) i,j,(j-1)*nx +i
    write(*,*) nx,ny
 
    call check(nf90_open(fno, nf90_write, ncFileID))
    
    call check(nf90_inq_varid(ncFileID, 'easting', xVarID)) 	     
    call check(nf90_put_var(ncFileID, xVarID, lon, start = (/ 1, 1/) ))      

    call check(nf90_inq_varid(ncFileID, 'northing', xVarID)) 	 
    call check(nf90_put_var(ncFileID, xVarID, lat, start = (/ 1, 1/) ))    
      
    call check(nf90_inq_varid(ncFileID, 'var1', xVarID))    
    call check(nf90_put_var(ncFileID, xVarID, var1, start = (/ 1, 1/) ))	   

    call check(nf90_inq_varid(ncFileID, 'var2', xVarID))    
    call check(nf90_put_var(ncFileID, xVarID, var2, start = (/ 1, 1/) ))	   
    
    
    call check(nf90_close(ncFileID))
    
    stop
    
88  continue   
    
    write(*,*) 'Erreur dans lecture des données' 	  

    end
