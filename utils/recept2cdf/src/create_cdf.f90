      program create_cdf
! ********************************************************************
! * CREER UN FICHIER NETCDF DESTINE A STOCKE LES RESULTATS           *
! * AU RECEPTEURS + ECRIT LES COORDONNEES X et Y                     *
! * AUTHOR:	  E. CHAXEL					     *
! *		  COPARLY	                                     *
! * LAST MODIFICATION: FEB. 2010				     *
! ********************************************************************

      use netcdf
      use typesizes

      implicit none
      
!-----Input 
      character(len=256) :: fn_pts   ! fichier nom,X,Y
      character(len=256) :: fn_vars  ! fichier var   
      character(len=256) :: fn_cdf   ! fichier NetCDF sorties
      character(len=256) :: fondstr  !   
      
!-----dimensions                 
      integer :: npts,nvars 

!-----tableaux dynamiques
      character(len=256), allocatable :: pts_name(:)      
      real, allocatable               :: pts_x(:)
      real, allocatable               :: pts_y(:)  
      real, allocatable               :: pts_easting(:)
      real, allocatable               :: pts_northing(:)            
      character(len=256), allocatable :: var_name(:)     

!-----local
      integer :: n
      character(len=1)  :: str_tmp
      integer,parameter  :: nmax = 10000000 

                
!-----netcdf related
      integer :: ncFileID, frTimeDimID, frTimeVarID, ptsDimID, varsDimID, StrLenDimID, frTimeStrDimID
      integer :: xVarID         
      character(len=39) :: tunits 
      
      logical :: ifond      
      logical :: igrille  ! pour utilisation CARTOPROX -> ecrit la valeur de la grille PREVALP
      logical :: iprox

!-----CORPS DU PROGRAMME---------------------------------      
      ! lit l'argument
      call getarg(1,fn_pts)
      call getarg(2,fn_vars)  
      call getarg(3,fn_cdf)
      call getarg(4,fondstr)

      if ( trim(fn_pts) == '' .or. trim(fn_vars) == '' ) then 
        write(*,*) 'Syntaxe : fn_pts fn_vars [fn_cdf]'
	stop
      end if
      
      if ( index(fn_cdf,'.nc') .eq. 0 ) then 
        fn_cdf='defaut.nc'
      end if
      
      ifond=.true.   ! par defaut creer la variable *_fond
      igrille=.false. ! par defaut creer la variable *_grille
      iprox=.false.   ! par defaut creer la variable *_prox
      
      !Cas ou on traite uniquement le fond
      if ( index(fondstr,'_fond').ne.0) then
        ifond=.true.
	igrille=.false.
	iprox=.false.
      end if

! lit fichier de pts 
      write(*,*) 'Open '//trim(fn_pts)
      open(unit=10,file=fn_pts,status='old')      
      do n=1, nmax
        read(unit=10,fmt=*,end=90)str_tmp
      end do       
90    continue
      npts=n-1  
      allocate(pts_name(npts))
      allocate(pts_x(npts))
      allocate(pts_y(npts))
      allocate(pts_easting(npts))
      allocate(pts_northing(npts))      
      rewind(10)      
      do n=1, npts
        read(unit=10,fmt=*)pts_name(n),pts_x(n),pts_y(n),pts_easting(n),pts_northing(n)
      end do       
      close(10)
      
! lit fichier de variables
      write(*,*) 'Open '//trim(fn_vars)
      open(unit=10,file=fn_vars,status='old')      
      do n=1, nmax
        read(unit=10,fmt=*,end=91)str_tmp
      end do       
91    continue
      nvars=n-1  
      allocate(var_name(nvars))
      rewind(10)      
      do n=1, nvars
        read(unit=10,fmt=*)var_name(n)
      end do       
      close(10)                

! afficage 
      write(*,*) 'pts=',npts
      write(*,*) 'vars=',nvars
                 
      write(*,*) 'Create '//trim(fn_cdf)  
      call check(nf90_create(trim(fn_cdf), nf90_clobber, ncFileID))
      call check(nf90_def_dim(ncFileID, 'Point' ,npts, ptsDimID )) 
      call check(nf90_def_dim(ncFileID, 'Variable',nvars,varsDimID))       
      call check(nf90_def_dim(ncFileID, 'Time',nf90_unlimited, frTimeDimID))
      call check(nf90_def_dim(ncFileID, 'StrLen',256, StrLenDimID)) 
      call check(nf90_def_dim(ncFileID, 'DateStrLen',len = 19, dimid = frTimeStrDimID))             
            
      write(*,*) 'Dimensions OK'      
!      call check(nf90_def_var(ncFileID, 'Time', nf90_float, jourDimID, XVarID) )
!      call check(nf90_put_att(ncFileID, XVarID, 'long_name','date TU'))
!      call check(nf90_put_att(ncFileID, XVarID, 'units',tunits))     

      call check(nf90_def_var(ncFileID,'x_pts'    ,nf90_float,(/ptsDimID/),XVarID))    
      call check(nf90_put_att(ncFileID, XVarID, 'long_name','Coordonnees X des points'))
      call check(nf90_put_att(ncFileID, XVarID, 'units','m'))
      call check(nf90_put_att(ncFileID, XVarID,  '_FillValue', -9999.0 ) )
      call check(nf90_put_att(ncFileID, XVarID,  'missing_value', -9999.0 ) )      
    
      call check(nf90_def_var(ncFileID,'y_pts'    ,nf90_float,(/ptsDimID/),XVarID))    
      call check(nf90_put_att(ncFileID, XVarID, 'long_name','Coordonnees Y des points'))
      call check(nf90_put_att(ncFileID, XVarID, 'units','m'))
      call check(nf90_put_att(ncFileID, XVarID,  '_FillValue', -9999.0 ) )
      call check(nf90_put_att(ncFileID, XVarID,  'missing_value', -9999.0 ) )      

      write(*,*) 'Variables x_pts et y_pts OK'
      
      call check(nf90_def_var(ncFileID,'easting_pts'    ,nf90_float,(/ptsDimID/),XVarID))    
      call check(nf90_put_att(ncFileID, XVarID, 'long_name','Coordonnees UTM easting des points'))
      call check(nf90_put_att(ncFileID, XVarID, 'units','m'))
      call check(nf90_put_att(ncFileID, XVarID,  '_FillValue', -9999.0 ) )
      call check(nf90_put_att(ncFileID, XVarID,  'missing_value', -9999.0 ) )      
    
      call check(nf90_def_var(ncFileID,'northing_pts'    ,nf90_float,(/ptsDimID/),XVarID))    
      call check(nf90_put_att(ncFileID, XVarID, 'long_name','Coordonnees UTM northing des points'))
      call check(nf90_put_att(ncFileID, XVarID, 'units','m'))
      call check(nf90_put_att(ncFileID, XVarID,  '_FillValue', -9999.0 ) )
      call check(nf90_put_att(ncFileID, XVarID,  'missing_value', -9999.0 ) )      

      write(*,*) 'Variables easting_pts et northing_pts OK'      

      call check(nf90_def_var(ncFileID,'area_pts'    ,nf90_float,(/ptsDimID/),XVarID))    
      call check(nf90_put_att(ncFileID, XVarID, 'long_name','Aire representative du point'))
      call check(nf90_put_att(ncFileID, XVarID, 'units','m2'))
      call check(nf90_put_att(ncFileID, XVarID,  '_FillValue', -9999.0 ) )
      call check(nf90_put_att(ncFileID, XVarID,  'missing_value', -9999.0 ) )      

      write(*,*) 'Variable area_pts OK'   
      
      call check(nf90_def_var(ncFileID,'pts' ,nf90_char,(/StrLenDimID,ptsDimID/),XVarID))    
      call check(nf90_put_att(ncFileID, XVarID, 'long_name','Noms des points'))
      write(*,*) 'Noms points OK'      
      
      call check(nf90_def_var(ncFileID,'vars' ,nf90_char,(/StrLenDimID,varsDimID/),XVarID))    
      call check(nf90_put_att(ncFileID, XVarID, 'long_name','Noms des varaibles'))            
      write(*,*) 'Noms variables OK'

      call check(nf90_def_var(ncFileID,'Times',nf90_char,(/frTimeStrDimID,frTimeDimID/),XVarID) )
      call check(nf90_put_att(ncFileID, XVarID, 'long_name','date au format CHIMERE/WRF'))            
      write(*,*) 'Variable Times OK'    
            
      do n= 1, nvars      
      call check(nf90_def_var(ncFileID,trim(var_name(n))    ,nf90_float,(/ptsDimID,frTimeDimID/),XVarID))    
      call check(nf90_put_att(ncFileID, XVarID, 'long_name','Concentration '//trim(var_name(n))))
      call check(nf90_put_att(ncFileID, XVarID, 'units','microg/m3'))
      call check(nf90_put_att(ncFileID, XVarID,  '_FillValue', -9999.0 ) )  
      call check(nf90_put_att(ncFileID, XVarID,  'missing_value', -9999.0 ) )          
      write(*,*) 'Variable '//trim(var_name(n))//' OK'

      if (ifond) then
      call check(nf90_def_var(ncFileID,trim(var_name(n))//'_fond',nf90_float,(/frTimeDimID/),XVarID))    
      call check(nf90_put_att(ncFileID, XVarID, 'long_name','Concentration de fond '//trim(var_name(n))))
      call check(nf90_put_att(ncFileID, XVarID, 'units','microg/m3'))
      call check(nf90_put_att(ncFileID, XVarID,  '_FillValue', -9999.0 ) )  
      call check(nf90_put_att(ncFileID, XVarID,  'missing_value', -9999.0 ) )          
      write(*,*) 'Variable '//trim(var_name(n))//'_fond OK'
      end if
      
      if (igrille) then
      call check(nf90_def_var(ncFileID,trim(var_name(n))//'_grille',nf90_float,(/ptsDimID,frTimeDimID/),XVarID))    
      call check(nf90_put_att(ncFileID, XVarID, 'long_name','Concentration grille interpolee '//trim(var_name(n))))
      call check(nf90_put_att(ncFileID, XVarID, 'units','microg/m3'))
      call check(nf90_put_att(ncFileID, XVarID,  '_FillValue', -9999.0 ) )  
      call check(nf90_put_att(ncFileID, XVarID,  'missing_value', -9999.0 ) )          
      write(*,*) 'Variable '//trim(var_name(n))//'_grille OK'
      end if            

      if (iprox) then
      call check(nf90_def_var(ncFileID,trim(var_name(n))//'_prox',nf90_float,(/frTimeDimID/),XVarID))    
      call check(nf90_put_att(ncFileID, XVarID, 'long_name','Concentration suremis prox '//trim(var_name(n))))
      call check(nf90_put_att(ncFileID, XVarID, 'units','microg/m3'))
      call check(nf90_put_att(ncFileID, XVarID,  '_FillValue', -9999.0 ) )  
      call check(nf90_put_att(ncFileID, XVarID,  'missing_value', -9999.0 ) )          
      write(*,*) 'Variable '//trim(var_name(n))//'_prox OK'
      end if 

      end do
                        
      call check(nf90_close(ncFileID))  
      
      ! ECRITURE
      call check(nf90_open(trim(fn_cdf), nf90_write, ncFileID))   
         
      call check(nf90_inq_varid(ncFileID, 'x_pts', xVarID) )
      call check(nf90_put_var(ncFileID, xVarID, pts_x, start = (/1/) ))   
         
      call check(nf90_inq_varid(ncFileID, 'y_pts', xVarID) )
      call check(nf90_put_var(ncFileID, xVarID, pts_y, start = (/1/) ))   
      
      call check(nf90_inq_varid(ncFileID, 'easting_pts', xVarID) )
      call check(nf90_put_var(ncFileID, xVarID, pts_easting, start = (/1/) ))   
         
      call check(nf90_inq_varid(ncFileID, 'northing_pts', xVarID) )
      call check(nf90_put_var(ncFileID, xVarID, pts_northing, start = (/1/) ))         
      
      write(*,*) 'Ecriture X et Y OK'  
      
      call check(nf90_inq_varid(ncFileID, 'pts', xVarID) )
      call check(nf90_put_var(ncFileID, xVarID, pts_name, start = (/1/) ))  

      write(*,*) 'Ecriture points OK' 
      
      call check(nf90_inq_varid(ncFileID, 'vars', xVarID) )
      call check(nf90_put_var(ncFileID, xVarID, var_name, start = (/1/) ))        
            
      write(*,*) 'Ecriture variables OK'                           

      call check(nf90_close(ncFileID))         
      
      write(*,*) 'Fichier NetCDF creer : '//trim(fn_cdf)
     
      end program

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!     
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
     

