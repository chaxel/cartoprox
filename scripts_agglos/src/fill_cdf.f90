      program write_cdf_sirane_v1
! ********************************************************************
! * ECRIT DANS UN FICHIER NETCDF LES RESULTATS AU RECEPTEURS ou RUES *
! * AUTHOR:	  E. CHAXEL					     *
! *		  COPARLY	                                     *
! * LAST MODIFICATION: FEB. 2010				     *
! ********************************************************************

      use netcdf
      use typesizes

      implicit none
      
!-----Input 
      character(len=256) :: fn_cdf   ! fichier Evol-recept
      character(len=256) :: fn_ijz   ! fichier NetCDF sorties
      character(len=30)  :: var_name, valminstr
      real :: valmin     
    
      
!-----local
      integer :: n, it, ipt, npts, nxx, nyy, nlev, ix, iy
      
!-----allocatable
      integer, parameter   :: npts_max=100000000
      integer, allocatable :: ix_pt(:), iy_pt(:)
      real,    allocatable :: val_pt(:) 
      
      real,    allocatable :: val_grille(:,:,:)
      
      integer :: var_dim
                    
      
!-----inutile                
      integer :: numFrTimes          
                    
 !----netcdf related
      integer :: ncFileID, frTimeDimID, latDimID, lonDimID
      integer :: xVarID         
      character(len=39) :: tunits  

!-----CORPS DU PROGRAMME---------------------------------      
      
      ! lit l'argument
      call getarg(1,fn_cdf)      
      call getarg(2,fn_ijz)
      call getarg(3,var_name)  
      call getarg(4,valminstr)    
      
      read(valminstr,*) valmin

      if ( trim(fn_ijz) == '' ) then 
        write(*,*) 'Syntaxe : fn_cdf fn_ijz var'
	stop
      end if
      
      if ( index(fn_cdf,'.nc') .eq. 0 ) then 
        write(*,*) '***ERREUR: Pas d extension .nc - Check NetCDF : '//trim(fn_cdf)
	stop
      end if
      
! Lit les données ASCII
      open(unit=10,file=fn_ijz,status='old')
      allocate(ix_pt(npts_max))
      allocate(iy_pt(npts_max))
      allocate(val_pt(npts_max))
      
      do ipt=1, npts_max
         read(10,*,end=99)ix_pt(ipt),iy_pt(ipt),val_pt(ipt)
      end do

99    continue
      npts = ipt - 1

      close(10)
      
! Lit les données 
      call check(nf90_open(trim(fn_cdf), nf90_write, ncFileID))
      
      if (nf90_inq_dimid(ncFileID, 'west_east'   , lonDimID).ne.nf90_noerr)&
          call check(nf90_inq_dimid(ncFileID, 'X'   , lonDimID))       
      if (nf90_inq_dimid(ncFileID, 'south_north' , latDimID).ne.nf90_noerr)&
          call check(nf90_inq_dimid(ncFileID, 'Y'   , latDimID))      
      
      call check(nf90_Inquire_Dimension(ncFileID, latDimID   , len=nyy))
      call check(nf90_Inquire_Dimension(ncFileID, lonDimID   , len=nxx))            
      numFrTimes=1
      if (nf90_inq_dimid(ncFileID, 'Time', frTimeDimID).eq.nf90_noerr)&
        call check(nf90_Inquire_Dimension(ncFileID, frTimeDimID, len= numFrTimes))      
      if (numFrTimes.gt.1) then
        write(*,*) '***WARNING dans fillcdf.f90 : numFrTimes > 1'
!	stop
      end if                         

      allocate(val_grille (nxx,nyy,1))
        write(*,*) '***info: allocate ',nxx,nyy      
      val_grille = 0.
  
      ! Lit la grille
      it= 1
      nlev= 1
      if (check(nf90_inq_varid(ncFileID, trim(var_name), xVarID).eq.nf90_noerr) then
            write(*,*) '***info: var '//trim(var_name)//' OK'
      else
        write(*,*) '***ERREUR dans fillcdf.f90 : variable inconnue '//trim(var_name)       
      end if   
      call check(nf90_inquire_variable(ncFileID, xVarID, ndims = var_dim ))
      if (itime) then
        if (var_dim.eq.4)call check(nf90_get_var(ncFileID, xVarID, val_grille(:,:,1) ,  start=(/1,1,1,it/),count=(/nxx,nyy,nlev/))) 
        if (var_dim.eq.3)call check(nf90_get_var(ncFileID, xVarID, val_grille(:,:,1) ,  start=(/1,1,  it/),count=(/nxx,nyy,   1/)))
        if (var_dim.eq.2)call check(nf90_get_var(ncFileID, xVarID, val_grille(:,:,1) ,  start=(/1,1     /),count=(/nxx,nyy,   1/)))		 
      else
        if (var_dim.eq.3)call check(nf90_get_var(ncFileID, xVarID, val_grille(:,:,1) ,  start=(/1,1,1   /),count=(/nxx,nyy,nlev/))) 
        if (var_dim.eq.2)call check(nf90_get_var(ncFileID, xVarID, val_grille(:,:,1) ,  start=(/1,1     /),count=(/nxx,nyy,   1/)))     
      end if
      
      
      !call check(nf90_inq_varid(ncFileID, trim(var_name), xVarID) )
      !call check(nf90_get_var(ncFileID, xVarID, val_grille, start = (/1,1,1,1/) ))     
      
      write(*,*) '***info: lit la variable '//trim(var_name)//' OK'
      
      if ( npts.ge.1) then
      do ipt=1,npts
        ix=ix_pt(ipt)
	iy=iy_pt(ipt)
        if ( ix.lt.1 .or. ix.gt.nxx .or. iy.lt.1 .or. iy.gt.nyy ) then
	  write(*,*) '***ERREUR: ligne, ix, iy',ipt,ix,iy
	  stop
	end if
	! formule propre à CARTOPROX
        val_grille(ix,iy,1) = val_grille(ix,iy,1) - val_pt(ipt)
	if ( val_grille(ix,iy,1).lt.valmin ) val_grille(ix,iy,1) = valmin
        write(*,*) '***info: ligne, ix, iy, val, dval',ipt,ix,iy,val_grille(ix,iy,1),val_pt(ipt)
      end do
      call check(nf90_inq_varid(ncFileID, trim(var_name), xVarID) )
      write(*,*) '***info: var '//trim(var_name)//' OK'
      if (itime) then
        if (var_dim.eq.4)call check(nf90_put_var(ncFileID, xVarID, val_grille(:,:,1) ,  start=(/1,1,1,it/),count=(/nxx,nyy,nlev/))) 
        if (var_dim.eq.3)call check(nf90_put_var(ncFileID, xVarID, val_grille(:,:,1) ,  start=(/1,1,  it/),count=(/nxx,nyy,   1/)))
        if (var_dim.eq.2)call check(nf90_put_var(ncFileID, xVarID, val_grille(:,:,1) ,  start=(/1,1     /),count=(/nxx,nyy,   1/)))		 
      else
        if (var_dim.eq.3)call check(nf90_put_var(ncFileID, xVarID, val_grille(:,:,1) ,  start=(/1,1,1   /),count=(/nxx,nyy,nlev/))) 
        if (var_dim.eq.2)call check(nf90_put_var(ncFileID, xVarID, val_grille(:,:,1) ,  start=(/1,1     /),count=(/nxx,nyy,   1/)))     
      end if      
            
      !call check(nf90_put_var(ncFileID, xVarID, val_grille, start = (/1,1,1,1/) ))                    
       
        write(*,*) '***info: fichier '//trim(fn_cdf)//' mis a jour' 
      else
        write(*,*) '***info: fichier '//trim(fn_cdf)//' non mis a jour'                   
      end if

      if (nf90_inq_varid(ncFileID, 'Time', xVarID).eq.nf90_noerr)&
        call check(nf90_put_var(ncFileID, xVarID, 0, start = (/1/) ))
      
      call check(nf90_close(ncFileID))
      
      end program

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!     
      subroutine check(status)
  ! Internal subroutine - checks error status after each netcdf, prints out text message each time
  !   an error code is returned. 
      use netcdf
      use typesizes
      integer, intent ( in) :: status
      if(status /= nf90_noerr) then     
      print*, 'Error occured'
      print *, trim(nf90_strerror(status))
      stop    
      end if
      end subroutine check  
     

