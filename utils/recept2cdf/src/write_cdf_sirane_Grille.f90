      program write_cdf_sirane_Iter
! ********************************************************************
! *  CARTOPROX                                                       *
! * ECRIT DANS UN FICHIER NETCDF LES RESULTATS AU RECEPTEURS ou RUES *
! * AUTEUR:	  E. CHAXEL					     *
! *		  COPARLY	                                     *
! * LAST MODIFICATION: DEC. 2010				     *
! ********************************************************************

      use netcdf
      use typesizes

      implicit none
      
!-----Input 
      character(len=256) :: fn_sir       ! fichier Evol-recept
      character(len=256) :: fn_cdf        ! fichier NetCDF sorties
      
!-----dimensions                 
      integer :: npts,nvars 

!-----noms des recepteurs et des variables
      character(len=256), allocatable :: pts_name(:)            
      character(len=256), allocatable :: var_name(:)     

!-----local
      integer :: it, ivar
      integer :: iter, ipt   
      integer :: ios
      
!-----sortie    
      real,allocatable    :: conc_pt(:,:)
      real,allocatable    :: x_pt(:), y_pt(:)
            
!-----Dates
      integer :: numFrTimes
      
      character(len=4) :: Yearstr 
      character(len=2) :: Mnthstr 
      character(len=2) :: Daystr
      character(len=2) :: Hourstr
      character(len=2) :: Minstr
      character(len=2) :: Secstr 
      
      integer :: Year		!Four-digit year of start time
      integer :: Mnth		!Month of start time
      integer :: Day		!Day of start time
      integer :: Hour		!Hour of start time
      integer :: Min		!Minute of start time
      integer :: Sec		!Second of start time  
      
      integer :: idate          !date au format YYYYMMDDHH  
         
      character(len=10) :: idateStr          !date au format YYYYMMDDHH      
                     
 !----netcdf related
      integer :: ncFileID, frTimeDimID, frTimeVarID, ptsDimID, varsDimID
      integer :: xVarID         
      character(len=39) :: tunits  
      
      character(len=19) :: datestr ! lit la date du debut du fichier exemple 2009-01-01 00:00:00  
      
      integer :: iter_valide  
      
!-----stockage des entrées de SIRANE
      integer, parameter :: niter_max = 366 * 24
      
!-----CORPS DU PROGRAMME---------------------------------      
      
      ! lit l'argument
      call getarg(1,idateStr)
      call getarg(2,fn_sir)      
      call getarg(3,fn_cdf)     

      if ( trim(fn_sir) == '' ) then 
        write(*,*) 'Syntaxe : date[AAAAMMJJ] fn_sir[X,Y,Z] (fn_cdf) '
	stop
      end if      
      
      if ( index(fn_cdf,'.nc') .eq. 0 ) then 
        fn_cdf='sirane.nc'
        write(*,*) 'Utilise defaut : '//trim(fn_cdf)
      end if
      
! Determine la date de debut -> 1er janvier de l'année en cours
      read(idateStr,*)idate !en argument
      idate = idate * 100
            
! Lit les données 

      call check(nf90_open(trim(fn_cdf), nf90_write, ncFileID))  
      
      call check(nf90_inq_dimid(ncFileID, 'Time', frTimeDimID)) 
      call check(nf90_Inquire_Dimension(ncFileID, frTimeDimID, len= numFrTimes))
      
      call check(nf90_inq_dimid(ncFileID, 'Point',  ptsDimID)) 
      call check(nf90_Inquire_Dimension(ncFileID, ptsDimID, len= npts))      

      call check(nf90_inq_dimid(ncFileID, 'Variable', varsDimID)) 
      call check(nf90_Inquire_Dimension(ncFileID, varsDimID, len= nvars))
          
      write(*,*) 'Dimensions OK'
     
      allocate(pts_name(npts))
      allocate(var_name(nvars))
                
       ! tableaux lecture
      !allocate(datestr(nmax))
      allocate(x_pt      (npts      ))
      allocate(y_pt      (npts      ))                
      allocate(conc_pt(npts,nvars))      
            
      ! Recupere le nom des points et des variables
      call check(nf90_inq_varid(ncFileID, 'pts', xVarID) )
      call check(nf90_get_var(ncFileID, xVarID, pts_name, start = (/1,1/) ))  

      call check(nf90_inq_varid(ncFileID, 'vars', xVarID) )
      call check(nf90_get_var(ncFileID, xVarID, var_name, start = (/1,1/) ))             
                         
      !!! Boucle sur les iterations horaires !!!   
      iter = 1
      
      !! Ecrit la date
      call ddate(idate,Year,Mnth,Day,Hour)
      
      Min = 0
      write(yearStr,'(I4)') Year
      if (Mnth.LT.10) then
        write(mnthStr,'("0",I1)') Mnth
      else
        write(mnthStr,'(I2)') Mnth
      end if
 
      if (Day.LT.10) then
        write(dayStr,'("0",I1)') Day
      else
        write(dayStr,'(I2)') Day
      end if
 
      if (Hour.LT.10) then
        write(hourStr,'("0",I1)') Hour
      else
        write(hourStr,'(I2)') Hour
      end if
 
      if (Min.LT.10) then
        write(minStr,'("0",I1)') Min
      else
        write(minStr,'(I2)') Min
      end if
      secStr='00'

      ! String de la date
      write(datestr,'(A4,"-",A2,"-",A2,"_",A2,":",A2,":00")') yearStr,mnthStr,dayStr,hourStr,minStr
                                   
      open(unit=10,file=trim(fn_sir),status='old',iostat=ios)           
            
      if (ios.eq.0) then ! fichier existe ?
      do ipt= 1, npts 
         read(10,fmt=*,end=91, err=28) x_pt(ipt), y_pt(ipt), conc_pt(ipt,1:nvars)
         go to 29
28       continue ! un problème sur les concentrations          
	 conc_pt(ipt,:) = -9999.0
29       continue	 	 
	 !write(*,*) receptstr, x_pt, y_pt, conc(ipt,1:nvars)  
      end do
      go to 92

91    continue
      write(*,*) 'ERREUR: fichier '//trim(fn_sir)//' incomplet au recepteur',ipt
      stop 1

92    continue ! tout va bien...      
      close(10)
      
      else if (ios.ne.0) then
      
        write(*,*) 'ERREUR: fichier '//trim(fn_sir)// 'introuvable'      
        stop 1
	
      end if
              
      ! Ecrit les concentrations dans le NetCDF pour le mois x pour tous les recepteurs 
      call check(nf90_inq_varid(ncFileID, 'Times', xVarID) )
      call check(nf90_put_var(ncFileID, xVarID, datestr, start = (/1,iter/) ))
       
      do ivar= 1, nvars
        call check(nf90_inq_varid(ncFileID, trim(var_name(ivar)), xVarID) )
        call check(nf90_put_var(ncFileID, xVarID, conc_pt(:,ivar), start = (/1,iter/) ))                  
      end do 
                                           
      ! Mise a jour de la date via idate -> ajoute une heure     
      call reldat(idate,1,idate) 

      !end do ! iter
                
      call check(nf90_close(ncFileID))
      
      write(*,*) 'fichier '//trim(fn_sir)
      write(*,*) 'points OK=',npts     

      stop 0

99    write(*,*) 'write_cdf_sirane_Grille.f90: erreur de lecture'
      write(*,*) 'fichier '//trim(fn_cdf)
      write(*,*) 'ligne',ipt
      
      stop 1
            
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

