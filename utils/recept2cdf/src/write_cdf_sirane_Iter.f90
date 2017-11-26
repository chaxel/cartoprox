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
      character(len=256) :: dir_out       ! repertoire sorties /chemin/RESULTAT_2008
      character(len=256) :: fn_evol       ! fichier Evol-recept
      character(len=256) :: fn_cdf        ! fichier NetCDF sorties
      character(len=256) :: fn_fond       ! fichier ASCII fond
      character(len=256) :: fn_evolmeteo  ! fichier ASCII evol-fond
      character(len=256) :: fn_meteo      ! fichier ASCII meteo 
      
!-----dimensions                 
      integer :: npts,nvars 

!-----noms des recepteurs et des variables
      character(len=256), allocatable :: pts_name(:)            
      character(len=256), allocatable :: var_name(:)     

!-----local
      integer :: n, it, l
      integer :: iter, ipt
      character(len=5)  :: iterstr
      character(len=256):: receptstr      
      character(len=1)  :: str_tmp    
      integer :: ios
      
!-----sortie    
      real,allocatable    :: conc_pt(:,:)
      real,allocatable    :: x_pt(:), y_pt(:)
            
!-----Dates
      integer :: numFrTimes
      
      character(len=8) :: YYYYMMDDstr 
      
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
                    
 !----netcdf related
      integer :: ncFileID, frTimeDimID, frTimeVarID, ptsDimID, varsDimID
      integer :: xVarID         
      character(len=39) :: tunits  
      
      character(len=19) :: datestr ! lit la date du debut du fichier exemple 2009-01-01 00:00:00  
      
      integer :: iter_valide  
      
!-----stockage des entrées de SIRANE
      integer, parameter :: niter_max = 366 * 24
      integer, parameter :: nvar_max  = 3
      integer   :: nvar_fond
      integer   :: nvar_meteo
      integer   :: nvar_evolmeteo          
      logical   :: imeteo, ifond, ievolmeteo
      real      :: conc_fond(niter_max,nvar_max)
      real      :: evolmeteo(niter_max,nvar_max)
      real      :: meteo    (niter_max,nvar_max)
      character(len=10) :: jourstr
      character(len=5)  :: heurestr
      
!-----CORPS DU PROGRAMME---------------------------------      
      
      ! lit l'argument
      call getarg(1,YYYYMMDDstr)
      call getarg(2,dir_out)      
      call getarg(3,fn_cdf)
      call getarg(4,fn_fond)      

      if ( trim(dir_out) == '' ) then 
        write(*,*) 'Syntaxe : dir_out [fn_cdf] '
	stop
      end if  
      
      !Inclu les fichiers de fond dans le NetCDF de sortie ? 
      ifond=.false.      
      if ( trim(fn_fond).ne. '' )ifond=.true.

      if ( index(fn_cdf,'.nc') .eq. 0 ) then 
        fn_cdf='defaut.nc'
        write(*,*) 'Utilise defaut : '//trim(fn_cdf)
      end if
      
! Determine la date de debut -> 1er janvier de l'année en cours
      write(*,*)'YYYYMMDDstr='//YYYYMMDDstr
      read(YYYYMMDDstr,'(I4,I2,I2)')Year,Mnth,Day !en argument
      write(*,*)'Year=',Year
      write(*,*)'Mnth=',Mnth      
      write(*,*)'Day=',Day      
      Hour=0
      Min=0
      Sec=0            
      call rdate(Year,Mnth,Day,Hour,idate) 
            
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
      
! Lecture des fichiers entrees de SIRANE
! FOND
      if (ifond) then
        open(unit=10,file=trim(fn_fond),status='old',iostat=ios)                       
          if (ios.eq.0) then ! fichier existe ?    
	  write(*,*) '->'//trim(fn_fond)             
          read(10,fmt=*,end=101, err=102) nvar_fond
	  do iter = 1, niter_max
            read(10,fmt=*,end=101, err=102) conc_fond(iter,1:nvar_fond)
	    !write(*,*) 'FOND',iter,conc_fond(iter,1:nvar_fond)
          end do
101       continue
          write(*,*) 'Fond SIRANE OK'
        end if
      end if
! FOND
                   
      !!! Boucle sur les iterations horaires !!!
      iter_valide=0      
      do iter = 1, niter_max
      
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
      
      ! String de l'iteration 
      write(iterstr,'(I5)') iter
      
      !fn_evol = trim(dir_out)//trim(Mnthstr)//'/Iteration-recept-'//trim(adjustl(iterstr))//'.dat'
      fn_evol = trim(dir_out)//'/Iteration-recept-'//trim(adjustl(iterstr))//'.dat'      
      
        !write(*,*) 'Lit dans '//trim(fn_evol)      
                  
      open(unit=10,file=trim(fn_evol),status='old',iostat=ios)           
            
      if (ios.eq.0) then ! fichier existe ?
      !write(*,*) 'Lit dans '//trim(fn_evol)    
        
      write(*,'("Iteration ",I4)') iter
             
      ! header
      read(10,fmt=*,end=91)
	
      do ipt= 1, npts 
         read(10,fmt=*,end=91, err=28) receptstr, x_pt(ipt), y_pt(ipt), conc_pt(ipt,1:nvars)
         go to 29
28       continue ! un problème sur les concentrations          
	 conc_pt(ipt,:) = -9999.0
29       continue	 	 
	 !write(*,*) receptstr, x_pt, y_pt, conc(ipt,1:nvars)  
      end do
      go to 92

91    continue
      write(*,*) 'ERREUR: fichier '//trim(fn_evol)//' incomplet au recepteur',ipt
      stop

92    continue ! tout va bien...
      
      close(10)
              
      iter_valide = iter_valide + 1
      ! Ecrit les concentrations dans le NetCDF pour le mois x pour tous les recepteurs 
      !write(*,'("Ecrit concentrations pour Iteration=",I6)') iter
      call check(nf90_inq_varid(ncFileID, 'Times', xVarID) )
      call check(nf90_put_var(ncFileID, xVarID, datestr, start = (/1,iter_valide/) ))
       
      do n= 1, nvars
        call check(nf90_inq_varid(ncFileID, trim(var_name(n)), xVarID) )
        call check(nf90_put_var(ncFileID, xVarID, conc_pt(:,n), start = (/1,iter_valide/) ))                  
      end do 
      
      ! ecrit le fond
      if (ifond) then
        if (nf90_inq_varid(ncFileID,'NO_fond', xVarID) .eq. nf90_noerr) & 
	call check(nf90_put_var(ncFileID, xVarID, conc_fond(iter_valide,1), start = (/iter_valide/) ))
        if (nf90_inq_varid(ncFileID,'NO2_fond', xVarID) .eq. nf90_noerr) & 
	call check(nf90_put_var(ncFileID, xVarID, conc_fond(iter_valide,2), start = (/iter_valide/) ))
        if (nf90_inq_varid(ncFileID,'O3_fond', xVarID) .eq. nf90_noerr) & 
	call check(nf90_put_var(ncFileID, xVarID, conc_fond(iter_valide,3), start = (/iter_valide/) ))
        if (nf90_inq_varid(ncFileID,'PM10_fond', xVarID) .eq. nf90_noerr) & 
	call check(nf90_put_var(ncFileID, xVarID, conc_fond(iter_valide,1), start = (/iter_valide/) ))
        if (nf90_inq_varid(ncFileID,'PM25_fond', xVarID) .eq. nf90_noerr) & 
	call check(nf90_put_var(ncFileID, xVarID, conc_fond(iter_valide,1), start = (/iter_valide/) ))					        
      end if
                                
      end if ! fichier Iteration existe -> ITER VALIDE
      
      ! Mise a jour de la date via idate -> ajoute une heure     
      call reldat(idate,1,idate) 

      end do ! iter
                
      call check(nf90_close(ncFileID))

      stop 0

99    write(*,*) 'write_cdf_sirane_Iter.f90: erreur de lecture'
      write(*,*) 'fichier '//trim(fn_evol)
      write(*,*) 'ligne',ipt
      
      stop 1
      
102   write(*,*) 'write_cdf_sirane_Iter.f90: erreur de lecture'
      write(*,*) 'fichier '//trim(fn_fond)
      write(*,*) 'ligne',iter
      
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

