      subroutine read_args

      use params

      implicit none
      
!     arguments
      
      character(len=256) :: argstr
      integer :: iarg
      logical :: ihelp 
            
      ! logical local
               
      ixmin=.false.      
      iymin=.false.
      
      ixp=.false.
      iyp=.false. 
      
      idx=.false.
      idy=.false.            
                 
      ific=.false.
      
      ific_stations=.false.      
      
      ivar=.false.
      
      ihelp=.false.
      
      ismooth=.false. !smoothing
      
      it1 = 0
      it2 = 0
      istep=.false.
      
      ihour = .false.
      
      ijour=.false. 
      
      idatestr = .false.

      !version 6      
      datestr1 = '0000-00-00_00:00:00'
      datestr2 = '0000-00-00_00:00:00'           
      
      agregation = 'moy'
         
      ! lit l'argument
      iarg = 0    
      argstr='null'	  
      do while ( trim(adjustl(argstr)) .ne. '' )
	iarg = iarg + 1	
        call getarg(iarg,argstr)		
        if ( trim(adjustl(argstr)).eq. '-i' )  then
	  call getarg(iarg+1,fic_grille_nc)
	  ific = .true.
        else if ( trim(adjustl(argstr)).eq. '-s' )  then
	  call getarg(iarg+1,fic_stations)
	  ific_stations = .true.	  	  	  	  	  					
        else if ( trim(adjustl(argstr)).eq. '-xmin' ) then
	  call getarg(iarg+1,argstr)
	  read(argstr,*) xmin
	  ixmin = .true.		
        else if ( trim(adjustl(argstr)).eq. '-ymin' )  then
	  call getarg(iarg+1,argstr)
	  read(argstr,*) ymin
	  iymin = .true.
        else if ( trim(adjustl(argstr)).eq. '-moy' )  then ! par defaut
	  call getarg(iarg+1,argstr)
	  agregation = 'moy'
        else if ( trim(adjustl(argstr)).eq. '-max' )  then
	  call getarg(iarg+1,argstr)
	  agregation = 'max'
        else if ( trim(adjustl(argstr)).eq. '-x' )  then
	  call getarg(iarg+1,argstr)
	  allocate(xp(1))
	  read(argstr,*) xp(1)
	  ixp = .true.	  
        else if ( trim(adjustl(argstr)).eq. '-y' )  then
	  call getarg(iarg+1,argstr)
	  allocate(yp(1))
	  read(argstr,*) yp(1)
	  iyp = .true.
        else if ( trim(adjustl(argstr)).eq. '-smooth' )  then
	  call getarg(iarg+1,argstr)
	  read(argstr,*) nsmooth
	  ismooth = .true.	  
        else if ( trim(adjustl(argstr)).eq. '-dx' )  then
	  call getarg(iarg+1,argstr)
	  read(argstr,*) dx
	  dy=dx
	  idx = .true.
        else if ( trim(adjustl(argstr)).eq. '-dy' )  then
	  call getarg(iarg+1,argstr)
	  read(argstr,*) dy
	  idy = .true.	
        else if ( trim(adjustl(argstr)).eq. '-var' )  then
	  call getarg(iarg+1,argstr)
	  read(argstr,*) var_nom
	  ivar = .true.	
        else if ( trim(adjustl(argstr)).eq. '-it1' )  then
	  call getarg(iarg+1,argstr)
	  read(argstr,*) it1
	  istep = .true.
        else if ( trim(adjustl(argstr)).eq. '-it' )  then
	  call getarg(iarg+1,argstr)
	  read(argstr,*) it1
	  istep = .true.	  
        else if ( trim(adjustl(argstr)).eq. '-it2' )  then
	  call getarg(iarg+1,argstr)
	  read(argstr,*) it2
	  istep = .true.
        else if ( trim(adjustl(argstr)).eq. '-datestr1' )  then
	  call getarg(iarg+1,argstr)
	  read(argstr,*) datestr1
        else if ( trim(adjustl(argstr)).eq. '-datestr2' )  then
	  call getarg(iarg+1,argstr)
	  read(argstr,*) datestr2
        else if ( trim(adjustl(argstr)).eq. '-hour' )  then
	  call getarg(iarg+1,argstr)
	  ihour = .true.
        else if ( trim(adjustl(argstr)).eq. '-jour' )  then
	  call getarg(iarg+1,argstr)
	  ijour = .true.	  	  	  		  	  		    	  	  	  	  	  
        else if ( trim(adjustl(argstr)).eq. '-debug'.or.trim(adjustl(argstr)).eq. '-d' )  then
          idebug=.true.		
        else if ( trim(adjustl(argstr)).eq. '-h'.or.trim(adjustl(argstr)).eq. '--help'.or.trim(adjustl(argstr)).eq. '-help' )  then
          ihelp=.true.	  	    
	endif	
		  		
      end do            
            
      ! TRAITEMENT DES ERREURS ET ENTREES
      !version 6
      if ((datestr1.ne.'0000-00-00_00:00:00' ).and.(datestr2.ne.'0000-00-00_00:00:00' ))then
        if(idebug)write(*,*)'datestr1='//datestr1
	if(idebug)write(*,*)'datestr2='//datestr2
        idatestr=.true.
      end if
      
      if (ixp.and.iyp)allocate(zp(1))

      if (it2.lt.it1) it2 = it1
      
      if (ihelp) then
        call help_me
	stop
      end if
      
      if (.not.(ific)) then
        write(*,*) 'Indiquer le fichier NetCDF -i'
	stop
      end if      

      if (.not.(ivar)) then
        write(*,*) 'Indiquer la variable -var'
	stop
      end if      
      
      if ((.not.ific_stations).and.((ixp.and.(.not.iyp)).or.(iyp.and.(.not.ixp)))) then
        write(*,*) 'Indiquer des valeurs pour -x et -y'
	stop
      end if
      
      if ((.not.idx).and.(.not.idy)) then
        write(*,*) 'Indiquer des valeurs pour -dx et -dy'
	stop
      end if 
      
      if (ific_stations) then
	ihour = .false. !desactive sorties horaires    
      end if                 
      
      if ((ixp.and.iyp).or.ific_stations) then
	goto 99        
      end if      
                                  
      write(*,*) 'Syntaxe  -i fichier.XY -xmin -ymin -dx -x -y'  
      write(*,*) 'Argument -h/--help pour obtenir de l aide'              
      
      stop       
      
99    continue

      !offre la posibilité de fournir un fichier dans le format netCDF *.nc (VERSION 2)
      !x_pts(Point),y_pts(Point)
      !on rempli alors ce fichier pour le polluant concerné
      if (index(fic_stations,'.nc').ne.0) then
        ific_stations_nc=.true.
	ihour = .true.
	write(*,*) 'Stations au format NetCDF'
	write(*,*) 'ihour=',ihour
      else
        ific_stations_nc=.false.      
      end if
      !----------------------------------------------------------------------------          

      if(idebug)write(*,'("xmin=",F10.2)')xmin
      if(idebug)write(*,'("ymin=",F10.2)')ymin
      if(idebug)write(*,'("dx  =",F10.2)')dx
      if(idebug)write(*,'("dy  =",F10.2)')dy      
      !if(idebug)write(*,'("xc  =",F10.2)')xp(1)
      !if(idebug)write(*,'("yc  =",F10.2)')yp(1)
      if(idebug)write(*,'("it1 =",I10  )')it1
      if(idebug)write(*,'("it2 =",I10  )')it2
              
      end subroutine  
      
      
!--------------------------------------------------------------------
      subroutine help_me
      
      use params
      
      implicit none

      write(*,*) '#######################################################################'
      write(*,*) '# CARTOPROX - Interpolateur de grille CHIMERE'
      write(*,*) '# Atmo Rhone-Alpes 2010'
      write(*,*) '# version '//trim(versionstr)
      write(*,*) '# contact: echaxel@atmo-rhonealpes.org'								       
      write(*,*) '#######################################################################'
      write(*,*) ''
      write(*,*) 'BUT: Extraction de données d une grille reguliere CHIMERE'

                                 
      end subroutine help_me
!--------------------------------------------------------------------
