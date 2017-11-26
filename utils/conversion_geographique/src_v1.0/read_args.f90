      subroutine read_args

      use params

      implicit none
      
!     arguments
      
      character(len=256) :: argstr
      integer :: iarg
      logical :: ihelp 
            
      ! logical local      
      zoneutm_i = 31
      zoneutm_o = 31
      
      ! Grille USER
      igrilleuser=.false.        
      
      ixmin=.false.
      ixmax=.false. 
      iymin=.false.
      iymax=.false.
      ixc=.false.
      iyc=.false. 
      idx=.false.
      idy=.false.            
                 
      iout=.false.
      iascii=.false.
      igrille=.false.
      
      ihelp=.false.
      
      igeoid_i=.false.      
      iutm_zone_i=.false.
      iutm_i=.false.
      il2_i=.false.
      igeo_i=.false. 
            
      igeoid_o=.false.      
      iutm_zone_o=.false.
      iutm_o=.false.
      il2_o=.false.
      igeo_o=.false. 
      
      ipolair=.false.
      
      idebug=.false.
      
      iverbose=.false.
           
      ! lit l'argument
      iarg = 0    
      argstr='null'	  
      do while ( trim(adjustl(argstr)) .ne. '' )
	iarg = iarg + 1	
        call getarg(iarg,argstr)		
        if ( trim(adjustl(argstr)).eq. '-i' )  then
	  call getarg(iarg+1,fichier_ascii)
	  iascii = .true.
        else if ( trim(adjustl(argstr)).eq. '-o' ) then
	  call getarg(iarg+1,fichier_recept)
          iout=.true.
        else if (( trim(adjustl(argstr)).eq. '-g' ).or.( trim(adjustl(argstr)).eq. '-grille' )) then
	  call getarg(iarg+1,fichier_grille)
          igrille=.true.	  					
        else if ( trim(adjustl(argstr)).eq. '-xc' )  then
	  call getarg(iarg+1,argstr)
	  read(argstr,*) xc
	  ixc = .true.	  
        else if ( trim(adjustl(argstr)).eq. '-yc' )  then
	  call getarg(iarg+1,argstr)
	  read(argstr,*) yc
	  iyc = .true.
        else if ( trim(adjustl(argstr)).eq. '-polair' )  then
	  ipolair = .true.		
        else if ( trim(adjustl(argstr)).eq. '-v' )  then
	  iverbose = .true.		      	  	  	  	  	  
        else if ( trim(adjustl(argstr)).eq. '-debug'.or.trim(adjustl(argstr)).eq. '-d' )  then
          idebug=.true.		  	
        else if ( trim(adjustl(argstr)).eq. '-h'.or.trim(adjustl(argstr)).eq. '--help'.or.trim(adjustl(argstr)).eq. '-help' )  then
          ihelp=.true.
! DONNEE GEOGRAPHIQUES
        else if ( trim(adjustl(argstr)).eq. '-l2' )  then	  
	  if ((.not.il2_i).and.(.not.iutm_i).and.(.not.igeo_i)                                )  then 
	    il2_i = .true.
	  else 
	   il2_o = .true.
	  end if
	  
        else if ( trim(adjustl(argstr)).eq. '-geo' )  then	  
	  if ((.not.il2_i).and.(.not.iutm_i).and.(.not.igeo_i)                                )  then 
	    igeo_i = .true.
	  else
	    igeo_o = .true.
	  end if

        else if ( trim(adjustl(argstr)).eq. '-utm' )  then	
	  
	  if ((.not.il2_i).and.(.not.iutm_i).and.(.not.igeo_i)) then
	    iutm_i = .true.
	    call getarg(iarg+1,argstr)
	    read(argstr,*) zoneutm_i
	    iutm_zone_i=.true.	    
	  else
	    iutm_o = .true.
	    call getarg(iarg+1,argstr)
	    read(argstr,*) zoneutm_o
	    iutm_zone_o=.true.
	  end if
	  	  	  
        else if ( trim(adjustl(argstr)).eq. '-geoid' )  then

	  if (((.not.il2_o).and.(.not.iutm_o).and.(.not.igeo_o)).and.(il2_i.or.iutm_i.or.igeo_i)) then		  
	    call getarg(iarg+1,argstr)
	    read(argstr,*)  geoidstr_i
	    igeoid_i = .true.
	  else		  
	    call getarg(iarg+1,argstr)
	    read(argstr,*)  geoidstr_o
	    igeoid_o = .true.
	  end if
	  	    
	endif	
		  		
      end do            
            
      ! TRAITEMENT DES ERREURS ET ENTREES
      
      if (ihelp) then 
        call help_me
        stop	    
      end if
      
      if ((ixc.and.(.not.iyc)).or.(iyc.and.(.not.ixc))) then
        write(*,*) 'Indiquer des valeurs pour -xc et -yc'
	stop
      end if
      
      if (ixc.and.iyc) then
	goto 99        
      end if      
      
      if (iascii) then
        if (.not. iout) then
	  fichier_recept = 'conversion.txt'
	  iout=.true.
	end if
	goto 99
      end if
                            
      write(*,*) 'Syntaxe  -i fichier.XY [-utm 31 -l2 -geo -xc -yc -dx -polair]'  
      write(*,*) 'Argument -h/--help pour obtenir de l aide'              
      
      stop       
      
99    continue

!     COORDONNEES GEOGRAPHIQUES
!---------------------------------------------------------------------
! ENTREES
!---------------------------------------------------------------------
      if ((.not.iutm_i).and.(.not.igeo_i).and.(.not.il2_i)) then
        write(*,*) 'indiquer le systeme entree: -geo -l2 -utm'
	stop
      end if
            
      if ( igeo_i ) projection_i = 0
      if ( il2_i  ) projection_i = 1
      if ( iutm_i ) projection_i = 2 
            
      if (iverbose)write(*,*) '*** info: projection entree:',projection_i
                 
      if (.not.iutm_zone_i) zoneutm_i = 31
      if (.not.igeoid_i   ) geoidstr_i = 'WGS84'
      
      geoid_i = 0
      if ( trim(geoidstr_i) == 'WGS84' ) geoid_i = 1
      if ( trim(geoidstr_i) == 'ED50'  ) geoid_i = 2	  
      if ( trim(geoidstr_i) == 'NTF'   ) geoid_i = 3
      if ( trim(geoidstr_i) == 'GRS80' ) geoid_i = 4
      
      if (geoid_i.eq.0) then
        write(*,*) '*** erreur: geoid inconnue: '//trim(geoidstr_i)
	stop
      else 
        if (iverbose)write(*,*) '*** info: geoid entree: '//trim(geoidstr_i)
      end if
!---------------------------------------------------------------------
! SORTIES
!---------------------------------------------------------------------
      if ((.not.iutm_o).and.(.not.igeo_o).and.(.not.il2_o)) then
        write(*,*) 'indiquer le systeme de sortie: -geo -l2 -utm'
	stop
      end if 

      if ( igeo_o ) projection_o = 0
      if ( il2_o  ) projection_o = 1
      if ( iutm_o ) projection_o = 2 

      if (iverbose)write(*,*) '*** info: projection sortie:',projection_o
      
      if (.not.iutm_zone_o) zoneutm_o = 31
      if (.not.igeoid_o) geoidstr_o = 'WGS84'    
      
      geoid_o = 0
      if ( trim(geoidstr_o) == 'WGS84' ) geoid_o = 1
      if ( trim(geoidstr_o) == 'ED50'  ) geoid_o = 2	  
      if ( trim(geoidstr_o) == 'NTF'   ) geoid_o = 3
      if ( trim(geoidstr_o) == 'GRS80' ) geoid_o = 4
      if (geoid_o.eq.0) then
        write(*,*) '*** erreur: geoid inconnue dans params.f90: '//trim(geoidstr_o)
	stop
      else 
        if (iverbose)write(*,*) '*** info: geoid sortie: '//trim(geoidstr_o)	
      end if
      

          
      end subroutine  
      
      
!--------------------------------------------------------------------
      subroutine help_me
      
      use params
      
      implicit none

      write(*,*) '#######################################################################'
      write(*,*) '# PREVALP - Convertisseur de coordonnees geographiques'
      write(*,*) '# Atmo Rhone-Alpes 2010'
      write(*,*) '# version '//trim(versionstr)
      write(*,*) '# contact: echaxel@atmo-rhonealpes.org'								       
      write(*,*) '#######################################################################'
      write(*,*) ''
      write(*,*) 'BUT: Conversion de coordonnees geographique, UTM et Lambert 2 etendu'

                                 
      end subroutine help_me
!--------------------------------------------------------------------
