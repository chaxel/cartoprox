      subroutine read_args

      use params

      implicit none
      
!     arguments
      
      character(len=256) :: argstr
      integer :: iarg 
            
      ! logical local               
      ivar=.false.
      
      icoarse=.false.
      ixminc=.false.      
      iyminc=.false.      
      idxc=.false.
      idyc=.false.            

      ifine=.false.
      ixminf=.false.      
      iyminf=.false.      
      idxf=.false.
      idyf=.false. 
                 
      ihelp=.false.
      
      it1 = 0
      it2 = 0
        
      ! lit l'argument
      iarg = 0    
      argstr='null'	  
      do while ( trim(adjustl(argstr)) .ne. '' )
	iarg = iarg + 1	
        call getarg(iarg,argstr)		
        if ( trim(adjustl(argstr)).eq. '-i' )  then
	  call getarg(iarg+1,fic_fine)
	  ifine = .true.
        else if ( trim(adjustl(argstr)).eq. '-o' )  then
	  call getarg(iarg+1,fic_coarse)
	  icoarse = .true.	  	  	  	  	  					
        else if ( trim(adjustl(argstr)).eq. '-xminc' ) then
	  call getarg(iarg+1,argstr)
	  read(argstr,*) xminc
	  ixminc = .true.		
        else if ( trim(adjustl(argstr)).eq. '-yminc' )  then
	  call getarg(iarg+1,argstr)
	  read(argstr,*) yminc
	  iyminc = .true.
        else if ( trim(adjustl(argstr)).eq. '-xminf' ) then
	  call getarg(iarg+1,argstr)
	  read(argstr,*) xminf
	  ixminf = .true.		
        else if ( trim(adjustl(argstr)).eq. '-yminf' )  then
	  call getarg(iarg+1,argstr)
	  read(argstr,*) yminf
	  iyminf = .true.	  	  
        else if ( trim(adjustl(argstr)).eq. '-dxc' )  then
	  call getarg(iarg+1,argstr)
	  read(argstr,*) dxc
	  dyc=dxc
	  idxc = .true.
        else if ( trim(adjustl(argstr)).eq. '-dxf' )  then
	  call getarg(iarg+1,argstr)
	  read(argstr,*) dxf
	  dyf=dxf
	  idxf = .true.	
        else if ( trim(adjustl(argstr)).eq. '-v' )  then
	  call getarg(iarg+1,argstr)
	  read(argstr,*) var_nom
	  ivar = .true.		  	  	  		  	  		    	  	  	  	  	  
        else if ( trim(adjustl(argstr)).eq. '-debug'.or.trim(adjustl(argstr)).eq. '-d' )  then
          idebug=.true.		
        else if ( trim(adjustl(argstr)).eq. '-h'.or.trim(adjustl(argstr)).eq. '--help'.or.trim(adjustl(argstr)).eq. '-help' )  then
          ihelp=.true.	  	    
	endif	
		  		
      end do            
            
      ! TRAITEMENT DES ERREURS ET ENTREES

      if (it2.lt.it1) it2 = it1
      
      if (ihelp) then
        call help_me
	stop
      end if
      
      if (.not.(icoarse)) then
        write(*,*) 'Indiquer le fichier NetCDF SORTIE -o'
	stop
      end if   
         
      if (.not.(ivar)) then
        write(*,*) 'Indiquer la variable -v'
	stop
      end if 
      
      if (.not.(ifine)) then
        write(*,*) 'Indiquer le fichier NetCDF ENTREE -i'
	stop
      end if     
            
      if (.not.idxf) then
        write(*,*) 'Indiquer valeur pour -dxf'
	stop
      end if 

      if (.not.idxc) then
        write(*,*) 'Indiquer valeur pour -dxc'
	stop
      end if 
                                             
      !write(*,*) 'Syntaxe  -i fichier.XY -xmin -ymin -dx -x -y'  
      !write(*,*) 'Argument -h/--help pour obtenir de l aide'              
      
      !stop       
      
99    continue

      !------------------------------------------------------------
      if(idebug)write(*,'("xminf=",F10.2)')xminf
      if(idebug)write(*,'("yminf=",F10.2)')yminf
      if(idebug)write(*,'("dxf  =",F10.2)')dxf
      if(idebug)write(*,'("dyf  =",F10.2)')dyf   
      if(idebug)write(*,'("xminc=",F10.2)')xminc
      if(idebug)write(*,'("yminc=",F10.2)')yminc
      if(idebug)write(*,'("dxc  =",F10.2)')dxc
      if(idebug)write(*,'("dyc  =",F10.2)')dyc                              
      end subroutine  
            
!-------------------------------------------------------------------
      subroutine help_me
      
      use params
      
      implicit none

      write(*,*) '#######################################################################'
      write(*,*) '# CARTOPROX - Mosaique de grilles CHIMERE'
      write(*,*) '# Atmo Rhone-Alpes 2011'
      write(*,*) '# version '//trim(versionstr)
      write(*,*) '# contact: echaxel@atmo-rhonealpes.org'								       
      write(*,*) '#######################################################################'
      write(*,*) ''
      write(*,*) 'BUT: concatener plusieurs grilles CHIMERE sous forme de mosaique'
      write(*,*) 'SYNTAXE: -i -dxf -xminf -xminc -o -dyc xminc -yminc'      

                                 
      end subroutine help_me
!--------------------------------------------------------------------
