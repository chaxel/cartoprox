      subroutine read_args

      use params

      implicit none
      
!     arguments
      
      character(len=256) :: argstr
      integer :: iarg
      logical :: ihelp 
      logical :: idebug  
            
      ! logical local
      ixmin=.false.
      ixmax=.false. 
      iymin=.false.
      iymax=.false.
      ixc=.false.
      iyc=.false. 
      idx=.false.
      idy=.false.            
                 
      maillage_optimal=.false.
      maillage_virtuel=.false.
      
      ibrin_dx=.false.
      irecept_dx=.false.
      icadre_dx=.false.
      
      iout=.false.
      imif=.false.      
      
      ihelp=.false.

      ! lit l'argument
      iarg = 0    
      argstr='null'	  
      do while ( trim(adjustl(argstr)) .ne. '' )
	iarg = iarg + 1	
        write(*,*) 'Lit argument', iarg, trim(adjustl(argstr))	
        call getarg(iarg,argstr)	
        if ( trim(adjustl(argstr)).eq. '-i' )  then
	  call getarg(iarg+1,fichier_mif)
	  imif = .true.
        else if ( trim(adjustl(argstr)).eq. '-o' ) then
	  call getarg(iarg+1,fichier_recept)
          iout=.true.			
        else if ( trim(adjustl(argstr)).eq. '-optimal' ) then
          maillage_optimal=.true.			
        else if ( trim(adjustl(argstr)).eq. '-virtuel' ) then
          maillage_virtuel=.true.		
        else if ( trim(adjustl(argstr)).eq. '-xmin' ) then
	  call getarg(iarg+1,argstr)
	  read(argstr,*) xmin
	  ixmin = .true.
        else if ( trim(adjustl(argstr)).eq. '-xmax' ) then
	  call getarg(iarg+1,argstr)
	  read(argstr,*) xmax
	  ixmax = .true.		
        else if ( trim(adjustl(argstr)).eq. '-ymin' )  then
	  call getarg(iarg+1,argstr)
	  read(argstr,*) ymin
	  iymin = .true.	
        else if ( trim(adjustl(argstr)).eq. '-ymax' )  then
	  call getarg(iarg+1,argstr)
	  read(argstr,*) ymax
	  iymax = .true.
        else if ( trim(adjustl(argstr)).eq. '-xc' )  then
	  call getarg(iarg+1,argstr)
	  read(argstr,*) xc
	  ixc = .true.	  
        else if ( trim(adjustl(argstr)).eq. '-yc' )  then
	  call getarg(iarg+1,argstr)
	  read(argstr,*) yc
	  iyc = .true.
        else if ( trim(adjustl(argstr)).eq. '-dx' )  then
	  call getarg(iarg+1,argstr)
	  read(argstr,*) dx
	  idx = .true.
        else if ( trim(adjustl(argstr)).eq. '-dy' )  then
	  call getarg(iarg+1,argstr)
	  read(argstr,*) dy
	  idy = .true.
        else if ( trim(adjustl(argstr)).eq. '-brin_dx' )  then
	  call getarg(iarg+1,argstr)
	  read(argstr,*) brin_dx
	  ibrin_dx = .true.	  
        else if ( trim(adjustl(argstr)).eq. '-recept_dx' )  then	  
	  call getarg(iarg+1,argstr)
	  read(argstr,*) dx_recept_ini
	  irecept_dx = .true.
        else if ( trim(adjustl(argstr)).eq. '-cadre_dx' )  then
	  call getarg(iarg+1,argstr)
	  read(argstr,*) cadre_dx
	  icadre_dx = .true.	  	  	  	  	  
        else if ( trim(adjustl(argstr)).eq. '-debug'.or.trim(adjustl(argstr)).eq. '-d' )  then
          idebug=.true.		
        else if ( trim(adjustl(argstr)).eq. '-h'.or.trim(adjustl(argstr)).eq. '--help'.or.trim(adjustl(argstr)).eq. '-help' )  then
          ihelp=.true.		
	endif	
		
      end do            

      if (ihelp) then 
        call help_me
        stop	    
      end if
      
      if (.not. icadre_dx) then
        cadre_dx = 0.
      end if
      
      if (.not. iout) then
        fichier_recept = 'recept_pts_all.txt'
      end if
      
      if (ibrin_dx.and.irecept_dx.and.imif) then
        write(*,'("Largeur de discretisation des brins (m)=",F5.0)') brin_dx
        write(*,'("Pas de la grille de depart (m)=",F5.0)') dx_recept_ini  
        write(*,'("Largeur du cadre (m)=",F6.0)') cadre_dx
        go to 99   
      end if


      write(*,*) 'Syntaxe -optimal[-virtuel] nom.MIF brin_dx dx_recept_ini cadre_dx'  
      write(*,*) 'Argument -h/--help pour obtenir de l aide'              
      
      stop       
      
99    continue
          
      end subroutine  
      
      
!--------------------------------------------------------------------
      subroutine help_me
      
      implicit none

      write(*,*) '#######################################################################'
      write(*,*) '# PREVALP - Generateur de maillage optimal'
      write(*,*) '# Atmo Rhone-Alpes 2009'
      write(*,*) '# version 1.0'
      write(*,*) '# contact: echaxel@atmo-rhonealpes.org'								       
      write(*,*) '#######################################################################'
      write(*,*) ''
      write(*,*) 'BUT     : A partir d un fichier au format MIF, genere un maillage optimal' 
      write(*,*) '          pour un modele de rue de type SIRANE'
      write(*,*) ''      
      write(*,*) 'PRINCIPE: a partir d un grille cartesienne en X/Y a un pas fin determine' 
      write(*,*) '          par l utilisateur dx_recept_ini, '
      write(*,*) '          on elimine les points a distance des brins de circulation' 
      write(*,*) '          pour ne conserver que les points a proximite des axes.'
      write(*,*) '          La loi de progression des distances inter-recepteurs est geometrique de raison 3'            
      write(*,*) ''      
      write(*,*) 'SYNTAXE : mailleur.e -optimal[-virtuel] -i fichier.MIF -brin_dx -recept_dx -cadre_dx'  
      write(*,*) ''	   
      write(*,*) 'ENTREE  : nom.MIF       = fichier de brins au format MIF'
      write(*,*) '          brin_dx       = longueur de discretisation des brins (m)'
      write(*,*) '          dx_recept_ini = pas de la grille de depart (m)'
      write(*,*) '          cadre_dx      = largeur du cadre autour du domaine (m)'      
      write(*,*) ''
      write(*,*) 'SORTIES : 2 fichiers'
      write(*,*) '          - recept_pts.txt : un fichier X Y Z1 Z2 de maillage optimal '
      write(*,*) '          - brin_pts.txt   : un fichier X Y Z1 Z2 avec la discretisation '
      write(*,*) '                             des axes en points '  
      write(*,*) 'OPTIONS : -virtuel : utilise un seul intervalle de recepteur qui est dx_recept_ini'      
                                 
      end subroutine help_me
!--------------------------------------------------------------------
