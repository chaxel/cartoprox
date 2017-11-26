      subroutine read_args

      use params

      implicit none
      
!     arguments
      
      character(len=256) :: argstr
      integer :: iarg
      logical :: ihelp 
      logical :: idebug  
            
      ! logical local
      
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
                 
      maillage_optimal=.false.
      maillage_virtuel=.false.
      
      ibrin_dx=.false.
      irecept_dx=.false.
      icadre_dx=.false.
      
      iout=.false.
      imif=.false.
      igrille=.false.
      
      ihelp=.false.
      
      igeoid=.false.
      iutm_zone=.false.
      iutm=.false.
      il2=.false.
      igeo=.false. 
           

      ! lit l'argument
      iarg = 0    
      argstr='null'	  
      do while ( trim(adjustl(argstr)) .ne. '' )
	iarg = iarg + 1	
        call getarg(iarg,argstr)
        !write(*,*) 'Lit argument', iarg, trim(adjustl(argstr))			
        if ( trim(adjustl(argstr)).eq. '-i' )  then
	  call getarg(iarg+1,fichier_mif)
	  imif = .true.
        else if ( trim(adjustl(argstr)).eq. '-o' ) then
	  call getarg(iarg+1,fichier_recept)
          iout=.true.
        else if (( trim(adjustl(argstr)).eq. '-g' ).or.( trim(adjustl(argstr)).eq. '-grille' )) then
	  call getarg(iarg+1,fichier_grille)
          igrille=.true.	  			
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
! DONNEE GEOGRAPHIQUES
        else if ( trim(adjustl(argstr)).eq. '-l2' )  then	  
	  il2 = .true.	  
        else if ( trim(adjustl(argstr)).eq. '-geo' )  then	  
	  igeo = .true.	  
        else if ( trim(adjustl(argstr)).eq. '-geoid' )  then	  
	  call getarg(iarg+1,argstr)
	  read(argstr,*) 
	  igeoid = .true.
        else if ( trim(adjustl(argstr)).eq. '-utm' )  then	  
	  call getarg(iarg+1,argstr)
	  read(argstr,*) zoneutm_i
	  iutm = .true.	
	  iutm_zone = .true.		  	  
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
      
      if (.not. icadre_dx) then
        cadre_dx = cadre_dx_defaut
      end if
      
      if (.not. iout) then
        fichier_recept = 'recept_pts_all.txt'
      end if
      
      if (.not. igrille) then
        fichier_grille = 'grille.txt'
      end if      
      
      ! active grille USER si infos sur centre -> passe auto_grille
      if (ixc.and.iyc.and.(idx.or.idy)) then
        igrilleuser=.true. 
        write(*,*) '-> Grille AUTO active'
      end if	
      
      if (ibrin_dx.and.irecept_dx.and.imif) then ! toutes les entrees sont OK
        write(*,'("Largeur de discretisation des brins (m)=",F5.0)') brin_dx
        write(*,'("Pas de la grille de depart (m)=",F5.0)') dx_recept_ini  
        write(*,'("Largeur du cadre (m)=",F6.0)') cadre_dx
        go to 99   
      else if (igrille) then     
        write(*,'("Definition de la grille seulement")')
        go to 99   	
      end if
      
      write(*,*) 'Syntaxe -optimal[-virtuel] -i fichier.MIF -brin_dx -recept_dx -cadre_dx [-utm 31 -xc -yc -dx]'  
      write(*,*) 'Argument -h/--help pour obtenir de l aide'              
      
      stop       
      
99    continue

!     COORDONNEES GEOGRAPHIQUES
      ! par defaut Lambert 2
      if ((.not.iutm).and.(.not.igeo).and.(.not.il2)) il2=.true.
      if (.not.iutm_zone) zoneutm_i = 31
      if (.not.igeoid) geoidstr_i = 'WGS84'
      geoid_i = 0
      if ( trim(geoidstr_i) == 'WGS84' ) geoid_i = 1
      if ( trim(geoidstr_i) == 'ED50'  ) geoid_i = 2	  
      if ( trim(geoidstr_i) == 'NTF'   ) geoid_i = 3
      if ( trim(geoidstr_i) == 'GRS80' ) geoid_i = 4
      if (geoid_i.eq.0) then
        write(*,*) '*** erreur: geoid inconnue: '//trim(geoidstr_i)
	stop
      end if

      geoidstr_o = 'NTF'      
      if ( trim(geoidstr_o) == 'WGS84' ) geoid_o = 1
      if ( trim(geoidstr_o) == 'ED50'  ) geoid_o = 2	  
      if ( trim(geoidstr_o) == 'NTF'   ) geoid_o = 3
      if ( trim(geoidstr_o) == 'GRS80' ) geoid_o = 4
      if (geoid_o.eq.0) then
        write(*,*) '*** erreur: geoid inconnue dans params.f90: '//trim(geoidstr_o)
	stop
      end if
      
      if ( igeo ) projection_i = 0
      if ( il2  ) projection_i = 1
      if ( iutm ) projection_i = 2 
      
      ! Regles de transformation (version 1.03)
      ! 0 geo -> WGS84 (1)
      ! 1 UTM -> WGS84 (1)
      ! 2 Lambert -> NTF (3)
      if ( (projection_i .eq. 0).or.(projection_i .eq. 2) ) geoid_i = 1
      if (  projection_i .eq. 1                           ) geoid_i = 3      
      if ( (projection_o .eq. 0).or.(projection_o .eq. 2) ) geoid_o = 1
      if (  projection_o .eq. 1                           ) geoid_o = 3

      ! associe la geoid au code
      if ( geoid_i .eq. 1 ) geoidstr_i = 'WGS84'
      if ( geoid_i .eq. 2 ) geoidstr_i = 'ED50'
      if ( geoid_i .eq. 3 ) geoidstr_i = 'NTF'
      if ( geoid_i .eq. 4 ) geoidstr_i = 'GRS80'
      if ( geoid_o .eq. 1 ) geoidstr_o = 'WGS84'
      if ( geoid_o .eq. 2 ) geoidstr_o = 'ED50'
      if ( geoid_o .eq. 3 ) geoidstr_o = 'NTF'
      if ( geoid_o .eq. 4 ) geoidstr_o = 'GRS80'
          
      end subroutine  
      
      
!--------------------------------------------------------------------
      subroutine help_me
      
      use params
      
      implicit none

      write(*,*) '#######################################################################'
      write(*,*) '# PREVALP - Generateur de maillage optimal'
      write(*,*) '# Atmo Rhone-Alpes 2009'
      write(*,*) '# version '//trim(versionstr)
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
      write(*,*) 'SYNTAXE : mailleur.e -optimal[-virtuel] -i fichier.MIF -brin_dx -recept_dx -cadre_dx -g grille.txt'  
      write(*,*) ''	   
      write(*,*) 'ENTREE  : -i fichier.MIF        = fichier de brins au format MIF'
      write(*,*) '          -brin_dxbrin_dx       = longueur de discretisation des brins (m)'
      write(*,*) '          -recept_dx recept_dx  = pas de la grille de depart (m)'
      write(*,*) '          -cadre_dx cadre_dx    = largeur du cadre autour du domaine (m)'
      write(*,*) '          -g fichier_grille.txt = informations sur la grille du maillage'            
      write(*,*) ''
      write(*,*) 'SORTIES : 2 fichiers'
      write(*,*) '          - recept_pts.txt : un fichier X Y Z1 Z2 de maillage optimal '
      write(*,*) '          - brin_pts.txt   : un fichier X Y Z1 Z2 avec la discretisation '
      write(*,*) '                             des axes en points '  
      write(*,*) 'OPTIONS : -virtuel : utilise un seul intervalle de recepteur qui est dx_recept_ini'      
                                 
      end subroutine help_me
!--------------------------------------------------------------------
