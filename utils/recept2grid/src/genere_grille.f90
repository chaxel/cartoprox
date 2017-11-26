 program genere_grille

 implicit none

 character(len=256) :: argstr
 character(len=256) :: fic_grille 
 integer :: iarg
 logical :: ixmin,iymin,ixmax,iymax,idx,idy,idebug,ific
 real    :: xmin,ymin,xmax,ymax,dx,dy
 integer :: nx,ny
 integer :: i,j
 real    :: xcentre, ycentre

 fic_grille='grille.txt'

! lit l'argument
 iarg = 0    
 argstr='null'	
 ific=.false.  
 do while ( trim(adjustl(argstr)) .ne. '' )
 iarg = iarg + 1
 call getarg(iarg,argstr)
 if ( trim(adjustl(argstr)).eq. '-o' ) then
   call getarg(iarg+1,fic_grille)
   ific = .true.
 else if ( trim(adjustl(argstr)).eq. '-xmin' ) then
   call getarg(iarg+1,argstr)
   read(argstr,*) xmin
   ixmin = .true.
 else if ( trim(adjustl(argstr)).eq. '-ymin' ) then
   call getarg(iarg+1,argstr)
   read(argstr,*) ymin
   iymin = .true.
 else if ( trim(adjustl(argstr)).eq. '-nx' ) then
   call getarg(iarg+1,argstr)
   read(argstr,*) nx
   ixmax = .true.
 else if ( trim(adjustl(argstr)).eq. '-ny' ) then
   call getarg(iarg+1,argstr)
   read(argstr,*) ny
   iymax = .true.
 else if ( trim(adjustl(argstr)).eq. '-dx' ) then
   call getarg(iarg+1,argstr)
   read(argstr,*) dx
   idx = .true.
 else if ( trim(adjustl(argstr)).eq. '-dy' ) then
   call getarg(iarg+1,argstr)
   read(argstr,*) dy
   idy = .true.
 else if ( trim(adjustl(argstr)).eq. '-debug'.or.&
           trim(adjustl(argstr)).eq. '-d'           ) then
   idebug=.true.
 endif
 
 end do

! creation de la grille
! nx = (xmax-xmin)/dx
! ny = (ymax-ymin)/dy 
 
! open(unit=10,file='dims',status='unknown') 
! write(10,*)  nx, ny 
! close(10)

!ecriture dans fichier
 if (ific)open(unit=10,file=fic_grille,status='unknown') 
  
 do j = 1, ny
   do i = 1, nx
      xcentre = xmin + dx * ( i - .5)
      ycentre = ymin + dy * ( j - .5)
      if (ific) then 
        write(10,*) xcentre, ycentre
      else
        write(*,*) xcentre, ycentre      
      end if
   end do
 end do  
 if (ific)close(10)


 if (ific)write(*,*) trim(fic_grille)//' OK'
 
 
end program
