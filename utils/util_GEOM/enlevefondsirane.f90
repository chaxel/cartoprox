! 
! soustrait à un fichier horaire la valeur du fond 
!
! input  : fichier horaire annuel + fond horaire annuel
! output : fichier horaire annuel sans fond
!
! attention dans le fichier de fond il faut (avant ce prog) : 
!       - remplacer / par espace 
!       - supprimer :00 de l'heure
!
! le calcul se fait là où sont les input -> faire ln du fond et de evol
! dans repertoire de travail !
!
! FT 2009/07/01
!.......................................................
MODULE PARAM
integer, parameter :: nrowmax=2000
integer, parameter :: ifr=11
integer, parameter :: ifrf=12
integer, parameter :: ifrc=13
integer, parameter :: ifw=21
character(len=132) :: fni
character(len=132) :: fnif
character(len=132) :: fnic
character(len=132) :: fout
character(len=4) :: caaaa,crue
real, dimension(:,:),allocatable :: val, fondbid
real, dimension(:,:),allocatable :: valout
real, dimension(:),allocatable :: fNOx,fNO,fNO2,fO3
real, dimension(:),allocatable :: NOx,NO,NO2,O3
real, dimension(:),allocatable :: x,y,inum
real, dimension(:,:),allocatable :: outNOx,outNO,outNO2,outO3

integer :: nbrues,nbech
integer :: i,j,nr,nrg
integer :: aaaa,iask
character(len=32) :: cbidon
!
!..............
!character (len = *), parameter :: FILE_NAME = "siranesansfond_2008.nc"
character (len = 60) :: FILE_NAME 
integer, parameter :: NDIMS = 2
integer :: Nrue,  Nech
integer, parameter :: Nesp = 1
!integer, parameter :: Nrue = nrowmax,  Nech = 8784, Nesp = 1
integer :: ncid, dimids(NDIMS),dimidsxy(1)
integer :: varidno,varidno2,varido3,varidnox
integer :: nrue_dimid, nech_dimid, nesp_dimid
integer  :: varidx, varidy,varidinum
!..............
END MODULE PARAM
!.............................................................
PROGRAM elevefondsirane 
 use netcdf
 USE PARAM
 implicit none
!
!...lecture des input
!
   print*,'annee ? AAAA'
   read*,aaaa
!  aaaa=2008
   print*,'nombre d"echeances 8784/8760 ?'
   if (aaaa.eq.2008) then
                       nbech=8784
                       iask=0
                     else
                       if (aaaa.eq.2008) then
                        nbech=8760
                        iask=0
                       else
                        iask=1
                      endif
   endif
   write(caaaa,'(i4)')aaaa
   if (iask.eq.1)  read*,nbech
!
      print*,'nombre de rues ?'
      read*,nbrues
    ! nbrues=206 !valeur à lire dans le repertoire evo_rues fichier Id_rue.txt
     print*,"nbrues = ",nbrues, "nbech= ",nbech
!   
     print*,'nom du fichier netcdf ?'
     read*,FILE_NAME
!................
!...allocations
!
allocate(fondbid(4,nbech)) ! pour les 4 1eres colonnes (non utilisees)
allocate(fNOx(nbech))
allocate(fNO(nbech))
allocate(fNO2(nbech))
allocate(fO3(nbech))
allocate(valout(3,nbech))
allocate(NOx(nbech))
allocate(NO(nbech))
allocate(NO2(nbech))
allocate(O3(nbech))
allocate(outNOx(nbrues,nbech))
allocate(outNO(nbrues,nbech))
allocate(outNO2(nbrues,nbech))
allocate(outO3(nbrues,nbech))
allocate(x(nbrues))
allocate(y(nbrues))
allocate(inum(nbrues))
!..
!...lecture du fichier de fond
      print*,'fichier du fond (formatte sans slash ni :)'
      read*,fnif
!     fnif="fond_2008.dat.4f90" 
      open(unit=ifrf,file=trim(fnif),status="old",action="read",form='formatted')
      rewind(ifrf)
      do i=1,nbech
       read(ifrf,*)(fondbid(j,i),j=1,4),fNO(i),fNO2(i),fO3(i)
       fNOx(i)=46.*fNO(i)/30.+fNO2(i)
      enddo !i
       i=10
      print*,'fond ech 10 : no no2 o3 ',  fNO(i),fNO2(i),fO3(i),fNOx(i)
      close(ifrf)
!
!..............
!...lecture coordonnées des rues
      print*,'fichier de coordonnees ? cree avec recupcoordrue.sh'
      read*,fnic
!     fnic="coordrues.dat"   ! cree avec recupcoordrue.sh
      open(unit=ifrc,file=trim(fnic),status="old",action="read",form='formatted')
      rewind(ifrc)
      read(ifrc,*)cbidon
      do i=1,nbrues
       read(ifrc,*)inum(i),x(i),y(i)
      enddo !i
       i=10
      print*,'coord ech 10 : inum(i),x(i),y(i) ', inum(i),x(i),y(i)
      close(ifrc)

!..............
!..traite chaque rue
   do nrg=1,nbrues
     nr=nrg-1
      if(nr<10) write(crue,'i1')nr
      if(nr>=10.and.nr<100) write(crue,'i2')nr
      if(nr>=100.and.nr<1000) write(crue,'i3')nr
      if(nr>=1000.and.nr<10000) write(crue,'i4')nr
      if(nr>=10000) stop 'dev programme pour crue !! '
              
      fni="evo."//caaaa//".rue."//trim(crue)//".txt"    
!      print*,'fni=',trim(fni),len(trim(crue))
      open(unit=ifr,file=fni,status="old",action="read",form='formatted')
      rewind(ifr)
        read(ifr,*)cbidon
        do i=1,nbech
          read(ifr,*)(valout(j,i),j=1,3),NOx(i),NO(i),NO2(i),O3(i)
        enddo !i
      close(ifr)
!  i=10
!  print*,'valrue : ',NOx(i),NO(i),NO2(i),O3(i)
!
!...calcul
  
    do i=1,nbech
       outNOx(nrg,i)=NOx(i)-fNOx(i)      
       outNO(nrg,i)=NO(i)-fNO(i)      
       outNO2(nrg,i)=NO2(i)-fNO2(i)      
       outO3(nrg,i)=O3(i)-fO3(i)      
    enddo !i
!  i=10
!  print*,'out : ',outNOx(nrg,i),outNO(nrg,i),outNO2(nrg,i),outO3(nrg,i)
!....
   enddo !nrg 
!
!.........................
!...creation du netcdf output
call check( nf90_create(FILE_NAME, NF90_CLOBBER, ncid) )
call check( nf90_def_dim(ncid, "idrue", nbrues, nrue_dimid))
call check( nf90_def_dim(ncid, "ech", nbech, nech_dimid) )
!call check( nf90_def_dim(ncid, "nesp", Nech, nesp_dimid) )
dimids =  (/ nrue_dimid, nech_dimid /)

dimidsxy =  (/ nrue_dimid/)


call check( nf90_def_var(ncid, "id", NF90_REAL, dimidsxy, varidinum) )
call check( nf90_def_var(ncid, "x", NF90_REAL, dimidsxy, varidx) )
call check( nf90_def_var(ncid, "y", NF90_REAL, dimidsxy, varidy) )
call check( nf90_def_var(ncid, "NOx", NF90_REAL, dimids, varidnox) )
call check( nf90_def_var(ncid, "NO", NF90_REAL, dimids, varidno) )
call check( nf90_def_var(ncid, "NO2", NF90_REAL, dimids, varidno2) )
call check( nf90_def_var(ncid, "O3", NF90_REAL, dimids, varido3) )
call check( nf90_enddef(ncid) )

call check( nf90_put_var(ncid, varidinum, inum) )
call check( nf90_put_var(ncid, varidx, x) )
call check( nf90_put_var(ncid, varidy, y) )
call check( nf90_put_var(ncid, varidnox, outNOx) )
call check( nf90_put_var(ncid, varidno , outNO ) )
call check( nf90_put_var(ncid, varidno2, outNO2) )
call check( nf90_put_var(ncid, varido3, outO3) )
call check( nf90_close(ncid) )


print*,'fichier ecrit : ',trim(FILE_NAME)

!.............
deallocate(fondbid)
deallocate(fNOx)
deallocate(fNO2)
deallocate(fNO)
deallocate(fO3)
deallocate(NOx)
deallocate(NO)
deallocate(NO2)
deallocate(O3)
deallocate(x)
deallocate(y)
deallocate(inum)

contains
  subroutine check(status)
    integer, intent ( in) :: status

    if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if
  end subroutine check

END

