! extraction des recepteurs correspondant au mini domaine
! input : nom du minidomaine
! output : fichier recepteurs format siraneV1 inclus dans ce minidomaine
!
! FT 2009/09/02
!.....................................................
program extractminirecept
implicit none
integer, parameter :: rmax=500000

integer, parameter :: ifr=2
integer, parameter :: ifw=4

character(len=132) :: fin
character(len=132) :: fout

integer :: rceptmax, nrecept
integer, dimension(:),allocatable :: irecpt, jrecpt

character*16, dimension(:),allocatable :: namerecpt
character*4, dimension(:),allocatable :: minidom
character*4 :: minidomextr

integer :: i
character*50 rbidon

print*,'fichier recepteurs entier ?'
read*,fin
print*,'nombre de recepteurs dans fichier entier ?'
read*,rceptmax

print*,'fichier input : ',trim(fin), rceptmax
print*,'nom du minidomaine a extraire ?'
read*,minidomextr
print*,trim(minidomextr)
!........
allocate(irecpt(rceptmax))
allocate(jrecpt(rceptmax))
allocate(namerecpt(rceptmax))
allocate(minidom(rceptmax))
!
fout='recept'//trim(minidomextr)//'.txt'
print*,'fichier out ?'
read*,fout
print*,trim(fout)
open(ifw,file=trim(fout))
!
print*,'nb : ',rceptmax,i
open(ifr,file=trim(fin))
rewind(ifr)
  read(ifr,*) rbidon
! read(ifr,*) namerecpt(:),irecpt(:),jrecpt(:) ,minidom(:)
   do i=1,rceptmax
  read(ifr,*,end=5000) namerecpt(i),irecpt(i),jrecpt(i) ,minidom(i)
   enddo !i
5000 continue
   do i=1,rceptmax
    if(trim(minidom(i)).eq.trim(minidomextr)) then
         write(ifw,*) namerecpt(i),irecpt(i),jrecpt(i) !,minidom(i)
    endif
   enddo !i

print*,'fichier resultat : ', trim(fout)
500 format(t1,a12,i6,1x,i7,i4)
    
!
deallocate(irecpt)
deallocate(jrecpt)

close(ifr)
close(ifw)


END
