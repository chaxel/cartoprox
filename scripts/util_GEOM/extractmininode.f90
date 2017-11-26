MODULE PARAM
integer, parameter :: ifr = 11
integer, parameter :: ifw = 21

integer :: nrow,ncol

integer :: i
character(len=132) :: fni
character(len=132) :: fout
integer,dimension(:),allocatable :: id,n1,n2,bid,n1n2
real   ,dimension(:),allocatable :: H,L
integer :: nline
END MODULE PARAM
!
!.......................................................

PROGRAM extractmininode

USE PARAM
!IMPLICIT NONE

integer,dimension(:),allocatable :: noeuds
integer,dimension(:),allocatable :: irgnt
Integer ::  NUNI
!Integer, Parameter :: kdp = selected_real_kind(15)
integer ::  nearless

!lecture des inputs

print*,'fichier rue ?'
read*,fni
!fni="rue_CA23.txt"
print*,'fni=',trim(fni)

open(unit=ifr,file=fni,action="read",form="formatted")
rewind(ifr)
read(ifr,*)nline
!print*,'nombre de lignes dans le fichier ', trim(fni)
!print*,'nline=',nline

allocate(id(nline))
allocate(n1(nline))
allocate(n2(nline))
allocate(H(nline))
allocate(L(nline))
allocate(bid(nline))
allocate(n1n2(nline+nline))
allocate(irgnt(nline*2))

  do i=1,nline
    read(ifr,*)id(i),n1(i),n2(i),H(i),L(i)
!   print*,id(i),n1(i),n2(i),H(i),L(i)
  enddo
 print*,'fichier lu !' 
 close(ifw)

!....recupère tous les noeuds utilisés 
print*,'tri et suppression des noeuds doublons'
   n1n2(1:nline)=n1(1:nline)
   n1n2(nline+1:nline+nline)=n2(1:nline)
   call I_unirnk (n1n2, irgnt, NUNI,nline*2)
!
!...output
print*,'nombre de noeuds :  ',NUNI
allocate(noeuds(nuni))
do i=1,nuni
 noeuds(i)=n1n2(irgnt(i))
enddo
!print*,' noeuds' , noeuds
!.........
print*,'fichier output ?'
 read*,fout
!  fout="num_noeuds_CA23.txt"
  open(ifw,file=trim(fout))
  write(ifw,'(t1,i5)')NUNI
  do i=1,nuni
   write(ifw,*)noeuds(i)
  enddo !i
  print*,'fichier num de noeuds ecrit : ',trim(fout)
  close(ifw)


!..........
deallocate(id)
deallocate(n1)
deallocate(n2)
deallocate(H)
deallocate(L)
deallocate(bid)
deallocate(n1n2)
deallocate(irgnt)
print*,'fin du programme extractmininode.f90' 
    
END PROGRAM extractmininode 
!...........
 
Subroutine I_unirnk (XVALT, IRNGT, NUNI,dimin)
! __________________________________________________________
!   UNIRNK = Merge-sort ranking of an array, with removal of
!   duplicate entries.
!   The routine is similar to pure merge-sort ranking, but on
!   the last pass, it discards indices that correspond to
!   duplicate entries.
!   For performance reasons, the first 2 passes are taken
!   out of the standard loop, and use dedicated coding.
! __________________________________________________________
     Integer , Intent (In) :: dimin
     Integer, Dimension (dimin) :: XVALT
!    Integer, Dimension (dimin), Intent (in) :: XVALT
     Integer, Dimension (dimin) :: IRNGT
      Integer, Intent (Out) :: NUNI
! __________________________________________________________
      Integer, Dimension (SIZE(IRNGT)) :: JWRKT
      Integer :: LMTNA, LMTNC, IRNG, IRNG1, IRNG2
      Integer :: NVAL, IIND, IWRKD, IWRK, IWRKF, JINDA, IINDA, IINDB
      Integer :: XTST, XVALA, XVALB
!
!
!     NVAL = Min (SIZE(XVALT), SIZE(IRNGT))
      NVAL=SIZE(XVALT)
      NUNI = NVAL
!
!     print*,'nval=',nval
!     print*,'input xvalt=',XVALT
      Select Case (NVAL)
      Case (:0)
         Return
      Case (1)
         IRNGT (1) = 1
         Return
      Case Default
         Continue
      End Select
!
!  Fill-in the index array, creating ordered couples
!
      Do IIND = 2, NVAL, 2
         If (XVALT(IIND-1) < XVALT(IIND)) Then
            IRNGT (IIND-1) = IIND - 1
            IRNGT (IIND) = IIND
         Else
            IRNGT (IIND-1) = IIND
            IRNGT (IIND) = IIND - 1
         End If
      End Do
      If (Modulo(NVAL, 2) /= 0) Then
         IRNGT (NVAL) = NVAL
      End If
!
!  We will now have ordered subsets A - B - A - B - ...
!  and merge A and B couples into     C   -   C   - ...
!
      LMTNA = 2
      LMTNC = 4
!
!  First iteration. The length of the ordered subsets goes from 2 to 4
!
      Do
         If (NVAL <= 4) Exit
!
!   Loop on merges of A and B into C
!
         Do IWRKD = 0, NVAL - 1, 4
            If ((IWRKD+4) > NVAL) Then
               If ((IWRKD+2) >= NVAL) Exit
!
!   1 2 3
!
               If (XVALT(IRNGT(IWRKD+2)) <= XVALT(IRNGT(IWRKD+3))) Exit
!
!   1 3 2
!
               If (XVALT(IRNGT(IWRKD+1)) <= XVALT(IRNGT(IWRKD+3))) Then
                  IRNG2 = IRNGT (IWRKD+2)
                  IRNGT (IWRKD+2) = IRNGT (IWRKD+3)
                  IRNGT (IWRKD+3) = IRNG2
!
!   3 1 2
!
               Else
                  IRNG1 = IRNGT (IWRKD+1)
                  IRNGT (IWRKD+1) = IRNGT (IWRKD+3)
                  IRNGT (IWRKD+3) = IRNGT (IWRKD+2)
                  IRNGT (IWRKD+2) = IRNG1
               End If
               Exit
            End If
!
!   1 2 3 4
!
            If (XVALT(IRNGT(IWRKD+2)) <= XVALT(IRNGT(IWRKD+3))) Cycle
!
!   1 3 x x
!
            If (XVALT(IRNGT(IWRKD+1)) <= XVALT(IRNGT(IWRKD+3))) Then
               IRNG2 = IRNGT (IWRKD+2)
               IRNGT (IWRKD+2) = IRNGT (IWRKD+3)
               If (XVALT(IRNG2) <= XVALT(IRNGT(IWRKD+4))) Then
!   1 3 2 4
                  IRNGT (IWRKD+3) = IRNG2
               Else
!   1 3 4 2
                  IRNGT (IWRKD+3) = IRNGT (IWRKD+4)
                  IRNGT (IWRKD+4) = IRNG2
               End If
!
!   3 x x x
!
            Else
               IRNG1 = IRNGT (IWRKD+1)
               IRNG2 = IRNGT (IWRKD+2)
               IRNGT (IWRKD+1) = IRNGT (IWRKD+3)
               If (XVALT(IRNG1) <= XVALT(IRNGT(IWRKD+4))) Then
                  IRNGT (IWRKD+2) = IRNG1
                  If (XVALT(IRNG2) <= XVALT(IRNGT(IWRKD+4))) Then
!   3 1 2 4
                     IRNGT (IWRKD+3) = IRNG2
                  Else
!   3 1 4 2
                     IRNGT (IWRKD+3) = IRNGT (IWRKD+4)
                     IRNGT (IWRKD+4) = IRNG2
                  End If
               Else
!   3 4 1 2
                  IRNGT (IWRKD+2) = IRNGT (IWRKD+4)
                  IRNGT (IWRKD+3) = IRNG1
                  IRNGT (IWRKD+4) = IRNG2
               End If
            End If
         End Do
!
!  The Cs become As and Bs
!
         LMTNA = 4
         Exit
      End Do
!
!  Iteration loop. Each time, the length of the ordered subsets
!  is doubled.
!
      Do
         If (2*LMTNA >= NVAL) Exit
         IWRKF = 0
         LMTNC = 2 * LMTNC
!
!   Loop on merges of A and B into C
!
         Do
            IWRK = IWRKF
            IWRKD = IWRKF + 1
            JINDA = IWRKF + LMTNA
            IWRKF = IWRKF + LMTNC
            If (IWRKF >= NVAL) Then
               If (JINDA >= NVAL) Exit
               IWRKF = NVAL
            End If
            IINDA = 1
            IINDB = JINDA + 1
!
!  One steps in the C subset, that we create in the final rank array
!
!  Make a copy of the rank array for the iteration
!
            JWRKT (1:LMTNA) = IRNGT (IWRKD:JINDA)
            XVALA = XVALT (JWRKT(IINDA))
            XVALB = XVALT (IRNGT(IINDB))
!
            Do
               IWRK = IWRK + 1
!
!  We still have unprocessed values in both A and B
!
               If (XVALA > XVALB) Then
                  IRNGT (IWRK) = IRNGT (IINDB)
                  IINDB = IINDB + 1
                  If (IINDB > IWRKF) Then
!  Only A still with unprocessed values
                     IRNGT (IWRK+1:IWRKF) = JWRKT (IINDA:LMTNA)
                     Exit
                  End If
                  XVALB = XVALT (IRNGT(IINDB))
               Else
                  IRNGT (IWRK) = JWRKT (IINDA)
                  IINDA = IINDA + 1
                  If (IINDA > LMTNA) Exit! Only B still with unprocessed values
                  XVALA = XVALT (JWRKT(IINDA))
               End If
!
            End Do
         End Do
!
!  The Cs become As and Bs
!
         LMTNA = 2 * LMTNA
      End Do
!
!   Last merge of A and B into C, with removal of duplicates.
!
      IINDA = 1
      IINDB = LMTNA + 1
      NUNI = 0
!
!  One steps in the C subset, that we create in the final rank array
!
      JWRKT (1:LMTNA) = IRNGT (1:LMTNA)
      XTST = nearless (Min(XVALT(JWRKT(1)), XVALT(IRNGT(IINDB))))
      Do IWRK = 1, NVAL
!
!  We still have unprocessed values in both A and B
!
         If (IINDA <= LMTNA) Then
            If (IINDB <= NVAL) Then
               If (XVALT(JWRKT(IINDA)) > XVALT(IRNGT(IINDB))) Then
                  IRNG = IRNGT (IINDB)
                  IINDB = IINDB + 1
               Else
                  IRNG = JWRKT (IINDA)
                  IINDA = IINDA + 1
               End If
            Else
!
!  Only A still with unprocessed values
!
               IRNG = JWRKT (IINDA)
               IINDA = IINDA + 1
            End If
         Else
!
!  Only B still with unprocessed values
!
            IRNG = IRNGT (IWRK)
         End If
         If (XVALT(IRNG) > XTST) Then
            XTST = XVALT (IRNG)
            NUNI = NUNI + 1
            IRNGT (NUNI) = IRNG
         End If
!
      End Do
!     print*,'output indices =',irngt(1:nuni)
!
      Return
!
End Subroutine I_unirnk
!...........
Function nearless (XVAL) result (I_nl)
!  Nearest value less than given value
! __________________________________________________________
      Integer, Intent (In) :: XVAL
      Integer :: I_nl
! __________________________________________________________
      I_nl = XVAL - 1
      return
!
End Function nearless

