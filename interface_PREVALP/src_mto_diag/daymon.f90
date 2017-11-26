
!.........................................................................
! Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/daymon.f,v 1.2 2000/11/28 21:22:37 smith_w Exp $"
! EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
! See file "LGPL.txt" for conditions of use.
!.........................................................................

      SUBROUTINE DAYMON( JDATE, MNTH, MDAY )
 
!***********************************************************************
!  function body starts at line  44
!
!  FUNCTION:
!
!    This routine determines the month and day of the month 
!    for the Julian date YYYYDDD that is input
!
!  REVISION HISTORY:
!
!    3/95   Adapted for Models-3/EDSS from ROM GREG.FOR by CJC
!
!***********************************************************************
 

      IMPLICIT NONE

!...........   ARGUMENTS and their descriptions:
        
        INTEGER	JDATE	!  Julian date, format YYYYDDD = 1000*Year + Day
        INTEGER MNTH    !  month (1...12)
        INTEGER MDAY    !  day-of-month (1...28,29,30,31)


!...........   SCRATCH LOCAL VARIABLES:
        
        INTEGER YEAR, DAY, L, J


!***********************************************************************
!   begin body of subroutine  DAYMON

 
      YEAR = JDATE / 1000
      DAY  = MOD( JDATE, 1000 )
      
      IF      ( MOD( YEAR, 400 ) .EQ. 0 ) THEN
          L = 366
      ELSE IF ( MOD( YEAR, 100 ) .EQ. 0 ) THEN
          L = 365
      ELSE IF ( MOD( YEAR, 4 )   .EQ. 0 ) THEN
          L = 366
      ELSE 
          L = 365
      END IF
 
      J = MOD( DAY + 305, L )
      J = MOD( J, 153 ) / 61 + ( J / 153 ) * 2 + J
 
      MNTH = MOD( J / 31 + 2, 12 ) + 1
      MDAY = MOD( J, 31 ) + 1
 
      RETURN
      END

