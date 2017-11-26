!***********************************************************************      	
      INTEGER FUNCTION JULIAN (YEAR, MNTH, MDAY)

!***********************************************************************
!  FUNCTION:  returns the Julian day (1...365,366) corresponding to
!      the date MNTH-MDAY-YEAR.
!      NOTE:  This is NOT the Julian DATE -- only the
!      day-number.  To get the Julian date:
!
!      JDATE = 1000 * YEAR  +  JULIAN ( YEAR , MNTH , MDAY )
!
!  REVISION HISTORY:
!
!    5/88   Modified for ROMNET
!    8/90   Modified for ROM 2.2:  improved comments; improved algorithm
!           using IF-THEN ... ELSE IF ... construction.
!
!  ARGUMENT LIST DESCRIPTION:
!
!    Input arguments:
!
!      YEAR     Calendar year
!      MNTH     Month of year  1, 12
!      MDAY     Day of month   1, 31
!
!     Output arguments:  none
!
!  RETURN VALUE:
!
!      JULIAN   The Julian DAY of the input arguments combined
!
!  LOCAL VARIABLE DESCRIPTION:
!
!
!***********************************************************************

      IMPLICIT NONE

      INTEGER   YEAR, MNTH, MDAY, M, N, L

      M = MOD ((MNTH + 9), 12)
      N = (M * 153 + 2) / 5 + MDAY + 58

      IF (MOD (YEAR, 4) .NE. 0) THEN
          L = 365
      ELSE IF (MOD (YEAR, 100) .NE. 0) THEN
          L = 366
          N = 1 + N
      ELSE IF (MOD (YEAR, 400) .NE. 0)  THEN
          L = 365
      ELSE 
          L = 366
          N = 1 + N
      END IF

      JULIAN = 1 + MOD (N, L)

      RETURN
      END
