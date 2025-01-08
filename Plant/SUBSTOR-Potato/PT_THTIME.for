C=======================================================================
C  PT_THTIME, Subroutine
C
C  Determines thermal time calculations
C-----------------------------------------------------------------------
C  Revision history
C
C               Written
C  02/28/1993 PWW Header revision and minor changes
C  08/28/2001 CHP Modified for modular format.
C  01/06/2025 HBD Added cardinal temperatures from species file
C=======================================================================

      SUBROUTINE PT_THTIME (
     &      ISTAGE, L0, ST, TMAX, TMIN,                     !Input
     &      VGCT, SGCT,                                     !Input
     &      VTBA, VTO1, VTO2, VTMA, STBA, STO1, STO2, STMA, !Input
     &      DTT, STT)                                       !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
!     NL defined in ModuleDefs.for

      IMPLICIT NONE

      INTEGER  ISTAGE, L0

      REAL DTT, STT, TMIN, TMAX, XTEMP
      REAL ST(NL)

      EXTERNAL CURV
      REAL  CURV  !Function subroutine
      CHARACTER*3 VGCT, SGCT
      REAL VTBA, VTO1, VTO2, VTMA, STBA, STO1, STO2, STMA

!-----------------------------------------------------------------------

      DTT = 0.0
      STT = 0.0

      XTEMP = (TMAX + TMIN)/2.0
      !
      ! Thermal time leaf expansion
      !
      IF (ISTAGE .LE. 4) THEN
!         IF (XTEMP .LE. 2.0) THEN
!            DTT = 0.0
!          ELSE IF (XTEMP .GT.  2.0 .AND. XTEMP .LE. 17.0) THEN
!            DTT = 0.0667*(XTEMP - 2.0)
!          ELSE IF (XTEMP .GT. 17.0 .AND. XTEMP .LE. 24.0) THEN
!            DTT = 1.0
!          ELSE IF (XTEMP .GT. 24.0 .AND. XTEMP .LE. 35.0) THEN
!            DTT = 1.0 -0.0909*(XTEMP - 24.0)
!          ELSE
!            DTT = 0.0
!         END IF
!     HBD (Jan 2025): de-hardwire (species); DONE
         DTT = CURV(VGCT, VTBA, VTO1, VTO2, VTMA, XTEMP)             
      END IF

      !
      ! Soil thermal time (tuber)
      !
      IF (ISTAGE .GE. 6 .OR. ISTAGE .LE. 2) THEN
!        IF (ST(L0) .GE. 2.0 .AND. ST(L0) .LT. 15.0) THEN
!           STT = 0.0769*(ST(L0)-2.0)
!         ELSE IF (ST(L0) .GE. 15.0 .AND. ST(L0) .LT. 23.0) THEN
!           STT = 1.0
!         ELSE IF (ST(L0) .GE. 23.0 .AND. ST(L0) .LT. 33.0) THEN
!           STT = 1.0 - 0.1*(ST(L0) - 23.0)
!         ELSE
!           STT = 0.0
!        ENDIF
!     HBD (Jan 2025): de-hardwire (species); DONE
         STT = CURV(SGCT, STBA, STO1, STO2, STMA, ST(L0))
      END IF

      RETURN
      END SUBROUTINE PT_THTIME

C=======================================================================
C  PT_IPTHTIME, Subroutine
C
C  Input species coefficients for potato soil and air thermal time
C-----------------------------------------------------------------------
C  Revision history
C
C  06/01/2025 HBD adapted PT_IPROOT written by CHP
C-----------------------------------------------------------------------

      SUBROUTINE PT_IPTHTIME(FILEIO,                      !Input
     &    VGCT, SGCT,                                     !Output
     &    VTBA, VTO1, VTO2, VTMA, STBA, STO1, STO2, STMA) !Output

!     ------------------------------------------------------------------

      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR, IGNORE

      INTEGER LUNIO, LUNCRP
      CHARACTER*1, PARAMETER :: BLANK = ' '
      CHARACTER*6, PARAMETER :: ERRKEY = 'ROOTGR'

      CHARACTER*12  FILEC
      CHARACTER*30  FILEIO
      CHARACTER*80  PATHCR
      CHARACTER*92  FILECC
      CHARACTER*180 CHAR
      CHARACTER*4   C4

      INTEGER ERR, ISECT, LNUM, PATHL

      CHARACTER*3 VGCT, SGCT
      REAL VTBA, VTO1, VTO2, VTMA, STBA, STO1, STO2, STMA

!     Read data from FILEIO for use in ROOTGR module
      CALL GETLUN('FILEIO', LUNIO)
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)

      READ(LUNIO,'(6(/),15X,A12,1X,A80)') FILEC, PATHCR
      LNUM = 7
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

      CLOSE (LUNIO)

C-----------------------------------------------------------------------
C     Read Crop Parameters from FILEC
C-----------------------------------------------------------------------
      LNUM   = 0
      PATHL  = INDEX (PATHCR,BLANK)
      IF (PATHL .LE. 1) THEN
         FILECC = FILEC
       ELSE
         FILECC = PATHCR(1:(PATHL-1)) // FILEC
      ENDIF

      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEC,0)

      LNUM = 0
      DO WHILE (ERR == 0)
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        IF (ISECT .EQ. 0) EXIT
        IF (ISECT .EQ. 2) CYCLE
        C4 = CHAR(10:13)
        SELECT CASE(C4)
          CASE('VGCT'); READ(CHAR,'(16X,A3)'  ,IOSTAT=ERR) VGCT
          CASE('VTBA'); READ(CHAR,'(14X,F6.0)',IOSTAT=ERR) VTBA
          CASE('VTO1'); READ(CHAR,'(14X,F6.0)',IOSTAT=ERR) VTO1
          CASE('VTO2'); READ(CHAR,'(14X,F6.0)',IOSTAT=ERR) VTO2
          CASE('VTMA'); READ(CHAR,'(14X,F6.0)',IOSTAT=ERR) VTMA
          CASE('SGCT'); READ(CHAR,'(16X,A3)'  ,IOSTAT=ERR) SGCT
          CASE('STBA'); READ(CHAR,'(14X,F6.0)',IOSTAT=ERR) STBA
          CASE('STO1'); READ(CHAR,'(14X,F6.0)',IOSTAT=ERR) STO1
          CASE('STO2'); READ(CHAR,'(14X,F6.0)',IOSTAT=ERR) STO2
          CASE('STMA'); READ(CHAR,'(14X,F6.0)',IOSTAT=ERR) STMA
        END SELECT
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEC,LNUM)
      END DO
      
      CLOSE (LUNCRP)

C-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE PT_IPTHTIME
C=======================================================================