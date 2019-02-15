*$ CREATE MAGFLD.FOR
*COPY MAGFLD
*
*===magfld=============================================================*
*
      SUBROUTINE MAGFLD ( X, Y, Z, BTX, BTY, BTZ, B, NREG, IDISC )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1988-2009      by Alberto Fasso` & Alfredo Ferrari *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     Created  in 1988    by     Alberto Fasso`, CERN - TIS            *
*                                                                      *
*     Last change on 15-oct-09     by    Alfredo Ferrari               *
*                                                                      *
*     Input variables:                                                 *
*            x,y,z = current position                                  *
*            nreg  = current region                                    *
*     Output variables:                                                *
*            btx,bty,btz = cosines of the magn. field vector           *
*            B = magnetic field intensity (Tesla)                      *
*            idisc = set to 1 if the particle has to be discarded      *
*                                                                      *
*----------------------------------------------------------------------*
*
      LOGICAL LFIRST
      PARAMETER (NPOINT = 31)
      DIMENSION ZZZ(NPOINT), BBB(NPOINT)

      SAVE LFIRST, ZZZ, BBB
      DATA LFIRST / .TRUE. /
*  +-------------------------------------------------------------------*
*  |  First call initializations:
      IF ( LFIRST ) THEN
*  |  *** User initialization ***
         WRITE(LUNOUT,*)
         WRITE(LUNOUT,'(A,132A)') ("*",I=1,132)
         WRITE(LUNOUT,*)
         WRITE(LUNOUT,*)
         WRITE(LUNOUT,'(A,A)') "             Solenoidal magnetic field"  
         WRITE(LUNOUT,*)
         WRITE(LUNOUT,*)
         WRITE(LUNOUT,'(A,132A)') ("*",I=1,132)
         LFIRST = .FALSE.
      END IF
*  |
*  +-------------------------------------------------------------------*

      DATA ZZZ / -100.D0, -80.D0, -60.D0, -40.D0, -20.D0, 0.D0, 20.D0,  
     &     40.D0,  60.D0,  80.D0, 100.D0, 120.D0, 140.D0, 160.D0,
     &    180.D0, 200.D0, 220.D0, 240.D0, 260.D0, 280.D0, 300.D0, 
     $    320.D0, 340.D0, 360.D0, 380.D0, 400.D0, 420.D0, 440.D0,
     &    460.D0, 480.D0, 500.D0 /

      DATA BBB / -0.1D0, -0.15D0, -0.25D0, -0.4D0, -0.7D0, -1.1D0,
     &   -1.4D0, -1.65D0, -1.85D0, -1.95D0, -2.05D0, -2.1D0, -2.15D0,
     &   -2.2D0, -2.2D0, -2.2D0, -2.2D0, -2.2D0, -2.2D0, -2.2D0,
     &   -2.2D0, -2.25D0, -2.0D0, -1.5D0, -1.1D0, -0.6D0, -0.3D0, 
     &   -0.25D0, -0.15D0, -0.12D0, -0.1D0 /
      
      IDISC = 0
*   field along +z:
      BTX   = ZERZER
      BTY   = ZERZER
      BTZ   = ONEONE

      IF ( (X*X + Y*Y) .GT. 92.8D0**2 ) THEN
         B = ZERZER
         RETURN
      ELSE IF ( Z .LT. -100.D0 .OR. Z .GT. 500.D0) THEN
         B = 0.01D0
      ELSE
         B = YINTER (Z, ZZZ, BBB, NPOINT)
      END IF

      RETURN
*=== End of subroutine magfld =========================================*
      END

