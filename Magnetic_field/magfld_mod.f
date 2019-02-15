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
*     Last change on 5-oct-10     by    Advanced FLUKA course teacher  *
*
      INCLUDE '(RTDFCM)'
      INCLUDE '(LTCLCM)'
*
      LOGICAL LFIRST
      SAVE LFIRST
      SAVE  GRADIENT
*
      DATA LFIRST / .TRUE. /
*
      IDISC = 0
      IF (LFIRST) THEN
*     gradient in tesla per cm
         GRADIENT = 0.08D+00   
         LFIRST  = .FALSE.
      END IF
*
      IF ( Z .LT. 218.96D0 .AND. Z .GT. 131.46D0 ) THEN
         IF ( (X*X + Y*Y) .LT. 5.D0**2 ) THEN 
            XNEW = X
            YNEW = Y
            ZNEW = Z
c     gradient in tesla per meter
 
            BTX = GRADIENT * YNEW
            BTY = GRADIENT * XNEW

         
            BTZ=ZERZER
            B=SQRT(BTX**2+BTY**2)
            IF (B.GT.1D-12) THEN
               BTX = BTX / B
               BTY = BTY / B           
            ELSE
               B = ZERZER
               BTX = ONEONE
               BTY = ZERZER
            END IF
         ELSE
            BTX = ZERZER
            BTY = ONEONE
            BTZ = ZERZER
            B   = ZERZER 
         END IF
      ELSE
         BTX = ZERZER
         BTY = ONEONE
         BTZ = ZERZER
         B   = ZERZER 
      END IF  
      RETURN
*===  End of subroutine magfld =========================================*
      END
