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
      DIMENSION IROTIN(10)
      LOGICAL LFIRST
      SAVE LFIRST
      SAVE  DIPFLD, GRADIENT, NQUADRA, NDIPOLE, IROTIN
*
      DATA LFIRST / .TRUE. /, IROTIN / 10*0 /
*
      IDISC = 0
      IF (LFIRST) THEN
*     gradient in tesla per meter for 1 GeV protons
         GRADIENT = 1.6960379182082D+00 
*     field in tesla for 1 GeV protons
         DIPFLD   = 1.330063459521429D+00 
         CALL GEON2R("rQuadrA ",NQUADRA,IERR)
         CALL GEON2R("rDipole ",NDIPOLE,IERR)
         CALL GEON2L("q1      ", NLATT, IRTLAT, IERR)
         IROTIN(NLATT)=IRTLAT
         CALL GEON2L("q2      ", NLATT, IRTLAT, IERR)
         IROTIN(NLATT)=IRTLAT
         CALL GEON2L("q3      ", NLATT, IRTLAT, IERR)
         IROTIN(NLATT)=IRTLAT
         CALL GEON2L("q4      ", NLATT, IRTLAT, IERR)
         IROTIN(NLATT)=IRTLAT
         LFIRST  = .FALSE.
      END IF
*
      IF (NREG.EQ.NQUADRA) THEN 
         XNEW = X
         YNEW = Y
         ZNEW = Z
         IF (MLATTC .GT. 0) THEN
            CALL DOTRSF ( 1, XNEW, YNEW, ZNEW, IROTIN(MLATTC) )
         ELSE
            CALL FLABRT ( 'MAGFLD', 'what do we do in the parking?!' )
         END IF
c     gradient in tesla per meter
         IF (MLATTC.EQ.1.OR.MLATTC.EQ.3) THEN
            BTX = -GRADIENT * YNEW/1D+02
            BTY = -GRADIENT * XNEW/1D+02
         ELSE 
            BTX =  GRADIENT * YNEW/1D+02
            BTY =  GRADIENT * XNEW/1D+02
         END IF 
         
         BTZ=ZERZER
         B=SQRT(BTX**2+BTY**2)
         IF (B.GT.1D-12) THEN
            BTX = BTX / B
            BTY = BTY / B
            CALL UNDRTO ( 1, BTX, BTY, BTZ, IROTIN(MLATTC) )            
         ELSE
            B = ZERZER
            BTX = ONEONE
            BTY = ZERZER
         END IF
      ELSE IF (NREG.EQ.NDIPOLE) THEN
         BTX = ZERZER
         BTY = ONEONE
         BTZ = ZERZER
         B   = DIPFLD 
      ELSE
         CALL FLABRT ( 'MAGFLD', 'NO MAGNETIC FIELD IN THIS REGION!' )
      END IF
      RETURN
*=== End of subroutine magfld =========================================*
      END
