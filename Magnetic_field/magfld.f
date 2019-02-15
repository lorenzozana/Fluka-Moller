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
*     Copyright (C) 1988-2010      by Alberto Fasso` & Alfredo Ferrari *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     Created  in     1988         by    Alberto Fasso`                *
*                                                                      *
*                                                                      *
*     Last change on 06-Nov-10     by    Alfredo Ferrari               *
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
* Start_Prod_Seq
*     INCLUDE '(CMEMFL)'
* End_Prod_Seq
      INCLUDE '(CSMCRY)'
*
*  +-------------------------------------------------------------------*
*  |  Earth geomagnetic field:
      IF ( LGMFLD ) THEN
         CALL GEOFLD ( X, Y, Z, BTX, BTY, BTZ, B, NREG, IDISC )
         RETURN
      END IF
*  |
*  +-------------------------------------------------------------------*
      IDISC = 0
* Start_Devel_Seq
      GO TO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,
     &       23,24,25,26,27,28,29,30,31,32,33), NREG
* Central cavity:
   2  CONTINUE
   3  CONTINUE
         BTX = 0.D+00
         BTY = 0.D+00
         BTZ = 1.D+00
         B   = 2.D+00
         RETURN
  15  CONTINUE
         BTX = 0.D+00
         BTY = 0.D+00
         BTZ =-1.D+00
         B   = 1.8618D+00
         RETURN
  16  CONTINUE
  17  CONTINUE
  18  CONTINUE
  19  CONTINUE
  20  CONTINUE
  21  CONTINUE
  22  CONTINUE
  23  CONTINUE
  24  CONTINUE
  25  CONTINUE
  26  CONTINUE
  27  CONTINUE
  28  CONTINUE
  29  CONTINUE
  30  CONTINUE
  31  CONTINUE
  32  CONTINUE
  33  CONTINUE
         RRR = SQRT ( X**2 + Y**2 )
         BTX = Y / RRR
         BTY = X / RRR
         BTZ = 0.D+00
         B0  = 1.D+00
         IF ( RRR .GT. 500.D+00 ) THEN
            IF ( RRR .LT. 1000.D+00 ) THEN
               B = B0 * 500.D+00 / RRR
            ELSE
*  In this way the field is going from 0.5 T to 0.03 T for R ranging
*  from 10 to 12 m
               B = B0 * ( 1000.D+00 / RRR )**15.431037D+00
            END IF
         ELSE
*  In this way the field is going from 1 T to 0.2 T for R ranging
*  from 5 to 4.5 m
            B = B0 * ( RRR / 500.D+00 )**15.275532D+00
         END IF
         RETURN
   1  CONTINUE
   4  CONTINUE
   5  CONTINUE
   6  CONTINUE
   7  CONTINUE
   8  CONTINUE
   9  CONTINUE
  10  CONTINUE
  11  CONTINUE
  12  CONTINUE
  13  CONTINUE
  14  CONTINUE
      WRITE (LUNOUT,*)
     &      ' Magfld called in zone ',NREG,' where there should'
      WRITE (LUNOUT,*)
     &      ' be no magnetic field. Something is wrong'
      IDISC = 1
* End_Devel_seq
* 2 Tesla uniform field along +z:
* Start_Prod_seq
*     BTX   = UMGFLD
*     BTY   = VMGFLD
*     BTZ   = WMGFLD
*     B     = BIFUNI
* End_Prod_seq
      RETURN
*=== End of subroutine Magfld =========================================*
      END

