*$ CREATE MAGFLD.FOR
*COPY MAGFLD
*
*=== magfld ===========================================================*
*
      SUBROUTINE MAGFLD ( X, Y, Z, T, BTX, BTY, BTZ, B, NREG, IDISC )

      INCLUDE '(DBLPRU)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1988-2020      by Alberto Fasso` & Alfredo Ferrari *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     Created  in     1988         by    Alberto Fasso`                *
*                                                                      *
*                                                                      *
*     Last change on  10-Mar-20    by    Alberto Fasso`                *
*                                          SLAC/USA                    *
*                                                                      *
*     Input variables:                                                 *
*            x,y,z = current position                                  *
*            t     = current time                                      *
*            nreg  = current region                                    *
*     Output variables:                                                *
*            btx,bty,btz = cosines of the magn. field vector           *
*            B = magnetic field intensity (Tesla)                      *
*            idisc = set to 1 if the particle has to be discarded      *
*                                                                      *
*----------------------------------------------------------------------*
*
      INCLUDE '(BLNKCM)'
      INCLUDE '(CMEMFL)'
      INCLUDE '(TRACKR)'
      INCLUDE '(CSMCRY)'
*  Statement functions: don't include commons belows this line
      INCLUDE '(SFEMFL)'
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
* 2 Tesla uniform field along +z:
      BTX   = UMGFLD
      BTY   = VMGFLD
      BTZ   = WMGFLD
      B     = BIFUNI
      IF ( .NOT. LMGFON (NREG,IPRODC) ) THEN
         WRITE (LUNOUT,*)
     &      ' Magfld called in zone ',NREG,' where there should'
         WRITE (LUNOUT,*)
     &      ' be no magnetic field. Something is wrong'
         WRITE (LUNERR,*)
     &      ' Magfld called in zone ',NREG,' where there should'
         WRITE (LUNERR,*)
     &      ' be no magnetic field. Something is wrong'
         IDISC = 1
      END IF
      RETURN
*=== End of subroutine Magfld =========================================*
      END

