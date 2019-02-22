c$$$There are 2 key points:
c$$$1) you must be able to OPEN and READ an external file
c$$$which gives a map of magnetic field (3 components) as a function of
c$$$x, y, z coordinates in a 3-D lattice, storing them in vectors defined by
c$$$you.
c$$$2) you must be able to code an algorithm capable to interpolate in
c$$$the map, using the above vector, and return the best estimate of the field
c$$$components at the run time coordinate X,Y,Z avaibale in the MAGFLD
c$$$routine.
c$$$
c$$$Such codes line can be places in the following skeleton, where I imagined
c$$$that you will store tha map in 3 vectors BX(I,J,K), BY(I,J,K) and
c$$$BZ(I,J,K), where I,J and K are the indeces runnnig through the lattice
c$$$points contained in vectors XB(I), YB(J), ZB(K). The index I runs from 1
c$$$to NX, J runs from 1 to NY and K runs from 1 to NZ. The dimensions NX, NY
c$$$and NZ are given as parameters,

*===magfld=============================================================*
*
      SUBROUTINE MAGFLD ( X, Y, Z, BTX, BTY, BTZ, B, NREG, IDISC )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
*
*----------------------------------------------------------------------*
* *
* Copyright (C) 1988-2005 by Alberto Fasso` & Alfredo Ferrari *
* All Rights Reserved. *
* *
* *
* Created in 1988 by Alberto Fasso`, CERN - TIS *
* *
* Last change on 11-dec-92 by Alfredo Ferrari *
* *
* Input variables: *
* x,y,z = current position *
* nreg = current region *
* Output variables: *
* btx,bty,btz = cosines of the magn. field vector *
* B = magnetic field intensity (Tesla) *
* idisc = set to 1 if the particle has to be discarded *
* *
*----------------------------------------------------------------------*
*
      PARAMETER (NX=37)
      PARAMETER (NY=11)
      PARAMETER (NZ=16)
      PARAMETER (NPOINT=NX*NY*NZ)
      DIMENSION BX(NX,NY,NZ)
      DIMENSION BY(NX,NY,NZ)
      DIMENSION BZ(NX,NY,NZ)
      DIMENSION XB(NX), YB(NY), ZB(NZ)
      DIMENSION DUMMY(3)

      LOGICAL LFIRST
      DATA LFIRST /.TRUE./
      SAVE LFIRST

      REAL XMIN,XMAX
      REAL YMIN,YMAX
      REAL ZMIN,ZMAX
      REAL DX, DY, DZ
      INTEGER BINX,BINY,BINZ

      XMIN = 0.015
      XMAX = 0.375
      YMIN = -25.0
      YMAX = 25.0
      ZMIN = 9.5
      ZMAX = 17.0

      DX=(XMAX-XMIN)/NX
      DY=(YMAX-YMIN)/NY
      DZ=(ZMAX-ZMIN)/NZ


      IDISC = 0

      IF (LFIRST) THEN
         
* Here, at the first time in which MAGFLD is called
* open and read a file containing a map of the 3 magnetic field components
* BX(I,J,K), BY(I,J,K), BZ(I,J,K)
* in a lattice of points in 3 dimensions XB(I), YB(J), ZB(K). There are
* NX points in X with running index I
* NY points in Y with running index J
* NZ points in Z with running index K
         LUNRD = NINT(WHASOU(1))
         iloop: DO I = 1, NX
            jloop: DO  J = 1, NY
               kloop: DO 3 K = 1, NZ
                  READ(LUNRD,*) XB(I), YB(J), ZB(K),
                                   BX(I,J,K), BY(I,J,K), BZ(I,J,K)
                      END DO kloop
            END DO jloop
         END DO iloop   
         LFIRST = .FALSE.
      ENDIF
* Optional check on the Region at run time, to avoid calculation in
* regions where magnetic field is expected to be null..
      IF(NREG .NE. ...) THEN
         WRITE (LUNOUT,*)
     & ' Magfld called in zone ',NREG,' where there should'
         WRITE (LUNOUT,*)
     & ' be no magnetic field. Something is wrong'
         IDISC = 1
         RETURN
      ELSE
* Each time the routine is called, activate an
* algorithm of your choice that, given the particle coordinates X, Y
* and Z, finds the closest XB(), YB() and ZB() coordinates and
* interpolates the local value of BX, BY and BZ...
*     Then:
         IF(X.GE.XMIN .AND. X.LE.XMAX .AND. 
            Y.GE.YMIN .AND. Y.LE.YMAX .AND.
            Z.GE.ZMIN .AND. Z.LE.ZMAX) THEN
            BINX = CEILING((X-XMIN)/DX)
            BINY = CEILING((Y-YMIN)/DY)
            BINZ = CEILING((Z-ZMIN)/DZ)
         
            B = SQRT(BX(BINX,BINY,BINZ)**2 + BY(BINX,BINY,BINZ)**2 
            + BZ(BINX,BINY,BINZ)**2)
            BTX = BX(BINX,BINY,BINZ) / B
            BTY = BY(BINX,BINY,BINZ) / B
            BTZ = SQRT(1 - BTX**2 -BTY**2)
            RETURN
         ELSE
            B = 0
            BTX = 0
            BTY = 0
            BTZ = 1
            RETURN
         ENDIF
         RETURN
      ENDIF
*=== End of subroutine magfld =========================================*
      END 
*
      REAL FUNCTION FINT3(ARG,NENT,ENT,TABLE)
C
C   INTERPOLATION ROUTINE. AUTHOR C. LETERTRE.
C   MODIFIED BY B. SCHORR, 1.07.1982.
C
      implicit none
      INTEGER   NARG,N,LMAX,ISTEP,KNOTS,NDIM,LOCA,LMIN,LOCB
      INTEGER   LOCC,ISHIFT,I,K
      REAL      X,H,ETA
      INTEGER   NENT(3)
      REAL      ARG(3),ENT(12),   TABLE(60)
      INTEGER   INDEX(32)
      REAL      WEIGHT(32)
      parameter (NARG=3)
      FINT3  =  0.
      IF(NARG .LT. 1  .OR.  NARG .GT. 5)  RETURN
      LMAX      =  0
      ISTEP     =  1
      KNOTS     =  1
      INDEX(1)  =  1
      WEIGHT(1) =  1.
      DO 100    N  =  1, NARG
         X     =  ARG(N)
         NDIM  =  NENT(N)
         LOCA  =  LMAX
         LMIN  =  LMAX + 1
         LMAX  =  LMAX + NDIM
         IF(NDIM .GT. 2)  GOTO 10
         IF(NDIM .EQ. 1)  GOTO 100
         H  =  X - ENT(LMIN)
         IF(H .EQ. 0.)  GOTO 90
         ISHIFT  =  ISTEP
         IF(X-ENT(LMIN+1) .EQ. 0.)  GOTO 21
         ISHIFT  =  0
         ETA     =  H / (ENT(LMIN+1) - ENT(LMIN))
         GOTO 30
 10      LOCB  =  LMAX + 1
 11      LOCC  =  (LOCA+LOCB) / 2
         IF(X-ENT(LOCC))  12, 20, 13
 12      LOCB  =  LOCC
         GOTO 14
 13      LOCA  =  LOCC
 14      IF(LOCB-LOCA .GT. 1)  GOTO 11
         LOCA    =  MIN( MAX(LOCA,LMIN), LMAX-1 )
         ISHIFT  =  (LOCA - LMIN) * ISTEP
         ETA     =  (X - ENT(LOCA)) / (ENT(LOCA+1) - ENT(LOCA))
         GOTO 30
 20      ISHIFT  =  (LOCC - LMIN) * ISTEP
 21      DO 22  K  =  1, KNOTS
            INDEX(K)  =  INDEX(K) + ISHIFT
 22      CONTINUE
         GOTO 90
 30      DO 31  K  =  1, KNOTS
            INDEX(K)         =  INDEX(K) + ISHIFT
            INDEX(K+KNOTS)   =  INDEX(K) + ISTEP
            WEIGHT(K+KNOTS)  =  WEIGHT(K) * ETA
            WEIGHT(K)        =  WEIGHT(K) - WEIGHT(K+KNOTS)
 31      CONTINUE
         KNOTS  =  2*KNOTS
 90      ISTEP  =  ISTEP * NDIM
 100  CONTINUE
      DO 200    K  =  1, KNOTS
         I  =  INDEX(K)
         FINT3  =  FINT3 + WEIGHT(K) * TABLE(I)
 200  CONTINUE
      RETURN
      END
