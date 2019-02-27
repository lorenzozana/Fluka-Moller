c$$$  There are 2 key points:
c$$$  1) you must be able to OPEN and READ an external file
c$$$  which gives a map of magnetic field (3 components) as a function of
c$$$  x, y, z coordinates in a 3-D lattice, storing them in vectors defined by
c$$$  you.
c$$$  2) you must be able to code an algorithm capable to interpolate in
c$$$  the map, using the above vector, and return the best estimate of the field
c$$$  components at the run time coordinate X,Y,Z avaibale in the MAGFLD
c$$$  routine.
c$$$  
c$$$  Such codes line can be places in the following skeleton, where I imagined
c$$$  that you will store tha map in 3 vectors BX(I,J,K), BY(I,J,K) and
c$$$  BZ(I,J,K), where I,J and K are the indeces runnnig through the lattice
c$$$  points contained in vectors XB(I), YB(J), ZB(K). The index I runs from 1
c$$$  to NX, J runs from 1 to NY and K runs from 1 to NZ. The dimensions NX, NY
c$$$  and NZ are given as parameters,

*===  magfld=============================================================*
*     
      SUBROUTINE MAGFLD ( X, Y, Z, BTX, BTY, BTZ, B, NREG, IDISC )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
*     
*----------------------------------------------------------------------*
*     *
*     Copyright (C) 1988-2005 by Alberto Fasso` & Alfredo Ferrari *
*     All Rights Reserved. *
*     *
*     *
*     Created in 1988 by Alberto Fasso`, CERN - TIS *
*     *
*     Last change on 11-dec-92 by Alfredo Ferrari *
*     *
*     Input variables: *
*     x,y,z = current position *
*     nreg = current region *
*     Output variables: *
*     btx,bty,btz = cosines of the magn. field vector *
*     B = magnetic field intensity (Tesla) *
*     idisc = set to 1 if the particle has to be discarded *
*     *
*----------------------------------------------------------------------*
*     
      PARAMETER (NX=37)
      PARAMETER (NY=11)
      PARAMETER (NZ=16)
      PARAMETER (NPOINT=NX*NY*NZ)
      DIMENSION BR(NX,NY,NZ)
      DIMENSION BPHI(NX,NY,NZ)
      DIMENSION BZ(NX,NY,NZ)
      DIMENSION RB(NX), PHIB(NY), ZB(NZ)

      LOGICAL LFIRST
      DATA LFIRST /.TRUE./
      SAVE LFIRST

      REAL DX, DY, DZ
      REAL BRV, BPHIV, BZV
      INTEGER BINX,BINY,BINZ

      PARAMETER (NSEC = 7)
      PARAMETER (PHIOFF = 25.71429)
      PARAMETER (XMIN = 0.015)
      PARAMETER (XMAX = 0.375)
      PARAMETER (YMIN = -25.0)
      PARAMETER (YMAX = 25.0)
      PARAMETER (ZMIN = 9.5)
      PARAMETER (ZMAX = 17.0)



      IDISC = 0

      IF (LFIRST) THEN
*     Here, at the first time in which MAGFLD is called
*     open and read a file containing a map of the 3 magnetic field components
*     BR(I,J,K), BPHI(I,J,K), BZ(I,J,K)
*     in a lattice of points in 3 dimensions RB(I), PHIB(J), ZB(K). There are
*     NX points in R with running index I
*     NY points in PHY with running index J
*     NZ points in Z with running index K
         OPEN(UNIT = 20, FILE = 'Magnetic_field/c_bH.txt')
*         LUNRD = NINT(WHASOU(1))
         DO I = 1, NX
            DO  J = 1, NY
               DO  K = 1, NZ
                  READ(20,*) DX,DY,DZ,BRV,BPHIV,BZV
                  RB(I) = DX*100
                  PHIB(J) = DY
                  ZB(K) = DZ*100
                  BR(I,J,K) = BRV
                  BPHI(I,J,K) = BPHIV
                  BZ(I,J,K) = BZV
               END DO
            END DO
         END DO    
         LFIRST = .FALSE.
      ENDIF
*     Optional check on the Region at run time, to avoid calculation in
*     regions where magnetic field is expected to be null..
*     Each time the routine is called, activate an
*     algorithm of your choice that, given the particle coordinates X, Y
*     and Z, finds the closest RB(), PHIB() and ZB() coordinates and
*     interpolates the local value of BR, BPHI and BZ...
*     Then:
      PRINT*, RB(1), PHIB(2), ZB(3), BR(1,2,3)

      IF (X.GE.XMIN .AND.X.LE.XMAX) THEN
         IF(Y.GE.YMIN.AND.Y.LE.YMAX) THEN
            IF(Z.GE.ZMIN.AND.Z.LE.ZMAX) THEN
               DX = (XMAX-XMIN)/NX
               DY = (YMAX-YMIN)/NY
               DZ = (ZMAX-ZMIN)/NZ
               BINX = NINT((X-XMIN)/DX)+1
               BINY = NINT((Y-YMIN)/DY)+1
               BINZ = NINT((Z-ZMIN)/DZ)+1
               BRV=BR(BINX,BINY,BINZ)
               BPHIV=BPHI(BINX,BINY,BINZ)
               BZV=BZ(BINX,BINY,BINZ)
               B = SQRT(BRV**2 + BZV**2)
               IF (B.GT.0.0) THEN
                  BTX = COS(BPHIV)
                  BTY = SIN(BPHIV)
                  BTZ = SQRT(1 - BTX**2 -BTY**2)
               ELSE
                  BTX = 0.0
                  BTY = 0.0
                  BTZ = 1.0
               END IF
               RETURN
            ELSE
               B = 0.0
               BTX = 0.0
               BTY = 0.0
               BTZ = 1.0
               RETURN
            ENDIF
         ELSE
            B = 0.0
            BTX = 0.0
            BTY = 0.0
            BTZ = 1.0
            RETURN
         ENDIF
      ELSE
         B = 0.0
         BTX = 0.0
         BTY = 0.0
         BTZ = 1.0
         RETURN
      ENDIF
      
*===  End of subroutine magfld =========================================*
      END 
*     
 
