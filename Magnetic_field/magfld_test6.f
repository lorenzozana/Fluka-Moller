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
      INTEGER NR, NPHI, NZ, NPOINT,LENT
      REAL RMIN,RMAX,PhiMIN,PhiMAX,ZMIN,ZMAX
      REAL PHIOFF,TOTSEC
      PARAMETER (NR=37)
      PARAMETER (NPHI=11)
      PARAMETER (NZ=16)
      PARAMETER (NPOINT=NR*NPHI*NZ)
      PARAMETER (LENT=NR+NPHI+NZ)
      REAL BR(NPOINT)
      REAL BPHI(NPOINT)
      REAL BZ(NPOINT)
      REAL RB(NR), PHIB(NPHI), ZB(NZ)
      REAL DX, DY, DZ, DR, DPHI
      REAL BRV, BPHIV, BZV
      INTEGER I, J, K, INDEX
      PARAMETER (RMIN = 1.5)
      PARAMETER (RMAX = 37.5)
      PARAMETER (PhiMIN = -25.0)
      PARAMETER (PhiMAX = 25.0)
      PARAMETER (ZMIN = 950.0)
      PARAMETER (ZMAX = 1700.0)
      PARAMETER (PHIOFF = 25.71429)
      PARAMETER (TOTSEC = 7.)
      INTEGER NSEC
      INTEGER BINR,BINP,BINZ
      REAL BXV,BYV
      REAL BR111,BP111,BZ111
      REAL    ARG(3),FX,FY,FZ
      INTEGER NENT(3)
      REAL ENT(LENT) 
      common/magtable/init,NENT,ENT,BR,BPHI,BZ
c
c
      IDISC = 0
      IF (LFIRST) THEN
*     gradient in tesla per cm
         GRADIENT = 0.08D+00  
         INDEX = 1
         OPEN(UNIT=18, FILE ='/home/zana/c_bH.txt')
*         LUNRD = NINT(WHASOU(1))
         DO I = 1, NR
            DO  J = 1, NPHI
               DO  K = 1, NZ
                  READ(18,*) DX,DY,DZ,BRV,BPHIV,BZV
                  RB(I) = DX*100
                  PHIB(J) = DY
                  ZB(K) = DZ*100
                  BR(INDEX) = BRV
                  BPHI(INDEX) = BPHIV
                  BZ(INDEX) = BZV
                  INDEX = INDEX + 1
               END DO
            END DO
         END DO     
         INDEX = 1
         NENT(1) = NZ
         DO  I = 1, NZ
            ENT(INDEX) = ZB(I)
            INDEX = INDEX + 1
         END DO
         NENT(2) = NPHI
         DO  J = 1, NPHI
            ENT(INDEX) = PHIB(J)
            INDEX = INDEX + 1
         END DO
         NENT(3) = NR
         DO  K = 1, NR
            ENT(INDEX) = RB(K)
            INDEX = INDEX + 1
         END DO
         LFIRST  = .FALSE.
      END IF
* Phi offset is added to the limits as from the Geant4 code (L245 remollMagneticField.cc)
      R = SQRT(X*X + Y*Y)
      PHI = ATAN2(Y,X) * 180. / 3.1416
* Setting PHI  between 0-360 deg
      IF ( PHI .LT. 0.0) THEN
         PHI = PHI + 360.
      END IF
* Getting which sector PHI is
      NSEC = INT(PHI / 360. *TOTSEC)
* Modify Phi to be in the evaluated sector
      PHI = PHI - NSEC * 360. / TOTSEC
      IF ( Z .LT. ZMAX .AND. Z .GT. ZMIN ) THEN
         IF ( R .LT. RMAX .AND. R .GT. RMIN ) THEN 
            IF (PHI.LT.(PhiMAX+PHIOFF).AND.PHI.GT.(PhiMIN+PHIOFF))THEN
               ARG(1) = Z
               ARG(2) = PHI-PHIOFF
               ARG(3) = R
               CALL MAGFINT3(ARG,FX,FY,FZ)
*     Getting the value of Field and Angle (in radiants) at the reticolate rotated to the correct sector 
*     Here is used 111 as corner in lowR,lowphi,lowZ
               BR111 = FX
               BP111 = FY
               BZ111 = FZ
               P111 = (PHI+PHIOFF+NSEC*360/TOTSEC)/180*3.1416
*     Here should happen the interpolation, before getting the XYZ coordinates, since the grid is a section of a tube
               BXV = BR111*COS(P111) - BP111*SIN(P111) 
               BYV = BR111*SIN(P111) + BP111*COS(P111)
               BZV = BZ111
               B=SQRT(BXV**2+BYV**2+BZV**2)
               IF (B.GT.1D-12) THEN
                  BTX = BXV / B
                  BTY = BYV / B
                  BTZ = BZV / B
               ELSE
                  B = 0.0
                  BTX = 1.0
                  BTZ= 0.0
                  BTY = 0.0
               END IF
            ELSE
               BTX = 0.0
               BTY = 1.0
               BTZ = 0.0
               B   = 0.0 
            END IF
         ELSE
            BTX = 0.0
            BTY = 1.0
            BTZ = 0.0
            B   = 0.0 
         END IF
      ELSE
         BTX = 0.0
         BTY = 1.0
         BTZ = 0.0
         B   = 0.0 
      END IF  
      RETURN
*===  End of subroutine magfld =========================================*
      END
*
      subroutine MAGFINT3(ARG,FX,FY,FZ)
C
C   P. Degtiarenko, JLAB, 2019
C   Based on the INTERPOLATION ROUTINE 'FINT' by C. LETERTRE
C   and MODIFIED BY B. SCHORR, 1.07.1982, from the CERNLIB
C
      implicit none
      REAL      FX,FY,FZ
      INTEGER   N,LMAX,ISTEP,KNOTS,NDIM,LOCA,LMIN,LOCB
      INTEGER   LOCC,ISHIFT,I,K
      REAL      X,H,ETA
      INTEGER   INDEX(32)
      REAL      WEIGHT(32)
c
c     -- common block description (makes sense to make it an include file) --
c     -- or make sure it is exactly the same in the calling routine        --
      integer NARG,NR,NPHI,NZ,LENT,LENBT
      parameter (NARG=3) ! =3 for the 3-dimensional space x,y,z
      parameter (NR=37)   ! Number of table nodes in x
      parameter (NPHI=11)   ! Number of table nodes in y
      parameter (NZ=16)   ! Number of table nodes in z
      parameter (LENT=NR+NPHI+NZ)   ! Length of the ENT array
      parameter (LENBT=NR*NPHI*NZ)  ! Length of the BX, BY, BZ component tables
      logical init
      integer NENT(NARG)
      real ENT(LENT),BR(LENBT),BPHI(LENBT),BZ(LENBT)
      common/magtable/init,NENT,ENT,BR,BPHI,BZ
c
      real    ARG(NARG)
c
      FX  =  0.
      FY  =  0.
      FZ  =  0.
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
         FX  =  FX + WEIGHT(K) * BR(I)
         FY  =  FY + WEIGHT(K) * BPHI(I)
         FZ  =  FZ + WEIGHT(K) * BZ(I)
 200  CONTINUE
      RETURN
      END

