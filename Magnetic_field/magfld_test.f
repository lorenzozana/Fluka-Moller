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
      INCLUDE '(CMEMFL)'
      INCLUDE '(CSMCRY)'
*
*  +-------------------------------------------------------------------*

*      IDISC = 0
* 2 Tesla uniform field along +z:
      real bx,by,bz
      real testmf
      real xmin,xmax,ymin,ymax,zmin,zmax
      xmin = 10.5
      xmax = 14.5
      ymin = 0.5
      ymax = 3.5
      zmin = 990
      zmax = 1200
      IF(X.LT.XMIN .AND. X.GT.XMAX) THEN
         BTX = 0.0
         BTY = 0.0
         BTZ = 1.0
         B = 0.0
         RETURN
      ELSE IF(Y.LT.YMIN .AND. Y.GT.YMAX) THEN
         BTX = 0.0
         BTY = 0.0
         BTZ = 1.0
         B = 0.0
         RETURN
      ELSE IF(Z.LT.ZMIN .AND. Z.GT.ZMAX) THEN
         BTX = 0.0
         BTY = 0.0
         BTZ = 1.0
         B = 0.0
         RETURN
      ELSE 
         bx =  testmf(X,Y,Z,1)
         by =  testmf(X,Y,Z,2)
         bz =  testmf(X,Y,Z,3)
         B = SQRT(bx**2 + by**2 +bz**2)
         IF(B.GT.0.0) THEN
            BTX   = bx/B
            BTY   = by/B
            BTZ   = SQRT(1.0 - BTX**2 - BTY**2)
         ELSE
            BTX = 0.0
            BTY = 0.0
            BTZ = 1.0
         END IF
         RETURN
      END IF
*=== End of subroutine Magfld =========================================*
      END
*
*
      real function testmf(x,y,z,iax)
      implicit none
      real x,y,z
      real vx,vy,vz,fv
      integer index,i,j,k,iax
c
c     -- common block description (makes sense to make it an include file) --
c     -- or make sure it is exactly the same in the calling routine        --
      integer NARG,NX,NY,NZ,LENT,LENBT
      parameter (NARG=3) ! =3 for the 3-dimensional space x,y,z
      parameter (NX=5)   ! Number of table nodes in x
      parameter (NY=4)   ! Number of table nodes in y
      parameter (NZ=3)   ! Number of table nodes in z
      parameter (LENT=NX+NY+NZ)   ! Length of the ENT array
      parameter (LENBT=NX*NY*NZ)  ! Length of the BX, BY, BZ component tables
      logical init
      integer NENT(NARG)
      real ENT(LENT),BX(LENBT),BY(LENBT),BZ(LENBT)
      common/magtable/init,NENT,ENT,BX,BY,BZ
c
      real    ARG(NARG),FX,FY,FZ
c
c -- Sample data --
      integer DNENT(NARG)
      real    DENT(LENT)
      data init/.true./
      data DNENT/NX,NY,NZ/ ! test matrix 5x4x3
      data DENT/10.5,11.5,12.5,13.5,14.5,0.5,1.5,2.5,3.5,
                990.,1100.,1150./            ! 5 pts in x, 4 in y, 3 in z
C -- BX,BY,BZ are the functions in the nodes:
C -- x1y1z1,x2y1z1,x3y1z1,x4y1z1,x5y1z1, x1y2z1,x2y2z1,x3y2z1,x4y2z1,x5y2z1, x1y3z1,x2y3z1,x3y3z1,x4y3z1,x5y3z1, 
C -- x1y4z1,x2y4z1,x3y4z1,x4y4z1,x5y4z1, x1y1z1,x2y1z1,x3y1z1,x4y1z1,x5y1z1, 
C -- x1y1z2,x2y1z2,x3y1z2,x4y1z2,x5y1z2, x1y2z2,x2y2z2,x3y2z2,x4y2z2,x5y2z2, x1y3z2,x2y3z2,x3y3z2,x4y3z2,x5y3z2, 
C -- x1y4z2,x2y4z2,x3y4z2,x4y4z2,x5y4z2, x1y1z2,x2y1z2,x3y1z2,x4y1z2,x5y1z2, 
C -- x1y1z3,x2y1z3,x3y1z3,x4y1z3,x5y1z3, x1y2z3,x2y2z3,x3y2z3,x4y2z3,x5y2z3, x1y3z3,x2y3z3,x3y3z3,x4y3z3,x5y3z3, 
C -- x1y4z3,x2y4z3,x3y4z3,x4y4z3,x5y4z3, x1y1z3,x2y1z3,x3y1z3,x4y1z3,x5y1z3
C
      if(init) then
C -- Test function F = sqrt(x)+sqrt(y)+sqrt(z), or F = x+y+z, or ... --
C -- Set values in common block from the sample data
         do i = 1, NARG
            NENT(i) = DNENT(i)
         enddo
         do i = 1, LENT
            ENT(i)  = DENT(i)
         enddo
         index = 1
         do k = 1, NENT(3)
            do j = 1, NENT(2)
               do i = 1, NENT(1)
                  vx = i
                  vy = j
                  vz = k
                  BX(index) = vx + vy + vz
                  BY(index) = sqrt(vx) + sqrt(vy) + sqrt(vz)
                  BZ(index) = vx**2 + vy**2 + vz**2
                  index = index + 1
               enddo
            enddo
         enddo
         init = .false.
      endif
      ARG(1) = x
      ARG(2) = y
      ARG(3) = z
      CALL MAGFINT3(ARG,FX,FY,FZ)
      if    (iax .eq. 1) then
         testmf = FX
      elseif(iax .eq. 2) then
         testmf = FY
      elseif(iax .eq. 3) then
         testmf = FZ
      else
         testmf = 0.
      endif
      return
      end
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
      integer NARG,NX,NY,NZ,LENT,LENBT
      parameter (NARG=3) ! =3 for the 3-dimensional space x,y,z
      parameter (NX=5)   ! Number of table nodes in x
      parameter (NY=4)   ! Number of table nodes in y
      parameter (NZ=3)   ! Number of table nodes in z
      parameter (LENT=NX+NY+NZ)   ! Length of the ENT array      DO 100    N  =  1, NARG
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
         FX  =  FX + WEIGHT(K) * BX(I)
         FY  =  FY + WEIGHT(K) * BY(I)
         FZ  =  FZ + WEIGHT(K) * BZ(I)
 200  CONTINUE
      RETURN
      END

