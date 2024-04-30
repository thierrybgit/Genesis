# 1 "ffte_zfft1d.f90"
# 1 "../lib/ffte_zfft1d.fpp"
# 1 "<built-in>"
# 1 "<command-line>"
# 23 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1

# 17 "/usr/include/stdc-predef.h" 3 4











































# 1 "../lib/ffte_zfft1d.fpp"
!C
!C     1: A FAST FOURIER TRANSFORM PACKAGE
!C
!C     (C) COPYRIGHT SOFTWARE, 2000-2004, 2008-2011, ALL RIGHTS RESERVED
!C                BY
!C         DAISUKE TAKAHASHI
!C         FACULTY OF ENGINEERING, INFORMATION AND SYSTEMS
!C         UNIVERSITY OF TSUKUBA
!C         1-1-1 TENNODAI, TSUKUBA, IBARAKI 305-8573, JAPAN
!C         E-MAIL: daisuke@cs.tsukuba.ac.jp
!C
!C
!C     1-D COMPLEX FFT ROUTINE
!C
!C     FORTRAN77 SOURCE PROGRAM
!C
!C     CALL ZFFT1D(A,N,IOPT,B)
!C
!C     A(N) IS COMPLEX INPUT/OUTPUT VECTOR (COMPLEX(WP))
!C     B(N*2) IS WORK/COEFFICIENT VECTOR (COMPLEX(WP))
!C     N IS THE LENGTH OF THE TRANSFORMS (INTEGER*4)
!C       -----------------------------------
!C         N = (2**IP) * (3**IQ) * (5**IR)
!C       -----------------------------------
!C     IOPT = 0 FOR INITIALIZING THE COEFFICIENTS (INTEGER*4)
!C          = -1 FOR FORWARD TRANSFORM
!C          = +1 FOR INVERSE TRANSFORM
!C
!C     WRITTEN BY DAISUKE TAKAHASHI
!C





!     !$recursive        &
# 37
      SUBROUTINE ZFFT1D(A,N,IOPT,B)
      USE CONSTANTS_MOD
      IMPLICIT REAL(WP) (A-H,O-Z)
!     INCLUDE 'ffte_param.h'
      COMPLEX(WP) A(*),B(*)
!     COMPLEX(WP) C((NDA2+NP)*NBLK),D(NDA2)
!     COMPLEX(WP) WX(NDA2),WY(NDA2)
      DIMENSION IP(3),LNX(3),LNY(3)
!     SAVE WX,WY
!C
      CALL FACTOR(N,IP)
!C
      IF (IOPT .EQ. 1) THEN
!!$omp PARALLEL DO
!!DIR$ VECTOR ALIGNED
        DO 10 I=1,N
          A(I)=CONJG(A(I))
   10   CONTINUE
      END IF
!CJJS
      IF (IOPT .EQ. 2) THEN
        DO I=1,N/2+1
          A(I)=CONJG(A(I))
        ENDDO
      ENDIF
!CJJE
!C
!     IF (N .LE. (L2SIZE/16)/3) THEN
        IF (IOPT .EQ. 0) THEN
          CALL SETTBL(B(N+1),N)
          RETURN
        END IF
!C
!CJJS
      IF (IOPT .EQ. 2) THEN
        A(1)=DCMPLX(REAL(A(1),WP),0.0_WP)
        DO I=2,N/2+1
          A(N-I+2)=CONJG(A(I))
        ENDDO
      ENDIF
        CALL FFT235(A,B,B(N+1),N,IP)
!     ELSE
!       CALL GETNXNY(N,NX,NY)
!       CALL FACTOR(NX,LNX)
!       CALL FACTOR(NY,LNY)
!C
!       IF (IOPT .EQ. 0) THEN
!         CALL SETTBL(WX,NX)
!         CALL SETTBL(WY,NY)
!         CALL SETTBL2(B(N+1),NX,NY)
!         RETURN
!       END IF
!C
!!$omp PARALLEL PRIVATE(C,D)
!       CALL ZFFT1D0(A,A,B,C,C,D,WX,WY,B(N+1),NX,NY,LNX,LNY)
!!$omp END PARALLEL
!     END IF
!C
      IF (IOPT .EQ. 1 .OR. IOPT .EQ. 2) THEN
!       DN=1.0_WP/REAL(N,WP)
        DN=1.0_WP
!!$omp PARALLEL DO
!!DIR$ VECTOR ALIGNED
        DO 20 I=1,N
          A(I)=CONJG(A(I))*DN
   20   CONTINUE
      END IF
      RETURN
      END
!      SUBROUTINE ZFFT1D0(A,AYX,B,CX,CY,D,WX,WY,W,NX,NY,LNX,LNY)
!      USE CONSTANTS_MOD
!      IMPLICIT REAL(WP) (A-H,O-Z)
!      INCLUDE 'ffte_param.h'
!      COMPLEX(WP) A(NX,*),AYX(NY,*),B(NX,*)
!      COMPLEX(WP) CX(NX+NP,*),CY(NY+NP,*),D(*)
!      COMPLEX(WP) WX(*),WY(*),W(NX,*)
!      DIMENSION LNX(*),LNY(*)
!!C
!!$omp DO
!      DO 70 II=1,NX,NBLK
!        DO 30 JJ=1,NY,NBLK
!          DO 20 I=II,MIN0(II+NBLK-1,NX)
!!DIR$ VECTOR ALIGNED
!            DO 10 J=JJ,MIN0(JJ+NBLK-1,NY)
!              CY(J,I-II+1)=A(I,J)
!   10       CONTINUE
!   20     CONTINUE
!   30   CONTINUE
!        DO 40 I=II,MIN0(II+NBLK-1,NX)
!          CALL FFT235(CY(1,I-II+1),D,WY,NY,LNY)
!   40   CONTINUE
!        DO 60 J=1,NY
!!DIR$ VECTOR ALIGNED
!          DO 50 I=II,MIN0(II+NBLK-1,NX)
!            B(I,J)=CY(J,I-II+1)
!   50     CONTINUE
!   60   CONTINUE
!   70 CONTINUE
!!$omp DO
!      DO 120 JJ=1,NY,NBLK
!        DO 90 J=JJ,MIN0(JJ+NBLK-1,NY)
!!DIR$ VECTOR ALIGNED
!          DO 80 I=1,NX
!            CX(I,J-JJ+1)=B(I,J)*W(I,J)
!   80     CONTINUE
!          CALL FFT235(CX(1,J-JJ+1),D,WX,NX,LNX)
!   90   CONTINUE
!        DO 110 I=1,NX
!!DIR$ VECTOR ALIGNED
!          DO 100 J=JJ,MIN0(JJ+NBLK-1,NY)
!            AYX(J,I)=CX(I,J-JJ+1)
!  100     CONTINUE
!  110   CONTINUE
!  120 CONTINUE
!      RETURN
!      END
!!