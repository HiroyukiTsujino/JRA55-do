      SUBROUTINE GAUSS     !"  calculate Gaussian latitude & weight
     O         ( CTHETA , GW     ,
     I           JMAX             )
      implicit none
*
*   [PARAM]
      INTEGER    JMAX
*
*   [OUTPUT]
      REAL * 8   CTHETA( JMAX )              !" sin(lat.)=mu
      REAL * 8   GW    ( JMAX )              !" Gaussian weight
*
*   [INTERNAL WORK]
      INTEGER    JMAXD
      PARAMETER (JMAXD=400)
      REAL * 8   QPN( 0:JMAXD+1 )            !" Pn (double precision)
      REAL * 8   DELTP
      REAL * 8   PI, X0, DELTX, R, RP
      REAL * 8   EPS, EPSM, EPSP, QDPN
      INTEGER    N, J, ITER, I
*
*   [INTERNAL PARM]
      INTEGER    ITRMAX
      DATA       ITRMAX / 50 /               !" max. of iteration
*
      IF ( JMAX .GT. JMAXD) THEN
            WRITE (6,*) ' ### GAUSS: WORK AREA(JMAXD) TOO SMALL < ',
     &                  JMAX
            stop
      ENDIF
*
*"         < 0. check precision >
*
      DELTP = 1.
      DO 1100 I = 1, 30
         DELTP = DELTP / 10.d0
         R   = 1.
         RP  = R + DELTP
C         WRITE ( 6,* ) ' *** precision check .. ', RP
         IF ( RP .LE. R ) GOTO 1200
 1100     CONTINUE
 1200      CONTINUE
      DELTP = DELTP *100.
*
      PI = ATAN( 1.d0 ) * 4.d0
*
      DO 5100 J = 1 , JMAX/2
*
*"         < 1. initial guess >
*
         X0 = COS (  ( DBLE ( J ) - 0.5d0  )/ DBLE ( JMAX ) * PI   )
*
         DO 4100 ITER = 1, ITRMAX
*
*"         < 2. calc. Pn >
*
            QPN   ( 0 ) = 1.d0
            QPN   ( 1 ) = SQRT( 3.d0  ) * X0
*
            DO 2100 N = 2, JMAX+1
               EPS      = SQRT (   ( DBLE ( N   )**2                )
     &                           / ( 4.d0 * DBLE ( N   )**2 - 1.d0  ) )
               EPSM     = SQRT (   ( DBLE ( N-1 )**2                )
     &                           / ( 4.d0 * DBLE ( N-1 )**2 - 1.d0  ) )
*
               QPN ( N ) = (   QPN  ( N-1 ) * X0
     &                        - QPN  ( N-2 ) * EPSM    ) / EPS
 2100                 CONTINUE
*
*"         < 3. calc. d/dmu Pn >
*
            EPS      = SQRT (   ( DBLE ( JMAX   )**2                  )
     &                        / ( 4.d0 * DBLE ( JMAX   )**2 - 1.d0 )  )
            EPSP     = SQRT (   ( DBLE ( JMAX+1 )**2                  )
     &                        / ( 4.d0 * DBLE ( JMAX+1 )**2 - 1.d0 )  )
*
            QDPN      =   QPN  ( JMAX+1 ) * DBLE ( JMAX   ) * EPSP
     &                  - QPN  ( JMAX-1 ) * DBLE ( JMAX+1 ) * EPS
*
*"         < 4. solve by Newton method >
*
            DELTX = QPN ( JMAX ) / QDPN * ( 1.d0 - X0 **2 )
            X0    = X0 + DELTX
*
            IF ( ABS( DELTX ) .LT. DELTP ) GOTO 4200
*"                                             >---- exit ----->
 4100           CONTINUE
         WRITE ( 6,* ) ' error in GAUSS(4) ', DELTX
*
*"                                             <---- exit -----<
 4200        CONTINUE
*
         CTHETA ( J ) = X0
*
*"         < 5. Gaussian weight >
*
         GW     ( J ) =    ( DBLE ( 2*JMAX ) - 1.d0  )
     &                   * ( 1.d0  - X0 **2 )
     &                   / (   DBLE ( JMAX ) * QPN ( JMAX-1 )  )**2
*
 5100     CONTINUE
*
      DO 6100 J = 1, JMAX/2
         GW     ( JMAX-J+1 ) =   GW     ( J )
         CTHETA ( JMAX-J+1 ) = - CTHETA ( J )
 6100     CONTINUE
*
      RETURN
      END
