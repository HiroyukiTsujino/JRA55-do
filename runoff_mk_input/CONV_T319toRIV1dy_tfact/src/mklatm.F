      SUBROUTINE MKLATM    !! latitude mid-points
     O        ( ALATM  ,
     I           DLAT  ,
     D           JMAX    )
      implicit none
*
*   [PARAMETER]
      INTEGER    JMAX
*
*   [OUTPUT]
      REAL*8     ALATM  ( JMAX+1 )        !! latitude mid-points
*
*   [INPUT]
      REAL*8     DLAT   ( JMAX )     !! latitudinal differ.
*
*    [INTERNAL WORK]
      INTEGER    J, IJ
*
      ALATM( 1 ) = 1.D0
      DO J = 1, JMAX/2
         ALATM( J+1 ) = ALATM( J ) - DLAT( J ) * 2.D0
      ENDDO
      ALATM( JMAX/2+1 ) = 0.D0
      DO J = 1, JMAX/2
         ALATM( JMAX-J+2 ) = - ALATM( J )
      ENDDO
      DO J = 1, JMAX+1
         ALATM( J ) = ASIN( ALATM( J ) )
      ENDDO
*
      RETURN
      END
