      MODULE dimmodule
      IMPLICIT NONE
      SAVE
!        dimension of JRA-55 grid (TL319)
!            (edit these parameters)
      INTEGER, PARAMETER ::  id=640, jd=320 
!
!        dimension of RIVER grid (0.25 deg)
!            (edit these parameters)
      INTEGER, PARAMETER ::  nx=1440, ny=720
      REAL(8), PARAMETER :: rdy=0.25D0, rdx=0.25D0
!     
!        physical parameters
      REAL(8), PARAMETER :: 
     &    pi    = 3.1415926535897932384626433832795D0, 
     &    qi    = pi / 180.D0,
     &    radius= 6370.D5,
     &    deg1  = 6370.D5 * 2.D0 * PI / 360.D0,
     &    omega = 2.D0 * pi / 8.64D4
!

      END MODULE dimmodule
      
      

