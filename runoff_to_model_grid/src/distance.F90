! -*-F90-*-
!- Calculate distance between (lon0,lat0) and (lon1,lat1).
module distance
 implicit none
 private


  public :: distance_rad
    !- Calculate great circle in radian.


contains 


!-----------------------------------------------------------------
function distance_rad(lon0,lat0,lon1,lat1)
 implicit none

  real(8) :: distance_rad
  real(8),intent(in) :: lon0,lat0,lon1,lat1
  real(8),parameter :: pi = 3.14159265358979323846264338327950
  real(8),parameter :: radian_r = pi / 180.d0
  real(8) :: x0,y0,x1,y1


  !-- degree to radian --
  x0 = lon0 * radian_r
  y0 = lat0 * radian_r
  x1 = lon1 * radian_r
  y1 = lat1 * radian_r


  !-- great circle in radian --
  distance_rad = acos( min( 1.d0, cos(y0)*cos(y1)*cos(x1-x0) + sin(y0)*sin(y1) ) )
  !- min(1.d0,): avoid NaN if x0=x1 and y0=y1

end function distance_rad


end module distance
