!-*-F90-*-

function distance_rad(lon0,lat0,lon1,lat1)

  implicit none

  real(8) :: distance_rad
  real(8),intent(in) :: lon0,lat0,lon1,lat1
  real(8),parameter :: pi = 3.141592653589793d0
  real(8),parameter :: radian_r = pi / 180.D0

  real(8) :: x0,y0,x1,y1

  !-- degree to radian --
  x0 = lon0 * radian_r
  y0 = lat0 * radian_r
  x1 = lon1 * radian_r
  y1 = lat1 * radian_r

  !-- great circle in radian --
  
  if ((x0 == x1) .and. (y0 == y1)) then
    distance_rad=0.0d0
  else
    distance_rad = acos( cos(y0)*cos(y1)*cos(x1-x0) &
         + sin(y0)*sin(y1) )
  end if
  
end function distance_rad
