! -*-F90-*-
module location
 implicit none
 private


  public :: location__seek_grid_curv


contains 


!-----------------------------------------------------------------
subroutine location__seek_grid_curv( im, jm, x, y, target_x, target_y, &
                                   & target_i, target_j, initial_ij )
  implicit none

  integer,intent(in)  :: im, jm
  real(8),intent(in)  :: x(im+1,jm+1), y(im+1,jm+1)
  real(8),intent(in)  :: target_x, target_y
  integer,intent(out) :: target_i, target_j
  integer,intent(in),optional  :: initial_ij(2)

  integer :: i, j, k, imove, jmove
  real(8) :: left_right_S, left_right_E,  left_right_N, left_right_W  

  if ( present(initial_ij) ) then
    i = initial_ij(1)
    j = initial_ij(2)
  else
    i = im / 2
    j = jm / 2
  endif

  target_i = 0
  target_j = 0

  do k = 1, im + jm

    imove = 0
    jmove = 0

    !-  |    |
    !- -O<-3-O-
    !-  |    ^
    !-  4    |
    !-  |  * 2
    !-  v    |
    !- -O-1->O-
    !-  |    |  
    !-         If *(xp,yp) is in the inside of the box (i:i+1,j:j+1),
    !-           it is placed in the left side of all the 4 vectors

    left_right_S = left_right( x(i  ,j  ), y(i  ,j  ), x(i+1,j  ), y(i+1,j  ), target_x, target_y )
    left_right_E = left_right( x(i+1,j  ), y(i+1,j  ), x(i+1,j+1), y(i+1,j+1), target_x, target_y )
    left_right_N = left_right( x(i+1,j+1), y(i+1,j+1), x(i  ,j+1), y(i  ,j+1), target_x, target_y ) 
    left_right_W = left_right( x(i  ,j+1), y(i  ,j+1), x(i  ,j  ), y(i  ,j  ), target_x, target_y ) 

    if ( left_right_S < 0.d0 ) then
      jmove = -1
    endif
    if ( left_right_E < 0.d0 ) then
      imove = 1
    endif
    if ( left_right_N < 0.d0 ) then
      jmove = 1
    endif
    if ( left_right_W < 0.d0 ) then
      imove = -1
    endif

    if ( imove == 0 .and. jmove == 0 ) then
      target_i = i
      target_j = j
      exit !- succeed
    endif

    if ( ( i + imove < 1 ) .or. ( i + imove >= im+1 ) ) then
      if ( j == 1 .or. j == jm .or. jmove == 0 ) then
        exit !- fail
      else
        imove = 0
      endif
    endif

    if ( ( j + jmove < 1 ).or.( j + jmove >= jm+1 ) ) then
      if ( i == 1 .or. i == im .or. imove == 0 ) then
        exit !- fail
      else
        jmove = 0
      endif
    endif

    i = i + imove
    j = j + jmove

  enddo

end subroutine location__seek_grid_curv


!- Is point(xp,yp) located at left of vector(0=>1) (1), or right (-1)?
function left_right( x0, y0, x1, y1, xp, yp )

  real(8) :: left_right

  real(8),intent(in) :: x0, y0, x1, y1, xp, yp

  left_right = ( x1 - x0 )*( yp - y0 ) - ( y1 - y0 )*( xp - x0 )

end function left_right

end module location
