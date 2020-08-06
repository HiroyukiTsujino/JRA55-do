!-*-F90-*-
subroutine lsr( x, y, num, nmax, a, b )

  !  Least Square Regression : y = a * x + b

  integer(4) :: num, nmax
  real(4) :: a, b
  real(8) :: x(nmax), y(nmax)
  real(8) :: sumx, sumx2, sumy, sumxy
  real(8) :: a8, b8
!
  if( num == 0 ) then
    write(6,*) 'Error ! num=',num
    stop
  endif
! ------------------------------
!    Summation
! ------------------------------
  sumx  = 0.0
  sumx2 = 0.0
  sumy  = 0.0
  sumxy = 0.0
  do j = 1, num
    sumx  = sumx  + x(j)
    sumx2 = sumx2 + x(j)*x(j)
    sumy  = sumy  + y(j)
    sumxy = sumxy + x(j)*y(j)
  end do

! ------------------------------
!    Slope 
! ------------------------------
  a8 = ( float(num)*sumxy - sumx*sumy )/ &
         &         ( float(num)*sumx2 - sumx*sumx )
! ------------------------------
!    Intercept
! ------------------------------
  b8 = ( sumy - a8*sumx )/float(num)
!
  a = a8
  b = b8
!
end subroutine lsr
