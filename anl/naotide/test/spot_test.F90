! -*-F90-*-
module spot_test
  use spot
  use libmxe_ut
  use fruit, only: assert_equals
  implicit none
  private


  character(*),parameter,public :: testname='spot'
  public :: ini_test
  public :: test_calc


  integer,parameter :: lun=20
  integer :: i


contains


subroutine ini_test
  implicit none

  call ini

end subroutine ini_test


subroutine test_calc
  implicit none

  real :: f

  namelist /spot_calc/ rval, rrange, nx

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
    read(lun,nml=spot_calc,iostat=i)
  close(lun)
  if ( i/=0 ) return

  call calc
  f = get_result(nx)

  call assert_equals( rval, f, rrange, 'calc' )

end subroutine test_calc


end module spot_test
