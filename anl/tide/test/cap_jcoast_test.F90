! -*-F90-*-
module cap_jcoast_test
  use cap_jcoast
  use libmxe_ut
  use fruit, only: assert_equals
  implicit none
  private


  character(*),parameter,public :: testname='cap_jcoast'
  public :: ini_test
  public :: test_calc

  integer,parameter :: lun=20
  integer :: i

contains


subroutine ini_test
  implicit none

  call ini
  call calc

end subroutine ini_test


subroutine test_calc
  implicit none

  real(4) :: r

  namelist /cap_jcoast_calc/ rval, rrange, nx

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
    read(lun,nml=cap_jcoast_calc,iostat=i)
  close(lun)
  if ( i/=0 ) return

  r = get_result( nx )
  call assert_equals( rval, r, rrange, 'calc(cap)')

end subroutine test_calc


end module cap_jcoast_test
