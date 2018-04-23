! -*-F90-*-
module snap_test
  use snap
  use libmxe_ut
  use fruit, only: assert_equals
  implicit none
  private


  character(*),parameter,public :: testname='snap'
  public :: ini_test
  public :: test_convert

  integer,parameter :: lun=20
  integer :: i


contains


subroutine ini_test
  implicit none

  call ini
  call convert

end subroutine ini_test


subroutine test_convert
  implicit none

  real :: f

  namelist /snap_convert/ rval, rrange, nx, ny

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
    read(lun,nml=snap_convert,iostat=i)
  close(lun)
  if ( i/=0 ) return

  f = get_result(nx,ny)

  call assert_equals( rval, f, rrange, 'convert' )

end subroutine test_convert


end module snap_test
