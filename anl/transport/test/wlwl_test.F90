! -*-F90-*-
module wlwl_test
  use wlwl
  use libmxe_ut
  use fruit, only: assert_equals
  implicit none
  private


  character(*),parameter,public :: testname='wlwl'
  public :: ini_test
  public :: test_wlwl
  public :: test_w2


  integer,parameter :: lun=20
  integer :: i


contains


subroutine ini_test
  implicit none

  call ini
  call calc

end subroutine ini_test


subroutine test_wlwl
  implicit none

  real :: f

  namelist /wlwl_wlwl/ rval, rrange, nx, ny, nz

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
    read(lun,nml=wlwl_wlwl,iostat=i)
  close(lun)
  if ( i/=0 ) return

  f = get_wlwl(nx,ny,nz)

  call assert_equals( rval, f, rrange, 'wlwl' )

end subroutine test_wlwl


subroutine test_w2
  implicit none

  real :: f

  namelist /wlwl_w2/ rval, rrange, nx, ny, nz

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
    read(lun,nml=wlwl_w2,iostat=i)
  close(lun)
  if ( i/=0 ) return

  f = get_w2(nx,ny,nz)

  call assert_equals( rval, f, rrange, 'w2' )

end subroutine test_w2


end module wlwl_test
