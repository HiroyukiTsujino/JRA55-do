! -*-F90-*-
module energy_test
  use energy
  use libmxe_ut
  use fruit, only: assert_equals
  implicit none
  private


  character(*),parameter,public :: testname='energy'
  public :: ini_test
  public :: test_w
  public :: test_px
  public :: test_py
  public :: test_nablap


  integer,parameter :: lun=20
  integer :: i


contains


subroutine ini_test
  implicit none

  call ini
  call calc

end subroutine ini_test


subroutine test_w
  implicit none

  real :: f

  namelist /energy_w/ rval, rrange, nx, ny

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
    read(lun,nml=energy_w,iostat=i)
  close(lun)
  if ( i/=0 ) return

  f = get_result(nx,ny,'w')

  call assert_equals( rval, f, rrange, 'W[e-3 W/m2](g/s3)' )

end subroutine test_w


subroutine test_px
  implicit none

  real :: f

  namelist /energy_px/ rval, rrange, nx, ny

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
    read(lun,nml=energy_px,iostat=i)
  close(lun)
  if ( i/=0 ) return

  f = get_result(nx,ny,'px')

  call assert_equals( rval, f, rrange, 'Px[e-5 W/m](cm g/s3)' )

end subroutine test_px


subroutine test_py
  implicit none

  real :: f

  namelist /energy_py/ rval, rrange, nx, ny

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
    read(lun,nml=energy_py,iostat=i)
  close(lun)
  if ( i/=0 ) return

  f = get_result(nx,ny,'py')

  call assert_equals( rval, f, rrange, 'Py[e-5 W/m](cm g/s3)' )

end subroutine test_py


subroutine test_nablap
  implicit none

  real :: f

  namelist /energy_nablap/ rval, rrange, nx, ny

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
    read(lun,nml=energy_nablap,iostat=i)
  close(lun)
  if ( i/=0 ) return

  f = get_result(nx,ny,'nablap')

  call assert_equals( rval, f, rrange, 'nablaP[e-3 W/m2](g/s3)' )

end subroutine test_nablap


end module energy_test
