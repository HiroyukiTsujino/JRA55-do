! -*-F90-*-
module runmean_test
  use runmean
  use libmxe_ut
  use fruit, only: assert_equals
  implicit none
  private


  character(*),parameter,public :: testname='runmean'
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

  namelist /runmean_calc/ rval, rrange, nx, ny, nz

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
    read(lun,nml=runmean_calc,iostat=i)
  close(lun)
  if ( i/=0 ) return

  call calc
  f = get_result(nx,ny,nz)

  call assert_equals( rval, f, rrange, 'calc' )

end subroutine test_calc


end module runmean_test
