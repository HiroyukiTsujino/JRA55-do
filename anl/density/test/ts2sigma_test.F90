! -*-F90-*-
module ts2sigma_test
  use ts2sigma
  use libmxe_ut
  use fruit, only: assert_equals
  implicit none
  private


  character(*),parameter,public :: testname='ts2sigma'
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

  real :: sigma_kgpm3

  namelist /ts2sigma_calc/ rval, rrange, nx, ny, nz

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
    read(lun,nml=ts2sigma_calc,iostat=i)
  close(lun)
  if ( i/=0 ) return

  call calc
  sigma_kgpm3 = get_result(nx,ny,nz)

  call assert_equals( rval, sigma_kgpm3, rrange, 'calc' )

end subroutine test_calc


end module ts2sigma_test
