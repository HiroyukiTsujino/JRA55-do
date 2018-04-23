! -*-F90-*-
module interpolate_test
  use interpolate
  use libmxe_ut
  use fruit, only: assert_equals
  implicit none
  private


  character(*),parameter,public :: testname='interpolate'
  public :: ini_test
  public :: test_calc

  integer,parameter :: lun=20
  integer           :: i


contains


subroutine ini_test
  implicit none

  call ini
  call calc

end subroutine ini_test


subroutine test_calc
  implicit none

  real(4) :: d

  namelist /interpolate_calc/ rval, rrange, nx, ny, nz

  call libmxe_ut__clear
  nz = 1
  call libmxe_ut__open_namelist( lun )
  read( lun, nml=interpolate_calc, iostat=i )

  do while ( i==0 )
    d = get_result( nx, ny, nz )
    call assert_equals( rval, d, rrange, 'calc' )
    nz = 1
    read( lun, nml=interpolate_calc, iostat=i )
  enddo

  close( lun )

end subroutine test_calc


end module interpolate_test
