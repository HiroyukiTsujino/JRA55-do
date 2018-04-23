! -*-F90-*-
module barotropic_ke_vol_test
  use barotropic_ke_vol
  use libmxe_ut
  use fruit, only: assert_equals
  implicit none
  private


  character(*),parameter,public :: testname='barotropic_ke_vol'
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

  real(8) :: d

  namelist /barotropic_ke_vol_calc/ dval, drange

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
    read(lun,nml=barotropic_ke_vol_calc,iostat=i)
  close(lun)
  if ( i/=0 ) return

  call calc
  d = get_result(1)
  call assert_equals (dval, d, drange, 'calc')

end subroutine test_calc


end module barotropic_ke_vol_test
