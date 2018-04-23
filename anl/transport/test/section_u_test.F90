! -*-F90-*-
module section_u_test
  use section_u
  use libmxe_ut
  use fruit, only: assert_equals
  implicit none
  private


  character(*),parameter,public :: testname='section_u'
  public :: ini_test
  public :: test_calc


  integer,parameter :: lun_ut = 20
  integer           :: ios


contains


subroutine ini_test
  implicit none

  call ini
  call libmxe_ut__open_namelist( lun_ut )

end subroutine ini_test


subroutine test_calc
  implicit none

  real(8) :: d

  namelist /section_u_calc/ dval, drange

  call libmxe_ut__clear
  rewind( lun_ut )
  read( lun_ut, nml=section_u_calc, iostat=ios )
  if ( ios/=0 ) return

  call calc
  d = get_result(1)
  call assert_equals( dval, d, drange, 'calc' )

end subroutine test_calc


end module section_u_test
