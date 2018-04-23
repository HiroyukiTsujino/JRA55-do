! -*-F90-*-
module integ_vert_test
  use integ_vert
  use libmxe_ut
  use fruit, only: assert_equals
  implicit none
  private


  character(*),parameter,public :: testname='integ_vert'
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

  namelist /integ_vert__calc/ dval, drange, nx, ny

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
  read( lun, nml=integ_vert__calc, iostat=i )

  if ( i/=0 ) then
    close( lun )
    return
  endif

  call calc

  do while ( i==0 )
    d = get_result(nx,ny)
    call assert_equals (dval, d, drange, 'calc')
    read( lun, nml=integ_vert__calc, iostat=i )
  enddo

  close( lun )

end subroutine test_calc


end module integ_vert_test
