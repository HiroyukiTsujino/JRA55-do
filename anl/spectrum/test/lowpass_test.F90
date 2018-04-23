! -*-F90-*-
module lowpass_test
  use lowpass
  use libmxe_ut
  use fruit, only: assert_equals
  use libmxe_para, only: type_libmxe_para, clen
  use libmxe_io,   only: type_libmxe_io
  implicit none
  private


  character(*),parameter,public :: testname='lowpass'
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

  namelist /lowpass__calc/ dval, drange, nx

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
  read( lun, nml=lowpass__calc, iostat=i )
  if ( i/=0 ) then
    close( lun )
    return
  endif

  call calc

  do while ( i==0 )
    d = get_result(nx)
    call assert_equals (dval, d, drange, 'calc')
    read( lun, nml=lowpass__calc, iostat=i )
  enddo

  close( lun )

end subroutine test_calc


end module lowpass_test
