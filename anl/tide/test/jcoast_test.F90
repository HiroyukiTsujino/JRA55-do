! -*-F90-*-
module jcoast_test
  use jcoast
  use libmxe_ut
  use fruit, only: assert_equals
  implicit none
  private


  character(*),parameter,public :: testname='jcoast'
  public :: ini_test
  public :: test_read

  character(clen),save :: filei  !- input file

  integer,parameter :: lun=20
  integer :: i

contains


subroutine ini_test
  implicit none

  namelist /jcoast_lst/ filei

  read(5,nml=jcoast_lst,iostat=i)
  if ( i /= 0 ) then
    write(*,*) ' Read error : namelist'
    stop
  endif

end subroutine ini_test


subroutine test_read
  implicit none

  real(4) :: r(njcoast)

  namelist /jcoast_read/ rval, rrange, nx

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
    read(lun,nml=jcoast_read,iostat=i)
  close(lun)
  if ( i/=0 ) return

  call jcoast__read( filei, r )
  call assert_equals( rval, r(nx), rrange, 'read')

end subroutine test_read


end module jcoast_test
