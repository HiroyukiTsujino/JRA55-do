! -*-F90-*-
module trim_hs_test
  use trim_hs
  use libmxe_ut
  use fruit, only: assert_equals
  implicit none
  private

  character(*),parameter,public :: testname='trim_hs'

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

  real(4) :: r

  namelist /trim_hs__calc/ rval, rrange, nx, ny, nz

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
  read( lun, nml=trim_hs__calc, iostat=i )
  if ( i/=0 ) then
    close( lun )
    return
  endif

  call calc

  do while ( i==0 )
    r = get_result(nx,ny,nz)
    call assert_equals (rval, r, rrange, 'calc')
    read( lun, nml=trim_hs__calc, iostat=i )
  enddo

  close( lun )

end subroutine test_calc


end module trim_hs_test
