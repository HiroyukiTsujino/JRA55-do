! -*-F90-*-
module remap_test
  use remap
  use libmxe_ut
  use fruit, only: assert_equals
  implicit none
  private


  character(*),parameter,public :: testname='remap'
  public :: ini_test
  public :: test


  integer,parameter  :: lun = 20
  integer            :: i


contains


subroutine ini_test
  implicit none

  call ini
  call calc

end subroutine ini_test


subroutine test
  implicit none

  real(8) :: d

  namelist /remap/ dval, drange, nx, ny, nz

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )

  read(lun,nml=remap,iostat=i)
  do while ( i == 0 )

    d = get_result( nx, ny, nz )
    call assert_equals( dval, d, drange, 'result' )

    read(lun,nml=remap,iostat=i)
  enddo
  close(lun)

end subroutine test


end module remap_test
