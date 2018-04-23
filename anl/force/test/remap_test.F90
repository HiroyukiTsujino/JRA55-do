! -*-F90-*-
module remap_test
  use remap
  use libmxe_ut
  use fruit, only: assert_equals
  implicit none
  private


  character(*),parameter,public :: testname='remap'
  public :: ini_test
  public :: test_nlink
  public :: test_isrc
  public :: test_idst

  integer,parameter :: lun=20
  integer           :: i


contains


subroutine ini_test
  implicit none

  call ini
  call calc

end subroutine ini_test


subroutine test_nlink
  implicit none

  integer :: nlink

  namelist /remap_nlink/ ival

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
  read( lun, nml=remap_nlink, iostat=i )

  if ( i==0 ) then
    nlink = get_result( 1, 'nlink' )
    call assert_equals (ival, nlink, 'nlink')
  endif

  close( lun )

end subroutine test_nlink


subroutine test_isrc
  implicit none

  integer :: isrc

  namelist /remap_isrc/ ival, nx

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
  read( lun, nml=remap_isrc, iostat=i )

  do while ( i==0 )
    isrc = get_result( nx, 'isrc' )
    call assert_equals (ival, isrc, 'isrc')
    read( lun, nml=remap_isrc, iostat=i )
  enddo

  close( lun )

end subroutine test_isrc


subroutine test_idst
  implicit none

  integer :: idst

  namelist /remap_idst/ ival, nx

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
  read( lun, nml=remap_idst, iostat=i )

  do while ( i==0 )
    idst = get_result( nx, 'idst' )
    call assert_equals (ival, idst, 'idst')
    read( lun, nml=remap_idst, iostat=i )
  enddo

  close( lun )

end subroutine test_idst


end module remap_test
