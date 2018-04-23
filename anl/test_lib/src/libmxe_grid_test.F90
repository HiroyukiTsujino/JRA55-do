! -*-F90-*-
module libmxe_grid_test
  use libmxe_para
  use libmxe_grid
  use libmxe_ut
  use fruit, only: assert_equals
  implicit none
  private


  character(*),parameter,public :: testname='libmxe_grid'
  public :: ini_test
  public :: test_dz_cm


  type(type_libmxe_para),save :: para
  type(type_libmxe_grid),save :: grid

  integer,parameter :: lun=20
  integer :: i


contains


subroutine ini_test
  implicit none

  call libmxe_para__register( para )
  call libmxe_grid__register( grid, para )

end subroutine ini_test


subroutine test_dz_cm
  implicit none

  namelist /grid_dz_cm/ dval, drange, nz

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
  read( lun, nml=grid_dz_cm, iostat=i )

  do while ( i==0 )
    call assert_equals( dval, grid%dz_cm(nz), drange, 'dz_cm' )
    read( lun, nml=grid_dz_cm, iostat=i )
  enddo

  close(lun)

end subroutine test_dz_cm


end module libmxe_grid_test
