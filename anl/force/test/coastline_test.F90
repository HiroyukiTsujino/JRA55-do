! -*-F90-*-
module coastline_test
  use coastline
  use libmxe_ut
  use fruit, only: assert_equals
  use libmxe_para, only: type_libmxe_para
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo
  implicit none
  private


  character(*),parameter,public :: testname='coastline'
  public :: ini_test
  public :: test_seek_nearest_sea
  public :: test_seek_nearest_sea2

  integer,parameter :: lun=20
  integer           :: ios

  type(type_libmxe_para),save :: para
  type(type_libmxe_grid),save :: grid
  type(type_libmxe_topo),save :: topo


contains


subroutine ini_test
  use libmxe_para, only: libmxe_para__register
  use libmxe_grid, only: libmxe_grid__register
  use libmxe_topo, only: libmxe_topo__register, libmxe_topo__aexl
  implicit none

  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)
  call libmxe_topo__aexl(topo,para)

end subroutine ini_test


subroutine test_seek_nearest_sea
  implicit none

  integer :: i, j, ios2
  real(8) :: lon, lat

  namelist /nml_seek_nearest_sea/ lon, lat
  namelist /coastline_seek_nearest_sea/ ival, nz

  rewind( 5 )

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
  read( lun, nml=coastline_seek_nearest_sea, iostat=ios )
  read( 5, nml=nml_seek_nearest_sea, iostat=ios2 )

  do while ( ( ios==0 ).and.( ios2==0 ) )
    call coastline__seek_nearest_sea( para, grid, topo, lon, lat, nz, 'T', i, j )
    call assert_equals (ival, i, 'seek_nearest_sea')
    read( lun, nml=coastline_seek_nearest_sea, iostat=ios )
    read( 5, nml=nml_seek_nearest_sea, iostat=ios2 )
  enddo

  close( lun )

end subroutine test_seek_nearest_sea


subroutine test_seek_nearest_sea2
  implicit none

  integer :: i, j, ios2, im, jm
  real(8) :: lon, lat
  real(8),allocatable :: x(:), y(:)
  integer,allocatable :: sea_index(:,:)

  namelist /nml_seek_nearest_sea/ lon, lat
  namelist /coastline_seek_nearest_sea/ ival, nz

  im = para%imut
  jm = para%jmut

  allocate( x(im) )
  allocate( y(jm) )
  allocate( sea_index(im,jm) )

  x = grid%lont
  y = grid%latt

  rewind( 5 )

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
  read( lun, nml=coastline_seek_nearest_sea, iostat=ios )
  read( 5, nml=nml_seek_nearest_sea, iostat=ios2 )

  do while ( ( ios==0 ).and.( ios2==0 ) )
    sea_index = int( topo%atexl(:,:,nz) )
    call coastline__seek_nearest_sea2( im, jm, x, y, sea_index, lon, lat, i, j )
    call assert_equals (ival, i, 'seek_nearest_sea2')
    read( lun, nml=coastline_seek_nearest_sea, iostat=ios )
    read( 5, nml=nml_seek_nearest_sea, iostat=ios2 )
  enddo

  close( lun )

  deallocate( x, y, sea_index )

end subroutine test_seek_nearest_sea2


end module coastline_test
