! -*-F90-*-
module btro_vector_test
  use btro_vector
  use libmxe_ut
  use fruit,       only: assert_equals
  use libmxe_para, only: type_libmxe_para, clen
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo
  use libmxe_io,   only: type_libmxe_io
  implicit none
  private


  character(*),parameter,public :: testname='btro_vector'
  public :: ini_test
  public :: test_divergence

  character(clen),save :: file_base_um  !- barotropic transport (zonal)
  character(clen),save :: file_base_vm  !-     (meridional)


  integer,parameter           :: lun=20

  type(type_libmxe_para),save :: para
  type(type_libmxe_grid),save :: grid
  type(type_libmxe_topo),save :: topo
  type(type_libmxe_io)  ,save :: io

  integer                     :: i,im,jm
  real(8),allocatable         :: um(:,:),vm(:,:),d8(:,:)

contains


subroutine ini_test
  use libmxe_para, only: libmxe_para__register
  use libmxe_grid, only: libmxe_grid__register
  use libmxe_topo, only: libmxe_topo__register, &
                       & libmxe_topo__aexl
  use libmxe_io,   only: libmxe_io__register,   &
                       & libmxe_io__open
  implicit none

  real,allocatable :: r4(:,:)
  integer          :: reclen

  namelist /nml_btro_vector/ file_base_um, file_base_vm

  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)
  call libmxe_topo__aexl(topo,para)
  call libmxe_io__register(io,para)

  read(5,nml=nml_btro_vector,iostat=i)
  if ( i /= 0 ) then
    write(*,*) ' Read error : namelist'
    stop
  endif

  im = para%imut
  jm = para%jmut
  reclen = im*jm*4

  allocate( r4(im,jm), um(im,jm), vm(im,jm) )
  allocate( d8(im,jm) )

  call libmxe_io__open( io, trim(file_base_um), 1, reclen, &
                      & lun, action='read' )
  read(lun,rec=1) r4
  close(lun)
  um(:,:) = dble( r4(:,:) )

  call libmxe_io__open( io, trim(file_base_vm), 1, reclen, &
                      & lun, action='read' )
  read(lun,rec=1) r4
  close(lun)
  vm(:,:) = dble( r4(:,:) )

  deallocate( r4 )

end subroutine ini_test


subroutine test_divergence
  use libmxe_io, only: libmxe_io__open
  implicit none

  namelist /btro_vector_divergence/ rval, rrange, nx, ny

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
  read( lun, nml=btro_vector_divergence, iostat=i )

  call btro_vector__divergence( um, vm, para, grid, topo, d8)

  do while ( i==0 )

    call assert_equals( rval, real(d8(nx,ny)), rrange, &
                      & 'divergence')
    read( lun, nml=btro_vector_divergence, iostat=i )

  enddo

  close(lun)

end subroutine test_divergence


end module btro_vector_test
