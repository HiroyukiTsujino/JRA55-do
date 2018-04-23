! -*-F90-*-
module force_data_test
  use force_data
  use libmxe_ut
  use fruit, only: assert_equals
  implicit none
  private


  character(*),parameter,public :: testname='force_data'
  public :: ini_test
  public :: test_lon
  public :: test_io_nm
  public :: test_read_data

  integer,parameter :: lun=20
  integer :: i

  type(type_force_data) :: force


contains


subroutine ini_test
  use libmxe_para, only : clen
  implicit none

  character(clen) :: file_data, file_data_grid
  integer :: im, jm, num_elm
  integer :: interval_ical(6), first_ical(6), last_ical(6)
  logical :: l_leap_year

  namelist /nml_force_data/ file_data, file_data_grid, &
                         &  im, jm, num_elm, interval_ical, &
                         &  first_ical, last_ical, &
                         &  l_leap_year

  l_leap_year = .false.

  read( 5, nml=nml_force_data, iostat=i )
  if ( i/=0 ) then
    write(*,*) 'Error: nml_force_data'
    stop
  endif

  call force_data__register( force, file_data, file_data_grid, &
                           & im, jm, num_elm, interval_ical, &
                           &  first_ical, last_ical, l_leap_year )

end subroutine ini_test


subroutine test_lon
  implicit none

  real(8) :: d

  namelist /force_data_lon/ dval, drange, nx

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
  read( lun, nml=force_data_lon, iostat=i )

  do while ( i==0 )
    d = force%lon(nx)
    call assert_equals( dval, d, drange, 'lon' )
    read( lun, nml=force_data_lon, iostat=i )
  enddo

  close( lun )

end subroutine test_lon


subroutine test_io_nm
  implicit none

  integer :: nm

  namelist /force_data_io_nm/ ival

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
  read( lun, nml=force_data_io_nm, iostat=i )

  if ( i==0 ) then
    nm = force%io%nm
    call assert_equals( ival, nm, 'io_nm')
  endif

  close( lun )

end subroutine test_io_nm


subroutine test_read_data
  implicit none

  real(4),allocatable :: d(:,:,:)

  namelist /force_data_read_data/ rval, rrange, nx, ny, nz

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
  read( lun, nml=force_data_read_data, iostat=i )

  allocate( d(force%im,force%jm,force%num_elm) )

  do while ( i==0 )
    call force_data__read_data( force, 1, d )
    call assert_equals( rval, d(nx,ny,nz), rrange, 'read_data' )
    read( lun, nml=force_data_read_data, iostat=i )
  enddo

  close( lun )
  deallocate( d )

end subroutine test_read_data


end module force_data_test
