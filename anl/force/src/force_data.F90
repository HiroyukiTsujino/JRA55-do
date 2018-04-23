! -*-F90-*-
!- forcing data
module force_data
  use libmxe_io,       only: type_libmxe_io
  implicit none
  private

  type,public :: type_force_data
    character(256)  :: file_data
    character(256)  :: file_data_grid
    integer         :: im
    integer         :: jm
    integer         :: km
    integer         :: num_elm
    type(type_libmxe_io) :: io

#ifdef MXE_F2003
    real(8),allocatable :: lon(:)
    real(8),allocatable :: lat(:)
#else /* MXE_F2003 */
    real(8),pointer :: lon(:)
    real(8),pointer :: lat(:)
#endif /* MXE_F2003 */
  end type

  public :: force_data__register
  public :: force_data__read_data

  integer,parameter :: lun = 31

contains 


subroutine force_data__register( force, file_data, file_data_grid, &
                               &  im, jm, num_elm, interval_ical, &
                               &  first_ical, last_ical, l_leap_year, &
                               &  km )

  use libmxe_calendar, only: type_calendar, libmxe_calendar__intarr2cal
  use libmxe_io,       only: libmxe_io__set_calendar
  implicit none

  type(type_force_data),intent(inout) :: force
  character(*),intent(in)             :: file_data
  character(*),intent(in)             :: file_data_grid
  integer,intent(in)                  :: im
  integer,intent(in)                  :: jm
  integer,intent(in)                  :: num_elm
  integer,intent(in)                  :: interval_ical(6)
  integer,intent(in)                  :: first_ical(6)
  integer,intent(in)                  :: last_ical(6)
  logical,intent(in)                  :: l_leap_year
  integer,intent(in),optional         :: km

  type(type_calendar) :: first_cal, last_cal, interval_cal

  force%file_data      = file_data
  force%file_data_grid = file_data_grid
  force%im             = im
  force%jm             = jm
  force%num_elm        = num_elm

  allocate( force%lon(1:force%im) )
  allocate( force%lat(1:force%jm) )
  open(lun,file=file_data_grid,form='unformatted')
    read(lun) force%lon, force%lat
  close(lun)

  first_cal    = libmxe_calendar__intarr2cal( first_ical )
  last_cal     = libmxe_calendar__intarr2cal( last_ical )
  interval_cal = libmxe_calendar__intarr2cal( interval_ical )

  force%io%l_leap_year = l_leap_year
  call libmxe_io__set_calendar( force%io, first_cal, last_cal, &
                              & interval_cal )
    !- io%nm is set.
  force%io%ldef = .true.

  if ( present(km) ) then
    force%km = km
  else
    force%km = 1
  endif

end subroutine force_data__register


subroutine force_data__read_data( force, nrec, d )
  implicit none

  type(type_force_data),intent(in) :: force
  integer,intent(in)               :: nrec
  real(4),intent(out) :: d(force%im,force%jm,force%km,force%num_elm)

  open( lun, file=trim(force%file_data), form='unformatted', &
      & access='direct', recl=force%im*force%jm *force%km *force%num_elm *4 )
  read( lun, rec=nrec ) d
  close( lun )

end subroutine force_data__read_data


end module force_data
