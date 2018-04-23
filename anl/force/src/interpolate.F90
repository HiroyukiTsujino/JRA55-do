! -*-F90-*-
!-- Interpolate based on remap.dat.
module interpolate
  use libmxe_para, only: type_libmxe_para, clen
  use force_data,  only: type_force_data
  implicit none
  private


  character(clen),save :: file_base_out
  character(clen),save :: file_remap
  integer,save         :: n_elm = 1
  integer,save         :: nrec_first = 1 !- first record
  integer,save         :: nrec_last = 0  !- last record [default:io%nm]

  public :: ini
  public :: calc
  public :: get_result
  public :: write_result
  public :: next
  public :: has_next

  integer, parameter       :: lun = 77

  integer,save             :: im, jm, km, reclen, nrec
  integer,save             :: nlink
  integer,allocatable,save :: isrc(:), idst(:)
  real(8),allocatable,save :: wgt(:)
  real(8),allocatable      :: ddst(:)
  real(4),allocatable      :: dsrc(:)
  logical,allocatable,save :: l_def(:)

  type(type_libmxe_para),save :: para
  type(type_force_data),save  :: force


contains


subroutine next
  implicit none

  nrec = nrec + 1
 
end subroutine next


logical function has_next
  implicit none

  if ( nrec > nrec_last ) then
    has_next = .false.
  else
    has_next = .true.
  endif

end function has_next


subroutine ini
  use libmxe_para, only: libmxe_para__register
  use force_data,  only: force_data__register
  implicit none

  integer :: i, j, n
  integer,allocatable :: nsrc(:)

  character(clen) :: file_data, file_data_grid
  integer :: num_elm
  integer :: interval_ical(6), first_ical(6), last_ical(6)
  logical :: l_leap_year

  namelist /nml_force_data/ file_data, file_data_grid, &
                         &  im, jm, num_elm, interval_ical, &
                         &  first_ical, last_ical, &
                         &  l_leap_year, km

  namelist /nml_interpolate/ file_base_out, file_remap, n_elm, &
                         &   nrec_first, nrec_last



  read( 5, nml=nml_interpolate, iostat=i )
  if ( i /= 0 ) then
    write(*,*) ' Read error : nml_interpolate'
    stop
  endif

  l_leap_year = .false.
  km = 1
  rewind( 5 )
  read( 5, nml=nml_force_data, iostat=i )
  if ( i /= 0 ) then
    write(*,*) ' Read error : nml_force_data'
    stop
  endif
  call force_data__register( force, file_data, file_data_grid, &
                           & im, jm, num_elm, interval_ical, &
                           &  first_ical, last_ical, l_leap_year, km=km )
  call libmxe_para__register(para)
  im = para%imut
  jm = para%jmut
!  km = force%km  !- already in
  reclen = im * jm * km * 4

  !-- read remap table --
  open(lun,file=trim(file_remap),form='unformatted',access='sequential')
    write(6,*) 'Read remapping table from ', trim(file_remap)
    read(lun)
    read(lun)
    read(lun)
    read(lun)
    read(lun)
    read(lun)
    read(lun) i, nlink
    write(6,*) 'There are ', nlink, ' links '
    allocate(isrc(nlink),idst(nlink))
    allocate(wgt(nlink))
    read(lun) isrc(1:nlink)
    read(lun) idst(1:nlink)
    read(lun) wgt(1:nlink)
  close(lun)

  allocate( dsrc(force%im*force%jm*force%km*force%num_elm) )
  allocate( ddst(im*jm*km) )

  !- l_def:  .T. interpolated grid, .F. not
  allocate( l_def(im*jm*km) )
  allocate( nsrc(im*jm*km) )
  l_def(:) = .true.
  nsrc(:)  = 0
  do n = 1, nlink
    j = idst(n)
    nsrc(j) = nsrc(j) + 1
  enddo
  do i = 1, im*jm*km
    if ( nsrc(i) == 0 ) l_def(i) = .false.
  enddo
  deallocate( nsrc )

  if ( nrec_last==0 ) nrec_last = force%io%nm
  nrec = nrec_first

end subroutine ini


subroutine calc
  use force_data, only: force_data__read_data
  implicit none

  integer :: i, j, n

  if ( mod(nrec,100) == 0 ) write(*,*) nrec

  call force_data__read_data( force, nrec, dsrc )

  ddst(:) = 0.d0

  do n = 1, nlink
    i = isrc(n) + force%im*force%jm*force%km*(n_elm-1)
    j = idst(n)
    ddst(j) = ddst(j) + dble(dsrc(i)) * wgt(n)
  enddo

  do i = 1, im*jm*km
    if ( .not. l_def(i) ) ddst(i) = para%rundefout
  enddo

end subroutine calc


subroutine write_result
  use libmxe_io, only: libmxe_io__open
  implicit none

  call libmxe_io__open( force%io, trim(file_base_out), &
                     &  nrec, reclen, lun, action='write')
    write(lun,rec=1) real(ddst(:))
  close(lun)

end subroutine write_result


real(4) function get_result( i, j, k )
  implicit none

  integer,intent(in)      :: i, j
  integer,intent(in),optional :: k

  integer :: ktemp

  if ( present(k) ) then
    ktemp = k
  else
    ktemp = 1
  endif

  get_result = real( ddst( (ktemp-1)*im*jm + (j-1)*im + i ) )

end function get_result


end module interpolate
