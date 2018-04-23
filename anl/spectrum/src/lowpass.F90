! -*-F90-*-
!-- lowpass
module lowpass
  use libmxe_para, only: type_libmxe_para, clen
  use libmxe_io,   only: type_libmxe_io
  implicit none
  private


  !-- arguments --
  character(clen),save :: file_base_in  !- input data
  character(clen),save :: file_base_out
  integer,save         :: nfreq_max  !- max frequency [0 - nm/2-1]
  integer,save         :: calc_i, calc_j, calc_k  !- calculation grid
  logical,save         :: l2d           !- T: 2D data / F: 3D
  integer,save         :: nrec_first = 1 !- first record
  integer,save         :: nrec_last = 0  !- last record [default:io%nm]

  public :: ini
  public :: calc
  public :: get_result
  public :: write_result


  integer, parameter :: lun = 77
  integer,save :: im, jm, km, nm, reclen
  real,allocatable         :: r(:,:,:)
  real(8),allocatable,save :: d(:), dl(:)

  type(type_libmxe_para),save :: para
  type(type_libmxe_io),save :: io


contains


subroutine ini
  use libmxe_para, only: libmxe_para__register
  use libmxe_io,   only: libmxe_io__register
  implicit none

  integer :: i

  namelist /nml_lowpass/ file_base_in, file_base_out, &
            & nfreq_max, &
            & calc_i, calc_j, calc_k, l2d, &
            & nrec_first, nrec_last

  read( 5, nml=nml_lowpass, iostat=i )
  if ( i /= 0 ) then
    write(*,*) ' Read error : namelist'
    stop
  endif

  call libmxe_para__register(para)
  call libmxe_io__register(io,para)
  im = para%imut
  jm = para%jmut
  km = para%km
  if ( l2d ) then
    km = 1
    calc_k = 1
  endif
  reclen = im*jm*km*4

  if ( nrec_last == 0 ) nrec_last = io%nm
  if ( ( nrec_first < 1).or.( nrec_first > io%nm ) &
       & .or.( nrec_last < 1).or.( nrec_last > io%nm ) ) then
    write(6,*) 'Error at ini'
    write(6,*) '  nrec_first = ',nrec_first
    write(6,*) '  nrec_last  = ',nrec_last
    stop
  endif
  nm = nrec_last - nrec_first + 1

  allocate( r(im,jm,km) )
  allocate( d(nm), dl(nm) )

end subroutine ini


subroutine calc
  use libmxe_io, only: libmxe_io__open
  use fft_pgi,   only: fft__lowpass
  implicit none

  integer :: n

  d(:) = 0.d0
  do n = nrec_first, nrec_last

    if ( mod(n,100) == 0 ) write(*,*) n

    call libmxe_io__open( io, trim(file_base_in), n, reclen, &
                        & lun, action='read' )
    read(lun,rec=1) r
    close(lun)

    d( n - nrec_first + 1 ) = dble( r( calc_i, calc_j, calc_k ) )

  enddo

  call fft__lowpass( nm, d, nfreq_max, dl )

  d(:) = d(:) - dl(:)  !- high pass filter

end subroutine calc


subroutine write_result
  implicit none

  open( lun, file=trim(file_base_out)//'.gd', form='unformatted', &
       & access='direct', recl=nm*4, action='write' )
    write(lun,rec=1) real(dl(1:nm))
  close(lun)

  open( lun, file=trim(file_base_out)//'-h.gd', form='unformatted', &
       & access='direct', recl=nm*4, action='write' )
    write(lun,rec=1) real(d(1:nm))
  close(lun)

  write(*,*)
  write(*,*) 'Output ('//trim(file_base_out)//'.gd, '//trim(file_base_out)//'-h.gd) is created,'
  write(*,*) ' but grads control file is not made automatically.'
  write(*,*) '  direct access, single, length:',nm

end subroutine write_result


real function get_result(i)
  implicit none

  integer,intent(in) :: i

  get_result = real(dl(i))

end function get_result


end module lowpass
