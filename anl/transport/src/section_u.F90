! -*-F90-*-
!- transport across a vertical section
module section_u
  use libmxe_para, only: clen, type_libmxe_para
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo
  use libmxe_io,   only: type_libmxe_io
  implicit none
  private

  character(*),parameter :: file_mask_default = 'no'


  !-- arguments --
  character(clen),save :: file_base_u
  character(clen),save :: file_base_ssh
  character(clen),save :: file_out
  logical,save         :: l_zonal            !- T: zonal, F: meridional
  integer,save         :: i_first = 1, i_last = 0
  integer,save         :: j_section
  integer,save         :: k_first = 1, k_last = 0
  integer,save         :: nrec_first = 1 !- first record
  integer,save         :: nrec_last = 0  !- last record [default:io%nm]


  public :: ini
  public :: calc
  public :: get_result
  public :: write_result
  public :: next
  public :: has_next


  integer,parameter :: lun = 80

  type(type_libmxe_para),save :: para
  type(type_libmxe_grid),save :: grid
  type(type_libmxe_topo),save :: topo
  type(type_libmxe_io),save   :: io

  real(8),allocatable,save :: f(:)
  integer,save :: nrec
  integer,save :: im, jm, km, reclen_u, reclen_ssh, reclen_out

  real(4),allocatable :: u(:,:,:), ssh(:,:)


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
  use libmxe_grid, only: libmxe_grid__register
  use libmxe_topo, only: libmxe_topo__register, libmxe_topo__dz3d
  use libmxe_io, only  : libmxe_io__register
  implicit none

  integer :: ios

  namelist /nml_section_u/ file_base_ssh, file_base_u, file_out, &
                         & l_zonal, i_first, i_last, j_section, &
                         & k_first, k_last, nrec_first, nrec_last

  read( 5, nml=nml_section_u, iostat=ios )
  if ( ios /= 0 ) then
    write(*,*) ' Read error : namelist'
    stop
  endif

  call libmxe_para__register( para )
  call libmxe_grid__register( grid, para )
  call libmxe_topo__register( topo, para )
  call libmxe_topo__dz3d( topo, para )
  call libmxe_io__register( io, para )
  im = para%imut
  jm = para%jmut
  km = para%km

  if ( k_last  == 0 ) k_last  = km

  if ( nrec_last == 0 ) nrec_last = io%nm
  if ( ( nrec_first < 1).or.( nrec_first > io%nm ) &
       & .or.( nrec_last < 1).or.( nrec_last > io%nm ) ) then
    write(6,*) 'Error at ini'
    write(6,*) '  nrec_first = ',nrec_first
    write(6,*) '  nrec_last  = ',nrec_last
    stop
  endif

  reclen_u   = im*jm*km*4
  reclen_ssh = im*jm*4
  reclen_out = ( nrec_last - nrec_first + 1 ) * 4

  allocate( u(im,jm,km), ssh(im,jm), f(nrec_first:nrec_last) )
  f(:) = 0.d0

  nrec = nrec_first

end subroutine ini


subroutine calc
  use libmxe_io,   only: libmxe_io__open
  use libmxe_topo, only: libmxe_topo__updatedz
  use integ,       only: integ__section
  implicit none

  call libmxe_io__open( io, trim(file_base_u), nrec, reclen_u, &
                      & lun, action='read' )
  read( lun, rec=1 ) u
  close( lun )
  call libmxe_io__open( io, trim(file_base_ssh), nrec, reclen_ssh, &
                      & lun, action='read' )
  read( lun, rec=1 ) ssh
  close( lun )

  call libmxe_topo__updatedz( dble(ssh), topo, para, grid )

  f(nrec) = integ__section( dble(u), para, grid, topo, 'U', l_zonal, &
                     &  i_first, i_last, j_section, k_first, k_last )

  if ( mod(nrec,100) == 0 ) write(*,*) nrec,f(nrec)

end subroutine calc


subroutine write_result
  use libmxe_grads, only: libmxe_grads__make, type_grads
  implicit none

  type(type_grads) :: grads
  character(256)   :: ctemp

  grads%file_base = trim(file_out)
  grads%dset_suffix  = '.gd'
  write(ctemp,'(i4,a,i4)') i_first, '-', i_last
  grads%title = 'Transport across a vertical section: '//trim(ctemp)
  grads%cgrid = 'U'
  grads%istr  = 1
  grads%iend  = 1
  grads%jstr  = 1
  grads%jend  = 1
  grads%ztype = 'surface'
  grads%nvar  = 1
  grads%var(1)= 'f 1 99 transport [cm3/s]'
  call libmxe_grads__make( grads, para, grid, io )

  open( lun, file=trim(file_out)//'.gd', form='unformatted', &
      & access='direct', recl=reclen_out, action='write' )
  write(lun,rec=1) real(f(:))
  close(lun)

end subroutine write_result


real(8) function get_result(nn)
  implicit none

  integer,intent(in) :: nn

  get_result = f(nn)

end function get_result


end module section_u

