! -*-F90-*-
!- vertical integration
module integ_vert
  use libmxe_para, only: clen, type_libmxe_para
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo
  use libmxe_io,   only: type_libmxe_io
  implicit none
  private


  !-- arguments --
  character(clen),save :: file_base_in   !- input data
  character(clen),save :: file_base_ssh  !- SSH data
  character(clen),save :: dir_out
  character(clen),save :: file_base_out
  character(1),save :: cgrid  !- "T": T-grid / "U": U-grid
  integer,save         :: nrec_first = 1 !- first record
  integer,save         :: nrec_last = 0  !- last record [default:io%nm]
  integer,save         :: k_first = 1    !- integration range
  integer,save         :: k_last = 0

  public :: ini
  public :: calc
  public :: get_result
  public :: write_result
  public :: next
  public :: has_next


  integer,save :: nrec
  integer,save :: im, jm, km, reclen2d, reclen3d

  integer,parameter :: lun = 80

  type(type_libmxe_para),save :: para
  type(type_libmxe_grid),save :: grid
  type(type_libmxe_topo),save :: topo
  type(type_libmxe_io),save   :: io

  real(8),allocatable :: d(:,:,:), f(:,:)
  real,allocatable :: r4(:,:,:), hm(:,:)
  integer,pointer  :: depth_k(:,:)

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
  use libmxe_topo, only: libmxe_topo__register, &
                       & libmxe_topo__dz3d
  use libmxe_io,   only: libmxe_io__register
  use libmxe_grads,only: type_grads, libmxe_grads__make
  implicit none

  type(type_grads) :: grads
  integer :: i

  namelist /nml_integ_vert/ file_base_in, file_base_ssh, &
           & file_base_out, dir_out, cgrid, &
           & nrec_first, nrec_last, k_first, k_last

  read( 5, nml=nml_integ_vert, iostat=i )
  if ( i /= 0 ) then
    write(*,*) ' Read error : namelist'
    stop
  endif

  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)
  call libmxe_topo__dz3d(topo,para)
  call libmxe_io__register(io,para)
  im = para%imut
  jm = para%jmut
  km = para%km
  reclen2d = 4*im*jm
  reclen3d = 4*im*jm*km

  if ( nrec_last == 0 ) nrec_last = io%nm
  if ( ( nrec_first < 1).or.( nrec_first > io%nm ) &
       & .or.( nrec_last < 1).or.( nrec_last > io%nm ) ) then
    write(6,*) 'Error at ini'
    write(6,*) '  nrec_first = ',nrec_first
    write(6,*) '  nrec_last  = ',nrec_last
    stop
  endif

  if ( k_last == 0 ) k_last = km

  if ( cgrid=='T' ) then
    depth_k => topo%texnn
  else
    depth_k => topo%exnn
  endif

  grads%file_base = trim(file_base_out)
  grads%title = 'Vertical integration : '//trim(file_base_in)
  grads%cgrid = cgrid
  grads%ztype = 'surface'
  grads%nvar = 1
  grads%var(1) = 'f 1 99 Vertical integration'
  call libmxe_grads__make( grads, para, grid, io)

  allocate( d(im,jm,km), r4(im,jm,km), hm(im,jm), f(im,jm) )

  nrec = nrec_first

end subroutine ini


subroutine calc
  use libmxe_io,   only: libmxe_io__open
  use libmxe_topo, only: libmxe_topo__updatedz
  use integ,       only: integ__vert
  implicit none

  integer :: k

  if ( mod(nrec,100) == 0 ) write(6,*) nrec

  call libmxe_io__open( io, trim(file_base_ssh), nrec, &
                      & reclen2d, lun, action='read' )
  read(lun,rec=1) hm
  close(lun)
  call libmxe_topo__updatedz( dble(hm), topo, para, grid )

  call libmxe_io__open( io, trim(file_base_in), nrec, &
                      & reclen3d, lun, action='read' )
  read(lun,rec=1) r4
  close(lun)

  call integ__vert( dble(r4(:,:,:)), cgrid, para, topo, &
                  & f(:,:), k_first, k_last )

  where ( depth_k == 0 ) f = para%rundefout

end subroutine calc


subroutine write_result
  use libmxe_io ,   only: libmxe_io__open
  implicit none

  call libmxe_io__open( io, trim(dir_out)//'/'//trim(file_base_out), &
                      & nrec, reclen2d, lun, action='write' )
    write(lun,rec=1) real(f(:,:))
  close(lun)

end subroutine write_result


real(8) function get_result(nx,ny)
  implicit none

  integer,intent(in) :: nx,ny

  get_result = f(nx,ny)

end function get_result


end module integ_vert

