! -*-F90-*-
module trim_hs
  use libmxe_para,  only: clen, type_libmxe_para
  use libmxe_io,    only: type_libmxe_io
  implicit none
  private


  !-- arguments --
  character(clen),save :: file_base_in
  character(clen),save :: file_base_out
  character(clen),save :: dir_out
  character(1),save    :: cgrid = 'T'
  logical,save         :: l2d            !- T: 2D data, F: 3D data
  integer,save         :: i_first = 1, i_last = 0
  integer,save         :: j_first = 1, j_last = 0
  integer,save         :: k_first = 1, k_last = 0
  integer,save         :: nrec_first = 1 !- first record
  integer,save         :: nrec_last = 0  !- last record [default:io%nm]


  public :: ini
  public :: calc
  public :: get_result
  public :: write_result
  public :: next
  public :: has_next


  integer, parameter          :: lun = 77
  integer,save                :: nrec
  integer,save                :: im, jm, km, reclen_in
  integer,save                :: reclen_out
  real,allocatable,save       :: r(:,:,:)

  type(type_libmxe_para),save :: para
  type(type_libmxe_io),save   :: io


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
  use libmxe_grid, only: libmxe_grid__register, type_libmxe_grid
  use libmxe_io,   only: libmxe_io__register
  use libmxe_grads,only: libmxe_grads__make, type_grads
  implicit none

  type(type_libmxe_grid),save :: grid
  type(type_grads)      ,save :: grads

  integer      :: i
  character(4) :: ctemp

  namelist /nml_trim_hs/ file_base_in, file_base_out, dir_out, cgrid &
            & l2d, i_first, i_last, j_first, j_last, & !k_first
            & k_last, nrec_first, nrec_last

  read( 5, nml=nml_trim_hs )

  call libmxe_para__register( para )
  call libmxe_grid__register( grid, para )
  call libmxe_io__register( io, para )

  im     = para%imut
  jm     = para%jmut
  km     = para%km
  if ( l2d ) km = 1
  reclen_in = im*jm*km*4

  if ( i_last  == 0 ) i_last  = im
  if ( j_last  == 0 ) j_last  = jm
  if ( k_last  == 0 ) k_last  = km
  if ( l2d ) then
    k_first = 1
    k_last  = 1
  endif
  reclen_out = ( i_last - i_first + 1 ) * ( j_last - j_first + 1 ) &
             &  * ( k_last - k_first + 1 ) * 4

  if ( nrec_last == 0 ) nrec_last = io%nm
  if ( ( nrec_first < 1).or.( nrec_first > io%nm ) &
       & .or.( nrec_last < 1).or.( nrec_last > io%nm ) ) then
    write(6,*) 'Error at ini'
    write(6,*) '  nrec_first = ',nrec_first
    write(6,*) '  nrec_last  = ',nrec_last
    stop
  endif

  !-- grads control file --
  grads%file_base = trim(file_base_out)
  grads%title = 'resize of '//trim(file_base_in)
  grads%ztype = 'center'
  if ( l2d ) grads%ztype = 'surface'
  grads%cgrid = cgrid
  grads%istr = i_first
  grads%iend = i_last
  grads%jstr = j_first
  grads%jend = j_last
  grads%nvar = 1
  write(ctemp,'(i4)') km
  grads%var(1) = 'd '//trim(ctemp)//' 99 resize'
  call libmxe_grads__make(grads,para,grid,io)

  allocate( r(im,jm,km) )
  r(:,:,:) = 0.e0

  nrec = nrec_first

end subroutine ini


subroutine calc
  use libmxe_io, only: libmxe_io__open
  implicit none

  if ( mod(nrec,100) == 0 ) write(*,*) nrec
  call libmxe_io__open( io, trim(file_base_in), nrec, reclen_in, &
                      & lun, action='read' )
  read( lun, rec=1 ) r
  close( lun )

end subroutine calc


subroutine write_result
  use libmxe_io, only: libmxe_io__open
  implicit none

  call libmxe_io__open( io, trim(dir_out)//'/'//trim(file_base_out), &
                      & nrec, reclen_out, lun, action='write')
  write(lun,rec=1) r(i_first:i_last,j_first:j_last,k_first:k_last)
  close(lun)

end subroutine write_result


real function get_result(i,j,k)
  implicit none

  integer,intent(in) :: i,j,k

  get_result = r(i,j,k)

end function get_result


end module trim_hs
