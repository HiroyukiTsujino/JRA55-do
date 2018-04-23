! -*-F90-*-
!-- Remap history, e.g. sub model output to parent model grid
module remap
  use libmxe_para, only: clen, type_libmxe_para
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo
  use libmxe_io, only: type_libmxe_io
  use libmxe_grads, only: type_grads
  implicit none
  private


  !-- arguments --
  character(clen)      :: file_configure_src
  character(clen),save :: file_base_src      !- include path
  character(clen)      :: file_configure_out
  character(clen),save :: dir_out
  character(clen),save :: file_base_out      !- filename only
  character(clen)      :: file_remap         !- remap table
  logical              :: l2d = .false.      !- .true. : 2D data
                                             !- .false.: 3D data
  character(1)         :: cgrid              !- "T" or "U"
  real(4),save         :: undef_out          !- undefined value
                                    !- [default:rundefout or rundef]
  integer,save         :: nrec_first = 1     !- first record
  integer,save         :: nrec_last          !- last record [default:nm]


  public :: ini
  public :: calc
  public :: get_result
  public :: write_result
  public :: next
  public :: has_next


  integer, parameter       :: lun = 77
  real,allocatable,save    :: r(:)
  real(8),allocatable,save :: d1(:), d3(:,:,:)
  integer,allocatable,save :: i1(:)
  real(8),pointer,save     :: aexl(:,:,:)
  integer,save             :: im, jm, km, ngrid_out
  integer,save             :: ngrid_src
  integer,save             :: nrec

  integer,save             :: nlink
  integer,allocatable,save :: isrc(:),idst(:)
  real(8),allocatable,save :: wgt(:)

  type(type_libmxe_para),save :: para,para_src
  type(type_libmxe_grid),save :: grid
  type(type_libmxe_topo),save :: topo
  type(type_libmxe_io),save   :: io,io_src
  type(type_grads),save       :: grads


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
  use libmxe_topo, only: libmxe_topo__register, libmxe_topo__aexl
  use libmxe_io, only: libmxe_io__register
  use libmxe_grads, only: libmxe_grads__make
  implicit none

  real(4),parameter :: undef_local = -9.87d33

  integer :: i
  character(4) :: ctemp

  namelist /nml_remap/  &
            & file_configure_src, file_base_src, &
            & file_configure_out, dir_out, file_base_out, &
            & file_remap, cgrid, l2d, &
            & undef_out, nrec_first, nrec_last

  undef_out = undef_local
  nrec_last = 0
  read(5,nml=nml_remap,iostat=i)
  if ( i /= 0 ) then
    write(*,*) ' Read error : namelist'
    stop
  endif

  call libmxe_para__register(para &
             & ,file_namelist=trim(file_configure_out))
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)
  call libmxe_topo__aexl(topo,para)
  call libmxe_io__register(io,para)

  if ( undef_out == undef_local ) undef_out = para%rundefout
  im = para%imut
  jm = para%jmut
  km = para%km
  if ( l2d ) km = 1
  ngrid_out = im * jm * km

  if ( nrec_last == 0 ) nrec_last = io%nm
  if ( ( nrec_first < 1).or.( nrec_first > io%nm ) &
       & .or.( nrec_last < 1).or.( nrec_last > io%nm ) ) then
    write(6,*) 'Error at ini'
    write(6,*) '  nrec_first = ',nrec_first
    write(6,*) '  nrec_last  = ',nrec_last
    stop
  endif

  if ( cgrid=='U' ) then
    aexl => topo%aexl
  else
    aexl => topo%atexl
  endif

  !-- grads control file --
  grads%file_base = trim(file_base_out)
  grads%title = 'Remap'
  grads%cgrid = cgrid
  grads%ztype = 'center'
  if ( l2d ) grads%ztype = 'surface'
  grads%nvar = 1
  write(ctemp,'(i4)') km
  grads%var(1) = 'd '//trim(ctemp)//' 99 remap'
  call libmxe_grads__make(grads,para,grid,io)

  !-- source model --
  call libmxe_para__register(para_src &
             & ,file_namelist=trim(file_configure_src))
  call libmxe_io__register(io_src,para_src)
  i = para_src%km
  if ( l2d ) i = 1
  ngrid_src = para_src%imut * para_src%jmut * i

  !-- read remap table --
  open(lun,file=file_remap,form='unformatted',access='sequential')
    write(6,*) ' Reading from ... ', trim(file_remap)
    read(lun)
    read(lun)
    read(lun)
    read(lun)
    read(lun)
    read(lun)
    read(lun) i, nlink
    write(6,*) ' There are ', nlink, ' links '
    allocate(isrc(nlink),idst(nlink))
    allocate(wgt(nlink))
    read(lun) isrc(1:nlink)
    read(lun) idst(1:nlink)
    read(lun) wgt(1:nlink)
  close(lun)

  allocate( r(ngrid_src) )
  allocate( d1(ngrid_out), i1(ngrid_out), d3(im,jm,km) )

  nrec = nrec_first

end subroutine ini


subroutine calc
  use libmxe_io, only: libmxe_io__open
  implicit none

  integer :: i,j,k,n


  if ( mod(nrec,100) == 0 ) write(*,*) nrec

  call libmxe_io__open(io_src,trim(file_base_src),nrec,ngrid_src*4 &
       & ,lun,action='read')
  read(lun,rec=1) r
  close(lun)

  d1(:) = 0.d0
  i1(:) = 0

  do n = 1, nlink
    i = isrc(n)
    j = idst(n)
    d1(j) = d1(j) + dble(r(i)) * wgt(n)
    i1(j) = i1(j) + 1
  enddo

  do k = 1, km
    do j = 1, jm
      do i = 1, im
        n = i + im*(j-1) + im*jm*(k-1)
        if ( i1(n) > 0 ) then
          d3(i,j,k) = d1(n)
        else
          d3(i,j,k) = undef_out
        endif
      enddo
    enddo
  enddo

  d3(:,:,:) = d3(:,:,:) * aexl(:,:,:) &
           &  + undef_out * ( 1.d0 - aexl(:,:,:) )

end subroutine calc


subroutine write_result
  use libmxe_io, only: libmxe_io__open
  implicit none

  call libmxe_io__open(io,trim(dir_out)//'/'//trim(file_base_out) &
                  & , nrec, ngrid_out*4, lun, action='write')
    write(lun,rec=1) real(d3(:,:,:))
  close(lun)

end subroutine write_result


real(8) function get_result(i,j,k)
  implicit none

  integer,intent(in) :: i,j,k

  get_result = d3(i,j,k)

end function get_result


end module remap
