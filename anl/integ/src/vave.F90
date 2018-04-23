! -*-F90-*-
!- volume average
module vave
  use libmxe_para, only: clen, type_libmxe_para
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo
  use libmxe_io, only: type_libmxe_io
  use libmxe_grads, only: type_grads
  implicit none
  private


  !-- arguments --
  character(clen),save :: file_base  !- input data (file base)
  character(clen),save :: file_base_ssh  !- SSH data (file base)
  character(clen),save :: fileo  !- output file name
  character(1),save :: cgrid  !- "T": T-grid / "U": U-grid

  public :: ini
  public :: calc
  public :: get_result
  public :: write_result
  public :: next
  public :: has_next


  real(8),allocatable,save :: f(:)
  integer,save :: n    !- count record loop
  integer,save :: im, jm, km, nm, reclen2d, reclen3d
  real(8),save :: vol

  integer,parameter :: lun = 80

  type(type_libmxe_para),save :: para
  type(type_libmxe_grid),save :: grid
  type(type_libmxe_topo),save :: topo
  type(type_libmxe_io),save :: io
  type(type_grads),save :: grads

  real(8),allocatable :: d(:,:,:)
  real,allocatable :: r4(:,:,:), hm(:,:)

contains


subroutine next
  implicit none

  n = n + 1
 
end subroutine next


logical function has_next
  implicit none

  if ( n > io%nm ) then
    has_next = .false.
  else
    has_next = .true.
  endif

end function has_next


subroutine ini
  use libmxe_para, only: libmxe_para__register
  use libmxe_grid, only: libmxe_grid__register
  use libmxe_topo, only: libmxe_topo__register &
                      & , libmxe_topo__dz3d
  use libmxe_io, only: libmxe_io__register
  use integ, only: integ__vol
  implicit none

  integer :: i,k
  character(3) :: ctemp

  namelist /vave_lst/ file_base, fileo, cgrid, file_base_ssh

  read(5,nml=vave_lst,iostat=i)
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
  nm = io%nm

  reclen2d = 4*im*jm
  reclen3d = 4*im*jm*km

  grads%file_base = trim(fileo)
  grads%title = 'Volume average : '//trim(fileo)
  grads%cgrid = cgrid
  grads%istr=1
  grads%iend=1
  grads%jstr=1
  grads%jend=1
  grads%ztype = 'surface'
  grads%nvar = 1
  grads%var(1) = 'f 1 99 Volume ave.'

  allocate(d(im,jm,km), r4(im,jm,km), hm(im,jm), f(nm))
  f(:) = 0.d0
  d(:,:,:) = 1.d0
  vol = integ__vol(d,cgrid,para,grid,topo)
!  vol = integ__vol(d,cgrid,para,grid,topo,istr=3,iend=553,jstr=153,jend=403)
  write(*,*) 'volume = ',vol

  n = 1

end subroutine ini


subroutine calc
  use libmxe_io, only: libmxe_io__open
  use libmxe_topo, only: libmxe_topo__updatedz
  use integ, only: integ__vol
  implicit none

  integer :: k

  if ( .not. para%ldef ) then
    write(*,*) 'Error: calc is called before ini.'
    stop
  endif

  call libmxe_io__open(io,trim(file_base_ssh),n &
                      & ,reclen2d,lun,action='read')
    read(lun,rec=1) hm
  close(lun)
  call libmxe_topo__updatedz(dble(hm),topo,para,grid)

  call libmxe_io__open(io,trim(file_base),n &
       & ,reclen3d,lun,action='read')
  read(lun,rec=1) r4
  close(lun)

  where ( r4 == para%rundefin ) r4 = 0.e0  !- missing value!
  d(:,:,:) = dble( r4(:,:,:) )

  f(n) = integ__vol(d,cgrid,para,grid,topo) / vol
!  f(n) = integ__vol(d,cgrid,para,grid,topo &
!                      &,istr=3,iend=553,jstr=153,jend=403) / vol

  write(*,*) n,f(n)

end subroutine calc


subroutine write_result
  use libmxe_grads, only: libmxe_grads__make
  implicit none

  call libmxe_grads__make(grads,para,grid,io)
  open(lun,file=trim(fileo)//'.gd',form='unformatted' &
       & ,access='direct',recl=4*nm,action='write')
    write(lun,rec=1) real(f(1:nm))
  close(lun)

end subroutine write_result


real(8) function get_result(nn)
  implicit none

  integer,intent(in) :: nn

  get_result = f(nn)

end function get_result


end module vave

