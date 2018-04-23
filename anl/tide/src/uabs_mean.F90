! -*-F90-*-
!-- Mean absolute velocity of barotropic current
module uabs_mean
  use libmxe_para, only: clen, type_libmxe_para
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo
  use libmxe_io, only: type_libmxe_io
  use libmxe_grads, only: type_grads
  implicit none
  private


  !-- arguments --
  character(clen),save :: file_base_ssh  !- input file base (SSH)
  character(clen),save :: file_base_um   !-   (UM)
  character(clen),save :: file_base_vm   !-   (VM)
  integer,save :: nstr = 1  !- start N  [default: 1]
  integer,save :: nend = 0  !- end N [default: nm]


  public :: ini
  public :: calc
  public :: get_result
  public :: write_result


  character(*), parameter :: fileo = 'uabs_mean'
  integer, parameter :: lun = 77
  real,allocatable,save :: r(:,:)
  integer,save :: im, jm, reclen

  type(type_libmxe_para),save :: para
  type(type_libmxe_grid),save :: grid
  type(type_libmxe_topo),save :: topo
  type(type_libmxe_io),save :: io
  type(type_grads),save :: grads


contains


subroutine ini
  use libmxe_para, only: libmxe_para__register
  use libmxe_grid, only: libmxe_grid__register
  use libmxe_topo, only: libmxe_topo__register
  use libmxe_io, only: libmxe_io__register
  implicit none

  integer :: i
  character(3) :: ctemp

  namelist /uabs_mean_lst/  file_base_ssh, file_base_um &
            & , file_base_vm, nstr, nend

  !---- arguments ----
  read(5,nml=uabs_mean_lst,iostat=i)
  if ( i /= 0 ) then
    write(*,*) ' Read error : namelist'
    stop
  endif

  !---- experiment settings ----
  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)
  call libmxe_io__register(io,para)
  im = para%imut
  jm = para%jmut
  reclen = im*jm*4
  if ( nend == 0 ) nend = io%nm
  if ( ( nstr < 1).or.( nstr > io%nm ) &
       & .or.( nend < 1).or.( nend > io%nm ) ) then
    write(*,*) 'Error at ini'
    write(*,*) '  Wrong nstr, nend',nstr,nend
    stop
  endif

  !-- grads control file --
  grads%file_base = trim(fileo)
  grads%title = trim(fileo)
  grads%cgrid = 'U'
  grads%ztype = 'surface'
  grads%timemode = 'stationary'
  grads%nvar = 1
  grads%var(1) = 'u 1 99 absolute velocity [cm/s]'

  allocate(r(im,jm))

end subroutine ini


subroutine calc
  use libmxe_io, only: libmxe_io__open
  implicit none

  integer :: i,j,n
  real(8),allocatable,dimension(:,:) :: um,vm,ssh,d
  real,allocatable :: r4(:,:)

  allocate(um(im,jm),vm(im,jm),ssh(im,jm))
  allocate(r4(im,jm),d(im,jm))
  d = 0.d0

  do n = nstr, nend

    if ( mod(n,100) == 0 ) write(*,*) n

    !-- input --
    call libmxe_io__open(io,trim(file_base_ssh),n,reclen &
         & ,lun,action='read')
    read(lun,rec=1) r4
    close(lun)
    ssh(:,:) = dble( r4(:,:) )
    call libmxe_io__open(io,trim(file_base_um),n,reclen &
         & ,lun,action='read')
    read(lun,rec=1) r4
    close(lun)
    um(:,:) = dble( r4(:,:) )
    call libmxe_io__open(io,trim(file_base_vm),n,reclen &
         & ,lun,action='read')
    read(lun,rec=1) r4
    close(lun)
    vm(:,:) = dble( r4(:,:) )

    !-- vol. transport => velocity --
    um(:,:) = um(:,:) / ( dble(topo%ho4(:,:)) + ssh(:,:) )
    vm(:,:) = vm(:,:) / ( dble(topo%ho4(:,:)) + ssh(:,:) )

    !-- absolute value --
    d(:,:) = d(:,:) + sqrt( um(:,:)**2 + vm(:,:)**2 )

  enddo

  !-- time mean --
  r(:,:) = real( d(:,:) / dble( nend - nstr + 1 ) )

  !-- land --
  do j = 1, jm
    do i = 1, im
      if ( topo%exnn(i,j) < 1 ) r(i,j) = para%rundefout
    enddo
  enddo

  deallocate(um,vm,ssh,d)

end subroutine calc


subroutine write_result
  use libmxe_grads, only: libmxe_grads__make
  implicit none

  call libmxe_grads__make(grads,para,grid,io)
  open(lun,file=trim(fileo)//'.gd',form='unformatted' &
     & , access='direct',recl=reclen,action='write')
    write(lun,rec=1) r
  close(lun)

end subroutine write_result


real function get_result(i,j)
  implicit none

  integer,intent(in) :: i,j

  get_result = r(i,j)

end function get_result


end module uabs_mean
