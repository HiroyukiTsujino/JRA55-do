! -*-F90-*-
!- Make T-grid 2D mask array (template).
program main
  use libmxe_para
  use libmxe_grid
  use libmxe_topo
  use libmxe_io
  use libmxe_grads
  implicit none

  character(*),parameter :: fileo = 'maskt'

  integer,parameter :: lun = 80
  integer :: i,j,im,jm,reclen
  real(4),allocatable :: d(:,:)
  integer,allocatable :: ho4t(:,:)
  type(type_libmxe_para) :: para
  type(type_libmxe_grid) :: grid
  type(type_libmxe_topo) :: topo
  type(type_libmxe_io)   :: io
  type(type_grads) :: grads

  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)
  call libmxe_io__register(io,para)
  im = para%imut
  jm = para%jmut
  reclen = 4*im*jm

  grads%file_base = trim(fileo)
  grads%title = 'Mask array : '//trim(fileo)
  grads%cgrid = 'T'
  grads%ztype = 'surface'
  grads%timemode = 'stationary'
  grads%nvar = 1
  grads%undef = 0.e0
  grads%var(1) = 'm 1 99 mask (1 or 0)'
  call libmxe_grads__make(grads,para,grid,io)

  allocate(d(im,jm),ho4t(im,jm))
  d(1:im,1:jm) = 0.e0

  !-- depth at T-grid (cm)
  ho4t(:,:) = 0
  ho4t(1,1) = topo%ho4(1,1)
  do i = 2, im
    ho4t(i,1) = max( topo%ho4(i,1), topo%ho4(i-1,1) )
  enddo
  do j = 2, jm
    ho4t(1,j) = max( topo%ho4(1,j), topo%ho4(1,j-1) )
  enddo
  do j = 2, jm
    do i = 2, im
      ho4t(i,j) = maxval( topo%ho4(i-1:i,j-1:j) )
    enddo
  enddo
  if ( para%lcyclic ) then
    ho4t(1,2:jm) = topo%ho4(im-3,2:jm)
    ho4t(2,2:jm) = topo%ho4(im-2,2:jm)
    ho4t(im-1,2:jm) = topo%ho4(3,2:jm)
    ho4t(im,2:jm) = topo%ho4(4,2:jm)
  endif


  !----------------------------
  do j = 1, jm
    do i = 1, im
      if ( ( grid%glatt(i,j) >= -66.d0 ) &
         & .and.( grid%glatt(i,j) <= 66.d0 ) &
         & .and.( ho4t(i,j) >= 100000 ) ) then
        d(i,j) = 1.e0
      endif
    enddo
  enddo
  !----------------------------


  open(lun,file=fileo//'.gd',form='unformatted' &
       & ,access='direct',recl=reclen,action='write')
    write(lun,rec=1) d(1:im,1:jm)
  close(lun)


end program main

