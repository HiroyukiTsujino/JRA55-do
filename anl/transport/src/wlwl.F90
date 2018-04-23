! -*-F90-*-
!-- Make vertical transport (wlwl) / velocity (w2) data.
module wlwl
  use libmxe_para, only: clen, type_libmxe_para
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo
  use libmxe_io, only: type_libmxe_io
  use libmxe_grads, only: type_grads
  implicit none
  private


  !-- arguments --
  character(clen),save :: file_base_ssh=''  !- input file base (SSH)
  character(clen),save :: file_base_u=''    !-   (u)
  character(clen),save :: file_base_v=''    !-   (v)
  character(clen),save :: diro           !- output directory
  integer,save :: nstr = 1  !- start N  [default: 1]
  integer,save :: nend = 0  !- end N [default: nm]
  logical :: lw2  !- .true.: make w2 (velocity) data / .false.: not
  real,save :: missing  !- missing value


  public :: ini
  public :: calc
  public :: get_wlwl
  public :: write_wlwl
  public :: get_w2
  public :: write_w2
  public :: next
  public :: has_next


  character(*), parameter :: fileo_wlwl = 'wlwl'
  character(*), parameter :: fileo_w2 = 'w2'
  integer, parameter :: lun = 77
  real, parameter :: missing_default = -999.999e10

  real,allocatable,save :: r(:,:,:), w2(:,:,:) !- store results
  integer,save :: im, jm, km, nm, reclen
  integer,save :: nrec    !- count record loop
  real(8),allocatable,save :: rareat(:,:) !- 1./ area of T-box

  type(type_libmxe_para),save :: para
  type(type_libmxe_grid),save :: grid
  type(type_libmxe_topo),save :: topo
  type(type_libmxe_io),save :: io
  type(type_grads),save :: grads

  !- temporal
  real,allocatable :: r4(:,:,:), r42(:,:)
  real(8),allocatable,dimension(:,:,:) :: u, v, wlwl8
  real(8),allocatable,dimension(:,:) :: ssh, ustarl, vstarl


contains


subroutine next
  implicit none

  nrec = nrec + 1
 
end subroutine next


logical function has_next
  implicit none

  if ( nrec > nend ) then
    has_next = .false.
  else
    has_next = .true.
  endif

end function has_next


subroutine ini
  use libmxe_para, only: libmxe_para__register
  use libmxe_grid, only: libmxe_grid__register
  use libmxe_topo, only: libmxe_topo__register &
                    &   , libmxe_topo__aexl, libmxe_topo__dz3d
  use libmxe_io, only: libmxe_io__register
  use libmxe_grads, only: libmxe_grads__make
  implicit none

  integer :: i, j
  character(3) :: ctemp

  namelist /wlwl_lst/ file_base_ssh, file_base_u &
            & , file_base_v, diro, lw2, missing, nstr, nend

  !---- arguments ----
  missing = missing_default
  read(5,nml=wlwl_lst,iostat=i)
  if ( i /= 0 ) then
    write(*,*) ' Read error : namelist'
    stop
  endif

  !---- experiment settings ----
  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)
  call libmxe_topo__aexl(topo,para)
  call libmxe_topo__dz3d(topo,para)
  call libmxe_io__register(io,para)
  im = para%imut
  jm = para%jmut
  km = para%km
  reclen = im*jm*km*4
  if ( nend == 0 ) nend = io%nm
  if ( ( nstr < 1).or.( nstr > io%nm ) &
       & .or.( nend < 1).or.( nend > io%nm ) ) then
    write(*,*) 'Error at ini'
    write(*,*) '  Wrong nstr, nend',nstr,nend
    stop
  endif
  if ( missing == missing_default ) then
    missing = para%rundefout
  endif

  !-- area --
  allocate( rareat(im,jm) )
  rareat(:,:) = 0.d0
  do j = 2, jm
    do i = 2, im
     rareat(i,j) = 1.d0 / ( grid%a_bl(i,j) + grid%a_br(i-1,j) &
              & + grid%a_tl(i,j-1) + grid%a_tr(i-1,j-1) )
    enddo
  enddo

  !-- grads control file --
  grads%file_base = fileo_wlwl
  grads%title = fileo_wlwl
  grads%cgrid = 'T'
  grads%ztype = 'bottom'
  grads%nvar = 1
  write(ctemp,'(i3)') km
  grads%var(1) = 'w '//trim(ctemp)//' 99 vertical transport'
  call libmxe_grads__make(grads,para,grid,io)

  if ( lw2 ) then
    grads%file_base = fileo_w2
    grads%title = fileo_w2
    grads%cgrid = 'T'
    grads%ztype = 'bottom'
    grads%nvar = 1
    write(ctemp,'(i3)') km
    grads%var(1) = 'w '//trim(ctemp)//' 99 vertical velocity'
    call libmxe_grads__make(grads,para,grid,io)

    allocate(w2(im,jm,km))
  endif

  allocate(r(im,jm,km))

  !-- work space --
  allocate(r42(im,jm), r4(im,jm,km))
  allocate(wlwl8(im,jm,km))
  allocate(ssh(im,jm), u(im,jm,km), v(im,jm,km))
  allocate(ustarl(im,jm), vstarl(im,jm))

  nrec = nstr

end subroutine ini


subroutine calc
  use libmxe_topo, only: libmxe_topo__updatedz
  use libmxe_io, only: libmxe_io__open
  use libmxe_io, only: libmxe_io__suffix
  implicit none

  integer :: i,j,k

  if ( mod(nrec,10) == 0 ) write(*,*) nrec

  !-- update dzu --
  call libmxe_io__open(io,trim(file_base_ssh),nrec,im*jm*4 &
       & ,lun,action='read')
  read(lun,rec=1) r42
  close(lun)
  ssh(:,:) = dble( r42(:,:) )
  call libmxe_topo__updatedz(ssh,topo,para,grid)

  !-- input of u, v --
  call libmxe_io__open(io,trim(file_base_u),nrec,reclen &
       & ,lun,action='read')
  read(lun,rec=1) r4
  close(lun)
  u(:,:,:) = dble( r4(:,:,:) )
  call libmxe_io__open(io,trim(file_base_v),nrec,reclen &
       & ,lun,action='read')
  read(lun,rec=1) r4
  close(lun)
  v(:,:,:) = dble( r4(:,:,:) )

  !-- vertical transport (wlwl) --
  !- wlwl is defined at grid: (T,bottom)
  !- wlwl = 0 at k=km (bottom)
  !- wlwl = 0 at sea surface (not calculated),
  !-     since SSH elevation is not taken into accout
  wlwl8(:,:,:) = 0.d0
  do k = km, 2, -1
    ustarl(:,:) = 0.d0
    vstarl(:,:) = 0.d0

    do j = 2, jm
      do i = 2, im
        ustarl(i,j) = 0.5d0 &
             & *(  ( 2.d0 - topo%aexl(i,j-1,k) ) &
             &      * u(i,j,k) * topo%dzu(i,j,k) &
             &    +( 2.d0 - topo%aexl(i,j,k)   ) &
             &      * u(i,j-1,k) * topo%dzu(i,j-1,k) ) &
             & *( grid%dy_br(i,j) + grid%dy_tr(i,j-1) )
        vstarl(i,j) = 0.5d0 &
             & *(  ( 2.d0 - topo%aexl(i-1,j,k) ) &
             &      * v(i,j,k) * topo%dzu(i,j,k) &
             &    +( 2.d0 - topo%aexl(i,j,k)   ) &
             &      *v(i-1,j,k) * topo%dzu(i-1,j,k) ) &
             & *( grid%dx_tl(i,j) + grid%dx_tr(i-1,j) )

        wlwl8(i,j,k-1) = topo%atexl(i,j,k) &
         & * ( wlwl8(i,j,k) - 0.5d0    &
         &      *(  ustarl(i,j) &
         &           *( topo%aexl(i,j,k) + topo%aexl(i,j-1,k) ) &
         &         -ustarl(i-1,j) &
         &           *( topo%aexl(i-1,j,k) + topo%aexl(i-1,j-1,k) ) &
         &         +vstarl(i,j) &
         &           *( topo%aexl(i,j,k) + topo%aexl(i-1,j,k) ) &
         &         -vstarl(i,j-1) &
         &           *( topo%aexl(i,j-1,k) + topo%aexl(i-1,j-1,k) ) ))

      enddo
    enddo
  enddo

  !-- zonal cyclic condition --
  if (para%lcyclic) then
    do k = 1, km
      do j = 2, jm
        wlwl8(para%iet+1,j,k) = wlwl8(para%ibt  ,j,k)
        wlwl8(para%iet+2,j,k) = wlwl8(para%ibt+1,j,k)
        wlwl8(para%ibt-1,j,k) = wlwl8(para%iet  ,j,k)
        wlwl8(para%ibt-2,j,k) = wlwl8(para%iet-1,j,k)
      enddo
    enddo
  endif

  !-- stored by real(4) array (r) --
  r(:,:,:) =  missing
  do k = 1, km
    do j = 2, jm
      do i = 2, im
        r(i,j,k) = real( topo%atexl(i,j,k) * wlwl8(i,j,k) ) &
             &   + real( 1.d0 - topo%atexl(i,j,k) ) * missing
      enddo
    enddo
  enddo

  !-- vertical velocity (w2) --
  if ( .not. lw2 ) return
  w2(:,:,:) =  missing
  do k = 1, km
    do j = 2, jm
      do i = 2, im
        w2(i,j,k) = topo%atexl(i,j,k) * wlwl8(i,j,k) * rareat(i,j) &
             &   + ( 1.d0 - topo%atexl(i,j,k) ) * missing
      enddo
    enddo
  enddo

end subroutine calc


subroutine write_wlwl
  use libmxe_io, only: libmxe_io__open
  implicit none

  call libmxe_io__open(io,trim(diro)//'/'//trim(fileo_wlwl) &
                  & , nrec, reclen, lun, action='write')
    write(lun,rec=1) r(:,:,:)
  close(lun)

end subroutine write_wlwl


real function get_wlwl(i,j,k)
  implicit none

  integer,intent(in) :: i,j,k

  get_wlwl = r(i,j,k)

end function get_wlwl


subroutine write_w2
  use libmxe_io, only: libmxe_io__open
  implicit none

  if ( .not. lw2 ) return

  call libmxe_io__open(io,trim(diro)//'/'//trim(fileo_w2) &
                  & , nrec, reclen, lun, action='write')
    write(lun,rec=1) w2(:,:,:)
  close(lun)

end subroutine write_w2


real function get_w2(i,j,k)
  implicit none

  integer,intent(in) :: i,j,k

  if ( lw2 ) then
    get_w2 = w2(i,j,k)
  else
    get_w2 = missing
  endif

end function get_w2



end module wlwl
