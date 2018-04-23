! -*-F90-*-
module barotropic_ke_vol
  use libmxe_para,  only: clen, type_libmxe_para
  use libmxe_grid,  only: type_libmxe_grid
  use libmxe_topo,  only: type_libmxe_topo
  use libmxe_io,    only: type_libmxe_io
  use libmxe_grads, only: type_grads
  implicit none
  private


  !-- arguments --
  character(clen),save :: file_base_ssh
  character(clen),save :: file_base_um   !- zonal transport
  character(clen),save :: file_base_vm   !- meridional
  character(clen),save :: file_base_out
  integer,save         :: nrec_first = 1 !- first record
  integer,save         :: nrec_last      !- last record [default:nm]

  public :: ini
  public :: calc
  public :: get_result
  public :: write_result
  public :: next
  public :: has_next


  real(8),allocatable,save :: ke_cm2ps2(:)
  integer,save :: nrec
  integer,save :: im, jm, nm, reclen
  real(8),save :: vol_cm3

  integer,parameter :: lun = 80

  type(type_libmxe_para),save :: para
  type(type_libmxe_grid),save :: grid
  type(type_libmxe_topo),save :: topo
  type(type_libmxe_io),save :: io
  type(type_grads),save :: grads

  real(8),allocatable :: d(:,:,:), d8(:,:), hu_cm(:,:)
  real,allocatable :: hm(:,:), um(:,:), vm(:,:)

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
                       & libmxe_topo__dz3d,     &
                       & libmxe_topo__aexl
  use libmxe_io,   only: libmxe_io__register
  use integ,       only: integ__vol
  implicit none

  integer :: i,k
  character(3) :: ctemp

  namelist /nml_barotropic_ke_vol/ file_base_ssh, &
             & file_base_um, file_base_vm, file_base_out, &
             & nrec_first, nrec_last

  nrec_last = 0
  read(5,nml=nml_barotropic_ke_vol,iostat=i)
  if ( i /= 0 ) then
    write(*,*) ' Read error : namelist'
    stop
  endif

  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)
  call libmxe_topo__dz3d(topo,para)
  call libmxe_topo__aexl(topo,para)
  call libmxe_io__register(io,para)
  im = para%imut
  jm = para%jmut
  reclen = 4*im*jm

  if ( nrec_last == 0 ) nrec_last = io%nm
  if ( ( nrec_first < 1).or.( nrec_first > io%nm ) &
       & .or.( nrec_last < 1).or.( nrec_last > io%nm ) ) then
    write(6,*) 'Error at ini'
    write(6,*) '  nrec_first = ',nrec_first
    write(6,*) '  nrec_last  = ',nrec_last
    stop
  endif
  nm = nrec_last - nrec_first + 1

  grads%file_base = trim(file_base_out)
  grads%title = 'Volume average of Barotropic KE'
  grads%cgrid = 'U'
  grads%istr=1
  grads%iend=1
  grads%jstr=1
  grads%jend=1
  grads%ztype = 'surface'
  grads%nvar = 1
  grads%var(1) = 'f 1 99 barotropic KE'

  allocate( d(im,jm,para%km) )
  d(:,:,:) = 1.d0
  vol_cm3 = integ__vol(d,'U',para,grid,topo)
  write(*,*) 'volume[cm^3] = ',vol_cm3
  deallocate( d )

  allocate( hm(im,jm), um(im,jm), vm(im,jm), hu_cm(im,jm) )
  allocate( d8(im,jm), ke_cm2ps2(nrec_first:nrec_last))
  hm(:,:) = 0.e0
  um(:,:) = 0.e0
  vm(:,:) = 0.e0
  ke_cm2ps2(:) = 0.d0
  d8(:,:) = 0.d0

  nrec = nrec_first

end subroutine ini


subroutine calc
  use libmxe_io, only: libmxe_io__open
  use integ,     only: integ__area
  use regrid,    only: regrid__h2d
  implicit none

  if ( mod(nrec,100) == 0 ) write(6,*) nrec

  call libmxe_io__open( io, trim(file_base_ssh), nrec, &
                      & reclen, lun, action='read' )
  read(lun,rec=1) hm
  close(lun)
  call libmxe_io__open( io, trim(file_base_um), nrec, &
                      & reclen, lun, action='read' )
  read(lun,rec=1) um
  close(lun)
  call libmxe_io__open( io, trim(file_base_vm), nrec, &
                      & reclen, lun, action='read' )
  read(lun,rec=1) vm
  close(lun)

  d8(:,:) = dble( hm(:,:) )
  call regrid__h2d( d8, 1, 'T', para, grid, topo, hu_cm )

  d8(:,:) = topo%aexl(:,:,1) * ( dble(um(:,:))**2 + dble(vm(:,:))**2 ) &
          & / ( hu_cm(:,:) + dble(topo%ho4(:,:)) + 1.d0 - topo%aexl(:,:,1) )

  ke_cm2ps2(nrec) = 0.5d0 * integ__area(d8,1,'U',para,grid,topo) / vol_cm3

  !- KE = (1/2) * integ_V( u**2+v**2 ) dxdydz
  !-    = (1/2) * integ_A( (um**2+vm**2)/(H+eta) ) dxdy

end subroutine calc


subroutine write_result
  use libmxe_grads, only: libmxe_grads__make
  implicit none

  call libmxe_grads__make(grads,para,grid,io)
  open(lun,file=trim(file_base_out)//'.gd',form='unformatted' &
       & ,access='direct',recl=4*nm,action='write')
    write(lun,rec=1) real(ke_cm2ps2(nrec_first:nrec_last))
  close(lun)

end subroutine write_result


real(8) function get_result(n)
  implicit none

  integer,intent(in) :: n

  get_result = ke_cm2ps2(n)

end function get_result


end module barotropic_ke_vol

