! -*-F90-*-
module ts2sigma
  use libmxe_para, only: clen, type_libmxe_para
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo
  use libmxe_io, only: type_libmxe_io
  use libmxe_grads, only: type_grads
  implicit none
  private


  !-- arguments --
  character(clen),save :: file_base_t    !- potential temperature
  character(clen),save :: file_base_s    !- salinity
  character(clen),save :: dir_out        !- output directory
  character(clen),save :: file_out       !- output file name
  real(4),save         :: undef_out      !- undefined value
                                         !-  [default:rundefout or rundef]
  integer,save         :: nrec_first = 1 !- first record
  integer,save         :: nrec_last      !- last record [default:nm]
  real(8),save         :: reference_depth_m = 0.d0

  public :: ini
  public :: calc
  public :: get_result
  public :: write_result
  public :: next
  public :: has_next

  integer,parameter    :: lun = 80

  type(type_libmxe_para),save :: para
  type(type_libmxe_grid),save :: grid
  type(type_libmxe_topo),save :: topo
  type(type_libmxe_io),save :: io
  type(type_grads),save :: grads

  integer,save         :: im, jm, km, nm, nrec, reclen
  real(8),allocatable  :: t8(:,:,:), s8(:,:,:), pressure_bar(:)
  real(8),allocatable  :: sigma_kgpm3(:,:,:)
  real,allocatable     :: t(:,:,:), s(:,:,:)

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
  use libmxe_para,  only: libmxe_para__register, rundef
  use libmxe_grid,  only: libmxe_grid__register
  use libmxe_topo,  only: libmxe_topo__register, libmxe_topo__aexl
  use libmxe_io,    only: libmxe_io__register
  use libmxe_grads, only: libmxe_grads__make
  implicit none

  real(4),parameter :: undef_default = -9.87d33

  integer :: i
  character(4) :: ctemp

  namelist /ts2sigma_lst/ file_base_t file_base_s, &
            & dir_out, file_out, undef_out, nrec_first, nrec_last, &
            & reference_depth_m

  undef_out = undef_default
  nrec_last = 0
  read(5,nml=ts2sigma_lst,iostat=i)
  if ( i /= 0 ) then
    write(*,*) ' Read error : namelist'
    stop
  endif

  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)
  call libmxe_topo__aexl(topo,para)
  call libmxe_io__register(io,para)

  if ( undef_out == undef_default ) undef_out = para%rundefout
  im = para%imut
  jm = para%jmut
  km = para%km
  reclen = im * jm * km * 4
  if ( nrec_last == 0 ) nrec_last = io%nm
  if ( ( nrec_first < 1).or.( nrec_first > io%nm ) &
       & .or.( nrec_last < 1).or.( nrec_last > io%nm ) ) then
    write(6,*) 'Error at ini'
    write(6,*) '  nrec_first = ',nrec_first
    write(6,*) '  nrec_last  = ',nrec_last
    stop
  endif

  !-- grads control file --
  grads%file_base = trim(file_out)
  write(ctemp,'(i4)') int(reference_depth_m)
  grads%title = 'Sigma [kg/m^3] reference depth:'//trim(ctemp)//' [m]'
  grads%cgrid = 'T'
  grads%ztype = 'center'
  grads%nvar = 1
  write(ctemp,'(i4)') km
  grads%var(1) = 'd '//trim(ctemp)//' 99 Sigma [kg/m^3]'
  call libmxe_grads__make(grads,para,grid,io)

  allocate(pressure_bar(km))
  pressure_bar(:) = 0.1d0 * reference_depth_m

  allocate(t(im,jm,km),s(im,jm,km))
  allocate(t8(im,jm,km),s8(im,jm,km))
  allocate(sigma_kgpm3(im,jm,km))

  nrec = nrec_first

end subroutine ini


subroutine calc
  use libmxe_io, only: libmxe_io__open
  use density,   only: dens
  implicit none

  integer :: k

  if ( mod(nrec,100) == 0 ) write(6,*) nrec

  call libmxe_io__open(io,trim(file_base_t),nrec,reclen &
       & ,lun,action='read')
  read(lun,rec=1) t
  close(lun)
  call libmxe_io__open(io,trim(file_base_s),nrec,reclen &
       & ,lun,action='read')
  read(lun,rec=1) s
  close(lun)

  t8(:,:,:) = dble(t(:,:,:)) * topo%atexl(:,:,:)
  s8(:,:,:) = dble(s(:,:,:)) * topo%atexl(:,:,:)

  call dens(im, jm, km, t8, s8, pressure_bar, sigma_kgpm3 )

  sigma_kgpm3(:,:,:) = 1.d3 * sigma_kgpm3(:,:,:) * topo%atexl(:,:,:) &
                    &  + undef_out * ( 1.d0 - topo%atexl(:,:,:) )

end subroutine calc


subroutine write_result
  use libmxe_io, only: libmxe_io__open
  implicit none

  call libmxe_io__open(io,trim(dir_out)//'/'//trim(file_out) &
                  & , nrec, reclen, lun, action='write')
    write(lun,rec=1) real(sigma_kgpm3(:,:,:))
  close(lun)

end subroutine write_result


real(4) function get_result(nx,ny,nz)
  implicit none

  integer,intent(in) :: nx,ny,nz

  get_result = real( sigma_kgpm3(nx,ny,nz) )

end function get_result


end module ts2sigma

