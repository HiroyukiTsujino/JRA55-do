!xy2uv.F90
!====================================================
!
!   Interpolation
!     from Ustar  Vstar points
!     to   U V points
!
!====================================================
program xy2uv
  !
  use oc_mod_param, only :  &
    &  imut, jmut, km
  !
  use oc_structure, only :  &
    &  read_topo,           &
    &  aexl
  !
  implicit none
  !
  real(4),    parameter :: UNDEF = -9.99e33
  !
  !==========================================
  !
  character(len=256)    :: flinx
  character(len=256)    :: fliny
  character(len=256)    :: flout
  character(len=256)    :: fltopo
  !
  integer(4), parameter :: mtin  = 80
  integer(4), parameter :: mtinx = 81
  integer(4), parameter :: mtiny = 82
  integer(4), parameter :: mtout = 83
  !
  real(4) :: ux(imut,jmut,km)
  real(4) :: vy(imut,jmut,km)
  !
  real(4) :: u(imut,jmut,km)
  real(4) :: v(imut,jmut,km)
  !
  integer(4) :: i, j
  integer(4) :: ios
  !
  namelist /nml_fpath/ flinx, fliny, flout, fltopo
  !
  !==========================================
  !
  read(unit=5, nml=nml_fpath, iostat=ios)
  if(ios /= 0) write(6, '(a,a,a)') 'namelist ', 'nml_fpath ', 'cannot be read.'
  !
  write(6, *) 'flinx :', trim(flinx)
  write(6, *) 'fliny :', trim(fliny)
  write(6, *) 'flout :', trim(flout)
  write(6, *) 'fltopo:', trim(fltopo)
  !
  call read_topo(fltopo)
  !------------------------------------------
  !
  ! X-point
  !
  open ( mtinx, file=flinx, form='unformatted', access='direct', recl=4*imut*jmut*km)
  read (mtinx, rec = 1)  ux(1:imut, 1:jmut, 1:km)
  close ( mtinx)
  !
  ! Y-point
  !
  open ( mtiny, file=fliny, form='unformatted', access='direct', recl=4*imut*jmut*km)
  read (mtiny, rec = 1)  vy(1:imut, 1:jmut, 1:km)
  close ( mtiny)
  !
  !---------------------------
  !
  do j=1, jmut-1
    do i=1, imut
      u(i, j, 1:km) = aexl(i, j, 1:km)*0.5*(ux(i, j, 1:km)+ux(i, j+1, 1:km))
    end do
  end do
  !
  do j=1, jmut
    do i=1, imut-1
      v(i, j, 1:km) = aexl(i, j, 1:km)*0.5*(vy(i, j, 1:km)+vy(i+1, j, 1:km))
    end do
  end do
  !
  where(aexl(1:imut, 1:jmut, 1:km) == 0.d0)
    u(1:imut, 1:jmut, 1:km) = UNDEF
    v(1:imut, 1:jmut, 1:km) = UNDEF
  end where
  !
  !---------------------------
  open ( mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*km)
  write( mtout, rec=1) u(1:imut, 1:jmut, 1:km)
  write( mtout, rec=2) v(1:imut, 1:jmut, 1:km)
  close (mtout)
  !----------------------------
end program xy2uv
!====================================================
