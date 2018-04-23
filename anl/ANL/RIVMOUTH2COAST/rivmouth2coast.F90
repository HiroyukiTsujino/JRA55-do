!rivmouth2coast.F90
!====================================================
!
! mapping point data of river transport [m^3/s] 
!                  on model coastal grid [cm/s]
!
!====================================================
program rivmouth2coast
  !
  use oc_mod_param, only : &
  &   imut, jmut,          &
  &   pi
  !
  !----------------------------------------------
  !
  implicit none
  !
  ! 入出力ファイル
  !
  logical, save :: l_initial
  !
  character(len=256)    :: file_point
  character(len=256)    :: file_coast
  character(len=256)    :: file_out
  real(4)        :: plon, plat
  integer(4)     :: nmax_pdata
  integer(4)     :: nmax_coast
  namelist /nml_rivmouth2coast/ l_initial, file_point, plon, plat, nmax_pdata, &
    &                           file_coast, nmax_coast, file_out
  !
  character(len=256)    :: file_out_ext
  integer(4), parameter :: mt_point = 81
  integer(4), parameter :: mt_coast = 82
  integer(4), parameter :: mt_out   = 83
  !
  real(4)   :: pdata
  integer(4), allocatable  :: ii(:), jj(:)
  real(4),    allocatable  :: glon(:), glat(:)
  real(4),    allocatable  :: area(:)
  real(4)   :: r4out(imut, jmut)

  integer(4) :: i, j
  integer(4) :: ic, ip, jp
  real(4)    :: areap
  real(4)    :: cos_phi, min_len, tmp_len
  !
  !==============================================
  ! read_namelist
  !==============================================
  !
  l_initial  = .false.
  file_point = 'm060_1998'
  plon = 135.4167
  plat = 34.6833
  nmax_pdata = 12
  file_coast = 'seto_00003371.d'
  nmax_coast = 3371
  file_out   = 'seto_river_m1998'
  !
  write(*,*) '=== reading namelist: nml_rivmouth2coast'
  read(unit=5, nml_rivmouth2coast)
  if(l_initial) then
    write(*,*) 'l_initial  : ', l_initial
    write(*,*) 'initialize ', trim(file_out)
  else
    write(*,*) 'file_point : ', trim(file_point)
    write(*,*) 'plon       : ', plon
    write(*,*) 'plat       : ', plat
    write(*,*) 'nmax_pdata : ', nmax_pdata
    write(*,*) 'file_coast : ', trim(file_coast)
    write(*,*) 'nmax_coast : ', nmax_coast
    write(*,*) 'file_out   : ', trim(file_out)
  end if
  !
  !----------------------------------------------
  !
  if(l_initial) then
    r4out(:, :) = 0.0
    do i = 1, nmax_pdata
      write(file_out_ext,'(a, i2.2)'), trim(file_out), nmax_pdata
      open(mt_out, file=file_out_ext, form='unformatted', access='direct', recl=4*imut*jmut)
      write(mt_out, rec=1) r4out(:, :)
      close(mt_out)
    end do
    return
  end if
  !
  allocate (ii(1:nmax_coast))
  allocate (jj(1:nmax_coast))
  allocate (glon(1:nmax_coast))
  allocate (glat(1:nmax_coast))
  allocate (area(1:nmax_coast))
  !
  open(unit=mt_coast, file=file_coast, form='unformatted')
  do i = 1, nmax_coast
    read(mt_coast) ii(i), jj(i), glon(i), glat(i), area(i)
  end do
  close(mt_coast)
  !
  min_len=1.0e30
  do i = 1, nmax_coast
    tmp_len = (cos(plat/180.0*real(pi, 4))*(glon(i)-plon))**2 + (glat(i)-plat)**2
    if(min_len > tmp_len) then
      min_len = tmp_len
      ic = i
    end if
  end do
  ip = ii(ic)
  jp = jj(ic)
  areap = area(ic)
  write(*,'(a, i7, a, i5, a, i5, a, f9.4, a, f9.4)') &
    &  'record:', ic, ', i:', ip, ', j:', jp, ', glon:', glon(ic), ', glat:', glat(ic)
  !
  open(unit=mt_point, file=file_point, form='unformatted', access='direct', recl=4)
  do i = 1, nmax_pdata
    write(file_out_ext,'(a, i2.2)'), trim(file_out), nmax_pdata
    open(unit=mt_out, file=file_out_ext, form='unformatted', access='direct', recl=4*imut*jmut)
    read(mt_point, rec = i) pdata
    read(mt_out, rec = 1) r4out(:, :)
    r4out(ip, jp) = r4out(ip, jp) + pdata * 1.e6 / areap ! [m^3/s] => [cm/s]
    write(mt_out, rec = 1) r4out(:, :)
    close(mt_out)
  end do
  close(mt_point)
  !
!====================================================
end program rivmouth2coast
