! -*-F90-*-
program zonal_mean_on_jra55_grid

  use libmxe_para, only: pi, radian, radian_r, radius

  implicit none

  integer(4) :: imut, jmut
  integer(4),allocatable :: num_xgrid(:)
  character(256) :: grid_name
  integer(4) :: total_grid_1d

  integer(4) :: ibgn, iend, i0

  real(4),allocatable :: work4(:)

  real(8),allocatable :: data_org(:)
  real(8),allocatable :: data_mask(:)

  real(8),allocatable :: data_zm(:)
  real(8),allocatable :: data_zm_out(:)

  real(8),allocatable :: data_azimuth(:)
  real(8),allocatable :: mask_azimuth(:)

  real(8),allocatable :: lon_org(:), lon_org_bound(:)
  real(8) :: dlon, dlon_rg
  real(8),allocatable :: lat_org(:), lat_org_bound(:)

  character(256) :: file_in
  character(256) :: file_mask
  character(256) :: file_out

  real(4) :: undef_in, undef_out
  real(8) :: undef8_in, undef8_out

  character(len=258) :: file_ydef

  integer(4),parameter :: lun=10

  integer(4) :: i, j, ii, jj, n, ios

  integer(4) :: n_valid, n_ocean, itmp
  real(8) :: frac_valid

  !---------------------------------------------

  namelist /nml_zonalmean_on_jra55/ &
       & file_in, &
       & undef_in, &
       & file_mask, &
       & file_out, &
       & undef_out, &
       & imut, jmut, dlon, grid_name, &
       & file_ydef, &
       & frac_valid

  !---------------------------------------------

  frac_valid = 0.0d0
  open(lun,file='namelist.zonalmean_on_jra55')
  read(lun,nml=nml_zonalmean_on_jra55)
  close(lun)

  undef8_in = real(undef_in,8)
  undef8_out = real(undef_out,8)

  !---------------------------------------------
  ! set regular JRA-55 grid

  allocate(num_xgrid(1:jmut))
  allocate(data_zm(1:jmut))
  allocate(data_zm_out(1:jmut))
  allocate(lat_org(1:jmut))
  allocate(lat_org_bound(1:jmut+1))

  open(lun,file=file_ydef)
  do j = 1, jmut
    read(lun,*,iostat=ios) lat_org(j)
    if (ios /= 0) then
      write(6,*) ' Error : inconsistent number of data ', jmut
      stop
    else
      !write(6,*) lat_org(j)
    end if
  end do
  close(lun)

  lat_org_bound(1) = - 90.0d0
  do j = 2, jmut
    lat_org_bound(j) = 0.5d0 * (lat_org(j-1) + lat_org(j))
  end do
  lat_org_bound(jmut+1) = 90.0d0

  !----------------------------------------------------------
  ! set reduced JRA-55 grid

  call set_reduced_grid(grid_name,num_xgrid,jmut,total_grid_1d)

  !write(6,*) 'total JRA-55 grid = ', total_grid_1d
  !write(6,*) 'regular JRA-55 grid = ', imut, jmut

  !--------------------------------

  write(6,*) 'READ JRA-55 data '

  allocate(work4(1:total_grid_1d))
  allocate(data_org(1:total_grid_1d))
  allocate(data_mask(1:total_grid_1d))

  open(lun,file=file_in,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_org(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  open(lun,file=file_mask,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_mask(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  !------

  do n = 1, total_grid_1d
    data_mask(n) = 1.d0 - data_mask(n) ! 0 for land, 1 for water
  end do

  !----------------------------------------------------------------------------

  i0 = 0

  do j = 1, jmut

    ibgn = i0 + 1
    iend = i0 + num_xgrid(j)

    allocate(data_azimuth(1:num_xgrid(j)))
    allocate(mask_azimuth(1:num_xgrid(j)))

    data_azimuth(1:num_xgrid(j)) = data_org (ibgn:iend)
    mask_azimuth(1:num_xgrid(j)) = data_mask(ibgn:iend)

    n_valid = 0
    n_ocean = 0
    data_zm(j) = 0.0d0
    do i = 1, num_xgrid(j)
      if (mask_azimuth(i) == 1.0d0) then
        n_ocean = n_ocean + 1
        if (data_azimuth(i) /= undef8_in) then
          n_valid = n_valid + 1
          data_zm(j) = data_zm(j) + data_azimuth(i)
        end if
      end if
    end do

    itmp = int(n_ocean * frac_valid) + 1
    if (n_valid > itmp) then
      data_zm(j) = data_zm(j) / real(n_valid,8)
    else
      data_zm(j) = undef8_out
    end if

    deallocate(data_azimuth)
    deallocate(mask_azimuth)

    i0 = iend

  end do

  !-------------------------------------------------------------

  write(6,*) 'Product written to ', trim(file_out)
  do j = 1, jmut
    data_zm_out(j) = data_zm(jmut-j+1)
  end do

  open(lun,file=file_out,form='unformatted',access='direct',recl=4*jmut)
  write(lun,rec=1) real(data_zm_out,4)
  close(lun)

  !-------------------------------------------------------------

end program zonal_mean_on_jra55_grid
