! -*-F90-*-
program remss_wind_on_jra55_grid

  use libmxe_para, only: pi, radian, radian_r, radius

  implicit none

  integer(4) :: imut, jmut
  integer(4),allocatable :: num_xgrid(:)
  character(256) :: grid_name
  integer(4) :: total_grid_1d

  integer(4) :: ibgn, iend, i0

  real(4),allocatable :: work4(:)

  real(8),allocatable :: data_wind(:) ! anl_surf

  real(8),allocatable :: data_wind_blend(:)

  real(8),allocatable :: data_mask(:)

  real(8),allocatable :: data_remss_avail(:)

  real(8),allocatable :: wind_org(:), wind_new(:)

  real(8),allocatable :: mask_org(:)

  real(8),allocatable :: wind_blend_org(:)
  real(8),allocatable :: remss_avail_org(:)

  real(8),allocatable :: lon_org(:), lon_org_bound(:)
  real(8),allocatable :: lon_new(:)
  real(8) :: dlon, dlon_rg
  real(8),allocatable :: lat_org(:), lat_org_bound(:)

  character(256) :: file_wind
  character(256) :: file_mask

  character(256) :: file_wind_blend_org

  character(len=258) :: file_ydef

  integer(4),parameter :: lun=10

  integer(4) :: i, j, ii, jj, n

  integer(4) :: iii, m, m_left, m_right, i_left, i_right

  real(4),allocatable :: work4_2d(:,:)

  !---------------
  ! QuikSCAT

  integer(4) :: iremss, jremss
  real(8) :: dlon_remss, dlat_remss, slon_remss, slat_remss
  real(8) :: undef8_remss
  real(8) :: wind_tmp
  real(8) :: model_lat_n, model_lat_s, model_lat
  real(8) :: model_lon_e, model_lon_w, model_lon
  real(4) :: undef4_remss
  character(len=256) :: file_remss
  real(8),allocatable :: wind_remss(:,:)
  real(8),allocatable :: lon_remss(:), lat_remss(:)

  !---------------
  ! Interpolation/Extrapolation

  integer(4),parameter :: max_bound = 31
  integer(4),allocatable :: ia(:,:)
  integer(4),allocatable :: iaq(:,:,:)
  integer(4),allocatable :: jsq(:), jnq(:)
  integer(4) :: i_in
  real(8) :: weight, weight_total
  real(8),parameter :: eps_dist = 1.0d-8
  integer(4) :: num_extrap

  !---------------

  real(8) :: distance_rad
  real(8) :: dist_tmp
  real(8) :: wgt_x, wgt_y
  real(8) :: altu, altt, altq, alt_target
  integer(4) :: iw, ie, js, jn, ios

  !---------------------------------------------

  namelist /nml_remss_on_jra55/ &
       & file_wind, &
       & file_mask, &
       & file_wind_blend_org, &
       & imut, jmut, dlon, grid_name, &
       & file_ydef, &
       & iremss, jremss, &
       & slon_remss, dlon_remss, slat_remss, dlat_remss, &
       & undef4_remss, &
       & file_remss

  !---------------------------------------------

  open(lun,file='namelist.remsswind_on_jra55')
  read(lun,nml=nml_remss_on_jra55)
  close(lun)

  !---------------------------------------------
  ! Set QuikSCAT

  allocate(work4_2d(1:iremss,1:jremss))
  allocate(wind_remss(0:iremss+1,1:jremss))
  allocate(lon_remss(0:iremss+1),lat_remss(1:jremss))

  undef8_remss = real(undef4_remss,8)

  do i = 0, iremss + 1 ! assume cyclic
    lon_remss(i) = slon_remss + dlon_remss * (i-1)
    !write(6,*) lon_remss(i)
  end do

  do j = 1, jremss
    lat_remss(j) = slat_remss + dlat_remss * (j-1)
    !write(6,*) lat_remss(j)
  end do

  open(lun,file=file_remss,form='unformatted',access='direct',recl=4*iremss*jremss)
  read(lun,rec=1) work4_2d
  wind_remss(1:iremss,1:jremss) = real(work4_2d(1:iremss,1:jremss),8)
  close(lun)
  wind_remss(0       ,1:jremss) = wind_remss(iremss,1:jremss) 
  wind_remss(iremss+1,1:jremss) = wind_remss(1     ,1:jremss) 

  deallocate(work4_2d)

  !---------------------------------------------
  ! set regular JRA-55 grid

  allocate(num_xgrid(1:jmut))
  allocate(lat_org(1:jmut))
  allocate(lat_org_bound(1:jmut+1))
  allocate(wind_new(1:imut))
  allocate(lon_new(1:imut))

  do i = 1, imut
    lon_new(i) = dlon * real(i-1,8)
  end do

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

  allocate(data_wind(1:total_grid_1d))

  allocate(data_mask(1:total_grid_1d))
  allocate(data_remss_avail(1:total_grid_1d))

  allocate(data_wind_blend(1:total_grid_1d))

  !------
  open(lun,file=file_wind,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_wind(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
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
  ! Bi-linear interpolation, if possible.

  write(6,*) ' bi-linear interpolation for wind '

  i0 = 0

  data_remss_avail(1:total_grid_1d) = 0.0d0

  do j = 1, jmut

    model_lat = lat_org(jmut-j+1)  ! JRA-55 grid (YREV 1:north)

    ibgn = i0 + 1
    iend = i0 + num_xgrid(j)

    do jj = 2, jremss
      if ((lat_remss(jj-1) <= model_lat) .and. (model_lat < lat_remss(jj))) then
        jn = jj
        js = jn - 1
        wgt_y = (model_lat - lat_remss(js)) / (lat_remss(jn) - lat_remss(js))
        exit
      end if
    end do
    if (model_lat < lat_remss(1)) then
      jn = 1
      js = 1
      wgt_y = 1.0d0
    end if
    if (model_lat > lat_remss(jremss)) then
      jn = jremss
      js = jremss
      wgt_y = 1.0d0
    end if

    allocate(wind_org(1:num_xgrid(j)))
    allocate(mask_org(1:num_xgrid(j)))
    allocate(wind_blend_org(1:num_xgrid(j)))
    allocate(lon_org(1:num_xgrid(j)))
    allocate(remss_avail_org(1:num_xgrid(j)))

    wind_org(1:num_xgrid(j)) = data_wind(ibgn:iend)
    mask_org(1:num_xgrid(j)) = data_mask(ibgn:iend)
    remss_avail_org(1:num_xgrid(j)) = 0.0d0

    dlon_rg = 360.0 / real(num_xgrid(j),8)

    do i = 1, num_xgrid(j)
      lon_org(i) = dlon_rg * real(i-1,8)
    end do

    do i = 1, num_xgrid(j)
      loop_remss_i: do ii = 1, iremss + 1
        if ( (lon_remss(ii-1) <= lon_org(i)) .and. (lon_org(i) < lon_remss(ii)) ) then
          if (ii > 1) then 
            iw = ii - 1
            ie = ii
            wgt_x = (lon_org(i) - lon_remss(iw)) / (lon_remss(ie) - lon_remss(iw))
          end if

          if ( (wind_remss(iw,js) /= undef8_remss) .and. &
             & (wind_remss(ie,js) /= undef8_remss) .and. &
             & (wind_remss(iw,jn) /= undef8_remss) .and. &
             & (wind_remss(ie,jn) /= undef8_remss)) then
            wind_tmp = (1.0d0-wgt_x)*(1.0d0-wgt_y)*wind_remss(iw,js) &
                 & +          wgt_x *(1.0d0-wgt_y)*wind_remss(ie,js) &
                 & +   (1.0d0-wgt_x)*       wgt_y *wind_remss(iw,jn) &
                 & +          wgt_x *       wgt_y *wind_remss(ie,jn)
          else
            wind_tmp = undef8_remss
          end if
          exit loop_remss_i
        end if
      end do loop_remss_i
      
      if ((mask_org(i) > 0.0d0) .and. (wind_tmp /= undef8_remss)) then
        wind_blend_org(i) = wind_tmp
        remss_avail_org(i) = 1.0d0
      else
        wind_blend_org(i) = wind_org(i)
      end if

    end do

    data_wind_blend (ibgn:iend) = wind_blend_org (1:num_xgrid(j))
    data_remss_avail(ibgn:iend) = remss_avail_org(1:num_xgrid(j))

    deallocate(wind_org)
    deallocate(mask_org)
    deallocate(wind_blend_org)
    deallocate(lon_org)
    deallocate(remss_avail_org)

    i0 = iend

  end do

  !------------------------------------------------------------------------

  do n = 1, total_grid_1d
    if (data_remss_avail(n) /= 1.0d0) then
      data_wind_blend(n) = undef8_remss
    end if
  end do

  !-------------------------------------------------------------

  write(6,*) 'Product written to ', trim(file_wind_blend_org)

  open(lun,file=file_wind_blend_org,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  write(lun,rec=1) real(data_wind_blend,4)
  close(lun)

end program remss_wind_on_jra55_grid
