! -*-F90-*-
program diag_neutral_wind

  use libmxe_para, only: pi, radian, radian_r, radius

  implicit none

  integer(4) :: imut, jmut
  integer(4),allocatable :: num_xgrid(:)
  character(256) :: grid_name
  integer(4) :: total_grid_1d
  real(8),parameter :: tab = 273.15d0

  ! minimum value of specific humidity

  real(8), parameter :: sphmin = 2.0d-5 ! minimum of specific humidity

  integer(4) :: ibgn, iend, i0

  real(4),allocatable :: work4(:)

  real(8),allocatable :: data_u10m(:)
  real(8),allocatable :: data_v10m(:)
  real(8),allocatable :: data_wind(:)

  real(8),allocatable :: data_tmp2m(:)
  real(8),allocatable :: data_tmp2m_raw(:)
  real(8),allocatable :: data_sph2m(:)

  real(8),allocatable :: data_brtmp(:)
  real(8),allocatable :: data_slprs(:)
  real(8),allocatable :: data_ice(:)
  real(8),allocatable :: data_mask(:)
  real(8),allocatable :: data_sst_blend(:) ! based on COBESST

  real(8),allocatable :: data_un10m(:)
  real(8),allocatable :: data_vn10m(:)
  real(8),allocatable :: data_wvn10m(:)

  real(8),allocatable :: data_tmp10m(:)
  real(8),allocatable :: data_sph10m(:)

  real(8),allocatable :: u10m_latlon(:,:)
  real(8),allocatable :: v10m_latlon(:,:)

  real(8),allocatable :: u10m_org(:), u10m_new(:)
  real(8),allocatable :: v10m_org(:), v10m_new(:)

  real(8),allocatable :: brtmp_org(:), ice_org(:), mask_org(:)
  real(8),allocatable :: tmp2m_raw(:), tmp2m_org(:)
  real(8),allocatable :: sst_org(:)

  real(8),allocatable :: lon_org(:), lon_org_bound(:)
  real(8),allocatable :: lon_new(:)
  real(8) :: dlon, dlon_rg
  real(8),allocatable :: lat_org(:)

  integer(4) :: ihour

  character(256) :: file_u10m, file_v10m
  character(256) :: file_brtmp, file_tmp2m, file_sph2m
  character(256) :: file_tmp2m_raw
  character(256) :: file_ice, file_slprs
  character(256) :: file_mask

  character(256) :: file_u10m_org
  character(256) :: file_v10m_org

  character(256) :: file_u10m_latlon
  character(256) :: file_v10m_latlon
  logical :: l_out_latlon

  character(len=258) :: file_ydef

  integer(4),parameter :: lun=10

  integer(4) :: i, j, ii, jj, n

  integer(4) :: iii, m, m_left, m_right, i_left, i_right

  real(4),allocatable :: work4_2d(:,:)

  logical :: l_adjust_icesurf

  !---------------
  ! COBESST

  integer(4) :: icobe, jcobe
  real(8) :: dlon_cobe, dlat_cobe, slon_cobe, slat_cobe
  real(8) :: sst_tmp
  real(8) :: undef8_cobe
  real(4) :: undef4_cobe
  character(len=256) :: file_cobe_a, file_cobe_b
  real(8),allocatable :: sst_cobe(:,:)
  real(8),allocatable :: sst_cobe_a(:,:)
  real(8),allocatable :: sst_cobe_b(:,:)
  real(8),allocatable :: lon_cobe(:), lat_cobe(:)
  integer(4) :: irec_cobe_a, irec_cobe_b
  real(8) :: ar, br

  real(8) :: model_lat, model_lon
  real(8) :: weight, weight_total
  real(8),parameter :: eps_dist = 1.0d-8
  !---------------

  real(8) :: distance_rad

  real(8) :: dist_tmp
  real(8) :: wgt_x, wgt_y
  real(8) :: altu, altt, altq, alt_target
  integer(4) :: iw, ie, js, jn, ios

  !---------------------------------------------

  namelist /nml_jra55_neutral_wind/ &
       & file_u10m, file_v10m, &
       & file_tmp2m, file_sph2m, &
       & l_adjust_icesurf, &
       & file_tmp2m_raw, &
       & file_ice, file_brtmp, file_slprs, &
       & file_mask, &
       & ihour, &
       & file_u10m_org, &
       & file_v10m_org, &
       & file_u10m_latlon, &
       & file_v10m_latlon, &
       & l_out_latlon, & 
       & imut, jmut, dlon, grid_name, &
       & file_ydef, &
       & icobe, jcobe, &
       & slon_cobe, dlon_cobe, slat_cobe, dlat_cobe, &
       & undef4_cobe, &
       & file_cobe_a, file_cobe_b, &
       & irec_cobe_a, irec_cobe_b

  !---------------------------------------------

  l_out_latlon = .false.
  l_adjust_icesurf = .false.
  file_tmp2m_raw='no_file'

  open(lun,file='namelist.jra55_neutral_wind')
  read(lun,nml=nml_jra55_neutral_wind)
  close(lun)

  !---------------------------------------------
  ! Set COBESST

  allocate(sst_cobe(0:icobe+1,1:jcobe))
  allocate(sst_cobe_b(0:icobe+1,1:jcobe))
  allocate(sst_cobe_a(0:icobe+1,1:jcobe))
  allocate(work4_2d(1:icobe,1:jcobe))
  allocate(lon_cobe(0:icobe+1),lat_cobe(1:jcobe))

  undef8_cobe = real(undef4_cobe,8)

  do i = 0, icobe + 1
    lon_cobe(i) = slon_cobe + dlon_cobe * (i-1)
    !write(6,*) lon_cobe(i)
  end do

  do j = 1, jcobe
    lat_cobe(j) = slat_cobe + dlat_cobe * (j-1)
    !write(6,*) lat_cobe(j)
  end do

  open(lun,file=file_cobe_b,form='unformatted',access='direct',recl=4*icobe*jcobe)
  write(6,*) ' SST (B) read from ', trim(file_cobe_b), ' record = ', irec_cobe_b
  read(lun,rec=irec_cobe_b) work4_2d
  sst_cobe_b(1:icobe,1:jcobe) = real(work4_2d(1:icobe,1:jcobe),8)
  close(lun)
  sst_cobe_b(icobe+1,1:jcobe) = sst_cobe_b(1    ,1:jcobe)
  sst_cobe_b(0,      1:jcobe) = sst_cobe_b(icobe,1:jcobe)

  open(lun,file=file_cobe_a,form='unformatted',access='direct',recl=4*icobe*jcobe)
  write(6,*) ' SST (A) read from ', trim(file_cobe_a), ' record = ', irec_cobe_a
  read(lun,rec=irec_cobe_a) work4_2d
  sst_cobe_a(1:icobe,1:jcobe) = real(work4_2d(1:icobe,1:jcobe),8)
  close(lun)
  sst_cobe_a(icobe+1,1:jcobe) = sst_cobe_a(1    ,1:jcobe)
  sst_cobe_a(0,      1:jcobe) = sst_cobe_a(icobe,1:jcobe)

  deallocate(work4_2d)

  if (12 <= ihour) then
    br = 1.0d0 - real((ihour - 12),8) / 24.d0
  else
    br = 1.0d0 - real((ihour + 12),8) / 24.d0
  end if
  
  ar = 1.0d0 - br

  write(6,'(1a,i4,1a,f9.3,1a,f9.3)') 'hour = ', ihour, ' br = ', br, ' ar = ', ar

  do j = 1, jcobe
    do i = 0, icobe + 1
      if ((sst_cobe_a(i,j) /= undef8_cobe) .and. (sst_cobe_b(i,j) /= undef8_cobe)) then
        sst_cobe(i,j) = sst_cobe_b(i,j) * br + sst_cobe_a(i,j) * ar
      else
        sst_cobe(i,j) = undef8_cobe
      end if
    end do
  end do

  !---------------------------------------------
  ! set regular JRA-55 grid

  allocate(num_xgrid(1:jmut))
  allocate(lat_org(1:jmut))
  allocate(u10m_latlon(1:imut,1:jmut))
  allocate(v10m_latlon(1:imut,1:jmut))
  allocate(u10m_new(1:imut))
  allocate(v10m_new(1:imut))
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

  !----------------------------------------------------------
  ! set reduced JRA-55 grid

  call set_reduced_grid(grid_name,num_xgrid,jmut,total_grid_1d)

  allocate(work4(1:total_grid_1d))

  allocate(data_u10m(1:total_grid_1d))
  allocate(data_v10m(1:total_grid_1d))
  allocate(data_wind(1:total_grid_1d))

  allocate(data_un10m(1:total_grid_1d))
  allocate(data_vn10m(1:total_grid_1d))
  allocate(data_wvn10m(1:total_grid_1d))

  allocate(data_tmp2m(1:total_grid_1d))
  allocate(data_tmp2m_raw(1:total_grid_1d))
  allocate(data_sph2m(1:total_grid_1d))
  allocate(data_brtmp(1:total_grid_1d))
  allocate(data_slprs(1:total_grid_1d))
  allocate(data_ice  (1:total_grid_1d))
  allocate(data_mask (1:total_grid_1d))
  allocate(data_sst_blend(1:total_grid_1d))

  allocate(data_tmp10m(1:total_grid_1d))
  allocate(data_sph10m(1:total_grid_1d))

  !------

  open(lun,file=file_u10m,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_u10m(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  open(lun,file=file_v10m,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_v10m(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  do n = 1, total_grid_1d
    data_wind(n) = sqrt(data_u10m(n)**2 + data_v10m(n)**2)
  end do

  open(lun,file=file_brtmp, form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_brtmp(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  open(lun,file=file_tmp2m, form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_tmp2m(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  open(lun,file=file_sph2m, form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_sph2m(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  open(lun,file=file_slprs, form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_slprs(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  open(lun,file=file_ice, form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_ice(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  if (l_adjust_icesurf) then
    open(lun,file=file_tmp2m_raw, form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
    read(lun,rec=1) work4
    data_tmp2m_raw(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
    close(lun)
  else
    data_tmp2m_raw(1:total_grid_1d) = data_tmp2m(1:total_grid_1d)
  end if

  open(lun,file=file_mask,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_mask(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  !------

  do n = 1, total_grid_1d
    data_mask(n) = 1.d0 - data_mask(n) ! 0 for land, 1 for water
  end do

  !----------------------------------------------------------------------------
  ! water surface is filled with COBESST

  write(6,*) ' set SST..... '

  i0 = 0

  do j = 1, jmut

    model_lat = lat_org(jmut-j+1)

    ibgn = i0 + 1
    iend = i0 + num_xgrid(j)

    do jj = 2, jcobe
      if ((lat_cobe(jj-1) <= model_lat) .and. (model_lat < lat_cobe(jj))) then
        jn = jj
        js = jj - 1
        wgt_y = (model_lat - lat_cobe(js)) / (lat_cobe(jn) - lat_cobe(js))
        exit
      end if
    end do
    if (model_lat < lat_cobe(1)) then
      jn = 1
      js = 1
      wgt_y = 1.0d0
    end if
    if (lat_cobe(jcobe) <= model_lat) then
      jn = jcobe
      js = jcobe
      wgt_y = 1.0d0
    end if

    allocate(sst_org(1:num_xgrid(j)))
    allocate(brtmp_org(1:num_xgrid(j)))
    allocate(ice_org(1:num_xgrid(j)))
    allocate(tmp2m_raw(1:num_xgrid(j)))
    allocate(tmp2m_org(1:num_xgrid(j)))
    allocate(mask_org(1:num_xgrid(j)))
    allocate(lon_org(1:num_xgrid(j)))

    brtmp_org(1:num_xgrid(j)) = data_brtmp(ibgn:iend)
    ice_org  (1:num_xgrid(j)) = data_ice  (ibgn:iend)
    mask_org (1:num_xgrid(j)) = data_mask (ibgn:iend)
    tmp2m_raw(1:num_xgrid(j)) = data_tmp2m_raw(ibgn:iend)
    tmp2m_org(1:num_xgrid(j)) = data_tmp2m(ibgn:iend)

    dlon_rg = 360.0 / real(num_xgrid(j),8)

    do i = 1, num_xgrid(j)
      lon_org(i) = dlon_rg * real(i-1,8)
    end do

    do i = 1, num_xgrid(j)
      model_lon = lon_org(i)
      loop_cobe_i: do ii = 1, icobe + 1
        if ((lon_cobe(ii-1) <= model_lon) .and. (model_lon < lon_cobe(ii))) then
          iw = ii - 1
          ie = ii
          wgt_x = (model_lon - lon_cobe(iw)) / (lon_cobe(ie) - lon_cobe(iw))

          if ( (sst_cobe(iw,js) /= undef8_cobe) .and. &
             & (sst_cobe(ie,js) /= undef8_cobe) .and. &
             & (sst_cobe(iw,jn) /= undef8_cobe) .and. &
             & (sst_cobe(ie,jn) /= undef8_cobe)) then
            sst_tmp = (1.0d0-wgt_x)*(1.0d0-wgt_y)*sst_cobe(iw,js) &
                 & +         wgt_x *(1.0d0-wgt_y)*sst_cobe(ie,js) &
                 & +  (1.0d0-wgt_x)*       wgt_y *sst_cobe(iw,jn) &
                 & +         wgt_x *       wgt_y *sst_cobe(ie,jn)
          else
            if (lon_cobe(iw) == model_lon) then
              ie = iw
            end if
            if (lat_cobe(js) == model_lat) then
              jn = js
            end if
            weight_total = 0.0d0
            sst_tmp = 0.0d0
            if (sst_cobe(iw,js) /= undef8_cobe) then
              dist_tmp = distance_rad(lon_cobe(iw),lat_cobe(js),model_lon,model_lat)
              if (dist_tmp > radian_r * 1.5d0) then
                write(6,*) ' erroneous distance : ', dist_tmp
                write(6,*) lon_cobe(iw),lat_cobe(js),model_lon,model_lat
                stop
              end if
              weight = 1.0d0 / (dist_tmp + eps_dist)
              weight_total = weight_total + weight
              sst_tmp = sst_tmp + sst_cobe(iw,js) * weight
            end if
            if (sst_cobe(ie,js) /= undef8_cobe) then
              dist_tmp = distance_rad(lon_cobe(ie),lat_cobe(js),model_lon,model_lat)
              if (dist_tmp > radian_r * 1.5d0) then
                write(6,*) ' erroneous distance : ', dist_tmp
                write(6,*) lon_cobe(ie),lat_cobe(js),model_lon,model_lat
                stop
              end if
              weight = 1.0d0 / (dist_tmp + eps_dist)
              weight_total = weight_total + weight
              sst_tmp = sst_tmp + sst_cobe(ie,js) * weight
            end if
            if (sst_cobe(iw,jn) /= undef8_cobe) then
              dist_tmp = distance_rad(lon_cobe(iw),lat_cobe(jn),model_lon,model_lat)
              if (dist_tmp > radian_r * 1.5d0) then
                write(6,*) ' erroneous distance : ', dist_tmp
                write(6,*) lon_cobe(iw),lat_cobe(jn),model_lon,model_lat
                stop
              end if
              weight = 1.0d0 / (dist_tmp + eps_dist)
              weight_total = weight_total + weight
              sst_tmp = sst_tmp + sst_cobe(iw,jn) * weight
            end if
            if (sst_cobe(ie,jn) /= undef8_cobe) then
              dist_tmp = distance_rad(lon_cobe(ie),lat_cobe(jn),model_lon,model_lat)
              if (dist_tmp > radian_r * 1.5d0) then
                write(6,*) ' erroneous distance : ', dist_tmp
                write(6,*) lon_cobe(ie),lat_cobe(jn),model_lon,model_lat
                stop
              end if
              weight = 1.0d0 / (dist_tmp + eps_dist)
              weight_total = weight_total + weight
              sst_tmp = sst_tmp + sst_cobe(ie,jn) * weight
            end if
            if (weight_total > 0.0d0) then
              sst_tmp = sst_tmp / weight_total
            else
              sst_tmp = undef8_cobe
            end if
          end if
          exit loop_cobe_i
        end if
      end do loop_cobe_i
      
      if ((ice_org(i) == 0.0d0) .and. (mask_org(i) > 0.0d0) .and. (sst_tmp /= undef8_cobe)) then
        ! ice free ocean surface
        sst_org(i) = sst_tmp + tab ! [K]
      else
        ! ice covered region or land or COBESST is missing
        if ((ice_org(i) > 0.0d0)  .and. (mask_org(i) > 0.0d0) .and. l_adjust_icesurf) then
          ! Offset "sea ice" surface temperature with 2m temperature
          ! because sea ice thickness is suspicious.
          ! By this operation, the original stability in terms of temeparature is kept.
          sst_org(i) = brtmp_org(i) + tmp2m_org(i) - tmp2m_raw(i)
          !if (i == num_xgrid(j)/2) then
          !  write(6,*) ' brtmp ', brtmp_org(i), ' -------> ', sst_org(i)
          !  write(6,*) '       tmp2m ', tmp2m_raw(i), ' -------> ', tmp2m_org(i)
          !end if
        else
          sst_org(i) = brtmp_org(i)
        end if
      end if

    end do

    data_sst_blend(ibgn:iend) = sst_org(1:num_xgrid(j))

    deallocate(sst_org)
    deallocate(ice_org,brtmp_org,mask_org)
    deallocate(tmp2m_org,tmp2m_raw)
    deallocate(lon_org)

    i0 = iend
    !write(6,*) j, i0

  end do

  write(6,*) ' .... done '
  !------------------------------------------------------------------------

  altu = 10.0d0
  altt = 2.0d0
  altq = 2.0d0
  alt_target = 10.0d0

  call shift_wind_real2neutral( &
       & data_wvn10m, data_tmp10m, data_sph10m, &
       & data_wind, data_tmp2m, data_sph2m, data_slprs, &
       & data_sst_blend, data_ice, &
       & total_grid_1d, 1, data_mask, &
       & altu, altt, altq, alt_target, sphmin)

  do n = 1, total_grid_1d
!    if ((data_ice(n) == 0.0d0) .and. (data_mask(n) > 0.0d0)) then
!    if (data_mask(n) > 0.0d0) then
    if (data_wind(n) > 0.0d0) then
      data_un10m(n) = data_u10m(n) * data_wvn10m(n) / data_wind(n)
      data_vn10m(n) = data_v10m(n) * data_wvn10m(n) / data_wind(n)
    else
      data_un10m(n) = data_u10m(n)
      data_vn10m(n) = data_v10m(n)
    end if
  end do

  write(6,*) 'Product written to ', trim(file_u10m_org)
  write(6,*) 'Product written to ', trim(file_v10m_org)

  open(lun,file=file_u10m_org,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  write(lun,rec=1) real(data_un10m,4)
  close(lun)

  open(lun,file=file_v10m_org,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  write(lun,rec=1) real(data_vn10m,4)
  close(lun)

  !------------------------------------------------------------------------
  ! reduced grid to lat-lon grid for check

  if (l_out_latlon) then

    i0 = 0

    do j = 1, jmut

      ibgn = i0 + 1
      iend = i0 + num_xgrid(j)

      !write(6,*) j, ibgn, iend

      if (num_xgrid(j) == imut) then

        u10m_latlon(1:imut,jmut-j+1) = data_un10m(ibgn:iend)
        v10m_latlon(1:imut,jmut-j+1) = data_vn10m(ibgn:iend)

      else

        allocate(u10m_org(1:num_xgrid(j)+1))
        allocate(v10m_org(1:num_xgrid(j)+1))
        allocate(lon_org (1:num_xgrid(j)+1))

        u10m_org(1:num_xgrid(j)) = data_un10m(ibgn:iend)
        u10m_org(num_xgrid(j)+1) = data_un10m(ibgn)
        v10m_org(1:num_xgrid(j)) = data_vn10m(ibgn:iend)
        v10m_org(num_xgrid(j)+1) = data_vn10m(ibgn)

        dlon_rg = 360.0 / real(num_xgrid(j),8)
        do ii = 1, num_xgrid(j) + 1
          lon_org(ii) = dlon_rg * real(ii-1,8)
        end do
        do i = 1, imut
          do ii = 1, num_xgrid(j)
            if (abs(lon_org(ii) - lon_new(i)) < 1.0d-4) then
              u10m_new(i) = u10m_org(ii)
              v10m_new(i) = v10m_org(ii)
              exit
            else if ( (lon_org(ii) < lon_new(i)) .and. (lon_new(i) <= lon_org(ii+1)) ) then
              weight = (lon_new(i) - lon_org(ii)) / (lon_org(ii+1) - lon_org(ii))
              u10m_new(i) = (1.0d0 - weight) * u10m_org(ii) + weight * u10m_org(ii+1)
              v10m_new(i) = (1.0d0 - weight) * v10m_org(ii) + weight * v10m_org(ii+1)
              exit
            end if
          end do
        end do
        u10m_latlon(1:imut,jmut-j+1) = u10m_new(1:imut)
        v10m_latlon(1:imut,jmut-j+1) = v10m_new(1:imut)
        deallocate(u10m_org,v10m_org)
        deallocate(lon_org)
      end if

      i0 = iend

    end do

    open (lun,file=file_u10m_latlon,form='unformatted',access='direct',recl=4*imut*jmut)
    write(lun,rec=1) real(u10m_latlon,4)
    close(lun)

    open (lun,file=file_v10m_latlon,form='unformatted',access='direct',recl=4*imut*jmut)
    write(lun,rec=1) real(v10m_latlon,4)
    close(lun)

  end if

end program diag_neutral_wind
