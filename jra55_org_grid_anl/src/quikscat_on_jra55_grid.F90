! -*-F90-*-
program quikscat_on_jra55_grid

  use libmxe_para, only: pi, radian, radian_r, radius

  implicit none

  integer(4) :: imut, jmut
  integer(4),allocatable :: num_xgrid(:)
  character(256) :: grid_name
  integer(4) :: total_grid_1d
  real(8),parameter :: tab = 273.15d0

  integer(4) :: ibgn, iend, i0

  real(4),allocatable :: work4(:)

  real(8),allocatable :: data_u10m(:) ! anl_surf
  real(8),allocatable :: data_v10m(:) ! anl_surf
  real(8),allocatable :: data_wind(:) ! anl_surf

  real(8),allocatable :: data_u10m_blend(:)
  real(8),allocatable :: data_v10m_blend(:)
  real(8),allocatable :: data_wind_blend(:)
  real(8),allocatable :: data_u10m_neutral(:)
  real(8),allocatable :: data_v10m_neutral(:)
  real(8),allocatable :: data_wind_neutral(:)

  real(8),allocatable :: data_tmp2m(:) ! anl_surf
  real(8),allocatable :: data_sph2m(:) ! anl_surf

  real(8),allocatable :: data_brtmp(:) ! fcst_surf
  real(8),allocatable :: data_slprs(:) ! fcst_surf
  real(8),allocatable :: data_ice(:)   ! ice
  real(8),allocatable :: data_mask(:)
  real(8),allocatable :: data_sst_blend(:) ! based on COBESST

  real(8),allocatable :: data_tmp10m(:)
  real(8),allocatable :: data_sph10m(:)

  real(8),allocatable :: data_qscat_avail(:)

  real(8),allocatable :: u10m_org(:), u10m_new(:)
  real(8),allocatable :: v10m_org(:), v10m_new(:)
  real(8),allocatable :: wind_org(:), wind_new(:)

  real(8),allocatable :: brtmp_org(:), sst_org(:), ice_org(:), mask_org(:)

  real(8),allocatable :: u10m_blend_org(:)
  real(8),allocatable :: v10m_blend_org(:)
  real(8),allocatable :: wind_blend_org(:)
  real(8),allocatable :: qscat_avail_org(:)

  real(8),allocatable :: lon_org(:), lon_org_bound(:)
  real(8),allocatable :: lon_new(:)
  real(8) :: dlon, dlon_rg
  real(8),allocatable :: lat_org(:), lat_org_bound(:)

  character(256) :: file_u10m, file_v10m, file_wind
  character(256) :: file_brtmp, file_tmp2m, file_sph2m
  character(256) :: file_ice, file_slprs
  character(256) :: file_mask

  character(256) :: file_u10m_blend_org
  character(256) :: file_v10m_blend_org
  character(256) :: file_wind_blend_org

  character(256) :: file_u10m_neutral_org
  character(256) :: file_v10m_neutral_org
  character(256) :: file_wind_neutral_org

  character(len=258) :: file_ydef

  integer(4),parameter :: lun=10

  integer(4) :: i, j, ii, jj, n

  integer(4) :: iii, m, m_left, m_right, i_left, i_right

  real(4),allocatable :: work4_2d(:,:)

  !---------------
  ! QuikSCAT

  integer(4) :: iqscat, jqscat
  real(8) :: dlon_qscat, dlat_qscat, slon_qscat, slat_qscat
  real(8) :: undef8_qscat
  real(8) :: u10m_tmp, v10m_tmp, wind_tmp
  real(8) :: model_lat_n, model_lat_s, model_lat
  real(8) :: model_lon_e, model_lon_w, model_lon
  real(4) :: undef4_qscat
  character(len=256) :: file_qscat
  real(8),allocatable :: u10m_qscat(:,:)
  real(8),allocatable :: v10m_qscat(:,:)
  real(8),allocatable :: wind_qscat(:,:)
  real(8),allocatable :: lon_qscat(:), lat_qscat(:)

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
  ! COBESST

  integer(4) :: icobe, jcobe
  real(8) :: dlon_cobe, dlat_cobe, slon_cobe, slat_cobe
  real(8) :: sst_tmp
  real(8) :: undef8_cobe
  real(4) :: undef4_cobe
  character(len=256) :: file_cobe
  real(8),allocatable :: sst_cobe(:,:)
  real(8),allocatable :: lon_cobe(:), lat_cobe(:)
  integer(4) :: irec_cobe

  !---------------

  real(8) :: distance_rad
  real(8) :: dist_tmp
  real(8) :: wgt_x, wgt_y
  real(8) :: altu, altt, altq, alt_target
  integer(4) :: iw, ie, js, jn, ios

  !---------------------------------------------

  namelist /nml_qscat_on_jra55/ &
       & file_u10m, file_v10m, file_wind, &
       & file_tmp2m, file_sph2m, &
       & file_ice, file_brtmp, file_slprs, &
       & file_mask, &
       & file_u10m_blend_org, &
       & file_v10m_blend_org, &
       & file_wind_blend_org, &
       & file_u10m_neutral_org, &
       & file_v10m_neutral_org, &
       & file_wind_neutral_org, &
       & imut, jmut, dlon, grid_name, &
       & file_ydef, &
       & iqscat, jqscat, &
       & slon_qscat, dlon_qscat, slat_qscat, dlat_qscat, &
       & undef4_qscat, &
       & file_qscat, &
       & icobe, jcobe, &
       & slon_cobe, dlon_cobe, slat_cobe, dlat_cobe, &
       & undef4_cobe, &
       & file_cobe, &
       & irec_cobe

  !---------------------------------------------

  open(lun,file='namelist.quikscat_on_jra55')
  read(lun,nml=nml_qscat_on_jra55)
  close(lun)

  !---------------------------------------------
  ! Set QuikSCAT

  allocate(u10m_qscat(1:iqscat,1:jqscat))
  allocate(v10m_qscat(1:iqscat,1:jqscat))
  allocate(wind_qscat(1:iqscat,1:jqscat))
  allocate(work4_2d(1:iqscat,1:jqscat))
  allocate(lon_qscat(1:iqscat),lat_qscat(1:jqscat))

  undef8_qscat = real(undef4_qscat,8)

  do i = 1, iqscat
    lon_qscat(i) = slon_qscat + dlon_qscat * (i-1)
    !write(6,*) lon_qscat(i)
  end do

  do j = 1, jqscat
    lat_qscat(j) = slat_qscat + dlat_qscat * (j-1)
    !write(6,*) lat_qscat(j)
  end do

  open(lun,file=file_qscat,form='unformatted',access='direct',recl=4*iqscat*jqscat)
  read(lun,rec=1) work4_2d
  wind_qscat(1:iqscat,1:jqscat) = real(work4_2d(1:iqscat,1:jqscat),8)
  read(lun,rec=2) work4_2d
  u10m_qscat(1:iqscat,1:jqscat) = real(work4_2d(1:iqscat,1:jqscat),8)
  read(lun,rec=3) work4_2d
  v10m_qscat(1:iqscat,1:jqscat) = real(work4_2d(1:iqscat,1:jqscat),8)
  close(lun)

  ! check consistency of data

  do j = 1, jqscat
    do i = 1, iqscat
      if (wind_qscat(i,j) == undef8_qscat) then
        if ((u10m_qscat(i,j) /= undef8_qscat) &
     & .or. (v10m_qscat(i,j) /= undef8_qscat)) then
          write(6,*) ' QuikSCAT data is grid-wise inconsistent (type1), '
          write(6,*) ' Please check. '
          write(6,*) ' location is ', lon_qscat(i), lat_qscat(j)
          stop 999
        end if
      end if
      if (wind_qscat(i,j) /= undef8_qscat) then
        if ((u10m_qscat(i,j) == undef8_qscat) &
     & .or. (v10m_qscat(i,j) == undef8_qscat)) then
          write(6,*) ' QuikSCAT data is grid-wise inconsistent (type2), '
          write(6,*) ' Please check. '
          write(6,*) ' location is ', lon_qscat(i), lat_qscat(j)
          stop 999
        end if
      end if
    end do
  end do

  deallocate(work4_2d)

  !---------------------------------------------
  ! Set COBESST

  allocate(sst_cobe(0:icobe+1,1:jcobe))
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

  open(lun,file=file_cobe,form='unformatted',access='direct',recl=4*icobe*jcobe)
  write(6,*) ' SST read from ', trim(file_cobe), ' record = ', irec_cobe
  read(lun,rec=irec_cobe) work4_2d
  sst_cobe(1:icobe,1:jcobe) = real(work4_2d(1:icobe,1:jcobe),8)
  close(lun)
  sst_cobe(0,1:jcobe) = sst_cobe(icobe,1:jcobe)
  sst_cobe(icobe+1,1:jcobe) = sst_cobe(1,1:jcobe)

  deallocate(work4_2d)

  !---------------------------------------------
  ! set regular JRA-55 grid

  allocate(num_xgrid(1:jmut))
  allocate(lat_org(1:jmut))
  allocate(lat_org_bound(1:jmut+1))
  allocate(u10m_new(1:imut))
  allocate(v10m_new(1:imut))
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

  allocate(jsq(1:jmut))
  allocate(jnq(1:jmut))
  allocate(ia(1:imut,1:jmut)) ! number of longitudinal qscat points bounded by the JRA-55 grid
  allocate(iaq(1:imut,1:jmut,1:max_bound))

  jsq(1:jmut) = -1
  jnq(1:jmut) = -1
  ia(1:imut,1:jmut) = 0
  iaq(1:imut,1:jmut,1:max_bound) = 0

  do j = 1, jmut

    model_lat_n = lat_org_bound(jmut-j+2)
    model_lat_s = lat_org_bound(jmut-j+1)

    do jj = 1, jqscat
      if (model_lat_s <= lat_qscat(jj)) then
        jsq(j) = jj
        exit
      end if
    end do

    do jj = jqscat, 1, -1
      if (lat_qscat(jj) <= model_lat_n) then
        jnq(j) = jj
        exit
      end if
    end do

    if (jnq(j) < jsq(j)) then
      write(6,*) ' latitude search is erroneous, please check '
      stop
    end if

    allocate(lon_org_bound(1:num_xgrid(j)+2)) ! assume cyclic grid

    dlon_rg = 360.0 / real(num_xgrid(j),8)

    do i = 1, num_xgrid(j) + 2
      lon_org_bound(i) = dlon_rg * (real(i-1,8) - 0.5d0)
    end do

    do i = 1, num_xgrid(j)
      model_lon_e = lon_org_bound(i+1)
      model_lon_w = lon_org_bound(i)
      do ii = 1, iqscat
        if ((model_lon_w <= lon_qscat(ii)) .and. (lon_qscat(ii) <= model_lon_e)) then
          ia(i,j) = ia(i,j) + 1
          iaq(i,j,ia(i,j)) = ii
        end if
      end do
    end do

    ! the eastern end (i=num_xgrid(j)+1) corresponds to the western end (i=1)

    model_lon_e = lon_org_bound(num_xgrid(j)+2)
    model_lon_w = lon_org_bound(num_xgrid(j)+1)
    do ii = 1, iqscat
      if ((model_lon_w <= lon_qscat(ii)) .and. (lon_qscat(ii) <= model_lon_e)) then
        ia(1,j) = ia(1,j) + 1
        iaq(1,j,ia(1,j)) = ii
      end if
    end do

    do i = 1, num_xgrid(j)
      if (ia(i,j) > max_bound) then
        write(6,*) 'too many qscat points in a grid cell'
      end if
    end do

    !write(6,'(1a,5i6)') 'j = ',j, ia(1,j), ia(2,j), ia(num_xgrid(j)-1,j), ia(num_xgrid(j),j)

    deallocate(lon_org_bound)

  end do

  !--------------------------------

  write(6,*) 'READ JRA-55 data '

  allocate(work4(1:total_grid_1d))

  allocate(data_u10m(1:total_grid_1d))
  allocate(data_v10m(1:total_grid_1d))
  allocate(data_wind(1:total_grid_1d))

  allocate(data_tmp10m(1:total_grid_1d))
  allocate(data_sph10m(1:total_grid_1d))

  allocate(data_tmp2m(1:total_grid_1d))
  allocate(data_sph2m(1:total_grid_1d))
  allocate(data_brtmp(1:total_grid_1d))
  allocate(data_slprs(1:total_grid_1d))
  allocate(data_ice  (1:total_grid_1d))
  allocate(data_mask (1:total_grid_1d))
  allocate(data_sst_blend(1:total_grid_1d))
  allocate(data_qscat_avail(1:total_grid_1d))

  allocate(data_u10m_blend(1:total_grid_1d))
  allocate(data_v10m_blend(1:total_grid_1d))
  allocate(data_wind_blend(1:total_grid_1d))

  !------
  open(lun,file=file_u10m,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_u10m(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  open(lun,file=file_v10m,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_v10m(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  open(lun,file=file_wind,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_wind(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  !------
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

  write(6,*) ' set SST '

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
    allocate(mask_org(1:num_xgrid(j)))
    allocate(lon_org(1:num_xgrid(j)))

    brtmp_org(1:num_xgrid(j)) = data_brtmp(ibgn:iend)
    ice_org  (1:num_xgrid(j)) = data_ice  (ibgn:iend)
    mask_org (1:num_xgrid(j)) = data_mask (ibgn:iend)

    dlon_rg = 360.0 / real(num_xgrid(j),8)

    do i = 1, num_xgrid(j)
      lon_org(i) = dlon_rg * real(i-1,8)
    end do

    do i = 1, num_xgrid(j)
      model_lon = lon_org(i)
      loop_cobe_i: do ii = 1, icobe + 1
        if ((lon_cobe(ii-1) <= model_lon) .and.  (model_lon < lon_cobe(ii))) then
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
            weight_total = 0.0d0
            sst_tmp = 0.0d0
            if (sst_cobe(iw,js) /= undef8_cobe) then
              dist_tmp = distance_rad(lon_cobe(iw),lat_cobe(js),model_lon,model_lat)
              if (dist_tmp > radian_r * 1.5d0) then
                write(6,*) ' erroneous distance (SST): ', dist_tmp * radian
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
                write(6,*) ' erroneous distance (SST): ', dist_tmp * radian
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
                write(6,*) ' erroneous distance (SST): ', dist_tmp * radian
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
                write(6,*) ' erroneous distance (SST): ', dist_tmp * radian
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
        ! ice free water surface
        sst_org(i) = sst_tmp + tab
        !write(6,*) sst_org(i), brtmp_org(i)
      else
        sst_org(i) = brtmp_org(i)
      end if

    end do

    data_sst_blend(ibgn:iend) = sst_org(1:num_xgrid(j))

    deallocate(sst_org)
    deallocate(ice_org,brtmp_org,mask_org)
    deallocate(lon_org)

    i0 = iend
    !write(6,*) j, i0

  end do

  !----------------------------------------------------------------------------
  ! Bi-linear interpolation, if possible.

  write(6,*) ' bi-linear interpolation for wind '

  i0 = 0

  data_qscat_avail(1:total_grid_1d) = 0.0d0

  do j = 1, jmut

    model_lat = lat_org(jmut-j+1)

    ibgn = i0 + 1
    iend = i0 + num_xgrid(j)

    do jj = 1, jqscat
      if (model_lat < lat_qscat(jj)) then
        jn = jj
        js = jn - 1
        wgt_y = (model_lat - lat_qscat(js)) / (lat_qscat(jn) - lat_qscat(js))
        exit
      end if
      if (jj == jqscat) then
        jn = jj
        js = jj
        wgt_y = 1.0d0
      end if
    end do

    allocate(u10m_org(1:num_xgrid(j)))
    allocate(v10m_org(1:num_xgrid(j)))
    allocate(wind_org(1:num_xgrid(j)))
    allocate(ice_org(1:num_xgrid(j)))
    allocate(mask_org(1:num_xgrid(j)))
    allocate(u10m_blend_org(1:num_xgrid(j)))
    allocate(v10m_blend_org(1:num_xgrid(j)))
    allocate(wind_blend_org(1:num_xgrid(j)))
    allocate(lon_org(1:num_xgrid(j)))
    allocate(qscat_avail_org(1:num_xgrid(j)))

    u10m_org(1:num_xgrid(j)) = data_u10m(ibgn:iend)
    v10m_org(1:num_xgrid(j)) = data_v10m(ibgn:iend)
    wind_org(1:num_xgrid(j)) = data_wind(ibgn:iend)
    ice_org (1:num_xgrid(j)) = data_ice (ibgn:iend)
    mask_org(1:num_xgrid(j)) = data_mask(ibgn:iend)
    qscat_avail_org(1:num_xgrid(j)) = 0.0d0

    dlon_rg = 360.0 / real(num_xgrid(j),8)

    do i = 1, num_xgrid(j)
      lon_org(i) = dlon_rg * real(i-1,8)
    end do

    do i = 1, num_xgrid(j)
      loop_qscat_i: do ii = 1, iqscat
        if ( (lon_org(i) < lon_qscat(ii)) ) then
          if (ii > 1) then 
            iw = ii - 1
            ie = ii
            wgt_x = (lon_org(i) - lon_qscat(iw)) / (lon_qscat(ie) - lon_qscat(iw))
          else
            iw = iqscat
            ie = ii
            wgt_x = (lon_org(i) - lon_qscat(iw) + 360.d0) / (lon_qscat(ie) - lon_qscat(iw) + 360.d0)
          end if

          if ( (wind_qscat(iw,js) /= undef8_qscat) .and. &
             & (wind_qscat(ie,js) /= undef8_qscat) .and. &
             & (wind_qscat(iw,jn) /= undef8_qscat) .and. &
             & (wind_qscat(ie,jn) /= undef8_qscat)) then
            u10m_tmp = (1.0d0-wgt_x)*(1.0d0-wgt_y)*u10m_qscat(iw,js) &
                 & +          wgt_x *(1.0d0-wgt_y)*u10m_qscat(ie,js) &
                 & +   (1.0d0-wgt_x)*       wgt_y *u10m_qscat(iw,jn) &
                 & +          wgt_x *       wgt_y *u10m_qscat(ie,jn)
            v10m_tmp = (1.0d0-wgt_x)*(1.0d0-wgt_y)*v10m_qscat(iw,js) &
                 & +          wgt_x *(1.0d0-wgt_y)*v10m_qscat(ie,js) &
                 & +   (1.0d0-wgt_x)*       wgt_y *v10m_qscat(iw,jn) &
                 & +          wgt_x *       wgt_y *v10m_qscat(ie,jn)
            wind_tmp = (1.0d0-wgt_x)*(1.0d0-wgt_y)*wind_qscat(iw,js) &
                 & +          wgt_x *(1.0d0-wgt_y)*wind_qscat(ie,js) &
                 & +   (1.0d0-wgt_x)*       wgt_y *wind_qscat(iw,jn) &
                 & +          wgt_x *       wgt_y *wind_qscat(ie,jn)
          else
            u10m_tmp = undef8_qscat
            v10m_tmp = undef8_qscat
            wind_tmp = undef8_qscat
          end if
          exit loop_qscat_i
        end if
      end do loop_qscat_i
      
      if ((ice_org(i) == 0.0d0) .and. (mask_org(i) > 0.0d0) .and. (wind_tmp /= undef8_qscat)) then
        u10m_blend_org(i) = u10m_tmp
        v10m_blend_org(i) = v10m_tmp
        wind_blend_org(i) = wind_tmp
        qscat_avail_org(i) = 1.0d0
      else
        u10m_blend_org(i) = u10m_org(i)
        v10m_blend_org(i) = v10m_org(i)
        wind_blend_org(i) = wind_org(i)
      end if

    end do

    data_u10m_blend (ibgn:iend) = u10m_blend_org (1:num_xgrid(j))
    data_v10m_blend (ibgn:iend) = v10m_blend_org (1:num_xgrid(j))
    data_wind_blend (ibgn:iend) = wind_blend_org (1:num_xgrid(j))
    data_qscat_avail(ibgn:iend) = qscat_avail_org(1:num_xgrid(j))

    deallocate(u10m_org,v10m_org,wind_org)
    deallocate(ice_org,mask_org)
    deallocate(u10m_blend_org,v10m_blend_org,wind_blend_org)
    deallocate(lon_org)
    deallocate(qscat_avail_org)

    i0 = iend

  end do

  !----------------------------------------------------------------------------
  ! Extrapolation by filter mapping, if possible.

  write(6,*) ' Extrapolation for wind '

  i0 = 0
  num_extrap = 0

  do j = 1, jmut

    model_lat = lat_org(jmut-j+1)

    js = jsq(j)
    jn = jnq(j)

    ibgn = i0 + 1
    iend = i0 + num_xgrid(j)

    allocate(u10m_org(1:num_xgrid(j)))
    allocate(v10m_org(1:num_xgrid(j)))
    allocate(wind_org(1:num_xgrid(j)))
    allocate(ice_org (1:num_xgrid(j)))
    allocate(mask_org(1:num_xgrid(j)))
    allocate(u10m_blend_org(1:num_xgrid(j)))
    allocate(v10m_blend_org(1:num_xgrid(j)))
    allocate(wind_blend_org(1:num_xgrid(j)))
    allocate(lon_org (1:num_xgrid(j)))
    allocate(qscat_avail_org(1:num_xgrid(j)))

    u10m_org(1:num_xgrid(j)) = data_u10m(ibgn:iend)
    v10m_org(1:num_xgrid(j)) = data_v10m(ibgn:iend)
    wind_org(1:num_xgrid(j)) = data_wind(ibgn:iend)
    ice_org (1:num_xgrid(j)) = data_ice (ibgn:iend)
    mask_org(1:num_xgrid(j)) = data_mask(ibgn:iend)

    u10m_blend_org (1:num_xgrid(j)) = data_u10m_blend (ibgn:iend)
    v10m_blend_org (1:num_xgrid(j)) = data_v10m_blend (ibgn:iend)
    wind_blend_org (1:num_xgrid(j)) = data_wind_blend (ibgn:iend)
    qscat_avail_org(1:num_xgrid(j)) = data_qscat_avail(ibgn:iend)

    dlon_rg = 360.0 / real(num_xgrid(j),8)

    do i = 1, num_xgrid(j)
      lon_org(i) = dlon_rg * real(i-1,8)
    end do

    do i = 1, num_xgrid(j)

      model_lon = lon_org(i)

      if (qscat_avail_org(i) == 0.0d0) then
        weight_total = 0.0d0
        u10m_tmp = 0.0d0
        v10m_tmp = 0.0d0
        wind_tmp = 0.0d0
        do jj = js, jn
          do i_in = 1, ia(i,j)
            ii = iaq(i,j,i_in)
            if (wind_qscat(ii,jj) /= undef8_qscat) then
              dist_tmp = distance_rad(lon_qscat(ii),lat_qscat(jj),model_lon,model_lat)
              if (dist_tmp > radian_r * 1.5d0 * max(dlon_rg,abs(lat_org_bound(j)-lat_org_bound(j+1)))) then
                write(6,*) ' erroneous distance (QuikSCAT): ', dist_tmp * radian
                write(6,*) lon_qscat(ii),lat_qscat(jj),model_lon,model_lat
                stop
              end if
              weight = 1.0d0 / (dist_tmp + eps_dist)
              weight_total = weight_total + weight
              u10m_tmp = u10m_tmp + u10m_qscat(ii,jj) * weight
              v10m_tmp = v10m_tmp + v10m_qscat(ii,jj) * weight
              wind_tmp = wind_tmp + wind_qscat(ii,jj) * weight
            end if
          end do
        end do
        if ((weight_total > 0.0d0) .and. (ice_org(i) == 0.0d0) .and. (mask_org(i) > 0.0d0)) then
          num_extrap = num_extrap + 1
          u10m_blend_org(i) = u10m_tmp / weight_total
          v10m_blend_org(i) = v10m_tmp / weight_total
          wind_blend_org(i) = wind_tmp / weight_total
          qscat_avail_org(i) = 1.0d0
        end if
      end if
    end do

    data_u10m_blend(ibgn:iend) = u10m_blend_org(1:num_xgrid(j))
    data_v10m_blend(ibgn:iend) = v10m_blend_org(1:num_xgrid(j))
    data_wind_blend(ibgn:iend) = wind_blend_org(1:num_xgrid(j))
    data_qscat_avail(ibgn:iend) = qscat_avail_org(1:num_xgrid(j))

    deallocate(u10m_org,v10m_org,wind_org)
    deallocate(ice_org,mask_org)
    deallocate(u10m_blend_org,v10m_blend_org,wind_blend_org)
    deallocate(lon_org)
    deallocate(qscat_avail_org)

    i0 = iend

  end do

  write(6,*) num_extrap ,' points are filled by extrapolation '

  !------------------------------------------------------------------------

  altu = 10.0d0
  altt = 2.0d0
  altq = 2.0d0
  alt_target = 10.0d0

  allocate(data_u10m_neutral(1:total_grid_1d))
  allocate(data_v10m_neutral(1:total_grid_1d))
  allocate(data_wind_neutral(1:total_grid_1d))
  data_u10m_neutral(:) = data_u10m_blend(:)
  data_v10m_neutral(:) = data_v10m_blend(:)
  data_wind_neutral(:) = data_wind_blend(:)

  call wind_neutral_to_real( &
       & data_wind_blend, data_tmp10m, data_sph10m, &
       & data_tmp2m, data_sph2m, data_slprs, data_sst_blend,&
       & total_grid_1d, 1, data_qscat_avail, &
       & altu, altt, altq, alt_target)

  do n = 1, total_grid_1d
    if (data_qscat_avail(n) == 1.0d0) then ! needs adjustment
      if (data_wind_neutral(n) > 0.0d0) then   ! there can be "no wind"
        data_u10m_blend(n) = data_u10m_blend(n) * data_wind_blend(n) / data_wind_neutral(n)
        data_v10m_blend(n) = data_v10m_blend(n) * data_wind_blend(n) / data_wind_neutral(n)
      else
        if ((data_u10m_blend(n) == 0.0d0) .and. (data_v10m_blend(n) == 0.0d0)) then
          data_u10m_blend(n) = 0.0d0
          data_v10m_blend(n) = 0.0d0
        else
          write(6,*) ' Inconsistent QuikSCAT wind field. Please check. '
          stop 999
        end if
      end if
    else ! only QuikSCAT data is written to file
      data_v10m_blend(n) = undef8_qscat
      data_v10m_blend(n) = undef8_qscat
      data_wind_blend(n) = undef8_qscat
      data_v10m_neutral(n) = undef8_qscat
      data_v10m_neutral(n) = undef8_qscat
      data_wind_neutral(n) = undef8_qscat
    end if
  end do

  !-------------------------------------------------------------

  write(6,*) 'Product written to ', trim(file_u10m_blend_org)
  write(6,*) 'Product written to ', trim(file_v10m_blend_org)
  write(6,*) 'Product written to ', trim(file_wind_blend_org)

  open(lun,file=file_u10m_blend_org,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  write(lun,rec=1) real(data_u10m_blend,4)
  close(lun)

  open(lun,file=file_v10m_blend_org,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  write(lun,rec=1) real(data_v10m_blend,4)
  close(lun)

  open(lun,file=file_wind_blend_org,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  write(lun,rec=1) real(data_wind_blend,4)
  close(lun)

  !-------

  write(6,*) 'Product written to ', trim(file_u10m_neutral_org)
  write(6,*) 'Product written to ', trim(file_v10m_neutral_org)
  write(6,*) 'Product written to ', trim(file_wind_neutral_org)

  open(lun,file=file_u10m_neutral_org,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  write(lun,rec=1) real(data_u10m_neutral,4)
  close(lun)

  open(lun,file=file_v10m_neutral_org,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  write(lun,rec=1) real(data_v10m_neutral,4)
  close(lun)

  open(lun,file=file_wind_neutral_org,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  write(lun,rec=1) real(data_wind_neutral,4)
  close(lun)

  deallocate(data_u10m_neutral)
  deallocate(data_v10m_neutral)
  deallocate(data_wind_neutral)

end program quikscat_on_jra55_grid
