! -*-F90-*-
program blend_remss_wind_anl

  use libmxe_para, only: pi, radian, radian_r, radius

  implicit none

  integer(4) :: imut, jmut
  integer(4),allocatable :: num_xgrid(:)
  character(256) :: grid_name
  integer(4) :: total_grid_1d
  real(8),parameter :: tab = 273.15

  integer(4) :: ibgn, iend, i0

  real(4),allocatable :: work4(:)

  real(8),allocatable :: data_wind10m(:)
  real(8),allocatable :: data_wind10m_blend(:)
  real(8),allocatable :: data_mask(:)

  real(8),allocatable :: wind10m_latlon(:,:)

  real(8),allocatable :: wind10m_org(:), wind10m_new(:)
  real(8),allocatable :: mask_org(:)
  real(8),allocatable :: wind10m_blend_org(:)
  real(8),allocatable :: lon_org(:), lon_new(:)
  real(8) :: dlon, dlon_rg
  real(8),allocatable :: lat_org(:)

  character(256) :: file_wind10m
  character(256) :: file_mask

  character(256) :: file_blend_org
  character(256) :: file_blend_latlon
  character(len=258) :: file_ydef

  integer(4),parameter :: lun=10

  integer(4) :: i, j, ii, jj, n

  integer(4) :: iii, m, m_left, m_right, i_left, i_right

  integer(4) :: iremss, jremss, iw, ie, js, jn, ios
  real(8) :: dlon_remss, dlat_remss, slon_remss, slat_remss
  real(8) :: wgt_x, wgt_y, tmp_remss, undef8_remss
  real(8) :: model_lat, model_lon
  real(4) :: undef4_remss
  character(len=256) :: file_remss
  real(8),allocatable :: wind10m_remss(:,:)
  real(4),allocatable :: work4_2d(:,:)
  real(8),allocatable :: lon_remss(:), lat_remss(:)

  real(8) :: weight
  real(8) :: weight_total
  real(8),parameter :: eps_dist = 1.0d-8

  real(8) :: distance_rad
  real(8) :: dist_tmp

  !---------------------------------------------

  namelist /nml_blend_remss_anl/ &
       & file_wind10m, &
       & file_mask, &
       & file_blend_org, &
       & file_blend_latlon, &
       & imut, jmut, dlon, grid_name, &
       & file_ydef, &
       & iremss, jremss, &
       & slon_remss, dlon_remss, slat_remss, dlat_remss, &
       & undef4_remss, &
       & file_remss

  !---------------------------------------------

  open(lun,file='namelist.blend_remss_wind_anl_neutral')
  read(lun,nml=nml_blend_remss_anl)
  close(lun)

  !---------------------------------------------

  allocate(work4_2d(1:iremss,1:jremss))
  allocate(wind10m_remss(0:iremss+1,1:jremss))
  allocate(lon_remss(0:iremss+1),lat_remss(1:jremss))

  undef8_remss = real(undef4_remss,8)

  do i = 0, iremss + 1
    lon_remss(i) = slon_remss + dlon_remss * (i-1)
    !write(6,*) lon_remss(i)
  end do

  do j = 1, jremss
    lat_remss(j) = slat_remss + dlat_remss * (j-1)
    !write(6,*) lat_remss(j)
  end do

  open(lun,file=file_remss,form='unformatted',access='direct',recl=4*iremss*jremss)
  read(lun,rec=1) work4_2d
  wind10m_remss(1:iremss,1:jremss) = real(work4_2d(1:iremss,1:jremss),8)
  close(lun)
  wind10m_remss(0       ,1:jremss) = wind10m_remss(iremss,1:jremss) 
  wind10m_remss(iremss+1,1:jremss) = wind10m_remss(1     ,1:jremss) 

  !---------------------------------------------

  allocate(num_xgrid(1:jmut))
  allocate(lat_org(1:jmut))
  allocate(wind10m_latlon(1:imut,1:jmut))
  allocate(wind10m_new(1:imut))
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

  call set_reduced_grid(grid_name,num_xgrid,jmut,total_grid_1d)

  allocate(work4(1:total_grid_1d))

  allocate(data_wind10m(1:total_grid_1d))
  allocate(data_mask (1:total_grid_1d))
  allocate(data_wind10m_blend(1:total_grid_1d))

  !------
  open(lun,file=file_wind10m,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_wind10m(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  !------
  open(lun,file=file_mask,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_mask(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  do n = 1, total_grid_1d
    data_mask(n) = 1.0d0 - data_mask(n) ! 0 for land, 1 for water
  end do

  !----------------------------------------------------------------------------

  i0 = 0

  do j = 1, jmut

    model_lat = lat_org(jmut-j+1)

    ibgn = i0 + 1
    iend = i0 + num_xgrid(j)

    do jj = 2, jremss
      if ((lat_remss(jj-1) <= model_lat) .and. (model_lat < lat_remss(jj))) then
        jn = jj
        js = jj - 1
        wgt_y = (model_lat - lat_remss(js)) / (lat_remss(jn) - lat_remss(js))
        exit
      end if
    end do
    if (model_lat < lat_remss(1)) then
      jn = 1
      js = 1
      wgt_y = 1.0d0
    end if
    if (lat_remss(jremss) <= model_lat) then
      jn = jremss
      js = jremss
      wgt_y = 1.0d0
    end if

    !write(6,*) j, model_lat, wgt_y

    allocate(wind10m_org(1:num_xgrid(j)))
    allocate(mask_org(1:num_xgrid(j)))
    allocate(wind10m_blend_org(1:num_xgrid(j)))
    allocate(lon_org(1:num_xgrid(j)))

    wind10m_org(1:num_xgrid(j)) = data_wind10m(ibgn:iend)
    mask_org(1:num_xgrid(j)) = data_mask(ibgn:iend)

    dlon_rg = 360.0 / real(num_xgrid(j),8)

    do i = 1, num_xgrid(j)
      lon_org(i) = dlon_rg * real(i-1,8)
    end do

    do i = 1, num_xgrid(j)
      model_lon = lon_org(i)
      loop_remss_i: do ii = 1, iremss + 1
        if ((lon_remss(ii-1) <= model_lon) .and. (model_lon < lon_remss(ii))) then
          iw = ii - 1
          ie = ii
          wgt_x = (model_lon - lon_remss(iw)) / (lon_remss(ie) - lon_remss(iw))
          !write(6,*) model_lat, lon_org(i), wgt_x
          if ( (wind10m_remss(iw,js) /= undef8_remss) .and. &
             & (wind10m_remss(ie,js) /= undef8_remss) .and. &
             & (wind10m_remss(iw,jn) /= undef8_remss) .and. &
             & (wind10m_remss(ie,jn) /= undef8_remss)) then
            tmp_remss = (1.0d0-wgt_x)*(1.0d0-wgt_y)*wind10m_remss(iw,js) &
                 & +           wgt_x *(1.0d0-wgt_y)*wind10m_remss(ie,js) &
                 & +    (1.0d0-wgt_x)* wgt_y       *wind10m_remss(iw,jn) &
                 & +           wgt_x * wgt_y       *wind10m_remss(ie,jn)
          else
            weight_total = 0.0d0
            tmp_remss = 0.0d0
            if (wind10m_remss(iw,js) /= undef8_remss) then
              dist_tmp = distance_rad(lon_remss(iw),lat_remss(js),model_lon,model_lat)
              if (dist_tmp > radian_r * 1.5d0) then
                write(6,*) ' erroneous distance : ', dist_tmp * radian
                write(6,*) lon_remss(iw),lat_remss(js),model_lon,model_lat
                stop
              end if
              weight = 1.0d0 / (dist_tmp + eps_dist)
              weight_total = weight_total + weight
              tmp_remss = tmp_remss + wind10m_remss(iw,js) * weight
            end if
            if (wind10m_remss(ie,js) /= undef8_remss) then
              dist_tmp = distance_rad(lon_remss(ie),lat_remss(js),model_lon,model_lat)
              if (dist_tmp > radian_r * 1.5d0) then
                write(6,*) ' erroneous distance : ', dist_tmp * radian
                write(6,*) lon_remss(ie),lat_remss(js),model_lon,model_lat
                stop
              end if
              weight = 1.0d0 / (dist_tmp + eps_dist)
              weight_total = weight_total + weight
              tmp_remss = tmp_remss + wind10m_remss(ie,js) * weight
            end if
            if (wind10m_remss(iw,jn) /= undef8_remss) then
              dist_tmp = distance_rad(lon_remss(iw),lat_remss(jn),model_lon,model_lat)
              if (dist_tmp > radian_r * 1.5d0) then
                write(6,*) ' erroneous distance : ', dist_tmp * radian
                write(6,*) lon_remss(iw),lat_remss(jn),model_lon,model_lat
                stop
              end if
              weight = 1.0d0 / (dist_tmp + eps_dist)
              weight_total = weight_total + weight
              tmp_remss = tmp_remss + wind10m_remss(iw,jn) * weight
            end if
            if (wind10m_remss(ie,jn) /= undef8_remss) then
              dist_tmp = distance_rad(lon_remss(ie),lat_remss(jn),model_lon,model_lat) 
              if (dist_tmp > radian_r * 1.5d0) then
                write(6,*) ' erroneous distance : ', dist_tmp * radian
                write(6,*) lon_remss(ie),lat_remss(jn),model_lon,model_lat
                stop
              end if
              weight = 1.0d0 / (dist_tmp + eps_dist)
              weight_total = weight_total + weight
              tmp_remss = tmp_remss + wind10m_remss(ie,jn) * weight
            end if
            if (weight_total > 0.0d0) then
              tmp_remss = tmp_remss / weight_total
            else
              tmp_remss = undef8_remss
            end if
          end if
          exit loop_remss_i
        end if
      end do loop_remss_i
      
      if ((mask_org(i) > 0.0d0) .and. (tmp_remss /= undef8_remss)) then
        wind10m_blend_org(i) = tmp_remss
      else
        wind10m_blend_org(i) = wind10m_org(i)
      end if
      
    end do

    data_wind10m_blend(ibgn:iend) = wind10m_blend_org(1:num_xgrid(j))

    deallocate(wind10m_org)
    deallocate(wind10m_blend_org)
    deallocate(lon_org)
    
    i0 = iend

    !write(6,*) j, i0
    
  end do

  open(lun,file=file_blend_org,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  write(lun,rec=1) real(data_wind10m_blend,4)
  close(lun)

  !------------------------------------------------------------------------
  ! reduced grid to lat-lon grid for check

  i0 = 0

  do j = 1, jmut

    ibgn = i0 + 1
    iend = i0 + num_xgrid(j)

    !write(6,*) j, ibgn, iend

    if (num_xgrid(j) == imut) then

      wind10m_latlon(1:imut,jmut-j+1) = data_wind10m_blend(ibgn:iend)

    else

      allocate(wind10m_org(1:num_xgrid(j)+1))
      allocate(lon_org (1:num_xgrid(j)+1))

      wind10m_org(1:num_xgrid(j)) = data_wind10m_blend(ibgn:iend)
      wind10m_org(num_xgrid(j)+1) = data_wind10m_blend(ibgn)

      dlon_rg = 360.0 / real(num_xgrid(j),8)
      do ii = 1, num_xgrid(j) + 1
        lon_org(ii) = dlon_rg * real(ii-1,8)
      end do
      do i = 1, imut
        do ii = 1, num_xgrid(j)
          if (abs(lon_org(ii) - lon_new(i)) < 1.0d-4) then
            wind10m_new(i) = wind10m_org(ii)
            exit
          else if ( (lon_org(ii) < lon_new(i)) .and. (lon_new(i) <= lon_org(ii+1)) ) then
            weight = (lon_new(i) - lon_org(ii)) / (lon_org(ii+1) - lon_org(ii))
            wind10m_new(i) = (1.0d0 - weight) * wind10m_org(ii) + weight * wind10m_org(ii+1)
            exit
          end if
        end do
      end do
      wind10m_latlon(1:imut,jmut-j+1) = wind10m_new(1:imut)
      deallocate(wind10m_org)
      deallocate(lon_org)
    end if

    i0 = iend

  end do

  !-----

  open(lun,file=file_blend_latlon,form='unformatted',access='direct',recl=4*imut*jmut)
  write(lun,rec=1) real(wind10m_latlon,4)
  close(lun)

  !---------------------------------------------

end program blend_remss_wind_anl
