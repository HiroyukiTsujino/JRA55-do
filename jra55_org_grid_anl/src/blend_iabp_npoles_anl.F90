! -*-F90-*-
program blend_iab_npoles_anl

  implicit none

  integer(4) :: imut, jmut
  integer(4),allocatable :: num_xgrid(:)
  character(256) :: grid_name
  integer(4) :: total_grid_1d
  real(8),parameter :: tab = 273.15

  integer(4) :: ibgn, iend, i0

  real(4),allocatable :: work4(:)

  real(8),allocatable :: data_tmp2m(:)
  real(8),allocatable :: data_tmp2m_blend(:)
  real(8),allocatable :: data_ice(:)
  real(8),allocatable :: data_mask(:)

  real(8),allocatable :: tmp2m_latlon(:,:)

  real(8),allocatable :: tmp2m_org(:), tmp2m_new(:)
  real(8),allocatable :: ice_org(:), mask_org(:)
  real(8),allocatable :: tmp2m_blend_org(:)
  real(8),allocatable :: lon_org(:), lon_new(:)
  real(8) :: dlon, dlon_rg
  real(8),allocatable :: lat_org(:)

  character(256) :: file_tmp2m
  character(256) :: file_ice
  character(256) :: file_mask

  character(256) :: file_blend_org
  character(256) :: file_blend_latlon
  character(len=258) :: file_ydef

  integer(4),parameter :: lun=10

  integer(4) :: i, j, ii, jj, n
  real(8) :: weight

  integer(4) :: iii, m, m_left, m_right, i_left, i_right

  integer(4) :: iiabp, jiabp, iw, ie, js, jn, ios
  real(8) :: dlon_iabp, dlat_iabp, slon_iabp, slat_iabp
  real(8) :: wgt_x, wgt_y, tmp_iabp, undef8_iabp
  real(8) :: model_lat, model_lon
  real(4) :: undef4_iabp
  character(len=256) :: file_iabp
  real(8),allocatable :: tmp2m_iabp(:,:)
  real(4),allocatable :: work4_2d(:,:)
  real(8),allocatable :: lon_iabp(:), lat_iabp(:)

  !---------------------------------------------

  namelist /nml_blend_iabp_anl/ &
       & file_tmp2m, &
       & file_ice, &
       & file_mask, &
       & file_blend_org, &
       & file_blend_latlon, &
       & imut, jmut, dlon, grid_name, &
       & file_ydef, &
       & iiabp, jiabp, &
       & slon_iabp, dlon_iabp, slat_iabp, dlat_iabp, &
       & undef4_iabp, &
       & file_iabp

  !---------------------------------------------

  open(lun,file='namelist.blend_iabp_anl')
  read(lun,nml=nml_blend_iabp_anl)
  close(lun)

  !---------------------------------------------

  allocate(work4_2d(1:iiabp,1:jiabp))
  allocate(tmp2m_iabp(0:iiabp+1,1:jiabp))
  allocate(lon_iabp(0:iiabp+1),lat_iabp(1:jiabp))

  undef8_iabp = real(undef4_iabp,8)

  do i = 0, iiabp + 1
    lon_iabp(i) = slon_iabp + dlon_iabp * (i-1)
    !write(6,*) lon_iabp(i)
  end do

  do j = 1, jiabp
    lat_iabp(j) = slat_iabp + dlat_iabp * (j-1)
    !write(6,*) lat_iabp(j)
  end do

  open(lun,file=file_iabp,form='unformatted',access='direct',recl=4*iiabp*jiabp)
  read(lun,rec=1) work4_2d
  tmp2m_iabp(1:iiabp,1:jiabp) = real(work4_2d(1:iiabp,1:jiabp),8)
  close(lun)
  tmp2m_iabp(0      ,1:jiabp) =  tmp2m_iabp(iiabp,1:jiabp) 
  tmp2m_iabp(iiabp+1,1:jiabp) =  tmp2m_iabp(1    ,1:jiabp) 

  !---------------------------------------------

  allocate(num_xgrid(1:jmut))
  allocate(lat_org(1:jmut))
  allocate(tmp2m_latlon(1:imut,1:jmut))
  allocate(tmp2m_new(1:imut))
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

  allocate(data_tmp2m(1:total_grid_1d))
  allocate(data_ice  (1:total_grid_1d))
  allocate(data_mask (1:total_grid_1d))
  allocate(data_tmp2m_blend(1:total_grid_1d))

  !------
  open(lun,file=file_tmp2m,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_tmp2m(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  !------
  open(lun,file=file_ice,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_ice(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  !------
  open(lun,file=file_mask,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_mask(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  do n = 1, total_grid_1d
    data_mask(n) = 1.0d0 - data_mask(n) ! 0 for land, 1 for water
    data_tmp2m(n) = data_tmp2m(n) - tab ! Celsius
  end do

  !----------------------------------------------------------------------------

  i0 = 0

  do j = 1, jmut

    model_lat = lat_org(jmut-j+1)

    ibgn = i0 + 1
    iend = i0 + num_xgrid(j)

    if (model_lat >= 50.5d0) then

      weight = (model_lat - 50.d0) / 10.0d0
      weight = min(1.0d0, weight)

      do jj = 2, jiabp
        if ((lat_iabp(jj-1) <= model_lat) .and. (model_lat < lat_iabp(jj))) then
          jn = jj
          js = jj - 1
          wgt_y = (model_lat - lat_iabp(js)) / (lat_iabp(jn) - lat_iabp(js))
          exit
        end if
      end do
      if (model_lat < lat_iabp(1)) then
        jn = 1
        js = 1
        wgt_y = 1.0d0
      end if
      if (lat_iabp(jiabp) <= model_lat) then
        jn = jiabp
        js = jiabp
        wgt_y = 1.0d0
      end if

      !write(6,*) j, model_lat, wgt_y

      allocate(tmp2m_org(1:num_xgrid(j)))
      allocate(ice_org(1:num_xgrid(j)))
      allocate(mask_org(1:num_xgrid(j)))
      allocate(tmp2m_blend_org(1:num_xgrid(j)))
      allocate(lon_org(1:num_xgrid(j)))

      tmp2m_org(1:num_xgrid(j)) = data_tmp2m(ibgn:iend)
      ice_org(1:num_xgrid(j)) = data_ice(ibgn:iend)
      mask_org(1:num_xgrid(j)) = data_mask(ibgn:iend)

      dlon_rg = 360.0 / real(num_xgrid(j),8)

      do i = 1, num_xgrid(j)
        lon_org(i) = dlon_rg * real(i-1,8)
      end do

      do i = 1, num_xgrid(j)
        model_lon = lon_org(i)
        loop_iabp_i: do ii = 1, iiabp + 1
          if ((lon_iabp(ii-1) <= model_lon) .and. (model_lon < lon_iabp(ii))) then
            iw = ii - 1
            ie = ii
            wgt_x = (model_lon - lon_iabp(iw)) / (lon_iabp(ie) - lon_iabp(iw))
            !write(6,*) model_lat, lon_org(i), wgt_x
            if ( (tmp2m_iabp(iw,js) /= undef8_iabp) .and. &
               & (tmp2m_iabp(ie,js) /= undef8_iabp) .and. &
               & (tmp2m_iabp(iw,jn) /= undef8_iabp) .and. &
               & (tmp2m_iabp(ie,jn) /= undef8_iabp)) then
              tmp_iabp = (1.0d0-wgt_x)*(1.0d0-wgt_y)*tmp2m_iabp(iw,js) &
                   & +          wgt_x *(1.0d0-wgt_y)*tmp2m_iabp(ie,js) &
                   & +   (1.0d0-wgt_x)* wgt_y       *tmp2m_iabp(iw,jn) &
                   & +          wgt_x * wgt_y       *tmp2m_iabp(ie,jn)
            else
              tmp_iabp = undef8_iabp
            end if
            exit loop_iabp_i
          end if
        end do loop_iabp_i

        if ((ice_org(i) > 0.0d0) .and. (mask_org(i) > 0.0d0) .and. (tmp_iabp /= undef8_iabp)) then
          ! ice covered water
          tmp2m_blend_org(i) = weight * tmp_iabp + (1.0d0 - weight) * tmp2m_org(i)
        else
          tmp2m_blend_org(i) = tmp2m_org(i)
        end if

      end do

      data_tmp2m_blend(ibgn:iend) = tmp2m_blend_org(1:num_xgrid(j))

      deallocate(tmp2m_org)
      deallocate(ice_org)
      deallocate(tmp2m_blend_org)
      deallocate(lon_org)

    else

      data_tmp2m_blend(ibgn:iend) = data_tmp2m(ibgn:iend)

    end if

    i0 = iend
    !write(6,*) j, i0

  end do

  ! reduced grid to lat-lon grid for check

  i0 = 0

  do j = 1, jmut

    ibgn = i0 + 1
    iend = i0 + num_xgrid(j)

    !write(6,*) j, ibgn, iend

    if (num_xgrid(j) == imut) then

      tmp2m_latlon(1:imut,jmut-j+1) = data_tmp2m_blend(ibgn:iend)

    else

      allocate(tmp2m_org(1:num_xgrid(j)+1))
      allocate(lon_org (1:num_xgrid(j)+1))

      tmp2m_org(1:num_xgrid(j)) = data_tmp2m_blend(ibgn:iend)
      tmp2m_org(num_xgrid(j)+1) = data_tmp2m_blend(ibgn)

      dlon_rg = 360.0 / real(num_xgrid(j),8)
      do ii = 1, num_xgrid(j) + 1
        lon_org(ii) = dlon_rg * real(ii-1,8)
      end do
      do i = 1, imut
        do ii = 1, num_xgrid(j)
          if (abs(lon_org(ii) - lon_new(i)) < 1.0d-4) then
            tmp2m_new(i) = tmp2m_org(ii)
            exit
          else if ( (lon_org(ii) < lon_new(i)) .and. (lon_new(i) <= lon_org(ii+1)) ) then
            weight = (lon_new(i) - lon_org(ii)) / (lon_org(ii+1) - lon_org(ii))
            tmp2m_new(i) = (1.0d0 - weight) * tmp2m_org(ii) + weight * tmp2m_org(ii+1)
            exit
          end if
        end do
      end do
      tmp2m_latlon(1:imut,jmut-j+1) = tmp2m_new(1:imut)
      deallocate(tmp2m_org)
      deallocate(lon_org)
    end if

    i0 = iend

  end do

  !-----

  data_tmp2m_blend(:) = data_tmp2m_blend(:) + tab

  open(lun,file=file_blend_org,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  write(lun,rec=1) real(data_tmp2m_blend,4)
  close(lun)

  !-----

  tmp2m_latlon(1:imut,1:jmut) = tmp2m_latlon(1:imut,1:jmut) + tab

  open(lun,file=file_blend_latlon,form='unformatted',access='direct',recl=4*imut*jmut)
  write(lun,rec=1) real(tmp2m_latlon,4)
  close(lun)

  !---------------------------------------------

end program blend_iab_npoles_anl
