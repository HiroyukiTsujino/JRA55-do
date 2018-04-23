! -*-F90-*-
program era40_on_jra55_grid

  use libmxe_para, only: pi, radian, radian_r, radius

  implicit none

  integer(4) :: imut, jmut
  integer(4),allocatable :: num_xgrid(:)
  character(256) :: grid_name
  integer(4) :: total_grid_1d
  real(8),parameter :: tab = 273.15d0

  integer(4) :: ibgn, iend, i0

  real(4),allocatable :: work4(:)

  real(8),allocatable :: data_sst(:)
  real(8),allocatable :: sst_latlon(:,:)

  real(8),allocatable :: data_mask(:) ! JRA55 mask

  real(8),allocatable :: sst_org(:), sst_new(:)
  real(8),allocatable :: mask_org(:)

  real(8),allocatable :: lon_org(:), lon_org_bound(:)
  real(8),allocatable :: lon_new(:)
  real(8) :: dlon, dlon_rg
  real(8),allocatable :: lat_org(:)

  real(8) :: hld1
  real(8) :: model_lat, model_lon

  character(256) :: file_prod_org
  character(256) :: file_prod_latlon
  character(256) :: file_mask

  character(len=258) :: file_ydef

  integer(4),parameter :: lun=10

  integer(4) :: i, j, ii, jj, n

  integer(4) :: iii, m, m_left, m_right, i_left, i_right

  real(4),allocatable :: work4_2d(:,:)

  !---------------
  ! Interpolation/Extrapolation

  real(8) :: weight, weight_total
  real(8),parameter :: eps_dist = 1.0d-8
  integer(4) :: num_extrap

  !---------------
  ! ERA-Interim

  integer(4) :: iera40, jera40
  real(8) :: dlon_era40, dlat_era40, slon_era40, slat_era40
  real(8) :: sst_tmp
  real(8) :: undef8_era40
  real(4) :: undef4_era40
  character(len=256) :: file_era40, file_mask_era40
  real(8),allocatable :: sst_era40(:,:)
  real(8),allocatable :: mask_era40(:,:)
  real(8),allocatable :: lon_era40(:), lat_era40(:)
  integer(4) :: irec_era40

  !---------------

  real(8) :: distance_rad
  real(8) :: dist_tmp
  real(8) :: wgt_x, wgt_y
  real(8) :: altu, altt, altq, alt_target
  integer(4) :: iw, ie, js, jn, ios

  !---------------------------------------------

  namelist /nml_era40_on_jra55/ &
       & file_mask, &
       & file_prod_org, &
       & file_prod_latlon, &
       & imut, jmut, dlon, grid_name, &
       & file_ydef, &
       & iera40, jera40, &
       & slon_era40, dlon_era40, slat_era40, dlat_era40, &
       & undef4_era40, &
       & file_era40, &
       & file_mask_era40, &
       & irec_era40

  !---------------------------------------------

  open(lun,file='namelist.era40_on_jra55')
  read(lun,nml=nml_era40_on_jra55)
  close(lun)

  !---------------------------------------------
  ! Set ERA-Interim

  allocate(sst_era40(0:iera40+1,1:jera40))
  allocate(mask_era40(0:iera40+1,1:jera40))
  allocate(work4_2d(1:iera40,1:jera40))
  allocate(lon_era40(0:iera40+1),lat_era40(1:jera40))

  undef8_era40 = real(undef4_era40,8)

  do i = 0, iera40 + 1
    lon_era40(i) = slon_era40 + dlon_era40 * (i-1)
    !write(6,*) lon_era40(i)
  end do

  do j = 1, jera40
    lat_era40(j) = slat_era40 + dlat_era40 * (j-1)
    !write(6,*) lat_era40(j)
  end do

  open(lun,file=file_era40,form='unformatted',access='direct',recl=4*iera40*jera40)
  write(6,*) ' DATA(ERA40) read from ', trim(file_era40), ' record = ', irec_era40
  read(lun,rec=irec_era40) work4_2d
  sst_era40(1:iera40,1:jera40) = real(work4_2d(1:iera40,1:jera40),8)
  close(lun)
  sst_era40(0,1:jera40) = sst_era40(iera40,1:jera40)
  sst_era40(iera40+1,1:jera40) = sst_era40(1,1:jera40)

  open(lun,file=file_mask_era40,form='unformatted',access='direct',recl=4*iera40*jera40)
  write(6,*) ' MASK(ERA40) read from ', trim(file_mask_era40)
  read(lun,rec=1) work4_2d
  mask_era40(1:iera40,1:jera40) = real(work4_2d(1:iera40,1:jera40),8)
  close(lun)
  mask_era40(0,1:jera40) = mask_era40(iera40,1:jera40)
  mask_era40(iera40+1,1:jera40) = mask_era40(1,1:jera40)

  deallocate(work4_2d)

  !---------------------------------------------
  ! set regular JRA-55 grid

  allocate(num_xgrid(1:jmut))
  allocate(lat_org(1:jmut))
  allocate(lon_new(1:imut))
  allocate(sst_new(1:imut))
  allocate(sst_latlon(1:imut,1:jmut))

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

  !--------------------------------

  allocate(work4(1:total_grid_1d))

  allocate(data_sst (1:total_grid_1d))
  allocate(data_mask(1:total_grid_1d))

  !------

  write(6,*) 'READ JRA-55 mask data '

  open(lun,file=file_mask,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_mask(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  do n = 1, total_grid_1d
    data_mask(n) = 1.d0 - data_mask(n) ! 0 for land, 1 for water
  end do

  !----------------------------------------------------------------------------

  i0 = 0

  do j = 1, jmut

    model_lat = lat_org(jmut-j+1)
    write(6,*) j, 'latitude=', model_lat

    ibgn = i0 + 1
    iend = i0 + num_xgrid(j)

    do jj = 2, jera40
      if ((lat_era40(jj-1) <= model_lat) .and. (model_lat < lat_era40(jj))) then
        jn = jj
        js = jj - 1
        wgt_y = (model_lat - lat_era40(js)) / (lat_era40(jn) - lat_era40(js))
        exit
      end if
    end do
    if (model_lat < lat_era40(1)) then
      jn = 1
      js = 1
      wgt_y = 1.0d0
    end if
    if (lat_era40(jera40) <= model_lat) then
      jn = jera40
      js = jera40
      wgt_y = 1.0d0
    end if

    allocate(sst_org(1:num_xgrid(j)))
    allocate(mask_org(1:num_xgrid(j)))
    allocate(lon_org(1:num_xgrid(j)))

    mask_org (1:num_xgrid(j)) = data_mask (ibgn:iend)

    dlon_rg = 360.0 / real(num_xgrid(j),8)

    do i = 1, num_xgrid(j)
      lon_org(i) = dlon_rg * real(i-1,8)
    end do

    do i = 1, num_xgrid(j)
      model_lon = lon_org(i)
      loop_era40_ii: do ii = 1, iera40 + 1
        if ((lon_era40(ii-1) <= model_lon) .and.  (model_lon < lon_era40(ii))) then
          iw = ii - 1
          ie = ii
          wgt_x = (model_lon - lon_era40(iw)) / (lon_era40(ie) - lon_era40(iw))
          hld1 = (1.0d0-wgt_x)*(1.0d0-wgt_y)*sst_era40(iw,js) &
            & +         wgt_x *(1.0d0-wgt_y)*sst_era40(ie,js) &
            & +  (1.0d0-wgt_x)*       wgt_y *sst_era40(iw,jn) &
            & +         wgt_x *       wgt_y *sst_era40(ie,jn)
          if (mask_org(i) == 1.0d0) then ! oceanic grid
            if ((mask_era40(iw,js) /= 0.0d0) .and. (mask_era40(ie,js) /= 0.0d0) &
         & .and.(mask_era40(iw,jn) /= 0.0d0) .and. (mask_era40(ie,jn) /= 0.0d0)) then
              sst_tmp = hld1
            else
              weight_total = 0.0d0
              sst_tmp = 0.0d0
              if (mask_era40(iw,js) /= 0.0d0) then
                dist_tmp = distance_rad(lon_era40(iw),lat_era40(js),model_lon,model_lat)
                if (dist_tmp > radian_r * 1.5d0) then
                  write(6,*) ' erroneous distance (ERA40): ', dist_tmp * radian
                  write(6,*) lon_era40(iw),lat_era40(js),model_lon,model_lat
                  stop
                end if
                weight = 1.0d0 / (dist_tmp + eps_dist)
                weight_total = weight_total + weight
                sst_tmp = sst_tmp + sst_era40(iw,js) * weight
              end if
              if (mask_era40(ie,js) /= 0.0d0) then
                dist_tmp = distance_rad(lon_era40(ie),lat_era40(js),model_lon,model_lat)
                if (dist_tmp > radian_r * 1.5d0) then
                  write(6,*) ' erroneous distance (ERA40): ', dist_tmp * radian
                  write(6,*) lon_era40(ie),lat_era40(js),model_lon,model_lat
                  stop
                end if
                weight = 1.0d0 / (dist_tmp + eps_dist)
                weight_total = weight_total + weight
                sst_tmp = sst_tmp + sst_era40(ie,js) * weight
              end if
              if (mask_era40(iw,jn) /= 0.0d0) then
                dist_tmp = distance_rad(lon_era40(iw),lat_era40(jn),model_lon,model_lat)
                if (dist_tmp > radian_r * 1.5d0) then
                  write(6,*) ' erroneous distance (ERA40): ', dist_tmp * radian
                  write(6,*) lon_era40(iw),lat_era40(jn),model_lon,model_lat
                  stop
                end if
                weight = 1.0d0 / (dist_tmp + eps_dist)
                weight_total = weight_total + weight
                sst_tmp = sst_tmp + sst_era40(iw,jn) * weight
              end if
              if (mask_era40(ie,jn) /= 0.0d0) then
                dist_tmp = distance_rad(lon_era40(ie),lat_era40(jn),model_lon,model_lat) 
                if (dist_tmp > radian_r * 1.5d0) then
                  write(6,*) ' erroneous distance (ERA40): ', dist_tmp * radian
                  write(6,*) lon_era40(ie),lat_era40(jn),model_lon,model_lat
                  stop
                end if
                weight = 1.0d0 / (dist_tmp + eps_dist)
                weight_total = weight_total + weight
                sst_tmp = sst_tmp + sst_era40(ie,jn) * weight
              end if
              if (weight_total > 0.0d0) then
                sst_tmp = sst_tmp / weight_total
              else
                sst_tmp = hld1
              end if
            end if
          else ! land grid
            sst_tmp = hld1
          end if
          exit loop_era40_ii
        end if
      end do loop_era40_ii

      sst_org(i) = sst_tmp
     
    end do

    data_sst(ibgn:iend) = sst_org(1:num_xgrid(j))

    deallocate(sst_org)
    deallocate(mask_org)
    deallocate(lon_org)

    i0 = iend
    !write(6,*) j, i0

  end do

  write(6,*) 'Product (reduced grid) written to ', trim(file_prod_org)
  open(lun,file=file_prod_org,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  write(lun,rec=1) real(data_sst,4)
  close(lun)

  !------------------------------------------------------------------------
  ! reduced grid to lat-lon grid for check

  i0 = 0

  do j = 1, jmut

    ibgn = i0 + 1
    iend = i0 + num_xgrid(j)

    if (num_xgrid(j) == imut) then

      sst_latlon(1:imut,jmut-j+1) = data_sst(ibgn:iend)

    else

      allocate(sst_org(1:num_xgrid(j)+1))
      allocate(lon_org(1:num_xgrid(j)+1))

      sst_org(1:num_xgrid(j)) = data_sst(ibgn:iend)
      sst_org(num_xgrid(j)+1) = data_sst(ibgn)

      dlon_rg = 360.0 / real(num_xgrid(j),8)
      do ii = 1, num_xgrid(j) + 1
        lon_org(ii) = dlon_rg * real(ii-1,8)
      end do
      do i = 1, imut
        do ii = 1, num_xgrid(j)
          if (abs(lon_org(ii) - lon_new(i)) < 1.0d-4) then
            sst_new(i) = sst_org(ii)
            exit
          else if ( (lon_org(ii) < lon_new(i)) .and. (lon_new(i) <= lon_org(ii+1)) ) then
            weight = (lon_new(i) - lon_org(ii)) / (lon_org(ii+1) - lon_org(ii))
            sst_new(i) = (1.0d0 - weight) * sst_org(ii) + weight * sst_org(ii+1)
            exit
          end if
        end do
      end do
      sst_latlon(1:imut,jmut-j+1) = sst_new(1:imut)
      deallocate(sst_org)
      deallocate(lon_org)
    end if

    i0 = iend

  end do

  !-----

  write(6,*) 'Product (latlon grid) written to ', trim(file_prod_latlon)
  open(lun,file=file_prod_latlon,form='unformatted',access='direct',recl=4*imut*jmut)
  write(lun,rec=1) real(sst_latlon,4)
  close(lun)

  !---------------------------------------------

end program era40_on_jra55_grid
