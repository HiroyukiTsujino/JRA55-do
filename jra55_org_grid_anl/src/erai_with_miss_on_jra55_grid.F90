! -*-F90-*-
program erai_with_miss_on_jra55_grid

  ! interpolate ERA-I data with missing values

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
  real(4) :: undef4_out
  real(8) :: undef8_out

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

  integer(4) :: ierai, jerai
  real(8) :: dlon_erai, dlat_erai, slon_erai, slat_erai
  real(8) :: sst_tmp
  real(8) :: undef8_erai
  real(4) :: undef4_erai
  character(len=256) :: file_erai, file_mask_erai
  real(8),allocatable :: sst_erai(:,:)
  real(8),allocatable :: mask_erai(:,:)
  real(8),allocatable :: lon_erai(:), lat_erai(:)
  integer(4) :: irec_erai

  logical :: l_out_latlon
  logical :: l_filled
  !---------------

  real(8) :: distance_rad
  real(8) :: dist_tmp
  real(8) :: wgt_x, wgt_y
  real(8) :: altu, altt, altq, alt_target
  integer(4) :: iw, ie, js, jn, ios

  !---------------------------------------------

  namelist /nml_erai_on_jra55/ &
       & file_mask, &
       & file_prod_org, &
       & file_prod_latlon, &
       & undef4_out, &
       & imut, jmut, dlon, grid_name, &
       & file_ydef, &
       & ierai, jerai, &
       & slon_erai, dlon_erai, slat_erai, dlat_erai, &
       & undef4_erai, &
       & file_erai, &
       & file_mask_erai, &
       & irec_erai, &
       & l_out_latlon

  !---------------------------------------------

  l_out_latlon = .false.

  open(lun,file='namelist.erai_on_jra55')
  read(lun,nml=nml_erai_on_jra55)
  close(lun)

  undef8_out = real(undef4_out,8)

  !---------------------------------------------
  ! Set ERA-Interim

  allocate(sst_erai(0:ierai+1,1:jerai))
  allocate(mask_erai(0:ierai+1,1:jerai))
  allocate(work4_2d(1:ierai,1:jerai))
  allocate(lon_erai(0:ierai+1),lat_erai(1:jerai))

  undef8_erai = real(undef4_erai,8)

  do i = 0, ierai + 1
    lon_erai(i) = slon_erai + dlon_erai * (i-1)
    !write(6,*) lon_erai(i)
  end do

  do j = 1, jerai
    lat_erai(j) = slat_erai + dlat_erai * (j-1)
    !write(6,*) lat_erai(j)
  end do

  open(lun,file=file_erai,form='unformatted',access='direct',recl=4*ierai*jerai)
  write(6,*) ' DATA(Era-i) read from ', trim(file_erai), ' record = ', irec_erai
  read(lun,rec=irec_erai) work4_2d
  sst_erai(1:ierai,1:jerai) = real(work4_2d(1:ierai,1:jerai),8)
  close(lun)
  sst_erai(0,1:jerai) = sst_erai(ierai,1:jerai)
  sst_erai(ierai+1,1:jerai) = sst_erai(1,1:jerai)

  open(lun,file=file_mask_erai,form='unformatted',access='direct',recl=4*ierai*jerai)
  write(6,*) ' MASK(Era-i) read from ', trim(file_mask_erai)
  read(lun,rec=1) work4_2d
  mask_erai(1:ierai,1:jerai) = real(work4_2d(1:ierai,1:jerai),8)
  close(lun)
  mask_erai(0,1:jerai) = mask_erai(ierai,1:jerai)
  mask_erai(ierai+1,1:jerai) = mask_erai(1,1:jerai)

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

    do jj = 2, jerai
      if ((lat_erai(jj-1) <= model_lat) .and. (model_lat < lat_erai(jj))) then
        jn = jj
        js = jj - 1
        wgt_y = (model_lat - lat_erai(js)) / (lat_erai(jn) - lat_erai(js))
        exit
      end if
    end do
    if (model_lat < lat_erai(1)) then
      jn = 1
      js = 1
      wgt_y = 1.0d0
    end if
    if (lat_erai(jerai) <= model_lat) then
      jn = jerai
      js = jerai
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
      loop_erai_ii: do ii = 1, ierai + 1
        if ((lon_erai(ii-1) <= model_lon) .and.  (model_lon < lon_erai(ii))) then
          iw = ii - 1
          ie = ii
          wgt_x = (model_lon - lon_erai(iw)) / (lon_erai(ie) - lon_erai(iw))

          l_filled = .false.

          if (mask_org(i) == 1.0d0) then ! oceanic grid
            if ((mask_erai(iw,js) /= 0.0d0) .and. (mask_erai(ie,js) /= 0.0d0) & ! surrounded by erai-ocean
         & .and.(mask_erai(iw,jn) /= 0.0d0) .and. (mask_erai(ie,jn) /= 0.0d0)) then
              if ((sst_erai(iw,js) /= undef8_erai) .and. (sst_erai(ie,js) /= undef8_erai) & ! all data are valid
           & .and.(sst_erai(iw,jn) /= undef8_erai) .and. (sst_erai(ie,jn) /= undef8_erai)) then
                hld1 = (1.0d0-wgt_x)*(1.0d0-wgt_y)*sst_erai(iw,js) &
                  & +         wgt_x *(1.0d0-wgt_y)*sst_erai(ie,js) &
                  & +  (1.0d0-wgt_x)*       wgt_y *sst_erai(iw,jn) &
                  & +         wgt_x *       wgt_y *sst_erai(ie,jn)
                sst_tmp = hld1
                l_filled = .true.
              end if
            end if
            if (.not. l_filled) then ! search valid oceanic value
              weight_total = 0.0d0
              sst_tmp = 0.0d0
              if ((mask_erai(iw,js) /= 0.0d0) .and. (sst_erai(iw,js) /= undef8_erai)) then
                dist_tmp = distance_rad(lon_erai(iw),lat_erai(js),model_lon,model_lat)
                if (dist_tmp > radian_r * 1.5d0) then
                  write(6,*) ' erroneous distance (ERAI): ', dist_tmp * radian
                  write(6,*) lon_erai(iw),lat_erai(js),model_lon,model_lat
                  stop
                end if
                weight = 1.0d0 / (dist_tmp + eps_dist)
                weight_total = weight_total + weight
                sst_tmp = sst_tmp + sst_erai(iw,js) * weight
              end if
              if ((mask_erai(ie,js) /= 0.0d0) .and. (sst_erai(ie,js) /= undef8_erai)) then
                dist_tmp = distance_rad(lon_erai(ie),lat_erai(js),model_lon,model_lat)
                if (dist_tmp > radian_r * 1.5d0) then
                  write(6,*) ' erroneous distance (ERAI): ', dist_tmp * radian
                  write(6,*) lon_erai(ie),lat_erai(js),model_lon,model_lat
                  stop
                end if
                weight = 1.0d0 / (dist_tmp + eps_dist)
                weight_total = weight_total + weight
                sst_tmp = sst_tmp + sst_erai(ie,js) * weight
              end if
              if ((mask_erai(iw,jn) /= 0.0d0) .and. (sst_erai(iw,jn) /= undef8_erai)) then
                dist_tmp = distance_rad(lon_erai(iw),lat_erai(jn),model_lon,model_lat)
                if (dist_tmp > radian_r * 1.5d0) then
                  write(6,*) ' erroneous distance (ERAI): ', dist_tmp * radian
                  write(6,*) lon_erai(iw),lat_erai(jn),model_lon,model_lat
                  stop
                end if
                weight = 1.0d0 / (dist_tmp + eps_dist)
                weight_total = weight_total + weight
                sst_tmp = sst_tmp + sst_erai(iw,jn) * weight
              end if
              if ((mask_erai(ie,jn) /= 0.0d0) .and. (sst_erai(ie,jn) /= undef8_erai)) then
                dist_tmp = distance_rad(lon_erai(ie),lat_erai(jn),model_lon,model_lat) 
                if (dist_tmp > radian_r * 1.5d0) then
                  write(6,*) ' erroneous distance (ERAI): ', dist_tmp * radian
                  write(6,*) lon_erai(ie),lat_erai(jn),model_lon,model_lat
                  stop
                end if
                weight = 1.0d0 / (dist_tmp + eps_dist)
                weight_total = weight_total + weight
                sst_tmp = sst_tmp + sst_erai(ie,jn) * weight
              end if
              if (weight_total > 0.0d0) then
                sst_tmp = sst_tmp / weight_total
              else
                sst_tmp = undef8_out
              end if
            end if
          else ! land grid
            sst_tmp = undef8_out
          end if
          exit loop_erai_ii
        end if
      end do loop_erai_ii

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
  ! Following is just for checking, do not use for scientific quality computation

  IF_LATLON: if (l_out_latlon) then 

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
            if ((sst_org(ii) /= undef8_out) .and. (sst_org(ii+1) /= undef8_out)) then
              sst_new(i) = (1.0d0 - weight) * sst_org(ii) + weight * sst_org(ii+1)
            else
              sst_new(i) = undef8_out
            end if
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

  end if IF_LATLON

  !---------------------------------------------

end program erai_with_miss_on_jra55_grid
