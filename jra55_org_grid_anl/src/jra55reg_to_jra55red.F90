! -*-F90-*-
program jra55_regular_to_jra55_reduced_grid

  use libmxe_para, only: pi, radian, radian_r, radius

  implicit none

  integer(4) :: imut, jmut
  integer(4),allocatable :: num_xgrid(:)
  character(256) :: grid_name
  integer(4) :: total_grid_1d

  integer(4) :: ibgn, iend, i0

  real(4),allocatable :: work4(:)

  real(8),allocatable :: data_sst(:)
  real(8),allocatable :: sst_latlon(:,:)

  real(8),allocatable :: data_mask(:) ! JRA55 mask
  real(8),allocatable :: mask_latlon(:,:) ! JRA55 mask

  real(8),allocatable :: sst_org(:), sst_new(:)
  real(8),allocatable :: mask_org(:)

  real(8),allocatable :: lon_org(:), lon_org_bound(:)
  real(8),allocatable :: lon_reg(:)
  real(8) :: dlon, dlon_rg
  real(8),allocatable :: lat_org(:)

  integer(4) :: num_data

  real(8) :: model_lat, model_lon
  real(4) :: undef4_out
  real(8) :: undef8_out
  real(4) :: undef4_in
  real(8) :: undef8_in

  character(256) :: file_prod_org
  character(256) :: file_prod_latlon
  character(256) :: file_mask
  character(256) :: file_mask_latlon

  character(len=258) :: file_ydef

  integer(4),parameter :: lun=10, lun_in=11, lun_out=12

  integer(4) :: i, j, ii, jj, n

  integer(4) :: iii, m, m_left, m_right, i_left, i_right

  real(4),allocatable :: work4_2d(:,:)

  !---------------
  ! Interpolation/Extrapolation

  real(8) :: weight, weight_total
  real(8),parameter :: eps_dist = 1.0d-8
  integer(4) :: num_extrap

  logical :: l_filled

  real(8) :: sst_tmp, hl1

  real(8) :: blank_value

  !---------------

  real(8) :: distance_rad
  real(8) :: dist_tmp
  real(8) :: wgt_x, wgt_y
  real(8) :: altu, altt, altq, alt_target
  integer(4) :: iw, ie, js, jn, ios

  !---------------------------------------------

  namelist /nml_jra55_reg2red/ &
       & file_mask, &
       & file_mask_latlon, &
       & file_prod_org, &
       & file_prod_latlon, &
       & undef4_in, &
       & undef4_out, &
       & blank_value, &
       & imut, jmut, dlon, grid_name, &
       & file_ydef, &
       & num_data

  !---------------------------------------------

  blank_value = 0.0d0

  open(lun,file='namelist.jra55_reg2red')
  read(lun,nml=nml_jra55_reg2red)
  close(lun)

  undef8_out = real(undef4_out,8)
  undef8_in = real(undef4_in,8)

  !---------------------------------------------
  ! set regular JRA-55 grid

  allocate(num_xgrid(1:jmut))
  allocate(lat_org(1:jmut))
  allocate(lon_reg(0:imut+1))
  allocate(sst_new(1:imut))
  allocate(sst_latlon(1:imut,1:jmut))

  do i = 0, imut + 1
    lon_reg(i) = dlon * real(i-1,8)
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

  allocate(work4_2d(1:imut,1:jmut))
  allocate(sst_latlon(0:imut+1,1:jmut))
  allocate(mask_latlon(0:imut+1,1:jmut))

  !--------------------------------

  write(6,*) 'READ JRA-55 mask data '

  open(lun,file=file_mask,form='unformatted',access='direct',convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_mask(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  do n = 1, total_grid_1d
    data_mask(n) = 1.d0 - data_mask(n) ! 0 for land, 1 for water
  end do

  open(lun,file=file_mask_latlon,form='unformatted',access='direct',recl=4*imut*jmut)
  write(6,*) ' MASK(LATLON) read from ', trim(file_mask_latlon)
  read(lun,rec=1) work4_2d
  mask_latlon(1:imut,1:jmut) = real(work4_2d(1:imut,1:jmut),8)
  close(lun)
  mask_latlon(0,1:jmut) = mask_latlon(imut,1:jmut)
  mask_latlon(imut+1,1:jmut) = mask_latlon(1,1:jmut)

  !---------------------------------------------

  write(6,*) 'Product (reduced grid) written to ', trim(file_prod_org)
  open(lun_out,file=file_prod_org,form='unformatted',access='direct',convert='little_endian',action='write',recl=4*total_grid_1d)

  write(6,*) ' DATA (LATLON) read from ', trim(file_prod_latlon)
  open(lun_in,file=file_prod_latlon,form='unformatted',access='direct',action='read',recl=4*imut*jmut)

  LOOP_NUM_DATA: DO m = 1, num_data

  read(lun_in,rec=m) work4_2d

  sst_latlon(1:imut,1:jmut) = real(work4_2d(1:imut,1:jmut),8)
  sst_latlon(     0,1:jmut) = sst_latlon(imut,1:jmut)
  sst_latlon(imut+1,1:jmut) = sst_latlon(   1,1:jmut)

  !----------------------------------------------------------------------------

  i0 = 0

  do j = 1, jmut ! reduced grid loop (j=1 : north pole)

    model_lat = lat_org(jmut-j+1)

    ibgn = i0 + 1
    iend = i0 + num_xgrid(j)

    write(6,'(I6,1a,2i8,1a,f12.5)') j, '(reduced-grid)', ibgn, iend, ', latitude=', model_lat

    ! jj: regular grid ... jj=1 : south pole

    jj = jmut - j + 1
    jn = min(jj+1,jmut)
    js = max(jj-1,1)

    allocate(sst_org (1:num_xgrid(j)))
    allocate(mask_org(1:num_xgrid(j)))
    allocate(lon_org (1:num_xgrid(j)))

    mask_org (1:num_xgrid(j)) = data_mask (ibgn:iend)

    if (num_xgrid(j) == imut) then

      sst_org (1:num_xgrid(j)) = sst_latlon(1:imut,jj)

      do i = 1, num_xgrid(j)
        if (sst_org(i) == undef8_in) then
          sst_org(i) = undef8_out
        end if
      end do
      do i = 1, num_xgrid(j)
        if ((mask_org(i) == 1.0d0) .and. (sst_org(i) == undef8_out)) then
          sst_org(i) = blank_value
        end if
      end do
    else

      dlon_rg = 360.d0 / real(num_xgrid(j),8)

      do i = 1, num_xgrid(j)
        lon_org(i) = dlon_rg * real(i-1,8)
      end do

      do i = 1, num_xgrid(j)

        if (mask_org(i) == 1.0d0) then ! oceanic grid
          model_lon = lon_org(i)
          loop_reg_ii: do ii = 1, imut + 1
            if ((lon_reg(ii-1) <= model_lon) .and. (model_lon < lon_reg(ii))) then
              iw = ii - 1
              ie = ii
              wgt_x = (model_lon - lon_reg(iw)) / (lon_reg(ie) - lon_reg(iw))
              l_filled = .false.

              if ((mask_latlon(iw,jj) /= 0.0d0) .and. (mask_latlon(ie,jj) /= 0.0d0)) then ! surrounded by ocean
                if ((sst_latlon(iw,jj) /= undef8_in) .and. (sst_latlon(ie,jj) /= undef8_in)) then
                  sst_tmp = (1.0d0 - wgt_x) * sst_latlon(iw,jj) + wgt_x * sst_latlon(ie,jj)
                  l_filled = .true.
                end if
              end if

              if (.not. l_filled) then ! search valid oceanic value

                weight_total = 0.0d0
                sst_tmp = 0.0d0
                if ((mask_latlon(iw,jj) /= 0.0d0) .and. (sst_latlon(iw,jj) /= undef8_in)) then
                  dist_tmp = distance_rad(lon_reg(iw),lat_org(jj),model_lon,model_lat)
                  if (dist_tmp > radian_r * 1.5d0) then
                    write(6,*) ' erroneous distance: ', dist_tmp * radian
                    write(6,*) lon_reg(iw),lat_org(jj),model_lon,model_lat
                    stop
                  end if
                  weight = 1.0d0 / (dist_tmp + eps_dist)
                  weight_total = weight_total + weight
                  sst_tmp = sst_tmp + sst_latlon(iw,jj) * weight
                end if
                if ((mask_latlon(ie,jj) /= 0.0d0) .and. (sst_latlon(ie,jj) /= undef8_in)) then
                  dist_tmp = distance_rad(lon_reg(ie),lat_org(jj),model_lon,model_lat)
                  if (dist_tmp > radian_r * 1.5d0) then
                    write(6,*) ' erroneous distance: ', dist_tmp * radian
                    write(6,*) lon_reg(ie),lat_org(jj),model_lon,model_lat
                    stop
                  end if
                  weight = 1.0d0 / (dist_tmp + eps_dist)
                  weight_total = weight_total + weight
                  sst_tmp = sst_tmp + sst_latlon(ie,jj) * weight
                end if

                if (js /= jj) then ! avoid double count
                  if ((mask_latlon(iw,js) /= 0.0d0) .and. (sst_latlon(iw,js) /= undef8_in)) then
                    dist_tmp = distance_rad(lon_reg(iw),lat_org(js),model_lon,model_lat)
                    if (dist_tmp > radian_r * 1.5d0) then
                      write(6,*) ' erroneous distance: ', dist_tmp * radian
                      write(6,*) lon_reg(iw),lat_org(js),model_lon,model_lat
                      stop
                    end if
                    weight = 1.0d0 / (dist_tmp + eps_dist)
                    weight_total = weight_total + weight
                    sst_tmp = sst_tmp + sst_latlon(iw,js) * weight
                  end if
                  if ((mask_latlon(ie,js) /= 0.0d0) .and. (sst_latlon(ie,js) /= undef8_in)) then
                    dist_tmp = distance_rad(lon_reg(ie),lat_org(js),model_lon,model_lat)
                    if (dist_tmp > radian_r * 1.5d0) then
                      write(6,*) ' erroneous distance: ', dist_tmp * radian
                      write(6,*) lon_reg(ie),lat_org(js),model_lon,model_lat
                      stop
                    end if
                    weight = 1.0d0 / (dist_tmp + eps_dist)
                    weight_total = weight_total + weight
                    sst_tmp = sst_tmp + sst_latlon(ie,js) * weight
                  end if
                end if
                
                if (jn /= jj) then ! avoid double count
                  if ((mask_latlon(iw,jn) /= 0.0d0) .and. (sst_latlon(iw,jn) /= undef8_in)) then
                    dist_tmp = distance_rad(lon_reg(iw),lat_org(jn),model_lon,model_lat)
                    if (dist_tmp > radian_r * 1.5d0) then
                      write(6,*) ' erroneous distance: ', dist_tmp * radian
                      write(6,*) lon_reg(iw),lat_org(jn),model_lon,model_lat
                      stop
                    end if
                    weight = 1.0d0 / (dist_tmp + eps_dist)
                    weight_total = weight_total + weight
                    sst_tmp = sst_tmp + sst_latlon(iw,jn) * weight
                  end if
                  if ((mask_latlon(ie,jn) /= 0.0d0) .and. (sst_latlon(ie,jn) /= undef8_in)) then
                    dist_tmp = distance_rad(lon_reg(ie),lat_org(jn),model_lon,model_lat) 
                    if (dist_tmp > radian_r * 1.5d0) then
                      write(6,*) ' erroneous distance: ', dist_tmp * radian
                      write(6,*) lon_reg(ie),lat_org(jn),model_lon,model_lat
                      stop
                    end if
                    weight = 1.0d0 / (dist_tmp + eps_dist)
                    weight_total = weight_total + weight
                    sst_tmp = sst_tmp + sst_latlon(ie,jn) * weight
                  end if
                end if
                
                if (weight_total > 0.0d0) then
                  sst_tmp = sst_tmp / weight_total
                else
                  hl1 = mask_latlon(iw,jj) + mask_latlon(ie,jj) &
                    & + mask_latlon(iw,js) + mask_latlon(ie,js) &
                    & + mask_latlon(iw,jn) + mask_latlon(ie,jn)
                  if (hl1 == 0.0d0) then
                    sst_tmp = blank_value
                  else
                    !write(6,*) mask_latlon(iw,jj), sst_latlon(iw,jj)
                    !write(6,*) mask_latlon(ie,jj), sst_latlon(ie,jj)
                    !write(6,*) mask_latlon(iw,js), sst_latlon(iw,js)
                    !write(6,*) mask_latlon(ie,js), sst_latlon(ie,js)
                    !write(6,*) mask_latlon(iw,jn), sst_latlon(iw,jn)
                    !write(6,*) mask_latlon(ie,jn), sst_latlon(ie,jn)
                    write(6,*) 'oceanic data cannot be filled '
                    write(6,*) model_lon, model_lat
                    !sst_tmp = undef8_out
                    sst_tmp = blank_value
                  end if
                end if
                
              end if

              exit loop_reg_ii

            end if

          end do loop_reg_ii

          sst_org(i) = sst_tmp

        else

          sst_org(i) = undef8_out

        end if
     
      end do

    end if

    data_sst(ibgn:iend) = sst_org(1:num_xgrid(j))

    deallocate(sst_org)
    deallocate(mask_org)
    deallocate(lon_org)

    i0 = iend
    !write(6,*) j, i0

  end do

  do n = 1, total_grid_1d
    if ((data_mask(n) == 1.0d0) .and. (data_sst(n) == undef8_out)) then
      write(6,*) ' undefined factor on ocean grid', n, m
      stop
    end if
  end do

  write(lun_out,rec=m) real(data_sst,4)

  end DO LOOP_NUM_DATA

  close(lun_out)
  close(lun_in)

  deallocate(work4_2d)

  !---------------------------------------------

end program jra55_regular_to_jra55_reduced_grid
