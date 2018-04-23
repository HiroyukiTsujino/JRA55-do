! -*-F90-*-
program reanl_on_jra55_grid

  use libmxe_para, only: libmxe_para__register &
                     & , clen, pi, radian, radian_r, radius &
                     & , type_libmxe_para
  use libmxe_grid, only: libmxe_grid__register  &
                     & , type_libmxe_grid
  use libmxe_topo, only: libmxe_topo__register, libmxe_topo__aexl &
                     & , type_libmxe_topo
  use libmxe_io, only: libmxe_io__register, type_libmxe_io

  use file_open_close_manager

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
  ! Re-analysis

  type(type_libmxe_para) :: reanlp
  type(type_libmxe_grid) :: reanlg
  type(type_libmxe_topo) :: reanlt
  character(128) :: file_mxe_reanl

  integer(4) :: ireanl, jreanl
  real(8) :: dlon_reanl, dlat_reanl, slon_reanl, slat_reanl
  real(8) :: sst_tmp
  real(8) :: undef8_reanl
  real(4) :: undef4_reanl
  character(len=256) :: file_reanl, file_mask_reanl
  real(8),allocatable :: sst_reanl(:,:)
  real(8),allocatable :: mask_reanl(:,:)
  real(8),allocatable :: lon_reanl(:), lat_reanl(:)
  integer(4) :: irec_reanl
  logical :: l_flip_mask_reanl
  real(8) :: reanl_mask_threshold

  real(8) :: scale_in, offset_in
  logical :: l_little_in
  logical :: l_out_latlon

  !---------------

  real(8) :: distance_rad
  real(8) :: dist_tmp
  real(8) :: wgt_x, wgt_y
  real(8) :: altu, altt, altq, alt_target
  integer(4) :: iw, ie, js, jn, ios

  !---------------------------------------------

  namelist /nml_reanl_on_jra55/ &
       & file_mask, &
       & file_prod_org, &
       & file_prod_latlon, &
       & undef4_out, &
       & imut, jmut, dlon, grid_name, &
       & file_ydef, &
       & undef4_reanl, &
       & file_mxe_reanl, &
       & file_reanl, &
       & file_mask_reanl, &
       & l_flip_mask_reanl, &
       & reanl_mask_threshold, &
       & irec_reanl, &
       & l_little_in, &
       & scale_in, offset_in, &
       & l_out_latlon

  !---------------------------------------------

  l_out_latlon = .false.
  l_flip_mask_reanl = .false.
  l_little_in = .false.
  scale_in = 1.0d0
  offset_in = 0.0d0

  open(lun,file='namelist.reanl_on_jra55')
  read(lun,nml=nml_reanl_on_jra55)
  close(lun)

  undef8_out = real(undef4_out,8)

  !---------------------------------------------
  ! Set Re-analysis

  call libmxe_para__register(reanlp, file_namelist=file_mxe_reanl)

  ireanl = reanlp%imut
  jreanl = reanlp%jmut

  write(6,*) 'Re-analysis grid size = ', ireanl, jreanl

  allocate(sst_reanl(0:ireanl+1,1:jreanl))
  allocate(mask_reanl(0:ireanl+1,1:jreanl))
  allocate(work4_2d(1:ireanl,1:jreanl))
  allocate(lon_reanl(0:ireanl+1),lat_reanl(1:jreanl))

  undef8_reanl = real(undef4_reanl,8)

  call libmxe_grid__register(reanlg,reanlp)

  lon_reanl(1:ireanl) = reanlg%lonu(1:ireanl)
  lon_reanl(0)        = reanlg%lonu(ireanl) - 360.d0
  lon_reanl(ireanl+1) = reanlg%lonu(1) + 360.d0
  lat_reanl(1:jreanl) = reanlg%latu(1:jreanl)

  write(6,*) ' location of re-analysis data '
  do i = 1, ireanl + 1
    write(6,*) i, lon_reanl(i)
  end do
  do j = 1, jreanl
    write(6,*) j, lat_reanl(j)
  end do

  if (l_little_in)  then
    open(lun,file=file_reanl,form='unformatted',access='direct',&
         & convert='little_endian',recl=4*ireanl*jreanl)
  else
    open(lun,file=file_reanl,form='unformatted',access='direct',recl=4*ireanl*jreanl)
  end if
  write(6,*) ' DATA(Reanl) read from ', trim(file_reanl), ' record = ', irec_reanl
  read(lun,rec=irec_reanl) work4_2d
  sst_reanl(1:ireanl,1:jreanl) = real(work4_2d(1:ireanl,1:jreanl),8)
  if (scale_in /= 1.0d0) then
    sst_reanl(1:ireanl,1:jreanl) = sst_reanl(1:ireanl,1:jreanl) * scale_in
  end if
  if (offset_in /= 0.0d0) then
    sst_reanl(1:ireanl,1:jreanl) = sst_reanl(1:ireanl,1:jreanl) + offset_in
  end if

  close(lun)
  sst_reanl(0,1:jreanl) = sst_reanl(ireanl,1:jreanl)
  sst_reanl(ireanl+1,1:jreanl) = sst_reanl(1,1:jreanl)

  if (l_little_in)  then
    open(lun,file=file_mask_reanl,form='unformatted',access='direct', &
         & convert='little_endian',recl=4*ireanl*jreanl)
  else
    open(lun,file=file_mask_reanl,form='unformatted',access='direct',recl=4*ireanl*jreanl)
  end if
  write(6,*) ' MASK(Reanl) read from ', trim(file_mask_reanl)
  read(lun,rec=1) work4_2d
  mask_reanl(1:ireanl,1:jreanl) = real(work4_2d(1:ireanl,1:jreanl),8)
  close(lun)

  if (l_flip_mask_reanl) then
    do j = 1, jreanl
      do i = 1, ireanl
        mask_reanl(i,j) = 1.d0 - mask_reanl(i,j)
      end do
    end do
  end if

  if (reanl_mask_threshold < 1.0d0) then
    do j = 1, jreanl
      do i = 1, ireanl
        if (mask_reanl(i,j) >= reanl_mask_threshold) then
          mask_reanl(i,j) = 1.0d0
        else
          mask_reanl(i,j) = 0.0d0
        end if
      end do
    end do
  end if


  mask_reanl(0,1:jreanl) = mask_reanl(ireanl,1:jreanl)
  mask_reanl(ireanl+1,1:jreanl) = mask_reanl(1,1:jreanl)

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

    do jj = 2, jreanl
      if ((lat_reanl(jj-1) <= model_lat) .and. (model_lat < lat_reanl(jj))) then
        jn = jj
        js = jj - 1
        wgt_y = (model_lat - lat_reanl(js)) / (lat_reanl(jn) - lat_reanl(js))
        exit
      end if
    end do
    if (model_lat < lat_reanl(1)) then
      jn = 1
      js = 1
      wgt_y = 1.0d0
    end if
    if (lat_reanl(jreanl) <= model_lat) then
      jn = jreanl
      js = jreanl
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
      loop_reanl_ii: do ii = 1, ireanl + 1
        if ((lon_reanl(ii-1) <= model_lon) .and.  (model_lon < lon_reanl(ii))) then
          iw = ii - 1
          ie = ii
          wgt_x = (model_lon - lon_reanl(iw)) / (lon_reanl(ie) - lon_reanl(iw))
          hld1 = (1.0d0-wgt_x)*(1.0d0-wgt_y)*sst_reanl(iw,js) &
            & +         wgt_x *(1.0d0-wgt_y)*sst_reanl(ie,js) &
            & +  (1.0d0-wgt_x)*       wgt_y *sst_reanl(iw,jn) &
            & +         wgt_x *       wgt_y *sst_reanl(ie,jn)
          if (mask_org(i) == 1.0d0) then ! oceanic grid
            if ((mask_reanl(iw,js) /= 0.0d0) .and. (mask_reanl(ie,js) /= 0.0d0) &
         & .and.(mask_reanl(iw,jn) /= 0.0d0) .and. (mask_reanl(ie,jn) /= 0.0d0)) then
              sst_tmp = hld1
            else
              weight_total = 0.0d0
              sst_tmp = 0.0d0
              if (mask_reanl(iw,js) /= 0.0d0) then
                dist_tmp = distance_rad(lon_reanl(iw),lat_reanl(js),model_lon,model_lat)
                if (dist_tmp > radian_r * 3.0d0) then
                  write(6,*) ' erroneous distance (REANL): ', dist_tmp * radian
                  write(6,*) lon_reanl(iw),lat_reanl(js),model_lon,model_lat
                  stop
                end if
                weight = 1.0d0 / (dist_tmp + eps_dist)
                weight_total = weight_total + weight
                sst_tmp = sst_tmp + sst_reanl(iw,js) * weight
              end if
              if (mask_reanl(ie,js) /= 0.0d0) then
                dist_tmp = distance_rad(lon_reanl(ie),lat_reanl(js),model_lon,model_lat)
                if (dist_tmp > radian_r * 3.0d0) then
                  write(6,*) ' erroneous distance (REANL): ', dist_tmp * radian
                  write(6,*) lon_reanl(ie),lat_reanl(js),model_lon,model_lat
                  stop
                end if
                weight = 1.0d0 / (dist_tmp + eps_dist)
                weight_total = weight_total + weight
                sst_tmp = sst_tmp + sst_reanl(ie,js) * weight
              end if
              if (mask_reanl(iw,jn) /= 0.0d0) then
                dist_tmp = distance_rad(lon_reanl(iw),lat_reanl(jn),model_lon,model_lat)
                if (dist_tmp > radian_r * 3.0d0) then
                  write(6,*) ' erroneous distance (REANL): ', dist_tmp * radian
                  write(6,*) lon_reanl(iw),lat_reanl(jn),model_lon,model_lat
                  stop
                end if
                weight = 1.0d0 / (dist_tmp + eps_dist)
                weight_total = weight_total + weight
                sst_tmp = sst_tmp + sst_reanl(iw,jn) * weight
              end if
              if (mask_reanl(ie,jn) /= 0.0d0) then
                dist_tmp = distance_rad(lon_reanl(ie),lat_reanl(jn),model_lon,model_lat) 
                if (dist_tmp > radian_r * 3.0d0) then
                  write(6,*) ' erroneous distance (REANL): ', dist_tmp * radian
                  write(6,*) lon_reanl(ie),lat_reanl(jn),model_lon,model_lat
                  stop
                end if
                weight = 1.0d0 / (dist_tmp + eps_dist)
                weight_total = weight_total + weight
                sst_tmp = sst_tmp + sst_reanl(ie,jn) * weight
              end if
              if (weight_total > 0.0d0) then
                sst_tmp = sst_tmp / weight_total
              else
                sst_tmp = hld1
              end if
            end if
          else ! land grid, bi-linear
            sst_tmp = hld1
          end if
          exit loop_reanl_ii
        end if
      end do loop_reanl_ii

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

  end if IF_LATLON

  !---------------------------------------------

end program reanl_on_jra55_grid
