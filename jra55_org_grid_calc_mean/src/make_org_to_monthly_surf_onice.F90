!-*-F90-*-
program make_monthly_time_series_onice

  implicit none

  integer(4) :: imut, jmut
  integer(4),allocatable :: num_xgrid(:)
  character(256) :: grid_name
  integer(4) :: total_grid_1d
  real(8),parameter :: tab = 273.15d0

  integer(4) :: ibgn, iend, i0

  real(4),allocatable :: work4(:), worku(:), workv(:)

  real(8),allocatable :: data_surf_all(:)
  real(8),allocatable :: data_surf_ocn(:)
  real(8),allocatable :: data_surf_ice(:)
  real(8),allocatable :: weight_all(:)
  real(8),allocatable :: weight_ocn(:)
  real(8),allocatable :: weight_ice(:)

  real(8),allocatable :: data_ice(:)
  real(8),allocatable :: data_mask(:)

  real(8),allocatable :: all_latlon(:,:)
  real(8),allocatable :: ice_latlon(:,:)
  real(8),allocatable :: ocn_latlon(:,:)

  real(8),allocatable :: all_org(:), all_new(:)
  real(8),allocatable :: ice_org(:), ice_new(:)
  real(8),allocatable :: ocn_org(:), ocn_new(:)

  real(8),allocatable :: lon_org(:), lon_new(:)
  real(8) :: dlon, dlon_rg

  character(256) :: file_surf
  character(256) :: file_ice
  character(256) :: file_mask

  character(256) :: file_surf_all
  character(256) :: file_surf_ocn
  character(256) :: file_surf_ice

  character(256) :: file_latlon_all
  character(256) :: file_latlon_ice
  character(256) :: file_latlon_ocn

  character(256) :: file_in_base
  character(256) :: file_in_ice_base
  character(256) :: file_out_base
  character(256) :: file_out_latlon_base

  character(256) :: var_out, var_in

  integer(4),parameter :: lun=10
  integer(4) :: int_hour
  integer(4) :: num_data_per_day

  integer(4) :: i, j, ii, n, nd, nh
  real(8) :: weight
  real(8) :: altu, altt, altq, alt_target

  integer(4) :: iii, m, m_left, m_right, i_left, i_right

  logical :: l_out_latlon

  integer(4) :: iyear, imon, iday
  integer(4) :: iyear_next, imon_next, iday_next
  integer(4) :: ihour
  real(8) :: w_hour, total_weight

  integer(4) :: ileap
  integer(4) :: idmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)
  integer(4) :: nday

  real(8),parameter :: undef_jra = 9.999e20
  real(8),parameter :: eps = 1.0d-6

  real(8) :: frac_ratio

  !---------------------------------------------
 
  namelist /nml_make_monthly_surf_onice/ &
       & iyear, imon,            &
       & int_hour,               &
       & file_in_base,           &
       & file_in_ice_base,       &
       & var_in,                 &
       & file_out_base,          &
       & var_out,                &
       & file_out_latlon_base,   &
       & file_mask,              &
       & l_out_latlon,           &
       & imut, jmut, dlon, grid_name, &
       & frac_ratio

  !---------------------------------------------

  frac_ratio = 1.0d0

  open(lun,file='namelist.make_monthly_surf_onice')
  read(lun,nml=nml_make_monthly_surf_onice)
  close(lun)

  num_data_per_day = 24 / int_hour

  !---------------------------------------------

  ileap = 0
  if (mod(iyear,4) == 0) ileap = 1
  if (mod(iyear,100) == 0) ileap = 0
  if (mod(iyear,400) == 0) ileap = 1
  idmon(2) = idmon(2) + ileap

  allocate(num_xgrid(1:jmut))
  allocate(lon_new(1:imut))

  do i = 1, imut
    lon_new(i) = dlon * real(i-1,8)
  end do

  call set_reduced_grid(grid_name,num_xgrid,jmut,total_grid_1d)

  allocate(work4(1:total_grid_1d))

  allocate(data_surf_all(1:total_grid_1d))
  allocate(data_surf_ocn(1:total_grid_1d))
  allocate(data_surf_ice(1:total_grid_1d))

  allocate(weight_all(1:total_grid_1d))
  allocate(weight_ocn(1:total_grid_1d))
  allocate(weight_ice(1:total_grid_1d))

  allocate(data_ice (1:total_grid_1d))
  allocate(data_mask(1:total_grid_1d))

  open(lun,file=file_mask,form='unformatted',action='read',access='direct', &
       & convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_mask(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  !------

  data_surf_all(1:total_grid_1d) = 0.0d0
  data_surf_ocn(1:total_grid_1d) = 0.0d0
  data_surf_ice(1:total_grid_1d) = 0.0d0

  weight_all(1:total_grid_1d) = 0.0d0
  weight_ocn(1:total_grid_1d) = 0.0d0
  weight_ice(1:total_grid_1d) = 0.0d0

  total_weight = 0.0d0

  nday = idmon(imon)

  write(6,'(1a,i6,1a,i4,1a,i4)') &
       & ' YEAR = ', iyear, ' MON = ', imon,  ' DAYS of this month = ', nday

  do nd = 1, nday
    do nh = 1, num_data_per_day

      ihour = int_hour * (nh - 1)

      if ((nd == 1) .and. (nh == 1)) then
        w_hour = real(int_hour,8) * 0.5d0
      else
        w_hour = real(int_hour,8)
      end if

      total_weight = total_weight + w_hour

      write(file_ice,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2,i2.2)') &
           & trim(file_in_ice_base),'/', iyear, imon,'/ice.',iyear,imon,nd,ihour
      open(lun,file=file_ice,form='unformatted',action='read',access='direct',convert='little_endian',recl=4*total_grid_1d)
      write(6,*) 'Input file ', trim(file_ice)
      read(lun,rec=1) work4
      close(lun)
      data_ice(1:total_grid_1d) = real(work4(1:total_grid_1d),8)

      write(file_surf,'(1a,1a,i4.4,i2.2,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') &
           & trim(file_in_base),'/', iyear, imon, '/', trim(var_in), '.',iyear,imon,nd,ihour
      open(lun,file=file_surf,form='unformatted',action='read',access='direct',convert='little_endian',recl=4*total_grid_1d)
      write(6,*) 'Input file ', trim(file_surf)
      read(lun,rec=1) work4
      close(lun)

      do n = 1, total_grid_1d
        if (work4(n) /= undef_jra) then
          weight_all(n) = weight_all(n) + w_hour
          data_surf_all(n) = data_surf_all(n) + real(work4(n),8) * w_hour
          if ((data_ice(n) > 0.0d0) .and. (data_mask(n) == 0.0d0)) then
            weight_ice(n) = weight_ice(n) + w_hour
            data_surf_ice(n) = data_surf_ice(n) + real(work4(n),8) * w_hour
          end if
          if ((data_ice(n) == 0.0d0) .and. (data_mask(n) == 0.0d0)) then
            weight_ocn(n) = weight_ocn(n) + w_hour
            data_surf_ocn(n) = data_surf_ocn(n) + real(work4(n),8) * w_hour
          end if
        end if
      end do

    end do
  end do

  !---------------------------------

  iday_next = 1
  imon_next = imon + 1
  if (imon_next == 13) then
    imon_next = 1
    iyear_next = iyear + 1
  else
    iyear_next = iyear
  end if

  ihour = 0
  w_hour = real(int_hour,8) * 0.5d0
  total_weight = total_weight + w_hour

  if (total_weight /= 24.d0 * real(nday,8)) then
    write(6,*) ' total weight = ', total_weight, ' is errorneous, please check. '
    stop
  end if

  write(file_ice,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2,i2.2)') &
       & trim(file_in_ice_base),'/', iyear_next,imon_next, &
       & '/ice.',iyear_next,imon_next,iday_next,ihour
  open(lun,file=file_ice,form='unformatted',action='read',access='direct',convert='little_endian',recl=4*total_grid_1d)
  write(6,*) 'Input file ', trim(file_ice)
  read(lun,rec=1) work4
  close(lun)
  data_ice(1:total_grid_1d) = real(work4(1:total_grid_1d),8)


  write(file_surf,'(1a,1a,i4.4,i2.2,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') &
       & trim(file_in_base),'/', iyear_next,imon_next, &
       & '/', trim(var_in),'.',iyear_next,imon_next,iday_next,ihour
  open(lun,file=file_surf,form='unformatted',action='read',access='direct',convert='little_endian',recl=4*total_grid_1d)
  write(6,*) 'Input file ', trim(file_surf)
  read(lun,rec=1) work4
  close(lun)

  do n = 1, total_grid_1d
    if (work4(n) /= undef_jra) then
      weight_all(n) = weight_all(n) + w_hour
      data_surf_all(n) = data_surf_all(n) + real(work4(n),8) * w_hour
      if ((data_ice(n) > 0.0d0) .and. (data_mask(n) == 0.0d0)) then
        weight_ice(n) = weight_ice(n) + w_hour
        data_surf_ice(n) = data_surf_ice(n) + real(work4(n),8) * w_hour
      end if
      if ((data_ice(n) == 0.0d0) .and. (data_mask(n) == 0.0d0)) then
        weight_ocn(n) = weight_ocn(n) + w_hour
        data_surf_ocn(n) = data_surf_ocn(n) + real(work4(n),8) * w_hour
      end if
    end if
  end do

  do n = 1, total_grid_1d

    if ((weight_all(n) + eps) >= 24.d0 * real(nday,8)) then
      data_surf_all(n) = data_surf_all(n) / weight_all(n)
    else
      data_surf_all(n) = real(undef_jra,8)
    end if

    if ((weight_ice(n) + eps) >= frac_ratio * 24.d0 * real(nday,8)) then
      data_surf_ice(n) = data_surf_ice(n) / weight_ice(n)
      !write(6,*) weight_ice(n) , data_surf_ice(n) - 273.15d0
    else
      data_surf_ice(n) = real(undef_jra,8)
    end if

    if ((weight_ocn(n) + eps) >= frac_ratio * 24.d0 * real(nday,8)) then
      data_surf_ocn(n) = data_surf_ocn(n) / weight_ocn(n)
    else
      data_surf_ocn(n) = real(undef_jra,8)
    end if

  end do

  !------------------------------------------------------------------------------------

  write(file_surf_all,'(1a,1a,1a,1a,i4.4,i2.2)') trim(file_out_base),'/',trim(var_out),'_all.',iyear,imon
  open(lun,file=file_surf_all,form='unformatted',action='write',access='direct',convert='little_endian',recl=4*total_grid_1d)
  write(6,*) 'Output monthly file ', trim(file_surf_all)
  write(lun,rec=1) real(data_surf_all(1:total_grid_1d),4)
  close(lun)

  write(file_surf_ocn,'(1a,1a,1a,1a,i4.4,i2.2)') trim(file_out_base),'/',trim(var_out),'_ocn.',iyear,imon
  open(lun,file=file_surf_ocn,form='unformatted',action='write',access='direct',convert='little_endian',recl=4*total_grid_1d)
  write(6,*) 'Output monthly file ', trim(file_surf_ocn)
  write(lun,rec=1) real(data_surf_ocn(1:total_grid_1d),4)
  close(lun)

  write(file_surf_ice,'(1a,1a,1a,1a,i4.4,i2.2)') trim(file_out_base),'/',trim(var_out),'_ice.',iyear,imon
  open(lun,file=file_surf_ice,form='unformatted',action='write',access='direct',convert='little_endian',recl=4*total_grid_1d)
  write(6,*) 'Output monthly file ', trim(file_surf_ice)
  write(lun,rec=1) real(data_surf_ice(1:total_grid_1d),4)
  close(lun)

  !-----------------------------------------------------------------------------------------
  ! Following is just for checking, do not use for scientific quality computation

  IF_LATLON: if (l_out_latlon) then 

  allocate(all_latlon(1:imut,1:jmut))
  allocate(ice_latlon(1:imut,1:jmut))
  allocate(ocn_latlon(1:imut,1:jmut))

  allocate(all_new(1:imut))
  allocate(ice_new(1:imut))
  allocate(ocn_new(1:imut))

  i0 = 0

  do j = 1, jmut

    ibgn = i0 + 1
    iend = i0 + num_xgrid(j)

    if (num_xgrid(j) == imut) then

      all_latlon(1:imut,jmut-j+1) = data_surf_all(ibgn:iend)
      ocn_latlon(1:imut,jmut-j+1) = data_surf_ocn(ibgn:iend)
      ice_latlon(1:imut,jmut-j+1) = data_surf_ice(ibgn:iend)

    else

      allocate(all_org(1:num_xgrid(j)+1))
      allocate(ocn_org(1:num_xgrid(j)+1))
      allocate(ice_org(1:num_xgrid(j)+1))

      allocate(lon_org(1:num_xgrid(j)+1))

      all_org(1:num_xgrid(j)) = data_surf_all(ibgn:iend)
      all_org(num_xgrid(j)+1) = data_surf_all(ibgn)
      ocn_org(1:num_xgrid(j)) = data_surf_ocn(ibgn:iend)
      ocn_org(num_xgrid(j)+1) = data_surf_ocn(ibgn)
      ice_org(1:num_xgrid(j)) = data_surf_ice(ibgn:iend)
      ice_org(num_xgrid(j)+1) = data_surf_ice(ibgn)

      dlon_rg = 360.0 / real(num_xgrid(j),8)
      do ii = 1, num_xgrid(j) + 1
        lon_org(ii) = dlon_rg * real(ii-1,8)
      end do
      do i = 1, imut
        do ii = 1, num_xgrid(j)
          if (abs(lon_org(ii) - lon_new(i)) < 1.0d-4) then
            all_new(i) = all_org(ii)
            ice_new(i) = ice_org(ii)
            ocn_new(i) = ocn_org(ii)
            exit
          else if ( (lon_org(ii) < lon_new(i)) .and. (lon_new(i) <= lon_org(ii+1)) ) then
            weight = (lon_new(i) - lon_org(ii)) / (lon_org(ii+1) - lon_org(ii))
            all_new(i) = (1.0d0 - weight) * all_org(ii) + weight * all_org(ii+1)
            if ((ice_org(ii) /= real(undef_jra,8)) .and. (ice_org(ii+1) /= real(undef_jra,8))) then
              ice_new(i) = (1.0d0 - weight) * ice_org(ii) + weight * ice_org(ii+1)
            else
              ice_new(i) = real(undef_jra,8)
            end if
            if ((ocn_org(ii) /= real(undef_jra,8)) .and. (ocn_org(ii+1) /= real(undef_jra,8))) then
              ocn_new(i) = (1.0d0 - weight) * ocn_org(ii) + weight * ocn_org(ii+1)
            else
              ocn_new(i) = real(undef_jra,8)
            end if
            exit
          end if
        end do
      end do

      all_latlon(1:imut,jmut-j+1) = all_new(1:imut)
      ice_latlon(1:imut,jmut-j+1) = ice_new(1:imut)
      ocn_latlon(1:imut,jmut-j+1) = ocn_new(1:imut)

      deallocate(all_org)
      deallocate(ocn_org)
      deallocate(ice_org)
      deallocate(lon_org)

    end if

    i0 = iend

  end do

  write(file_latlon_all,'(1a,1a,1a,1a,i4.4,i2.2)') trim(file_out_latlon_base),'/',trim(var_out),'_all.',iyear,imon
  open(lun,file=file_latlon_all,form='unformatted',action='write',access='direct',recl=4*imut*jmut)
  write(6,*) 'Output lat-lon file ', trim(file_latlon_all)
  write(lun,rec=1) real(all_latlon,4)
  close(lun)

  write(file_latlon_ocn,'(1a,1a,1a,1a,i4.4,i2.2)') trim(file_out_latlon_base),'/',trim(var_out),'_ocn.',iyear,imon
  open(lun,file=file_latlon_ocn,form='unformatted',action='write',access='direct',recl=4*imut*jmut)
  write(6,*) 'Output lat-lon file ', trim(file_latlon_ocn)
  write(lun,rec=1) real(ocn_latlon,4)
  close(lun)

  write(file_latlon_ice,'(1a,1a,1a,1a,i4.4,i2.2)') trim(file_out_latlon_base),'/',trim(var_out),'_ice.',iyear,imon
  open(lun,file=file_latlon_ice,form='unformatted',action='write',access='direct',recl=4*imut*jmut)
  write(6,*) 'Output lat-lon file ', trim(file_latlon_ice)
  write(lun,rec=1) real(ice_latlon,4)
  close(lun)

  end if IF_LATLON

end program make_monthly_time_series_onice
