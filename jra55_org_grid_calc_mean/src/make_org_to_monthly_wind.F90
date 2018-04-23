!-*-F90-*-
program make_monthly_time_series_wind

  implicit none

  integer(4) :: imut, jmut
  integer(4),allocatable :: num_xgrid(:)
  character(256) :: grid_name
  integer(4) :: total_grid_1d
  real(8),parameter :: tab = 273.15d0

  integer(4) :: ibgn, iend, i0

  real(4),allocatable :: work4(:), work4u(:), work4v(:)

  real(8),allocatable :: data_surf_w10m(:)
  real(8),allocatable :: data_surf_u10m(:)
  real(8),allocatable :: data_surf_v10m(:)
  real(8),allocatable :: weight_w10m(:)
!  real(8),allocatable :: weight_u10m(:)
!  real(8),allocatable :: weight_v10m(:)

  real(8),allocatable :: data_mask(:)

  real(8),allocatable :: w10m_latlon(:,:)
  real(8),allocatable :: u10m_latlon(:,:)
  real(8),allocatable :: v10m_latlon(:,:)

  real(8),allocatable :: w10m_org(:), w10m_new(:)
  real(8),allocatable :: u10m_org(:), u10m_new(:)
  real(8),allocatable :: v10m_org(:), v10m_new(:)

  real(8),allocatable :: lon_org(:), lon_new(:)
  real(8) :: dlon, dlon_rg

  character(256) :: file_u10m, file_v10m
  character(256) :: file_mask

  character(256) :: file_surf_w10m
  character(256) :: file_surf_u10m
  character(256) :: file_surf_v10m

  character(256) :: file_latlon_w10m
  character(256) :: file_latlon_u10m
  character(256) :: file_latlon_v10m

  character(256) :: file_in_base
  character(256) :: file_out_base
  character(256) :: file_out_latlon_base

  integer(4),parameter :: lun=10
  integer(4) :: int_hour
  integer(4) :: num_data_per_day

  integer(4) :: i, j, ii, n, nd, nh
  real(8) :: weight
  real(8) :: altu, altt, altq, alt_target

  integer(4) :: iii, m, m_left, m_right, i_left, i_right

  logical :: l_out_latlon
  logical :: l_neutral_wind

  integer(4) :: iyear, imon, iday
  integer(4) :: iyear_next, imon_next, iday_next
  integer(4) :: ihour
  real(8) :: w_hour, total_weight

  integer(4) :: ileap
  integer(4) :: idmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)
  integer(4) :: nday

  real(8) :: hl1, hl2, hl3

  real(8),parameter :: undef_jra = 9.999e20
  real(8),parameter :: eps = 1.0d-6

  !---------------------------------------------
 
  namelist /nml_make_monthly_wind/ &
       & iyear, imon,            &
       & int_hour,               &
       & file_in_base,           &
       & file_out_base,          &
       & file_out_latlon_base,   &
       & file_mask,              &
       & l_out_latlon,           &
       & l_neutral_wind,         &
       & imut, jmut, dlon, grid_name

  !---------------------------------------------

  l_neutral_wind = .false.

  open(lun,file='namelist.make_monthly_wind')
  read(lun,nml=nml_make_monthly_wind)
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
  allocate(work4u(1:total_grid_1d))
  allocate(work4v(1:total_grid_1d))

  allocate(data_surf_w10m(1:total_grid_1d))
  allocate(data_surf_u10m(1:total_grid_1d))
  allocate(data_surf_v10m(1:total_grid_1d))

  allocate(weight_w10m(1:total_grid_1d))
!  allocate(weight_u10m(1:total_grid_1d))
!  allocate(weight_v10m(1:total_grid_1d))

  allocate(data_mask(1:total_grid_1d))

  open(lun,file=file_mask,form='unformatted',action='read',access='direct', &
       & convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_mask(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  !------

  data_surf_w10m(1:total_grid_1d) = 0.0d0
  data_surf_u10m(1:total_grid_1d) = 0.0d0
  data_surf_v10m(1:total_grid_1d) = 0.0d0

  weight_w10m(1:total_grid_1d) = 0.0d0
!  weight_u10m(1:total_grid_1d) = 0.0d0
!  weight_v10m(1:total_grid_1d) = 0.0d0

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

      if (l_neutral_wind) then
        write(file_u10m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2,i2.2)') &
             & trim(file_in_base),'/', iyear, imon, '/un10m.',iyear,imon,nd,ihour
      else
        write(file_u10m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2,i2.2)') &
             & trim(file_in_base),'/', iyear, imon, '/u10m.',iyear,imon,nd,ihour
      end if
      open(lun,file=file_u10m,form='unformatted',action='read',access='direct',convert='little_endian',recl=4*total_grid_1d)
      write(6,*) 'Input file ', trim(file_u10m)
      read(lun,rec=1) work4u
      close(lun)

      if (l_neutral_wind) then
        write(file_v10m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2,i2.2)') &
             & trim(file_in_base),'/', iyear, imon,'/vn10m.',iyear,imon,nd,ihour
      else
        write(file_v10m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2,i2.2)') &
             & trim(file_in_base),'/', iyear, imon,'/v10m.',iyear,imon,nd,ihour
      end if
      open(lun,file=file_v10m,form='unformatted',action='read',access='direct',convert='little_endian',recl=4*total_grid_1d)
      write(6,*) 'Input file ', trim(file_v10m)
      read(lun,rec=1) work4v
      close(lun)

      do n = 1, total_grid_1d
        if ((work4u(n) /= undef_jra) .and. (work4v(n) /= undef_jra))then
          hl1 = real(work4u(n),8)
          hl2 = real(work4v(n),8)
          hl3 = sqrt(hl1**2 + hl2**2)
          data_surf_u10m(n) = data_surf_u10m(n) + hl1 * w_hour
          data_surf_v10m(n) = data_surf_v10m(n) + hl2 * w_hour
          data_surf_w10m(n) = data_surf_w10m(n) + hl3 * w_hour
          weight_w10m(n) = weight_w10m(n) + w_hour
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

  if (l_neutral_wind) then
    write(file_u10m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2,i2.2)') &
         & trim(file_in_base),'/', iyear_next,imon_next, &
         & '/un10m.',iyear_next,imon_next,iday_next,ihour
  else
    write(file_u10m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2,i2.2)') &
         & trim(file_in_base),'/', iyear_next,imon_next, &
         & '/u10m.',iyear_next,imon_next,iday_next,ihour
  end if
  open(lun,file=file_u10m,form='unformatted',action='read',access='direct',convert='little_endian',recl=4*total_grid_1d)
  write(6,*) 'Input file ', trim(file_u10m)
  read(lun,rec=1) work4u
  close(lun)


  if (l_neutral_wind) then
    write(file_v10m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2,i2.2)') &
         & trim(file_in_base),'/', iyear_next,imon_next, &
         & '/vn10m.',iyear_next,imon_next,iday_next,ihour
  else
    write(file_v10m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2,i2.2)') &
         & trim(file_in_base),'/', iyear_next,imon_next, &
         & '/v10m.',iyear_next,imon_next,iday_next,ihour
  end if
  open(lun,file=file_v10m,form='unformatted',action='read',access='direct',convert='little_endian',recl=4*total_grid_1d)
  write(6,*) 'Input file ', trim(file_v10m)
  read(lun,rec=1) work4v
  close(lun)

  do n = 1, total_grid_1d
    if ((work4u(n) /= undef_jra) .and. (work4v(n) /= undef_jra))  then
      hl1 = real(work4u(n),8)
      hl2 = real(work4v(n),8)
      hl3 = sqrt(hl1**2 + hl2**2)
      data_surf_u10m(n) = data_surf_u10m(n) + hl1 * w_hour
      data_surf_v10m(n) = data_surf_v10m(n) + hl2 * w_hour
      data_surf_w10m(n) = data_surf_w10m(n) + hl3 * w_hour
      weight_w10m(n) = weight_w10m(n) + w_hour
    end if
  end do

  do n = 1, total_grid_1d

    if ((weight_w10m(n) + eps) >= 24.d0 * real(nday,8)) then
      data_surf_u10m(n) = data_surf_u10m(n) / weight_w10m(n)
      data_surf_v10m(n) = data_surf_v10m(n) / weight_w10m(n)
      data_surf_w10m(n) = data_surf_w10m(n) / weight_w10m(n)
    else
      write(6,*) ' some data are missing, cannot compute average ', weight_w10m(n), 24.d0 * real(nday,8)
      data_surf_u10m(n) = real(undef_jra,8)
      data_surf_v10m(n) = real(undef_jra,8)
      data_surf_w10m(n) = real(undef_jra,8)
    end if

  end do

  !------------------------------------------------------------------------------------

  if (l_neutral_wind) then
    write(file_surf_w10m,'(1a,1a,i4.4,i2.2)') trim(file_out_base),'/wn10m.',iyear,imon
  else
    write(file_surf_w10m,'(1a,1a,i4.4,i2.2)') trim(file_out_base),'/wind10m.',iyear,imon
  end if
  open(lun,file=file_surf_w10m,form='unformatted',action='write',access='direct',convert='little_endian',recl=4*total_grid_1d)
  write(6,*) 'Output monthly file ', trim(file_surf_w10m)
  write(lun,rec=1) real(data_surf_w10m(1:total_grid_1d),4)
  close(lun)

  if (l_neutral_wind) then
    write(file_surf_u10m,'(1a,1a,i4.4,i2.2)') trim(file_out_base),'/un10m.',iyear,imon
  else
    write(file_surf_u10m,'(1a,1a,i4.4,i2.2)') trim(file_out_base),'/u10m.',iyear,imon
  end if
  open(lun,file=file_surf_u10m,form='unformatted',action='write',access='direct',convert='little_endian',recl=4*total_grid_1d)
  write(6,*) 'Output monthly file ', trim(file_surf_u10m)
  write(lun,rec=1) real(data_surf_u10m(1:total_grid_1d),4)
  close(lun)

  if (l_neutral_wind) then
    write(file_surf_v10m,'(1a,1a,i4.4,i2.2)') trim(file_out_base),'/vn10m.',iyear,imon
  else
    write(file_surf_v10m,'(1a,1a,i4.4,i2.2)') trim(file_out_base),'/v10m.',iyear,imon
  end if
  open(lun,file=file_surf_v10m,form='unformatted',action='write',access='direct',convert='little_endian',recl=4*total_grid_1d)
  write(6,*) 'Output monthly file ', trim(file_surf_v10m)
  write(lun,rec=1) real(data_surf_v10m(1:total_grid_1d),4)
  close(lun)

  !-----------------------------------------------------------------------------------------
  ! Following is just for checking, do not use for scientific quality computation

  IF_LATLON: if (l_out_latlon) then 

  allocate(w10m_latlon(1:imut,1:jmut))
  allocate(v10m_latlon(1:imut,1:jmut))
  allocate(u10m_latlon(1:imut,1:jmut))

  allocate(w10m_new(1:imut))
  allocate(v10m_new(1:imut))
  allocate(u10m_new(1:imut))

  i0 = 0

  do j = 1, jmut

    ibgn = i0 + 1
    iend = i0 + num_xgrid(j)

    if (num_xgrid(j) == imut) then

      u10m_latlon(1:imut,jmut-j+1) = data_surf_u10m(ibgn:iend)
      v10m_latlon(1:imut,jmut-j+1) = data_surf_v10m(ibgn:iend)
      w10m_latlon(1:imut,jmut-j+1) = data_surf_w10m(ibgn:iend)

    else

      allocate(u10m_org(1:num_xgrid(j)+1))
      allocate(v10m_org(1:num_xgrid(j)+1))
      allocate(w10m_org(1:num_xgrid(j)+1))

      allocate(lon_org(1:num_xgrid(j)+1))

      u10m_org(1:num_xgrid(j)) = data_surf_u10m(ibgn:iend)
      u10m_org(num_xgrid(j)+1) = data_surf_u10m(ibgn)
      v10m_org(1:num_xgrid(j)) = data_surf_v10m(ibgn:iend)
      v10m_org(num_xgrid(j)+1) = data_surf_v10m(ibgn)
      w10m_org(1:num_xgrid(j)) = data_surf_w10m(ibgn:iend)
      w10m_org(num_xgrid(j)+1) = data_surf_w10m(ibgn)

      dlon_rg = 360.0 / real(num_xgrid(j),8)
      do ii = 1, num_xgrid(j) + 1
        lon_org(ii) = dlon_rg * real(ii-1,8)
      end do
      do i = 1, imut
        do ii = 1, num_xgrid(j)
          if (abs(lon_org(ii) - lon_new(i)) < 1.0d-4) then
            w10m_new(i) = w10m_org(ii)
            v10m_new(i) = v10m_org(ii)
            u10m_new(i) = u10m_org(ii)
            exit
          else if ( (lon_org(ii) < lon_new(i)) .and. (lon_new(i) <= lon_org(ii+1)) ) then
            weight = (lon_new(i) - lon_org(ii)) / (lon_org(ii+1) - lon_org(ii))
            if ((u10m_org(ii) /= real(undef_jra,8)) .and. (u10m_org(ii+1) /= real(undef_jra,8))) then
              u10m_new(i) = (1.0d0 - weight) * u10m_org(ii) + weight * u10m_org(ii+1)
            else
              u10m_new(i) = real(undef_jra,8)
            end if
            if ((v10m_org(ii) /= real(undef_jra,8)) .and. (v10m_org(ii+1) /= real(undef_jra,8))) then
              v10m_new(i) = (1.0d0 - weight) * v10m_org(ii) + weight * v10m_org(ii+1)
            else
              v10m_new(i) = real(undef_jra,8)
            end if
            if ((w10m_org(ii) /= real(undef_jra,8)) .and. (w10m_org(ii+1) /= real(undef_jra,8))) then
              w10m_new(i) = (1.0d0 - weight) * w10m_org(ii) + weight * w10m_org(ii+1)
            else
              w10m_new(i) = real(undef_jra,8)
            end if
            exit
          end if
        end do
      end do

      u10m_latlon(1:imut,jmut-j+1) = u10m_new(1:imut)
      v10m_latlon(1:imut,jmut-j+1) = v10m_new(1:imut)
      w10m_latlon(1:imut,jmut-j+1) = w10m_new(1:imut)

      deallocate(w10m_org)
      deallocate(u10m_org)
      deallocate(v10m_org)
      deallocate(lon_org)

    end if

    i0 = iend

  end do

  if (l_neutral_wind) then
    write(file_latlon_u10m,'(1a,1a,i4.4,i2.2)') trim(file_out_latlon_base),'/un10m.',iyear,imon
  else
    write(file_latlon_u10m,'(1a,1a,i4.4,i2.2)') trim(file_out_latlon_base),'/u10m.',iyear,imon
  end if
  open(lun,file=file_latlon_u10m,form='unformatted',action='write',access='direct',recl=4*imut*jmut)
  write(6,*) 'Output lat-lon file ', trim(file_latlon_u10m)
  write(lun,rec=1) real(u10m_latlon,4)
  close(lun)

  if (l_neutral_wind) then
    write(file_latlon_v10m,'(1a,1a,i4.4,i2.2)') trim(file_out_latlon_base),'/vn10m.',iyear,imon
  else
    write(file_latlon_v10m,'(1a,1a,i4.4,i2.2)') trim(file_out_latlon_base),'/v10m.',iyear,imon
  end if
  open(lun,file=file_latlon_v10m,form='unformatted',action='write',access='direct',recl=4*imut*jmut)
  write(6,*) 'Output lat-lon file ', trim(file_latlon_v10m)
  write(lun,rec=1) real(v10m_latlon,4)
  close(lun)

  if (l_neutral_wind) then
    write(file_latlon_w10m,'(1a,1a,i4.4,i2.2)') trim(file_out_latlon_base),'/wn10m.',iyear,imon
  else
    write(file_latlon_w10m,'(1a,1a,i4.4,i2.2)') trim(file_out_latlon_base),'/wind10m.',iyear,imon
  end if
  open(lun,file=file_latlon_w10m,form='unformatted',action='write',access='direct',recl=4*imut*jmut)
  write(6,*) 'Output lat-lon file ', trim(file_latlon_w10m)
  write(lun,rec=1) real(w10m_latlon,4)
  close(lun)

  end if IF_LATLON

end program make_monthly_time_series_wind
