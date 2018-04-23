!-*-F90-*-
program make_daily_time_series

  ! shift 2m air temperature and specific humidity to those at 10 m

  implicit none

  integer(4) :: imut, jmut
  integer(4),allocatable :: num_xgrid(:)
  character(256) :: grid_name
  integer(4) :: total_grid_1d
  real(8),parameter :: tab = 273.15d0

  integer(4) :: ibgn, iend, i0

  real(4),allocatable :: work4(:), worku(:), workv(:)

  real(8),allocatable :: data_tmp2m(:), data_sph2m(:)
  real(8),allocatable :: data_u10m(:), data_v10m(:)
  real(8),allocatable :: data_swind(:)
  real(8),allocatable :: data_brtmp(:), data_slprs(:)
  real(8),allocatable :: data_ice(:)
  real(8),allocatable :: data_mask(:)

  real(8),allocatable :: daily_tmp2m(:), daily_sph2m(:)
  real(8),allocatable :: daily_u10m(:),  daily_v10m(:)
  real(8),allocatable :: daily_swind(:)
  real(8),allocatable :: daily_brtmp(:), daily_slprs(:)
  real(8),allocatable :: daily_ice(:)

  real(8),allocatable :: tmp2m_latlon(:,:)
  real(8),allocatable :: sph2m_latlon(:,:)
  real(8),allocatable :: u10m_latlon(:,:)
  real(8),allocatable :: v10m_latlon(:,:)
  real(8),allocatable :: swind_latlon(:,:)
  real(8),allocatable :: brtmp_latlon(:,:)
  real(8),allocatable :: slprs_latlon(:,:)
  real(8),allocatable :: ice_latlon(:,:)

  real(8),allocatable :: tmp2m_org(:), tmp2m_new(:)
  real(8),allocatable :: sph2m_org(:), sph2m_new(:)
  real(8),allocatable :: u10m_org(:), u10m_new(:)
  real(8),allocatable :: v10m_org(:), v10m_new(:)
  real(8),allocatable :: swind_org(:), swind_new(:)
  real(8),allocatable :: brtmp_org(:), brtmp_new(:)
  real(8),allocatable :: slprs_org(:), slprs_new(:)
  real(8),allocatable :: ice_org(:), ice_new(:)

  real(8),allocatable :: lon_org(:), lon_new(:)
  real(8) :: dlon, dlon_rg

  character(256) :: file_tmp2m
  character(256) :: file_sph2m
  character(256) :: file_u10m
  character(256) :: file_v10m
  character(256) :: file_brtmp
  character(256) :: file_slprs
  character(256) :: file_ice
  character(256) :: file_mask

  character(256) :: file_daily_tmp2m
  character(256) :: file_daily_sph2m
  character(256) :: file_daily_u10m
  character(256) :: file_daily_v10m
  character(256) :: file_daily_swind
  character(256) :: file_daily_brtmp
  character(256) :: file_daily_slprs
  character(256) :: file_daily_ice

  character(256) :: file_latlon_tmp2m
  character(256) :: file_latlon_sph2m
  character(256) :: file_latlon_u10m
  character(256) :: file_latlon_v10m
  character(256) :: file_latlon_swind
  character(256) :: file_latlon_brtmp
  character(256) :: file_latlon_slprs
  character(256) :: file_latlon_ice

  character(256) :: file_surf_base
  character(256) :: file_daily_base
  character(256) :: file_daily_latlon_base

  integer(4),parameter :: lun=10
  integer(4) :: int_hour
  integer(4) :: num_data_per_day

  integer(4) :: i, j, ii, n, nh
  real(8) :: weight
  real(8) :: altu, altt, altq, alt_target

  integer(4) :: iii, m, m_left, m_right, i_left, i_right

  logical :: l_daily_tmp2m, l_daily_sph2m
  logical :: l_daily_wind
  logical :: l_daily_brtmp, l_daily_slprs
  logical :: l_daily_ice

  logical :: l_latlon_tmp2m, l_latlon_sph2m
  logical :: l_latlon_wind
  logical :: l_latlon_brtmp, l_latlon_slprs
  logical :: l_latlon_ice

  logical :: l_neutral_wind

  integer(4) :: iyear, imon, iday
  integer(4) :: iyear_next, imon_next, iday_next
  integer(4) :: ihour
  real(8) :: w_hour, total_weight

  integer(4) :: ileap
  integer(4) :: idmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  !---------------------------------------------
 
  namelist /nml_make_daily_surf/ &
       & iyear, imon, iday,      &
       & int_hour,               &
       & file_surf_base,         &
       & file_daily_base,        &
       & file_daily_latlon_base, &
       & file_mask,              &
       & l_daily_tmp2m, l_daily_sph2m,   &
       & l_daily_wind, &
       & l_daily_brtmp, l_daily_slprs,   &
       & l_daily_ice,     &
       & l_latlon_tmp2m, l_latlon_sph2m, &
       & l_latlon_wind, &
       & l_latlon_brtmp, l_latlon_slprs, &
       & l_latlon_ice,    &
       & l_neutral_wind,  &
       & imut, jmut, dlon, grid_name

  !---------------------------------------------

  l_neutral_wind = .false.
  open(lun,file='namelist.make_daily_surf')
  read(lun,nml=nml_make_daily_surf)
  close(lun)

  num_data_per_day = 24 / int_hour

  !---------------------------------------------

  ileap = 0
  if (mod(iyear,4) == 0) ileap = 1
  if (mod(iyear,100) == 0) ileap = 0
  if (mod(iyear,400) == 0) ileap = 1
  idmon(2) = idmon(2) + ileap

  iday_next = iday + 1
  if (iday_next > idmon(imon)) then
    imon_next = imon + 1
    iday_next = 1
    if (imon_next == 13) then
      imon_next = 1
      iyear_next = iyear + 1
    else
      iyear_next = iyear
    end if
  else
    imon_next = imon
    iyear_next = iyear
  end if

  allocate(num_xgrid(1:jmut))
  allocate(tmp2m_latlon(1:imut,1:jmut))
  allocate(sph2m_latlon(1:imut,1:jmut))
  allocate(u10m_latlon(1:imut,1:jmut))
  allocate(v10m_latlon(1:imut,1:jmut))
  allocate(swind_latlon(1:imut,1:jmut))
  allocate(brtmp_latlon(1:imut,1:jmut))
  allocate(slprs_latlon(1:imut,1:jmut))
  allocate(ice_latlon(1:imut,1:jmut))

  allocate(tmp2m_new(1:imut))
  allocate(sph2m_new(1:imut))
  allocate(u10m_new(1:imut))
  allocate(v10m_new(1:imut))
  allocate(swind_new(1:imut))
  allocate(brtmp_new(1:imut))
  allocate(slprs_new(1:imut))
  allocate(ice_new(1:imut))
  allocate(lon_new(1:imut))

  do i = 1, imut
    lon_new(i) = dlon * real(i-1,8)
  end do

  call set_reduced_grid(grid_name,num_xgrid,jmut,total_grid_1d)

  allocate(work4(1:total_grid_1d))

  if (l_daily_wind) then
    allocate(worku(1:total_grid_1d))
    allocate(workv(1:total_grid_1d))
  end if

  allocate(data_tmp2m(1:total_grid_1d))
  allocate(data_sph2m(1:total_grid_1d))
  allocate(data_u10m (1:total_grid_1d))
  allocate(data_v10m (1:total_grid_1d))
  allocate(data_swind(1:total_grid_1d))
  allocate(data_brtmp(1:total_grid_1d))
  allocate(data_slprs(1:total_grid_1d))
  allocate(data_ice (1:total_grid_1d))

  allocate(data_mask(1:total_grid_1d))

  open(lun,file=file_mask,form='unformatted',action='read',access='direct', &
       & convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_mask(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)

  !------

  data_tmp2m(1:total_grid_1d) = 0.0d0
  data_sph2m(1:total_grid_1d) = 0.0d0
  data_u10m (1:total_grid_1d) = 0.0d0
  data_v10m (1:total_grid_1d) = 0.0d0
  data_swind(1:total_grid_1d) = 0.0d0
  data_brtmp(1:total_grid_1d) = 0.0d0
  data_slprs(1:total_grid_1d) = 0.0d0
  data_ice  (1:total_grid_1d) = 0.0d0

  total_weight = 0.0d0

  do nh = 1, num_data_per_day

    ihour = int_hour * (nh - 1)
    if (nh == 1) then
      w_hour = real(int_hour,8) * 0.5d0
    else
      w_hour = real(int_hour,8)
    end if

    total_weight = total_weight + w_hour

    if (l_daily_tmp2m) then
      write(file_tmp2m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2,i2.2)') &
           & trim(file_surf_base),'/', iyear, imon, &
           & '/tmp2m.',iyear,imon,iday,ihour
      open(lun,file=file_tmp2m,form='unformatted',action='read',access='direct', &
           & convert='little_endian',recl=4*total_grid_1d)
      write(6,*) 'Input file ', trim(file_tmp2m)
      read(lun,rec=1) work4
      data_tmp2m(1:total_grid_1d) = data_tmp2m(1:total_grid_1d) + real(work4(1:total_grid_1d),8) * w_hour
      close(lun)
    end if

    if (l_daily_sph2m) then
      write(file_sph2m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2,i2.2)') &
           & trim(file_surf_base),'/', iyear, imon, &
           & '/sph2m.',iyear,imon,iday,ihour
      open(lun,file=file_sph2m,form='unformatted',action='read',access='direct',convert='little_endian',recl=4*total_grid_1d)
      write(6,*) 'Input file ', trim(file_sph2m)
      read(lun,rec=1) work4
      data_sph2m(1:total_grid_1d) = data_sph2m(1:total_grid_1d) + real(work4(1:total_grid_1d),8) * w_hour
      close(lun)
    end if

    if (l_daily_wind) then
      if (l_neutral_wind) then
        write(file_u10m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2,i2.2)') &
             & trim(file_surf_base),'/', iyear, imon, &
             & '/un10m.',iyear,imon,iday,ihour
      else
        write(file_u10m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2,i2.2)') &
             & trim(file_surf_base),'/', iyear, imon, &
             & '/u10m.',iyear,imon,iday,ihour
      end if
      open(lun,file=file_u10m,form='unformatted',action='read',access='direct',convert='little_endian',recl=4*total_grid_1d)
      write(6,*) 'Input file ', trim(file_u10m)
      read(lun,rec=1) worku
      data_u10m(1:total_grid_1d) = data_u10m(1:total_grid_1d) + real(worku(1:total_grid_1d),8) * w_hour
      close(lun)

      if (l_neutral_wind) then
        write(file_v10m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2,i2.2)') &
             & trim(file_surf_base),'/', iyear, imon, &
             & '/vn10m.',iyear,imon,iday,ihour
      else
        write(file_v10m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2,i2.2)') &
             & trim(file_surf_base),'/', iyear, imon, &
             & '/v10m.',iyear,imon,iday,ihour
      end if
      open(lun,file=file_v10m,form='unformatted',action='read',access='direct',convert='little_endian',recl=4*total_grid_1d)
      write(6,*) 'Input file ', trim(file_v10m)
      read(lun,rec=1) workv
      data_v10m(1:total_grid_1d) = data_v10m(1:total_grid_1d) + real(workv(1:total_grid_1d),8) * w_hour
      close(lun)

      data_swind(1:total_grid_1d) = data_swind(1:total_grid_1d) &
           & + sqrt(real(worku(1:total_grid_1d),8)**2 + real(workv(1:total_grid_1d),8)**2) * w_hour
    end if

    if (l_daily_brtmp) then
      write(file_brtmp,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2,i2.2)') &
           & trim(file_surf_base),'/', iyear, imon, &
           & '/brtmp.',iyear,imon,iday,ihour
      open(lun,file=file_brtmp,form='unformatted',action='read',access='direct',convert='little_endian',recl=4*total_grid_1d)
      write(6,*) 'Input file ', trim(file_brtmp)
      read(lun,rec=1) work4
      data_brtmp(1:total_grid_1d) = data_brtmp(1:total_grid_1d) + real(work4(1:total_grid_1d),8) * w_hour
      close(lun)
    end if

    if (l_daily_slprs) then
      write(file_slprs,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2,i2.2)') &
           & trim(file_surf_base),'/', iyear, imon, &
           & '/slprs.',iyear,imon,iday,ihour
      open(lun,file=file_slprs,form='unformatted',action='read',access='direct',convert='little_endian',recl=4*total_grid_1d)
      write(6,*) 'Input file ', trim(file_slprs)
      read(lun,rec=1) work4
      data_slprs(1:total_grid_1d) = data_slprs(1:total_grid_1d) + real(work4(1:total_grid_1d),8) * w_hour
      close(lun)
    end if

    if (l_daily_ice) then
      write(file_ice,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2,i2.2)') &
           & trim(file_surf_base),'/', iyear, imon, &
           & '/ice.',iyear,imon,iday,ihour
      open(lun,file=file_ice,form='unformatted',action='read',access='direct',convert='little_endian',recl=4*total_grid_1d)
      write(6,*) 'Input file ', trim(file_ice)
      read(lun,rec=1) work4
      data_ice(1:total_grid_1d) = data_ice(1:total_grid_1d) + real(work4(1:total_grid_1d),8) * w_hour
      close(lun)
    end if

  end do

  !---------------------------------

  ihour = 0
  w_hour = real(int_hour,8) * 0.5d0
  total_weight = total_weight + w_hour

  if (total_weight /= 24.d0) then
    write(6,*) ' total weight = ', total_weight, ' is errorneous, please check. '
    stop
  end if

  if (l_daily_tmp2m) then
    write(file_tmp2m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2,i2.2)') &
         & trim(file_surf_base),'/', iyear_next,imon_next, &
         & '/tmp2m.',iyear_next,imon_next,iday_next,ihour
    open(lun,file=file_tmp2m,form='unformatted',action='read',access='direct',convert='little_endian',recl=4*total_grid_1d)
    write(6,*) 'Input file ', trim(file_tmp2m)
    read(lun,rec=1) work4
    data_tmp2m(1:total_grid_1d) = data_tmp2m(1:total_grid_1d) + real(work4(1:total_grid_1d),8) * w_hour
    close(lun)
    data_tmp2m(1:total_grid_1d) = data_tmp2m(1:total_grid_1d) / 24.0d0
  end if

  if (l_daily_sph2m) then
    write(file_sph2m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2,i2.2)') &
         & trim(file_surf_base),'/', iyear_next,imon_next, &
         & '/sph2m.',iyear_next,imon_next,iday_next,ihour
    open(lun,file=file_sph2m,form='unformatted',action='read',access='direct',convert='little_endian',recl=4*total_grid_1d)
    write(6,*) 'Input file ', trim(file_sph2m)
    read(lun,rec=1) work4
    data_sph2m(1:total_grid_1d) = data_sph2m(1:total_grid_1d) + real(work4(1:total_grid_1d),8) * w_hour
    close(lun)
    data_sph2m(1:total_grid_1d) = data_sph2m(1:total_grid_1d) / 24.0d0
  end if

  if (l_daily_wind) then

    if (l_neutral_wind) then
      write(file_u10m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2,i2.2)') &
           & trim(file_surf_base),'/', iyear_next,imon_next, &
           & '/un10m.',iyear_next,imon_next,iday_next,ihour
    else
      write(file_u10m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2,i2.2)') &
           & trim(file_surf_base),'/', iyear_next,imon_next, &
           & '/u10m.',iyear_next,imon_next,iday_next,ihour
    end if
    open(lun,file=file_u10m,form='unformatted',action='read',access='direct',convert='little_endian',recl=4*total_grid_1d)
    write(6,*) 'Input file ', trim(file_u10m)
    read(lun,rec=1) worku
    data_u10m(1:total_grid_1d) = data_u10m(1:total_grid_1d) + real(worku(1:total_grid_1d),8) * w_hour
    close(lun)
    data_u10m (1:total_grid_1d) = data_u10m (1:total_grid_1d) / 24.0d0

    if (l_neutral_wind) then
      write(file_v10m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2,i2.2)') &
           & trim(file_surf_base),'/', iyear_next,imon_next, &
           & '/vn10m.',iyear_next,imon_next,iday_next,ihour
    else
      write(file_v10m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2,i2.2)') &
           & trim(file_surf_base),'/', iyear_next,imon_next, &
           & '/v10m.',iyear_next,imon_next,iday_next,ihour
    end if
    open(lun,file=file_v10m,form='unformatted',action='read',access='direct',convert='little_endian',recl=4*total_grid_1d)
    write(6,*) 'Input file ', trim(file_v10m)
    read(lun,rec=1) workv
    data_v10m(1:total_grid_1d) = data_v10m(1:total_grid_1d) + real(workv(1:total_grid_1d),8) * w_hour
    close(lun)
    data_v10m (1:total_grid_1d) = data_v10m (1:total_grid_1d) / 24.0d0

    data_swind(1:total_grid_1d) = data_swind(1:total_grid_1d) &
         & + sqrt(real(worku(1:total_grid_1d),8)**2 + real(workv(1:total_grid_1d),8)**2) * w_hour

    data_swind(1:total_grid_1d) = data_swind(1:total_grid_1d) / 24.0d0

  end if

  if (l_daily_brtmp) then
    write(file_brtmp,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2,i2.2)') &
         & trim(file_surf_base),'/', iyear_next,imon_next, &
         & '/brtmp.',iyear_next,imon_next,iday_next,ihour
    open(lun,file=file_brtmp,form='unformatted',action='read',access='direct',convert='little_endian',recl=4*total_grid_1d)
    write(6,*) 'Input file ', trim(file_brtmp)
    read(lun,rec=1) work4
    data_brtmp(1:total_grid_1d) = data_brtmp(1:total_grid_1d) + real(work4(1:total_grid_1d),8) * w_hour
    close(lun)
    data_brtmp(1:total_grid_1d) = data_brtmp(1:total_grid_1d) / 24.0d0
  end if

  if (l_daily_slprs) then
    write(file_slprs,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2,i2.2)') &
         & trim(file_surf_base),'/', iyear_next,imon_next, &
         & '/slprs.',iyear_next,imon_next,iday_next,ihour
    open(lun,file=file_slprs,form='unformatted',action='read',access='direct',convert='little_endian',recl=4*total_grid_1d)
    write(6,*) 'Input file ', trim(file_slprs)
    read(lun,rec=1) work4
    data_slprs(1:total_grid_1d) = data_slprs(1:total_grid_1d) + real(work4(1:total_grid_1d),8) * w_hour
    close(lun)
    data_slprs(1:total_grid_1d) = data_slprs(1:total_grid_1d) / 24.0d0
  end if

  if (l_daily_ice) then
    write(file_ice,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2,i2.2)') &
         & trim(file_surf_base),'/', iyear_next,imon_next, &
         & '/ice.',iyear_next,imon_next,iday_next,ihour
    open(lun,file=file_ice,form='unformatted',action='read',access='direct',convert='little_endian',recl=4*total_grid_1d)
    write(6,*) 'Input file ', trim(file_ice)
    read(lun,rec=1) work4
    data_ice(1:total_grid_1d) = data_ice(1:total_grid_1d) + real(work4(1:total_grid_1d),8) * w_hour
    close(lun)
    data_ice (1:total_grid_1d) = data_ice (1:total_grid_1d) / 24.0d0
  end if

  !------------------------------------------------------------------------------------

  if (l_daily_tmp2m) then
    write(file_daily_tmp2m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2)') &
         & trim(file_daily_base),'/',iyear,imon, &
         & '/tmp2m.',iyear,imon,iday
    open(lun,file=file_daily_tmp2m,form='unformatted',action='write',access='direct',convert='little_endian',recl=4*total_grid_1d)
    write(6,*) 'Output daily file ', trim(file_daily_tmp2m)
    write(lun,rec=1) real(data_tmp2m(1:total_grid_1d),4)
    close(lun)
  end if

  if (l_daily_sph2m) then
    write(file_daily_sph2m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2)') &
         & trim(file_daily_base),'/',iyear,imon, &
         & '/sph2m.',iyear,imon,iday
    open(lun,file=file_daily_sph2m,form='unformatted',action='write',access='direct',convert='little_endian',recl=4*total_grid_1d)
    write(6,*) 'Output daily file ', trim(file_daily_sph2m)
    write(lun,rec=1) real(data_sph2m(1:total_grid_1d),4)
    close(lun)
  end if

  if (l_daily_wind) then
    if (l_neutral_wind) then
      write(file_daily_u10m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2)') &
           & trim(file_daily_base),'/',iyear,imon, &
           & '/un10m.',iyear,imon,iday
    else
      write(file_daily_u10m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2)') &
           & trim(file_daily_base),'/',iyear,imon, &
           & '/u10m.',iyear,imon,iday
    end if
    open(lun,file=file_daily_u10m,form='unformatted',action='write',access='direct',convert='little_endian',recl=4*total_grid_1d)
    write(6,*) 'Output daily file ', trim(file_daily_u10m)
    write(lun,rec=1) real(data_u10m(1:total_grid_1d),4)
    close(lun)

    if (l_neutral_wind) then
      write(file_daily_v10m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2)') &
           & trim(file_daily_base),'/',iyear,imon, &
           & '/vn10m.',iyear,imon,iday
    else
      write(file_daily_v10m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2)') &
           & trim(file_daily_base),'/',iyear,imon, &
           & '/v10m.',iyear,imon,iday
    end if
    open(lun,file=file_daily_v10m,form='unformatted',action='write',access='direct',convert='little_endian',recl=4*total_grid_1d)
    write(6,*) 'Output daily file ', trim(file_daily_v10m)
    write(lun,rec=1) real(data_v10m(1:total_grid_1d),4)
    close(lun)

    if (l_neutral_wind) then
      write(file_daily_swind,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2)') &
           & trim(file_daily_base),'/',iyear,imon, &
           & '/windn10m.',iyear,imon,iday
    else
      write(file_daily_swind,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2)') &
           & trim(file_daily_base),'/',iyear,imon, &
           & '/wind10m.',iyear,imon,iday
    end if
    open(lun,file=file_daily_swind,form='unformatted',action='write',access='direct',convert='little_endian',recl=4*total_grid_1d)
    write(6,*) 'Output daily file ', trim(file_daily_swind)
    write(lun,rec=1) real(data_swind(1:total_grid_1d),4)
    close(lun)
  end if

  if (l_daily_brtmp) then
    write(file_daily_brtmp,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2)') &
         & trim(file_daily_base),'/',iyear,imon, &
         & '/brtmp.',iyear,imon,iday
    open(lun,file=file_daily_brtmp,form='unformatted',action='write',access='direct',convert='little_endian',recl=4*total_grid_1d)
    write(6,*) 'Output daily file ', trim(file_daily_brtmp)
    write(lun,rec=1) real(data_brtmp(1:total_grid_1d),4)
    close(lun)
  end if

  if (l_daily_slprs) then
    write(file_daily_slprs,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2)') &
         & trim(file_daily_base),'/',iyear,imon, &
         & '/slprs.',iyear,imon,iday
    open(lun,file=file_daily_slprs,form='unformatted',action='write',access='direct',convert='little_endian',recl=4*total_grid_1d)
    write(6,*) 'Output daily file ', trim(file_daily_slprs)
    write(lun,rec=1) real(data_slprs(1:total_grid_1d),4)
    close(lun)
  end if

  if (l_daily_ice) then
    write(file_daily_ice,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2)') &
         & trim(file_daily_base),'/',iyear,imon, &
         & '/ice.',iyear,imon,iday
    open(lun,file=file_daily_ice,form='unformatted',action='write',access='direct',convert='little_endian',recl=4*total_grid_1d)
    write(6,*) 'Output daily file ', trim(file_daily_ice)
    write(lun,rec=1) real(data_ice(1:total_grid_1d),4)
    close(lun)
  end if

  !-----------------------------------------------------------------------------------------

  i0 = 0

  do j = 1, jmut

    ibgn = i0 + 1
    iend = i0 + num_xgrid(j)

    if (num_xgrid(j) == imut) then

      tmp2m_latlon(1:imut,jmut-j+1) = data_tmp2m(ibgn:iend)
      sph2m_latlon(1:imut,jmut-j+1) = data_sph2m(ibgn:iend)
      u10m_latlon(1:imut,jmut-j+1) = data_u10m(ibgn:iend)
      v10m_latlon(1:imut,jmut-j+1) = data_v10m(ibgn:iend)
      swind_latlon(1:imut,jmut-j+1) = data_swind(ibgn:iend)
      brtmp_latlon(1:imut,jmut-j+1) = data_brtmp(ibgn:iend)
      slprs_latlon(1:imut,jmut-j+1) = data_slprs(ibgn:iend)
      ice_latlon(1:imut,jmut-j+1) = data_ice(ibgn:iend)

    else

      allocate(tmp2m_org(1:num_xgrid(j)+1))
      allocate(sph2m_org(1:num_xgrid(j)+1))
      allocate(u10m_org(1:num_xgrid(j)+1))
      allocate(v10m_org(1:num_xgrid(j)+1))
      allocate(swind_org(1:num_xgrid(j)+1))
      allocate(brtmp_org(1:num_xgrid(j)+1))
      allocate(slprs_org(1:num_xgrid(j)+1))
      allocate(ice_org(1:num_xgrid(j)+1))

      allocate(lon_org (1:num_xgrid(j)+1))

      tmp2m_org(1:num_xgrid(j)) = data_tmp2m(ibgn:iend)
      tmp2m_org(num_xgrid(j)+1) = data_tmp2m(ibgn)
      sph2m_org(1:num_xgrid(j)) = data_sph2m(ibgn:iend)
      sph2m_org(num_xgrid(j)+1) = data_sph2m(ibgn)
      u10m_org(1:num_xgrid(j)) = data_u10m(ibgn:iend)
      u10m_org(num_xgrid(j)+1) = data_u10m(ibgn)
      v10m_org(1:num_xgrid(j)) = data_v10m(ibgn:iend)
      v10m_org(num_xgrid(j)+1) = data_v10m(ibgn)
      swind_org(1:num_xgrid(j)) = data_swind(ibgn:iend)
      swind_org(num_xgrid(j)+1) = data_swind(ibgn)
      brtmp_org(1:num_xgrid(j)) = data_brtmp(ibgn:iend)
      brtmp_org(num_xgrid(j)+1) = data_brtmp(ibgn)
      slprs_org(1:num_xgrid(j)) = data_slprs(ibgn:iend)
      slprs_org(num_xgrid(j)+1) = data_slprs(ibgn)
      ice_org(1:num_xgrid(j)) = data_ice(ibgn:iend)
      ice_org(num_xgrid(j)+1) = data_ice(ibgn)

      dlon_rg = 360.0 / real(num_xgrid(j),8)
      do ii = 1, num_xgrid(j) + 1
        lon_org(ii) = dlon_rg * real(ii-1,8)
      end do
      do i = 1, imut
        do ii = 1, num_xgrid(j)
          if (abs(lon_org(ii) - lon_new(i)) < 1.0d-4) then
            tmp2m_new(i) = tmp2m_org(ii)
            sph2m_new(i) = sph2m_org(ii)
            u10m_new(i) = u10m_org(ii)
            v10m_new(i) = v10m_org(ii)
            swind_new(i) = swind_org(ii)
            brtmp_new(i) = brtmp_org(ii)
            slprs_new(i) = slprs_org(ii)
            ice_new(i) = ice_org(ii)
            exit
          else if ( (lon_org(ii) < lon_new(i)) .and. (lon_new(i) <= lon_org(ii+1)) ) then
            weight = (lon_new(i) - lon_org(ii)) / (lon_org(ii+1) - lon_org(ii))
            tmp2m_new(i) = (1.0d0 - weight) * tmp2m_org(ii) + weight * tmp2m_org(ii+1)
            sph2m_new(i) = (1.0d0 - weight) * sph2m_org(ii) + weight * sph2m_org(ii+1)
            u10m_new(i) = (1.0d0 - weight) * u10m_org(ii) + weight * u10m_org(ii+1)
            v10m_new(i) = (1.0d0 - weight) * v10m_org(ii) + weight * v10m_org(ii+1)
            swind_new(i) = (1.0d0 - weight) * swind_org(ii) + weight * swind_org(ii+1)
            brtmp_new(i) = (1.0d0 - weight) * brtmp_org(ii) + weight * brtmp_org(ii+1)
            slprs_new(i) = (1.0d0 - weight) * slprs_org(ii) + weight * slprs_org(ii+1)
            ice_new(i) = (1.0d0 - weight) * ice_org(ii) + weight * ice_org(ii+1)
            exit
          end if
        end do
      end do

      tmp2m_latlon(1:imut,jmut-j+1) = tmp2m_new(1:imut)
      sph2m_latlon(1:imut,jmut-j+1) = sph2m_new(1:imut)
      u10m_latlon(1:imut,jmut-j+1) = u10m_new(1:imut)
      v10m_latlon(1:imut,jmut-j+1) = v10m_new(1:imut)
      swind_latlon(1:imut,jmut-j+1) = swind_new(1:imut)
      brtmp_latlon(1:imut,jmut-j+1) = brtmp_new(1:imut)
      slprs_latlon(1:imut,jmut-j+1) = slprs_new(1:imut)
      ice_latlon(1:imut,jmut-j+1) = ice_new(1:imut)

      deallocate(tmp2m_org, sph2m_org)
      deallocate(u10m_org, v10m_org, swind_org)
      deallocate(brtmp_org, slprs_org)
      deallocate(ice_org)
      deallocate(lon_org)

    end if

    i0 = iend

  end do


  if (l_latlon_tmp2m) then
    write(file_latlon_tmp2m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2)') &
         & trim(file_daily_latlon_base),'/',iyear, imon, &
         & '/tmp2m.',iyear,imon,iday
    open(lun,file=file_latlon_tmp2m,form='unformatted',action='write',access='direct',recl=4*imut*jmut)
    write(6,*) 'Output lat-lon file ', trim(file_latlon_tmp2m)
    write(lun,rec=1) real(tmp2m_latlon,4)
    close(lun)
  end if

  if (l_latlon_sph2m) then
    write(file_latlon_sph2m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2)') &
         & trim(file_daily_latlon_base),'/',iyear, imon, &
         & '/sph2m.',iyear,imon,iday
    open(lun,file=file_latlon_sph2m,form='unformatted',action='write',access='direct',recl=4*imut*jmut)
    write(6,*) 'Output lat-lon file ', trim(file_latlon_sph2m)
    write(lun,rec=1) real(sph2m_latlon,4)
    close(lun)
  end if

  if (l_latlon_wind) then
    if (l_neutral_wind) then
      write(file_latlon_u10m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2)') &
           & trim(file_daily_latlon_base),'/',iyear, imon, &
           & '/un10m.',iyear,imon,iday
    else
      write(file_latlon_u10m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2)') &
           & trim(file_daily_latlon_base),'/',iyear, imon, &
           & '/u10m.',iyear,imon,iday
    end if
    open(lun,file=file_latlon_u10m,form='unformatted',action='write',access='direct',recl=4*imut*jmut)
    write(6,*) 'Output lat-lon file ', trim(file_latlon_u10m)
    write(lun,rec=1) real(u10m_latlon,4)
    close(lun)

    if (l_neutral_wind) then
      write(file_latlon_v10m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2)') &
           & trim(file_daily_latlon_base),'/',iyear, imon, &
           & '/vn10m.',iyear,imon,iday
    else
      write(file_latlon_v10m,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2)') &
           & trim(file_daily_latlon_base),'/',iyear, imon, &
           & '/v10m.',iyear,imon,iday
    end if
    open(lun,file=file_latlon_v10m,form='unformatted',action='write',access='direct',recl=4*imut*jmut)
    write(6,*) 'Output lat-lon file ', trim(file_latlon_v10m)
    write(lun,rec=1) real(v10m_latlon,4)
    close(lun)

    if (l_neutral_wind) then
      write(file_latlon_swind,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2)') &
           & trim(file_daily_latlon_base),'/',iyear, imon, &
           & '/windn10m.',iyear,imon,iday
    else
      write(file_latlon_swind,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2)') &
           & trim(file_daily_latlon_base),'/',iyear, imon, &
           & '/wind10m.',iyear,imon,iday
    end if
    open(lun,file=file_latlon_swind,form='unformatted',action='write',access='direct',recl=4*imut*jmut)
    write(6,*) 'Output lat-lon file ', trim(file_latlon_swind)
    write(lun,rec=1) real(swind_latlon,4)
    close(lun)
  end if

  if (l_latlon_brtmp) then
    write(file_latlon_brtmp,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2)') &
         & trim(file_daily_latlon_base),'/',iyear, imon, &
         & '/brtmp.',iyear,imon,iday
    open(lun,file=file_latlon_brtmp,form='unformatted',action='write',access='direct',recl=4*imut*jmut)
    write(6,*) 'Output lat-lon file ', trim(file_latlon_brtmp)
    write(lun,rec=1) real(brtmp_latlon,4)
    close(lun)
  end if

  if (l_latlon_slprs) then
    write(file_latlon_slprs,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2)') &
         & trim(file_daily_latlon_base),'/',iyear, imon, &
         & '/slprs.',iyear,imon,iday
    open(lun,file=file_latlon_slprs,form='unformatted',action='write',access='direct',recl=4*imut*jmut)
    write(6,*) 'Output lat-lon file ', trim(file_latlon_slprs)
    write(lun,rec=1) real(slprs_latlon,4)
    close(lun)
  end if

  if (l_latlon_ice) then
    write(file_latlon_ice,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2)') &
         & trim(file_daily_latlon_base),'/',iyear, imon, &
         & '/ice.',iyear,imon,iday
    open(lun,file=file_latlon_ice,form='unformatted',action='write',access='direct',recl=4*imut*jmut)
    write(6,*) 'Output lat-lon file ', trim(file_latlon_ice)
    write(lun,rec=1) real(ice_latlon,4)
    close(lun)
  end if

end program make_daily_time_series
