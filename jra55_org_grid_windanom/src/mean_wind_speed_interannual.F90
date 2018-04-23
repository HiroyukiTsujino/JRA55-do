! -*-F90-*-
!------------------ mean_wind_speed_interannual.F90 ----------------
!  Information:
!    Compute multiplicative factor to be applied to wind anomaly.
!-------------------------------------------------------------------
program mean_wind_speed_search_factor

  use libmxe_para, only: libmxe_para__register &
                     & , clen, pi, radian, radian_r, radius, rho &
                     & , type_libmxe_para
  use libmxe_grid, only: libmxe_grid__register  &
                     & , type_libmxe_grid
  use libmxe_topo, only: libmxe_topo__register, libmxe_topo__aexl &
                     & , type_libmxe_topo
  use libmxe_io, only: libmxe_io__register, type_libmxe_io
  use libmxe_calendar

  use file_open_close_manager
  use force_process

  implicit none

  !------------

  type(type_calendar) :: start_date
  type(type_calendar) :: end_date

  type(type_calendar) :: current_date
  type(type_calendar) :: current_file
  type(type_calendar) :: next_date
  type(type_calendar) :: next_file

  type(type_calendar) :: current_month_start
  type(type_calendar) :: prev_month_start
  type(type_calendar) :: latest_month_start

  integer(4) :: calc_start(6)
  integer(4) :: calc_end(6)

  logical :: l_exit_loop

  ! Surface Atmospheric State

  integer(4) :: imf, jmf, kmf
  real(8),allocatable :: usf(:,:), vsf(:,:)
  real(8),allocatable :: usc(:,:), vsc(:,:)
  real(4),allocatable :: dat4f(:,:)

  real(8),allocatable :: wscm(:,:,:)
  real(8),allocatable :: factor(:,:)

  integer(4) :: num_data
  real(8) :: r_num_data, weight_wind

  real(8),allocatable :: mask_org(:,:)

  integer(4),parameter :: nfdiv = 23
  real(8) :: alpha(nfdiv)

  !-----------------------------------------------

  integer(4) :: mtin1, mtin2, mtin3
  integer(4) :: mtot1, mtot2, mtot3

  !-----------------------------------------------
  ! Surface data

  character(256) :: srf_base_dir, srf_dir
  character(256) :: fin_base_us, fin_base_vs
  character(256) :: fin_us, fin_vs
  character(256) :: file_mask

  integer(4) :: file_first_srf(6)
  integer(4) :: file_intv_type_srf
  integer(4) :: file_intv_srf
  integer(4) :: total_rec_srf
  integer(4) :: data_intv_sec_srf
  integer(4) :: srf_first(6)
  real(4)    :: undef_srf
  logical    :: l_ymdir_srf
  logical    :: l_little_srf

  real(8)    :: alt_wind

  !------------------

  logical :: l_leap_valid

  integer(4) :: lreclen

  integer(4) :: i, j, k, l, m, n

  !----------------------------------------------------------------------

  integer(4),parameter :: lun = 10

  integer(4) :: ios, istat

  !---------------------------------------------------------------------

  logical :: l_mean_monthly

  character(256) :: file_meanu_org, file_meanv_org
  character(256) :: file_meanu_ref, file_meanv_ref

  real(8),allocatable :: meanu_org(:,:,:), meanv_org(:,:,:)
  real(8),allocatable :: meanu_ref(:,:,:), meanv_ref(:,:,:)
  real(8),allocatable :: means_ref(:,:)

  real(8) :: meanu_ref_this, meanv_ref_this
  real(8) :: meanu_org_this, meanv_org_this

  character(256) :: file_meanw_ref
  real(8),allocatable :: meanw_ref(:,:)

  integer(4) :: current_month_sec
  integer(4) :: this_month_sec
  integer(4) :: prev_month_sec
  integer(4) :: latest_month_sec
  integer(4) :: from_previous_data
  real(8) :: afm, bfm
  integer(4) :: monb, mona

  real(8) :: mag_a, mag_b, mag_this
  real(8) :: rot_a, rot_b, rot_this

  real(8) :: wind_speed, wind_speed_adj
  real(8),parameter :: wind_speed_cut = 0.3d0 ! 0.3 [m/s] floor for wind speed
  real(8),parameter :: wind_speed_min = 0.3d0 ! 0.3 [m/s] floor for wind speed
  
  real(8) :: hl1, hl2, hl3

  character(256) :: fout_factor_reduce
  character(256) :: fout_meanw_reduce
  character(256) :: fout_meanw_latlon

  character(len=258) :: file_ydef

  integer(4) :: lreclen_latlon
  integer(4) :: imut, jmut
  integer(4),allocatable :: num_xgrid(:)
  character(256) :: grid_name
  integer(4) :: total_grid_1d
  integer(4) :: ibgn, iend, i0, ii

  real(8) :: weight

  real(8),allocatable :: data_w10m(:)
  real(8),allocatable :: lon_org(:)
  real(8),allocatable :: lon_new(:)
  real(8) :: dlon, dlon_rg
  real(8),allocatable :: lat_org(:)

  real(8),allocatable :: w10m_latlon(:,:)
  real(8),allocatable :: w10m_org(:), w10m_new(:)

  !--------------------------------------------------------------------

  namelist /nml_srfdata/ &
       &  srf_base_dir, &
       &  fin_base_us, fin_base_vs,   &
       &  file_first_srf, &
       &  file_intv_type_srf, file_intv_srf, total_rec_srf, data_intv_sec_srf, &
       &  srf_first, undef_srf, l_ymdir_srf, &
       &  imut, jmut, dlon, grid_name, &
       &  file_ydef, &
       &  l_little_srf, &
       &  alt_wind

  namelist /nml_mean_wind_speed/ &
       &  calc_start, calc_end,  &
       &  l_leap_valid,   &
       &  l_mean_monthly, &
       &  file_meanu_ref, &
       &  file_meanu_org, &
       &  file_meanv_ref, &
       &  file_meanv_org, &
       &  file_meanw_ref, &
       &  fout_meanw_reduce,  &
       &  fout_meanw_latlon,  &
       &  fout_factor_reduce, &
       &  file_mask

  !-----------------------------------------------------------------------

  file_meanu_ref = 'no_file'
  file_meanu_org = 'no_file'
  file_meanv_ref = 'no_file'
  file_meanv_org = 'no_file'
  file_meanw_ref = 'no_file'

  open(lun,file='namelist.mean_wind_speed',iostat=istat,status='old')

  if ( istat /= 0 ) then
    write(6,*) ' Cannot find file : namelist.mean_wind_speed'
    stop
  end if

  ios = 0

  rewind(lun)
  file_intv_srf = 0
  read(lun, nml=nml_srfdata, iostat=istat)
  ios = ios + istat

  rewind(lun)
  read(lun, nml=nml_mean_wind_speed, iostat=istat)
  ios = ios + istat

  if ( ios /= 0 ) then
    write(6,*) ' Read error : namelist.mean_wind_speed', ios
    stop
  end if

  close(lun)

  do n = 1, (nfdiv-1)/2
    alpha(n) = 0.45d0 + 0.05d0 * (n-1)
  end do

  alpha((nfdiv-1)/2+1) = 1.0d0

  do n = (nfdiv-1)/2 + 2, nfdiv
    !alpha(n) = alpha((nfdiv-1)/2+1) + 0.1d0 * (n - (nfdiv-1)/2 - 1)
    alpha(n) = alpha((nfdiv-1)/2+1) + 0.05d0 * (n - (nfdiv-1)/2 - 1)
  end do

  write(6,*) ' weighting factor applied to anomaly '
  do n = 1, nfdiv
    write(6,*) n ,' = ', alpha(n)
  end do

  !----------------------------------------------------------------------

  call force_process__ini(l_leap_valid)

  !----------------------------------------------------------------------
  ! set reduced JRA-55 grid

  allocate(num_xgrid(1:jmut))

  call set_reduced_grid(grid_name,num_xgrid,jmut,total_grid_1d)

  imf = total_grid_1d
  jmf = 1
  kmf = 1
  lreclen = 4 * total_grid_1d

  write(6,*) ' imf = ', imf
  write(6,*) ' jmf = ', jmf
  write(6,*) ' kmf = ', kmf

  allocate(data_w10m(1:total_grid_1d))

  !---------------------------------------------
  ! set regular JRA-55 grid

  allocate(lat_org(1:jmut))
  allocate(w10m_latlon(1:imut,1:jmut))
  allocate(w10m_new(1:imut))
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

  !---------------------------------------------------

  allocate(usf(1:imf,1:jmf), vsf(1:imf,1:jmf))
  allocate(usc(1:imf,1:jmf), vsc(1:imf,1:jmf))
  allocate(dat4f(1:imf,1:jmf))
  allocate(mask_org(1:imf,1:jmf))

  allocate(meanu_ref(1:imf,1:jmf,1:num_month))
  allocate(meanu_org(1:imf,1:jmf,1:num_month))
  allocate(meanv_ref(1:imf,1:jmf,1:num_month))
  allocate(meanv_org(1:imf,1:jmf,1:num_month))

  allocate(means_ref(1:imf,1:jmf))
  allocate(meanw_ref(1:imf,1:jmf))

  allocate(wscm(1:imf,1:jmf,1:nfdiv+1))

  allocate(factor(1:imf,1:jmf))

  !--------------------------------------------------------------------
  ! read mask

  lreclen = 4 * imf * jmf
  call open_file_direct(mtin1, file_mask, lreclen, &
       & convert_mode='little_endian',action='read')

  read(mtin1,rec=1) dat4f
  mask_org(1:imf,1:jmf) = real(dat4f(1:imf,1:jmf),8)
  call close_file(mtin1)

  do j = 1, jmf
    do i = 1, imf
      mask_org(i,j) = 1.d0 - mask_org(i,j) ! 0 for land, 1 for water
    end do
  end do

  !--------------------------------------------------------------------
  ! read mean wind field

  write(6,*) ' Mean wind field '

  write(6,*) ' Reference(U) : ', trim(file_meanu_ref)
  write(6,*) ' Reference(V) : ', trim(file_meanv_ref)
  write(6,*) ' Original (U) : ', trim(file_meanu_org)
  write(6,*) ' Original (V) : ', trim(file_meanv_org)
  write(6,*) ' Reference(W) : ', trim(file_meanw_ref)

  ! Reference U

  lreclen = 4 * imf * jmf
  call open_file_direct(mtin1, file_meanu_ref, lreclen, &
       & convert_mode='little_endian',action='read')

  do m = 1, num_month
    if (l_mean_monthly) then
      read(mtin1,rec=m) dat4f
    else
      read(mtin1,rec=1) dat4f
    end if
    meanu_ref(1:imf,1:jmf,m) = real(dat4f(1:imf,1:jmf),8)
  end do
  call close_file(mtin1)

  ! Reference V

  lreclen = 4 * imf * jmf
  call open_file_direct(mtin1, file_meanv_ref, lreclen, &
       & convert_mode='little_endian',action='read')

  do m = 1, num_month
    if (l_mean_monthly) then
      read(mtin1,rec=m) dat4f
    else
      read(mtin1,rec=1) dat4f
    end if
    meanv_ref(1:imf,1:jmf,m) = real(dat4f(1:imf,1:jmf),8)
  end do
  call close_file(mtin1)

  means_ref(:,:) = 0.0d0
  if (.not. l_mean_monthly) then
    do j = 1, jmf
      do i = 1, imf
        if (mask_org(i,j) == 1.0d0) then
          means_ref(i,j) = sqrt(meanu_ref(i,j,1)**2 + meanv_ref(i,j,1)**2)
        end if
      end do
    end do
  end if

  ! Original U

  lreclen = 4 * imf * jmf
  call open_file_direct(mtin1, file_meanu_org, lreclen, &
         & convert_mode='little_endian',action='read')

  do m = 1, num_month
    if (l_mean_monthly) then
      read(mtin1,rec=m) dat4f
    else
      read(mtin1,rec=1) dat4f
    end if
    meanu_org(1:imf,1:jmf,m) = real(dat4f(1:imf,1:jmf),8)
  end do
  call close_file(mtin1)

  ! Original V

  lreclen = 4 * imf * jmf
  call open_file_direct(mtin1, file_meanv_org, lreclen, &
       & convert_mode='little_endian',action='read')
  
  do m = 1, num_month
    if (l_mean_monthly) then
      read(mtin1,rec=m) dat4f
    else
      read(mtin1,rec=1) dat4f
    end if
    meanv_org(1:imf,1:jmf,m) = real(dat4f(1:imf,1:jmf),8)
  end do
  call close_file(mtin1)

  !--------------------------------------------------------------------
  ! Reference W

  lreclen = 4 * imf * jmf
  call open_file_direct(mtin1, file_meanw_ref, lreclen, &
       & convert_mode='little_endian',action='read')
  
  read(mtin1,rec=1) dat4f
  meanw_ref(1:imf,1:jmf) = real(dat4f(1:imf,1:jmf),8)
  call close_file(mtin1)

  !--------------------------------------------------------------------

  start_date%year   = calc_start(1)
  start_date%month  = calc_start(2)
  start_date%day    = calc_start(3)
  start_date%hour   = calc_start(4)
  start_date%minute = calc_start(5)
  start_date%second = calc_start(6)

  end_date%year   = calc_end(1)
  end_date%month  = calc_end(2)
  end_date%day    = calc_end(3)
  end_date%hour   = calc_end(4)
  end_date%minute = calc_end(5)
  end_date%second = calc_end(6)

  current_date = start_date

  current_file%year   = file_first_srf(1)
  current_file%month  = file_first_srf(2)
  current_file%day    = file_first_srf(3)
  current_file%hour   = file_first_srf(4)
  current_file%minute = file_first_srf(5)
  current_file%second = file_first_srf(6)

  wscm(1:imf,1:jmf,1:nfdiv+1) = 0.d0
  num_data = 0
  r_num_data = 0.0d0

  write(6,*) 
  write(6,*) ' MAIN LOOP START '

  LOOP_SURF_FORCE: do while ( .true. )

    write(6,*) ' This step ', current_date%year, current_date%month, current_date%day, &
         & current_date%hour, current_date%minute, data_intv_sec_srf

    num_data = num_data + 1

    write(srf_dir,'(1a,1a,i4.4,i2.2)') trim(srf_base_dir),'/',current_file%year,current_file%month
    write(fin_us,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(srf_dir),'/',trim(fin_base_us),'.', &
         & current_file%year,current_file%month,current_file%day,current_file%hour
    write(fin_vs,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(srf_dir),'/',trim(fin_base_vs),'.', &
         & current_file%year,current_file%month,current_file%day,current_file%hour

    lreclen = 4 * imf * jmf
    call open_file_direct(mtin1, fin_us, lreclen, &
       & convert_mode='little_endian',action='read')
!!!    write(6,*) ' reading from ', trim(fin_us)
    read(mtin1,rec=1) dat4f
    usf(1:imf,1:jmf) = real(dat4f(1:imf,1:jmf),8)
    call close_file(mtin1)

    lreclen = 4 * imf * jmf
    call open_file_direct(mtin2, fin_vs, lreclen, &
       & convert_mode='little_endian',action='read')
!!!    write(6,*) ' reading from ', trim(fin_vs)
    read(mtin2,rec=1) dat4f
    vsf(1:imf,1:jmf) = real(dat4f(1:imf,1:jmf),8)
    call close_file(mtin2)

    !---------------------

    current_month_start%year   = current_date%year
    current_month_start%month  = current_date%month
    current_month_start%day    = 1
    current_month_start%hour   = 0
    current_month_start%minute = 0
    current_month_start%second = 0

    this_month_sec = days_of_month(current_date) * 86400
    current_month_sec = libmxe_calendar__diffsec(current_month_start,current_date,l_leap_valid)

    hl1 = real(current_month_sec,8) / real(this_month_sec,8)

    if (hl1 < 0.5d0) then
      latest_month_start%year = current_date%year
      latest_month_start%month = current_date%month
      if (current_date%month == 1) then
        prev_month_start%year = current_date%year - 1
        prev_month_start%month = 12
      else
        prev_month_start%year = current_date%year
        prev_month_start%month = current_date%month - 1
      end if
    else
      prev_month_start%year = current_date%year
      prev_month_start%month = current_date%month
      if (current_date%month == 12) then
        latest_month_start%year = current_date%year + 1
        latest_month_start%month = 1
      else
        latest_month_start%year = current_date%year
        latest_month_start%month = current_date%month + 1
      end if
    end if

    prev_month_start%day    = 1
    prev_month_start%hour   = 0
    prev_month_start%minute = 0
    prev_month_start%second = 0

    latest_month_start%day    = 1
    latest_month_start%hour   = 0
    latest_month_start%minute = 0
    latest_month_start%second = 0

    monb = prev_month_start%month
    mona = latest_month_start%month

    prev_month_sec = days_of_month(prev_month_start) * 86400
    latest_month_sec = days_of_month(latest_month_start) * 86400
    from_previous_data = libmxe_calendar__diffsec(prev_month_start,current_date,l_leap_valid) - prev_month_sec / 2
    afm = 2.0d0 * real(from_previous_data,8) / real(prev_month_sec+latest_month_sec,8)
    bfm = 1.0d0 - afm
    write(6,*) ' bfm  = ', bfm,  ' afm  = ', afm
    write(6,*) ' monb = ', monb, ' mona = ', mona

    !------------------------------------------------

    next_date = libmxe_calendar__addsec(current_date, data_intv_sec_srf, l_use_leap)
    next_file = libmxe_calendar__addsec(current_file, data_intv_sec_srf, l_leap_valid)

    !---------------------

    if ( (current_date%year == end_date%year) &
         .and. (current_date%month == end_date%month) &
         .and. (current_date%day == end_date%day) &
         .and. (current_date%hour == end_date%hour) &
         .and. (current_date%minute == end_date%minute) &
         .and. (current_date%second == end_date%second) ) then
      l_exit_loop = .true.
      weight_wind = 0.5d0
    else
      l_exit_loop = .false.
      if (num_data == 1) then
        weight_wind = 0.5d0
      else
        weight_wind = 1.0d0
      end if
    end if

    r_num_data = r_num_data + weight_wind

    do j = 1, jmf
      do i = 1, imf

        if (mask_org(i,j) == 1.0d0) then
          meanu_ref_this = meanu_ref(i,j,monb) * bfm + meanu_ref(i,j,mona) * afm
          meanv_ref_this = meanv_ref(i,j,monb) * bfm + meanv_ref(i,j,mona) * afm
          meanu_org_this = meanu_org(i,j,monb) * bfm + meanu_org(i,j,mona) * afm
          meanv_org_this = meanv_org(i,j,monb) * bfm + meanv_org(i,j,mona) * afm
        end if

        do k = 1, nfdiv
          if (mask_org(i,j) == 1.0d0) then
            usc(i,j) = meanu_ref_this + alpha(k) * (usf(i,j) - meanu_org_this)
            vsc(i,j) = meanv_ref_this + alpha(k) * (vsf(i,j) - meanv_org_this)
          else
            usc(i,j) = usf(i,j)
            vsc(i,j) = vsf(i,j)
          end if
          wscm(i,j,k) = wscm(i,j,k) + sqrt(usc(i,j)**2 + vsc(i,j)**2) * weight_wind
        end do

      end do
    end do

    !---------------------

    if (l_exit_loop) then
      exit LOOP_SURF_FORCE
    end if

    current_date = next_date
    current_file = next_file

  end do LOOP_SURF_FORCE

  write(6,*) ' MAIN LOOP END '

  !--------------------------------------------------------------

  num_data = num_data - 1
  write(6,*) 'r_num_data = ', r_num_data, 'num_data = ',num_data

  if (int(r_num_data + 1.0d-4) /= num_data) then
    write(6,*) ' Error : r_num_data does not equal (num_data - 1)'
    stop
  end if

  do k = 1, nfdiv
    do j = 1, jmf
      do i = 1, imf
        if (mask_org(i,j) == 1.0d0) then
          wscm(i,j,k) = wscm(i,j,k) / real(num_data,8) - meanw_ref(i,j)
        else
          wscm(i,j,k) = 0.0d0
        end if
      end do
    end do
  end do

  !------------------------------------------------------------------------

  do j = 1, jmf
    do i = 1, imf

      if (mask_org(i,j) == 1.0d0) then
        if (means_ref(i,j) / meanw_ref(i,j) >= 1.0d0) then
          factor(i,j) = 1.0d0
        else
          factor(i,j) = 10.0d0
          do k = 1, nfdiv - 1
            if (wscm(i,j,k) * wscm(i,j,k+1) < 0.0d0) then
              hl1 = alpha(k) &
                   & - wscm(i,j,k) * (alpha(k+1) - alpha(k)) / (wscm(i,j,k+1) - wscm(i,j,k))
              if (abs(hl1-1.0d0) < abs(factor(i,j)-1.0d0)) then
                factor(i,j) = hl1
              end if
            end if
          end do
          if (factor(i,j) == 10.0d0) then
            hl1 = abs(wscm(i,j,1))
            factor(i,j) = alpha(1)
            do k = 2, nfdiv
              if ( abs(wscm(i,j,k)) < hl1 ) then
                hl1 = abs(wscm(i,j,k))
                factor(i,j) = alpha(k)
              end if
            end do
          end if
        end if

        factor(i,j) = max(min(factor(i,j),1.5d0),0.5d0)

      else

        factor(i,j) = 1.0d0

      end if

    end do
  end do
    
  wscm(1:imf,1:jmf,nfdiv+1) = factor(1:imf,1:jmf)
    
  !------------------------------------------------------------------------

  lreclen = 4 * imf * jmf
  call open_file_direct(mtot1, fout_meanw_reduce, lreclen, &
       & convert_mode='little_endian',action='write')
  write(6,*) ' written to ', trim(fout_meanw_reduce)
  do k = 1, nfdiv
    write(mtot1,rec=k) real(wscm(1:imf,1:jmf,k),4)
  end do
  call close_file(mtot1)

  lreclen = 4 * imf * jmf
  call open_file_direct(mtot1, fout_factor_reduce, lreclen, &
       & convert_mode='little_endian',action='write')
  write(6,*) ' written to ', trim(fout_factor_reduce)
  write(mtot1,rec=1) real(factor(1:imf,1:jmf),4)
  call close_file(mtot1)

  !------------------------------------------------------------------------
  ! reduced grid to lat-lon grid for check

  lreclen_latlon = 4 * imut * jmut
  call open_file_direct(mtot1, fout_meanw_latlon, lreclen_latlon, &
       & convert_mode='big_endian',action='write')
  write(6,*) ' written to ', trim(fout_meanw_latlon)

  do k = 1, nfdiv + 1

    data_w10m(1:total_grid_1d) = wscm(1:total_grid_1d,1,k)

    i0 = 0

    do j = 1, jmut

      ibgn = i0 + 1
      iend = i0 + num_xgrid(j)

      if (num_xgrid(j) == imut) then

        w10m_latlon(1:imut,jmut-j+1) = data_w10m(ibgn:iend)

      else

        allocate(w10m_org(1:num_xgrid(j)+1))
        allocate(lon_org (1:num_xgrid(j)+1))

        w10m_org(1:num_xgrid(j)) = data_w10m(ibgn:iend)
        w10m_org(num_xgrid(j)+1) = data_w10m(ibgn)

        dlon_rg = 360.0 / real(num_xgrid(j),8)
        do ii = 1, num_xgrid(j) + 1
          lon_org(ii) = dlon_rg * real(ii-1,8)
        end do
        do i = 1, imut
          do ii = 1, num_xgrid(j)
            if (abs(lon_org(ii) - lon_new(i)) < 1.0d-4) then
              w10m_new(i) = w10m_org(ii)
              exit
            else if ( (lon_org(ii) < lon_new(i)) .and. (lon_new(i) <= lon_org(ii+1)) ) then
              weight = (lon_new(i) - lon_org(ii)) / (lon_org(ii+1) - lon_org(ii))
              w10m_new(i) = (1.0d0 - weight) * w10m_org(ii) + weight * w10m_org(ii+1)
              exit
            end if
          end do
        end do
        w10m_latlon(1:imut,jmut-j+1) = w10m_new(1:imut)
        deallocate(w10m_org)
        deallocate(lon_org)
        
      end if

      i0 = iend

    end do

    write(mtot1,rec=k) real(w10m_latlon,4)

  end do

  call close_file(mtot1)

end program mean_wind_speed_search_factor
