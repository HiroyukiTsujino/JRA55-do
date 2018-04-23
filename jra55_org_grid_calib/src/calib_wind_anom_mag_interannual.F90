! -*-F90-*-
!------------------ calib_wind_anom_mag_interannual.F90 ---------------------
!  Information:
!     Calibrate wind field
!-------------------------------------------------------------------
program calibration_surface_wind

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

  ! Surface Atmospheric State

  integer(4) :: imf, jmf, kmf
  real(8),allocatable :: usf(:,:), vsf(:,:)
  real(8),allocatable :: usc(:,:), vsc(:,:)
  real(4),allocatable :: dat4f(:,:)

  !-----------------------------------------------

  integer(4) :: mtin1, mtin2, mtin3
  integer(4) :: mtot1, mtot2, mtot3

  !-----------------------------------------------
  ! Surface data

  character(256) :: srf_base_dir, srf_dir
  character(256) :: fin_base_us, fin_base_vs
  character(256) :: fin_us, fin_vs

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

  integer(4) :: imut, jmut
  integer(4),allocatable :: num_xgrid(:)
  character(256) :: grid_name
  integer(4) :: total_grid_1d

  !------------------
  ! Output

  character(256) :: calib_base_dir, calib_dir
  character(256) :: fout_base_us, fout_base_vs
  character(256) :: fout_us, fout_vs

  !------------------

  logical :: l_leap_valid

  integer(4) :: lreclen

  integer(4) :: i, j, k, l, m, n

  !----------------------------------------------------------------------

  integer(4),parameter :: lun = 10

  integer(4) :: ios, istat

  !---------------------------------------------------------------------

  logical :: l_calib_monthly
  logical :: l_calib_transition

  integer(4) :: trans_start(6)
  integer(4) :: trans_end(6)

  type(type_calendar) :: trans_start_date
  type(type_calendar) :: trans_end_date

  character(256) :: file_calib_mag_a, file_calib_mag_b

  real(8),allocatable :: wind_mag_a(:,:,:), wind_mag_b(:,:,:)

  character(256) :: file_meanu_org_a, file_meanu_org_b
  character(256) :: file_meanv_org_a, file_meanv_org_b
  character(256) :: file_meanu_new_a, file_meanu_new_b
  character(256) :: file_meanv_new_a, file_meanv_new_b

  real(8),allocatable :: wind_meanu_org_a(:,:,:), wind_meanu_org_b(:,:,:)
  real(8),allocatable :: wind_meanv_org_a(:,:,:), wind_meanv_org_b(:,:,:)
  real(8),allocatable :: wind_meanu_new_a(:,:,:), wind_meanu_new_b(:,:,:)
  real(8),allocatable :: wind_meanv_new_a(:,:,:), wind_meanv_new_b(:,:,:)

  integer(4) :: current_month_sec
  integer(4) :: this_month_sec
  integer(4) :: prev_month_sec
  integer(4) :: latest_month_sec
  integer(4) :: from_previous_data
  real(8) :: afm, bfm
  integer(4) :: monb, mona

  real(8) :: mag_a, mag_b, mag_this
  real(8) :: meanu_org_a, meanv_org_a
  real(8) :: meanu_org_b, meanv_org_b
  real(8) :: meanu_new_a, meanv_new_a
  real(8) :: meanu_new_b, meanv_new_b
  real(8) :: meanu_org_this, meanv_org_this
  real(8) :: meanu_new_this, meanv_new_this
  real(8) :: act, bct
  integer(4) from_trans_start, to_trans_end, trans_period

  real(8) :: hl1, hl2, hl3

  !--------------------------------------------------------------------

  namelist /nml_srfdata/ &
       &  srf_base_dir, &
       &  fin_base_us, fin_base_vs,   &
       &  file_first_srf, &
       &  file_intv_type_srf, file_intv_srf, total_rec_srf, data_intv_sec_srf, &
       &  srf_first, undef_srf, l_ymdir_srf, &
       &  imut, jmut, grid_name, &
       &  l_little_srf, &
       &  alt_wind

  namelist /nml_calibwind_anom_mag/ &
       &  calc_start, calc_end, &
       &  l_leap_valid,    &
       &  l_calib_monthly, &
       &  l_calib_transition, &
       &  trans_start, trans_end, &
       &  file_calib_mag_a, &
       &  file_calib_mag_b, &
       &  file_meanu_org_a, &
       &  file_meanu_org_b, &
       &  file_meanv_org_a, &
       &  file_meanv_org_b, &
       &  file_meanu_new_a, &
       &  file_meanu_new_b, &
       &  file_meanv_new_a, &
       &  file_meanv_new_b, &
       &  calib_base_dir,  &
       &  fout_base_us, &
       &  fout_base_vs

  !-----------------------------------------------------------------------

  l_calib_transition = .false.

  file_calib_mag_a = 'no_file'
  file_calib_mag_b = 'no_file'
  file_meanu_org_a = 'no_file'
  file_meanu_org_b = 'no_file'
  file_meanv_org_a = 'no_file'
  file_meanv_org_b = 'no_file'
  file_meanu_new_a = 'no_file'
  file_meanu_new_b = 'no_file'
  file_meanv_new_a = 'no_file'
  file_meanv_new_b = 'no_file'

  open(lun,file='namelist.calibwind_anom_mag',iostat=istat,status='old')

  if ( istat /= 0 ) then
    write(6,*) ' Cannot find file : namelist.calibwind_anom_mag'
    stop
  end if

  ios = 0

  rewind(lun)
  file_intv_srf = 0
  read(lun, nml=nml_srfdata, iostat=istat)
  ios = ios + istat

  rewind(lun)
  read(lun, nml=nml_calibwind_anom_mag, iostat=istat)
  ios = ios + istat

  if ( ios /= 0 ) then
    write(6,*) ' Read error : namelist.calibwind_anom_mag', ios
    stop
  end if

  close(lun)

  call force_process__ini(l_leap_valid)

  !----------------------------------------------------------------------

  allocate(num_xgrid(1:jmut))

  call set_reduced_grid(grid_name,num_xgrid,jmut,total_grid_1d)

  imf = total_grid_1d
  jmf = 1
  kmf = 1
  lreclen = 4 * total_grid_1d

  allocate(usf(1:imf,1:jmf), vsf(1:imf,1:jmf))
  allocate(usc(1:imf,1:jmf), vsc(1:imf,1:jmf))
  allocate(dat4f(1:imf,1:jmf))

  allocate(wind_mag_a(1:imf,1:jmf,1:num_month))
  allocate(wind_mag_b(1:imf,1:jmf,1:num_month))
  allocate(wind_meanu_org_a(1:imf,1:jmf,1:num_month))
  allocate(wind_meanu_org_b(1:imf,1:jmf,1:num_month))
  allocate(wind_meanv_org_a(1:imf,1:jmf,1:num_month))
  allocate(wind_meanv_org_b(1:imf,1:jmf,1:num_month))
  allocate(wind_meanu_new_a(1:imf,1:jmf,1:num_month))
  allocate(wind_meanu_new_b(1:imf,1:jmf,1:num_month))
  allocate(wind_meanv_new_a(1:imf,1:jmf,1:num_month))
  allocate(wind_meanv_new_b(1:imf,1:jmf,1:num_month))

  !--------------------------------------------------------------------
  ! read calibration factor

  lreclen = 4 * imf * jmf
  call open_file_direct(mtin1, file_calib_mag_a, lreclen, &
       & convert_mode='little_endian',action='read')

  do m = 1, num_month
    if (l_calib_monthly) then
      read(mtin1,rec=m) dat4f
    else
      read(mtin1,rec=1) dat4f
    end if
    wind_mag_a(1:imf,1:jmf,m) = real(dat4f(1:imf,1:jmf),8)
  end do
  call close_file(mtin1)

  !---------------------------------------------------------------------

  lreclen = 4 * imf * jmf
  call open_file_direct(mtin1, file_meanu_org_a, lreclen, &
       & convert_mode='little_endian',action='read')
  do m = 1, num_month
    if (l_calib_monthly) then
      read(mtin1,rec=m) dat4f
    else
      read(mtin1,rec=1) dat4f
    end if
    wind_meanu_org_a(1:imf,1:jmf,m) = real(dat4f(1:imf,1:jmf),8)
  end do
  call close_file(mtin1)

  lreclen = 4 * imf * jmf
  call open_file_direct(mtin1, file_meanv_org_a, lreclen, &
       & convert_mode='little_endian',action='read')
  do m = 1, num_month
    if (l_calib_monthly) then
      read(mtin1,rec=m) dat4f
    else
      read(mtin1,rec=1) dat4f
    end if
    wind_meanv_org_a(1:imf,1:jmf,m) = real(dat4f(1:imf,1:jmf),8)
  end do
  call close_file(mtin1)

  !-----

  lreclen = 4 * imf * jmf
  call open_file_direct(mtin1, file_meanu_new_a, lreclen, &
       & convert_mode='little_endian',action='read')
  do m = 1, num_month
    if (l_calib_monthly) then
      read(mtin1,rec=m) dat4f
    else
      read(mtin1,rec=1) dat4f
    end if
    wind_meanu_new_a(1:imf,1:jmf,m) = real(dat4f(1:imf,1:jmf),8)
  end do
  call close_file(mtin1)

  lreclen = 4 * imf * jmf
  call open_file_direct(mtin1, file_meanv_new_a, lreclen, &
       & convert_mode='little_endian',action='read')
  do m = 1, num_month
    if (l_calib_monthly) then
      read(mtin1,rec=m) dat4f
    else
      read(mtin1,rec=1) dat4f
    end if
    wind_meanv_new_a(1:imf,1:jmf,m) = real(dat4f(1:imf,1:jmf),8)
  end do
  call close_file(mtin1)

  !---------------------------------------

  if (l_calib_transition) then

    write(6,*) ' make transition of calibration factor '

    write(6,*) ' Factor (b) : ', trim(file_calib_mag_b)
    write(6,*) ' Factor (a) : ', trim(file_calib_mag_a)
    write(6,*) ' Factor (b) : ', trim(file_meanu_org_b)
    write(6,*) ' Factor (a) : ', trim(file_meanu_org_a)
    write(6,*) ' Factor (b) : ', trim(file_meanv_org_b)
    write(6,*) ' Factor (a) : ', trim(file_meanv_org_a)
    write(6,*) ' Factor (b) : ', trim(file_meanu_new_b)
    write(6,*) ' Factor (a) : ', trim(file_meanu_new_a)
    write(6,*) ' Factor (b) : ', trim(file_meanv_new_b)
    write(6,*) ' Factor (a) : ', trim(file_meanv_new_a)

    lreclen = 4 * imf * jmf
    call open_file_direct(mtin1, file_calib_mag_b, lreclen, &
         & convert_mode='little_endian',action='read')
    do m = 1, num_month
      if (l_calib_monthly) then
        read(mtin1,rec=m) dat4f
      else
        read(mtin1,rec=1) dat4f
      end if
      wind_mag_b(1:imf,1:jmf,m) = real(dat4f(1:imf,1:jmf),8)
    end do
    call close_file(mtin1)

    lreclen = 4 * imf * jmf
    call open_file_direct(mtin1, file_meanu_org_b, lreclen, &
         & convert_mode='little_endian',action='read')
    do m = 1, num_month
      if (l_calib_monthly) then
        read(mtin1,rec=m) dat4f
      else
        read(mtin1,rec=1) dat4f
      end if
      wind_meanu_org_b(1:imf,1:jmf,m) = real(dat4f(1:imf,1:jmf),8)
    end do
    call close_file(mtin1)

    lreclen = 4 * imf * jmf
    call open_file_direct(mtin1, file_meanv_org_b, lreclen, &
         & convert_mode='little_endian',action='read')
    do m = 1, num_month
      if (l_calib_monthly) then
        read(mtin1,rec=m) dat4f
      else
        read(mtin1,rec=1) dat4f
      end if
      wind_meanv_org_b(1:imf,1:jmf,m) = real(dat4f(1:imf,1:jmf),8)
    end do
    call close_file(mtin1)

    lreclen = 4 * imf * jmf
    call open_file_direct(mtin1, file_meanu_new_b, lreclen, &
         & convert_mode='little_endian',action='read')
    do m = 1, num_month
      if (l_calib_monthly) then
        read(mtin1,rec=m) dat4f
      else
        read(mtin1,rec=1) dat4f
      end if
      wind_meanu_new_b(1:imf,1:jmf,m) = real(dat4f(1:imf,1:jmf),8)
    end do
    call close_file(mtin1)

    lreclen = 4 * imf * jmf
    call open_file_direct(mtin1, file_meanv_new_b, lreclen, &
         & convert_mode='little_endian',action='read')
    do m = 1, num_month
      if (l_calib_monthly) then
        read(mtin1,rec=m) dat4f
      else
        read(mtin1,rec=1) dat4f
      end if
      wind_meanv_new_b(1:imf,1:jmf,m) = real(dat4f(1:imf,1:jmf),8)
    end do
    call close_file(mtin1)

    trans_start_date%year   = trans_start(1)
    trans_start_date%month  = trans_start(2)
    trans_start_date%day    = trans_start(3)
    trans_start_date%hour   = trans_start(4)
    trans_start_date%minute = trans_start(5)
    trans_start_date%second = trans_start(6)

    trans_end_date%year   = trans_end(1)
    trans_end_date%month  = trans_end(2)
    trans_end_date%day    = trans_end(3)
    trans_end_date%hour   = trans_end(4)
    trans_end_date%minute = trans_end(5)
    trans_end_date%second = trans_end(6)

    write(6,'(1a,5i8)') ' Transition starts at ', &
         & trans_start_date%year, trans_start_date%month, trans_start_date%day, &
         & trans_start_date%hour, trans_start_date%minute
    write(6,'(1a,5i8)') ' Transition ends at   ', &
         & trans_end_date%year, trans_end_date%month, trans_end_date%day, &
         & trans_end_date%hour, trans_end_date%minute

  end if

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

  if (l_calib_transition) then
    trans_period = libmxe_calendar__diffsec(trans_start_date,trans_end_date,l_leap_valid)
    write(6,*) ' Transition period = ', trans_period, ' = ', trans_period/86400, ' days'
  end if

  write(6,*) 
  write(6,*) ' MAIN LOOP START '

  LOOP_SURF_FORCE: do while ( .true. )

    write(6,*) ' This step ', current_date%year, current_date%month, current_date%day, &
         & current_date%hour, current_date%minute, data_intv_sec_srf

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

    if (l_calib_transition) then

      from_trans_start = libmxe_calendar__diffsec(trans_start_date,current_date,l_leap_valid)
      to_trans_end = libmxe_calendar__diffsec(current_date,trans_end_date,l_leap_valid)

      if ((from_trans_start > 0) .and. (to_trans_end > 0)) then
        if (from_trans_start + to_trans_end /= trans_period) then
          write(6,*) ' calendar errror for transtion '
          stop
        else
          act = real(from_trans_start,8) / real(trans_period,8)
          bct = real(to_trans_end,8) / real(trans_period,8)
        end if
      else
        if (from_trans_start <= 0) then
          act = 0.0d0
          bct = 1.0d0
        end if
        if (to_trans_end <= 0) then
          act = 1.0d0
          bct = 0.0d0
        end if
      end if

      write(6,'(1a,f12.7,1a,f12.7)') &
           & ' transition factor : bct = ', bct, ' act = ', act

    end if

    !------------------------------------------------

    do j = 1, jmf
      do i = 1, imf

        mag_a = wind_mag_a(i,j,monb) * bfm + wind_mag_a(i,j,mona) * afm

        meanu_org_a = wind_meanu_org_a(i,j,monb) * bfm + wind_meanu_org_a(i,j,mona) * afm
        meanv_org_a = wind_meanv_org_a(i,j,monb) * bfm + wind_meanv_org_a(i,j,mona) * afm
        meanu_new_a = wind_meanu_new_a(i,j,monb) * bfm + wind_meanu_new_a(i,j,mona) * afm
        meanv_new_a = wind_meanv_new_a(i,j,monb) * bfm + wind_meanv_new_a(i,j,mona) * afm

        if (l_calib_transition) then
          mag_b = wind_mag_b(i,j,monb) * bfm + wind_mag_b(i,j,mona) * afm
          mag_this = act * mag_a + bct * mag_b
          meanu_org_b = wind_meanu_org_b(i,j,monb) * bfm + wind_meanu_org_b(i,j,mona) * afm
          meanv_org_b = wind_meanv_org_b(i,j,monb) * bfm + wind_meanv_org_b(i,j,mona) * afm
          meanu_new_b = wind_meanu_new_b(i,j,monb) * bfm + wind_meanu_new_b(i,j,mona) * afm
          meanv_new_b = wind_meanv_new_b(i,j,monb) * bfm + wind_meanv_new_b(i,j,mona) * afm
          meanu_org_this = act * meanu_org_a + bct * meanu_org_b
          meanv_org_this = act * meanv_org_a + bct * meanv_org_b
          meanu_new_this = act * meanu_new_a + bct * meanu_new_b
          meanv_new_this = act * meanv_new_a + bct * meanv_new_b
        else
          mag_this = mag_a
          meanu_org_this = meanu_org_a
          meanv_org_this = meanv_org_a
          meanu_new_this = meanu_new_a
          meanv_new_this = meanv_new_a
        end if

        usc(i,j) = meanu_new_this + mag_this * (usf(i,j) - meanu_org_this)
        vsc(i,j) = meanv_new_this + mag_this * (vsf(i,j) - meanv_org_this)

      end do
    end do

    !---------------------

    next_date = libmxe_calendar__addsec(current_date, data_intv_sec_srf, l_use_leap)
    next_file = libmxe_calendar__addsec(current_file, data_intv_sec_srf, l_leap_valid)

    !---------------------
    ! output data

    write(calib_dir,'(1a,1a,i4.4,i2.2)') trim(calib_base_dir),'/',current_file%year,current_file%month
    write(fout_us,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(calib_dir),'/',trim(fout_base_us),'.', &
         & current_file%year,current_file%month,current_file%day,current_file%hour
    write(fout_vs,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(calib_dir),'/',trim(fout_base_vs),'.', &
         & current_file%year,current_file%month,current_file%day,current_file%hour

    lreclen = 4 * imf * jmf
    call open_file_direct(mtot1, fout_us, lreclen, &
       & convert_mode='little_endian',action='write')
!!!    write(6,*) ' written to ', trim(fout_us)
    write(mtot1,rec=1) real(usc(1:imf,1:jmf),4)
    call close_file(mtot1)

    lreclen = 4 * imf * jmf
    call open_file_direct(mtot1, fout_vs, lreclen, &
       & convert_mode='little_endian',action='write')
!!!    write(6,*) ' written to ', trim(fout_vs)
    write(mtot1,rec=1) real(vsc(1:imf,1:jmf),4)
    call close_file(mtot1)

    !---------------------

    if ( (next_date%year == end_date%year) &
         .and. (next_date%month == end_date%month) &
         .and. (next_date%day == end_date%day) &
         .and. (next_date%hour == end_date%hour) &
         .and. (next_date%minute == end_date%minute) &
         .and. (next_date%second == end_date%second) ) then
      exit LOOP_SURF_FORCE
    end if

    current_date = next_date
    current_file = next_file

  end do LOOP_SURF_FORCE

  write(6,*) ' MAIN LOOP END '

end program calibration_surface_wind
