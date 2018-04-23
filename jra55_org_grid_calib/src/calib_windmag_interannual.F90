! -*-F90-*-
!------------------ calib_windmag_interannual.F90 ---------------------
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
  real(8),allocatable :: usf(:,:)
  real(8),allocatable :: usc(:,:)
  real(4),allocatable :: dat4f(:,:)

  !-----------------------------------------------

  integer(4) :: mtin1, mtin2, mtin3
  integer(4) :: mtot1, mtot2, mtot3

  !-----------------------------------------------
  ! Surface data

  character(256) :: srf_base_dir, srf_dir
  character(256) :: fin_base_us
  character(256) :: fin_us

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
  character(256) :: fout_base_us
  character(256) :: fout_us

  !------------------

  logical :: l_leap_valid

  integer(4) :: lreclen

  integer(4) :: i, j, k, l, m, n

  !----------------------------------------------------------------------

  integer(4),parameter :: lun = 10

  integer(4) :: ios, istat

  !---------------------------------------------------------------------

  logical :: l_calib_monthly

  character(256) :: file_calib_mag
  character(256) :: file_calib_rot

  real(8),allocatable :: wind_mag(:,:,:)
  real(8),allocatable :: wind_rot(:,:,:)

  integer(4) :: current_month_sec
  integer(4) :: this_month_sec
  integer(4) :: prev_month_sec
  integer(4) :: latest_month_sec
  integer(4) :: from_previous_data
  real(8) :: afm, bfm
  integer(4) :: monb, mona

  real(8) :: hl1, hl2

  !--------------------------------------------------------------------

  namelist /nml_srfdata/ &
       &  srf_base_dir, &
       &  fin_base_us, &
       &  file_first_srf, &
       &  file_intv_type_srf, file_intv_srf, total_rec_srf, data_intv_sec_srf, &
       &  srf_first, undef_srf, l_ymdir_srf, &
       &  imut, jmut, grid_name, &
       &  l_little_srf, &
       &  alt_wind

  namelist /nml_calibwind/ &
       &  calc_start, calc_end, &
       &  l_leap_valid,    &
       &  l_calib_monthly, &
       &  file_calib_mag,  &
       &  file_calib_rot,  &
       &  calib_base_dir,  &
       &  fout_base_us

  !-----------------------------------------------------------------------

  open(lun,file='namelist.calibwindmag',iostat=istat,status='old')

  if ( istat /= 0 ) then
    write(6,*) ' Cannot find file : namelist.calibwindmag'
    stop
  end if

  ios = 0

  rewind(lun)
  file_intv_srf = 0
  read(lun, nml=nml_srfdata, iostat=istat)
  ios = ios + istat

  rewind(lun)
  read(lun, nml=nml_calibwind, iostat=istat)
  ios = ios + istat

  if ( ios /= 0 ) then
    write(6,*) ' Read error : namelist.calibwind', ios
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

  allocate(usf(1:imf,1:jmf))
  allocate(usc(1:imf,1:jmf))
  allocate(dat4f(1:imf,1:jmf))

  allocate(wind_mag(1:imf,1:jmf,1:num_month))

  !--------------------------------------------------------------------

  lreclen = 4 * imf * jmf
  call open_file_direct(mtin1, file_calib_mag, lreclen, &
       & convert_mode='little_endian',action='read')

  do m = 1, num_month
    if (l_calib_monthly) then
      read(mtin1,rec=m) dat4f
    else
      read(mtin1,rec=1) dat4f
    end if
    wind_mag(1:imf,1:jmf,m) = real(dat4f(1:imf,1:jmf),8)
  end do
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

  write(6,*) 
  write(6,*) ' MAIN LOOP START '

  LOOP_SURF_FORCE: do while ( .true. )

    write(6,*) ' This step ', current_date%year, current_date%month, current_date%day, &
         & current_date%hour, current_date%minute, data_intv_sec_srf

    if (data_intv_sec_srf == -1) then
      write(fin_us,'(1a,1a,1a,1a,i4.4,i2.2)') &
           & trim(srf_base_dir),'/',trim(fin_base_us),'.',current_file%year,current_file%month
    else
      write(srf_dir,'(1a,1a,i4.4,i2.2)') trim(srf_base_dir),'/',current_file%year,current_file%month
      write(fin_us,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(srf_dir),'/',trim(fin_base_us),'.', &
           & current_file%year,current_file%month,current_file%day,current_file%hour
    end if

    lreclen = 4 * imf * jmf
    call open_file_direct(mtin1, fin_us, lreclen, &
       & convert_mode='little_endian',action='read')
    read(mtin1,rec=1) dat4f
    usf(1:imf,1:jmf) = real(dat4f(1:imf,1:jmf),8)
    call close_file(mtin1)

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

    if (data_intv_sec_srf == -1) then
      afm = 0.0d0
      bfm = 1.0d0
    else
      afm = 2.0d0 * real(from_previous_data,8) / real(prev_month_sec+latest_month_sec,8)
      bfm = 1.0d0 - afm
    end if

    write(6,*) ' bfm  = ', bfm,  ' afm  = ', afm
    write(6,*) ' monb = ', monb, ' mona = ', mona

    !------------------------------------------------

    do j = 1, jmf
      do i = 1, imf
        hl1 = wind_mag(i,j,monb) * bfm + wind_mag(i,j,mona) * afm
        usc(i,j) = hl1 * usf(i,j)
      end do
    end do

    !---------------------

    if (data_intv_sec_srf == -1) then

      next_date = current_date
      next_file = current_file

      next_date%month = next_date%month + 1
      next_file%month = next_file%month + 1

      if (next_date%month == 13) then
        next_date%month = 1
        next_date%year = next_date%year + 1
        next_file%month = 1
        next_file%year = next_file%year + 1
      end if

    else
      next_date = libmxe_calendar__addsec(current_date, data_intv_sec_srf, l_use_leap)
      next_file = libmxe_calendar__addsec(current_file, data_intv_sec_srf, l_leap_valid)
    end if
    !---------------------
    ! output data

    if (data_intv_sec_srf == -1) then
      write(fout_us,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(calib_base_dir),'/',trim(fout_base_us),'.', &
           & current_file%year,current_file%month
    else
      write(calib_dir,'(1a,1a,i4.4,i2.2)') trim(calib_base_dir),'/',current_file%year,current_file%month
      write(fout_us,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(calib_dir),'/',trim(fout_base_us),'.', &
           & current_file%year,current_file%month,current_file%day,current_file%hour
    end if

    lreclen = 4 * imf * jmf
    call open_file_direct(mtot1, fout_us, lreclen, &
       & convert_mode='little_endian',action='write')
    write(mtot1,rec=1) real(usc(1:imf,1:jmf),4)
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
