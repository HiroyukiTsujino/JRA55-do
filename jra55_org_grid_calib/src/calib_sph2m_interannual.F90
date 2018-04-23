! -*-F90-*-
!---------------- calib_sph2m_interannual.F90 ----------------------
!  Information:
!     Calibrate surface temperature and specific humidity
!-------------------------------------------------------------------
program calibration_specific_humidity

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

  ! [cgs]

  real(8), parameter :: ro = rho
  real(8), parameter :: ems = 1.00d0
  real(8), parameter :: stfblz = 5.67d-5
  real(8), parameter :: tab = 273.15d0

  real(8), parameter :: rhoa = 1.22d0 ! air density [kg/m3]
  real(8), parameter :: q0 = 0.98d0    ! dimensionless factor
  real(8), parameter :: q1 = 640380.d0 ! [kg/m3]
  real(8), parameter :: q2 = -5107.4d0 ! [K]
  real(8), parameter :: rgas = 287.04d0
  real(8), parameter :: r0 = 0.6078d0

  ! minimum value of specific humidity

  real(8), parameter :: sphmin = 2.0d-5 ! minimum of specific humidity

  !------------

  character(len=16) :: item_name

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

  real(8),allocatable :: sphf(:,:)
  real(8),allocatable :: sphc(:,:)

  real(4),allocatable :: dat4f(:,:)

  real(8),allocatable :: msk(:,:)

  !-----------------------------------------------

  integer(4) :: mtin1, mtin2, mtin3
  integer(4) :: mtot1, mtot2, mtot3

  !------------------
  ! Surface data

  character(256) :: srf_base_dir, srf_dir
  character(256) :: fin_base_sph

  character(256) :: fin_sph

  integer(4) :: file_first_srf(6)
  integer(4) :: file_intv_type_srf
  integer(4) :: file_intv_srf
  integer(4) :: total_rec_srf
  integer(4) :: data_intv_sec_srf
  integer(4) :: srf_first(6)
  real(4)    :: undef_srf
  logical    :: l_ymdir_srf
  integer(4) :: num_grid_srf
  logical    :: l_little_srf

  real(8)    :: alt_sph

  !------------------
  ! Mask data

  character(256) :: fin_msk
  logical    :: l_little_msk
  integer(4) :: num_grid_msk

  !------------------
  ! Output

  character(256) :: calib_base_dir, calib_dir
  character(256) :: fout_base_sph

  character(256) :: fout_sph

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

  character(256) :: file_calib_mag_a
  character(256) :: file_calib_mag_b

  real(8),allocatable :: sph_mag_a(:,:,:)
  real(8),allocatable :: sph_mag_b(:,:,:)

  integer(4) :: current_month_sec
  integer(4) :: this_month_sec
  integer(4) :: prev_month_sec
  integer(4) :: latest_month_sec
  integer(4) :: from_previous_data
  real(8) :: afm, bfm
  integer(4) :: monb, mona

  real(8) :: calib_a, calib_b, calib
  real(8) :: act, bct
  integer(4) from_trans_start, to_trans_end, trans_period

  real(8) :: hl1, hl2

  !--------------------------------------------------------------------

  namelist /nml_srfdata/ &
       &  srf_base_dir, &
       &  fin_base_sph, &
       &  file_first_srf, &
       &  file_intv_type_srf, file_intv_srf, total_rec_srf, data_intv_sec_srf, &
       &  srf_first, undef_srf, l_ymdir_srf, &
       &  num_grid_srf, &
       &  l_little_srf, &
       &  alt_sph

  namelist /nml_mskdata/ &
       &  fin_msk, &
       &  l_little_msk, &
       &  num_grid_msk

  namelist /nml_calibsph/ calc_start, calc_end, &
       &  l_leap_valid, &
       &  l_calib_monthly, &
       &  l_calib_transition, &
       &  trans_start, trans_end, &
       &  file_calib_mag_a, &
       &  file_calib_mag_b, &
       &  calib_base_dir, &
       &  fout_base_sph

  !-----------------------------------------------------------------------

  idmon(1:num_month) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

  l_calib_transition = .false.

  open(lun,file='namelist.calibsph',iostat=istat,status='old')

  if ( istat /= 0 ) then
    write(6,*) ' Cannot find file : namelist.calibsph'
    stop
  end if

  ios = 0

  rewind(lun)
  file_intv_srf = 0
  read(lun, nml=nml_srfdata, iostat=istat)
  ios = ios + istat

  rewind(lun)
  read(lun, nml=nml_calibsph, iostat=istat)
  ios = ios + istat

  rewind(lun)
  read(lun, nml=nml_mskdata, iostat=istat)
  ios = ios + istat

  if ( ios /= 0 ) then
    write(6,*) ' Read error : namelist.calibsph', ios
    stop
  end if

  close(lun)

  !----------------------------------------------------------------------

  call force_process__ini(l_leap_valid)

  !------

  imf = num_grid_srf
  jmf = 1
  kmf = 1

  allocate(sphf(1:imf,1:jmf))
  allocate(sphc(1:imf,1:jmf))
  allocate(dat4f(1:imf,1:jmf))
  allocate(msk(1:imf,1:jmf))

  allocate(sph_mag_a(1:imf,1:jmf,1:num_month))
  allocate(sph_mag_b(1:imf,1:jmf,1:num_month))

  !--------------------------------------------------------------------

  lreclen = 4 * imf * jmf
  call open_file_direct(mtin1, file_calib_mag_a, lreclen, &
       & convert_mode='little_endian', action='read')

  do m = 1, num_month
    if (l_calib_monthly) then
      read(mtin1,rec=m) dat4f
    else
      read(mtin1,rec=1) dat4f
    end if
    sph_mag_a(1:imf,1:jmf,m) = real(dat4f(1:imf,1:jmf),8)
  end do
  call close_file(mtin1)

  if (l_calib_transition) then
    write(6,*) ' make transition of calibration factor '
    write(6,*) ' Factor (b) : ', trim(file_calib_mag_b)
    write(6,*) ' Factor (a) : ', trim(file_calib_mag_a)
    lreclen = 4 * imf * jmf
    call open_file_direct(mtin1, file_calib_mag_b, lreclen, &
       & convert_mode='little_endian', action='read')
    do m = 1, num_month
      if (l_calib_monthly) then
        read(mtin1,rec=m) dat4f
      else
        read(mtin1,rec=1) dat4f
      end if
      sph_mag_b(1:imf,1:jmf,m) = real(dat4f(1:imf,1:jmf),8)
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
  ! read mask

  lreclen = 4 * imf * jmf
  call open_file_direct(mtin1, fin_msk, lreclen, convert_mode='little_endian')

  read(mtin1,rec=1) dat4f
  msk(1:imf,1:jmf) = 1.0d0 - real(dat4f(1:imf,1:jmf),8)  ! 1:water, 0:land

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
    write(fin_sph,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(srf_dir),'/',trim(fin_base_sph),'.', &
         & current_file%year,current_file%month,current_file%day,current_file%hour

    lreclen = 4 * imf * jmf
    call open_file_direct(mtin2, fin_sph, lreclen, convert_mode='little_endian')
    read(mtin2,rec=1) dat4f
    sphf(1:imf,1:jmf) = real(dat4f(1:imf,1:jmf),8)
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
        calib_a = sph_mag_a(i,j,monb) * bfm + sph_mag_a(i,j,mona) * afm
        if (l_calib_transition) then
          calib_b = sph_mag_b(i,j,monb) * bfm + sph_mag_b(i,j,mona) * afm
          calib = act * calib_a + bct * calib_b
        else
          calib = calib_a
        end if
        sphc(i,j) = calib * sphf(i,j)
        sphc(i,j) = max(sphc(i,j),sphmin)
      end do
    end do

    !---------------------

    next_date = libmxe_calendar__addsec(current_date, data_intv_sec_srf, l_leap_valid)
    next_file = libmxe_calendar__addsec(current_file, data_intv_sec_srf, l_leap_valid)

    !---------------------
    ! output data

    write(calib_dir,'(1a,1a,i4.4,i2.2)') trim(calib_base_dir),'/',current_file%year,current_file%month
    write(fout_sph,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(calib_dir),'/',trim(fout_base_sph),'.', &
         & current_file%year,current_file%month,current_file%day,current_file%hour

    lreclen = 4 * imf * jmf
    call open_file_direct(mtot1, fout_sph, lreclen, convert_mode='little_endian')
    write(mtot1,rec=1) real(sphc(1:imf,1:jmf),4)
    call close_file(mtot1)

    !--------------------

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

end program calibration_specific_humidity
