! -*-F90-*-
!---------------- calib_tmp2m_ocn_interannual.F90 ------------------
!  Information:
!     Calibrate surface temperature and specific humidity
!-------------------------------------------------------------------
program calibration_surface_temperature

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

  real(8), parameter :: ro = rho
  real(8), parameter :: ems = 1.00d0
  real(8), parameter :: stfblz = 5.67d-5
  real(8), parameter :: tab = 273.15d0
  real(8), parameter :: sst_freeze = -1.8d0
  real(8), parameter :: sst_bad = 40.0d0

  real(8), parameter :: molw = 18.016d0
  real(8), parameter :: mola = 28.966d0
  real(8), parameter :: epsilon = molw/mola

  ! CORE (LY04)

  !real(8), parameter :: rhoa = 1.22d0 ! air density [kg/m3]
  !real(8), parameter :: q0 = 0.98d0    ! dimensionless factor
  !real(8), parameter :: q1a = 640380.d0 ! [kg/m3]
  !real(8), parameter :: q2a = -5107.4d0 ! [K]
  !real(8), parameter :: q1i = 11637800.d0 ! L-Y p16
  !real(8), parameter :: q2i = -5897.8d0
  !real(8), parameter :: rgas = 287.04d0
  !real(8), parameter :: r0 = 0.6078d0

  ! Gill (1982)

  real(8), parameter :: g1 = 0.7859d0
  real(8), parameter :: g2 = 0.03477d0
  real(8), parameter :: g3 = 0.00412d0

  real(8), parameter :: h1 = 4.5d0
  real(8), parameter :: h2 = 0.0006d0

  real(8), parameter :: i1 = 0.00422d0

  ! minimum value of specific humidity

  real(8), parameter :: sphmin = 2.0d-5 ! minimum of specific humidity

  !------------

  character(len=16) :: item_name

  type(type_force) :: sst_file
  type(type_force) :: ice_file

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
  real(8),allocatable :: alonf(:), alatf(:)

  real(8),allocatable :: satf(:,:), sphf(:,:)
  real(8),allocatable :: slpf(:,:)
  real(8),allocatable :: roaf(:,:), rlhf(:,:)

  real(8),allocatable :: satc(:,:), sphc(:,:)

  real(8),allocatable :: sstf(:,:), icef(:,:)
  real(4),allocatable :: dat4f(:,:)

  ! SST

  integer(4) :: imt, jmt, kmt
  real(8),allocatable :: sst(:,:)
  real(8),allocatable :: ice(:,:)
  real(8),allocatable :: msk(:,:)

  !-----------------------------------------------

  integer(4) :: mtin1, mtin2, mtin3
  integer(4) :: mtot1, mtot2, mtot3

  !------------------
  ! SST data

  character(256) :: sst_base_dir, sst_dir
  character(256) :: fin_base_sst, fin_base_ice
  character(256) :: fin_sst, fin_ice
  integer(4) :: file_first_sst(6)
  integer(4) :: file_intv_type_sst
  integer(4) :: file_intv_sst
  integer(4) :: total_rec_sst
  integer(4) :: data_intv_sec_sst
  integer(4) :: sst_first(6)
  real(4)    :: undef_sst
  logical    :: l_ymdir_sst
  integer(4) :: num_grid_sst
  logical    :: l_little_sst

  !------------------
  ! Mask data

  character(256) :: fin_msk
  logical    :: l_little_msk
  integer(4) :: num_grid_msk

  !------------------
  ! Surface data

  character(256) :: srf_base_dir, srf_dir
  character(256) :: fin_base_sat, fin_base_sph
  character(256) :: fin_base_slp

  character(256) :: fin_sat, fin_sph
  character(256) :: fin_slp

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

  real(8)    :: alt_sat
  real(8)    :: alt_sph

  !------------------
  ! Output

  character(256) :: calib_base_dir, calib_dir
  character(256) :: fout_base_sat
  character(256) :: fout_base_sph

  character(256) :: fout_sat
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

  character(256) :: file_calib_ocn

  real(8),allocatable :: sat_ocn(:,:,:)
  real(4)    :: undef_calib

  integer(4) :: current_month_sec
  integer(4) :: this_month_sec
  integer(4) :: prev_month_sec
  integer(4) :: latest_month_sec
  integer(4) :: from_previous_data
  real(8) :: afm, bfm
  integer(4) :: monb, mona

  real(8) :: hl1, hl2, hl3
  real(8) :: rhoqsat, qsat, esat

  logical :: l_shift_sph

  !--------------------------------------------------------------------

  namelist /nml_sstdata/ &
       &  sst_base_dir, &
       &  fin_base_sst, fin_base_ice, &
       &  file_first_sst, &
       &  file_intv_type_sst, file_intv_sst, total_rec_sst, data_intv_sec_sst, &
       &  sst_first, undef_sst, &
       &  num_grid_sst, &
       &  l_ymdir_sst, &
       &  l_little_sst

  namelist /nml_mskdata/ &
       &  fin_msk, &
       &  l_little_msk, &
       &  num_grid_msk

  namelist /nml_srfdata/ &
       &  srf_base_dir, &
       &  fin_base_sat, fin_base_sph, &
       &  file_first_srf, &
       &  fin_base_slp,               &
       &  file_intv_type_srf, file_intv_srf, total_rec_srf, data_intv_sec_srf, &
       &  srf_first, undef_srf,       &
       &  num_grid_srf, &
       &  l_ymdir_srf, &
       &  l_little_srf, &
       &  alt_sat, alt_sph

  namelist /nml_calibsat_ocn/ calc_start, calc_end, &
       &  l_leap_valid, &
       &  l_calib_monthly, &
       &  file_calib_ocn, &
       &  undef_calib, &
       &  calib_base_dir, &
       &  fout_base_sat, &
       &  fout_base_sph, &
       &  l_shift_sph

  !-----------------------------------------------------------------------

  idmon(1:num_month) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

  open(lun,file='namelist.calibsat_ocn',iostat=istat,status='old')

  if ( istat /= 0 ) then
    write(6,*) ' Cannot find file : namelist.calibsat_ocn'
    stop
  end if

  ios = 0

  rewind(lun)
  file_intv_sst = 0
  read(lun, nml=nml_sstdata, iostat=istat)
  ios = ios + istat

  rewind(lun)
  file_intv_srf = 0
  read(lun, nml=nml_srfdata, iostat=istat)
  ios = ios + istat

  rewind(lun)
  read(lun, nml=nml_mskdata, iostat=istat)
  ios = ios + istat

  rewind(lun)
  read(lun, nml=nml_calibsat_ocn, iostat=istat)
  ios = ios + istat

  if ( ios /= 0 ) then
    write(6,*) ' Read error : namelist.calibsat_ocn', ios
    stop
  end if

  close(lun)

  !----------------------------------------------------------------------

  call force_process__ini(l_leap_valid)

  imt = num_grid_sst
  jmt = 1
  kmt = 1

  allocate(sst(1:imt,1:jmt))
  allocate(ice(1:imt,1:jmt))
  allocate(msk(1:imt,1:jmt))

  item_name='SST'
  call register_file( &
       &  item_name, &
       &  sst_file,  &
       &  fin_base_sst,  &
       &  file_first_sst, &
       &  file_intv_type_sst, &
       &  file_intv_sst, &
       &  total_rec_sst, &
       &  data_intv_sec_sst, &
       &  sst_first, &
       &  imt, jmt,  &
       &  undef_sst, &
       &  l_little_endian = l_little_sst)

  item_name='ICE'
  call register_file( &
       &  item_name, &
       &  ice_file,  &
       &  fin_base_ice,  &
       &  file_first_sst, &
       &  file_intv_type_sst, &
       &  file_intv_sst, &
       &  total_rec_sst, &
       &  data_intv_sec_sst, &
       &  sst_first, &
       &  imt, jmt,  &
       &  undef_sst, &
       &  l_little_endian = l_little_srf)

  !------

  imf = num_grid_srf
  jmf = 1
  kmf = 1

  allocate(satf(1:imf,1:jmf), sphf(1:imf,1:jmf))
  allocate(slpf(1:imf,1:jmf))
  allocate(rlhf(1:imf,1:jmf), roaf(1:imf,1:jmf))

  allocate(satc(1:imf,1:jmf), sphc(1:imf,1:jmf))
  allocate(dat4f(1:imf,1:jmf))

  allocate(sat_ocn(1:imf,1:jmf,1:num_month))

  allocate(sstf(1:imf,1:jmf), icef(1:imf,1:jmf))

  !--------------------------------------------------------------------
  ! read mask

  lreclen = 4 * imt * jmt
  call open_file_direct(mtin1, fin_msk, lreclen, convert_mode='little_endian')

  read(mtin1,rec=1) dat4f
  msk(1:imt,1:jmt) = 1.0d0 - real(dat4f(1:imt,1:jmt),8)

  call close_file(mtin1)

  !--------------------------------------------------------------------
  ! read correction factor

  lreclen = 4 * imf * jmf
  call open_file_direct(mtin1, file_calib_ocn, lreclen, convert_mode='little_endian')

  do m = 1, num_month
    if (l_calib_monthly) then
      read(mtin1,rec=m) dat4f
    else
      read(mtin1,rec=1) dat4f
    end if
    sat_ocn(1:imf,1:jmf,m) = real(dat4f(1:imf,1:jmf),8)
  end do
  call close_file(mtin1)

  ! consistency check

  do m = 1, num_month
    do j = 1, jmf
      do i = 1, imf
        if ((msk(i,j) == 1.0d0) .and. (sat_ocn(i,j,m) == real(undef_calib,8))) then
          write(6,*) ' calibration factor undefined for ', i,j,m
          stop
        end if
      end do
    end do
  end do

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

    write(sst_dir,'(1a,1a,i4.4,i2.2)') trim(sst_base_dir),'/',current_file%year,current_file%month
    write(fin_sst,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(sst_dir),'/',trim(fin_base_sst),'.', &
         & current_file%year,current_file%month,current_file%day,current_file%hour
    write(fin_ice,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(sst_dir),'/',trim(fin_base_ice),'.', &
         & current_file%year,current_file%month,current_file%day,current_file%hour

    lreclen = 4 * imt * jmt
    call open_file_direct(mtin1, fin_sst, lreclen, convert_mode='little_endian')
    read(mtin1,rec=1) dat4f
    sstf(1:imf,1:jmf) = real(dat4f(1:imf,1:jmf),8) - tab
    call close_file(mtin1)

    lreclen = 4 * imt * jmt
    call open_file_direct(mtin2, fin_ice, lreclen, convert_mode='little_endian')
    read(mtin2,rec=1) dat4f
    icef(1:imf,1:jmf) = real(dat4f(1:imf,1:jmf),8)
    call close_file(mtin2)

    write(srf_dir,'(1a,1a,i4.4,i2.2)') trim(srf_base_dir),'/',current_file%year,current_file%month
    write(fin_sat,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(srf_dir),'/',trim(fin_base_sat),'.', &
         & current_file%year,current_file%month,current_file%day,current_file%hour

    lreclen = 4 * imf * jmf
    call open_file_direct(mtin1, fin_sat, lreclen, convert_mode='little_endian')
    read(mtin1,rec=1) dat4f
    satf(1:imf,1:jmf) = real(dat4f(1:imf,1:jmf),8) - tab
    call close_file(mtin1)

    if (l_shift_sph) then

      write(fin_sph,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(srf_dir),'/',trim(fin_base_sph),'.', &
           & current_file%year,current_file%month,current_file%day,current_file%hour
      write(fin_slp,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(srf_dir),'/',trim(fin_base_slp),'.', &
           & current_file%year,current_file%month,current_file%day,current_file%hour

      lreclen = 4 * imf * jmf
      call open_file_direct(mtin2, fin_sph, lreclen, convert_mode='little_endian')
      read(mtin2,rec=1) dat4f
      sphf(1:imf,1:jmf) = real(dat4f(1:imf,1:jmf),8)
      call close_file(mtin2)

      lreclen = 4 * imf * jmf
      call open_file_direct(mtin3, fin_slp, lreclen, convert_mode='little_endian')
      read(mtin3,rec=1) dat4f
      slpf(1:imf,1:jmf) = real(dat4f(1:imf,1:jmf),8) * 1.0d-2 ! [Pa] -> [hPa]
      call close_file(mtin3)

      do j = 1, jmf
        do i = 1, imf
          if (icef(i,j) > 0.0d0) then
            hl1 = (g1 + g2 * satf(i,j)) / (1.0d0 + g3 * satf(i,j)) + i1 * satf(i,j)
            hl2 = 10 ** hl1
            hl3 = 1.0d0 + 1.0d-6 * slpf(i,j) * (h1 + h2 * satf(i,j)**2)
            esat = hl2 * hl3 
            qsat = epsilon * esat / (slpf(i,j) - (1.0d0 - epsilon) * esat)
            !qsat = q1i * exp(q2i / (satf(i,j) + tab)) / rhoa
          else
            hl1 = (g1 + g2 * satf(i,j)) / (1.0d0 + g3 * satf(i,j))
            hl2 = 10 ** hl1
            hl3 = 1.0d0 + 1.0d-6 * slpf(i,j) * (h1 + h2 * satf(i,j)**2)
            esat = hl2 * hl3
            qsat = epsilon * esat / (slpf(i,j) - (1.0d0 - epsilon) * esat)
            !qsat = q0 * q1a * exp(q2a / (satf(i,j) + tab)) / rhoa 
          end if
          rlhf(i,j) = sphf(i,j) * (1.0d0 - qsat) / (qsat * (1.0d0 - sphf(i,j)))
        end do
      end do

    end if

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

    satc(:,:) = satf(:,:)

    do j = 1, jmf
      do i = 1, imf
        if ((msk(i,j) == 1.0d0) .and. (icef(i,j) == 0.0d0)) then
          satc(i,j) = satc(i,j) + (sat_ocn(i,j,monb) * bfm + sat_ocn(i,j,mona) * afm)
        end if
      end do
    end do

    if (l_shift_sph) then
      do j = 1, jmf
        do i = 1, imf
          if (msk(i,j) == 1.0d0) then
            if (icef(i,j) > 0.0d0) then
              hl1 = (g1 + g2 * satc(i,j)) / (1.0d0 + g3 * satc(i,j)) + i1 * satc(i,j)
              hl2 = 10 ** hl1
              hl3 = 1.0d0 + 1.0d-6 * slpf(i,j) * (h1 + h2 * satc(i,j)**2)
              esat = hl2 * hl3 
              qsat = epsilon * esat / (slpf(i,j) - (1.0d0 - epsilon) * esat)
              !qsat = q1i * exp(q2i / (satc(i,j) + tab)) / rhoa
            else
              hl1 = (g1 + g2 * satc(i,j)) / (1.0d0 + g3 * satc(i,j))
              hl2 = 10 ** hl1
              hl3 = 1.0d0 + 1.0d-6 * slpf(i,j) * (h1 + h2 * satc(i,j)**2)
              esat = hl2 * hl3
              qsat = epsilon * esat / (slpf(i,j) - (1.0d0 - epsilon) * esat)
              !qsat = q0 * q1a * exp(q2a / (satc(i,j) + tab)) / rhoa
            end if
            hl1 = 1.0d0 - qsat * (1.0d0 - rlhf(i,j))
            hl2 = qsat * rlhf(i,j)
            sphc(i,j) = hl2 / hl1
            sphc(i,j) = max(sphc(i,j),sphmin)
          end if
        end do
      end do
    end if

    !---------------------

    next_date = libmxe_calendar__addsec(current_date, data_intv_sec_srf, l_leap_valid)
    next_file = libmxe_calendar__addsec(current_file, data_intv_sec_srf, l_leap_valid)

    !---------------------
    ! output data

    write(calib_dir,'(1a,1a,i4.4,i2.2)') trim(calib_base_dir),'/',current_file%year,current_file%month

    write(fout_sat,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(calib_dir),'/',trim(fout_base_sat),'.', &
         & current_file%year,current_file%month,current_file%day,current_file%hour
    lreclen = 4 * imf * jmf
    call open_file_direct(mtot1, fout_sat, lreclen, convert_mode='little_endian')
    satc(1:imf,1:jmf) = satc(1:imf,1:jmf) + tab
    write(mtot1,rec=1) real(satc(1:imf,1:jmf),4)
    call close_file(mtot1)

    if (l_shift_sph) then
      write(fout_sph,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(calib_dir),'/',trim(fout_base_sph),'.', &
           & current_file%year,current_file%month,current_file%day,current_file%hour
      lreclen = 4 * imf * jmf
      call open_file_direct(mtot1, fout_sph, lreclen, convert_mode='little_endian')
      write(mtot1,rec=1) real(sphc(1:imf,1:jmf),4)
      call close_file(mtot1)
    end if

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

end program calibration_surface_temperature
