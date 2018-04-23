! -*-F90-*-
!------------------- calib_precip_mediterranean.F90 -------------------
! Information:
!   Calibration of downward fluxes (shortwave, longwave, precipitation)
!----------------------------------------------------------------------
program calibration_precipitation_mediterranean

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

  type(type_calendar) :: current_year_start
  type(type_calendar) :: prev_year_start
  type(type_calendar) :: latest_year_start

  integer(4) :: calc_start(6)
  integer(4) :: calc_end(6)

  ! Precipitation

  integer(4) :: imp, jmp, kmp
  real(8),allocatable :: alonp(:), alatp(:)
  real(8),allocatable :: prcpp(:,:)
  real(8),allocatable :: prcpc(:,:)
  real(4),allocatable :: dat4p(:,:)

  !-----------------------------------------------

  integer(4) :: mtin1, mtin2, mtin3
  integer(4) :: mtot1, mtot2, mtot3

  !-----------------------------------------------
  ! Precipitation data

  character(256) :: pcp_base_dir, pcp_dir
  character(256) :: fin_base_pcp
  character(256) :: fin_pcp

  integer(4) :: file_first_pcp(6)
  integer(4) :: file_intv_type_pcp
  integer(4) :: file_intv_pcp
  integer(4) :: total_rec_pcp
  integer(4) :: data_intv_sec_pcp
  integer(4) :: pcp_first(6)
  real(4)    :: undef_pcp
  logical    :: l_ymdir_pcp

  !------------------
  ! Output

  character(256) :: calib_base_dir, calib_dir
  character(256) :: fout_base_pcp

  character(256) :: fout_pcp

  !------------------

  logical :: l_leap_valid, l_leap

  integer(4) :: lreclen

  integer(4) :: i, j, k, l, m, n

  !----------------------------------------------------------------------

  type(type_libmxe_para) :: pcpp
  type(type_libmxe_grid) :: pcpg
  character(256) :: file_namelist_pcp

  integer(4),parameter :: lun = 10

  integer(4) :: ios, istat

  !---------------------------------------------------------------------

  character(256) :: file_calib_pcp
  character(256) :: file_mask
  real(8),allocatable :: apply_mask(:,:)

  integer(4) :: start_yr_calib
  integer(4) :: end_yr_calib
  integer(4) :: irec_in, nyear
  real(8),allocatable :: precip_mag(:)
  real(4) :: work_tmp

  integer(4) :: current_year_sec
  integer(4) :: this_year_sec
  integer(4) :: prev_year_sec
  integer(4) :: latest_year_sec
  integer(4) :: from_previous_data
  real(8) :: afy, bfy
  integer(4) :: yrb, yra

  real(8) :: hl1, hl2, hld
  real(8) :: factor_calib_pcp
  real(8) :: factor_div_pcp

  !---------------------------------------------------------------------

  namelist /nml_pcpdata/ file_namelist_pcp, &
       &  pcp_base_dir, &
       &  fin_base_pcp, &
       &  file_first_pcp, &
       &  file_intv_type_pcp, file_intv_pcp, total_rec_pcp, data_intv_sec_pcp, &
       &  pcp_first, undef_pcp, &
       &  l_ymdir_pcp

  namelist /nml_calibpcp_med/ &
       &  calc_start, calc_end, &
       &  l_leap_valid, &
       &  start_yr_calib, &
       &  end_yr_calib, &
       &  file_calib_pcp, &
       &  file_mask,      &
       &  calib_base_dir, &
       &  fout_base_pcp, &
       &  factor_div_pcp

  !-----------------------------------------------------------------------

  factor_calib_pcp = 0.0d0
  factor_div_pcp = 1.0d0

  allocate(precip_mag(start_yr_calib:end_yr_calib))

  open(lun,file='namelist.calibpcp.med',iostat=istat,status='old')

  if ( istat /= 0 ) then
    write(6,*) ' Cannot find file : namelist.calibpcp.med'
    stop
  end if

  ios = 0

  rewind(lun)
  file_intv_pcp = 0
  read(lun, nml=nml_pcpdata, iostat=istat)
  ios = ios + istat

  rewind(lun)
  read(lun, nml=nml_calibpcp_med, iostat=istat)
  ios = ios + istat

  if ( ios /= 0 ) then
    write(6,*) ' Read error : namelist.calibpcp.med', ios
    stop
  end if

  close(lun)

  call force_process__ini(l_leap_valid)

  !----------------------------------------------------------------------

  call libmxe_para__register(pcpp,file_namelist=file_namelist_pcp)
  call libmxe_grid__register(pcpg,pcpp)
  imp = pcpp%imut
  jmp = pcpp%jmut
  kmp = pcpp%km
  allocate(alonp(1:imp), alatp(1:jmp))
  alonp(1:imp) = pcpg%lonu(1:imp)
  alatp(1:jmp) = pcpg%latu(1:jmp)

  allocate(prcpp(1:imp,1:jmp))
  allocate(prcpc(1:imp,1:jmp))
  allocate(dat4p(1:imp,1:jmp))
  allocate(apply_mask(1:imp,1:jmp))

  !--------------------------------------------------------------------
  ! read calibration factor

  lreclen = 4

  call open_file_direct(mtin1, file_calib_pcp, lreclen)

  irec_in = 0

  do nyear = start_yr_calib, end_yr_calib
    irec_in = irec_in + 1
    write(6,*) irec_in
    read(mtin1,rec=irec_in) work_tmp
    precip_mag(nyear) = real(work_tmp,8)
  end do

  call close_file(mtin1)

  lreclen = 4 * imp * jmp

  call open_file_direct(mtin2, file_mask, lreclen)

  read(mtin2,rec=1) dat4p
  apply_mask(1:imp,1:jmp) = real(dat4p(1:imp,1:jmp),8)

  call close_file(mtin2)

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

  current_file%year   = file_first_pcp(1)
  current_file%month  = file_first_pcp(2)
  current_file%day    = file_first_pcp(3)
  current_file%hour   = file_first_pcp(4)
  current_file%minute = file_first_pcp(5)
  current_file%second = file_first_pcp(6)

  write(6,*) 
  write(6,*) ' MAIN LOOP START '

  LOOP_SURF_FORCE: do while ( .true. )

    write(6,*) ' This step ', current_date%year, current_date%month, current_date%day, &
         & current_date%hour, current_date%minute, data_intv_sec_pcp

    write(pcp_dir,'(1a,1a,i4.4,i2.2)') trim(pcp_base_dir),'/',current_file%year,current_file%month
    write(fin_pcp,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(pcp_dir),'/',trim(fin_base_pcp),'.', &
         & current_file%year,current_file%month,current_file%day,current_file%hour

    lreclen = 4 * imp * jmp
    call open_file_direct(mtin3, fin_pcp, lreclen)

    read(mtin3,rec=1) dat4p
    prcpp(1:imp,1:jmp) = real(dat4p(1:imp,1:jmp),8)
    call close_file(mtin3)

    current_year_start%year   = current_date%year
    current_year_start%month  = 1
    current_year_start%day    = 1
    current_year_start%hour   = 0
    current_year_start%minute = 0
    current_year_start%second = 0

    if (l_leap_valid) then
      l_leap = libmxe_calendar__l_leap_year( current_date%year )
      if (l_leap) then
        this_year_sec = 366 * 86400
      else
        this_year_sec = 365 * 86400
      end if
    else
      l_leap = .false.
      this_year_sec = 365 * 86400
    end if

    current_year_sec = libmxe_calendar__diffsec(current_year_start, current_date, l_leap_valid)

    hl1 = real(current_year_sec,8) / real(this_year_sec,8)

    if (hl1 < 0.5d0) then
      latest_year_start%year = current_date%year
      prev_year_start%year   = current_date%year - 1
    else
      latest_year_start%year = current_date%year + 1
      prev_year_start%year   = current_date%year
    end if

    prev_year_start%month  = 1
    prev_year_start%day    = 1
    prev_year_start%hour   = 0
    prev_year_start%minute = 0
    prev_year_start%second = 0

    latest_year_start%month  = 1
    latest_year_start%day    = 1
    latest_year_start%hour   = 0
    latest_year_start%minute = 0
    latest_year_start%second = 0

    yrb = prev_year_start%year
    yra = latest_year_start%year

    if (l_leap_valid) then
      l_leap = libmxe_calendar__l_leap_year(prev_year_start%year)
      if (l_leap) then
        prev_year_sec = 366 * 86400
      else
        prev_year_sec = 365 * 86400
      end if
    else
      l_leap = .false.
      prev_year_sec = 365 * 86400
    end if

    if (l_leap_valid) then
      l_leap = libmxe_calendar__l_leap_year(latest_year_start%year)
      if (l_leap) then
        latest_year_sec = 366 * 86400
      else
        latest_year_sec = 365 * 86400
      end if
    else
      l_leap = .false.
      latest_year_sec = 365 * 86400
    end if

    from_previous_data = libmxe_calendar__diffsec(prev_year_start,current_date,l_leap_valid) - prev_year_sec / 2
    afy = 2.0d0 * real(from_previous_data,8) / real(prev_year_sec+latest_year_sec,8)
    bfy = 1.0d0 - afy
    write(6,*) ' bfy = ', bfy, ' afm  = ', afy
    write(6,*) ' yrb = ', yrb, ' yra  = ', yra
    write(6,*) ' multiplication factor = ', hld

    hld = precip_mag(yrb) * bfy + precip_mag(yra) * afy
    do j = 1, jmp
      do i = 1, imp
        hl2 = hld * apply_mask(i,j) + (1.0d0 - apply_mask(i,j))
        prcpc(i,j) = hl2 * prcpp(i,j)
      end do
    end do

    ! advance in time

    next_date = libmxe_calendar__addsec(current_date, data_intv_sec_pcp, l_leap_valid)
    next_file = libmxe_calendar__addsec(current_file, data_intv_sec_pcp, l_leap_valid)

    !---------------------
    ! output data

    write(calib_dir,'(1a,1a,i4.4,i2.2)') trim(calib_base_dir),'/',current_file%year,current_file%month
    write(fout_pcp,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(calib_dir),'/',trim(fout_base_pcp),'.', &
         & current_file%year,current_file%month,current_file%day,current_file%hour

    lreclen = 4 * imp * jmp
    call open_file_direct(mtot1, fout_pcp, lreclen)
!!!    write(6,*) ' written to ', trim(fout_pcp)
    write(mtot1,rec=1) real(prcpc(1:imp,1:jmp),4)
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

end program calibration_precipitation_mediterranean
