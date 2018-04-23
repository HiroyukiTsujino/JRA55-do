! -*-F90-*-
!------------------- calib_rad_interannual.F90 ------------------------
! Information:
!   Calibration of downward fluxes (shortwave, longwave, precipitation)
!----------------------------------------------------------------------
program calibration_downward_flux

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

  ! Radiation

  integer(4) :: imr, jmr, kmr
  real(8),allocatable :: alonr(:), alatr(:)
  real(8),allocatable :: dswr(:,:), dlwr(:,:)
  real(8),allocatable :: dswc(:,:), dlwc(:,:)
  real(4),allocatable :: dat4r(:,:)

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
  ! Radiation data

  character(256) :: rad_base_dir, rad_dir
  character(256) :: fin_base_dsw, fin_base_dlw
  character(256) :: fin_dsw, fin_dlw

  integer(4) :: file_first_rad(6)
  integer(4) :: file_intv_type_rad
  integer(4) :: file_intv_rad
  integer(4) :: total_rec_rad
  integer(4) :: data_intv_sec_rad
  integer(4) :: rad_first(6)
  real(4)    :: undef_rad
  logical    :: l_ymdir_rad

  !------------------
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
  character(256) :: fout_base_dsw
  character(256) :: fout_base_dlw
  character(256) :: fout_base_pcp

  character(256) :: fout_dsw
  character(256) :: fout_dlw
  character(256) :: fout_pcp

  !------------------

  logical :: l_leap_valid

  integer(4) :: lreclen

  integer(4) :: i, j, k, l, m, n

  !----------------------------------------------------------------------

  type(type_libmxe_para) :: radp, pcpp
  type(type_libmxe_grid) :: radg, pcpg
  character(256) :: file_namelist_rad, file_namelist_pcp

  integer(4),parameter :: lun = 10

  integer(4) :: ios, istat

  !---------------------------------------------------------------------

  logical :: l_calib_monthly

  character(256) :: file_calib_dsw
  character(256) :: file_calib_dlw
  character(256) :: file_calib_pcp

  real(8),allocatable :: dswrf_mag (:,:,:)
  real(8),allocatable :: dlwrf_mag (:,:,:)
  real(8),allocatable :: precip_mag(:,:,:)

  integer(4) :: current_month_sec
  integer(4) :: this_month_sec
  integer(4) :: prev_month_sec
  integer(4) :: latest_month_sec
  integer(4) :: from_previous_data
  real(8) :: afm, bfm
  integer(4) :: monb, mona

  real(8) :: hl1
  real(8) :: factor_div_pcp

  !---------------------------------------------------------------------

  namelist /nml_raddata/ file_namelist_rad, &
       &  rad_base_dir, &
       &  fin_base_dsw, fin_base_dlw, &
       &  file_first_rad, &
       &  file_intv_type_rad, file_intv_rad, total_rec_rad, data_intv_sec_rad, &
       &  rad_first, undef_rad, &
       &  l_ymdir_rad

  namelist /nml_pcpdata/ file_namelist_pcp, &
       &  pcp_base_dir, &
       &  fin_base_pcp, &
       &  file_first_pcp, &
       &  file_intv_type_pcp, file_intv_pcp, total_rec_pcp, data_intv_sec_pcp, &
       &  pcp_first, undef_pcp, &
       &  l_ymdir_pcp

  namelist /nml_calibrad/ calc_start, calc_end, &
       &  l_leap_valid, &
       &  l_calib_monthly, &
       &  file_calib_dsw, &
       &  file_calib_dlw, &
       &  file_calib_pcp, &
       &  calib_base_dir, &
       &  fout_base_dsw, fout_base_dlw, &
       &  fout_base_pcp, &
       &  factor_div_pcp

  !-----------------------------------------------------------------------

  factor_div_pcp = 1.0d0

  open(lun,file='namelist.calibradprcp',iostat=istat,status='old')

  if ( istat /= 0 ) then
    write(6,*) ' Cannot find file : namelist.calibrad'
    stop
  end if

  ios = 0

  rewind(lun)
  file_intv_rad = 0
  read(lun, nml=nml_raddata, iostat=istat)
  ios = ios + istat

  rewind(lun)
  file_intv_pcp = 0
  read(lun, nml=nml_pcpdata, iostat=istat)
  ios = ios + istat

  rewind(lun)
  read(lun, nml=nml_calibrad, iostat=istat)
  ios = ios + istat

  if ( ios /= 0 ) then
    write(6,*) ' Read error : namelist.calibradprcp ', ios
    stop
  end if

  close(lun)

  call force_process__ini(l_leap_valid)

  !----------------------------------------------------------------------

  call libmxe_para__register(radp,file_namelist=file_namelist_rad)
  call libmxe_grid__register(radg,radp)
  imr = radp%imut
  jmr = radp%jmut
  kmr = radp%km
  allocate(alonr(1:imr), alatr(1:jmr))
  alonr(1:imr) = radg%lonu(1:imr)
  alatr(1:jmr) = radg%latu(1:jmr)

  allocate(dswr(1:imr,1:jmr), dlwr(1:imr,1:jmr))
  allocate(dswc(1:imr,1:jmr), dlwc(1:imr,1:jmr))
  allocate(dat4r(1:imr,1:jmr))

  allocate(dswrf_mag(1:imr,1:jmr,1:num_month))
  allocate(dlwrf_mag(1:imr,1:jmr,1:num_month))

  !---------------------------------------------------------------

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

  allocate(precip_mag(1:imp,1:jmp,1:num_month))

  !--------------------------------------------------------------------
  ! read calibration factor

  lreclen = 4 * imr * jmr
  call open_file_direct(mtin1, file_calib_dsw, lreclen)

  do m = 1, num_month
    if (l_calib_monthly) then
      read(mtin1,rec=m) dat4r
    else
      read(mtin1,rec=1) dat4r
    end if
    dswrf_mag(1:imr,1:jmr,m) = real(dat4r(1:imr,1:jmr),8)
  end do
  call close_file(mtin1)

  lreclen = 4 * imr * jmr
  call open_file_direct(mtin1, file_calib_dlw, lreclen)

  do m = 1, num_month
    if (l_calib_monthly) then
      read(mtin1,rec=m) dat4r
    else
      read(mtin1,rec=1) dat4r
    end if
    dlwrf_mag(1:imr,1:jmr,m) = real(dat4r(1:imr,1:jmr),8)
  end do
  call close_file(mtin1)

  write(6,*) 'Precipitation: Divided for unit conversion = ', factor_div_pcp

  lreclen = 4 * imp * jmp
  call open_file_direct(mtin1, file_calib_pcp, lreclen)

  do m = 1, num_month
    if (l_calib_monthly) then
      read(mtin1,rec=m) dat4p
    else
      read(mtin1,rec=1) dat4p
    end if
    precip_mag(1:imp,1:jmp,m) = real(dat4p(1:imp,1:jmp),8) / factor_div_pcp
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

  current_file%year   = file_first_rad(1)
  current_file%month  = file_first_rad(2)
  current_file%day    = file_first_rad(3)
  current_file%hour   = file_first_rad(4)
  current_file%minute = file_first_rad(5)
  current_file%second = file_first_rad(6)

  write(6,*) 
  write(6,*) ' MAIN LOOP START '

  LOOP_SURF_FORCE: do while ( .true. )

    write(6,*) ' This step ', current_date%year, current_date%month, current_date%day, &
         & current_date%hour, current_date%minute, data_intv_sec_rad

    write(rad_dir,'(1a,1a,i4.4,i2.2)') trim(rad_base_dir),'/',current_file%year,current_file%month
    write(fin_dsw,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(rad_dir),'/',trim(fin_base_dsw),'.', &
         & current_file%year,current_file%month,current_file%day,current_file%hour
    write(fin_dlw,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(rad_dir),'/',trim(fin_base_dlw),'.', &
         & current_file%year,current_file%month,current_file%day,current_file%hour

    write(pcp_dir,'(1a,1a,i4.4,i2.2)') trim(pcp_base_dir),'/',current_file%year,current_file%month
    write(fin_pcp,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(pcp_dir),'/',trim(fin_base_pcp),'.', &
         & current_file%year,current_file%month,current_file%day,current_file%hour

    lreclen = 4 * imr * jmr
    call open_file_direct(mtin1, fin_dsw, lreclen)
!!!    write(6,*) ' reading from ', trim(fin_dsw)
    read(mtin1,rec=1) dat4r
    dswr(1:imr,1:jmr) = real(dat4r(1:imr,1:jmr),8)
    call close_file(mtin1)

    lreclen = 4 * imr * jmr
    call open_file_direct(mtin2, fin_dlw, lreclen)
!!!    write(6,*) ' reading from ', trim(fin_dlw)
    read(mtin2,rec=1) dat4r
    dlwr(1:imr,1:jmr) = real(dat4r(1:imr,1:jmr),8)
    call close_file(mtin2)

    lreclen = 4 * imp * jmp
    call open_file_direct(mtin3, fin_pcp, lreclen)
!!!    write(6,*) ' reading from ', trim(fin_pcp)
    read(mtin3,rec=1) dat4p
    prcpp(1:imp,1:jmp) = real(dat4p(1:imp,1:jmp),8)
    call close_file(mtin3)

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

    do j = 1, jmr
      do i = 1, imr
        dswc(i,j) = (dswrf_mag(i,j,monb) * bfm + dswrf_mag(i,j,mona) * afm) * dswr(i,j)
        dlwc(i,j) = (dlwrf_mag(i,j,monb) * bfm + dlwrf_mag(i,j,mona) * afm) * dlwr(i,j)
      end do
    end do

    do j = 1, jmp
      do i = 1, imp
        prcpc(i,j) = (precip_mag(i,j,monb) * bfm + precip_mag(i,j,mona) * afm) * prcpp(i,j)
      end do
    end do

    ! advance in time

    next_date = libmxe_calendar__addsec(current_date, data_intv_sec_rad, l_leap_valid)
    next_file = libmxe_calendar__addsec(current_file, data_intv_sec_rad, l_leap_valid)

    !---------------------
    ! output data


    write(calib_dir,'(1a,1a,i4.4,i2.2)') trim(calib_base_dir),'/',current_file%year,current_file%month
    write(fout_dsw,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(calib_dir),'/',trim(fout_base_dsw),'.', &
         & current_file%year,current_file%month,current_file%day,current_file%hour
    write(fout_dlw,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(calib_dir),'/',trim(fout_base_dlw),'.', &
         & current_file%year,current_file%month,current_file%day,current_file%hour
    write(fout_pcp,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(calib_dir),'/',trim(fout_base_pcp),'.', &
         & current_file%year,current_file%month,current_file%day,current_file%hour

    lreclen = 4 * imr * jmr
    call open_file_direct(mtot1, fout_dsw, lreclen)
!!!    write(6,*) ' written to ', trim(fout_dsw)
    write(mtot1,rec=1) real(dswc(1:imr,1:jmr),4)
    call close_file(mtot1)

    lreclen = 4 * imr * jmr
    call open_file_direct(mtot1, fout_dlw, lreclen)
!!!    write(6,*) ' written to ', trim(fout_dlw)
    write(mtot1,rec=1) real(dlwc(1:imr,1:jmr),4)
    call close_file(mtot1)

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

end program calibration_downward_flux
