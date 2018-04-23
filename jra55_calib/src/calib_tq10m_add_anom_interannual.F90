! -*-F90-*-
!------------- calib_tmp10m_add_anom_interannual.F90 -----------------
!  Information:
!     Calibrate surface temperature and specific humidity
!-------------------------------------------------------------------
program calibration_surface_temperature_add_anom

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

  real(8), parameter :: sphmin = 2.0d-5 ! minimum of specific humidity

  !------------

  character(len=16) :: item_name
  type(type_force) :: tanm_file
  type(type_force) :: qanm_file

  !------------

  type(type_calendar) :: start_date
  type(type_calendar) :: end_date

  type(type_calendar) :: current_date
  type(type_calendar) :: current_file
  type(type_calendar) :: next_date
  type(type_calendar) :: next_file

  integer(4) :: calc_start(6)
  integer(4) :: calc_end(6)

  ! Surface Atmospheric State

  integer(4) :: imf, jmf, kmf
  real(8),allocatable :: alonf(:), alatf(:)

  real(8),allocatable :: satf(:,:), sphf(:,:)
  real(8),allocatable :: satc(:,:), sphc(:,:) ! corrected

  real(4),allocatable :: dat4f(:,:)

  ! Air temperature anomaly

  integer(4) :: imt, jmt, kmt
  real(8),allocatable :: alond(:), alatd(:)
  real(8),allocatable :: aexl_anm(:,:)
  real(8),allocatable :: tanm(:,:), qanm(:,:)

  !-----------------------------------------------

  integer(4) :: mtin1, mtin2, mtin3
  integer(4) :: mtot1, mtot2, mtot3

  !------------------
  ! Anomaly data

  character(256) :: anm_base_dir, anm_dir
  character(256) :: fin_base_tanm, fin_base_qanm
  integer(4) :: file_first_anm(6)
  integer(4) :: file_intv_type_anm
  integer(4) :: file_intv_anm
  integer(4) :: total_rec_anm
  integer(4) :: data_intv_sec_anm
  integer(4) :: anm_first(6)
  real(4)    :: undef_anm
  logical    :: l_ymdir_anm

  !------------------
  ! Surface data

  character(256) :: srf_base_dir, srf_dir
  character(256) :: sat_base_dir
  character(256) :: sph_base_dir
  character(256) :: fin_base_sat, fin_base_sph

  character(256) :: fin_sat, fin_sph

  integer(4) :: file_first_srf(6)
  integer(4) :: file_intv_type_srf
  integer(4) :: file_intv_srf
  integer(4) :: total_rec_srf
  integer(4) :: data_intv_sec_srf
  integer(4) :: srf_first(6)
  real(4)    :: undef_srf
  logical    :: l_ymdir_srf

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

  type(type_libmxe_para) :: srfp, anmp
  type(type_libmxe_grid) :: srfg, anmg
  type(type_libmxe_topo) :: srft, anmt
  character(256) :: file_namelist_srf, file_namelist_anm

  integer(4),parameter :: lun = 10

  integer(4) :: ios, istat

  !---------------------------------------------------------------------

  real(8) :: hl1, hl2, hl3

  !---------------------------------------------------------------------

  namelist /nml_anmdata/ file_namelist_anm, &
       &  anm_base_dir, &
       &  fin_base_tanm, &
       &  fin_base_qanm, &
       &  file_first_anm, &
       &  file_intv_type_anm, file_intv_anm, total_rec_anm, data_intv_sec_anm, &
       &  anm_first, undef_anm, &
       &  l_ymdir_anm

  namelist /nml_srfdata/ file_namelist_srf, &
       &  srf_base_dir, &
       &  sat_base_dir, &
       &  sph_base_dir, &
       &  fin_base_sat, fin_base_sph, &
       &  file_first_srf, &
       &  file_intv_type_srf, file_intv_srf, total_rec_srf, data_intv_sec_srf, &
       &  srf_first, undef_srf, l_ymdir_srf, &
       &  alt_sat, alt_sph

  namelist /nml_calib_satsph/ calc_start, calc_end, &
       &  l_leap_valid, &
       &  calib_base_dir, &
       &  fout_base_sat, &
       &  fout_base_sph

  !-----------------------------------------------------------------------

  idmon(1:num_month) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

  open(lun,file='namelist.calib_satsph',iostat=istat,status='old')

  if ( istat /= 0 ) then
    write(6,*) ' Cannot find file : namelist.calib_satsph'
    stop
  end if

  ios = 0

  rewind(lun)
  file_intv_anm = 0
  read(lun, nml=nml_anmdata, iostat=istat)
  ios = ios + istat

  rewind(lun)
  file_intv_srf = 0
  srf_base_dir  = 'no_dir' 
  sat_base_dir  = 'no_dir' 
  sph_base_dir  = 'no_dir' 
  read(lun, nml=nml_srfdata, iostat=istat)
  ios = ios + istat

  rewind(lun)
  read(lun, nml=nml_calib_satsph, iostat=istat)
  ios = ios + istat

  if ( ios /= 0 ) then
    write(6,*) ' Read error : namelist.calib_satsph', ios
    stop
  end if

  close(lun)

  !----------------------------------------------------------------------

  call libmxe_para__register(srfp,file_namelist=file_namelist_srf)
  call libmxe_grid__register(srfg,srfp)
  call libmxe_topo__register(srft,srfp)
  call libmxe_topo__aexl(srft,srfp)
  imf = srfp%imut
  jmf = srfp%jmut
  kmf = srfp%km
  allocate(alonf(1:imf), alatf(1:jmf))
  alonf(1:imf) = srfg%lonu(1:imf)
  alatf(1:jmf) = srfg%latu(1:jmf)

  allocate(satf(1:imf,1:jmf), sphf(1:imf,1:jmf))

  allocate(satc(1:imf,1:jmf), sphc(1:imf,1:jmf))
  allocate(dat4f(1:imf,1:jmf))

  !------

  call libmxe_para__register(anmp,file_namelist=file_namelist_anm)
  call libmxe_grid__register(anmg,anmp)
  call libmxe_topo__register(anmt,anmp)
  call libmxe_topo__aexl(anmt,anmp)
  imt = anmp%imut
  jmt = anmp%jmut
  kmt = anmp%km

  if ( imt /= imf .or. jmt /= jmt .or. kmt /= kmf ) then
    write(6,*) ' This program assumes that raw data and anomaly have common grid structure. '
    write(6,*) ' Terminating .....'
    stop
  end if

  allocate(alond(1:imt), alatd(1:jmt))
  alond(1:imt) = anmg%lonu(1:imt)
  alatd(1:jmt) = anmg%latu(1:jmt)
  allocate(aexl_anm(1:imt,1:jmt))
  aexl_anm(1:imt,1:jmt) = anmt%aexl(1:imt,1:jmt,1)

  call force_process__ini(l_leap_valid)

  allocate(tanm(1:imt,1:jmt))
  allocate(qanm(1:imt,1:jmt))

  item_name='TANM'
  call register_file( &
       &  item_name, &
       &  tanm_file,  &
       &  fin_base_tanm, &
       &  file_first_anm, &
       &  file_intv_type_anm, &
       &  file_intv_anm, &
       &  total_rec_anm, &
       &  data_intv_sec_anm, &
       &  anm_first, &
       &  imt, jmt,  &
       &  undef_anm )

  item_name='QANM'
  call register_file( &
       &  item_name, &
       &  qanm_file,  &
       &  fin_base_qanm, &
       &  file_first_anm, &
       &  file_intv_type_anm, &
       &  file_intv_anm, &
       &  total_rec_anm, &
       &  data_intv_sec_anm, &
       &  anm_first, &
       &  imt, jmt,  &
       &  undef_anm )

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

  anm_dir = trim(anm_base_dir)
  call get_first_two_data(current_date, tanm_file, anm_dir, l_ymdir_anm)
  call get_first_two_data(current_date, qanm_file, anm_dir, l_ymdir_anm)

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

    !-----------------------------
    ! reading anomaly

    anm_dir = trim(anm_base_dir)
    call update_data(current_date, tanm_file, anm_dir, l_ymdir_anm)
    call update_data(current_date, qanm_file, anm_dir, l_ymdir_anm)
    do j = 1, jmt
      do i = 1, imt
        tanm(i,j) = tanm_file%dat_b(i,j) * tanm_file%br + tanm_file%dat_a(i,j) * tanm_file%ar
        tanm(i,j) = tanm(i,j) * aexl_anm(i,j)
        qanm(i,j) = qanm_file%dat_b(i,j) * qanm_file%br + qanm_file%dat_a(i,j) * qanm_file%ar
        qanm(i,j) = qanm(i,j) * aexl_anm(i,j)
      end do
    end do

    !-----------------------------

    lreclen = 4 * imf * jmf

    if (trim(sat_base_dir) /= 'no_dir') then
      write(srf_dir,'(1a,1a,i4.4,i2.2)') trim(sat_base_dir),'/',current_file%year,current_file%month
    else
      write(srf_dir,'(1a,1a,i4.4,i2.2)') trim(srf_base_dir),'/',current_file%year,current_file%month
    end if
    write(fin_sat,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(srf_dir),'/',trim(fin_base_sat),'.', &
         & current_file%year,current_file%month,current_file%day,current_file%hour
    call open_file_direct(mtin1, fin_sat, lreclen)
    read(mtin1,rec=1) dat4f
    satf(1:imf,1:jmf) = real(dat4f(1:imf,1:jmf),8)
    call close_file(mtin1)

    if (trim(sph_base_dir) /= 'no_dir') then
      write(srf_dir,'(1a,1a,i4.4,i2.2)') trim(sph_base_dir),'/',current_file%year,current_file%month
    else
      write(srf_dir,'(1a,1a,i4.4,i2.2)') trim(srf_base_dir),'/',current_file%year,current_file%month
    end if
    write(fin_sph,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(srf_dir),'/',trim(fin_base_sph),'.', &
         & current_file%year,current_file%month,current_file%day,current_file%hour
    call open_file_direct(mtin2, fin_sph, lreclen)
    read(mtin2,rec=1) dat4f
    sphf(1:imf,1:jmf) = real(dat4f(1:imf,1:jmf),8)
    call close_file(mtin2)

    !------------------------------------------------
    ! Adding anomaly

    do j = 1, jmf
      do i = 1, imf
        satc(i,j) = satf(i,j) + tanm(i,j)
        sphc(i,j) = sphf(i,j) + qanm(i,j)
        sphc(i,j) = max(sphc(i,j),sphmin)
      end do
    end do

    !---------------------

    next_date = libmxe_calendar__addsec(current_date, data_intv_sec_srf, l_leap_valid)
    next_file = libmxe_calendar__addsec(current_file, data_intv_sec_srf, l_leap_valid)

    !---------------------
    ! output data

    write(calib_dir,'(1a,1a,i4.4,i2.2)') trim(calib_base_dir),'/',current_file%year,current_file%month
    write(fout_sat,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(calib_dir),'/',trim(fout_base_sat),'.', &
         & current_file%year,current_file%month,current_file%day,current_file%hour
    write(fout_sph,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(calib_dir),'/',trim(fout_base_sph),'.', &
         & current_file%year,current_file%month,current_file%day,current_file%hour

    lreclen = 4 * imf * jmf
    call open_file_direct(mtot1, fout_sat, lreclen)
    satc(1:imf,1:jmf) = satc(1:imf,1:jmf)
    write(mtot1,rec=1) real(satc(1:imf,1:jmf),4)
    call close_file(mtot1)

    lreclen = 4 * imf * jmf
    call open_file_direct(mtot1, fout_sph, lreclen)
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

end program calibration_surface_temperature_add_anom
