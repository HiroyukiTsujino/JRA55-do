! -*-F90-*-
!----------- diagflux_bulk_interannual_on_brtmp.F90 ----------------
!  Information:
!     Diangnose bulk flux using SST and Surface Atmospheric State
!-------------------------------------------------------------------
program diagnosis_bulk_flux

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
  real(8), parameter :: sst_freeze = -1.8d0
  real(8), parameter :: sst_bad = 40.0d0

  !------------

  character(len=16) :: item_name

  type(type_force) :: sst_file
  type(type_force) :: ice_file

  type(type_force) :: sat_file
  type(type_force) :: sph_file
  type(type_force) :: slp_file
  type(type_force) :: us_file
  type(type_force) :: vs_file

  type(type_calendar) :: start_date
  type(type_calendar) :: current_date
  type(type_calendar) :: next_date
  type(type_calendar) :: end_date

  integer(4) :: calc_start(6)
  integer(4) :: calc_end(6)

  ! Surface Atmospheric State

  integer(4) :: imf, jmf, kmf
  real(8),allocatable :: alonf(:), alatf(:)

  real(8),allocatable :: usf(:,:), vsf(:,:)
  real(8),allocatable :: satf(:,:), sphf(:,:)
  real(8),allocatable :: slpf(:,:), wdvf(:,:)

  ! SST and ICE

  integer(4) :: imt, jmt, kmt
  real(8),allocatable :: alond(:), alatd(:)

  real(8),allocatable :: aexl(:,:)
  real(8),allocatable :: sst(:,:)
  real(8),allocatable :: ice(:,:)

  ! following items are diagnosed in this program

  integer(4) :: int_hist
  logical :: l_histout
  real(8),allocatable :: wsx(:,:), wsy(:,:)
  real(8),allocatable :: qla(:,:), qsn(:,:)
  real(8),allocatable :: evp(:,:)
  real(8),allocatable :: satu(:,:), sphu(:,:)
  real(8),allocatable :: dtu(:,:), dqu(:,:)
  real(8),allocatable :: u10n(:,:), v10n(:,:)
  real(8),allocatable :: w10n(:,:)

  integer(4),allocatable :: num_hist(:,:)
  real(8),allocatable :: wsxm(:,:), wsym(:,:)
  real(8),allocatable :: qlam(:,:), qsnm(:,:)
  real(8),allocatable :: evpm(:,:)
  real(8),allocatable :: u10nm(:,:), v10nm(:,:)
  real(8),allocatable :: w10nm(:,:)

  !-----------------------------------------------

  integer(4) :: mtot1

  !-----------------------------------------------
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

  !------------------
  ! Surface data

  character(256) :: srf_base_dir, srf_dir
  character(256) :: sat_base_dir
  character(256) :: sph_base_dir
  character(256) :: wind_base_dir
  character(256) :: slp_base_dir
  character(256) :: fin_base_sat, fin_base_sph
  character(256) :: fin_base_us, fin_base_vs
  character(256) :: fin_base_slp

  character(256) :: fin_sat, fin_sph
  character(256) :: fin_us, fin_vs
  character(256) :: fin_slp

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
  real(8)    :: alt_wind

  !------------------
  ! Output

  character(256) :: flx_base_dir, flx_dir

  character(256) :: fout_base_taux, fout_base_tauy
  character(256) :: fout_base_sens
  character(256) :: fout_base_latent
  character(256) :: fout_base_evapor
  character(256) :: fout_base_u10n
  character(256) :: fout_base_v10n
  character(256) :: fout_base_w10n

  character(256) :: fout_taux, fout_tauy
  character(256) :: fout_sens
  character(256) :: fout_latent
  character(256) :: fout_evapor
  character(256) :: fout_u10n
  character(256) :: fout_v10n
  character(256) :: fout_w10n

  !------------------

  logical :: l_leap_valid

  integer(4) :: nstep

  integer(4) :: lreclen

  integer(4) :: i, j, k, l, m, n

  !----------------------------------------------------------------------

  type(type_libmxe_para) :: srfp, sstp
  type(type_libmxe_grid) :: srfg, sstg
  type(type_libmxe_topo) :: sstt
  character(256) :: file_namelist_sst, file_namelist_srf

  integer(4),parameter :: lun = 10

  integer(4) :: ios, istat

  real(8) :: area_earth_orgdata
  real(8) :: hl1

  !--------------------------------------------------------------------

  namelist /nml_sstdata/ file_namelist_sst, &
       &  sst_base_dir, &
       &  fin_base_sst, fin_base_ice, &
       &  file_first_sst, &
       &  file_intv_type_sst, file_intv_sst, total_rec_sst, data_intv_sec_sst, &
       &  sst_first, undef_sst, &
       &  l_ymdir_sst

  namelist /nml_srfdata/ file_namelist_srf, &
       &  srf_base_dir, &
       &  sat_base_dir, &
       &  sph_base_dir, &
       &  wind_base_dir, &
       &  slp_base_dir, &
       &  fin_base_sat, fin_base_sph, &
       &  fin_base_us, fin_base_vs,   &
       &  fin_base_slp,               &
       &  file_first_srf, &
       &  file_intv_type_srf, file_intv_srf, total_rec_srf, data_intv_sec_srf, &
       &  srf_first, undef_srf, l_ymdir_srf, &
       &  alt_sat, alt_sph, alt_wind

  namelist /nml_diagbulkflx/ calc_start, calc_end, &
       &  l_leap_valid, &
       &  int_hist, &
       &  flx_base_dir, &
       &  fout_base_taux, fout_base_tauy, &
       &  fout_base_sens, fout_base_latent, &
       &  fout_base_evapor, &
       &  fout_base_u10n, &
       &  fout_base_v10n, &
       &  fout_base_w10n

  !-----------------------------------------------------------------------

  open(lun,file='namelist.diagbulkflx',iostat=istat,status='old')

  if ( istat /= 0 ) then
    write(6,*) ' Cannot find file : namelist.diagbulkflx'
    stop
  end if

  ios = 0

  rewind(lun)
  file_intv_sst = 0
  read(lun, nml=nml_sstdata, iostat=istat)
  ios = ios + istat

  rewind(lun)
  file_intv_srf = 0
  srf_base_dir  = 'no_dir' 
  sat_base_dir  = 'no_dir' 
  sph_base_dir  = 'no_dir' 
  wind_base_dir = 'no_dir' 
  slp_base_dir  = 'no_dir' 

  read(lun, nml=nml_srfdata, iostat=istat)
  ios = ios + istat

  rewind(lun)
  read(lun, nml=nml_diagbulkflx, iostat=istat)
  ios = ios + istat

  if ( ios /= 0 ) then
    write(6,*) ' Read error : namelist.diagbulkflx', ios
    stop
  end if

  close(lun)

  !----------------------------------------------------------------------

  call libmxe_para__register(sstp,file_namelist=file_namelist_sst)
  call libmxe_grid__register(sstg,sstp)
  call libmxe_topo__register(sstt,sstp)
  call libmxe_topo__aexl(sstt,sstp)
  imt = sstp%imut
  jmt = sstp%jmut
  kmt = sstp%km
  allocate(alond(1:imt), alatd(1:jmt))
  alond(1:imt) = sstg%lonu(1:imt)
  alatd(1:jmt) = sstg%latu(1:jmt)
  allocate(aexl(1:imt,1:jmt))
  aexl(1:imt,1:jmt) = sstt%aexl(1:imt,1:jmt,1)

  call force_process__ini(l_leap_valid)

  allocate(sst(1:imt,1:jmt))
  allocate(ice(1:imt,1:jmt))

  allocate(wsx(1:imt,1:jmt), wsy(1:imt,1:jmt))
  allocate(qla(1:imt,1:jmt), qsn(1:imt,1:jmt))
  allocate(evp(1:imt,1:jmt))
  allocate(satu(1:imt,1:jmt), sphu(1:imt,1:jmt))
  allocate(dtu(1:imt,1:jmt))
  allocate(dqu(1:imt,1:jmt))
  allocate(u10n(1:imt,1:jmt))
  allocate(v10n(1:imt,1:jmt))
  allocate(w10n(1:imt,1:jmt))
  u10n(1:imt,1:jmt) = 0.0d0
  v10n(1:imt,1:jmt) = 0.0d0
  w10n(1:imt,1:jmt) = 0.0d0

  allocate(num_hist(1:imt,1:jmt))
  allocate(wsxm(1:imt,1:jmt), wsym(1:imt,1:jmt))
  allocate(qlam(1:imt,1:jmt), qsnm(1:imt,1:jmt))
  allocate(evpm(1:imt,1:jmt))
  allocate(u10nm(1:imt,1:jmt))
  allocate(v10nm(1:imt,1:jmt))
  allocate(w10nm(1:imt,1:jmt))
  
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
       &  undef_sst )

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
       &  undef_sst )

  !------

  call libmxe_para__register(srfp,file_namelist=file_namelist_srf)
  call libmxe_grid__register(srfg,srfp)
  imf = srfp%imut
  jmf = srfp%jmut
  kmf = srfp%km
  allocate(alonf(1:imf), alatf(1:jmf))
  alonf(1:imf) = srfg%lonu(1:imf)
  alatf(1:jmf) = srfg%latu(1:jmf)

  allocate(usf(1:imf,1:jmf), vsf(1:imf,1:jmf))
  allocate(satf(1:imf,1:jmf), sphf(1:imf,1:jmf))
  allocate(slpf(1:imf,1:jmf))
  allocate(wdvf(1:imf,1:jmf))

  item_name='SAT'
  call register_file( &
       &  item_name, &
       &  sat_file,  &
       &  fin_base_sat,  &
       &  file_first_srf, &
       &  file_intv_type_srf, &
       &  file_intv_srf, &
       &  total_rec_srf, &
       &  data_intv_sec_srf, &
       &  srf_first, &
       &  imf, jmf,  &
       &  undef_srf )

  item_name='SPH'
  call register_file( &
       &  item_name, &
       &  sph_file,  &
       &  fin_base_sph,  &
       &  file_first_srf, &
       &  file_intv_type_srf, &
       &  file_intv_srf, &
       &  total_rec_srf, &
       &  data_intv_sec_srf, &
       &  srf_first, &
       &  imf, jmf,  &
       &  undef_srf )

  item_name='SLP'
  call register_file( &
       &  item_name, &
       &  slp_file,  &
       &  fin_base_slp,  &
       &  file_first_srf, &
       &  file_intv_type_srf, &
       &  file_intv_srf, &
       &  total_rec_srf, &
       &  data_intv_sec_srf, &
       &  srf_first, &
       &  imf, jmf,  &
       &  undef_srf )

  item_name='U10'
  call register_file( &
       &  item_name, &
       &  us_file,  &
       &  fin_base_us,  &
       &  file_first_srf, &
       &  file_intv_type_srf, &
       &  file_intv_srf, &
       &  total_rec_srf, &
       &  data_intv_sec_srf, &
       &  srf_first, &
       &  imf, jmf,  &
       &  undef_srf )

  item_name='V10'
  call register_file( &
       &  item_name, &
       &  vs_file,  &
       &  fin_base_vs,  &
       &  file_first_srf, &
       &  file_intv_type_srf, &
       &  file_intv_srf, &
       &  total_rec_srf, &
       &  data_intv_sec_srf, &
       &  srf_first, &
       &  imf, jmf,  &
       &  undef_srf )

  !-------------------------------------------------

  area_earth_orgdata = 0.0d0
  do j = 1, jmt
    do i = 1, imt
      area_earth_orgdata = area_earth_orgdata &
           & + sstg%a_br(i,j) + sstg%a_bl(i,j) &
           & + sstg%a_tr(i,j) + sstg%a_tl(i,j)
    end do
  end do

  write(6,*) ' Area of the Earth (original data) ', area_earth_orgdata
  write(6,*) ' Area of the Earth (theoretical)   ' &
                & , 4.0d0 * pi * radius * radius

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

  sst_dir = trim(sst_base_dir)
  call get_first_two_data(current_date,sst_file,sst_dir,l_ymdir_sst)
  call get_first_two_data(current_date,ice_file,sst_dir,l_ymdir_sst)

  if (trim(sat_base_dir) /= 'no_dir') then
    srf_dir = trim(sat_base_dir)
  else
    srf_dir = trim(srf_base_dir)
  end if
  call get_first_two_data(current_date,sat_file,srf_dir,l_ymdir_srf)

  if (trim(sph_base_dir) /= 'no_dir') then
    srf_dir = trim(sph_base_dir)
  else
    srf_dir = trim(srf_base_dir)
  end if
  call get_first_two_data(current_date,sph_file,srf_dir,l_ymdir_srf)

  if (trim(slp_base_dir) /= 'no_dir') then
    srf_dir = trim(slp_base_dir)
  else
    srf_dir = trim(srf_base_dir)
  end if
  call get_first_two_data(current_date,slp_file,srf_dir,l_ymdir_srf)

  if (trim(wind_base_dir) /= 'no_dir') then
    srf_dir = trim(wind_base_dir)
  else
    srf_dir = trim(srf_base_dir)
  end if
  call get_first_two_data(current_date,us_file,srf_dir,l_ymdir_srf)
  call get_first_two_data(current_date,vs_file,srf_dir,l_ymdir_srf)

  nstep = 0
  num_hist(:,:) = 0
  wsxm (:,:) = 0.0d0
  wsym (:,:) = 0.0d0
  qlam (:,:) = 0.0d0
  qsnm (:,:) = 0.0d0
  evpm (:,:) = 0.0d0
  u10nm (:,:) = 0.0d0
  v10nm (:,:) = 0.0d0
  w10nm (:,:) = 0.0d0

  write(6,*) 
  write(6,*) ' MAIN LOOP START '

  LOOP_SURF_FORCE: do while ( .true. )

    write(6,*) ' This step ', current_date%year, current_date%month, current_date%day, &
         & current_date%hour, current_date%minute, data_intv_sec_srf
    nstep = nstep + 1

    sst_dir = trim(sst_base_dir)
    call update_data(current_date,sst_file,sst_dir,l_ymdir_sst)
    call update_data(current_date,ice_file,sst_dir,l_ymdir_sst)
    
    do j = 1, jmt
      do i = 1, imt
        sst(i,j) = sst_file%dat_b(i,j) * sst_file%br + sst_file%dat_a(i,j) * sst_file%ar - tab
        ice(i,j) = ice_file%dat_b(i,j) * ice_file%br + ice_file%dat_a(i,j) * ice_file%ar
        if ((sst(i,j) < -2.0d0) .and. (ice(i,j) > 0.0d0)) then
          sst(i,j) = sst_freeze
        end if
        sst(i,j) = sst(i,j) * aexl(i,j)
        ice(i,j) = ice(i,j) * aexl(i,j)
        if ((ice(i,j) < 0.0d0) .or. (ice(i,j) > 1.0d0)) then
          ice(i,j) = max(ice(i,j),0.0d0)
          ice(i,j) = min(ice(i,j),1.0d0)
        end if
      end do
    end do

    if (trim(sat_base_dir) /= 'no_dir') then
      srf_dir = trim(sat_base_dir)
    else
      srf_dir = trim(srf_base_dir)
    end if
    call update_data(current_date,sat_file,srf_dir,l_ymdir_srf)

    if (trim(sph_base_dir) /= 'no_dir') then
      srf_dir = trim(sph_base_dir)
    else
      srf_dir = trim(srf_base_dir)
    end if
    call update_data(current_date,sph_file,srf_dir,l_ymdir_srf)

    if (trim(slp_base_dir) /= 'no_dir') then
      srf_dir = trim(slp_base_dir)
    else
      srf_dir = trim(srf_base_dir)
    end if
    call update_data(current_date,slp_file,srf_dir,l_ymdir_srf)

    if (trim(wind_base_dir) /= 'no_dir') then
      srf_dir = trim(wind_base_dir)
    else
      srf_dir = trim(srf_base_dir)
    end if
    call update_data(current_date,us_file,srf_dir,l_ymdir_srf)
    call update_data(current_date,vs_file,srf_dir,l_ymdir_srf)

    do j = 1, jmf
      do i = 1, imf
        satf(i,j) = (sat_file%dat_b(i,j) * sat_file%br + sat_file%dat_a(i,j) * sat_file%ar) - tab
        sphf(i,j) =  sph_file%dat_b(i,j) * sph_file%br + sph_file%dat_a(i,j) * sph_file%ar 
        slpf(i,j) = (slp_file%dat_b(i,j) * slp_file%br + slp_file%dat_a(i,j) * slp_file%ar) * 1.0d-2 ! [hPa]
        usf (i,j) = us_file%dat_b(i,j) * us_file%br + us_file%dat_a(i,j) * us_file%ar
        vsf (i,j) = vs_file%dat_b(i,j) * vs_file%br + vs_file%dat_a(i,j) * vs_file%ar
        wdvf(i,j) = sqrt(us_file%dat_b(i,j) ** 2 + vs_file%dat_b(i,j)**2) * us_file%br &
             &    + sqrt(us_file%dat_a(i,j) ** 2 + vs_file%dat_a(i,j)**2) * us_file%ar 
      end do
    end do

    !do j = 1, jmt
    !  do i = 1, imt
    !    wdvf(i,j) = sqrt(usf(i,j)**2 + vsf(i,j)**2)
    !  end do
    !end do

    !------

    call bulk(wsx,wsy,qla,qsn,evp,&
         &    satu,sphu,dtu,dqu,w10n,&
         &    usf,vsf,satf,sphf,wdvf,slpf,sst, &
         &    imt,jmt,aexl,alt_wind,alt_sat,alt_sph)
    
    !------

    do j = 1, jmt
      do i = 1, imt
        if (aexl(i,j) == 1.0d0) then
          num_hist(i,j) = num_hist(i,j) + 1
          hl1 = 1.0d0 - ice(i,j)
          wsxm(i,j) = wsxm(i,j) + wsx(i,j)
          wsym(i,j) = wsym(i,j) + wsy(i,j)
          qlam(i,j) = qlam(i,j) + qla(i,j) * hl1
          qsnm(i,j) = qsnm(i,j) + qsn(i,j) * hl1
          evpm(i,j) = evpm(i,j) + evp(i,j) * hl1
          u10nm(i,j) = u10nm(i,j) + usf(i,j) * w10n(i,j) / max(wdvf(i,j),0.3d0) ! 0.3 [m/s] floor on wind
          v10nm(i,j) = v10nm(i,j) + vsf(i,j) * w10n(i,j) / max(wdvf(i,j),0.3d0) ! 0.3 [m/s] floor on wind
          w10nm(i,j) = w10nm(i,j) + w10n(i,j)
        end if
      end do
    end do

    !---------------------

    next_date = libmxe_calendar__addsec(current_date, data_intv_sec_srf, l_use_leap)

    !---------------------

    l_histout = .false.

    if (int_hist == -1) then
      if (current_date%month /= next_date%month) then
        l_histout = .true.
      end if
    else
      if (mod(nstep,int_hist) == 0) then
        l_histout = .true.
      end if
    end if
    
    if (l_histout) then
      do j = 1, jmt
        do i = 1, imt
          if (num_hist(i,j) > 0) then
            wsxm (i,j) = wsxm (i,j) / real(num_hist(i,j),8)
            wsym (i,j) = wsym (i,j) / real(num_hist(i,j),8)
            qlam (i,j) = qlam (i,j) / real(num_hist(i,j),8)
            qsnm (i,j) = qsnm (i,j) / real(num_hist(i,j),8)
            evpm (i,j) = evpm (i,j) / real(num_hist(i,j),8)
            u10nm(i,j) = u10nm(i,j) / real(num_hist(i,j),8)
            v10nm(i,j) = v10nm(i,j) / real(num_hist(i,j),8)
            w10nm(i,j) = w10nm(i,j) / real(num_hist(i,j),8)
          else
            wsxm (i,j) = real(undef_srf,8)
            wsym (i,j) = real(undef_srf,8)
            qlam (i,j) = real(undef_srf,8)
            qsnm (i,j) = real(undef_srf,8)
            evpm (i,j) = real(undef_srf,8)
            u10nm(i,j) = real(undef_srf,8)
            v10nm(i,j) = real(undef_srf,8)
            w10nm(i,j) = real(undef_srf,8)
          end if
        end do
      end do

      ! output data

      lreclen = 4 * imt * jmt

      write(flx_dir,'(1a)') trim(flx_base_dir)

      if (int_hist == -1) then
        write(fout_taux,'(1a,1a,1a,1a,i4.4,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_taux),'.',current_date%year, current_date%month
      else
        write(fout_taux,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_taux),'.',&
             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
      end if
      call open_file_direct(mtot1, fout_taux, lreclen)
      write(mtot1,rec=1) real(wsxm(1:imt,1:jmt),4)
      call close_file(mtot1)

      if (int_hist == -1) then
        write(fout_tauy,'(1a,1a,1a,1a,i4.4,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_tauy),'.',current_date%year, current_date%month
      else
        write(fout_tauy,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_tauy),'.', &
             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
      end if
      call open_file_direct(mtot1, fout_tauy, lreclen)
      write(mtot1,rec=1) real(wsym(1:imt,1:jmt),4)
      call close_file(mtot1)

      if (int_hist == -1) then
        write(fout_latent,'(1a,1a,1a,1a,i4.4,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_latent),'.',current_date%year, current_date%month
      else
        write(fout_latent,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
           & trim(flx_dir),'/',trim(fout_base_latent),'.',&
           & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
      end if
      call open_file_direct(mtot1, fout_latent, lreclen)
      write(mtot1,rec=1) real(qlam(1:imt,1:jmt),4)
      call close_file(mtot1)

      if (int_hist == -1) then
        write(fout_sens,'(1a,1a,1a,1a,i4.4,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_sens),'.',current_date%year, current_date%month
      else
        write(fout_sens,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_sens),'.',&
             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
      end if
      call open_file_direct(mtot1, fout_sens, lreclen)
      write(mtot1,rec=1) real(qsnm(1:imt,1:jmt),4)
      call close_file(mtot1)
      
      if (int_hist == -1) then
        write(fout_evapor,'(1a,1a,1a,1a,i4.4,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_evapor),'.',current_date%year, current_date%month
      else
        write(fout_evapor,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_evapor),'.',&
             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
      end if
      call open_file_direct(mtot1, fout_evapor, lreclen)
      write(mtot1,rec=1) real(evpm(1:imt,1:jmt),4)
      call close_file(mtot1)

      if (int_hist == -1) then
        write(fout_u10n,'(1a,1a,1a,1a,i4.4,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_u10n),'.',current_date%year, current_date%month
      else
        write(fout_u10n,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_u10n),'.',&
             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
      end if
      call open_file_direct(mtot1, fout_u10n, lreclen)
      write(mtot1,rec=1) real(u10nm(1:imt,1:jmt),4)
      call close_file(mtot1)

      if (int_hist == -1) then
        write(fout_v10n,'(1a,1a,1a,1a,i4.4,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_v10n),'.',current_date%year, current_date%month
      else
        write(fout_v10n,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_v10n),'.',&
             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
      end if
      call open_file_direct(mtot1, fout_v10n, lreclen)
      write(mtot1,rec=1) real(v10nm(1:imt,1:jmt),4)
      call close_file(mtot1)

      if (int_hist == -1) then
        write(fout_w10n,'(1a,1a,1a,1a,i4.4,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_w10n),'.',current_date%year, current_date%month
      else
        write(fout_w10n,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_w10n),'.',&
             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
      end if
      call open_file_direct(mtot1, fout_w10n, lreclen)
      write(mtot1,rec=1) real(w10nm(1:imt,1:jmt),4)
      call close_file(mtot1)

      num_hist(:,:) = 0
      wsxm(:,:) = 0.0d0
      wsym(:,:) = 0.0d0
      qlam(:,:) = 0.0d0
      qsnm(:,:) = 0.0d0
      evpm(:,:) = 0.0d0
      w10nm(:,:) = 0.0d0

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

  end do LOOP_SURF_FORCE

  write(6,*) ' MAIN LOOP END '

end program diagnosis_bulk_flux
