! -*-F90-*-
!----------- shift_surf_interannual_ocean.F90 ------------------
!  Information:
!     Diangnose heat flux using SST and Surface Atmospheric State
!-------------------------------------------------------------------
program shift_surf_interannual_ocean

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
  use mapping_table

  implicit none

  ! [cgs]

  real(8), parameter :: ro = rho
  real(8), parameter :: ems = 1.00d0
  real(8), parameter :: stfblz = 5.67d-5
  real(8), parameter :: tab = 273.15d0
  real(8), parameter :: sst_freeze = -1.8d0
  real(8), parameter :: sst_bad = 40.0d0

  real(8), parameter :: sph_min = 2.0d-5

  !------------

  character(len=16) :: item_name

  type(type_force) :: sst_file
  type(type_force) :: ice_file

  type(type_force) :: sat_file
  type(type_force) :: sph_file
  type(type_force) :: slp_file
  type(type_force) :: us_file
  type(type_force) :: vs_file

  type(type_force) :: brt_file

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
  real(8),allocatable :: area_recv(:,:)

  ! Brightness
  !
  !integer(4) :: imb, jmb, kmb
  !real(8),allocatable :: alonb(:), alatb(:)
  !real(8),allocatable :: britb(:,:)
  !
  ! SST

  integer(4) :: imt, jmt, kmt
  real(8),allocatable :: alond(:), alatd(:)

  real(8),allocatable :: aexl(:,:)
  real(8),allocatable :: sst(:,:)
  real(8),allocatable :: ice(:,:)

  real(8),allocatable :: us (:,:), vs (:,:)
  real(8),allocatable :: sat(:,:), sph(:,:)
  real(8),allocatable :: slp(:,:), wdv(:,:)
  !real(8),allocatable :: brit(:,:)

  ! following items are diagnosed in this program

  integer(4) :: int_hist
  logical :: l_histout
  real(8),allocatable :: sattrg(:,:), sphtrg(:,:)

  integer(4),allocatable :: num_hist_srf(:,:)
  integer(4),allocatable :: num_hist_btm(:,:)
  real(8),allocatable :: britm(:,:)
  real(8),allocatable :: sattrgm(:,:), sphtrgm(:,:)

  !-----------------------------------------------

  integer(4) :: mtot1

  !-----------------------------------------------
  ! SST data

  character(256) :: sst_base_dir, sst_dir
  character(256) :: fin_base_sst, fin_base_ice
  character(256) :: fin_sst
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
  real(8)    :: alt_target

  !------------------
  ! Brightness data

  character(256) :: brt_base_dir, brt_dir
  character(256) :: fin_base_brt

  character(256) :: fin_brt

  integer(4) :: file_first_brt(6)
  integer(4) :: file_intv_type_brt
  integer(4) :: file_intv_brt
  integer(4) :: total_rec_brt
  integer(4) :: data_intv_sec_brt
  integer(4) :: brt_first(6)
  real(4)    :: undef_brt
  logical    :: l_ymdir_brt

  !------------------
  ! Output

  character(256) :: out_base_dir, out_dir
  character(256) :: fout_base_bright
  character(256) :: fout_base_sattrg
  character(256) :: fout_base_sphtrg


  character(256) :: fout_bright
  character(256) :: fout_sattrg
  character(256) :: fout_sphtrg

  !------------------

  logical :: l_leap_valid

  integer(4) :: nstep
  integer(4) :: calc_intv_sec

  integer(4) :: lreclen

  integer(4) :: i, j, k, l, m, n

  !----------------------------------------------------------------------

  type(type_libmxe_para) :: srfp, sstp
  type(type_libmxe_grid) :: srfg, sstg
  type(type_libmxe_topo) :: sstt
!  type(type_libmxe_para) :: brtp
!  type(type_libmxe_grid) :: brtg
  character(256) :: file_namelist_sst, file_namelist_srf
!  character(256) :: file_namelist_brt

  integer(4),parameter :: lun = 10

  integer(4) :: ios, istat

  real(8) :: area_earth_orgdata
  real(8) :: hl1

  !--------------------------------------------------------------------
  ! Mapping table to SST grid

  type(type_table) :: srfo2sst, srfa2sst
  character(len=256) :: file_table_ocn_srf, file_table_all_srf
  logical :: l_need_table_srf
  logical :: l_lin_wind, l_lin_sat, l_lin_sph, l_lin_slp
  logical :: l_ocn_wind, l_ocn_sat, l_ocn_sph, l_ocn_slp

  !type(type_table) :: btmo2sst, btma2sst
  !character(len=256) :: file_table_ocn_btm, file_table_all_btm
  !logical :: l_need_table_btm
  !logical :: l_lin_brit
  !logical :: l_ocn_brit

  character(len=256) :: name_tmp

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

!  namelist /nml_brtdata/ file_namelist_brt, &
!       &  brt_base_dir, &
!       &  fin_base_brt, &
!       &  file_first_brt, &
!       &  file_intv_type_brt, file_intv_brt, total_rec_brt, data_intv_sec_brt, &
!       &  brt_first, undef_brt, l_ymdir_brt

  namelist /nml_shiftsurf/ calc_start, calc_end, &
       &  l_leap_valid, &
       &  calc_intv_sec, &
       &  alt_target, &
       &  int_hist, &
       &  out_base_dir, &
       &  fout_base_bright, &
       &  fout_base_sattrg, fout_base_sphtrg

  namelist /nml_intpol_srf/ file_table_ocn_srf, file_table_all_srf, &
       &                l_lin_wind, l_lin_sat, l_lin_sph, l_lin_slp, &
       &                l_ocn_wind, l_ocn_sat, l_ocn_sph, l_ocn_slp

!  namelist /nml_intpol_btm/ file_table_ocn_btm, file_table_all_btm, &
!       &                l_lin_brit, l_ocn_brit

  !-----------------------------------------------------------------------
  ! factor to convert input data to MKS

  open(lun,file='namelist.shiftsurf',iostat=istat,status='old')

  if ( istat /= 0 ) then
    write(6,*) ' Cannot find file : namelist.shiftsurf'
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

!  rewind(lun)
!  file_intv_brt = 0
!  read(lun, nml=nml_brtdata, iostat=istat)
!  ios = ios + istat

  rewind(lun)
  read(lun, nml=nml_shiftsurf, iostat=istat)
  ios = ios + istat

  file_table_ocn_srf='no_file'
  file_table_all_srf='no_file'

  rewind(lun)
  read(lun, nml=nml_intpol_srf, iostat=istat)
  ios = ios + istat

!  file_table_ocn_btm='no_file'
!  file_table_all_btm='no_file'
!
!  rewind(lun)
!  read(lun, nml=nml_intpol_btm, iostat=istat)
!  ios = ios + istat

  if ( ios /= 0 ) then
    write(6,*) ' Read error : namelist.shiftsurf', ios
    stop
  end if

  close(lun)

  if (l_lin_wind .and. l_lin_sat .and. l_lin_sph .and. l_lin_slp) then
    l_need_table_srf = .false.
  else
    l_need_table_srf = .true.
  end if

!  if (l_lin_brit) then
!    l_need_table_btm = .false.
!  else
!    l_need_table_btm = .true.
!  end if

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

  allocate(area_recv(1:imt,1:jmt))

  allocate(us (1:imt,1:jmt), vs (1:imt,1:jmt))
  allocate(sat(1:imt,1:jmt), sph(1:imt,1:jmt))
  allocate(slp(1:imt,1:jmt), wdv(1:imt,1:jmt))

!  allocate(brit(1:imt,1:jmt))
  allocate(sattrg(1:imt,1:jmt))
  allocate(sphtrg(1:imt,1:jmt))

  allocate(num_hist_srf(1:imt,1:jmt))
  allocate(num_hist_btm(1:imt,1:jmt))
  allocate(britm(1:imt,1:jmt))
  allocate(sattrgm(1:imt,1:jmt),sphtrgm(1:imt,1:jmt))
  
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

  !---------------
  ! surface data

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

  !-----------------------------------------------------------
  ! Brightness
!
!  call libmxe_para__register(brtp,file_namelist=file_namelist_brt)
!  call libmxe_grid__register(brtg,brtp)
!  imb = brtp%imut
!  jmb = brtp%jmut
!  kmb = brtp%km
!  allocate(alonb(1:imb), alatb(1:jmb))
!  alonb(1:imb) = brtg%lonu(1:imb)
!  alatb(1:jmb) = brtg%latu(1:jmb)
!
!  allocate(britb(1:imb,1:jmb))
!
!  item_name='BRT'
!  call register_file( &
!       &  item_name, &
!       &  brt_file,  &
!       &  fin_base_brt,  &
!       &  file_first_brt, &
!       &  file_intv_type_brt, &
!       &  file_intv_brt, &
!       &  total_rec_brt, &
!       &  data_intv_sec_brt, &
!       &  brt_first, &
!       &  imb, jmb,  &
!       &  undef_brt )
!
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
  ! mapping table

  if (l_need_table_srf) then
    if (file_table_ocn_srf /= 'no_file') then
      name_tmp = "Ocean to Ocean table"
      call mapping_table__ini(  &
           & name_tmp, &
           & file_table_ocn_srf, srfo2sst,   &
           & imf, jmf, kmf, &
           & imt, jmt, kmt  &
           & )
    end if
    if (file_table_all_srf /= 'no_file') then
      name_tmp = "All to All table"
      call mapping_table__ini(  &
           & name_tmp,          &
           & file_table_all_srf, srfa2sst,   &
           & imf, jmf, kmf, &
           & imt, jmt, kmt  &
           & )
    end if
  end if

!  if (l_need_table_btm) then
!    if (file_table_ocn_btm /= 'no_file') then
!      name_tmp = "Ocean to Ocean table"
!      call mapping_table__ini(  &
!           & name_tmp, &
!           & file_table_ocn_btm, btmo2sst,   &
!           & imb, jmb, kmb, &
!           & imt, jmt, kmt  &
!           & )
!    end if
!    if (file_table_all_btm /= 'no_file') then
!      name_tmp = "All to All table"
!      call mapping_table__ini(  &
!           & name_tmp,          &
!           & file_table_all_btm, btma2sst,   &
!           & imb, jmb, kmb, &
!           & imt, jmt, kmt  &
!           & )
!    end if
!  end if

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

!  brt_dir = trim(brt_base_dir)
!  call get_first_two_data(current_date,brt_file,brt_dir,l_ymdir_brt)

  nstep = 0
  num_hist_srf(:,:) = 0
  num_hist_btm(:,:) = 0

  britm(:,:) = 0.0d0

  sattrgm(:,:) = 0.0d0
  sphtrgm(:,:) = 0.0d0

  write(6,*) 
  write(6,*) ' MAIN LOOP START '

  LOOP_SURF_FORCE: do while ( .true. )

    write(6,*) ' This step ', current_date%year, current_date%month, current_date%day, &
         & current_date%hour, current_date%minute, calc_intv_sec
    nstep = nstep + 1

    sst_dir = trim(sst_base_dir)
    call update_data(current_date,sst_file,sst_dir,l_ymdir_sst)
    call update_data(current_date,ice_file,sst_dir,l_ymdir_sst)
    
    do j = 1, jmt
      do i = 1, imt
        sst(i,j) = sst_file%dat_b(i,j) * sst_file%br + sst_file%dat_a(i,j) * sst_file%ar - tab
        ice(i,j) = ice_file%dat_b(i,j) * ice_file%br + ice_file%dat_a(i,j) * ice_file%ar
        !if ((sst(i,j) < -2.0d0) .and. (ice(i,j) > 0.0d0)) then
        !  sst(i,j) = sst_freeze
        !end if
        !if ((sst(i,j) > sst_bad) .or. (sst(i,j) < -sst_bad)) then
        !  aexl(i,j) = 0.0d0
        !else
        !  aexl(i,j) = 1.0d0
        !end if
        !sst(i,j) = sst(i,j) * aexl(i,j)
        !ice(i,j) = ice(i,j) * aexl(i,j)
        if ((ice(i,j) < 0.0d0) .or. (ice(i,j) > 1.0d0)) then
          !write(6,*) ' WARNING: ice data is erroneous ', i, j, ice(i,j), aexl(i,j)
          !write(6,*) ' b ', ice_file%dat_b(i,j)
          !write(6,*) ' a ', ice_file%dat_a(i,j)
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

!    brt_dir = trim(brt_base_dir)
!    call update_data(current_date,brt_file,brt_dir,l_ymdir_brt)

    do j = 1, jmf
      do i = 1, imf
        satf(i,j) = (sat_file%dat_b(i,j) * sat_file%br + sat_file%dat_a(i,j) * sat_file%ar) - tab
        sphf(i,j) =  sph_file%dat_b(i,j) * sph_file%br + sph_file%dat_a(i,j) * sph_file%ar 
        sphf(i,j) = max(sphf(i,j),sph_min)
        slpf(i,j) = (slp_file%dat_b(i,j) * slp_file%br + slp_file%dat_a(i,j) * slp_file%ar) * 1.0d-2 ! [Pa] -> [hPa]
        usf (i,j) = us_file%dat_b(i,j) * us_file%br + us_file%dat_a(i,j) * us_file%ar
        vsf (i,j) = vs_file%dat_b(i,j) * vs_file%br + vs_file%dat_a(i,j) * vs_file%ar
      end do
    end do

!    do j = 1, jmb
!      do i = 1, imb
!        britb(i,j) = (brt_file%dat_b(i,j) * brt_file%br + brt_file%dat_a(i,j) * brt_file%ar) - tab
!      end do
!    end do

    if (l_lin_wind) then
      call hintpl(us,imt,jmt,alond,alatd,usf,imf,jmf,alonf,alatf)
    else
      if (l_ocn_wind) then
        call mapping_table__main(         &
             & srfo2sst,                  &
             & us,area_recv,imt,jmt,kmt,  &
             & usf,imf,jmf,kmf,us_file%dundef, &
             & 1, imf, 1, jmf             &
             & )
      else
        call mapping_table__main(         &
             & srfa2sst,                  &
             & us,area_recv,imt,jmt,kmt,  &
             & usf,imf,jmf,kmf,us_file%dundef, &
             & 1, imf, 1, jmf             &
             & )
      end if
      do j = 1, jmt
        do i = 1, imt
          if (area_recv(i,j) == 0.0d0) then
            us(i,j) = us_file%dundef
          end if
        end do
      end do
    end if

    if (l_lin_wind) then
      call hintpl(vs,imt,jmt,alond,alatd,vsf,imf,jmf,alonf,alatf)
    else
      if (l_ocn_wind) then
        call mapping_table__main(         &
             & srfo2sst,                  &
             & vs,area_recv,imt,jmt,kmt,  &
             & vsf,imf,jmf,kmf,vs_file%dundef, &
             & 1, imf, 1, jmf             &
             & )
      else
        call mapping_table__main(         &
             & srfa2sst,                  &
             & vs,area_recv,imt,jmt,kmt,  &
             & vsf,imf,jmf,kmf,vs_file%dundef, &
             & 1, imf, 1, jmf             &
             & )
      end if
      do j = 1, jmt
        do i = 1, imt
          if (area_recv(i,j) == 0.0d0) then
            vs(i,j) = vs_file%dundef
          end if
        end do
      end do
    end if

    if (l_lin_sat) then
      call hintpl(sat,imt,jmt,alond,alatd,satf,imf,jmf,alonf,alatf)
    else
      if (l_ocn_sat) then
        call mapping_table__main(         &
             & srfo2sst,                  &
             & sat,area_recv,imt,jmt,kmt,  &
             & satf,imf,jmf,kmf,sat_file%dundef, &
             & 1, imf, 1, jmf             &
             & )
      else
        call mapping_table__main(         &
             & srfa2sst,                  &
             & sat,area_recv,imt,jmt,kmt,  &
             & satf,imf,jmf,kmf,sat_file%dundef, &
             & 1, imf, 1, jmf             &
             & )
      end if
      do j = 1, jmt
        do i = 1, imt
          if (area_recv(i,j) == 0.0d0) then
            sat(i,j) = sat_file%dundef
          end if
        end do
      end do
    end if

    if (l_lin_sph) then
      call hintpl(sph,imt,jmt,alond,alatd,sphf,imf,jmf,alonf,alatf)
    else
      if (l_ocn_sph) then
        call mapping_table__main(         &
             & srfo2sst,                  &
             & sph,area_recv,imt,jmt,kmt,  &
             & sphf,imf,jmf,kmf,sph_file%dundef, &
             & 1, imf, 1, jmf             &
             & )
      else
        call mapping_table__main(         &
             & srfa2sst,                  &
             & sph,area_recv,imt,jmt,kmt,  &
             & sphf,imf,jmf,kmf,sph_file%dundef, &
             & 1, imf, 1, jmf             &
             & )
      end if
      do j = 1, jmt
        do i = 1, imt
          if (area_recv(i,j) == 0.0d0) then
            sph(i,j) = sph_file%dundef
          end if
        end do
      end do
    end if

    if (l_lin_slp) then
      call hintpl(slp,imt,jmt,alond,alatd,slpf,imf,jmf,alonf,alatf)
    else
      if (l_ocn_slp) then
        call mapping_table__main(         &
             & srfo2sst,                  &
             & slp,area_recv,imt,jmt,kmt,  &
             & slpf,imf,jmf,kmf,slp_file%dundef, &
             & 1, imf, 1, jmf             &
             & )
      else
        call mapping_table__main(         &
             & srfa2sst,                  &
             & slp,area_recv,imt,jmt,kmt,  &
             & slpf,imf,jmf,kmf,slp_file%dundef, &
             & 1, imf, 1, jmf             &
             & )
      end if
      do j = 1, jmt
        do i = 1, imt
          if (area_recv(i,j) == 0.0d0) then
            slp(i,j) = slp_file%dundef
          end if
        end do
      end do
    end if

    do j = 1, jmt
      do i = 1, imt
        wdv(i,j) = sqrt(us(i,j)**2 + vs(i,j)**2)
      end do
    end do

!    if (l_lin_brit) then
!      call hintpl(brit,imt,jmt,alond,alatd,britb,imb,jmb,alonb,alatb)
!    else
!      if (l_ocn_brit) then
!        call mapping_table__main(          &
!             & btmo2sst,                   &
!             & brit,area_recv,imt,jmt,kmt, &
!             & britb,imb,jmb,kmb,brt_file%dundef, &
!             & 1, imb, 1, jmb              &
!             & )
!      else
!        call mapping_table__main(          &
!             & btma2sst,                   &
!             & brit,area_recv,imt,jmt,kmt, &
!             & britb,imb,jmb,kmb,brt_file%dundef, &
!             & 1, imb, 1, jmb              &
!             & )
!      end if
!    end if

    !------

    call bulk_shift(sattrg, sphtrg, &
         &    us,vs,sat,sph,wdv,slp,sst,ice, &
         &    imt,jmt,aexl,alt_wind,alt_sat,alt_sph,alt_target,sph_min)

    !------

    do j = 1, jmt
      do i = 1, imt

        sphtrg(i,j) = max(sphtrg(i,j),sph_min)

        num_hist_btm(i,j) = num_hist_btm(i,j) + 1
!        britm(i,j) = britm(i,j) + brit(i,j)
        britm(i,j) = britm(i,j) + sst(i,j)

        !if (aexl(i,j) == 1.0d0) then
        num_hist_srf(i,j) = num_hist_srf(i,j) + 1
        sattrgm(i,j) = sattrgm(i,j) + sattrg(i,j)
        sphtrgm(i,j) = sphtrgm(i,j) + sphtrg(i,j)
        !end if
      end do
    end do

    !---------------------

    next_date = libmxe_calendar__addsec(current_date, calc_intv_sec, l_use_leap)

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

          if (num_hist_btm(i,j) > 0) then
            britm(i,j) = britm(i,j) / real(num_hist_btm(i,j),8) + tab
          else
            britm(i,j)   = real(undef_sst,8)
          end if

          if (num_hist_srf(i,j) > 0) then
            sattrgm(i,j) = sattrgm(i,j) / real(num_hist_srf(i,j),8) + tab
            sphtrgm(i,j) = sphtrgm(i,j) / real(num_hist_srf(i,j),8)
          else
            sattrgm(i,j) = real(undef_srf,8)
            sphtrgm(i,j) = real(undef_srf,8)
          end if

        end do
      end do

      ! output data

      lreclen = 4 * imt * jmt

      write(out_dir,'(1a)') trim(out_base_dir)

      if (int_hist == -1) then
        write(fout_bright,'(1a,1a,1a,1a,i4.4,i2.2)') &
             & trim(out_dir),'/',trim(fout_base_bright),'.',current_date%year, current_date%month
      else
        write(fout_bright,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
             & trim(out_dir),'/',trim(fout_base_bright),'.',&
             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
      end if
      call open_file_direct(mtot1, fout_bright, lreclen)
      write(mtot1,rec=1) real(britm(1:imt,1:jmt),4)
      call close_file(mtot1)

      if (int_hist == -1) then
        write(fout_sattrg,'(1a,1a,1a,1a,i4.4,i2.2)') &
             & trim(out_dir),'/',trim(fout_base_sattrg),'.',current_date%year, current_date%month
      else
        write(fout_sattrg,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
             & trim(out_dir),'/',trim(fout_base_sattrg),'.',&
             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
      end if
      call open_file_direct(mtot1, fout_sattrg, lreclen)
      write(mtot1,rec=1) real(sattrgm(1:imt,1:jmt),4)
      call close_file(mtot1)

      if (int_hist == -1) then
        write(fout_sphtrg,'(1a,1a,1a,1a,i4.4,i2.2)') &
             & trim(out_dir),'/',trim(fout_base_sphtrg),'.',current_date%year, current_date%month
      else
        write(fout_sphtrg,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
             & trim(out_dir),'/',trim(fout_base_sphtrg),'.',&
             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
      end if
      call open_file_direct(mtot1, fout_sphtrg, lreclen)
      write(mtot1,rec=1) real(sphtrgm(1:imt,1:jmt),4)
      call close_file(mtot1)

      num_hist_btm(:,:) = 0
      num_hist_srf(:,:) = 0
      britm(:,:) = 0.0d0
      sattrgm(:,:) = 0.0d0
      sphtrgm(:,:) = 0.0d0

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

end program shift_surf_interannual_ocean
