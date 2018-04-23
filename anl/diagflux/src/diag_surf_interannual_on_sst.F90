! -*-F90-*-
!----------- diagflux_surf_interannual_on_sst.F90 ------------------
!  Information:
!     Diangnose Surface Atmospheric State on SST grid
!-------------------------------------------------------------------
program diagnosis_surface_atmospheric_state

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

  real(8), parameter :: rgas = 287.04d0

  real(8), parameter :: molw = 18.016d0
  real(8), parameter :: mola = 28.966d0
  real(8), parameter :: epsilon = molw/mola
  real(8), parameter :: r0 = 0.6078d0

  ! CORE (LY04)

  !real(8), parameter :: rhoa = 1.22d0 ! air density [kg/m3]
  !real(8), parameter :: q0 = 0.98d0       ! L-Y (5)
  !real(8), parameter :: q1a = 640380.d0   ! L-Y (5)
  !real(8), parameter :: q2a = -5107.4d0   ! L-Y (5)
  !
  !real(8), parameter :: q1i = 11637800.d0 ! L-Y p16
  !real(8), parameter :: q2i = -5897.8d0

  ! Gill (1982)

  real(8), parameter :: g1 = 0.7859d0
  real(8), parameter :: g2 = 0.03477d0
  real(8), parameter :: g3 = 0.00412d0

  real(8), parameter :: h1 = 4.5d0
  real(8), parameter :: h2 = 0.0006d0

  real(8), parameter :: i1 = 0.00422d0

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

  ! SST

  integer(4) :: imt, jmt, kmt
  real(8),allocatable :: alond(:), alatd(:)

  real(8),allocatable :: aexl(:,:)
  real(8),allocatable :: sst(:,:)
  real(8),allocatable :: ice(:,:)

  real(8),allocatable :: us (:,:), vs (:,:)
  real(8),allocatable :: sat(:,:), sph(:,:)
  real(8),allocatable :: slp(:,:), wdv(:,:)
  real(8),allocatable :: rlh(:,:), roa(:,:)
  real(8),allocatable :: area_recv(:,:)

  ! following items are diagnosed in this program

  integer(4) :: int_hist
  logical :: l_histout

  integer(4),allocatable :: num_hist_atm(:,:)
  integer(4),allocatable :: num_hist_wind(:,:)
  integer(4),allocatable :: num_hist_slp(:,:)

  real(8),allocatable :: usm (:,:), vsm (:,:)
  real(8),allocatable :: wdvm(:,:)
  real(8),allocatable :: satm(:,:), sphm(:,:)
  real(8),allocatable :: rlhm(:,:), roam(:,:)
  real(8),allocatable :: slpm(:,:)

  !-----------------------------------------------

  integer(4) :: mtot1

  !------------------
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

  !------------------
  ! Output

  character(256) :: flx_base_dir, flx_dir
  character(256) :: fout_base_xwind, fout_base_ywind
  character(256) :: fout_base_swind
  character(256) :: fout_base_sat
  character(256) :: fout_base_sph
  character(256) :: fout_base_rlh
  character(256) :: fout_base_roa
  character(256) :: fout_base_slp

  character(256) :: fout_xwind, fout_ywind
  character(256) :: fout_swind
  character(256) :: fout_sat
  character(256) :: fout_sph
  character(256) :: fout_rlh
  character(256) :: fout_roa
  character(256) :: fout_slp

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
  real(8) :: hl1, hl2, hl3
  real(8) :: qsat, esat

  !--------------------------------------------------------------------

  type(type_table) :: orgo2sst, orga2sst
  character(len=256) :: file_table_ocn, file_table_all
  character(len=256) :: name_tmp
  logical :: l_lin_wind, l_lin_sat, l_lin_sph, l_lin_slp
  logical :: l_need_table
  logical :: l_ocn_wind, l_ocn_sat, l_ocn_sph, l_ocn_slp

  logical :: l_proc_wind, l_proc_atm

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

  namelist /nml_diagsurf/ calc_start, calc_end, &
       &  l_leap_valid, &
       &  int_hist, &
       &  flx_base_dir, &
       &  l_proc_wind, l_proc_atm, &
       &  fout_base_xwind, fout_base_ywind, &
       &  fout_base_swind, &
       &  fout_base_sat, fout_base_sph, &
       &  fout_base_rlh, fout_base_roa, &
       &  fout_base_slp

  namelist /nml_table_ocn/ file_table_ocn

  namelist /nml_table_all/ file_table_all

  namelist /nml_intpol/ l_lin_wind, l_lin_sat, l_lin_sph, l_lin_slp, &
       &                l_ocn_wind, l_ocn_sat, l_ocn_sph, l_ocn_slp

  !-----------------------------------------------------------------------

  open(lun,file='namelist.diagsurf',iostat=istat,status='old')

  if ( istat /= 0 ) then
    write(6,*) ' Cannot find file : namelist.diagsurf'
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

  l_proc_wind=.true.
  l_proc_atm=.true.

  rewind(lun)
  read(lun, nml=nml_diagsurf, iostat=istat)
  ios = ios + istat

  rewind(lun)
  read(lun, nml=nml_intpol, iostat=istat)
  ios = ios + istat

  if ( ios /= 0 ) then
    write(6,*) ' Read error : namelist.diagsurf', ios
    stop
  end if

  close(lun)

  if (l_lin_wind .and. l_lin_sat .and. l_lin_sph .and. l_lin_slp) then
    l_need_table = .false.
  else
    l_need_table = .true.
  end if

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

  allocate(us (1:imt,1:jmt), vs (1:imt,1:jmt))
  allocate(sat(1:imt,1:jmt), sph(1:imt,1:jmt))
  allocate(slp(1:imt,1:jmt), wdv(1:imt,1:jmt))
  allocate(rlh(1:imt,1:jmt), roa(1:imt,1:jmt))
  allocate(area_recv(1:imt,1:jmt))

  allocate(num_hist_atm(1:imt,1:jmt))
  allocate(num_hist_slp(1:imt,1:jmt))
  allocate(num_hist_wind(1:imt,1:jmt))
  allocate(usm(1:imt,1:jmt), vsm(1:imt,1:jmt))
  allocate(wdvm(1:imt,1:jmt))
  allocate(satm(1:imt,1:jmt),sphm(1:imt,1:jmt))
  allocate(rlhm(1:imt,1:jmt),roam(1:imt,1:jmt))
  allocate(slpm(1:imt,1:jmt))
  
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

  if (l_need_table) then

    open(lun,file='namelist.diagsurf',iostat=istat,status='old')

    rewind(lun)
    read(lun, nml=nml_table_ocn, iostat=istat)
    if (istat == 0) then
      name_tmp = "Ocean to Ocean table"
      call mapping_table__ini(  &
           & name_tmp, &
           & file_table_ocn, orgo2sst,   &
           & imf, jmf, kmf, &
           & imt, jmt, kmt  &
           & )
    end if

    rewind(lun)
    read(lun, nml=nml_table_all, iostat=istat)
    if (istat == 0) then
      name_tmp = "All to All table"
      call mapping_table__ini(  &
           & name_tmp,          &
           & file_table_all, orga2sst,   &
           & imf, jmf, kmf, &
           & imt, jmt, kmt  &
           & )
    end if

    close(lun)

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

  if (l_proc_atm) then

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

  end if

  if (l_proc_wind) then
    if (trim(wind_base_dir) /= 'no_dir') then
      srf_dir = trim(wind_base_dir)
    else
      srf_dir = trim(srf_base_dir)
    end if
    call get_first_two_data(current_date,us_file,srf_dir,l_ymdir_srf)
    call get_first_two_data(current_date,vs_file,srf_dir,l_ymdir_srf)
  end if

  nstep = 0
  num_hist_atm(:,:) = 0
  num_hist_slp(:,:) = 0
  num_hist_wind(:,:) = 0
  usm (:,:) = 0.0d0
  vsm (:,:) = 0.0d0
  wdvm(:,:) = 0.0d0

  satm(:,:) = 0.0d0
  sphm(:,:) = 0.0d0

  rlhm(:,:) = 0.0d0
  roam(:,:) = 0.0d0

  slpm(:,:) = 0.0d0

  write(6,*) 
  write(6,*) ' MAIN LOOP START '

  LOOP_SURF_FORCE: do while ( .true. )

    write(6,*) ' This step ', current_date%year, current_date%month, current_date%day, &
         & current_date%hour, current_date%minute, data_intv_sec_srf
    nstep = nstep + 1

    if (l_proc_atm) then

      sst_dir = trim(sst_base_dir)
      call update_data(current_date,sst_file,sst_dir,l_ymdir_sst)
      call update_data(current_date,ice_file,sst_dir,l_ymdir_sst)
    
      do j = 1, jmt
        do i = 1, imt
          sst(i,j) = sst_file%dat_b(i,j) * sst_file%br + sst_file%dat_a(i,j) * sst_file%ar
          ice(i,j) = ice_file%dat_b(i,j) * ice_file%br + ice_file%dat_a(i,j) * ice_file%ar
          if ((sst(i,j) < -2.0d0) .and. (ice(i,j) > 0.0d0)) then
            sst(i,j) = sst_freeze
          end if
          !if ((sst(i,j) > sst_bad) .or. (sst(i,j) < -sst_bad)) then
          !  aexl(i,j) = 0.0d0
          !else
          !  aexl(i,j) = 1.0d0
          !end if
          sst(i,j) = sst(i,j) * aexl(i,j)
          ice(i,j) = ice(i,j) * aexl(i,j)
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

      do j = 1, jmf
        do i = 1, imf
          satf(i,j) = (sat_file%dat_b(i,j) * sat_file%br + sat_file%dat_a(i,j) * sat_file%ar) - tab
          sphf(i,j) =  sph_file%dat_b(i,j) * sph_file%br + sph_file%dat_a(i,j) * sph_file%ar 
          slpf(i,j) = (slp_file%dat_b(i,j) * slp_file%br + slp_file%dat_a(i,j) * slp_file%ar) * 1.0d-2
        end do
      end do

      if (l_lin_sat) then
        call hintpl(sat,imt,jmt,alond,alatd,satf,imf,jmf,alonf,alatf)
      else
        if (l_ocn_sat) then
          call mapping_table__main(         &
               & orgo2sst,                  &
               & sat,area_recv,imt,jmt,kmt,  &
               & satf,imf,jmf,kmf,sat_file%dundef, &
               & 1, imf, 1, jmf             &
               & )
        else
          call mapping_table__main(         &
               & orga2sst,                  &
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
               & orgo2sst,                  &
               & sph,area_recv,imt,jmt,kmt,  &
               & sphf,imf,jmf,kmf,sph_file%dundef, &
               & 1, imf, 1, jmf             &
               & )
        else
          call mapping_table__main(         &
               & orga2sst,                  &
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
               & orgo2sst,                  &
               & slp,area_recv,imt,jmt,kmt,  &
               & slpf,imf,jmf,kmf,slp_file%dundef, &
               & 1, imf, 1, jmf             &
               & )
        else
          call mapping_table__main(         &
               & orga2sst,                  &
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
          if ((aexl(i,j) == 1.0d0) .and. (slp(i,j) /= slp_file%dundef) .and. &
               & (sat(i,j) /= sat_file%dundef) .and. (sph(i,j) /= sph_file%dundef)) then
            roa(i,j) = slp(i,j) * 1.0d2 / rgas / (sat(i,j) + tab) &
                 & / (1.0d0 + r0 * sph(i,j))
            ! LY04
            !if (ice(i,j) < 0.5d0) then
            !  qsat = q0 * q1a * exp(q2a / (sat(i,j) + tab)) / rhoa  ! L-Y (5)
            !else
            !  qsat = q1i * exp(q2i / (sat(i,j) + tab)) / rhoa       ! L-Y p.16
            !end if
            ! Gill (1982)
            if (ice(i,j) > 0.0d0) then
              hl1 = (g1 + g2 * sat(i,j)) / (1.0d0 + g3 * sat(i,j)) + i1 * sat(i,j)
              hl2 = 10 ** hl1
              hl3 = 1.0d0 + 1.0d-6 * slp(i,j)* (h1 + h2 * sat(i,j)**2)
              esat = hl2 * hl3 
              qsat = epsilon * esat / (slp(i,j) - (1.0d0 - epsilon) * esat)
              !qsat = q1i * exp(q2i / (sat(i,j) + tab)) / rhoa
            else
              hl1 = (g1 + g2 * sat(i,j)) / (1.0d0 + g3 * sat(i,j))
              hl2 = 10 ** hl1
              hl3 = 1.0d0 + 1.0d-6 * slp(i,j) * (h1 + h2 * sat(i,j)**2)
              esat = hl2 * hl3
              qsat = epsilon * esat / (slp(i,j) - (1.0d0 - epsilon) * esat)
              !qsat = q0 * q1a * exp(q2a / (sat(i,j) + tab)) / rhoa 
            end if
            rlh(i,j) = sph(i,j) * (1.0d0 - qsat) / (qsat * (1.0d0 - sph(i,j)))
          end if
        end do
      end do

      do j = 1, jmt
        do i = 1, imt
          if ((aexl(i,j) == 1.0d0) .and. (sph(i,j) /= sph_file%dundef)  .and. (sat(i,j) /= sat_file%dundef)) then
            num_hist_atm(i,j) = num_hist_atm(i,j) + 1
            satm(i,j) = satm(i,j) + sat(i,j)
            sphm(i,j) = sphm(i,j) + sph(i,j)
            rlhm(i,j) = rlhm(i,j) + rlh(i,j)
            roam(i,j) = roam(i,j) + roa(i,j)
          end if
        end do
      end do

      do j = 1, jmt
        do i = 1, imt
          if (slp(i,j) /= slp_file%dundef) then
            num_hist_slp(i,j) = num_hist_slp(i,j) + 1
            slpm(i,j) = slpm(i,j) + slp(i,j)
          end if
        end do
      end do

    end if

    if (l_proc_wind) then

      if (trim(wind_base_dir) /= 'no_dir') then
        srf_dir = trim(wind_base_dir)
      else
        srf_dir = trim(srf_base_dir)
      end if
      call update_data(current_date,us_file,srf_dir,l_ymdir_srf)
      call update_data(current_date,vs_file,srf_dir,l_ymdir_srf)

      do j = 1, jmf
        do i = 1, imf
          usf (i,j) = (us_file%dat_b(i,j) * us_file%br + us_file%dat_a(i,j) * us_file%ar) * 1.0d2
          vsf (i,j) = (vs_file%dat_b(i,j) * vs_file%br + vs_file%dat_a(i,j) * vs_file%ar) * 1.0d2
        end do
      end do

      if (l_lin_wind) then
        call hintpl(us,imt,jmt,alond,alatd,usf,imf,jmf,alonf,alatf)
      else
        if (l_ocn_wind) then
          call mapping_table__main(         &
               & orgo2sst,                  &
               & us,area_recv,imt,jmt,kmt,  &
               & usf,imf,jmf,kmf,us_file%dundef, &
               & 1, imf, 1, jmf             &
               & )
        else
          call mapping_table__main(         &
               & orga2sst,                  &
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
               & orgo2sst,                  &
               & vs,area_recv,imt,jmt,kmt,  &
               & vsf,imf,jmf,kmf,vs_file%dundef, &
               & 1, imf, 1, jmf             &
               & )
        else
          call mapping_table__main(         &
               & orga2sst,                  &
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

      do j = 1, jmt
        do i = 1, imt
          if ((aexl(i,j) == 1.0d0) .and. (us(i,j) /= us_file%dundef) .and. (vs(i,j) /= vs_file%dundef)) then
            wdv(i,j) = sqrt(us(i,j)**2 + vs(i,j)**2)
          end if
        end do
      end do

      do j = 1, jmt
        do i = 1, imt
          if ((aexl(i,j) == 1.0d0) .and. (us(i,j) /= us_file%dundef)  .and. (vs(i,j) /= vs_file%dundef)) then
            num_hist_wind(i,j) = num_hist_wind(i,j) + 1
            usm(i,j) = usm(i,j) + us(i,j)
            vsm(i,j) = vsm(i,j) + vs(i,j)
            wdvm(i,j) = wdvm(i,j) + wdv(i,j)
          end if
        end do
      end do

    end if

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
          if (num_hist_wind(i,j) > 0) then
            usm (i,j) = usm (i,j) / real(num_hist_wind(i,j),8)
            vsm (i,j) = vsm (i,j) / real(num_hist_wind(i,j),8)
            wdvm(i,j) = wdvm(i,j) / real(num_hist_wind(i,j),8)
          else
            usm (i,j) = real(undef_srf,8)
            vsm (i,j) = real(undef_srf,8)
            wdvm(i,j) = real(undef_srf,8)
          end if
          if (num_hist_atm(i,j) > 0) then
            satm(i,j) = satm(i,j) / real(num_hist_atm(i,j),8)
            sphm(i,j) = sphm(i,j) / real(num_hist_atm(i,j),8)
            rlhm(i,j) = rlhm(i,j) / real(num_hist_atm(i,j),8)
            roam(i,j) = roam(i,j) / real(num_hist_atm(i,j),8)
          else
            satm(i,j) = real(undef_srf,8)
            sphm(i,j) = real(undef_srf,8)
            rlhm(i,j) = real(undef_srf,8)
            roam(i,j) = real(undef_srf,8)
          end if
          if (num_hist_slp(i,j) > 0) then
            slpm(i,j) = slpm(i,j) / real(num_hist_slp(i,j),8)
          else
            slpm(i,j) = real(undef_srf,8)
          end if
        end do
      end do

      ! output data

      lreclen = 4 * imt * jmt

      write(flx_dir,'(1a)') trim(flx_base_dir)

      if (l_proc_wind) then
        if (int_hist == -1) then
          write(fout_xwind,'(1a,1a,1a,1a,i4.4,i2.2)') &
               & trim(flx_dir),'/',trim(fout_base_xwind),'.',current_date%year, current_date%month
        else
          write(fout_xwind,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
               & trim(flx_dir),'/',trim(fout_base_xwind),'.',&
               & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
        end if
        call open_file_direct(mtot1, fout_xwind, lreclen)
        write(mtot1,rec=1) real(usm(1:imt,1:jmt),4)
        call close_file(mtot1)

        if (int_hist == -1) then
          write(fout_ywind,'(1a,1a,1a,1a,i4.4,i2.2)') &
               & trim(flx_dir),'/',trim(fout_base_ywind),'.',current_date%year, current_date%month
        else
          write(fout_ywind,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
               & trim(flx_dir),'/',trim(fout_base_ywind),'.', &
               & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
        end if
        call open_file_direct(mtot1, fout_ywind, lreclen)
        write(mtot1,rec=1) real(vsm(1:imt,1:jmt),4)
        call close_file(mtot1)

        if (int_hist == -1) then
          write(fout_swind,'(1a,1a,1a,1a,i4.4,i2.2)') &
               & trim(flx_dir),'/',trim(fout_base_swind),'.',current_date%year, current_date%month
        else
          write(fout_swind,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
               & trim(flx_dir),'/',trim(fout_base_swind),'.',&
               & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
        end if
        call open_file_direct(mtot1, fout_swind, lreclen)
        write(mtot1,rec=1) real(wdvm(1:imt,1:jmt),4)
        call close_file(mtot1)
      end if

      if (l_proc_atm) then
        if (int_hist == -1) then
          write(fout_sat,'(1a,1a,1a,1a,i4.4,i2.2)') &
               & trim(flx_dir),'/',trim(fout_base_sat),'.',current_date%year, current_date%month
        else
          write(fout_sat,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
               & trim(flx_dir),'/',trim(fout_base_sat),'.',&
               & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
        end if
        call open_file_direct(mtot1, fout_sat, lreclen)
        write(mtot1,rec=1) real(satm(1:imt,1:jmt),4)
        call close_file(mtot1)

        if (int_hist == -1) then
          write(fout_sph,'(1a,1a,1a,1a,i4.4,i2.2)') &
               & trim(flx_dir),'/',trim(fout_base_sph),'.',current_date%year, current_date%month
        else
          write(fout_sph,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
               & trim(flx_dir),'/',trim(fout_base_sph),'.',&
               & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
        end if
        call open_file_direct(mtot1, fout_sph, lreclen)
        write(mtot1,rec=1) real(sphm(1:imt,1:jmt),4)
        call close_file(mtot1)
        
        if (int_hist == -1) then
          write(fout_rlh,'(1a,1a,1a,1a,i4.4,i2.2)') &
               & trim(flx_dir),'/',trim(fout_base_rlh),'.',current_date%year, current_date%month
        else
          write(fout_rlh,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
               & trim(flx_dir),'/',trim(fout_base_rlh),'.',&
               & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
        end if
        call open_file_direct(mtot1, fout_rlh, lreclen)
        write(mtot1,rec=1) real(rlhm(1:imt,1:jmt),4)
        call close_file(mtot1)
        
        if (int_hist == -1) then
          write(fout_roa,'(1a,1a,1a,1a,i4.4,i2.2)') &
               & trim(flx_dir),'/',trim(fout_base_roa),'.',current_date%year, current_date%month
        else
          write(fout_roa,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
               & trim(flx_dir),'/',trim(fout_base_roa),'.',&
               & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
        end if
        call open_file_direct(mtot1, fout_roa, lreclen)
        write(mtot1,rec=1) real(roam(1:imt,1:jmt),4)
        call close_file(mtot1)

        if (int_hist == -1) then
          write(fout_slp,'(1a,1a,1a,1a,i4.4,i2.2)') &
               & trim(flx_dir),'/',trim(fout_base_slp),'.',current_date%year, current_date%month
        else
          write(fout_slp,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
               & trim(flx_dir),'/',trim(fout_base_slp),'.',&
               & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
        end if
        call open_file_direct(mtot1, fout_slp, lreclen)
        write(mtot1,rec=1) real(slpm(1:imt,1:jmt),4)
        call close_file(mtot1)
      end if

      num_hist_wind(:,:) = 0
      num_hist_atm(:,:) = 0
      num_hist_slp(:,:) = 0
      usm (:,:) = 0.0d0
      vsm (:,:) = 0.0d0
      wdvm(:,:) = 0.0d0
      satm(:,:) = 0.0d0
      sphm(:,:) = 0.0d0
      rlhm(:,:) = 0.0d0
      roam(:,:) = 0.0d0
      slpm(:,:) = 0.0d0

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

end program diagnosis_surface_atmospheric_state
