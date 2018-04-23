! -*-F90-*-
!----------- diagflux_bulk_interannual_on_cobesst.F90 ----------------
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

  type(type_force) :: sst1_file
  type(type_force) :: ice1_file

  !------------

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

  !---------------------------------------------------
  ! Surface Atmospheric State

  integer(4) :: imf, jmf, kmf
  real(8),allocatable :: alonf(:), alatf(:)

  real(8),allocatable :: usf(:,:), vsf(:,:)
  real(8),allocatable :: satf(:,:), sphf(:,:)
  real(8),allocatable :: slpf(:,:), wdvf(:,:)
  real(8),allocatable :: aexl(:,:)
  real(8),allocatable :: aexl_icefree(:,:)

  real(8),allocatable :: sstf(:,:), icef(:,:)
  real(8),parameter :: undef_out = -9.99e33

  !---------------------------------------------------
  ! SST and ICE

  ! SST (1)

  real(8),allocatable :: sst1f(:,:), ice1f(:,:)
  integer(4) :: imt1, jmt1, kmt1
  real(8),allocatable :: alond1(:), alatd1(:)
  real(8),allocatable :: aexl_sst1(:,:)
  real(8),allocatable :: sst1(:,:)
  real(8),allocatable :: ice1(:,:)

  ! following items are diagnosed in this program

  integer(4) :: int_hist
  logical :: l_histout
  logical :: l_one_file_per_year, l_already_open

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

  integer(4) :: mtin1, mtin2, mtin3
  integer(4) :: mtot1, mtot2, mtot3

  !------------------
  ! SST(1) data

  character(256) :: sst1_base_dir, sst1_dir
  character(256) :: fin_base_sst1, fin_base_ice1
  character(256) :: fin_sst1
  integer(4) :: file_first_sst1(6)
  integer(4) :: file_intv_type_sst1
  integer(4) :: file_intv_sst1
  integer(4) :: total_rec_sst1
  integer(4) :: data_intv_sec_sst1
  integer(4) :: sst1_first(6)
  real(4)    :: undef_sst1
  logical    :: l_ymdir_sst1

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

  integer(4) :: mtot_w10n
  integer(4) :: irec_w10n

  !------------------

  logical :: l_leap_valid

  integer(4) :: nstep

  integer(4) :: lreclen

  integer(4) :: i, j, k, l, m, n

  !----------------------------------------------------------------------

  type(type_libmxe_para) :: srfp, sst1p
  type(type_libmxe_grid) :: srfg, sst1g
  type(type_libmxe_topo) :: srft, sst1t
  character(256) :: file_namelist_sst1
  character(256) :: file_namelist_srf

  integer(4),parameter :: lun = 10

  integer(4) :: ios, istat

  real(8) :: area_earth_orgdata
  real(8) :: hl1

  logical :: l_bulk_core

  integer(4) :: jcool, jwave
  real(8),allocatable :: x(:,:,:), y(:,:,:)

  !--------------------------------------------------------------------

  namelist /nml_sst1data/ file_namelist_sst1, &
       &  sst1_base_dir, &
       &  fin_base_sst1, fin_base_ice1, &
       &  file_first_sst1, &
       &  file_intv_type_sst1, file_intv_sst1, total_rec_sst1, data_intv_sec_sst1, &
       &  sst1_first, undef_sst1, &
       &  l_ymdir_sst1

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
       &  fout_base_w10n, &
       &  l_bulk_core,    &
       &  l_one_file_per_year

  !-----------------------------------------------------------------------

  open(lun,file='namelist.diagbulkflx_cobe',iostat=istat,status='old')

  if ( istat /= 0 ) then
    write(6,*) ' Cannot find file : namelist.diagbulkflx_cobe'
    stop
  end if

  ios = 0

  rewind(lun)
  file_intv_sst1 = 0
  read(lun, nml=nml_sst1data, iostat=istat)
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
  l_bulk_core = .true.
  l_one_file_per_year = .false.
  l_already_open = .false.

  read(lun, nml=nml_diagbulkflx, iostat=istat)
  ios = ios + istat

  if ( ios /= 0 ) then
    write(6,*) ' Read error : namelist.diagbulkflx_cobe', ios
    stop
  end if
  close(lun)

  if (l_bulk_core) then
    write(6,*) ' Use Large and Yeager (2009) bulk formula '
  else
    write(6,*) ' Use Fairall et al. (2003) bulk formula '
  end if

  !----------------------------------------------------------------------
  ! SST (1) : Brightness temperature

  call libmxe_para__register(sst1p,file_namelist=file_namelist_sst1)
  call libmxe_grid__register(sst1g,sst1p)
  call libmxe_topo__register(sst1t,sst1p)
  call libmxe_topo__aexl(sst1t,sst1p)
  imt1 = sst1p%imut
  jmt1 = sst1p%jmut
  kmt1 = sst1p%km
  allocate(alond1(1:imt1), alatd1(1:jmt1))
  alond1(1:imt1) = sst1g%lonu(1:imt1)
  alatd1(1:jmt1) = sst1g%latu(1:jmt1)
  allocate(aexl_sst1(1:imt1,1:jmt1))
  aexl_sst1(1:imt1,1:jmt1) = sst1t%aexl(1:imt1,1:jmt1,1)

  call force_process__ini(l_leap_valid)

  allocate(sst1(1:imt1,1:jmt1))
  allocate(ice1(1:imt1,1:jmt1))

  item_name='SST1'
  call register_file( &
       &  item_name, &
       &  sst1_file,  &
       &  fin_base_sst1,  &
       &  file_first_sst1, &
       &  file_intv_type_sst1, &
       &  file_intv_sst1, &
       &  total_rec_sst1, &
       &  data_intv_sec_sst1, &
       &  sst1_first, &
       &  imt1, jmt1,  &
       &  undef_sst1 )

  item_name='ICE1'
  call register_file( &
       &  item_name, &
       &  ice1_file,  &
       &  fin_base_ice1,  &
       &  file_first_sst1, &
       &  file_intv_type_sst1, &
       &  file_intv_sst1, &
       &  total_rec_sst1, &
       &  data_intv_sec_sst1, &
       &  sst1_first, &
       &  imt1, jmt1,  &
       &  undef_sst1 )

  !-----------------------------------------------------------

  call libmxe_para__register(srfp,file_namelist=file_namelist_srf)

  imf = srfp%imut
  jmf = srfp%jmut
  kmf = srfp%km

  call libmxe_grid__register(srfg,srfp)

  allocate(alonf(1:imf), alatf(1:jmf))
  alonf(1:imf) = srfg%lonu(1:imf)
  alatf(1:jmf) = srfg%latu(1:jmf)

  call libmxe_topo__register(srft,srfp)
  call libmxe_topo__aexl(srft,srfp)

  allocate(aexl(1:imf,1:jmf))
  aexl(1:imf,1:jmf) = srft%aexl(1:imf,1:jmf,1)

  allocate(aexl_icefree(1:imf,1:jmf))
  aexl_icefree(1:imf,1:jmf) = 0.d0

  allocate(usf (1:imf,1:jmf), vsf (1:imf,1:jmf))
  allocate(satf(1:imf,1:jmf), sphf(1:imf,1:jmf))
  allocate(slpf(1:imf,1:jmf))
  allocate(wdvf(1:imf,1:jmf))


  if (.not. l_bulk_core) then
    allocate(x(1:imf,1:jmf,1:16))
    allocate(y(1:imf,1:jmf,1:23))
  end if

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
  ! Check

  area_earth_orgdata = 0.0d0

  do j = 1, jmt1
    do i = 1, imt1
      area_earth_orgdata = area_earth_orgdata &
           & + sst1g%a_br(i,j) + sst1g%a_bl(i,j) &
           & + sst1g%a_tr(i,j) + sst1g%a_tl(i,j)
    end do
  end do

  write(6,*) ' Area of the Earth (original data) ', area_earth_orgdata
  write(6,*) ' Area of the Earth (theoretical)   ', 4.0d0 * pi * radius * radius

  !--------------------------------------------------------------------
  ! diagnosed

  allocate(wsx(1:imf,1:jmf), wsy(1:imf,1:jmf))
  allocate(qla(1:imf,1:jmf), qsn(1:imf,1:jmf))
  allocate(evp(1:imf,1:jmf))
  allocate(satu(1:imf,1:jmf), sphu(1:imf,1:jmf))
  allocate(dtu(1:imf,1:jmf))
  allocate(dqu(1:imf,1:jmf))
  allocate(u10n(1:imf,1:jmf))
  allocate(v10n(1:imf,1:jmf))
  allocate(w10n(1:imf,1:jmf))
  u10n(1:imf,1:jmf) = 0.0d0
  v10n(1:imf,1:jmf) = 0.0d0
  w10n(1:imf,1:jmf) = 0.0d0

  ! sampling

  allocate(num_hist(1:imf,1:jmf))
  allocate(wsxm(1:imf,1:jmf), wsym(1:imf,1:jmf))
  allocate(qlam(1:imf,1:jmf), qsnm(1:imf,1:jmf))
  allocate(evpm(1:imf,1:jmf))
  allocate(u10nm(1:imf,1:jmf))
  allocate(v10nm(1:imf,1:jmf))
  allocate(w10nm(1:imf,1:jmf))

  !--------------------------------------------------------------------

  allocate(sstf (1:imf,1:jmf), icef (1:imf,1:jmf))
  allocate(sst1f(1:imf,1:jmf), ice1f(1:imf,1:jmf))

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

  sst1_dir = trim(sst1_base_dir)
  call get_first_two_data(current_date,sst1_file,sst1_dir,l_ymdir_sst1)
  call get_first_two_data(current_date,ice1_file,sst1_dir,l_ymdir_sst1)

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
  u10nm(:,:) = 0.0d0
  v10nm(:,:) = 0.0d0
  w10nm(:,:) = 0.0d0

  write(6,*) 
  write(6,*) ' MAIN LOOP START '

  LOOP_SURF_FORCE: do while ( .true. )

    write(6,*) ' This step ', current_date%year, current_date%month, current_date%day, &
         & current_date%hour, current_date%minute, data_intv_sec_srf
    nstep = nstep + 1

    !----------------------------------------------------------------------------
    ! COBESST

    sst1_dir = trim(sst1_base_dir)
    call update_data(current_date,sst1_file,sst1_dir,l_ymdir_sst1)
    call update_data(current_date,ice1_file,sst1_dir,l_ymdir_sst1)

    do j = 1, jmt1
      do i = 1, imt1
        sst1(i,j) = sst1_file%dat_b(i,j) * sst1_file%br + sst1_file%dat_a(i,j) * sst1_file%ar
        ice1(i,j) = ice1_file%dat_b(i,j) * ice1_file%br + ice1_file%dat_a(i,j) * ice1_file%ar
        if ((sst1(i,j) < -2.0d0) .and. (ice1(i,j) > 0.0d0)) then
          sst1(i,j) = sst_freeze
        end if
        if ((ice1(i,j) < 0.0d0) .or. (ice1(i,j) > 1.0d0)) then
          write(6,*) ' WARNING: ice data is erroneous ', i, j, ice1(i,j), aexl_sst1(i,j)
          write(6,*) ' b ', ice1_file%dat_b(i,j)
          write(6,*) ' a ', ice1_file%dat_a(i,j)
          ice1(i,j) = max(ice1(i,j),0.0d0)
          ice1(i,j) = min(ice1(i,j),1.0d0)
        end if
        sst1(i,j) = sst1(i,j) * aexl_sst1(i,j)
        ice1(i,j) = ice1(i,j) * aexl_sst1(i,j)
      end do
    end do

    sst1f(:,:) = sst1(:,:)
    ice1f(:,:) = ice1(:,:)

    call hintpl_mask(sst1f,imf,jmf,alonf,alatf,undef_out,sst1,imt1,jmt1,alond1,alatd1,aexl_sst1)
    call hintpl_mask(ice1f,imf,jmf,alonf,alatf,undef_out,ice1,imt1,jmt1,alond1,alatd1,aexl_sst1)

    !---------------------
    ! Unified product

    icef(:,:) = ice1f(:,:) 

    do j = 1, jmf
      do i = 1, imf
        if (ice1f(i,j) >= 0.15d0) then
          aexl_icefree(i,j) = 0.0d0
          sstf(i,j) = aexl(i,j) * sst1f(i,j)
        else            
          if (sst1f(i,j) /= undef_out) then
            aexl_icefree(i,j) = 1.0d0
            sstf(i,j) = aexl(i,j) * sst1f(i,j)
          else
            aexl_icefree(i,j) = 0.0d0
            sstf(i,j) = aexl(i,j) * sst1f(i,j) 
          end if
        end if
      end do
    end do

    !---------------------------------------------------------------------

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
        wdvf(i,j) = sqrt(us_file%dat_b(i,j)**2 + vs_file%dat_b(i,j)**2) * us_file%br &
             &    + sqrt(us_file%dat_a(i,j)**2 + vs_file%dat_a(i,j)**2) * us_file%ar 
      end do
    end do

    !do j = 1, jmt
    !  do i = 1, imt
    !    wdvf(i,j) = sqrt(usf(i,j)**2 + vsf(i,j)**2)
    !  end do
    !end do

    !------

    if (l_bulk_core) then
      call bulk(wsx,wsy,qla,qsn,evp,&
           &    satu,sphu,dtu,dqu,w10n,&
           &    usf,vsf,satf,sphf,wdvf,slpf,sstf, &
           &    imf,jmf,aexl_icefree,alt_wind,alt_sat,alt_sph)
    else
      ! packing
      do j = 1, jmf
        do i = 1, imf
          x(i,j,1) = wdvf(i,j) !wind speed (m/s)  at height zu (m)
          x(i,j,2) = 0.0d0     !surface current speed in the wind direction (m/s)
          x(i,j,3) = sstf(i,j) !bulk water temperature (C) if jcool=1, interface water T if jcool=0  
          x(i,j,4) = satf(i,j) !bulk air temperature (C), height zt
          x(i,j,5) = sphf(i,j) * 1.0d3 !bulk air spec hum (g/kg), height zq
          x(i,j,6) = -999.d0   !downward solar flux (W/m**2)
          x(i,j,7) = -999.d0   !downard IR flux (W/m**2)
          x(i,j,8) = -999.d0   !rain rate (mm/hr)
          x(i,j,9) = 600.d0    !PBL depth (m) = 600 (m)
          x(i,j,10)= slpf(i,j) !Atmos surface pressure (mb)
          x(i,j,11)= alt_wind  !wind speed measurement height (m)
          x(i,j,12)= alt_sat   !air T measurement height (m)
          x(i,j,13)= alt_sph   !air q measurement height (m)
          x(i,j,14)= alatf(j)  !latitude (deg, N=+)
          x(i,j,15)= -999.d0   !wave period (s)
          x(i,j,16)= -999.d0   !wave height (m)
        end do
      end do

      jcool = 0
      jwave = 0

      call bulk_cor30a(imf,jmf,x,y,aexl_icefree,jcool,jwave)

      do j = 1, jmf
        do i = 1, imf
          wsx (i,j) = 0.0d0
          wsy (i,j) = 0.0d0
          qsn (i,j) = y(i,j,1)
          qla (i,j) = y(i,j,2)
          evp (i,j) = 0.0d0
          w10n(i,j) = y(i,j,23)
        end do
      end do

    end if
    
    !------

    do j = 1, jmf
      do i = 1, imf
        if (aexl_icefree(i,j) == 1.0d0) then
          u10n(i,j) = usf(i,j) * w10n(i,j) / wdvf(i,j)
          v10n(i,j) = vsf(i,j) * w10n(i,j) / wdvf(i,j)
        end if
      end do
    end do

    do j = 1, jmf
      do i = 1, imf
        if (aexl_icefree(i,j) == 1.0d0) then
          num_hist(i,j) = num_hist(i,j) + 1
          hl1 = 1.0d0 - icef(i,j)
          wsxm(i,j) = wsxm(i,j) + wsx(i,j)
          wsym(i,j) = wsym(i,j) + wsy(i,j)
          qlam(i,j) = qlam(i,j) + qla(i,j) * hl1
          qsnm(i,j) = qsnm(i,j) + qsn(i,j) * hl1
          evpm(i,j) = evpm(i,j) + evp(i,j) * hl1
          u10nm(i,j) = u10nm(i,j) + u10n(i,j)
          v10nm(i,j) = v10nm(i,j) + v10n(i,j)
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
      do j = 1, jmf
        do i = 1, imf
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

      lreclen = 4 * imf * jmf

      !-----------------------------------------------

      if (l_one_file_per_year) then
        if (.not. l_already_open) then
          write(fout_w10n,'(1a,1a,1a,1a,i4.4)') trim(flx_base_dir),'/',trim(fout_base_w10n),'.',current_date%year
          call open_file_direct(mtot_w10n, fout_w10n, lreclen)
          irec_w10n = 0
          l_already_open = .true.
        end if
      else
        if (int_hist == -1) then
          write(flx_dir,'(1a)') trim(flx_base_dir)
        else
          write(flx_dir,'(1a,1a,i4.4,i2.2)') trim(flx_base_dir),'/',current_date%year, current_date%month
        end if
      end if

!      if (int_hist == -1) then
!        write(fout_taux,'(1a,1a,1a,1a,i4.4,i2.2)') &
!             & trim(flx_dir),'/',trim(fout_base_taux),'.',current_date%year, current_date%month
!      else
!        write(fout_taux,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
!             & trim(flx_dir),'/',trim(fout_base_taux),'.',&
!             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
!      end if
!      call open_file_direct(mtot1, fout_taux, lreclen)
!      write(mtot1,rec=1) real(wsxm(1:imf,1:jmf),4)
!      call close_file(mtot1)
!
!      if (int_hist == -1) then
!        write(fout_tauy,'(1a,1a,1a,1a,i4.4,i2.2)') &
!             & trim(flx_dir),'/',trim(fout_base_tauy),'.',current_date%year, current_date%month
!      else
!        write(fout_tauy,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
!             & trim(flx_dir),'/',trim(fout_base_tauy),'.', &
!             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
!      end if
!      call open_file_direct(mtot1, fout_tauy, lreclen)
!      write(mtot1,rec=1) real(wsym(1:imf,1:jmf),4)
!      call close_file(mtot1)
!
!      if (int_hist == -1) then
!        write(fout_latent,'(1a,1a,1a,1a,i4.4,i2.2)') &
!             & trim(flx_dir),'/',trim(fout_base_latent),'.',current_date%year, current_date%month
!      else
!        write(fout_latent,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
!             & trim(flx_dir),'/',trim(fout_base_latent),'.',&
!             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
!      end if
!      call open_file_direct(mtot1, fout_latent, lreclen)
!      write(mtot1,rec=1) real(qlam(1:imf,1:jmf),4)
!      call close_file(mtot1)
!
!      if (int_hist == -1) then
!        write(fout_sens,'(1a,1a,1a,1a,i4.4,i2.2)') &
!             & trim(flx_dir),'/',trim(fout_base_sens),'.',current_date%year, current_date%month
!      else
!        write(fout_sens,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
!             & trim(flx_dir),'/',trim(fout_base_sens),'.',&
!             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
!      end if
!      call open_file_direct(mtot1, fout_sens, lreclen)
!      write(mtot1,rec=1) real(qsnm(1:imf,1:jmf),4)
!      call close_file(mtot1)
!      
!      if (int_hist == -1) then
!        write(fout_evapor,'(1a,1a,1a,1a,i4.4,i2.2)') &
!             & trim(flx_dir),'/',trim(fout_base_evapor),'.',current_date%year, current_date%month
!      else
!        write(fout_evapor,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
!             & trim(flx_dir),'/',trim(fout_base_evapor),'.',&
!             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
!      end if
!      call open_file_direct(mtot1, fout_evapor, lreclen)
!      write(mtot1,rec=1) real(evpm(1:imf,1:jmf),4)
!      call close_file(mtot1)
!
!      if (int_hist == -1) then
!        write(fout_u10n,'(1a,1a,1a,1a,i4.4,i2.2)') &
!             & trim(flx_dir),'/',trim(fout_base_u10n),'.',current_date%year, current_date%month
!      else
!        write(fout_u10n,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
!             & trim(flx_dir),'/',trim(fout_base_u10n),'.',&
!             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
!      end if
!      call open_file_direct(mtot1, fout_u10n, lreclen)
!      write(mtot1,rec=1) real(u10nm(1:imf,1:jmf),4)
!      call close_file(mtot1)
!
!      if (int_hist == -1) then
!        write(fout_v10n,'(1a,1a,1a,1a,i4.4,i2.2)') &
!             & trim(flx_dir),'/',trim(fout_base_v10n),'.',current_date%year, current_date%month
!      else
!        write(fout_v10n,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
!             & trim(flx_dir),'/',trim(fout_base_v10n),'.',&
!             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
!      end if
!      call open_file_direct(mtot1, fout_v10n, lreclen)
!      write(mtot1,rec=1) real(v10nm(1:imf,1:jmf),4)
!      call close_file(mtot1)

      if (.not. l_one_file_per_year) then
        if (int_hist == -1) then
          write(fout_w10n,'(1a,1a,1a,1a,i4.4,i2.2)') &
               & trim(flx_dir),'/',trim(fout_base_w10n),'.',current_date%year, current_date%month
        else
          write(fout_w10n,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
               & trim(flx_dir),'/',trim(fout_base_w10n),'.',&
               & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
        end if
        call open_file_direct(mtot_w10n, fout_w10n, lreclen)
        irec_w10n = 0
      end if

      irec_w10n = irec_w10n + 1
      write(mtot_w10n,rec=irec_w10n) real(w10nm(1:imf,1:jmf),4)

      if (l_one_file_per_year) then
        if ((l_already_open) .and. (current_date%year /= next_date%year)) then
          call close_file(mtot_w10n)
          write(6,*) 'close file, irec_w10n = ', irec_w10n
          l_already_open = .false.
        end if
      else
        call close_file(mtot_w10n)
      end if

!----------------------------------------------------

      num_hist(:,:) = 0
      wsxm(:,:) = 0.0d0
      wsym(:,:) = 0.0d0
      qlam(:,:) = 0.0d0
      qsnm(:,:) = 0.0d0
      evpm(:,:) = 0.0d0
      u10nm(:,:) = 0.0d0
      v10nm(:,:) = 0.0d0
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

  if (l_one_file_per_year .and. l_already_open) then
    call close_file(mtot_w10n)
    l_already_open = .false.
    write(6,*) 'close file, irec_w10n = ', irec_w10n
  end if

  write(6,*) ' MAIN LOOP END '

end program diagnosis_bulk_flux
