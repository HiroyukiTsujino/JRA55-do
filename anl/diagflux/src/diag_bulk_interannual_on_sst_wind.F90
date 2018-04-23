! -*-F90-*-
!----------- diagflux_bulk_interannual_on_sst.F90 ------------------
!  Information:
!     Diangnose heat flux using SST and Surface Atmospheric State
!-------------------------------------------------------------------
program diagnosis_turbulent_heat_flux

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

  ! [MKS]

  real(8), parameter :: stfblz_mks = 5.67d-8 ! [W/m2/K4]

  ! for albedo (Large and Yeager 2009)
  real(8), parameter :: cnst1ly = 0.069d0
  real(8), parameter :: cnst2ly = 0.011d0

  !------------

  character(len=16) :: item_name

  type(type_force) :: sst_file
  type(type_force) :: ice_file

  type(type_force) :: sat_file
  type(type_force) :: sph_file
  type(type_force) :: slp_file
  type(type_force) :: us_file
  type(type_force) :: vs_file

  type(type_force) :: dsw_file
  type(type_force) :: dlw_file

  type(type_force) :: pcp_file

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
  real(8),allocatable :: area_recv(:,:), area_recv_vec(:,:)

  ! Radiation

  integer(4) :: imr, jmr, kmr
  real(8),allocatable :: alonr(:), alatr(:)
  real(8),allocatable :: dswr(:,:), dlwr(:,:)

  ! Precipitation

  integer(4) :: imp, jmp, kmp
  real(8),allocatable :: alonp(:), alatp(:)
  real(8),allocatable :: prcpp(:,:)

  ! SST

  integer(4) :: imt, jmt, kmt
  real(8),allocatable :: alond(:), alatd(:)

  real(8),allocatable :: aexl(:,:)
  real(8),allocatable :: sst(:,:)
  real(8),allocatable :: ice(:,:)

  real(8),allocatable :: us (:,:), vs (:,:)
  real(8),allocatable :: usv(:,:), vsv(:,:), wdvv(:,:)
  real(8),allocatable :: sat(:,:), sph(:,:)
  real(8),allocatable :: slp(:,:), wdv(:,:)
  real(8),allocatable :: dsw(:,:), dlw(:,:)
  real(8),allocatable :: prcp(:,:)

  real(8),allocatable :: alb_ly(:)

  ! following items are diagnosed in this program

  integer(4) :: int_hist
  logical :: l_histout
  real(8),allocatable :: wsx(:,:), wsy(:,:)
  real(8),allocatable :: qla(:,:), qsn(:,:)
  real(8),allocatable :: evp(:,:)
  real(8),allocatable :: satu(:,:), sphu(:,:)
  real(8),allocatable :: dtu(:,:), dqu(:,:)
  real(8),allocatable :: w10n(:,:)

  integer(4),allocatable :: num_hist(:,:)
  real(8),allocatable :: wsxm(:,:), wsym(:,:)
  real(8),allocatable :: qlam(:,:), qsnm(:,:)
  real(8),allocatable :: evpm(:,:)
  real(8),allocatable :: nswm(:,:), nlwm(:,:)
  real(8),allocatable :: dswm(:,:), dlwm(:,:)
  real(8),allocatable :: prcpm(:,:)
  real(8),allocatable :: satum(:,:), sphum(:,:)
  real(8),allocatable :: dtum(:,:), dqum(:,:)
  real(8),allocatable :: w10nm(:,:)

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

  !------------------
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

  character(256) :: flx_base_dir, flx_dir
  character(256) :: fout_base_taux, fout_base_tauy
  character(256) :: fout_base_sens
  character(256) :: fout_base_latent
  character(256) :: fout_base_evapor
  character(256) :: fout_base_nswrf
  character(256) :: fout_base_nlwrf
  character(256) :: fout_base_dswrf
  character(256) :: fout_base_dlwrf
  character(256) :: fout_base_precip
  character(256) :: fout_base_satu
  character(256) :: fout_base_sphu
  character(256) :: fout_base_dtu
  character(256) :: fout_base_dqu
  character(256) :: fout_base_w10n


  character(256) :: fout_taux, fout_tauy
  character(256) :: fout_sens
  character(256) :: fout_latent
  character(256) :: fout_evapor
  character(256) :: fout_nswrf
  character(256) :: fout_nlwrf
  character(256) :: fout_dswrf
  character(256) :: fout_dlwrf
  character(256) :: fout_precip
  character(256) :: fout_satu
  character(256) :: fout_sphu
  character(256) :: fout_dtu
  character(256) :: fout_dqu
  character(256) :: fout_w10n

  !------------------

  logical :: l_leap_valid

  integer(4) :: nstep

  integer(4) :: lreclen

  integer(4) :: i, j, k, l, m, n

  !----------------------------------------------------------------------

  type(type_libmxe_para) :: srfp, sstp, radp, pcpp
  type(type_libmxe_grid) :: srfg, sstg, radg, pcpg
  type(type_libmxe_topo) :: sstt
  character(256) :: file_namelist_sst, file_namelist_srf
  character(256) :: file_namelist_rad, file_namelist_pcp

  integer(4),parameter :: lun = 10

  integer(4) :: ios, istat

  real(8) :: area_earth_orgdata
  real(8) :: hl1

  real(8) :: factor_dswrf, factor_dlwrf, factor_precip ! convert to standard MKS units
  real(8) :: factor_model_dswrf, factor_model_dlwrf, factor_model_precip ! convert to model's units

  !--------------------------------------------------------------------

  type(type_table) :: orgo2sst, orga2sst
  character(len=256) :: file_table_ocn, file_table_all

  type(type_table) :: orgo2sst_wind
  character(len=256) :: file_table_ocn_wind

  type(type_table_vec) :: orgo2sst_vec
  character(len=256) :: file_table_ocn_vec

  character(len=256) :: name_tmp
  logical :: l_lin_dswrf, l_lin_dlwrf, l_lin_precip
  logical :: l_lin_wind, l_lin_sat, l_lin_sph, l_lin_slp
  logical :: l_need_table
  logical :: l_ocn_dswrf, l_ocn_dlwrf, l_ocn_precip
  logical :: l_ocn_wind, l_ocn_wind_vec, l_ocn_sat, l_ocn_sph, l_ocn_slp

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

  namelist /nml_raddata/ file_namelist_rad, &
       &  rad_base_dir, &
       &  fin_base_dsw, fin_base_dlw, &
       &  file_first_rad, &
       &  file_intv_type_rad, file_intv_rad, total_rec_rad, data_intv_sec_rad, &
       &  rad_first, undef_rad, l_ymdir_rad

  namelist /nml_pcpdata/ file_namelist_pcp, &
       &  pcp_base_dir, &
       &  fin_base_pcp, &
       &  file_first_pcp, &
       &  file_intv_type_pcp, file_intv_pcp, total_rec_pcp, data_intv_sec_pcp, &
       &  pcp_first, undef_pcp, l_ymdir_pcp

  namelist /nml_diagflux/ calc_start, calc_end, &
       &  l_leap_valid, &
       &  int_hist, &
       &  flx_base_dir, &
       &  fout_base_taux, fout_base_tauy, &
       &  fout_base_sens, fout_base_latent, &
       &  fout_base_evapor, &
       &  fout_base_nswrf, fout_base_nlwrf, &
       &  fout_base_dswrf, fout_base_dlwrf, &
       &  fout_base_precip, &
       &  fout_base_satu, fout_base_sphu, &
       &  fout_base_dtu, fout_base_dqu, &
       &  fout_base_w10n, &
       &  factor_dswrf, factor_dlwrf, factor_precip

  namelist /nml_table_ocn/ file_table_ocn

  namelist /nml_table_all/ file_table_all

  namelist /nml_table_ocn_vec/ file_table_ocn_vec

  namelist /nml_table_ocn_wind/ file_table_ocn_wind

  namelist /nml_intpol/ l_lin_dswrf, l_lin_dlwrf, l_lin_precip, &
       &                l_lin_wind, l_lin_sat, l_lin_sph, l_lin_slp, &
       &                l_ocn_dswrf, l_ocn_dlwrf, l_ocn_precip, &
       &                l_ocn_wind, l_ocn_sat, l_ocn_sph, l_ocn_slp, l_ocn_wind_vec

  !-----------------------------------------------------------------------
  ! factor to convert product data

  ! to cgs (MRI.COM)
  !factor_model_dswrf = 1.0d3         ! [W/m^2] => cgs
  !factor_model_dlwrf = 1.0d3         ! [W/m^2] => cgs
  !factor_model_precip = 1.0d-1 / rho ! [kg/m^2/s] => [cm/s]

  ! keep MKS
  factor_model_dswrf = 1.0d0
  factor_model_dlwrf = 1.0d0
  factor_model_precip = 1.0d0

  ! factor to convert input data to MKS (read from namelist)

  factor_dswrf  = 1.0d0
  factor_dlwrf  = 1.0d0
  factor_precip = 1.0d0

  open(lun,file='namelist.diagflux',iostat=istat,status='old')

  if ( istat /= 0 ) then
    write(6,*) ' Cannot find file : namelist.diagflux'
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
  file_intv_rad = 0
  read(lun, nml=nml_raddata, iostat=istat)
  ios = ios + istat

  rewind(lun)
  file_intv_pcp = 0
  read(lun, nml=nml_pcpdata, iostat=istat)
  ios = ios + istat

  rewind(lun)
  read(lun, nml=nml_diagflux, iostat=istat)
  ios = ios + istat

  l_ocn_wind_vec = .false.
  rewind(lun)
  read(lun, nml=nml_intpol, iostat=istat)
  ios = ios + istat

  if ( ios /= 0 ) then
    write(6,*) ' Read error : namelist.diagflux', ios
    stop
  end if

  close(lun)

  if (l_lin_dswrf .and. l_lin_dlwrf .and. l_lin_precip &
       & .and. l_lin_wind .and. l_lin_sat .and. l_lin_sph .and. l_lin_slp) then
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

  allocate(alb_ly(1:jmt))
  do j = 1, jmt
    hl1 = 2.d0 * alatd(j) * radian_r
    alb_ly(j) = cnst1ly - cnst2ly * cos(hl1)
    write(6,*) ' albedo ', alatd(j), alb_ly(j)
  end do

  call force_process__ini(l_leap_valid)

  allocate(sst(1:imt,1:jmt))
  allocate(ice(1:imt,1:jmt))

  allocate(area_recv(1:imt,1:jmt))
  allocate(area_recv_vec(1:imt,1:jmt))

  allocate(us (1:imt,1:jmt), vs (1:imt,1:jmt))
  allocate(usv(1:imt,1:jmt), vsv(1:imt,1:jmt))
  allocate(sat(1:imt,1:jmt), sph(1:imt,1:jmt))
  allocate(slp(1:imt,1:jmt), wdv(1:imt,1:jmt))
  allocate(wdvv(1:imt,1:jmt))

  allocate(dsw(1:imt,1:jmt), dlw(1:imt,1:jmt))
  allocate(prcp(1:imt,1:jmt))
  allocate(satu(1:imt,1:jmt))
  allocate(sphu(1:imt,1:jmt))
  allocate(dtu(1:imt,1:jmt))
  allocate(dqu(1:imt,1:jmt))
  allocate(w10n(1:imt,1:jmt))

  allocate(wsx(1:imt,1:jmt), wsy(1:imt,1:jmt))
  allocate(qla(1:imt,1:jmt), qsn(1:imt,1:jmt))
  allocate(evp(1:imt,1:jmt))

  allocate(num_hist(1:imt,1:jmt))
  allocate(wsxm(1:imt,1:jmt), wsym(1:imt,1:jmt))
  allocate(qlam(1:imt,1:jmt), qsnm(1:imt,1:jmt))
  allocate(evpm(1:imt,1:jmt))
  allocate(nswm(1:imt,1:jmt), nlwm(1:imt,1:jmt))
  allocate(dswm(1:imt,1:jmt), dlwm(1:imt,1:jmt))
  allocate(prcpm(1:imt,1:jmt))
  allocate(satum(1:imt,1:jmt),sphum(1:imt,1:jmt))
  allocate(dtum(1:imt,1:jmt),dqum(1:imt,1:jmt))
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
  allocate(wdvf(1:imf,1:jmf))
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

  call libmxe_para__register(radp,file_namelist=file_namelist_rad)
  call libmxe_grid__register(radg,radp)
  imr = radp%imut
  jmr = radp%jmut
  kmr = radp%km
  allocate(alonr(1:imr), alatr(1:jmr))
  alonr(1:imr) = radg%lonu(1:imr)
  alatr(1:jmr) = radg%latu(1:jmr)

  allocate(dswr(1:imr,1:jmr), dlwr(1:imr,1:jmr))

  item_name='DSW'
  call register_file( &
       &  item_name, &
       &  dsw_file,  &
       &  fin_base_dsw,  &
       &  file_first_rad, &
       &  file_intv_type_rad, &
       &  file_intv_rad, &
       &  total_rec_rad, &
       &  data_intv_sec_rad, &
       &  rad_first, &
       &  imr, jmr,  &
       &  undef_rad )

  item_name='DLW'
  call register_file( &
       &  item_name, &
       &  dlw_file,  &
       &  fin_base_dlw,  &
       &  file_first_rad, &
       &  file_intv_type_rad, &
       &  file_intv_rad, &
       &  total_rec_rad, &
       &  data_intv_sec_rad, &
       &  rad_first, &
       &  imr, jmr,  &
       &  undef_rad )

  !-----------------------------------------------------------

  call libmxe_para__register(pcpp,file_namelist=file_namelist_pcp)
  call libmxe_grid__register(pcpg,pcpp)
  imp = pcpp%imut
  jmp = pcpp%jmut
  kmp = pcpp%km
  allocate(alonp(1:imp), alatp(1:jmp))
  alonp(1:imp) = pcpg%lonu(1:imp)
  alatp(1:jmp) = pcpg%latu(1:jmp)

  allocate(prcpp(1:imp,1:jmp))

  item_name='PCP'
  call register_file( &
       &  item_name, &
       &  pcp_file,  &
       &  fin_base_pcp,  &
       &  file_first_pcp, &
       &  file_intv_type_pcp, &
       &  file_intv_pcp, &
       &  total_rec_pcp, &
       &  data_intv_sec_pcp, &
       &  pcp_first, &
       &  imp, jmp,  &
       &  undef_pcp )

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

  if (l_need_table) then

    open(lun,file='namelist.diagflux',iostat=istat,status='old')

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

    rewind(lun)
    read(lun, nml=nml_table_ocn_wind, iostat=istat)
    if (istat == 0) then
      name_tmp = "Ocean to Ocean table (Wind)"
      call mapping_table__ini(  &
           & name_tmp,          &
           & file_table_ocn_wind, orgo2sst_wind,   &
           & imf, jmf, kmf, &
           & imt, jmt, kmt  &
           & )
    end if

    rewind(lun)
    read(lun, nml=nml_table_ocn_vec, iostat=istat)
    if (istat == 0) then
      name_tmp = "Ocean to Ocean table (Vector)"
      call mapping_table_vec__ini(  &
           & name_tmp,          &
           & file_table_ocn_vec, orgo2sst_vec,   &
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

  rad_dir = trim(rad_base_dir)
  call get_first_two_data(current_date,dsw_file,rad_dir,l_ymdir_rad)
  call get_first_two_data(current_date,dlw_file,rad_dir,l_ymdir_rad)

  pcp_dir = trim(pcp_base_dir)
  call get_first_two_data(current_date,pcp_file,pcp_dir,l_ymdir_pcp)

  nstep = 0
  num_hist(:,:) = 0
  wsxm (:,:) = 0.0d0
  wsym (:,:) = 0.0d0
  qlam (:,:) = 0.0d0
  qsnm (:,:) = 0.0d0
  evpm (:,:) = 0.0d0

  nswm (:,:) = 0.0d0
  nlwm (:,:) = 0.0d0

  dswm (:,:) = 0.0d0
  dlwm (:,:) = 0.0d0

  prcpm(:,:) = 0.0d0

  satum(:,:) = 0.0d0
  sphum(:,:) = 0.0d0

  dtum(:,:) = 0.0d0
  dqum(:,:) = 0.0d0
  w10nm(:,:) = 0.0d0

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

    if (trim(wind_base_dir) /= 'no_dir') then
      srf_dir = trim(wind_base_dir)
    else
      srf_dir = trim(srf_base_dir)
    end if
    call update_data(current_date,us_file,srf_dir,l_ymdir_srf)
    call update_data(current_date,vs_file,srf_dir,l_ymdir_srf)

    rad_dir = trim(rad_base_dir)
    call update_data(current_date,dsw_file,rad_dir,l_ymdir_rad)
    call update_data(current_date,dlw_file,rad_dir,l_ymdir_rad)

    pcp_dir = trim(pcp_base_dir)
    call update_data(current_date,pcp_file,pcp_dir,l_ymdir_pcp)

    do j = 1, jmf
      do i = 1, imf
        satf(i,j) = (sat_file%dat_b(i,j) * sat_file%br + sat_file%dat_a(i,j) * sat_file%ar) - tab
        sphf(i,j) =  sph_file%dat_b(i,j) * sph_file%br + sph_file%dat_a(i,j) * sph_file%ar 
        slpf(i,j) = (slp_file%dat_b(i,j) * slp_file%br + slp_file%dat_a(i,j) * slp_file%ar) * 1.0d-2 ! [hPa]
        usf (i,j) = (us_file%dat_b(i,j) * us_file%br + us_file%dat_a(i,j) * us_file%ar)
        vsf (i,j) = (vs_file%dat_b(i,j) * vs_file%br + vs_file%dat_a(i,j) * vs_file%ar)
        wdvf(i,j) = sqrt(usf(i,j)**2 + vsf(i,j)**2)
      end do
    end do

    do j = 1, jmr
      do i = 1, imr
        dswr(i,j) = (dsw_file%dat_b(i,j) * dsw_file%br + dsw_file%dat_a(i,j) * dsw_file%ar) * factor_dswrf * factor_model_dswrf
        dlwr(i,j) = (dlw_file%dat_b(i,j) * dlw_file%br + dlw_file%dat_a(i,j) * dlw_file%ar) * factor_dlwrf * factor_model_dlwrf
      end do
    end do

    do j = 1, jmp
      do i = 1, imp
        prcpp(i,j) = (pcp_file%dat_b(i,j) * pcp_file%br + pcp_file%dat_a(i,j) * pcp_file%ar) * factor_precip * factor_model_precip
      end do
    end do

   !--------------------------

    if (l_ocn_wind_vec) then
      call mapping_table_vec__main(     &
           & orgo2sst_vec,              &
           & usv,vsv,area_recv_vec,imt,jmt,kmt,  &
           & usf,vsf,imf,jmf,kmf,us_file%dundef, &
           & 1, imf, 1, jmf             &
           & )
    end if

    if (l_lin_wind) then
      call hintpl(us,imt,jmt,alond,alatd,usf,imf,jmf,alonf,alatf)
    else
      if (l_ocn_wind) then
!        if (l_ocn_wind_vec) then
!          call mapping_table__main(         &
!               & orgo2sst_wind,             &
!               & usv,area_recv_vec,imt,jmt,kmt, &
!               & usf,imf,jmf,kmf,us_file%dundef, &
!               & 1, imf, 1, jmf             &
!               & )
!        end if
        call mapping_table__main(         &
             & orgo2sst,                  &
             & usv,area_recv,imt,jmt,kmt, &
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
      if (l_ocn_wind_vec) then
        do j = 1, jmt
          do i = 1, imt
            if (area_recv_vec(i,j) > 0.0d0) then
              us(i,j) = usv(i,j)
            end if
          end do
        end do
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
!        if (l_ocn_wind_vec) then
!          call mapping_table__main(         &
!               & orgo2sst_wind,             &
!               & vsv,area_recv_vec,imt,jmt,kmt,  &
!               & vsf,imf,jmf,kmf,vs_file%dundef, &
!               & 1, imf, 1, jmf             &
!               & )
!        end if
        call mapping_table__main(          &
             & orgo2sst,                   &
             & vs,area_recv,imt,jmt,kmt,  &
             & vsf,imf,jmf,kmf,us_file%dundef, &
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
      if (l_ocn_wind_vec) then
        do j = 1, jmt
          do i = 1, imt
            if (area_recv_vec(i,j) > 0.0d0) then
              vs(i,j) = vsv(i,j)
            end if
          end do
        end do
      end if
      do j = 1, jmt
        do i = 1, imt
          if (area_recv(i,j) == 0.0d0) then
            vs(i,j) = vs_file%dundef
          end if
        end do
      end do
    end if

    if (l_ocn_wind_vec) then
      wdv(:,:) = 0.0d0
      do j = 1, jmt
        do i = 1, imt
          if (us(i,j) /= us_file%dundef .and. vs(i,j) /= vs_file%dundef) then
            wdv(i,j) = sqrt(us(i,j)**2 + vs(i,j)**2)
          else
            wdv(i,j) = us_file%dundef
          end if
        end do
      end do
    else
      if (l_lin_wind) then
        call hintpl(wdv,imt,jmt,alond,alatd,wdvf,imf,jmf,alonf,alatf)
      else
        if (l_ocn_wind) then
          call mapping_table__main(          &
               & orgo2sst,                   &
               & wdv,area_recv,imt,jmt,kmt,  &
               & wdvf,imf,jmf,kmf,us_file%dundef, &
               & 1, imf, 1, jmf             &
               & )
        else
          call mapping_table__main(         &
               & orga2sst,                  &
               & wdv,area_recv,imt,jmt,kmt,  &
               & wdvf,imf,jmf,kmf,us_file%dundef, &
               & 1, imf, 1, jmf             &
               & )
        end if
        do j = 1, jmt
          do i = 1, imt
            if (area_recv(i,j) == 0.0d0) then
              wdv(i,j) = us_file%dundef
            end if
          end do
        end do
      end if
    end if

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

!    dsw(:,:) = 0.0d0
    if (l_lin_dswrf) then
      call hintpl(dsw,imt,jmt,alond,alatd,dswr,imr,jmr,alonr,alatr)
    else
      if (l_ocn_dswrf) then
        call mapping_table__main(          &
             & orgo2sst,                   &
             & dsw,area_recv,imt,jmt,kmt,  &
             & dswr,imr,jmr,kmr,dsw_file%dundef, &
             & 1, imr, 1, jmr              &
             & )
      else
        call mapping_table__main(          &
             & orga2sst,                   &
             & dsw,area_recv,imt,jmt,kmt,  &
             & dswr,imr,jmr,kmr,dsw_file%dundef, &
             & 1, imr, 1, jmr              &
             & )
      end if
    end if

!    dlw(:,:) = 0.0d0
    if (l_lin_dlwrf) then
      call hintpl(dlw,imt,jmt,alond,alatd,dlwr,imr,jmr,alonr,alatr)
    else
      if (l_ocn_dlwrf) then
        call mapping_table__main(          &
             & orgo2sst,                   &
             & dlw,area_recv,imt,jmt,kmt,  &
             & dlwr,imr,jmr,kmr,dlw_file%dundef, &
             & 1, imr, 1, jmr              &
             & )
      else
        call mapping_table__main(          &
             & orga2sst,                   &
             & dlw,area_recv,imt,jmt,kmt,  &
             & dlwr,imr,jmr,kmr,dlw_file%dundef, &
             & 1, imr, 1, jmr              &
             & )
      end if
    end if

!    prcp(:,:) = 0.0d0
    if (l_lin_precip) then
      call hintpl(prcp,imt,jmt,alond,alatd,prcpp,imp,jmp,alonp,alatp)
    else
      if (l_ocn_precip) then
        call mapping_table__main(          &
             & orgo2sst,                   &
             & prcp,area_recv,imt,jmt,kmt, &
             & prcpp,imp,jmp,kmp,pcp_file%dundef, &
             & 1, imp, 1, jmp              &
             & )
      else
        call mapping_table__main(          &
             & orga2sst,                   &
             & prcp,area_recv,imt,jmt,kmt, &
             & prcpp,imp,jmp,kmp,pcp_file%dundef, &
             & 1, imp, 1, jmp              &
             & )
      end if
    end if

    !------

    call bulk(wsx,wsy,qla,qsn,evp,&
         &    satu,sphu,dtu,dqu,w10n, &
         &    us,vs,sat,sph,wdv,slp,sst, &
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
          nswm(i,j) = nswm(i,j) + (1.0d0 - alb_ly(j)) * dsw(i,j) * hl1
          nlwm(i,j) = nlwm(i,j) + (dlw(i,j) - ems * stfblz_mks * (sst(i,j) + tab) ** 4) * hl1
          dswm(i,j) = dswm(i,j) + dsw(i,j) * hl1
          dlwm(i,j) = dlwm(i,j) + dlw(i,j) * hl1
          !prcpm(i,j) = prcpm(i,j) + prcp(i,j) * hl1
          prcpm(i,j) = prcpm(i,j) + prcp(i,j)
          satum(i,j) = satum(i,j) + satu(i,j)
          sphum(i,j) = sphum(i,j) + sphu(i,j)
          dtum(i,j) = dtum(i,j) + dtu(i,j) * hl1
          dqum(i,j) = dqum(i,j) + dqu(i,j) * hl1
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
            nswm (i,j) = nswm (i,j) / real(num_hist(i,j),8)
            nlwm (i,j) = nlwm (i,j) / real(num_hist(i,j),8)
            dswm (i,j) = dswm (i,j) / real(num_hist(i,j),8)
            dlwm (i,j) = dlwm (i,j) / real(num_hist(i,j),8)
            prcpm(i,j) = prcpm(i,j) / real(num_hist(i,j),8)
            satum(i,j) = satum(i,j) / real(num_hist(i,j),8)
            sphum(i,j) = sphum(i,j) / real(num_hist(i,j),8)
            dtum (i,j) = dtum (i,j)  / real(num_hist(i,j),8)
            dqum (i,j) = dqum (i,j)  / real(num_hist(i,j),8)
            w10nm(i,j) = w10nm(i,j) / real(num_hist(i,j),8)
          else
            wsxm (i,j) = real(undef_srf,8)
            wsym (i,j) = real(undef_srf,8)
            qlam (i,j) = real(undef_srf,8)
            qsnm (i,j) = real(undef_srf,8)
            evpm (i,j) = real(undef_srf,8)
            nswm (i,j) = real(undef_srf,8)
            nlwm (i,j) = real(undef_srf,8)
            dswm (i,j) = real(undef_srf,8)
            dlwm (i,j) = real(undef_srf,8)
            prcpm(i,j) = real(undef_srf,8)
            satum(i,j) = real(undef_srf,8)
            sphum(i,j) = real(undef_srf,8)
            dtum (i,j) = real(undef_srf,8)
            dqum (i,j) = real(undef_srf,8)
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
        write(fout_nswrf,'(1a,1a,1a,1a,i4.4,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_nswrf),'.',current_date%year, current_date%month
      else
        write(fout_nswrf,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_nswrf),'.',&
             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
      end if
      call open_file_direct(mtot1, fout_nswrf, lreclen)
      write(mtot1,rec=1) real(nswm(1:imt,1:jmt),4)
      call close_file(mtot1)

      if (int_hist == -1) then
        write(fout_nlwrf,'(1a,1a,1a,1a,i4.4,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_nlwrf),'.',current_date%year, current_date%month
      else
        write(fout_nlwrf,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_nlwrf),'.',&
             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
      end if
      call open_file_direct(mtot1, fout_nlwrf, lreclen)
      write(mtot1,rec=1) real(nlwm(1:imt,1:jmt),4)
      call close_file(mtot1)

      if (int_hist == -1) then
        write(fout_dswrf,'(1a,1a,1a,1a,i4.4,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_dswrf),'.',current_date%year, current_date%month
      else
        write(fout_dswrf,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_dswrf),'.',&
             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
      end if
      call open_file_direct(mtot1, fout_dswrf, lreclen)
      write(mtot1,rec=1) real(dswm(1:imt,1:jmt),4)
      call close_file(mtot1)

      if (int_hist == -1) then
        write(fout_dlwrf,'(1a,1a,1a,1a,i4.4,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_dlwrf),'.',current_date%year, current_date%month
      else
        write(fout_dlwrf,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_dlwrf),'.',&
             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
      end if
      call open_file_direct(mtot1, fout_dlwrf, lreclen)
      write(mtot1,rec=1) real(dlwm(1:imt,1:jmt),4)
      call close_file(mtot1)

      if (int_hist == -1) then
        write(fout_precip,'(1a,1a,1a,1a,i4.4,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_precip),'.',current_date%year, current_date%month
      else
        write(fout_precip,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_precip),'.',&
             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
      end if
      call open_file_direct(mtot1, fout_precip, lreclen)
      write(mtot1,rec=1) real(prcpm(1:imt,1:jmt),4)
      call close_file(mtot1)

      if (int_hist == -1) then
        write(fout_satu,'(1a,1a,1a,1a,i4.4,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_satu),'.',current_date%year, current_date%month
      else
        write(fout_satu,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_satu),'.',&
             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
      end if
      call open_file_direct(mtot1, fout_satu, lreclen)
      write(mtot1,rec=1) real(satum(1:imt,1:jmt),4)
      call close_file(mtot1)

      if (int_hist == -1) then
        write(fout_sphu,'(1a,1a,1a,1a,i4.4,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_sphu),'.',current_date%year, current_date%month
      else
        write(fout_sphu,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_sphu),'.',&
             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
      end if
      call open_file_direct(mtot1, fout_sphu, lreclen)
      write(mtot1,rec=1) real(sphum(1:imt,1:jmt),4)
      call close_file(mtot1)

      if (int_hist == -1) then
        write(fout_dtu,'(1a,1a,1a,1a,i4.4,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_dtu),'.',current_date%year, current_date%month
      else
        write(fout_dtu,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_dtu),'.',&
             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
      end if
      call open_file_direct(mtot1, fout_dtu, lreclen)
      write(mtot1,rec=1) real(dtum(1:imt,1:jmt),4)
      call close_file(mtot1)

      if (int_hist == -1) then
        write(fout_dqu,'(1a,1a,1a,1a,i4.4,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_dqu),'.',current_date%year, current_date%month
      else
        write(fout_dqu,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_dqu),'.',&
             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
      end if
      call open_file_direct(mtot1, fout_dqu, lreclen)
      write(mtot1,rec=1) real(dqum(1:imt,1:jmt),4)
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
      nswm(:,:) = 0.0d0
      nlwm(:,:) = 0.0d0
      dswm(:,:) = 0.0d0
      dlwm(:,:) = 0.0d0
      prcpm(:,:) = 0.0d0
      satum(:,:) = 0.0d0
      sphum(:,:) = 0.0d0

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

end program diagnosis_turbulent_heat_flux
