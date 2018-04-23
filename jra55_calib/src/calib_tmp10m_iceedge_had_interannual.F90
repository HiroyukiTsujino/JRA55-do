! -*-F90-*-
!------------- calib_tmp2m_iceedge_interannual.F90 -----------------
!  Information:
!     Calibrate surface temperature and specific humidity
!-------------------------------------------------------------------
program calibration_surface_temperature_iceedge

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

  real(8), parameter :: molw = 18.016d0
  real(8), parameter :: mola = 28.966d0
  real(8), parameter :: epsilon = molw/mola
  real(8), parameter :: ewa = 0.62197d0    !Molecular Weight Ratio Water/DryAir

  ! LY

  !real(8), parameter :: rhoa = 1.22d0 ! air density [kg/m3]
  !real(8), parameter :: q0 = 0.98d0    ! dimensionless factor
  !real(8), parameter :: q1a = 640380.d0 ! [kg/m3]
  !real(8), parameter :: q2a = -5107.4d0 ! [K]
  !real(8), parameter :: q1i = 11637800.d0 ! L-Y p16
  !real(8), parameter :: q2i = -5897.8d0
  !real(8), parameter :: rgas = 287.04d0
  !real(8), parameter :: r0 = 0.6078d0

  ! parameters to calculate saturation water vapor presure

  ! Gill (1982)

  real(8), parameter :: g1 = 0.7859d0
  real(8), parameter :: g2 = 0.03477d0
  real(8), parameter :: g3 = 0.00412d0

  real(8), parameter :: h1 = 4.5d0
  real(8), parameter :: h2 = 0.0006d0

  real(8), parameter :: i1 = 0.00422d0

  ! work variables

  real(8) :: es
  real(8) :: ewp1, ewp2

  real(8) :: satmos, slpres

  real(8), parameter :: sphmin = 2.0d-5 ! minimum of specific humidity

  !------------
  ! low temperature cut-off south of 60S
  !
  !real(8), parameter :: avg_c0 = 61.846d0
  !real(8), parameter :: avg_c1 =  1.107d0
  !real(8), parameter :: amp_c0 = -21.841d0
  !real(8), parameter :: amp_c1 = -0.447d0
  !real(8), parameter :: phs_c0 = 0.298d0
  !real(8) :: rsec
  !real(8) :: cosfactor
  !real(8) :: tmin
  !integer(4) :: current_year_sec, this_year_sec
  !logical    :: l_leap_year
  !
  !------------

  character(len=16) :: item_name

  type(type_force) :: sst1_file
  type(type_force) :: ice1_file

  type(type_force) :: sst2_file
  type(type_force) :: ice2_file

  !------------

  type(type_calendar) :: start_date
  type(type_calendar) :: end_date

  type(type_calendar) :: current_date
  type(type_calendar) :: current_file
  type(type_calendar) :: next_date
  type(type_calendar) :: next_file

  type(type_calendar) :: current_year_start

  integer(4) :: calc_start(6)
  integer(4) :: calc_end(6)

  ! Surface Atmospheric State

  integer(4) :: imf, jmf, kmf
  real(8),allocatable :: alonf(:), alatf(:)

  real(8),allocatable :: satf(:,:), sphf(:,:)
  real(8),allocatable :: slpf(:,:), icjf(:,:)
  real(8),allocatable :: roaf(:,:), rlhf(:,:)

  real(8),allocatable :: sat_fill(:,:)
  real(8),allocatable :: satc(:,:), sphc(:,:)

  real(4),allocatable :: dat4f(:,:)

  ! SST (1)

  real(8),allocatable :: sst1f(:,:), ice1f(:,:)
  integer(4) :: imt1, jmt1, kmt1
  real(8),allocatable :: alond1(:), alatd1(:)
  real(8),allocatable :: aexl_sst1(:,:)
  real(8),allocatable :: sst1(:,:)
  real(8),allocatable :: ice1(:,:)

  ! SST (2)

  real(8),allocatable :: sst2f(:,:), ice2f(:,:)
  integer(4) :: imt2, jmt2, kmt2
  real(8),allocatable :: alond2(:), alatd2(:)
  real(8),allocatable :: aexl_sst2(:,:)
  real(8),allocatable :: sst2(:,:)
  real(8),allocatable :: ice2(:,:)

  !-----------------------------------------------

  real(8),allocatable :: mask_fill(:,:) ! must be filled
  real(8),allocatable :: mask_wrk(:,:)
  real(8),allocatable :: mask_free(:,:)
  real(8),allocatable :: mask_next(:,:)
  real(8),allocatable :: sat_wrk(:,:)
  real(8),allocatable :: sat_tmp(:,:)
  integer(4) :: num_fill, num_fill_old

  !-----------------------------------------------

  integer(4) :: mtin1, mtin2, mtin3
  integer(4) :: mtot1, mtot2, mtot3

  integer(4) :: iter, itermax

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
  ! SST(2) data

  character(256) :: sst2_base_dir, sst2_dir
  character(256) :: fin_base_sst2, fin_base_ice2
  character(256) :: fin_sst2
  integer(4) :: file_first_sst2(6)
  integer(4) :: file_intv_type_sst2
  integer(4) :: file_intv_sst2
  integer(4) :: total_rec_sst2
  integer(4) :: data_intv_sec_sst2
  integer(4) :: sst2_first(6)
  real(4)    :: undef_sst2
  logical    :: l_ymdir_sst2

  !------------------
  ! Surface data

  character(256) :: srf_base_dir, srf_dir
  character(256) :: sat_base_dir
  character(256) :: sph_base_dir
  character(256) :: slp_base_dir
  character(256) :: icj_base_dir
  character(256) :: fin_base_sat, fin_base_sph
  character(256) :: fin_base_slp, fin_base_icj

  character(256) :: fin_sat, fin_sph
  character(256) :: fin_slp, fin_icj

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

  type(type_libmxe_para) :: srfp, sst1p, sst2p
  type(type_libmxe_grid) :: srfg, sst1g, sst2g
  type(type_libmxe_topo) :: srft, sst1t, sst2t
  character(256) :: file_namelist_sst1, file_namelist_sst2
  character(256) :: file_namelist_srf

  integer(4),parameter :: lun = 10

  integer(4) :: ios, istat

  !---------------------------------------------------------------------

  real(8) :: hl1, hl2, hl3
  real(8) :: qsat, esat

  !---------------------------------------------------------------------

  namelist /nml_sst1data/ file_namelist_sst1, &
       &  sst1_base_dir, &
       &  fin_base_sst1, fin_base_ice1, &
       &  file_first_sst1, &
       &  file_intv_type_sst1, file_intv_sst1, total_rec_sst1, data_intv_sec_sst1, &
       &  sst1_first, undef_sst1, &
       &  l_ymdir_sst1

  namelist /nml_sst2data/ file_namelist_sst2, &
       &  sst2_base_dir, &
       &  fin_base_sst2, fin_base_ice2, &
       &  file_first_sst2, &
       &  file_intv_type_sst2, file_intv_sst2, total_rec_sst2, data_intv_sec_sst2, &
       &  sst2_first, undef_sst2, &
       &  l_ymdir_sst2

  namelist /nml_srfdata/ file_namelist_srf, &
       &  srf_base_dir, &
       &  sat_base_dir, &
       &  sph_base_dir, &
       &  slp_base_dir, &
       &  icj_base_dir, &
       &  fin_base_sat, fin_base_sph, &
       &  file_first_srf, &
       &  fin_base_slp, fin_base_icj, &
       &  file_intv_type_srf, file_intv_srf, total_rec_srf, data_intv_sec_srf, &
       &  srf_first, undef_srf, l_ymdir_srf, &
       &  alt_sat, alt_sph

  namelist /nml_calibsat_iceedge/ calc_start, calc_end, &
       &  l_leap_valid, &
       &  calib_base_dir, &
       &  itermax, &
       &  fout_base_sat, &
       &  fout_base_sph

  !-----------------------------------------------------------------------

  idmon(1:num_month) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

  open(lun,file='namelist.calibtmp10m_iceedge',iostat=istat,status='old')

  if ( istat /= 0 ) then
    write(6,*) ' Cannot find file : namelist.calibtmp10m_iceedge'
    stop
  end if

  ios = 0

  rewind(lun)
  file_intv_sst1 = 0
  read(lun, nml=nml_sst1data, iostat=istat)
  ios = ios + istat

  file_intv_sst2 = 0
  read(lun, nml=nml_sst2data, iostat=istat)
  ios = ios + istat

  rewind(lun)
  file_intv_srf = 0
  srf_base_dir  = 'no_dir' 
  sat_base_dir  = 'no_dir' 
  sph_base_dir  = 'no_dir' 
  slp_base_dir  = 'no_dir' 
  icj_base_dir  = 'no_dir' 
  read(lun, nml=nml_srfdata, iostat=istat)
  ios = ios + istat

  rewind(lun)
  read(lun, nml=nml_calibsat_iceedge, iostat=istat)
  ios = ios + istat

  if ( ios /= 0 ) then
    write(6,*) ' Read error : namelist.calibtmp10m_iceedge', ios
    stop
  end if

  close(lun)

  !----------------------------------------------------------------------
  ! SST (1)

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

  !------
  ! SST (2)

  call libmxe_para__register(sst2p,file_namelist=file_namelist_sst2)
  call libmxe_grid__register(sst2g,sst2p)
  call libmxe_topo__register(sst2t,sst2p)
  call libmxe_topo__aexl(sst2t,sst2p)
  imt2 = sst2p%imut
  jmt2 = sst2p%jmut
  kmt2 = sst2p%km
  allocate(alond2(1:imt2), alatd2(1:jmt2))
  alond2(1:imt2) = sst2g%lonu(1:imt2)
  alatd2(1:jmt2) = sst2g%latu(1:jmt2)
  allocate(aexl_sst2(1:imt2,1:jmt2))
  aexl_sst2(1:imt2,1:jmt2) = sst2t%aexl(1:imt2,1:jmt2,1)

  call force_process__ini(l_leap_valid)

  allocate(sst2(1:imt2,1:jmt2))
  allocate(ice2(1:imt2,1:jmt2))

  item_name='SST2'
  call register_file( &
       &  item_name, &
       &  sst2_file,  &
       &  fin_base_sst2,  &
       &  file_first_sst2, &
       &  file_intv_type_sst2, &
       &  file_intv_sst2, &
       &  total_rec_sst2, &
       &  data_intv_sec_sst2, &
       &  sst2_first, &
       &  imt2, jmt2,  &
       &  undef_sst2 )

  item_name='ICE2'
  call register_file( &
       &  item_name, &
       &  ice2_file,  &
       &  fin_base_ice2,  &
       &  file_first_sst2, &
       &  file_intv_type_sst2, &
       &  file_intv_sst2, &
       &  total_rec_sst2, &
       &  data_intv_sec_sst2, &
       &  sst2_first, &
       &  imt2, jmt2,  &
       &  undef_sst2 )

  !------

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
  allocate(slpf(1:imf,1:jmf), icjf(1:imf,1:jmf))
  allocate(rlhf(1:imf,1:jmf), roaf(1:imf,1:jmf))

  allocate(satc(1:imf,1:jmf), sphc(1:imf,1:jmf))
  allocate(sat_fill(1:imf,1:jmf))
  allocate(dat4f(1:imf,1:jmf))

  allocate(sst1f(1:imf,1:jmf), ice1f(1:imf,1:jmf))
  allocate(sst2f(1:imf,1:jmf), ice2f(1:imf,1:jmf))

  allocate(mask_fill(0:imf+1,0:jmf+1))
  allocate(mask_free(0:imf+1,0:jmf+1))
  allocate(mask_wrk(0:imf+1,0:jmf+1))
  allocate(mask_next(0:imf+1,0:jmf+1))
  allocate(sat_wrk (0:imf+1,0:jmf+1))
  allocate(sat_tmp (0:imf+1,0:jmf+1))

  mask_wrk(:,:) = 0.0d0
  mask_fill(:,:) = 0.0d0
  mask_free(:,:) = 0.0d0
  mask_next(:,:) = 0.0d0

  sat_wrk (:,:) = 0.0d0
  sat_tmp (:,:) = 0.0d0

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

  sst2_dir = trim(sst2_base_dir)
  call get_first_two_data(current_date,sst2_file,sst2_dir,l_ymdir_sst2)
  call get_first_two_data(current_date,ice2_file,sst2_dir,l_ymdir_sst2)

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

    call hintpl(sst1f,imf,jmf,alonf,alatf,sst1,imt1,jmt1,alond1,alatd1)
    call hintpl(ice1f,imf,jmf,alonf,alatf,ice1,imt1,jmt1,alond1,alatd1)

    !-----

    sst2_dir = trim(sst2_base_dir)
    call update_data(current_date,sst2_file,sst2_dir,l_ymdir_sst2)
    call update_data(current_date,ice2_file,sst2_dir,l_ymdir_sst2)
    do j = 1, jmt2
      do i = 1, imt2
        sst2(i,j) = sst2_file%dat_b(i,j) * sst2_file%br + sst2_file%dat_a(i,j) * sst2_file%ar
        ice2(i,j) = ice2_file%dat_b(i,j) * ice2_file%br + ice2_file%dat_a(i,j) * ice2_file%ar
        if ((sst2(i,j) < -2.0d0) .and. (ice2(i,j) > 0.0d0)) then
          sst1(i,j) = sst_freeze
        end if
        if ((ice2(i,j) < 0.0d0) .or. (ice2(i,j) > 1.0d0)) then
          write(6,*) ' WARNING: ice data is erroneous ', i, j, ice2(i,j), aexl_sst2(i,j)
          write(6,*) ' b ', ice2_file%dat_b(i,j)
          write(6,*) ' a ', ice2_file%dat_a(i,j)
          ice2(i,j) = max(ice2(i,j),0.0d0)
          ice2(i,j) = min(ice2(i,j),1.0d0)
        end if
        sst2(i,j) = sst2(i,j) * aexl_sst2(i,j)
        ice2(i,j) = ice2(i,j) * aexl_sst2(i,j)
      end do
    end do

    call hintpl(sst2f,imf,jmf,alonf,alatf,sst2,imt2,jmt2,alond2,alatd2)
    call hintpl(ice2f,imf,jmf,alonf,alatf,ice2,imt2,jmt2,alond2,alatd2)

    !-----

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
    satf(1:imf,1:jmf) = real(dat4f(1:imf,1:jmf),8) - tab
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

    if (trim(slp_base_dir) /= 'no_dir') then
      write(srf_dir,'(1a,1a,i4.4,i2.2)') trim(slp_base_dir),'/',current_file%year,current_file%month
    else
      write(srf_dir,'(1a,1a,i4.4,i2.2)') trim(srf_base_dir),'/',current_file%year,current_file%month
    end if
    write(fin_slp,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(srf_dir),'/',trim(fin_base_slp),'.', &
         & current_file%year,current_file%month,current_file%day,current_file%hour
    call open_file_direct(mtin3, fin_slp, lreclen)
    read(mtin3,rec=1) dat4f
    slpf(1:imf,1:jmf) = real(dat4f(1:imf,1:jmf),8) * 1.0d-2 ! [hPa]
    call close_file(mtin3)

    if (trim(icj_base_dir) /= 'no_dir') then
      write(srf_dir,'(1a,1a,i4.4,i2.2)') trim(icj_base_dir),'/',current_file%year,current_file%month
    else
      write(srf_dir,'(1a,1a,i4.4,i2.2)') trim(srf_base_dir),'/',current_file%year,current_file%month
    end if
    write(fin_icj,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(srf_dir),'/',trim(fin_base_icj),'.', &
         & current_file%year,current_file%month,current_file%day,current_file%hour
    call open_file_direct(mtin3, fin_icj, lreclen)
    read(mtin3,rec=1) dat4f
    icjf(1:imf,1:jmf) = real(dat4f(1:imf,1:jmf),8)
    call close_file(mtin3)

    rlhf(:,:) = 0.0d0

    do j = 1, jmf
      do i = 1, imf
        if (srft%aexl(i,j,1) == 1.0d0) then ! ocean grid
          if (icjf(i,j) >= 0.5d0) then ! on sea ice
            hl1 = (g1 + g2 * satf(i,j)) / (1.0d0 + g3 * satf(i,j)) + i1 * satf(i,j)
            hl2 = 10 ** hl1
            hl3 = 1.0d0 + 1.0d-6 * slpf(i,j)* (h1 + h2 * satf(i,j)**2)
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
        else ! same as water
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

    !---------------------

    current_year_start%year   = current_date%year
    current_year_start%month  = 1
    current_year_start%day    = 1
    current_year_start%hour   = 0
    current_year_start%minute = 0
    current_year_start%second = 0

    !l_leap_year = libmxe_calendar__l_leap_year( current_date%year )
    !current_year_sec = libmxe_calendar__diffsec(current_year_start,current_date,l_leap_valid)

    !------------------------------------------------
    ! smoothing in marginal sea ice zone

    satc(:,:) = satf(:,:)
    sphc(:,:) = sphf(:,:)

    mask_fill(:,:) = 0.0d0
    num_fill = 0
    do j = 1, jmf
      if (alatf(j) > 30.0d0) then
        do i = 1, imf
          if ((srft%aexl(i,j,1) == 1.0d0) .and. (ice2f(i,j) <= 0.95d0)) then
            if ((ice1f(i,j) > 0.1d0) .and. (ice1f(i,j) > ice2f(i,j))) then ! COBESST > HadISST
              mask_fill(i,j) = 1.0d0 ! need filling
              num_fill = num_fill + 1
            end if
          end if
        end do
      end if
    end do

    mask_wrk(:,:) = mask_fill(:,:)

    do j = 1, jmf
      do i = 1, imf
        hl3 = mask_wrk(i  ,j)   + mask_wrk(i+1,j  ) + mask_wrk(i  ,j+1) &
          & + mask_wrk(i-1,j)   + mask_wrk(i  ,j-1) + mask_wrk(i+1,j+1) &
          & + mask_wrk(i-1,j+1) + mask_wrk(i+1,j-1) + mask_wrk(i-1,j-1) 
        if ((mask_fill(i,j) == 0.0d0) .and. (hl3 * srft%aexl(i,j,1) > 0.0d0) .and. (ice1f(i,j) > 0.1d0)) then
          mask_fill(i,j) = 1.0d0 ! remove spikes
          num_fill = num_fill + 1
        end if
      end do
    end do

    mask_wrk(:,:) = mask_fill(:,:)

    mask_free(:,:) = 0.0d0
    do j = 1, jmf
      do i = 1, imf
        if ((srft%aexl(i,j,1) == 1.0d0) .and. (ice1f(i,j) <= 0.1d0)) then
          mask_free(i,j) = 1.0d0 ! available for filling
        end if
      end do
    end do
    ! cyclic
    mask_free(0,    1:jmf) = mask_free(imf,1:jmf)
    mask_free(imf+1,1:jmf) = mask_free(1  ,1:jmf)
    mask_next(:,:) = mask_free(:,:)

    sat_wrk(1:imf,1:jmf) = mask_free(1:imf,1:jmf) * satf(1:imf,1:jmf)

    sat_fill(1:imf,1:jmf) = -9.99e33

    LOOP_ITER: do iter = 1, itermax

      num_fill_old = num_fill

      if (num_fill > 0) then

        sat_tmp(1:imf,1:jmf) = sat_wrk(1:imf,1:jmf)
        sat_tmp(0,    1:jmf) = sat_tmp(imf,1:jmf)
        sat_tmp(imf+1,1:jmf) = sat_tmp(1  ,1:jmf)

        mask_free(1:imf,1:jmf) = mask_next(1:imf,1:jmf)
        mask_free(0,    1:jmf) = mask_free(imf,1:jmf)
        mask_free(imf+1,1:jmf) = mask_free(1  ,1:jmf)

        do j = 1, jmf
          do i = 1, imf
            if (mask_wrk(i,j) == 1.0d0) then
              hl1 = 12.0d0 * mask_free(i  ,j  ) &
                   & + 2.0d0 * mask_free(i+1,j  ) &
                   & + 2.0d0 * mask_free(i  ,j+1) &
                   & + 2.0d0 * mask_free(i-1,j  ) &
                   & + 2.0d0 * mask_free(i  ,j-1) &
                   & + 1.0d0 * mask_free(i+1,j+1) &
                   & + 1.0d0 * mask_free(i-1,j+1) &
                   & + 1.0d0 * mask_free(i+1,j-1) &
                   & + 1.0d0 * mask_free(i-1,j-1) 
              hl2 = 12.0d0 * mask_free(i  ,j  ) * sat_tmp(i  ,j  ) &
                   & + 2.0d0 * mask_free(i+1,j  ) * sat_tmp(i+1,j  ) &
                   & + 2.0d0 * mask_free(i  ,j+1) * sat_tmp(i  ,j+1) &
                   & + 2.0d0 * mask_free(i-1,j  ) * sat_tmp(i-1,j  ) &
                   & + 2.0d0 * mask_free(i  ,j-1) * sat_tmp(i  ,j-1) &
                   & + 1.0d0 * mask_free(i+1,j+1) * sat_tmp(i+1,j+1) &
                   & + 1.0d0 * mask_free(i-1,j+1) * sat_tmp(i-1,j+1) &
                   & + 1.0d0 * mask_free(i+1,j-1) * sat_tmp(i+1,j-1) &
                   & + 1.0d0 * mask_free(i-1,j-1) * sat_tmp(i-1,j-1)
              if (hl1 > 0.0d0) then
                num_fill = num_fill - 1
                mask_wrk(i,j) = 0.0d0
                sat_fill(i,j) = hl2 / hl1
                mask_next(i,j) = 1.0d0
                sat_wrk(i,j) = hl2 / hl1
!                write(6,*) sat_fill(i,j), satf(i,j)
              end if
            end if
          end do
        end do

        write(6,*) iter, num_fill

        if (num_fill_old == num_fill) exit LOOP_ITER

      end if

    end do LOOP_ITER

    if (num_fill > 0) then
      write(6,*) num_fill, ' points remain unfilled, please increase intermax '
    end if

    do j = 1, jmf
      do i = 1, imf
        if (mask_fill(i,j) == 1.0d0) then
          if (sat_fill(i,j) /= -9.99e33) then
            !hl1 = 0.2d0 * ice1f(i,j) 
            hl2 = 0.5d0 * (1.0d0 + tanh(5.0d0*(ice2f(i,j)-0.55d0))) ! 0.35-0.75 transition
            satc(i,j) = (1.0d0 - hl2) * sat_fill(i,j) + hl2 * satf(i,j)
!            write(6,*) sat_fill(i,j), satf(i,j)
          else
            satc(i,j) = satf(i,j)
          end if
        end if
      end do
    end do

    ! finalize

    sat_tmp(1:imf,1:jmf) = satc(1:imf,1:jmf)
    sat_wrk(1:imf,1:jmf) = satc(1:imf,1:jmf)
    ! cyclic
    sat_tmp(0,    1:jmf) = sat_tmp(imf,1:jmf)
    sat_tmp(imf+1,1:jmf) = sat_tmp(1  ,1:jmf)

    mask_wrk(:,:) = 0.0d0
    mask_wrk(1:imf,1:jmf) = srft%aexl(1:imf,1:jmf,1)
    mask_wrk(0,1:jmf) = mask_wrk(imf,1:jmf)
    mask_wrk(imf+1,1:jmf) = mask_wrk(1,1:jmf)

    do j = 1, jmf
      do i = 1, imf
        hl3 = mask_fill(i,j) + mask_fill(i+1,j) + mask_fill(i,j+1) &
          & + mask_fill(i-1,j) + mask_fill(i  ,j-1) + mask_fill(i+1,j+1)  &
          & + mask_fill(i-1,j+1) + mask_fill(i+1,j-1) + mask_fill(i-1,j-1) 
        if (hl3 * mask_wrk(i,j) > 0.0d0) then
          hl1 = 12.0d0 * mask_wrk(i  ,j  )  &
             & + 2.0d0 * mask_wrk(i+1,j  )  &
             & + 2.0d0 * mask_wrk(i  ,j+1)  &
             & + 2.0d0 * mask_wrk(i-1,j  )  &
             & + 2.0d0 * mask_wrk(i  ,j-1)  &
             & + 1.0d0 * mask_wrk(i+1,j+1)  &
             & + 1.0d0 * mask_wrk(i-1,j+1)  &
             & + 1.0d0 * mask_wrk(i+1,j-1)  &
             & + 1.0d0 * mask_wrk(i-1,j-1) 
          hl2 = 12.0d0 * mask_wrk(i  ,j  ) * sat_tmp(i  ,j  ) &
             & + 2.0d0 * mask_wrk(i+1,j  ) * sat_tmp(i+1,j  ) &
             & + 2.0d0 * mask_wrk(i  ,j+1) * sat_tmp(i  ,j+1) &
             & + 2.0d0 * mask_wrk(i-1,j  ) * sat_tmp(i-1,j  ) &
             & + 2.0d0 * mask_wrk(i  ,j-1) * sat_tmp(i  ,j-1) &
             & + 1.0d0 * mask_wrk(i+1,j+1) * sat_tmp(i+1,j+1) &
             & + 1.0d0 * mask_wrk(i-1,j+1) * sat_tmp(i-1,j+1) &
             & + 1.0d0 * mask_wrk(i+1,j-1) * sat_tmp(i+1,j-1) &
             & + 1.0d0 * mask_wrk(i-1,j-1) * sat_tmp(i-1,j-1)
          sat_wrk(i,j) = hl2 / hl1
        end if
      end do
    end do

    satc(1:imf,1:jmf) = sat_wrk(1:imf,1:jmf)

    !------------------------------------------------

    do j = 1, jmf
      do i = 1, imf
        if (satc(i,j) /= satf(i,j)) then ! if smoothing applied
          if (ice1f(i,j) > 0.0d0) then
            hl1 = (g1 + g2 * satc(i,j)) / (1.0d0 + g3 * satc(i,j)) + i1 * satc(i,j)
            hl2 = 10 ** hl1
            hl3 = 1.0d0 + 1.0d-6 * slpf(i,j)* (h1 + h2 * satc(i,j)**2)
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
          sphc(i,j) = max(hl2 / hl1, sphmin)
        end if
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
    satc(1:imf,1:jmf) = satc(1:imf,1:jmf) + tab
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

end program calibration_surface_temperature_iceedge
