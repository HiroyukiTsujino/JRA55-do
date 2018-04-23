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

  type(type_force) :: sst_file
  type(type_force) :: ice_file

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
  real(8),allocatable :: slpf(:,:)
  real(8),allocatable :: roaf(:,:), rlhf(:,:)

  real(8),allocatable :: satc(:,:), sphc(:,:)

  real(8),allocatable :: sstf(:,:), icef(:,:)
  real(4),allocatable :: dat4f(:,:)

  real(8),allocatable :: mask_wrk(:,:)
  real(8),allocatable :: mask_ice(:,:)
  real(8),allocatable :: sat_wrk(:,:)
  real(8),allocatable :: sat_tmp(:,:)

  ! SST

  integer(4) :: imt, jmt, kmt
  real(8),allocatable :: alond(:), alatd(:)
  real(8),allocatable :: aexl_sst(:,:)
  real(8),allocatable :: sst(:,:)
  real(8),allocatable :: ice(:,:)

  !-----------------------------------------------

  integer(4) :: mtin1, mtin2, mtin3
  integer(4) :: mtot1, mtot2, mtot3

  integer(4) :: iter, itermax

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
  character(256) :: slp_base_dir
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

  type(type_libmxe_para) :: srfp, sstp
  type(type_libmxe_grid) :: srfg, sstg
  type(type_libmxe_topo) :: srft, sstt
  character(256) :: file_namelist_sst, file_namelist_srf

  integer(4),parameter :: lun = 10

  integer(4) :: ios, istat

  !---------------------------------------------------------------------

  real(8) :: hl1, hl2, hl3
  real(8) :: qsat, esat

  !---------------------------------------------------------------------

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
       &  slp_base_dir, &
       &  fin_base_sat, fin_base_sph, &
       &  file_first_srf, &
       &  fin_base_slp,               &
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
  file_intv_sst = 0
  read(lun, nml=nml_sstdata, iostat=istat)
  ios = ios + istat

  rewind(lun)
  file_intv_srf = 0
  srf_base_dir  = 'no_dir' 
  sat_base_dir  = 'no_dir' 
  sph_base_dir  = 'no_dir' 
  slp_base_dir  = 'no_dir' 
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
  allocate(aexl_sst(1:imt,1:jmt))
  aexl_sst(1:imt,1:jmt) = sstt%aexl(1:imt,1:jmt,1)

  call force_process__ini(l_leap_valid)

  allocate(sst(1:imt,1:jmt))
  allocate(ice(1:imt,1:jmt))

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
  call libmxe_topo__register(srft,srfp)
  call libmxe_topo__aexl(srft,srfp)
  imf = srfp%imut
  jmf = srfp%jmut
  kmf = srfp%km
  allocate(alonf(1:imf), alatf(1:jmf))
  alonf(1:imf) = srfg%lonu(1:imf)
  alatf(1:jmf) = srfg%latu(1:jmf)

  allocate(satf(1:imf,1:jmf), sphf(1:imf,1:jmf))
  allocate(slpf(1:imf,1:jmf))
  allocate(rlhf(1:imf,1:jmf), roaf(1:imf,1:jmf))

  allocate(satc(1:imf,1:jmf), sphc(1:imf,1:jmf))
  allocate(dat4f(1:imf,1:jmf))

  allocate(sstf(1:imf,1:jmf), icef(1:imf,1:jmf))

  allocate(mask_wrk(0:imf+1,0:jmf+1))
  allocate(mask_ice(0:imf+1,0:jmf+1))
  allocate(sat_wrk (0:imf+1,0:jmf+1))
  allocate(sat_tmp (0:imf+1,0:jmf+1))

  mask_wrk(:,:) = 0.0d0
  mask_wrk(1:imf,1:jmf) = srft%aexl(1:imf,1:jmf,1)
  mask_wrk(0,1:jmf) = mask_wrk(imf,1:jmf)
  mask_wrk(imf+1,1:jmf) = mask_wrk(1,1:jmf)

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

  sst_dir = trim(sst_base_dir)
  call get_first_two_data(current_date,sst_file,sst_dir,l_ymdir_sst)
  call get_first_two_data(current_date,ice_file,sst_dir,l_ymdir_sst)

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
        if ((ice(i,j) < 0.0d0) .or. (ice(i,j) > 1.0d0)) then
          write(6,*) ' WARNING: ice data is erroneous ', i, j, ice(i,j), aexl_sst(i,j)
          write(6,*) ' b ', ice_file%dat_b(i,j)
          write(6,*) ' a ', ice_file%dat_a(i,j)
          ice(i,j) = max(ice(i,j),0.0d0)
          ice(i,j) = min(ice(i,j),1.0d0)
        end if
        sst(i,j) = sst(i,j) * aexl_sst(i,j)
        ice(i,j) = ice(i,j) * aexl_sst(i,j)
      end do
    end do

    call hintpl(sstf,imf,jmf,alonf,alatf,sst,imt,jmt,alond,alatd)
    call hintpl(icef,imf,jmf,alonf,alatf,ice,imt,jmt,alond,alatd)

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

    rlhf(:,:) = 0.0d0

    do j = 1, jmf
      do i = 1, imf
        if (srft%aexl(i,j,1) == 1.0d0) then ! ocean grid
          if (icef(i,j) > 0.0d0) then ! on sea ice
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

    mask_ice(:,:) = mask_wrk(:,:)

    do j = 1, jmf
      do i = 1, imf
        if ((icef(i,j) < 0.5d0) .or. (icef(i,j) > 0.99d0))  then
!        if ((icef(i,j) < 0.55d0) .or. (icef(i,j) > 0.99d0))  then
!        if ((icef(i,j) < 0.15d0) .or. (icef(i,j) > 0.99d0))  then
          mask_ice(i,j) = 0.0d0
        end if
      end do
    end do

    ! cyclic
    mask_ice(0,    1:jmf) = mask_ice(imf,1:jmf)
    mask_ice(imf+1,1:jmf) = mask_ice(1  ,1:jmf)
    sat_wrk(1:imf,1:jmf) = satf(1:imf,1:jmf)

    LOOP_ITER: do iter = 1, itermax

      sat_tmp(1:imf,1:jmf) = satc(1:imf,1:jmf)
      ! cyclic
      sat_tmp(0,    1:jmf) = sat_tmp(imf,1:jmf)
      sat_tmp(imf+1,1:jmf) = sat_tmp(1  ,1:jmf)

      do j = 1, jmf
        do i = 1, imf
          if (mask_ice(i,j) > 0.0d0) then
!            hl1 = 12.0d0 * mask_wrk(i  ,j  ) * max(1.0d0-icef(i  ,j  )+0.0d0,1.0d0) &
!               & + 2.0d0 * mask_wrk(i+1,j  ) * max(1.0d0-icef(i+1,j  )+0.0d0,1.0d0) &
!               & + 2.0d0 * mask_wrk(i  ,j+1) * max(1.0d0-icef(i  ,j+1)+0.0d0,1.0d0) &
!               & + 2.0d0 * mask_wrk(i-1,j  ) * max(1.0d0-icef(i-1,j  )+0.0d0,1.0d0) &
!               & + 2.0d0 * mask_wrk(i  ,j-1) * max(1.0d0-icef(i  ,j-1)+0.0d0,1.0d0) &
!               & + 1.0d0 * mask_wrk(i+1,j+1) * max(1.0d0-icef(i+1,j+1)+0.0d0,1.0d0) &
!               & + 1.0d0 * mask_wrk(i-1,j+1) * max(1.0d0-icef(i-1,j+1)+0.0d0,1.0d0) &
!               & + 1.0d0 * mask_wrk(i+1,j-1) * max(1.0d0-icef(i+1,j-1)+0.0d0,1.0d0) &
!               & + 1.0d0 * mask_wrk(i-1,j-1) * max(1.0d0-icef(i-1,j-1)+0.0d0,1.0d0)
!            hl2 = 12.0d0 * mask_wrk(i  ,j  ) * max(1.0d0-icef(i  ,j  )+0.0d0,1.0d0) * sat_tmp(i  ,j  ) &
!               & + 2.0d0 * mask_wrk(i+1,j  ) * max(1.0d0-icef(i+1,j  )+0.0d0,1.0d0) * sat_tmp(i+1,j  ) &
!               & + 2.0d0 * mask_wrk(i  ,j+1) * max(1.0d0-icef(i  ,j+1)+0.0d0,1.0d0) * sat_tmp(i  ,j+1) &
!               & + 2.0d0 * mask_wrk(i-1,j  ) * max(1.0d0-icef(i-1,j  )+0.0d0,1.0d0) * sat_tmp(i-1,j  ) &
!               & + 2.0d0 * mask_wrk(i  ,j-1) * max(1.0d0-icef(i  ,j-1)+0.0d0,1.0d0) * sat_tmp(i  ,j-1) &
!               & + 1.0d0 * mask_wrk(i+1,j+1) * max(1.0d0-icef(i+1,j+1)+0.0d0,1.0d0) * sat_tmp(i+1,j+1) &
!               & + 1.0d0 * mask_wrk(i-1,j+1) * max(1.0d0-icef(i-1,j+1)+0.0d0,1.0d0) * sat_tmp(i-1,j+1) &
!               & + 1.0d0 * mask_wrk(i+1,j-1) * max(1.0d0-icef(i+1,j-1)+0.0d0,1.0d0) * sat_tmp(i+1,j-1) &
!               & + 1.0d0 * mask_wrk(i-1,j-1) * max(1.0d0-icef(i-1,j-1)+0.0d0,1.0d0) * sat_tmp(i-1,j-1)
            hl1 = 12.0d0 * mask_wrk(i  ,j  ) * max((1.0d0-icef(i  ,j  )**2),1.0d0) &
               & + 2.0d0 * mask_wrk(i+1,j  ) * max((1.0d0-icef(i+1,j  )**2),1.0d0) &
               & + 2.0d0 * mask_wrk(i  ,j+1) * max((1.0d0-icef(i  ,j+1)**2),1.0d0) &
               & + 2.0d0 * mask_wrk(i-1,j  ) * max((1.0d0-icef(i-1,j  )**2),1.0d0) &
               & + 2.0d0 * mask_wrk(i  ,j-1) * max((1.0d0-icef(i  ,j-1)**2),1.0d0) &
               & + 1.0d0 * mask_wrk(i+1,j+1) * max((1.0d0-icef(i+1,j+1)**2),1.0d0) &
               & + 1.0d0 * mask_wrk(i-1,j+1) * max((1.0d0-icef(i-1,j+1)**2),1.0d0) &
               & + 1.0d0 * mask_wrk(i+1,j-1) * max((1.0d0-icef(i+1,j-1)**2),1.0d0) &
               & + 1.0d0 * mask_wrk(i-1,j-1) * max((1.0d0-icef(i-1,j-1)**2),1.0d0)
            hl2 = 12.0d0 * mask_wrk(i  ,j  ) * max((1.0d0-icef(i  ,j  )**2),1.0d0) * sat_tmp(i  ,j  ) &
               & + 2.0d0 * mask_wrk(i+1,j  ) * max((1.0d0-icef(i+1,j  )**2),1.0d0) * sat_tmp(i+1,j  ) &
               & + 2.0d0 * mask_wrk(i  ,j+1) * max((1.0d0-icef(i  ,j+1)**2),1.0d0) * sat_tmp(i  ,j+1) &
               & + 2.0d0 * mask_wrk(i-1,j  ) * max((1.0d0-icef(i-1,j  )**2),1.0d0) * sat_tmp(i-1,j  ) &
               & + 2.0d0 * mask_wrk(i  ,j-1) * max((1.0d0-icef(i  ,j-1)**2),1.0d0) * sat_tmp(i  ,j-1) &
               & + 1.0d0 * mask_wrk(i+1,j+1) * max((1.0d0-icef(i+1,j+1)**2),1.0d0) * sat_tmp(i+1,j+1) &
               & + 1.0d0 * mask_wrk(i-1,j+1) * max((1.0d0-icef(i-1,j+1)**2),1.0d0) * sat_tmp(i-1,j+1) &
               & + 1.0d0 * mask_wrk(i+1,j-1) * max((1.0d0-icef(i+1,j-1)**2),1.0d0) * sat_tmp(i+1,j-1) &
               & + 1.0d0 * mask_wrk(i-1,j-1) * max((1.0d0-icef(i-1,j-1)**2),1.0d0) * sat_tmp(i-1,j-1)
            sat_wrk(i,j) = mask_ice(i,j) * hl2 / hl1
          end if
        end do
      end do

      satc(1:imf,1:jmf) = sat_wrk(1:imf,1:jmf)

    end do LOOP_ITER

    ! finalize

    sat_tmp(1:imf,1:jmf) = satc(1:imf,1:jmf)
    sat_wrk(1:imf,1:jmf) = satc(1:imf,1:jmf)
    ! cyclic
    sat_tmp(0,    1:jmf) = sat_tmp(imf,1:jmf)
    sat_tmp(imf+1,1:jmf) = sat_tmp(1  ,1:jmf)

    do j = 1, jmf
      do i = 1, imf
        hl3 = mask_ice(i,j) + mask_ice(i+1,j) + mask_ice(i,j+1) &
          & + mask_ice(i-1,j) + mask_ice(i  ,j-1) + mask_ice(i+1,j+1)  &
          & + mask_ice(i-1,j+1) + mask_ice(i+1,j-1) + mask_ice(i-1,j-1) 
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
          if (icef(i,j) > 0.0d0) then
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
