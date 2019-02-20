! -*-F90-*-
!----------- diagflux_latent_fusion_interannual_on_cobesst.F90 ----------------
!  Information:
!     Diangnose bulk flux using SST and Surface Atmospheric State
!-------------------------------------------------------------------
program rain_snow_conversion

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
  real(8), parameter :: rfus = 3.347d5 ! [J kg-1]

  !------------

  character(len=16) :: item_name

  type(type_force) :: sst2_file
  type(type_force) :: ice2_file

  !------------

  type(type_force) :: rain_file
  type(type_force) :: snow_file

  type(type_calendar) :: start_date
  type(type_calendar) :: current_date
  type(type_calendar) :: next_date
  type(type_calendar) :: end_date

  integer(4) :: calc_start(6)
  integer(4) :: calc_end(6)

  !---------------------------------------------------
  ! Rain and Snow fall flux

  integer(4) :: imf, jmf, kmf
  real(8),allocatable :: alonf(:), alatf(:)

  real(8),allocatable :: rain(:,:), snow(:,:)
  real(8),allocatable :: aexl(:,:)

  real(8),allocatable :: sstf(:,:), icef(:,:)
  real(8),parameter :: undef_out = -9.99e33

  !---------------------------------------------------
  ! SST and ICE

  ! SST (2)

  real(8),allocatable :: sst2f(:,:), ice2f(:,:)
  integer(4) :: imt2, jmt2, kmt2
  real(8),allocatable :: alond2(:), alatd2(:)
  real(8),allocatable :: aexl_sst2(:,:)
  real(8),allocatable :: sst2(:,:)
  real(8),allocatable :: ice2(:,:)

  ! following items are diagnosed in this program

  integer(4) :: int_hist
  logical :: l_histout

  real(8),allocatable :: r2s(:,:), s2r(:,:)

  integer(4),allocatable :: num_hist(:,:)
  real(8),allocatable :: r2sm(:,:), s2rm(:,:)

  logical :: l_all_precip_liquid

  !-----------------------------------------------

  integer(4) :: mtin1, mtin2, mtin3
  integer(4) :: mtot1, mtot2, mtot3

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

  character(256) :: pcp_dir
  character(256) :: pcp_base_dir
  character(256) :: rain_base_dir
  character(256) :: snow_base_dir

  character(256) :: fin_base_rain
  character(256) :: fin_base_snow

  character(256) :: fin_rain, fin_snow

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
  character(256) :: fout_base_r2s, fout_base_s2r
  character(256) :: fout_r2s, fout_s2r

  integer(4) :: mtot_w10n
  integer(4) :: irec_w10n

  !------------------

  logical :: l_leap_valid

  integer(4) :: nstep

  integer(4) :: lreclen

  integer(4) :: i, j, k, l, m, n

  !----------------------------------------------------------------------

  type(type_libmxe_para) :: pcpp, sst2p
  type(type_libmxe_grid) :: pcpg, sst2g
  type(type_libmxe_topo) :: pcpt, sst2t
  character(256) :: file_namelist_sst2
  character(256) :: file_namelist_pcp

  integer(4),parameter :: lun = 10

  integer(4) :: ios, istat

  real(8) :: area_earth_orgdata
  real(8) :: hl1

  !--------------------------------------------------------------------

  namelist /nml_sst2data/ file_namelist_sst2, &
       &  sst2_base_dir, &
       &  fin_base_sst2, fin_base_ice2, &
       &  file_first_sst2, &
       &  file_intv_type_sst2, file_intv_sst2, total_rec_sst2, data_intv_sec_sst2, &
       &  sst2_first, undef_sst2, &
       &  l_ymdir_sst2

  namelist /nml_pcpdata/ file_namelist_pcp, &
       &  pcp_base_dir, &
       &  rain_base_dir, &
       &  snow_base_dir, &
       &  fin_base_rain, fin_base_snow, &
       &  file_first_pcp, &
       &  file_intv_type_pcp, file_intv_pcp, total_rec_pcp, data_intv_sec_pcp, &
       &  pcp_first, undef_pcp, l_ymdir_pcp

  namelist /nml_rain2snow/ calc_start, calc_end, &
       &  l_leap_valid, &
       &  int_hist, &
       &  flx_base_dir, &
       &  fout_base_r2s, fout_base_s2r, &
       &  l_all_precip_liquid

  !-----------------------------------------------------------------------

  open(lun,file='namelist.rain2snow_cobe',iostat=istat,status='old')

  if ( istat /= 0 ) then
    write(6,*) ' Cannot find file : namelist.rain2snow_cobe'
    stop
  end if

  ios = 0

  rewind(lun)

  file_intv_sst2 = 0
  read(lun, nml=nml_sst2data, iostat=istat)
  ios = ios + istat

  rewind(lun)
  file_intv_pcp = 0
  pcp_base_dir  = 'no_dir' 
  rain_base_dir = 'no_dir' 
  snow_base_dir = 'no_dir' 

  read(lun, nml=nml_pcpdata, iostat=istat)
  ios = ios + istat

  rewind(lun)

  l_all_precip_liquid = .false.
  read(lun, nml=nml_rain2snow, iostat=istat)
  ios = ios + istat

  if ( ios /= 0 ) then
    write(6,*) ' Read error : namelist.rain2snow_cobe', ios
    stop
  end if
  close(lun)

  !----------------------------------------------------------------------
  ! SST (2) : COBESST

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

  !-----------------------------------------------------------

  call libmxe_para__register(pcpp,file_namelist=file_namelist_pcp)

  imf = pcpp%imut
  jmf = pcpp%jmut
  kmf = pcpp%km

!  if (imf /= imt2 .or. jmf /= jmt2) then
!    write(6,*) 'Grid specification seems to be inconsistent '
!    write(6,*) ' imf  : jmf  = ' , imf, ' : ', jmf
!    write(6,*) ' imt2 : jmt2 = ' , imt2, ' : ', jmt2
!    stop
!  end if

  call libmxe_grid__register(pcpg,pcpp)

  allocate(alonf(1:imf), alatf(1:jmf))
  alonf(1:imf) = pcpg%lonu(1:imf)
  alatf(1:jmf) = pcpg%latu(1:jmf)

  call libmxe_topo__register(pcpt,pcpp)
  call libmxe_topo__aexl(pcpt,pcpp)

  allocate(aexl(1:imf,1:jmf))
  aexl(1:imf,1:jmf) = pcpt%aexl(1:imf,1:jmf,1)

  allocate(rain(1:imf,1:jmf), snow(1:imf,1:jmf))

  item_name='RAIN'
  call register_file( &
       &  item_name, &
       &  rain_file,  &
       &  fin_base_rain,  &
       &  file_first_pcp, &
       &  file_intv_type_pcp, &
       &  file_intv_pcp, &
       &  total_rec_pcp, &
       &  data_intv_sec_pcp, &
       &  pcp_first, &
       &  imf, jmf,  &
       &  undef_pcp )

  item_name='SNOW'
  call register_file( &
       &  item_name, &
       &  snow_file,  &
       &  fin_base_snow, &
       &  file_first_pcp, &
       &  file_intv_type_pcp, &
       &  file_intv_pcp, &
       &  total_rec_pcp, &
       &  data_intv_sec_pcp, &
       &  pcp_first, &
       &  imf, jmf,  &
       &  undef_pcp )

  !-------------------------------------------------
  ! Check

  area_earth_orgdata = 0.0d0

  do j = 1, jmt2
    do i = 1, imt2
      area_earth_orgdata = area_earth_orgdata &
           & + sst2g%a_br(i,j) + sst2g%a_bl(i,j) &
           & + sst2g%a_tr(i,j) + sst2g%a_tl(i,j)
    end do
  end do

  write(6,*) ' Area of the Earth (original data) ', area_earth_orgdata
  write(6,*) ' Area of the Earth (theoretical)   ' &
                & , 4.0d0 * pi * radius * radius

  !--------------------------------------------------------------------
  ! diagnosed

  allocate(r2s(1:imf,1:jmf), s2r(1:imf,1:jmf))
  r2s(1:imf,1:jmf) = 0.0d0
  s2r(1:imf,1:jmf) = 0.0d0

  ! sampling

  allocate(num_hist(1:imf,1:jmf))
  allocate(r2sm(1:imf,1:jmf), s2rm(1:imf,1:jmf))

  !--------------------------------------------------------------------

  allocate(sstf (1:imf,1:jmf), icef (1:imf,1:jmf))
  allocate(sst2f(1:imf,1:jmf), ice2f(1:imf,1:jmf))

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

  sst2_dir = trim(sst2_base_dir)
  call get_first_two_data(current_date,sst2_file,sst2_dir,l_ymdir_sst2)
  call get_first_two_data(current_date,ice2_file,sst2_dir,l_ymdir_sst2)

  if (trim(rain_base_dir) /= 'no_dir') then
    pcp_dir = trim(rain_base_dir)
  else
    pcp_dir = trim(pcp_base_dir)
  end if
  call get_first_two_data(current_date,rain_file,pcp_dir,l_ymdir_pcp)

  if (trim(snow_base_dir) /= 'no_dir') then
    pcp_dir = trim(snow_base_dir)
  else
    pcp_dir = trim(pcp_base_dir)
  end if
  call get_first_two_data(current_date,snow_file,pcp_dir,l_ymdir_pcp)

  nstep = 0
  num_hist(:,:) = 0
  r2sm (:,:) = 0.0d0
  s2rm (:,:) = 0.0d0

  write(6,*) 
  write(6,*) ' MAIN LOOP START '

  LOOP_SURF_FORCE: do while ( .true. )

    write(6,*) ' This step ', current_date%year, current_date%month, current_date%day, &
         & current_date%hour, current_date%minute, data_intv_sec_pcp
    nstep = nstep + 1

    !----------------------------------------------------------------------------
    ! COBESST

    sst2_dir = trim(sst2_base_dir)
    call update_data(current_date,sst2_file,sst2_dir,l_ymdir_sst2)
    call update_data(current_date,ice2_file,sst2_dir,l_ymdir_sst2)
    do j = 1, jmt2
      do i = 1, imt2
        sst2(i,j) = sst2_file%dat_b(i,j) * sst2_file%br + sst2_file%dat_a(i,j) * sst2_file%ar
        ice2(i,j) = ice2_file%dat_b(i,j) * ice2_file%br + ice2_file%dat_a(i,j) * ice2_file%ar
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

    call hintpl_mask(sst2f,imf,jmf,alonf,alatf,undef_out,sst2,imt2,jmt2,alond2,alatd2,aexl_sst2)
    call hintpl_mask(ice2f,imf,jmf,alonf,alatf,undef_out,ice2,imt2,jmt2,alond2,alatd2,aexl_sst2)

    do j = 1, jmf
      do i = 1, imf
        if (ice2f(i,j) /= undef_out) then
          icef(i,j) = ice2f(i,j)
        else
          icef(i,j) = 0.0d0
        end if
      end do
    end do

    !---------------------------------------------------------------------

    if (trim(rain_base_dir) /= 'no_dir') then
      pcp_dir = trim(rain_base_dir)
    else
      pcp_dir = trim(pcp_base_dir)
    end if
    call update_data(current_date,rain_file,pcp_dir,l_ymdir_pcp)

    if (trim(snow_base_dir) /= 'no_dir') then
      pcp_dir = trim(snow_base_dir)
    else
      pcp_dir = trim(pcp_base_dir)
    end if
    call update_data(current_date,snow_file,pcp_dir,l_ymdir_pcp)

    do j = 1, jmf
      do i = 1, imf
        rain(i,j) = rain_file%dat_b(i,j) * rain_file%br + rain_file%dat_a(i,j) * rain_file%ar
        snow(i,j) = snow_file%dat_b(i,j) * snow_file%br + snow_file%dat_a(i,j) * snow_file%ar 
      end do
    end do

    !------

    do j = 1, jmf
      do i = 1, imf
        if (aexl(i,j) == 1.0d0)then
          if (l_all_precip_liquid) then
            r2s(i,j) = - icef(i,j) * rfus * (rain(i,j) + snow(i,j))
          else
            r2s(i,j) = - icef(i,j) * rfus * rain(i,j)
          end if
          s2r(i,j) = (1.d0 - icef(i,j)) * rfus * snow(i,j)
        end if
      end do
    end do

    do j = 1, jmf
      do i = 1, imf
        if (aexl(i,j) == 1.0d0) then
          num_hist(i,j) = num_hist(i,j) + 1
          r2sm(i,j) = r2sm(i,j) + r2s(i,j)
          s2rm(i,j) = s2rm(i,j) + s2r(i,j)
        end if
      end do
    end do

    !---------------------

    next_date = libmxe_calendar__addsec(current_date, data_intv_sec_pcp, l_use_leap)

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
            r2sm (i,j) = r2sm (i,j) / real(num_hist(i,j),8)
            s2rm (i,j) = s2rm (i,j) / real(num_hist(i,j),8)
          else
            r2sm (i,j) = real(undef_pcp,8)
            s2rm (i,j) = real(undef_pcp,8)
          end if
        end do
      end do

      ! output data

      lreclen = 4 * imf * jmf

      !-----------------------------------------------

      if (int_hist == -1) then
        write(flx_dir,'(1a)') trim(flx_base_dir)
      else
        write(flx_dir,'(1a,1a,i4.4,i2.2)') trim(flx_base_dir),'/',current_date%year, current_date%month
      end if

      if (int_hist == -1) then
        write(fout_r2s,'(1a,1a,1a,1a,i4.4,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_r2s),'.',current_date%year, current_date%month
      else
        write(fout_r2s,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_r2s),'.',&
             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
      end if
      call open_file_direct(mtot1, fout_r2s, lreclen)
      write(mtot1,rec=1) real(r2sm(1:imf,1:jmf),4)
      call close_file(mtot1)

      if (int_hist == -1) then
        write(fout_s2r,'(1a,1a,1a,1a,i4.4,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_s2r),'.',current_date%year, current_date%month
      else
        write(fout_s2r,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_s2r),'.', &
             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
      end if
      call open_file_direct(mtot1, fout_s2r, lreclen)
      write(mtot1,rec=1) real(s2rm(1:imf,1:jmf),4)
      call close_file(mtot1)

!----------------------------------------------------

      num_hist(:,:) = 0
      r2sm(:,:) = 0.0d0
      s2rm(:,:) = 0.0d0

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

end program rain_snow_conversion
