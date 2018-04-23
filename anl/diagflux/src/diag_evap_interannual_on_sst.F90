! -*-F90-*-
!----------- diagflux_evap_interannual_on_sst.F90 ------------------
!  Information:
!     Diangnose downward fluxes using SST and Surface Atmospheric State
!-------------------------------------------------------------------
program diagnosis_evaporation

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

  !------------

  character(len=16) :: item_name

  type(type_force) :: sst_file
  type(type_force) :: ice_file

  type(type_force) :: evp_file ! [kg/m^2/s]

  type(type_calendar) :: start_date
  type(type_calendar) :: current_date
  type(type_calendar) :: next_date
  type(type_calendar) :: end_date

  integer(4) :: calc_start(6)
  integer(4) :: calc_end(6)

  ! Evaporation

  integer(4) :: imp, jmp, kmp
  real(8),allocatable :: alonp(:), alatp(:)
  real(8),allocatable :: evapp(:,:)

  ! SST

  integer(4) :: imt, jmt, kmt
  real(8),allocatable :: alond(:), alatd(:)

  real(8),allocatable :: aexl(:,:)
  real(8),allocatable :: sst(:,:)
  real(8),allocatable :: ice(:,:)

  real(8),allocatable :: evap(:,:)

  ! following items are diagnosed in this program

  integer(4) :: int_hist
  logical :: l_histout

  integer(4),allocatable :: num_hist(:,:)
  real(8),allocatable :: evapm(:,:)

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
  ! Evaporation data

  character(256) :: evp_base_dir, evp_dir
  character(256) :: fin_base_evp

  character(256) :: fin_evp

  integer(4) :: file_first_evp(6)
  integer(4) :: file_intv_type_evp
  integer(4) :: file_intv_evp
  integer(4) :: total_rec_evp
  integer(4) :: data_intv_sec_evp
  integer(4) :: evp_first(6)
  real(4)    :: undef_evp
  logical    :: l_ymdir_evp

  !------------------
  ! Output

  character(256) :: flx_base_dir, flx_dir
  character(256) :: fout_base_evap

  character(256) :: fout_evap

  !------------------

  logical :: l_leap_valid

  integer(4) :: nstep

  integer(4) :: lreclen

  integer(4) :: i, j, k, l, m, n

  !----------------------------------------------------------------------

  type(type_libmxe_para) :: sstp, evpp
  type(type_libmxe_grid) :: sstg, evpg
  type(type_libmxe_topo) :: sstt
  character(256) :: file_namelist_sst
  character(256) :: file_namelist_evp

  integer(4),parameter :: lun = 10

  integer(4) :: ios, istat

  real(8) :: area_earth_orgdata
  real(8) :: hl1
  real(8) :: factor_evap ! convert to standard MKS units
  real(8) :: factor_model_evap ! convert to model's units

  !--------------------------------------------------------------------

  type(type_table) :: orgo2sst, orga2sst
  character(len=256) :: file_table_ocn, file_table_all
  character(len=256) :: name_tmp
  logical :: l_lin_evp
  logical :: l_need_table
  logical :: l_ocn_evp

  !--------------------------------------------------------------------

  namelist /nml_sstdata/ file_namelist_sst, &
       &  sst_base_dir, &
       &  fin_base_sst, fin_base_ice, &
       &  file_first_sst, &
       &  file_intv_type_sst, file_intv_sst, total_rec_sst, data_intv_sec_sst, &
       &  sst_first, undef_sst, &
       &  l_ymdir_sst

  namelist /nml_evpdata/ file_namelist_evp, &
       &  evp_base_dir, &
       &  fin_base_evp, &
       &  file_first_evp, &
       &  file_intv_type_evp, file_intv_evp, total_rec_evp, data_intv_sec_evp, &
       &  evp_first, undef_evp, &
       &  l_ymdir_evp

  namelist /nml_diagevp/ calc_start, calc_end, &
       &  l_leap_valid, &
       &  int_hist, &
       &  flx_base_dir, &
       &  fout_base_evap, &
       &  factor_evap

  namelist /nml_table_ocn/ file_table_ocn

  namelist /nml_table_all/ file_table_all

  namelist /nml_intpol/ l_lin_evp, l_ocn_evp

  !-----------------------------------------------------------------------

  !factor_model_evap = 1.0d-1 / rho ! [kg/m^2/s] => [cm/s]
  ! Keep MKS
  factor_model_evap = 1.0d0

  ! factor to convert input data to MKS

  factor_evap = 1.0d0

  open(lun,file='namelist.diagevp',iostat=istat,status='old')

  if ( istat /= 0 ) then
    write(6,*) ' Cannot find file : namelist.diagevp'
    stop
  end if

  ios = 0

  rewind(lun)
  file_intv_sst = 0
  read(lun, nml=nml_sstdata, iostat=istat)
  ios = ios + istat

  rewind(lun)
  file_intv_evp = 0
  read(lun, nml=nml_evpdata, iostat=istat)
  ios = ios + istat

  rewind(lun)
  read(lun, nml=nml_diagevp, iostat=istat)
  ios = ios + istat

  rewind(lun)
  read(lun, nml=nml_intpol, iostat=istat)
  ios = ios + istat

  if ( ios /= 0 ) then
    write(6,*) ' Read error : namelist.diagevp', ios
    stop
  end if

  close(lun)

  if (l_lin_evp) then
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

  allocate(evap(1:imt,1:jmt))

  allocate(num_hist(1:imt,1:jmt))
  allocate(evapm(1:imt,1:jmt))
  
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

  !-----------------------------------------------------------

  call libmxe_para__register(evpp,file_namelist=file_namelist_evp)
  call libmxe_grid__register(evpg,evpp)
  imp = evpp%imut
  jmp = evpp%jmut
  kmp = evpp%km
  allocate(alonp(1:imp), alatp(1:jmp))
  alonp(1:imp) = evpg%lonu(1:imp)
  alatp(1:jmp) = evpg%latu(1:jmp)

  allocate(evapp(1:imp,1:jmp))

  item_name='EVP'
  call register_file( &
       &  item_name, &
       &  evp_file,  &
       &  fin_base_evp,  &
       &  file_first_evp, &
       &  file_intv_type_evp, &
       &  file_intv_evp, &
       &  total_rec_evp, &
       &  data_intv_sec_evp, &
       &  evp_first, &
       &  imp, jmp,  &
       &  undef_evp )

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

    open(lun,file='namelist.diagevp',iostat=istat,status='old')

    rewind(lun)
    read(lun, nml=nml_table_ocn, iostat=istat)
    if (istat == 0) then
      name_tmp = "Ocean to Ocean table"
      call mapping_table__ini(  &
           & name_tmp,          &
           & file_table_ocn, orgo2sst,   &
           & imp, jmp, kmp, &
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
           & imp, jmp, kmp, &
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

  evp_dir = trim(evp_base_dir)
  call get_first_two_data(current_date,evp_file,evp_dir,l_ymdir_evp)

  nstep = 0
  num_hist(:,:) = 0

  evapm(:,:) = 0.0d0

  write(6,*) 
  write(6,*) ' MAIN LOOP START '

  LOOP_SURF_FORCE: do while ( .true. )

    write(6,*) ' This step ', current_date%year, current_date%month, current_date%day, &
         & current_date%hour, current_date%minute, data_intv_sec_evp
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
          write(6,*) ' WARNING: ice data is erroneous ', i, j, ice(i,j), aexl(i,j)
          write(6,*) ' b ', ice_file%dat_b(i,j)
          write(6,*) ' a ', ice_file%dat_a(i,j)
          ice(i,j) = max(ice(i,j),0.0d0)
          ice(i,j) = min(ice(i,j),1.0d0)
        end if
      end do
    end do

    call update_data(current_date,evp_file,evp_dir,l_ymdir_evp)

    ! Covert to the unit used in the model

    do j = 1, jmp
      do i = 1, imp
        evapp(i,j) = (evp_file%dat_b(i,j) * evp_file%br + evp_file%dat_a(i,j) * evp_file%ar) * factor_evap * factor_model_evap
      end do
    end do

    call hintpl(evap,imt,jmt,alond,alatd,evapp,imp,jmp,alonp,alatp)

    !------

    do j = 1, jmt
      do i = 1, imt
        if (aexl(i,j) == 1.0d0) then
          num_hist(i,j) = num_hist(i,j) + 1
          hl1 = 1.0d0 - ice(i,j)
          evapm(i,j) = evapm(i,j) + evap(i,j) * hl1
        end if
      end do
    end do

    !---------------------

    next_date = libmxe_calendar__addsec(current_date, data_intv_sec_evp, l_use_leap)

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
            evapm(i,j) = evapm(i,j) / real(num_hist(i,j),8)
          else
            evapm(i,j) = real(undef_evp,8)
          end if
        end do
      end do

      ! output data

      lreclen = 4 * imt * jmt

      write(flx_dir,'(1a)') trim(flx_base_dir)

      if (int_hist == -1) then
        write(fout_evap,'(1a,1a,1a,1a,i4.4,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_evap),'.',current_date%year, current_date%month
      else
        write(fout_evap,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_evap),'.',&
             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
      end if
      call open_file_direct(mtot1, fout_evap, lreclen)
      write(mtot1,rec=1) real(evapm(1:imt,1:jmt),4)
      call close_file(mtot1)

      num_hist(:,:) = 0
      evapm(:,:) = 0.0d0

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

end program diagnosis_evaporation
