! -*-F90-*-
!----------- diagflux_hturb_interannual_on_sst.F90 ------------------
!  Information:
!     Diangnose downward fluxes using SST and Surface Atmospheric State
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

  !------------

  character(len=16) :: item_name

  type(type_force) :: sst_file
  type(type_force) :: ice_file

  type(type_force) :: lhf_file ! [W/m^2]
  type(type_force) :: shf_file ! [W/m^2]

  type(type_calendar) :: start_date
  type(type_calendar) :: current_date
  type(type_calendar) :: next_date
  type(type_calendar) :: end_date

  integer(4) :: calc_start(6)
  integer(4) :: calc_end(6)

  ! Latent and Sensible heat flux

  integer(4) :: imp, jmp, kmp
  real(8),allocatable :: alonp(:), alatp(:)
  real(8),allocatable :: lhfp(:,:), shfp(:,:)
  real(8),allocatable :: area_recv(:,:)

  ! SST grid

  integer(4) :: imt, jmt, kmt
  real(8),allocatable :: alond(:), alatd(:)

  real(8),allocatable :: aexl(:,:)
  real(8),allocatable :: sst(:,:)
  real(8),allocatable :: ice(:,:)

  real(8),allocatable :: lhf(:,:), shf(:,:)

  ! following items are diagnosed in this program

  integer(4) :: int_hist
  logical :: l_histout

  integer(4),allocatable :: num_hist(:,:)
  real(8),allocatable :: lhfm(:,:)
  real(8),allocatable :: shfm(:,:)

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
  ! Turbulent heat flux data

  character(256) :: hturb_base_dir, hturb_dir
  character(256) :: fin_base_lhf, fin_base_shf

  character(256) :: fin_lhf, fin_shf

  integer(4) :: file_first_hturb(6)
  integer(4) :: file_intv_type_hturb
  integer(4) :: file_intv_hturb
  integer(4) :: total_rec_hturb
  integer(4) :: data_intv_sec_hturb
  integer(4) :: hturb_first(6)
  real(4)    :: undef_hturb
  logical    :: l_ymdir_hturb

  !------------------
  ! Output

  character(256) :: flx_base_dir, flx_dir
  character(256) :: fout_base_lhf, fout_base_shf

  character(256) :: fout_lhf, fout_shf

  !------------------

  logical :: l_leap_valid

  integer(4) :: nstep

  integer(4) :: lreclen

  integer(4) :: i, j, k, l, m, n

  !----------------------------------------------------------------------

  type(type_libmxe_para) :: sstp, hturbp
  type(type_libmxe_grid) :: sstg, hturbg
  type(type_libmxe_topo) :: sstt
  character(256) :: file_namelist_sst
  character(256) :: file_namelist_hturb

  integer(4),parameter :: lun = 10

  integer(4) :: ios, istat

  real(8) :: area_earth_orgdata
  real(8) :: hl1
  real(8) :: factor_hturb ! convert to standard MKS units
  real(8) :: factor_model_hturb ! convert to model's units

  !--------------------------------------------------------------------

  type(type_table) :: orgo2sst, orga2sst
  character(len=256) :: file_table_ocn, file_table_all
  character(len=256) :: name_tmp
  logical :: l_lin_lhf, l_lin_shf
  logical :: l_need_table
  logical :: l_ocn_lhf, l_ocn_shf

  !--------------------------------------------------------------------

  namelist /nml_sstdata/ file_namelist_sst, &
       &  sst_base_dir, &
       &  fin_base_sst, fin_base_ice, &
       &  file_first_sst, &
       &  file_intv_type_sst, file_intv_sst, total_rec_sst, data_intv_sec_sst, &
       &  sst_first, undef_sst, &
       &  l_ymdir_sst

  namelist /nml_hturbdata/ file_namelist_hturb, &
       &  hturb_base_dir, &
       &  fin_base_lhf, &
       &  fin_base_shf, &
       &  file_first_hturb, &
       &  file_intv_type_hturb, file_intv_hturb, total_rec_hturb, data_intv_sec_hturb, &
       &  hturb_first, undef_hturb, &
       &  l_ymdir_hturb

  namelist /nml_diaghturb/ calc_start, calc_end, &
       &  l_leap_valid, &
       &  int_hist, &
       &  flx_base_dir, &
       &  fout_base_lhf, &
       &  fout_base_shf, &
       &  factor_hturb

  namelist /nml_table_ocn/ file_table_ocn

  namelist /nml_table_all/ file_table_all

  namelist /nml_intpol/ l_lin_lhf, l_lin_shf, &
       &                l_ocn_lhf, l_ocn_shf

  !-----------------------------------------------------------------------

  factor_model_hturb = 1.0d0

  ! factor to convert input data to MKS

  factor_hturb = 1.0d0

  open(lun,file='namelist.diaghturb',iostat=istat,status='old')

  if ( istat /= 0 ) then
    write(6,*) ' Cannot find file : namelist.diaghturb'
    stop
  end if

  ios = 0

  rewind(lun)
  file_intv_sst = 0
  read(lun, nml=nml_sstdata, iostat=istat)
  ios = ios + istat

  rewind(lun)
  file_intv_hturb = 0
  read(lun, nml=nml_hturbdata, iostat=istat)
  ios = ios + istat

  rewind(lun)
  read(lun, nml=nml_diaghturb, iostat=istat)
  ios = ios + istat

  rewind(lun)
  read(lun, nml=nml_intpol, iostat=istat)
  ios = ios + istat

  if ( ios /= 0 ) then
    write(6,*) ' Read error : namelist.diaghturb', ios
    stop
  end if

  close(lun)

  if (l_lin_lhf .and. l_lin_shf) then
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

  allocate(area_recv(1:imt,1:jmt))

  allocate(lhf(1:imt,1:jmt),shf(1:imt,1:jmt))

  allocate(num_hist(1:imt,1:jmt))
  allocate(lhfm(1:imt,1:jmt),shfm(1:imt,1:jmt))
  
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

  call libmxe_para__register(hturbp,file_namelist=file_namelist_hturb)
  call libmxe_grid__register(hturbg,hturbp)
  imp = hturbp%imut
  jmp = hturbp%jmut
  kmp = hturbp%km
  allocate(alonp(1:imp), alatp(1:jmp))
  alonp(1:imp) = hturbg%lonu(1:imp)
  alatp(1:jmp) = hturbg%latu(1:jmp)

  allocate(lhfp(1:imp,1:jmp))
  allocate(shfp(1:imp,1:jmp))

  item_name='LHF'
  call register_file( &
       &  item_name, &
       &  lhf_file,  &
       &  fin_base_lhf,  &
       &  file_first_hturb, &
       &  file_intv_type_hturb, &
       &  file_intv_hturb, &
       &  total_rec_hturb, &
       &  data_intv_sec_hturb, &
       &  hturb_first, &
       &  imp, jmp,  &
       &  undef_hturb )

  item_name='SHF'
  call register_file( &
       &  item_name, &
       &  shf_file,  &
       &  fin_base_shf,  &
       &  file_first_hturb, &
       &  file_intv_type_hturb, &
       &  file_intv_hturb, &
       &  total_rec_hturb, &
       &  data_intv_sec_hturb, &
       &  hturb_first, &
       &  imp, jmp,  &
       &  undef_hturb )

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

    open(lun,file='namelist.diaghturb',iostat=istat,status='old')

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

  hturb_dir = trim(hturb_base_dir)
  call get_first_two_data(current_date,lhf_file,hturb_dir,l_ymdir_hturb)
  call get_first_two_data(current_date,shf_file,hturb_dir,l_ymdir_hturb)

  nstep = 0
  num_hist(:,:) = 0

  lhfm(:,:) = 0.0d0
  shfm(:,:) = 0.0d0

  write(6,*) 
  write(6,*) ' MAIN LOOP START '

  LOOP_SURF_FORCE: do while ( .true. )

    write(6,*) ' This step ', current_date%year, current_date%month, current_date%day, &
         & current_date%hour, current_date%minute, data_intv_sec_hturb
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

    call update_data(current_date,lhf_file,hturb_dir,l_ymdir_hturb)
    call update_data(current_date,shf_file,hturb_dir,l_ymdir_hturb)

    ! Covert to the unit used in the model

    do j = 1, jmp
      do i = 1, imp
        lhfp(i,j) = (lhf_file%dat_b(i,j) * lhf_file%br + lhf_file%dat_a(i,j) * lhf_file%ar) * factor_hturb * factor_model_hturb
        shfp(i,j) = (shf_file%dat_b(i,j) * shf_file%br + shf_file%dat_a(i,j) * shf_file%ar) * factor_hturb * factor_model_hturb
      end do
    end do

    if (l_lin_lhf) then
      call hintpl(lhf,imt,jmt,alond,alatd,lhfp,imp,jmp,alonp,alatp)
    else
      if (l_ocn_lhf) then
        call mapping_table__main(          &
             & orgo2sst,                   &
             & lhf,area_recv,imt,jmt,kmt,  &
             & lhfp,imp,jmp,kmp,lhf_file%dundef, &
             & 1, imp, 1, jmp              &
             & )
      else
        call mapping_table__main(          &
             & orga2sst,                   &
             & lhf,area_recv,imt,jmt,kmt,  &
             & lhfp,imp,jmp,kmp,lhf_file%dundef, &
             & 1, imp, 1, jmp              &
             & )
      end if
    end if

    if (l_lin_shf) then
      call hintpl(shf,imt,jmt,alond,alatd,shfp,imp,jmp,alonp,alatp)
    else
      if (l_ocn_shf) then
        call mapping_table__main(          &
             & orgo2sst,                   &
             & shf,area_recv,imt,jmt,kmt,  &
             & shfp,imp,jmp,kmp,shf_file%dundef, &
             & 1, imp, 1, jmp              &
             & )
      else
        call mapping_table__main(          &
             & orga2sst,                   &
             & shf,area_recv,imt,jmt,kmt,  &
             & shfp,imp,jmp,kmp,shf_file%dundef, &
             & 1, imp, 1, jmp              &
             & )
      end if
    end if

    !------

    do j = 1, jmt
      do i = 1, imt
        if (aexl(i,j) == 1.0d0) then
          num_hist(i,j) = num_hist(i,j) + 1
          hl1 = 1.0d0 - ice(i,j)
          lhfm(i,j) = lhfm(i,j) + lhf(i,j) * hl1
          shfm(i,j) = shfm(i,j) + shf(i,j) * hl1
        end if
      end do
    end do

    !---------------------

    next_date = libmxe_calendar__addsec(current_date, data_intv_sec_hturb, l_use_leap)

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
            lhfm(i,j) = lhfm(i,j) / real(num_hist(i,j),8)
            shfm(i,j) = shfm(i,j) / real(num_hist(i,j),8)
          else
            lhfm(i,j) = real(undef_hturb,8)
            shfm(i,j) = real(undef_hturb,8)
          end if
        end do
      end do

      ! output data

      lreclen = 4 * imt * jmt

      write(flx_dir,'(1a)') trim(flx_base_dir)

      if (int_hist == -1) then
        write(fout_lhf,'(1a,1a,1a,1a,i4.4,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_lhf),'.',current_date%year, current_date%month
      else
        write(fout_lhf,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_lhf),'.',&
             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
      end if
      call open_file_direct(mtot1, fout_lhf, lreclen)
      write(mtot1,rec=1) real(lhfm(1:imt,1:jmt),4)
      call close_file(mtot1)

      if (int_hist == -1) then
        write(fout_shf,'(1a,1a,1a,1a,i4.4,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_shf),'.',current_date%year, current_date%month
      else
        write(fout_lhf,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2,i2.2)') &
             & trim(flx_dir),'/',trim(fout_base_shf),'.',&
             & current_date%year, current_date%month, current_date%day, current_date%hour, current_date%minute
      end if
      call open_file_direct(mtot1, fout_shf, lreclen)
      write(mtot1,rec=1) real(shfm(1:imt,1:jmt),4)
      call close_file(mtot1)

      num_hist(:,:) = 0
      lhfm(:,:) = 0.0d0
      shfm(:,:) = 0.0d0

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
