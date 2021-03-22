! -*-F90-*-
!--------------- diagflux_net_rad_interannual_on_sst.F90 ------------------
! Information:
!   Diangnose net radiation fluxes
!----------------------------------------------------------------------
program diagnosis_net_radiation_flux

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

  ! for albedo
  real(8), parameter :: cnst1ly = 0.069d0
  real(8), parameter :: cnst2ly = 0.011d0

  !------------

  character(len=16) :: item_name

  type(type_force) :: sst_file
  type(type_force) :: ice_file

  type(type_force) :: dsw_file, usw_file ! [W/m^2]
  type(type_force) :: dlw_file, ulw_file ! [W/m^2]

  type(type_calendar) :: start_date
  type(type_calendar) :: current_date
  type(type_calendar) :: next_date
  type(type_calendar) :: end_date

  integer(4) :: calc_start(6)
  integer(4) :: calc_end(6)

  ! Radiation

  integer(4) :: imr, jmr, kmr
  real(8),allocatable :: alonr(:), alatr(:)
  real(8),allocatable :: dswr(:,:), dlwr(:,:)
  real(8),allocatable :: uswr(:,:), ulwr(:,:)
  real(8),allocatable :: nswr(:,:), nlwr(:,:)

  ! SST

  integer(4) :: imt, jmt, kmt
  real(8),allocatable :: alond(:), alatd(:)

  real(8),allocatable :: aexl(:,:)
  real(8),allocatable :: sst(:,:)
  real(8),allocatable :: ice(:,:)

  real(8),allocatable :: nsw(:,:), nlw(:,:)
  real(8),allocatable :: area_recv(:,:)

  ! following items are diagnosed in this program

  integer(4) :: int_hist
  logical :: l_histout

  integer(4),allocatable :: num_hist(:,:)
  real(8),allocatable :: nswm(:,:), nlwm(:,:)

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
  ! Radiation data

  character(256) :: drad_base_dir, drad_dir
  character(256) :: urad_base_dir, urad_dir
  character(256) :: fin_base_dsw, fin_base_dlw
  character(256) :: fin_base_usw, fin_base_ulw

  character(256) :: fin_dsw, fin_dlw
  character(256) :: fin_usw, fin_ulw

  integer(4) :: file_first_rad(6)
  integer(4) :: file_intv_type_rad
  integer(4) :: file_intv_rad
  integer(4) :: total_rec_rad
  integer(4) :: data_intv_sec_rad
  integer(4) :: rad_first(6)
  real(4)    :: undef_rad
  logical    :: l_ymdir_rad

  !------------------
  ! Output

  character(256) :: flx_base_dir, flx_dir
  character(256) :: fout_base_nswrf
  character(256) :: fout_base_nlwrf

  character(256) :: fout_nswrf
  character(256) :: fout_nlwrf

  !------------------

  logical :: l_leap_valid

  integer(4) :: nstep

  integer(4) :: lreclen

  integer(4) :: i, j, k, l, m, n

  !----------------------------------------------------------------------

  type(type_libmxe_para) :: sstp, radp
  type(type_libmxe_grid) :: sstg, radg
  type(type_libmxe_topo) :: sstt
  character(256) :: file_namelist_sst
  character(256) :: file_namelist_rad

  integer(4),parameter :: lun = 10

  integer(4) :: ios, istat

  real(8) :: area_earth_orgdata
  real(8) :: hl1
  real(8) :: factor_swrf, factor_lwrf             ! convert to standard MKS units
  real(8) :: factor_model_swrf, factor_model_lwrf ! convert to model's units

  !--------------------------------------------------------------------

  type(type_table) :: orgo2sst, orga2sst
  character(len=256) :: file_table_ocn, file_table_all
  character(len=256) :: name_tmp
  logical :: l_lin_swrf, l_lin_lwrf
  logical :: l_need_table
  logical :: l_ocn_swrf, l_ocn_lwrf

  !--------------------------------------------------------------------

  namelist /nml_sstdata/ file_namelist_sst, &
       &  sst_base_dir, &
       &  fin_base_sst, fin_base_ice, &
       &  file_first_sst, &
       &  file_intv_type_sst, file_intv_sst, total_rec_sst, data_intv_sec_sst, &
       &  sst_first, undef_sst, &
       &  l_ymdir_sst

  namelist /nml_raddata/ file_namelist_rad, &
       &  drad_base_dir, &
       &  urad_base_dir, &
       &  fin_base_dsw, fin_base_dlw, &
       &  fin_base_usw, fin_base_ulw, &
       &  file_first_rad, &
       &  file_intv_type_rad, file_intv_rad, total_rec_rad, data_intv_sec_rad, &
       &  rad_first, undef_rad, &
       &  l_ymdir_rad

  namelist /nml_diagnetrad/ calc_start, calc_end, &
       &  l_leap_valid, &
       &  int_hist, &
       &  flx_base_dir, &
       &  fout_base_nswrf, fout_base_nlwrf, &
       &  factor_swrf, factor_lwrf

  namelist /nml_table_ocn/ file_table_ocn

  namelist /nml_table_all/ file_table_all

  namelist /nml_intpol/ l_lin_swrf, l_lin_lwrf, &
       &                l_ocn_swrf, l_ocn_lwrf

  !-----------------------------------------------------------------------

  !factor_model_swrf = 1.0d3         ! [W/m^2] => cgs
  !factor_model_lwrf = 1.0d3         ! [W/m^2] => cgs

  ! Keep MKS
  factor_model_swrf = 1.0d0
  factor_model_lwrf = 1.0d0

  ! factor to convert input data to MKS

  factor_swrf  = 1.0d0
  factor_lwrf  = 1.0d0

  open(lun,file='namelist.diagnetrad',iostat=istat,status='old')

  if ( istat /= 0 ) then
    write(6,*) ' Cannot find file : namelist.diagnetrad'
    stop
  end if

  ios = 0

  rewind(lun)
  file_intv_sst = 0
  read(lun, nml=nml_sstdata, iostat=istat)
  ios = ios + istat

  rewind(lun)
  file_intv_rad = 0
  read(lun, nml=nml_raddata, iostat=istat)
  ios = ios + istat

  rewind(lun)
  read(lun, nml=nml_diagnetrad, iostat=istat)
  ios = ios + istat

  rewind(lun)
  read(lun, nml=nml_intpol, iostat=istat)
  ios = ios + istat

  if ( ios /= 0 ) then
    write(6,*) ' Read error : namelist.diagnetrad', ios
    stop
  end if

  close(lun)

  if (l_lin_swrf .and. l_lin_lwrf) then
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

  allocate(nsw(1:imt,1:jmt), nlw(1:imt,1:jmt))
  allocate(area_recv(1:imt,1:jmt))

  allocate(num_hist(1:imt,1:jmt))
  allocate(nswm(1:imt,1:jmt), nlwm(1:imt,1:jmt))
  
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

  call libmxe_para__register(radp,file_namelist=file_namelist_rad)
  call libmxe_grid__register(radg,radp)
  imr = radp%imut
  jmr = radp%jmut
  kmr = radp%km
  allocate(alonr(1:imr), alatr(1:jmr))
  alonr(1:imr) = radg%lonu(1:imr)
  alatr(1:jmr) = radg%latu(1:jmr)

  allocate(dswr(1:imr,1:jmr), dlwr(1:imr,1:jmr))
  allocate(uswr(1:imr,1:jmr), ulwr(1:imr,1:jmr))
  allocate(nswr(1:imr,1:jmr), nlwr(1:imr,1:jmr))

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

  item_name='USW'
  call register_file( &
       &  item_name, &
       &  usw_file,  &
       &  fin_base_usw,  &
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

  item_name='ULW'
  call register_file( &
       &  item_name, &
       &  ulw_file,  &
       &  fin_base_ulw,  &
       &  file_first_rad, &
       &  file_intv_type_rad, &
       &  file_intv_rad, &
       &  total_rec_rad, &
       &  data_intv_sec_rad, &
       &  rad_first, &
       &  imr, jmr,  &
       &  undef_rad )

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

    open(lun,file='namelist.diagnetrad',iostat=istat,status='old')

    rewind(lun)
    read(lun, nml=nml_table_ocn, iostat=istat)
    if (istat == 0) then
      name_tmp = "Ocean to Ocean table"
      write(6,*) ' reading ', trim(name_tmp)
      call mapping_table__ini(  &
           & name_tmp,          &
           & file_table_ocn, orgo2sst,   &
           & imr, jmr, kmr, &
           & imt, jmt, kmt  &
           & )
    end if

    rewind(lun)
    read(lun, nml=nml_table_all, iostat=istat)
    if (istat == 0) then
      name_tmp = "All to All table"
      write(6,*) ' reading ', trim(name_tmp)
      call mapping_table__ini(  &
           & name_tmp,          &
           & file_table_all, orga2sst,   &
           & imr, jmr, kmr, &
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

  drad_dir = trim(drad_base_dir)
  call get_first_two_data(current_date,dsw_file,drad_dir,l_ymdir_rad)
  call get_first_two_data(current_date,dlw_file,drad_dir,l_ymdir_rad)
  urad_dir = trim(urad_base_dir)
  call get_first_two_data(current_date,usw_file,urad_dir,l_ymdir_rad)
  call get_first_two_data(current_date,ulw_file,urad_dir,l_ymdir_rad)

  nstep = 0
  num_hist(:,:) = 0

  nswm(:,:) = 0.0d0
  nlwm(:,:) = 0.0d0

  write(6,*) 
  write(6,*) ' MAIN LOOP START '

  LOOP_SURF_FORCE: do while ( .true. )

    write(6,*) ' This step ', current_date%year, current_date%month, current_date%day, &
         & current_date%hour, current_date%minute, data_intv_sec_rad
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

    call update_data(current_date,dsw_file,drad_dir,l_ymdir_rad)
    call update_data(current_date,dlw_file,drad_dir,l_ymdir_rad)
    call update_data(current_date,usw_file,urad_dir,l_ymdir_rad)
    call update_data(current_date,ulw_file,urad_dir,l_ymdir_rad)

    ! Covert to the unit used in the model

    do j = 1, jmr
      do i = 1, imr
        dswr(i,j) = (dsw_file%dat_b(i,j) * dsw_file%br + dsw_file%dat_a(i,j) * dsw_file%ar) * factor_swrf * factor_model_swrf
        uswr(i,j) = (usw_file%dat_b(i,j) * usw_file%br + usw_file%dat_a(i,j) * usw_file%ar) * factor_swrf * factor_model_swrf
        dlwr(i,j) = (dlw_file%dat_b(i,j) * dlw_file%br + dlw_file%dat_a(i,j) * dlw_file%ar) * factor_lwrf * factor_model_lwrf
        ulwr(i,j) = (ulw_file%dat_b(i,j) * ulw_file%br + ulw_file%dat_a(i,j) * ulw_file%ar) * factor_lwrf * factor_model_lwrf
        nswr(i,j) = dswr(i,j) - uswr(i,j)
        nlwr(i,j) = dlwr(i,j) - ulwr(i,j)
      end do
    end do

    if (l_lin_swrf) then
      call hintpl(nsw,imt,jmt,alond,alatd,nswr,imr,jmr,alonr,alatr)
    else
      if (l_ocn_swrf) then
        call mapping_table__main(          &
             & orgo2sst,                   &
             & nsw,area_recv,imt,jmt,kmt,  &
             & nswr,imr,jmr,kmr,dsw_file%dundef, &
             & 1, imr, 1, jmr              &
             & )
      else
        call mapping_table__main(          &
             & orga2sst,                   &
             & nsw,area_recv,imt,jmt,kmt,  &
             & nswr,imr,jmr,kmr,dsw_file%dundef, &
             & 1, imr, 1, jmr              &
             & )
      end if
    end if

    if (l_lin_lwrf) then
      call hintpl(nlw,imt,jmt,alond,alatd,nlwr,imr,jmr,alonr,alatr)
    else
      if (l_ocn_lwrf) then
        call mapping_table__main(          &
             & orgo2sst,                   &
             & nlw,area_recv,imt,jmt,kmt,  &
             & nlwr,imr,jmr,kmr,dlw_file%dundef, &
             & 1, imr, 1, jmr              &
             & )
      else
        call mapping_table__main(          &
             & orga2sst,                   &
             & nlw,area_recv,imt,jmt,kmt,  &
             & nlwr,imr,jmr,kmr,dlw_file%dundef, &
             & 1, imr, 1, jmr              &
             & )
      end if
    end if

    !---------------------

    do j = 1, jmt
      do i = 1, imt
        if (aexl(i,j) == 1.0d0) then
          num_hist(i,j) = num_hist(i,j) + 1
          hl1 = 1.0d0 - ice(i,j)
          nswm(i,j) = nswm(i,j) + nsw(i,j) * hl1
          nlwm(i,j) = nlwm(i,j) + nlw(i,j) * hl1
        end if
      end do
    end do

    !---------------------

    next_date = libmxe_calendar__addsec(current_date, data_intv_sec_rad, l_use_leap)

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
            nswm (i,j) = nswm (i,j) / real(num_hist(i,j),8)
            nlwm (i,j) = nlwm (i,j) / real(num_hist(i,j),8)
          else
            nswm (i,j) = real(undef_rad,8)
            nlwm (i,j) = real(undef_rad,8)
          end if
        end do
      end do

      ! output data

      lreclen = 4 * imt * jmt

      write(flx_dir,'(1a)') trim(flx_base_dir)

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

      num_hist(:,:) = 0
      nswm(:,:) = 0.0d0
      nlwm(:,:) = 0.0d0

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

end program diagnosis_net_radiation_flux
