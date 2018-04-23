! -*-F90-*-
!------------------ filter_wind_interannual.F90 ---------------------
!  Information:
!     Apply zonally 1-2-1 filter to wind field
!-------------------------------------------------------------------
program filter_surface_wind

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

  !------------

  type(type_calendar) :: start_date
  type(type_calendar) :: end_date

  type(type_calendar) :: current_date
  type(type_calendar) :: current_file
  type(type_calendar) :: next_date
  type(type_calendar) :: next_file

  integer(4) :: calc_start(6)
  integer(4) :: calc_end(6)

  ! Surface Atmospheric State

  integer(4) :: imf, jmf, kmf
  real(8),allocatable :: alonf(:), alatf(:)
  real(8),allocatable :: usf(:,:), vsf(:,:)
  real(8),allocatable :: usc(:,:), vsc(:,:)
  real(4),allocatable :: dat4f(:,:)

  !-----------------------------------------------

  integer(4) :: mtin1, mtin2, mtin3
  integer(4) :: mtot1, mtot2, mtot3

  !-----------------------------------------------
  ! Surface data

  character(256) :: srf_base_dir, srf_dir
  character(256) :: fin_base_us, fin_base_vs
  character(256) :: fin_us, fin_vs

  integer(4) :: file_first_srf(6)
  integer(4) :: file_intv_type_srf
  integer(4) :: file_intv_srf
  integer(4) :: total_rec_srf
  integer(4) :: data_intv_sec_srf
  integer(4) :: srf_first(6)
  real(4)    :: undef_srf
  logical    :: l_ymdir_srf

  real(8)    :: alt_wind

  !------------------
  ! Output

  character(256) :: calib_base_dir, calib_dir
  character(256) :: fout_base_us, fout_base_vs
  character(256) :: fout_us, fout_vs

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

  character(256) :: file_mask_jra55

  real(8),allocatable :: mask_jra(:,:)

  integer(4) :: iw, ie
  real(8) :: hl1, hl2, hl3

  !--------------------------------------------------------------------

  namelist /nml_srfdata/ file_namelist_srf, &
       &  srf_base_dir, &
       &  fin_base_us, fin_base_vs,   &
       &  file_first_srf, &
       &  file_intv_type_srf, file_intv_srf, total_rec_srf, data_intv_sec_srf, &
       &  srf_first, undef_srf, l_ymdir_srf, &
       &  alt_wind

  namelist /nml_filterwind/ calc_start, calc_end, &
       &  l_leap_valid,    &
       &  file_mask_jra55, &
       &  calib_base_dir,  &
       &  fout_base_us,    &
       &  fout_base_vs

  !-----------------------------------------------------------------------

  open(lun,file='namelist.filterwind',iostat=istat,status='old')

  if ( istat /= 0 ) then
    write(6,*) ' Cannot find file : namelist.filterwind'
    stop
  end if

  ios = 0

  rewind(lun)
  file_intv_srf = 0
  read(lun, nml=nml_srfdata, iostat=istat)
  ios = ios + istat

  rewind(lun)
  read(lun, nml=nml_filterwind, iostat=istat)
  ios = ios + istat

  if ( ios /= 0 ) then
    write(6,*) ' Read error : namelist.filterwind', ios
    stop
  end if

  close(lun)

  call force_process__ini(l_leap_valid)

  !----------------------------------------------------------------------

  call libmxe_para__register(srfp,file_namelist=file_namelist_srf)
  call libmxe_grid__register(srfg,srfp)
  imf = srfp%imut
  jmf = srfp%jmut
  kmf = srfp%km
  allocate(alonf(1:imf), alatf(1:jmf))
  alonf(1:imf) = srfg%lonu(1:imf)
  alatf(1:jmf) = srfg%latu(1:jmf)

  allocate(usf(1:imf,1:jmf), vsf(1:imf,1:jmf))
  allocate(usc(1:imf,1:jmf), vsc(1:imf,1:jmf))
  allocate(dat4f(1:imf,1:jmf))

  allocate(mask_jra(1:imf,1:jmf))

  !--------------------------------------------------------------------

  lreclen = 4 * imf * jmf
  call open_file_direct(mtin1, file_mask_jra55, lreclen)
  read(mtin1,rec=1) dat4f
  mask_jra(1:imf,1:jmf) = real(dat4f(1:imf,1:jmf),8)
  call close_file(mtin1)

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

    write(srf_dir,'(1a,1a,i4.4,i2.2)') trim(srf_base_dir),'/',current_file%year,current_file%month
    write(fin_us,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(srf_dir),'/',trim(fin_base_us),'.', &
         & current_file%year,current_file%month,current_file%day,current_file%hour
    write(fin_vs,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(srf_dir),'/',trim(fin_base_vs),'.', &
         & current_file%year,current_file%month,current_file%day,current_file%hour

    lreclen = 4 * imf * jmf
    call open_file_direct(mtin1, fin_us, lreclen)
    read(mtin1,rec=1) dat4f
    usf(1:imf,1:jmf) = real(dat4f(1:imf,1:jmf),8)
    call close_file(mtin1)

    lreclen = 4 * imf * jmf
    call open_file_direct(mtin2, fin_vs, lreclen)
    read(mtin2,rec=1) dat4f
    vsf(1:imf,1:jmf) = real(dat4f(1:imf,1:jmf),8)
    call close_file(mtin2)

    !---------------------

    usc(:,:) = usf(:,:)
    vsc(:,:) = vsf(:,:)

    do j = 2, jmf - 1
      do i = 1, imf
        iw = i - 1
        ie = i + 1
        if (i == 1) iw = imf
        if (i == imf) ie = 1
        !hl1 = mask_jra(iw,j) + mask_jra(ie,j) + mask_jra(i,j-1) + mask_jra(i,j+1) + 4.0d0 * mask_jra(i,j)
        hl1 = mask_jra(iw,j) + mask_jra(ie,j) + 2.0d0 * mask_jra(i,j)
        if (hl1 == 4.0d0) then
          !hl2 = mask_jra(iw,j) * usf(iw,j) + mask_jra(ie,j) * usf(ie,j) &
          !     & + mask_jra(i,j-1) * usf(i,j-1) + mask_jra(i,j+1) * usf(i,j+1) &
          !     & + 4.0d0 * mask_jra(i,j) * usf(i,j)
          !hl3 = mask_jra(iw,j) * vsf(iw,j) + mask_jra(ie,j) * vsf(ie,j) &
          !     & + mask_jra(i,j-1) * vsf(i,j-1) + mask_jra(i,j+1) * vsf(i,j+1) &
          !    & + 4.0d0 * mask_jra(i,j) * vsf(i,j)
          hl2 = mask_jra(iw,j) * usf(iw,j) + mask_jra(ie,j) * usf(ie,j) &
               & + 2.0d0 * mask_jra(i,j) * usf(i,j)
          hl3 = mask_jra(iw,j) * vsf(iw,j) + mask_jra(ie,j) * vsf(ie,j) &
               & + 2.0d0 * mask_jra(i,j) * vsf(i,j)
          usc(i,j) = hl2 / hl1
          vsc(i,j) = hl3 / hl1
        else
          usc(i,j) = usf(i,j)
          vsc(i,j) = vsf(i,j)
        end if
      end do
    end do

    !---------------------

    next_date = libmxe_calendar__addsec(current_date, data_intv_sec_srf, l_use_leap)
    next_file = libmxe_calendar__addsec(current_file, data_intv_sec_srf, l_leap_valid)

    !---------------------
    ! output data

    write(calib_dir,'(1a,1a,i4.4,i2.2)') trim(calib_base_dir),'/',current_file%year,current_file%month
    write(fout_us,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(calib_dir),'/',trim(fout_base_us),'.', &
         & current_file%year,current_file%month,current_file%day,current_file%hour
    write(fout_vs,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(calib_dir),'/',trim(fout_base_vs),'.', &
         & current_file%year,current_file%month,current_file%day,current_file%hour

    lreclen = 4 * imf * jmf
    call open_file_direct(mtot1, fout_us, lreclen)
    write(mtot1,rec=1) real(usc(1:imf,1:jmf),4)
    call close_file(mtot1)

    lreclen = 4 * imf * jmf
    call open_file_direct(mtot1, fout_vs, lreclen)
    write(mtot1,rec=1) real(vsc(1:imf,1:jmf),4)
    call close_file(mtot1)

    !---------------------

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

end program filter_surface_wind
