! -*-F90-*-
!------------------- precip_minus_snow_interannual.F90 ------------------------
! Information:
!   Create liquid precipitation by total minus snow.
!----------------------------------------------------------------------
program create_liquid_precipitation

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

  type(type_calendar) :: current_month_start
  type(type_calendar) :: prev_month_start
  type(type_calendar) :: latest_month_start

  integer(4) :: calc_start(6)
  integer(4) :: calc_end(6)

  ! Precipitation

  integer(4) :: imp, jmp, kmp
  real(8),allocatable :: alonp(:), alatp(:)
  real(8),allocatable :: prcp(:,:)
  real(8),allocatable :: snow(:,:)
  real(8),allocatable :: rain(:,:)
  real(4),allocatable :: dat4p(:,:)

  !-----------------------------------------------

  integer(4) :: mtin1, mtin2, mtin3
  integer(4) :: mtot1, mtot2, mtot3

  !-----------------------------------------------
  ! Precipitation data

  character(256) :: pcp_base_dir, pcp_dir
  character(256) :: fin_base_pcp, fin_base_snw
  character(256) :: fin_pcp, fin_snw

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

  character(256) :: rain_base_dir, rain_dir
  character(256) :: fout_base_rain

  character(256) :: fout_rain

  !------------------

  logical :: l_leap_valid
  logical :: l_separate

  integer(4) :: lreclen, ireco

  integer(4) :: i, j, k, l, m, n

  !----------------------------------------------------------------------

  type(type_libmxe_para) :: pcpp
  type(type_libmxe_grid) :: pcpg
  character(256) :: file_namelist_pcp

  integer(4),parameter :: lun = 10

  integer(4) :: ios, istat

  !---------------------------------------------------------------------

  namelist /nml_pcpsnwdata/ file_namelist_pcp, &
       &  pcp_base_dir, &
       &  fin_base_pcp, fin_base_snw, &
       &  file_first_pcp, &
       &  file_intv_type_pcp, file_intv_pcp, total_rec_pcp, data_intv_sec_pcp, &
       &  pcp_first, undef_pcp, &
       &  l_ymdir_pcp

  namelist /nml_rain/ calc_start, calc_end, &
       &  l_leap_valid, &
       &  l_separate, &
       &  rain_base_dir, &
       &  fout_base_rain

  !-----------------------------------------------------------------------

  open(lun,file='namelist.create_rain',iostat=istat,status='old')

  if ( istat /= 0 ) then
    write(6,*) ' Cannot find file : namelist.create_rain'
    stop
  end if

  ios = 0

  rewind(lun)
  file_intv_pcp = 0
  read(lun, nml=nml_pcpsnwdata, iostat=istat)
  ios = ios + istat

  rewind(lun)

  l_separate=.false.
  read(lun, nml=nml_rain, iostat=istat)
  ios = ios + istat

  if ( ios /= 0 ) then
    write(6,*) ' Read error : namelist.create_rain', ios
    stop
  end if

  close(lun)

  call force_process__ini(l_leap_valid)

  !----------------------------------------------------------------------

  call libmxe_para__register(pcpp,file_namelist=file_namelist_pcp)
  call libmxe_grid__register(pcpg,pcpp)
  imp = pcpp%imut
  jmp = pcpp%jmut
  kmp = pcpp%km
  allocate(alonp(1:imp), alatp(1:jmp))
  alonp(1:imp) = pcpg%lonu(1:imp)
  alatp(1:jmp) = pcpg%latu(1:jmp)

  allocate(prcp(1:imp,1:jmp))
  allocate(snow(1:imp,1:jmp))
  allocate(rain(1:imp,1:jmp))
  allocate(dat4p(1:imp,1:jmp))

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

  current_file%year   = file_first_pcp(1)
  current_file%month  = file_first_pcp(2)
  current_file%day    = file_first_pcp(3)
  current_file%hour   = file_first_pcp(4)
  current_file%minute = file_first_pcp(5)
  current_file%second = file_first_pcp(6)

  write(6,*) 
  write(6,*) ' MAIN LOOP START '

  LOOP_SURF_FORCE: do while ( .true. )

    write(6,*) ' This step ', current_date%year, current_date%month, current_date%day, &
         & current_date%hour, current_date%minute, data_intv_sec_pcp


    if (.not. l_separate) then
      if (         (current_file%month  == 1) &
           & .and. (current_file%day    == 1) &
           & .and. (current_file%hour   == 0) &
           & .and. (current_file%minute == 0) &
           & .and. (current_file%second == 0) ) then
        write(rain_dir,'(1a,1a,i4.4)') trim(rain_base_dir),'/',current_file%year
        write(fout_rain,'(1a,1a,1a,1a,i4.4)') trim(rain_dir),'/',trim(fout_base_rain),'.',current_file%year
        lreclen = 4 * imp * jmp
        call open_file_direct(mtot1, fout_rain, lreclen)
        ireco = 0
      end if
    end if

    write(pcp_dir,'(1a,1a,i4.4,i2.2)') trim(pcp_base_dir),'/',current_file%year,current_file%month
    write(fin_pcp,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(pcp_dir),'/',trim(fin_base_pcp),'.', &
         & current_file%year,current_file%month,current_file%day,current_file%hour
    write(fin_snw,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(pcp_dir),'/',trim(fin_base_snw),'.', &
         & current_file%year,current_file%month,current_file%day,current_file%hour

    lreclen = 4 * imp * jmp

    call open_file_direct(mtin1, fin_pcp, lreclen)
    read(mtin1,rec=1) dat4p
    prcp(1:imp,1:jmp) = real(dat4p(1:imp,1:jmp),8)
    call close_file(mtin1)

    call open_file_direct(mtin2, fin_snw, lreclen)
    read(mtin2,rec=1) dat4p
    snow(1:imp,1:jmp) = real(dat4p(1:imp,1:jmp),8)
    call close_file(mtin2)

    do j = 1, jmp
      do i = 1, imp
        rain(i,j) = max(prcp(i,j) - snow(i,j),0.0d0)
      end do
    end do

    ! advance in time

    next_date = libmxe_calendar__addsec(current_date, data_intv_sec_pcp, l_leap_valid)
    next_file = libmxe_calendar__addsec(current_file, data_intv_sec_pcp, l_leap_valid)

    !---------------------
    ! output data

    if (l_separate) then
      write(rain_dir,'(1a,1a,i4.4,i2.2)') trim(rain_base_dir),'/',current_file%year,current_file%month
      write(fout_rain,'(1a,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') trim(rain_dir),'/',trim(fout_base_rain),'.', &
           & current_file%year,current_file%month,current_file%day,current_file%hour
      lreclen = 4 * imp * jmp
      call open_file_direct(mtot1, fout_rain, lreclen)
      write(mtot1,rec=1) real(rain(1:imp,1:jmp),4)
      call close_file(mtot1)
    else
      ireco = ireco + 1
      write(mtot1,rec=ireco) real(rain(1:imp,1:jmp),4)
    end if

    !--------------------

    if (.not. l_separate) then
      if (         (next_file%month  ==  1) &
           & .and. (next_file%day    ==  1) &
           & .and. (next_file%hour   ==  0) &
           & .and. (next_file%minute ==  0) &
           & .and. (next_file%second ==  0) ) then
        call close_file(mtot1)
      end if
    end if

    if ( (next_date%year == end_date%year) &
         .and. (next_date%month == end_date%month) &
         .and. (next_date%day == end_date%day) &
         .and. (next_date%hour == end_date%hour) &
         .and. (next_date%minute == end_date%minute) &
         .and. (next_date%second == end_date%second) ) then

      if (.not. l_separate) then
        if (        (next_file%month  /=  1) &
             & .or. (next_file%day    /=  1) &
             & .or. (next_file%hour   /=  0) &
             & .or. (next_file%minute /=  0) &
             & .or. (next_file%second /=  0) ) then
          call close_file(mtot1)
        end if
      end if

      exit LOOP_SURF_FORCE

    end if

    current_date = next_date
    current_file = next_file

  end do LOOP_SURF_FORCE

  write(6,*) ' MAIN LOOP END '

end program create_liquid_precipitation
