!-*-F90-*-
program merge_cama_core

  use libmxe_para, only: libmxe_para__register &
                     & , clen, pi, radian, radian_r, radius &
                     & , type_libmxe_para
  use libmxe_grid, only: libmxe_grid__register  &
                     & , type_libmxe_grid
  use libmxe_topo, only: libmxe_topo__register, libmxe_topo__aexl &
                     & , type_libmxe_topo
  use libmxe_io, only: libmxe_io__register, type_libmxe_io
  use libmxe_calendar

  use file_open_close_manager

  implicit none

  !------------------------------------------------------------

  type(type_libmxe_para) :: camap
  type(type_libmxe_grid) :: camag

  character(256) :: file_namelist_cama
  character(256) :: file_mask_cama
  character(256) :: file_cama_in
  character(256) :: file_river_out
  character(256) :: riv_cama_base
  character(256) :: riv_out_base

  integer(4) :: mtinf, mtot
  integer(4) :: ireclen
  integer(4) :: data_intv_sec_cama
  logical :: l_leap_valid

  integer(4) :: nxf, nyf
  real(8),allocatable :: roff(:,:), rofm(:,:), rofa(:,:), rofb(:,:)
  real(4),allocatable :: workf(:,:)
  real(4) :: undef_in_cama
  real(8),allocatable :: alonuf(:), alatuf(:)
  real(8),allocatable :: alontf(:), alattf(:)
  real(8),allocatable :: mask_cama(:,:)

  !------------------------------------------------------------

  type(type_libmxe_para) :: corep
  type(type_libmxe_grid) :: coreg

  character(256) :: file_namelist_core
  character(256) :: riv_core_base
  character(256) :: file_core_in
  character(256) :: file_mask_core

  integer(4) :: mtinc

  integer(4) :: nxc, nyc
  real(8),allocatable :: rofc(:,:)
  real(4),allocatable :: workc(:,:)
  real(4) :: undef_in_core
  real(8),allocatable :: alonuc(:), alatuc(:)
  real(8),allocatable :: alontc(:), alattc(:)
  real(8),allocatable :: mask_core(:,:)

  !------------------------------------------------------------

  integer(4) :: ibyr, ieyr

  integer(4) :: nt, nyr, nm, nd, irec, i, j
  integer(4) :: ii, jj, iii, jjj
  
  integer(4) :: irec1, irec2, irec3

  integer(4),parameter :: nmonyr = 12
  integer(4) :: ndmon(nmonyr) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)
  integer(4) :: nday_mon

  !------------------------------------------------------------

  type(type_calendar) :: start_date
  type(type_calendar) :: end_date

  type(type_calendar) :: today
  type(type_calendar) :: current_date
  type(type_calendar) :: next_date

  type(type_calendar) :: current_month_start
  type(type_calendar) :: prev_month_start
  type(type_calendar) :: latest_month_start

  integer(4) :: calc_start(6)
  integer(4) :: calc_end(6)

  integer(4) :: current_month_sec
  integer(4) :: this_month_sec
  integer(4) :: prev_month_sec
  integer(4) :: latest_month_sec
  integer(4) :: from_previous_data
  real(8) :: afm, bfm
  integer(4) :: yra, yrb
  integer(4) :: monb, mona, mont

  integer(4) :: lun

  real(8) :: hl1, hl2

  real(8),parameter :: dens_pw = 1.0d3 ! density of pure water

  !--------------------------------------------------------------------------

  namelist /nml_merge_cama_core/ &
       & file_namelist_cama, file_namelist_core, &
       & l_leap_valid, &
       & file_mask_cama, file_mask_core, &
       & ibyr, ieyr, &
       & riv_cama_base, riv_core_base, &
       & data_intv_sec_cama, &
       & undef_in_cama, undef_in_core, &
       & riv_out_base

  !--------------------------------------------------------------------------

  open(10,file='namelist.merge_cama_core')
  read(10,nml=nml_merge_cama_core)
  close(10)

  !--------------------------------------------------------------------------

  call libmxe_para__register(camap,file_namelist=file_namelist_cama)
  call libmxe_grid__register(camag,camap)

  nxf = camap%imut
  nyf = camap%jmut
  allocate(alonuf(1:nxf), alatuf(1:nyf))
  allocate(alontf(1:nxf), alattf(1:nyf))
  alonuf(1:nxf) = camag%lonu(1:nxf)
  alatuf(1:nyf) = camag%latu(1:nyf)
  alontf(1:nxf) = camag%lont(1:nxf)
  alattf(1:nyf) = camag%latt(1:nyf)

  allocate(workf(1:nxf,1:nyf))
  allocate(mask_cama(1:nxf,1:nyf))
  allocate(roff(1:nxf,1:nyf))
  allocate(rofm(1:nxf,1:nyf))
  allocate(rofa(1:nxf,1:nyf),rofb(1:nxf,1:nyf))

  !------

  call libmxe_para__register(corep,file_namelist=file_namelist_core)
  call libmxe_grid__register(coreg,corep)

  nxc = corep%imut
  nyc = corep%jmut

  allocate(alonuc(1:nxc), alatuc(1:nyc))
  allocate(alontc(1:nxc), alattc(1:nyc))

  alonuc(1:nxc) = coreg%lonu(1:nxc)
  alatuc(1:nyc) = coreg%latu(1:nyc)
  alontc(1:nxc) = coreg%lont(1:nxc)
  alattc(1:nyc) = coreg%latt(1:nyc)

  allocate(workc(1:nxc,1:nyc))
  allocate(mask_core(1:nxc,1:nyc))
  allocate(rofc(1:nxc,1:nyc))

  !--------------------------------------------------------------------------

  ireclen = 4*nxf*nyf
  call open_file_direct(lun,file_mask_cama,ireclen)
  write(6,*) ' Read MASK (CaMa) from ', trim(file_mask_cama)
  read(lun,rec=1) workf
  call close_file(lun)
  mask_cama(1:nxf,1:nyf) = real(workf(1:nxf,1:nyf),8)

  !--------------------------------------------------------------------------

  ireclen = 4*nxc*nyc
  call open_file_direct(lun,file_mask_core,ireclen)
  write(6,*) ' Read MASK (Core) from ', trim(file_mask_core)
  read(lun,rec=1) workc
  call close_file(lun)
  mask_core(1:nxc,1:nyc) = real(workc(1:nxc,1:nyc),8)

  !-------------------------------------------------------------------------

  start_date%year   = ibyr
  start_date%month  = 1
  start_date%day    = 1
  start_date%hour   = 12
  start_date%minute = 0
  start_date%second = 0

  end_date%year   = ieyr + 1
  end_date%month  = 1
  end_date%day    = 1
  end_date%hour   = 12
  end_date%minute = 0
  end_date%second = 0

  current_date = start_date

  ! read first two data (monthly)

!  if ( lleap(start_date%year) ) then
!    ndmon(2) = 29
!  else
!    ndmon(2) = 28
!  end if

  hl1 = real(start_date%month,8) &
       & + real(start_date%hour,8) / 24.d0 &
       & + real(start_date%minute,8) / 24.d0 / 60.d0 &
       & + real(start_date%second,8) / 24.d0 / 60.d0 / 60.d0

  hl2 = real(days_of_month(start_date,l_leap_valid),8) / 2.0d0

  if (hl1 > hl2)  then
    if (start_date%month == 12) then
      yrb = start_date%year
      monb = start_date%month
      yra = start_date%year + 1
      mona = 1
    else
      yrb = start_date%year
      monb = start_date%month
      yra = start_date%year + 1
      mona = start_date%month + 1
    end if
  else
    if (start_date%month == 1) then
      yrb = start_date%year - 1
      monb = 12
      yra = start_date%year
      mona = start_date%month
    else
      yrb = start_date%year
      monb = start_date%month - 1
      yra = start_date%year
      mona = start_date%month
    end if
  end if

  ireclen = 4*nxc*nyc

  write(file_core_in,'(1a,i4.4,i2.2)') trim(riv_core_base),yrb,monb
  call open_file_direct(mtinc,file_core_in,ireclen)
  write(6,*) ' reading from ', trim(file_core_in)
  read(mtinc,rec=1) workc(1:nxc,1:nyc)
  call close_file(mtinc)

  rofc(1:nxc,1:nyc) = mask_core(1:nxc,1:nyc) * real(workc(1:nxc,1:nyc),8) / dens_pw ! [kg/m^2/s] -> [m/s]

  do j = 1, nyc
    do i = 1, nxc

!      if (abs(rofc(i,j)) > 1.0d20) then
!        write(6,*) rofc(i,j), mask_core(i,j)
!      end if

      do jj = 1, 4
        do ii = 1, 4
          iii = 4*(i-1) + ii
          jjj = 4*(j-1) + jj
          rofb(iii,jjj) = rofc(i,j) * camag%areau(iii,jjj) * 1.0d-4 ! area [cm2] -> [m2]
!          if (abs(rofb(iii,jjj)) > 1.0d20) then
!            write(6,*) rofb(iii,jjj), rofc(i,j), camag%areau(iii,jjj)
!          end if
        end do
      end do
    end do
  end do

  !-----

  write(file_core_in,'(1a,i4.4,i2.2)') trim(riv_core_base),yra,mona
  call open_file_direct(mtinc,file_core_in,ireclen)
  write(6,*) ' reading from ', trim(file_core_in)
  read(mtinc,rec=1) workc(1:nxc,1:nyc)
  call close_file(mtinc)

  rofc(1:nxc,1:nyc) = mask_core(1:nxc,1:nyc) * real(workc(1:nxc,1:nyc),8) / dens_pw ! [kg/m^2/s] -> [m/s]

  do j = 1, nyc
    do i = 1, nxc
      do jj = 1, 4
        do ii = 1, 4
          iii = 4*(i-1) + ii
          jjj = 4*(j-1) + jj
          rofa(iii,jjj) = rofc(i,j) * camag%areau(iii,jjj) * 1.0d-4 ! area [cm2] -> [m2]
        end do
      end do
    end do
  end do

  mont = mona

  !------

  do nyr = ibyr, ieyr

    write(*,*) nyr

    ireclen = 4*nxf*nyf

    write(file_cama_in,'(1a,i4.4,1a)') trim(riv_cama_base),nyr,'.bin'
    call open_file_direct(mtinf,file_cama_in,ireclen)
    write(6,*) ' reading from ', trim(file_cama_in)

    write(file_river_out,'(1a,i4.4,1a)') trim(riv_out_base),nyr,'.bin'
    call open_file_direct(mtot,file_river_out,ireclen)
    write(6,*) ' writing to ', trim(file_river_out)

    irec = 0

!    if ( lleap(nyr) ) then
!      ndmon(2) = 29
!    else
!      ndmon(2) = 28
!    end if

!    do nm = 1, 1
    do nm = 1, nmonyr
      
      nday_mon = days_of_month(current_date,l_leap_valid)

      write(6,*) nyr, nm, 'days_of_month = ', nday_mon

!      do nd = 1, 1
      do nd = 1, nday_mon

        write(6,*) current_date%year, current_date%month, current_date%day
        irec = irec + 1
        read(mtinf,rec=irec) workf(1:nxf,1:nyf)
        roff(1:nxf,1:nyf) = mask_cama(1:nxf,1:nyf) * real(workf(1:nxf,1:nyf),8)

        !---------------------

        current_month_start%year   = current_date%year
        current_month_start%month  = current_date%month
        current_month_start%day    = 1
        current_month_start%hour   = 0
        current_month_start%minute = 0
        current_month_start%second = 0

        this_month_sec = days_of_month(current_date,l_leap_valid) * 86400
        current_month_sec = libmxe_calendar__diffsec(current_month_start,current_date,l_leap_valid)

        hl1 = real(current_month_sec,8) / real(this_month_sec,8)

        if (hl1 < 0.5d0) then
          latest_month_start%year = current_date%year
          latest_month_start%month = current_date%month
          if (current_date%month == 1) then
            prev_month_start%year = current_date%year - 1
            prev_month_start%month = 12
          else
            prev_month_start%year = current_date%year
            prev_month_start%month = current_date%month - 1
          end if
        else
          prev_month_start%year = current_date%year
          prev_month_start%month = current_date%month
          if (current_date%month == 12) then
            latest_month_start%year = current_date%year + 1
            latest_month_start%month = 1
          else
            latest_month_start%year = current_date%year
            latest_month_start%month = current_date%month + 1
          end if
        end if

        prev_month_start%day    = 1
        prev_month_start%hour   = 0
        prev_month_start%minute = 0
        prev_month_start%second = 0

        latest_month_start%day    = 1
        latest_month_start%hour   = 0
        latest_month_start%minute = 0
        latest_month_start%second = 0

        monb = prev_month_start%month
        mona = latest_month_start%month
        
        prev_month_sec = days_of_month(prev_month_start,l_leap_valid) * 86400
        latest_month_sec = days_of_month(latest_month_start,l_leap_valid) * 86400
        from_previous_data = &
             & libmxe_calendar__diffsec(prev_month_start,current_date,l_leap_valid) - prev_month_sec / 2
        afm = 2.0d0 * real(from_previous_data,8) / real(prev_month_sec+latest_month_sec,8)
        bfm = 1.0d0 - afm
        write(6,*) ' bfm  = ', bfm,  ' afm  = ', afm

        !------------------------------------------------

        if (mont /= mona) then

          write(6,*) ' monb = ', monb, ' mona = ', mona, 'mont = ',mont

          rofb(:,:) = rofa(:,:)

          ireclen = 4*nxc*nyc
          write(file_core_in,'(1a,i4.4,i2.2)') trim(riv_core_base),latest_month_start%year,mona
          call open_file_direct(mtinc,file_core_in,ireclen)
          write(6,*) ' reading from ', trim(file_core_in)
          read(mtinc,rec=1) workc(1:nxc,1:nyc)
          call close_file(mtinc)

          rofc(1:nxc,1:nyc) = mask_core(1:nxc,1:nyc) * real(workc(1:nxc,1:nyc),8) / dens_pw ! [kg/m^2/s] -> [m/s]

          do j = 1, nyc
            do i = 1, nxc
              do jj = 1, 4
                do ii = 1, 4
                  iii = 4*(i-1) + ii
                  jjj = 4*(j-1) + jj
                  rofa(iii,jjj) = rofc(i,j) * camag%areau(iii,jjj) * 1.0d-4 ! area [cm2] -> [m2]
                end do
              end do
            end do
          end do

          mont = mona
          write(6,*) ' monb = ', monb, ' mona = ', mona, 'mont = ',mont

        end if

        do j = 1, nyf
          do i = 1, nxf
            rofm(i,j) = roff(i,j) + afm * rofa(i,j) + bfm * rofb(i,j)
            !if (rofm(i,j) > 1.0d20) then
            !  write(6,*) i,j,roff(i,j), rofa(i,j), rofb(i,j)
            !end if
          end do
        end do

        write(mtot,rec=irec) real(rofm(1:nxf,1:nyf),4)

        next_date = libmxe_calendar__addsec(current_date, data_intv_sec_cama, l_leap_valid)

        current_date = next_date

      end do
    end do

    write(6,*) ' closing file, total record = ', irec

    call close_file(mtot)
    call close_file(mtinf)

  end do

  write(6,*) nxf, nyf

  deallocate(alonuf, alatuf)
  deallocate(alontf, alattf)
  deallocate(workf)
  deallocate(mask_cama)
  deallocate(roff)
  deallocate(rofm)
  deallocate(rofa,rofb)

  write(6,*) nxc, nyc

  deallocate(alonuc, alatuc)
  deallocate(alontc, alattc)
  deallocate(workc)
  deallocate(mask_core)
  deallocate(rofc)

  write(6,*) ' Operation ended '
  
contains

  !============================================================
  function days_of_month(today,l_use_leap)

    logical, intent(in) :: l_use_leap
    integer(4) :: days_of_month
    type(type_calendar),intent(in) :: today
    logical :: l_leap

    integer(4) :: mon
    integer(4) :: itmp

    !---------------------------------------------------------

    if (l_use_leap) then
      l_leap = libmxe_calendar__l_leap_year( today%year )
    else
      l_leap = .false.
    end if

    mon = today%month

    if (mon == 2) then
      if (l_leap) then
        itmp = ndmon(mon) + 1
      else
        itmp = ndmon(mon)
      end if
    else
      itmp = ndmon(mon)
    end if

    days_of_month = itmp

  end function days_of_month
  !============================================================
  logical function lleap( year )
    implicit none
    integer(4) :: year
    logical :: lflg
    if ( mod(year,4) == 0 ) then
      if ( mod(year,100) == 0 ) then
        if ( mod(year,400) == 0 ) then
          lflg = .true.
        else
          lflg = .false.
        end if
      else
        lflg = .true.
      end if
    else
      lflg = .false.
    end if
    lleap = lflg
  end function lleap

end program merge_cama_core
