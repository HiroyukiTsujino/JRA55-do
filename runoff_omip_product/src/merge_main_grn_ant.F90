!-*-F90-*-
program merge_main_grn_ant

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
  logical :: l_area_out
  character(256) :: file_area_out
  logical :: l_solid_out
  character(256) :: sld_out_base
  character(256) :: file_solid_out

  integer(4) :: mtinm
  integer(4) :: mtinas, mtinal, mtinat
  integer(4) :: mtings, mtingl, mtingt
  integer(4) :: mtot, mtota, mtots
  integer(4) :: ireclen
  logical :: l_leap_valid

  integer(4) :: nxf, nyf
  real(8),allocatable :: rofa(:,:), rofm(:,:)
  real(4),allocatable :: workf(:,:)
  real(4) :: undef_in_cama
  real(8),allocatable :: alonuf(:), alatuf(:)
  real(8),allocatable :: alontf(:), alattf(:)
  real(8),allocatable :: mask_cama(:,:)
  real(8),allocatable :: area_cama(:,:)

  !------------------------------------------------------------

  integer(4) :: ibyr, ieyr
  integer(4) :: nmon_last, mon_end
  integer(4) :: nday_last, day_end

  integer(4) :: nt, nyr, nm, nd, irec, i, j
  integer(4) :: ii, jj, iii, jjj
  
  integer(4) :: irec1, irec2, irec3

  integer(4),parameter :: nmonyr = 12
  integer(4) :: ndmon(nmonyr) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)
  integer(4) :: nday_mon

  !------------------------------------------------------------

  real(8),parameter :: dens_pw = 1.0d3 ! density of pure water

  !--------------------------------------------------------------------------

  real(8) :: total_main, total_green, total_antarc
  integer(4) :: nday_year
  

  logical :: l_total_solid, l_liquid_solid

  logical :: l_green_sep, l_green_total
  logical :: l_green_clim
  logical :: l_antarc_sep, l_antarc_total

  character(256) :: file_grn_solid_in, file_grn_solid_base
  character(256) :: file_grn_liquid_in, file_grn_liquid_base
  character(256) :: file_grn_total_in, file_grn_total_base
  character(8)   :: suffix

  character(256) :: file_ant_solid_in
  character(256) :: file_ant_liquid_in
  character(256) :: file_ant_total_in

  real(8),allocatable :: rof_ant_l(:,:), rof_ant_s(:,:), rof_ant_t(:,:)
  real(8),allocatable :: rof_grn_l(:,:), rof_grn_s(:,:), rof_grn_t(:,:)
  real(8),allocatable :: rof_sld_t(:,:)

  !--------------------------------------------------------------------------

  namelist /nml_main_grn_ant/ &
       & file_namelist_cama, &
       & l_leap_valid, &
       & file_mask_cama, &
       & ibyr, ieyr, &
       & nmon_last, nday_last, &
       & riv_cama_base, &
       & riv_out_base,  &
       & l_green_sep, l_green_total, l_green_clim, &
       & file_grn_solid_base, file_grn_liquid_base, file_grn_total_base, &
       & l_antarc_sep, l_antarc_total, &
       & file_ant_solid_in, file_ant_liquid_in, file_ant_total_in, &
       & l_area_out, file_area_out, &
       & l_solid_out, sld_out_base, &
       & l_total_solid, l_liquid_solid

  !--------------------------------------------------------------------------

  nmon_last = -9
  nday_last = -9
  l_green_clim = .true.
  l_total_solid = .false.
  l_liquid_solid = .false.
  open(10,file='namelist.merge_main_grn_ant')
  read(10,nml=nml_main_grn_ant)
  close(10)

  if (l_total_solid .and. l_liquid_solid) then
    write(6,*) ' l_total_solid and l_liquid_solid cannot be true at the same time'
    write(6,*) ' Terminating.... '
    stop
  end if

  if (.not. l_total_solid .and. .not. l_liquid_solid) then
    write(6,*) ' Either l_total_solid or l_liquid_solid must true'
    write(6,*) ' Terminating.... '
    stop
  end if

  !--------------------------------------------------------------------------
  ! consistency check

  if (l_antarc_sep) then
    if (l_antarc_total) then
      write(6,*) ' When l_antarc_sep = .true., l_antarc_total = .false. '
      stop
    end if
  else
    if (.not. l_antarc_total) then
      write(6,*) ' When l_antarc_sep = .false., l_antarc_total = .true. '
      stop
    end if
  end if

  if (l_antarc_total) then
    if (l_antarc_sep) then
      write(6,*) ' When l_antarc_total = .true., l_antarc_sep = .false. '
      stop
    end if
  else
    if (.not. l_antarc_sep) then
      write(6,*) ' When l_antarc_total = .false., l_antarc_sep = .true. '
      stop
    end if
  end if

  if (l_green_sep) then
    if (l_green_total) then
      write(6,*) ' When l_green_sep = .true., l_green_total = .false. '
      stop
    end if
  else
    if (.not. l_green_total) then
      write(6,*) ' When l_green_sep = .false., l_green_total = .true. '
      stop
    end if
  end if

  if (l_green_total) then
    if (l_green_sep) then
      write(6,*) ' When l_green_total = .true., l_green_sep = .false. '
      stop
    end if
  else
    if (.not. l_green_sep) then
      write(6,*) ' When l_green_total = .false., l_green_sep = .true. '
      stop
    end if
  end if

  if (l_solid_out) then
    if (.not. l_green_sep) then
      write(6,*) 'WARNING When l_solid_out = .true., l_green_sep = .true. '
      l_green_sep = .true.
    end if
    if (.not. l_antarc_sep) then
      write(6,*) 'WARNING When l_soild_out = .true., l_antarc_sep = .true. '
      l_antarc_sep = .true.
    end if
  end if

  !--------------------------------------------------------------------------

  call libmxe_para__register(camap,file_namelist=file_namelist_cama)
  call libmxe_grid__register(camag,camap)

  nxf = camap%imut
  nyf = camap%jmut
  ireclen = 4*nxf*nyf
  allocate(alonuf(1:nxf), alatuf(1:nyf))
  allocate(alontf(1:nxf), alattf(1:nyf))
  alonuf(1:nxf) = camag%lonu(1:nxf)
  alatuf(1:nyf) = camag%latu(1:nyf)
  alontf(1:nxf) = camag%lont(1:nxf)
  alattf(1:nyf) = camag%latt(1:nyf)

  allocate(workf(1:nxf,1:nyf))
  allocate(mask_cama(1:nxf,1:nyf))
  allocate(area_cama(1:nxf,1:nyf))
  allocate(rofa(1:nxf,1:nyf))
  allocate(rofm(1:nxf,1:nyf))
  allocate(rof_ant_t(1:nxf,1:nyf),rof_grn_t(1:nxf,1:nyf))

  if (l_antarc_sep) then
    allocate(rof_ant_l(1:nxf,1:nyf), rof_ant_s(1:nxf,1:nyf))
  end if

  if (l_green_sep) then
    allocate(rof_grn_l(1:nxf,1:nyf), rof_grn_s(1:nxf,1:nyf))
  end if

  if (l_solid_out) then
    allocate(rof_sld_t(1:nxf,1:nyf))
  end if

  area_cama(1:nxf,1:nyf) = camag%areau(1:nxf,1:nyf) * 1.0d-4 ! cgs => MKS

  !-------------------------------------------------------------

  call open_file_direct(mtota,file_area_out,ireclen)
  write(6,*) ' Writing area to ', trim(file_area_out)
  write(mtota,rec=1) real(area_cama(1:nxf,1:nyf),4)
  call close_file(mtota)

  !-------------------------------------------------------------

  if (l_antarc_sep) then

    call open_file_direct(mtinas,file_ant_solid_in,ireclen)
    write(6,*) ' reading from ', trim(file_ant_solid_in)
    read(mtinas,rec=1) workf(1:nxf,1:nyf)
    call close_file(mtinas)
    rof_ant_s(1:nxf,1:nyf) = real(workf(1:nxf,1:nyf),8)
    
    call open_file_direct(mtinal,file_ant_liquid_in,ireclen)
    write(6,*) ' reading from ', trim(file_ant_liquid_in)
    read(mtinal,rec=1) workf(1:nxf,1:nyf)
    call close_file(mtinal)
    rof_ant_l(1:nxf,1:nyf) = real(workf(1:nxf,1:nyf),8)

    rof_ant_t(:,:) = rof_ant_s(:,:) + rof_ant_l(:,:)

  else

    call open_file_direct(mtinat,file_ant_total_in,ireclen)
    write(6,*) ' reading from ', trim(file_ant_total_in)
    read(mtinat,rec=1) workf(1:nxf,1:nyf)
    call close_file(mtinat)

    rof_ant_t(1:nxf,1:nyf) = real(workf(1:nxf,1:nyf),8)

  end if

  do nyr = ibyr, ieyr

    write(6,*) 'Year = ', nyr

    ireclen = 4*nxf*nyf

    write(file_cama_in,'(1a,i4.4)') trim(riv_cama_base),nyr
    call open_file_direct(mtinm,file_cama_in,ireclen)
    write(6,*) ' reading from ', trim(file_cama_in)

    write(file_river_out,'(1a,i4.4)') trim(riv_out_base),nyr
    call open_file_direct(mtot,file_river_out,ireclen)
    write(6,*) ' writing to ', trim(file_river_out)

    if (l_solid_out) then
      write(file_solid_out,'(1a,i4.4)') trim(sld_out_base),nyr
      call open_file_direct(mtots,file_solid_out,ireclen)
      write(6,*) ' writing to ', trim(file_solid_out)
    end if

    irec = 0

    if (l_leap_valid .and. lleap(nyr)) then
      ndmon(2) = 29
      nday_year = 366
      if (l_green_clim) then
        suffix='_366dy'
      else
        write(suffix,'(i4.4)') nyr
      end if
    else
      ndmon(2) = 28
      nday_year = 365
      if (l_green_clim) then
        suffix='_365dy'
      else
        write(suffix,'(i4.4)') nyr
      end if
    end if
    
    total_main   = 0.0d0
    total_green  = 0.0d0
    total_antarc = 0.0d0

    if (l_green_sep) then

      write(file_grn_solid_in,'(1a,1a)') trim(file_grn_solid_base),trim(suffix)
      call open_file_direct(mtings,file_grn_solid_in,ireclen)
      write(6,*) ' reading from ', trim(file_grn_solid_in)
    
      write(file_grn_liquid_in,'(1a,1a)') trim(file_grn_liquid_base),trim(suffix)
      call open_file_direct(mtingl,file_grn_liquid_in,ireclen)
      write(6,*) ' reading from ', trim(file_grn_liquid_in)

    else

      write(file_grn_total_in,'(1a,1a)') trim(file_grn_total_base),trim(suffix)
      call open_file_direct(mtingt,file_grn_total_in,ireclen)
      write(6,*) ' reading from ', trim(file_grn_total_in)
      
    end if

    if (nyr == ieyr) then
      if (nmon_last == -9) then
        mon_end = 12
      else
        mon_end = nmon_last
      end if
    else
      mon_end = 12
    end if

    write(6,*) ' mon_end = ', mon_end

    do nm = 1, mon_end

      nday_mon = ndmon(nm)

      if ((nyr == ieyr) .and. (nm == mon_end)) then
        if (nday_last == -9) then
          day_end = nday_mon
        else
          day_end = nday_last
        end if
      else
        day_end = nday_mon
      end if

      write(6,*) ' month = ', nm , ' days = ', day_end

      do nd = 1, day_end

        irec = irec + 1
        read(mtinm,rec=irec) workf(1:nxf,1:nyf)
        rofa(1:nxf,1:nyf) = real(workf(1:nxf,1:nyf),8)

        if (l_green_sep) then
          read(mtings,rec=irec) workf(1:nxf,1:nyf)
          rof_grn_s(1:nxf,1:nyf) = real(workf(1:nxf,1:nyf),8)
          read(mtingl,rec=irec) workf(1:nxf,1:nyf)
          rof_grn_l(1:nxf,1:nyf) = real(workf(1:nxf,1:nyf),8)
          rof_grn_t(:,:) = rof_grn_s(:,:) + rof_grn_l(:,:)
        else
          read(mtingt,rec=irec) workf(1:nxf,1:nyf)
          rof_grn_t(1:nxf,1:nyf) = real(workf(1:nxf,1:nyf),8)
        end if

        if (l_total_solid) then
          do j = 1, nyf
            do i = 1, nxf
              rofm(i,j) = max(rofa(i,j),rof_ant_t(i,j),rof_grn_t(i,j)) &
                   &  / area_cama(i,j) * dens_pw
            end do
          end do
        end if
        if (l_liquid_solid) then
          if (l_green_sep .and. l_antarc_sep) then
            write(6,*) ' Liquid from all '
            do j = 1, nyf
              do i = 1, nxf
                rofm(i,j) = max(rofa(i,j),rof_ant_l(i,j),rof_grn_l(i,j)) &
                     &  / area_cama(i,j) * dens_pw
              end do
            end do
          end if
          if (l_green_total .and. l_antarc_sep) then
            write(6,*) ' Total from Greenland '
            do j = 1, nyf
              do i = 1, nxf
                rofm(i,j) = max(rofa(i,j),rof_ant_l(i,j),rof_grn_t(i,j)) &
                     &  / area_cama(i,j) * dens_pw
              end do
            end do
          end if
          if (l_green_sep .and. l_antarc_total) then
            write(6,*) ' Total from Antarctica '
            do j = 1, nyf
              do i = 1, nxf
                rofm(i,j) = max(rofa(i,j),rof_ant_t(i,j),rof_grn_l(i,j)) &
                     &  / area_cama(i,j) * dens_pw
              end do
            end do
          end if
          if (l_green_total .and. l_antarc_total) then
            write(6,*) ' Total from Greenland and Antarctica '
            do j = 1, nyf
              do i = 1, nxf
                rofm(i,j) = max(rofa(i,j),rof_ant_t(i,j),rof_grn_t(i,j)) &
                     &  / area_cama(i,j) * dens_pw
              end do
            end do
          end if
        end if

        write(mtot,rec=irec) real(rofm(1:nxf,1:nyf),4)

        if (l_solid_out) then
          rof_sld_t(:,:) = (rof_ant_s(:,:) + rof_grn_s(:,:)) / area_cama(:,:) * dens_pw
          write(mtots,rec=irec) real(rof_sld_t(1:nxf,1:nyf),4)
        end if

        total_main   = total_main   + sum(rofa)
        total_green  = total_green  + sum(rof_grn_t)
        total_antarc = total_antarc + sum(rof_ant_t)
        !write(6,*) nm, nd
        !write(6,*) sum(rofa), sum(rof_ant_t), sum(rof_grn_t)
      end do
    end do

    write(6,*) nyr, &
         & real(total_main/real(nday_year,8),4), &
         & real(total_green/real(nday_year,8),4), &
         & real(total_antarc/real(nday_year,8),4)

    write(6,*) ' closing file, total record = ', irec


    if (l_green_sep) then
      call close_file(mtings)
      call close_file(mtingl)
    else
      call close_file(mtingt)
    end if

    call close_file(mtot)
    call close_file(mtinm)
    if (l_solid_out) then
      call close_file(mtots)
    end if

  end do

  !-------------------------------------------------------

  if (l_solid_out) then
    deallocate(rof_sld_t)
  end if
  if (l_green_sep) then
    deallocate(rof_grn_s,rof_grn_l)
  end if
  if (l_antarc_sep) then
    deallocate(rof_ant_s,rof_ant_l)
  end if
  deallocate(rof_ant_t,rof_grn_t)

  deallocate(alonuf, alatuf)
  deallocate(alontf, alattf)
  deallocate(workf)
  deallocate(rofa)
  deallocate(rofm)

  deallocate(mask_cama)

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

end program merge_main_grn_ant
