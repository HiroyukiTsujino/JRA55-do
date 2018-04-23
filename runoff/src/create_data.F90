!-*-F90-*-
program main

  implicit none

  integer(4), parameter :: nx = 1440, ny = 720
  real(4) :: roff(nx,ny), work4(nx,ny), undef_in
  real(4) :: factor, facter_a, factor_b

  real(4) :: roff_clim(nx,ny,365)
  real(4) :: roff_leap(nx,ny)


  character(256) :: cfriv, cdate
  character(256) :: file_river_clim
  character(256) :: file_river_leap
  character(256) :: file_river_out
  character(256) :: file_prcp_land_base
  character(256) :: file_prcp_land_in
  character(256) :: riv_out_base

  integer(4) :: ibyr, ieyr

  integer(4) :: nt, nyr, nm, nd, irec, i, j, tday, clday, ii, jj
  
  integer(4) :: irec1, irec2, irec3

  integer(4),parameter :: nmonyr = 12
  integer(4) :: ndmon(nmonyr) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  integer(4) :: nday_prev, nday_next, nday

!  real(8), parameter :: ro = 1.036d0

  real(4) :: tmp_prcp
  real(8),allocatable :: total_prcp(:)
  real(8),allocatable :: factor_river(:)
  real(8) :: factor_prcp2river

  real(8) :: annual, monthly, daily
  real(8) :: day_of_year, ar, br

  logical :: l_normal_year

  !--------------------------------------------------------------------------

  namelist /nml_create_data/ ibyr, ieyr, &
       & l_normal_year, &
       & file_river_clim, file_river_leap, &
       & file_prcp_land_base, &
       & factor_prcp2river, &
       & riv_out_base

  !--------------------------------------------------------------------------

  l_normal_year=.false.
  open(10, file='namelist.create_data')
  read(10, nml=nml_create_data)
  close(10)

  !--------------------------------------------------------------------------

  open (20,file=file_river_clim,form='unformatted',access='direct',action='read',recl=4*nx*ny)
  write(6,*) ' read from ', trim(file_river_clim)
  do nd = 1, 365
    read(20,rec=nd) roff_clim(1:nx,1:ny,nd)
  end do
  close(20)

  open (20,file=file_river_leap,form='unformatted',access='direct',action='read',recl=4*nx*ny)
  write(6,*) ' read from ', trim(file_river_leap)
  read (20,rec=1) roff_leap(1:nx,1:ny)
  close(20)

  annual = 0.d0
  tday = 0
  do nm = 1, 12
    monthly = 0.d0
    do nd = 1, ndmon(nm)
      tday = tday + 1
      daily = 0.d0
      do j = 1, ny
        do i = 1, nx
          daily = daily + real(roff_clim(i,j,tday),8)
        end do
      end do
      monthly = monthly + daily
    end do
    annual = annual + monthly
    monthly = monthly / real(ndmon(nm),8)
  end do
  write(6,*) ' total days for climatology = ', tday
  annual = annual / real(tday,8)

  write(6,*) ' annual mean of climatology = ', annual

  !-----

  allocate(total_prcp(ibyr-1:ieyr+1))
  allocate(factor_river(ibyr-1:ieyr+1))

  if (.not. l_normal_year) then
    do nyr = ibyr, ieyr
      write(*,*) nyr
      write(file_prcp_land_in,'(1a,i4.4)') trim(file_prcp_land_base),nyr
      open(20,file=file_prcp_land_in,form='unformatted',access='direct',action='read',recl=4)
      write(6,*) ' reading from ', trim(file_prcp_land_in)
      read(20,rec=3) tmp_prcp ! land precipitation except for Antarctica
      close(20)
      total_prcp(nyr) = real(tmp_prcp,8) * 1.0d-3 ! kg/s -> m^3/s (1kg = 10^-3 m^3)
    end do
  else
    total_prcp(ibyr:ieyr) = annual / factor_prcp2river
  end if

  total_prcp(ibyr-1) = total_prcp(ibyr)
  total_prcp(ieyr+1) = total_prcp(ieyr)

  do nyr = ibyr-1, ieyr+1
!    factor_river(nyr) = total_prcp(nyr) * ro * factor_prcp2river / annual
    factor_river(nyr) = total_prcp(nyr) * factor_prcp2river / annual ! do not apply "ro" for 2015 correction
    write(6,*) nyr, factor_river(nyr)
  end do

  !-----

  do nyr = ibyr, ieyr

    write(*,*) nyr

    write(file_river_out,'(1a,i4.4,1a)') trim(riv_out_base),nyr,'.bin'
    open(20,file=file_river_out,form='unformatted',access='direct',action='write',recl=4*nx*ny)
    write(6,*) ' writing to ', trim(file_river_out)

    irec = 0

    if ( lleap(nyr) ) then
      ndmon(2) = 29
      nday = 366
    else
      ndmon(2) = 28
      nday = 365
    end if

    if ( lleap(nyr-1) ) then
      nday_prev = 366
    else
      nday_prev = 365
    end if

    if ( lleap(nyr+1) ) then
      nday_next = 366
    else
      nday_next = 365
    end if

    tday = 0
    clday = 0

    do nm = 1, nmonyr
      do nd = 1, ndmon(nm)

        tday = tday + 1
        irec = irec + 1

        if (lleap(nyr) .and. (nm == 2) .and. (nd == 29)) then
        else
          clday = clday + 1
        end if

        day_of_year = tday - 0.5d0
        if (day_of_year <= 0.5d0 * real(nday,8)) then
          ar = (real(nday_prev,8) + 2.0d0 * day_of_year) / real(nday_prev + nday,8)
          br = 1.0d0 - ar
          factor = br * factor_river(nyr-1) + ar * factor_river(nyr)
        else
          ar = (2.0d0 * day_of_year - real(nday,8)) / real(nday + nday_next,8)
          br = 1.0d0 - ar
          factor = br * factor_river(nyr) + ar * factor_river(nyr+1)
        end if

        write(6,*) tday
        write(6,*) br, ar, factor

        if (lleap(nyr) .and. (nm == 2) .and. (nd == 29)) then
          do j = 1, ny
            do i = 1, nx
              roff(i,j) = roff_leap(i,j) * factor
            end do
          end do
        else
          do j = 1, ny
            do i = 1, nx
              roff(i,j) = roff_clim(i,j,clday) * factor
            end do
          end do
        end if

        write(20,rec=irec) roff

      end do
    end do

    write(6,*) ' closing file, total record = ', irec, clday

    close(20)

  end do
    
contains

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

end program main
