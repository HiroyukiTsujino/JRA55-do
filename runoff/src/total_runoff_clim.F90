!-*-F90-*-
program main

  implicit none

  integer(4), parameter :: nx = 1440, ny = 720
  real(4) :: roff(nx,ny), mask(nx,ny), undef
  integer(4) :: tmp(0:nx+1,0:ny+1)
  real(8) :: clim, annual, monthly, daily

  character(256) :: cfriv, cdate
  character(256) :: file_river_in
  character(256) :: file_riv_ann
  character(256) :: file_riv_mon
  character(256) :: file_daily, file_clim

  integer(4) :: mo(12,2), yr(2)
  integer(4) :: nt, nyr, nm, nd, irec, i, j, tday, ii, jj
  
  integer(4) :: irec1, irec2, irec3
  character(256) :: dir_out

  data yr / 365, 366 /
  data mo / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, &
          & 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
  data undef / 1e20 /


  !--------------------------------------------------------------------

  namelist /nml_total_runoff_clim/ file_river_in, dir_out

  !--------------------------------------------------------------------
 
  open(10, file='namelist.total_runoff_clim')
  read(10, nml=nml_total_runoff_clim)
  close(10)

  !--------------------------------------------------------------------

  write(file_daily,'(1a,1a)') trim(dir_out),'/daily.gd'
  open(20,file=file_daily,form='unformatted',access='direct',recl=4)
  irec1 = 0

  annual = 0.d0
  open(10,file=file_river_in,form='unformatted',status='old',access='direct',recl=4*nx*ny)
  irec = 0

  nt = 1

  do nm = 1, 12
    monthly = 0.d0

    write(file_riv_mon,'(1a,1a,i2.2)') trim(dir_out),'/runoff.m',nm
    open(30,file=file_riv_mon,form='unformatted',access='direct',recl=4)

    do nd = 1, mo(nm,nt)
      daily = 0.d0
      irec = irec + 1
      read(10,rec=irec) roff
      do j = 1, ny
        do i = 1, nx
          if ( roff(i,j) == undef ) roff(i,j) = 0.
          daily = daily + real(roff(i,j),8)
        end do
      end do
      irec1 = irec1 + 1
      write(20,rec=irec1) real(daily,4)
      monthly = monthly + daily
    end do
    annual = annual + monthly
    monthly = monthly / real(mo(nm,nt),8)
    irec2 = irec2 + 1
    write(30,rec=1) real(monthly,4)
    close(30)
  end do
  close(10)

  write(file_riv_ann,'(1a,1a)') trim(dir_out),'/annual.gd'
  open(40,file=file_riv_ann,form='unformatted',access='direct',recl=4)
  irec3 = 0
  annual = annual / real(yr(nt),8)
  irec3 = irec3 + 1
  write(40,rec=irec3) real(annual,4)  
  write(6,*) ' annual mean = ', real(annual,4)
  close(40)
     
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
