!-*-F90-*-
program main

  implicit none

  integer(4), parameter :: nx = 1440, ny = 720
  real(4) :: roff(nx,ny), mask(nx,ny), undef
  integer(4) :: tmp(0:nx+1,0:ny+1)
  real(8) :: clim, annual, monthly, daily

  character(256) :: file_river_in
  character(256) :: file_riv_ann
  character(256) :: file_riv_mon
  character(256) :: file_riv_tmp
  character(256) :: file_out_base

  integer(4) :: mo(12,2), yr(2)
  integer(4) :: nt, nyr, nm, nd, irec, i, j, tday, ii, jj
  integer(4) :: nbyr, neyr, nrec
  
  integer(4) :: irec1, irec2, irec3
  character(256) :: dir_out

  data yr / 365, 366 /
  data mo / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, &
          & 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
  data undef / 1e20 /

  !--------------------------------------------------------------------

  namelist /nml_total_runoff_monthly/ file_river_in, &
       & dir_out, file_out_base, &
       & nrec, nbyr, neyr

  !--------------------------------------------------------------------
 
  open(10, file='namelist.total_runoff_monthly')
  read(10, nml=nml_total_runoff_monthly)
  close(10)

  !--------------------------------------------------------------------

  do nyr = nbyr, neyr

    if (lleap(nyr)) then
      nt = 1
    else
      nt = 2
    end if

    annual = 0.d0

    do nm = 1, 12

      monthly = 0.d0

      write(file_riv_tmp,'(1a,i4.4,i2.2)') trim(file_river_in),nyr,nm
    
      open (10,file=file_riv_tmp,form='unformatted',status='old',access='direct',recl=4*nx*ny)
      read (10,rec=nrec) roff
      close(10)

      write(file_riv_mon,'(1a,a1,1a,i4.4,i2.2)') &
           & trim(dir_out),'/',trim(file_out_base),nyr,nm
      open(30,file=file_riv_mon,form='unformatted',access='direct',recl=4)

      do j = 1, ny
        do i = 1, nx
          if ( roff(i,j) == undef ) roff(i,j) = 0.
          monthly = monthly + real(roff(i,j),8)
        end do
      end do

      write(30,rec=1) real(monthly,4)
      close(30)
      annual = annual + monthly * real(mo(nm,nt),8)

    end do

    write(file_riv_ann,'(1a,a1,1a,i4.4)') &
         & trim(dir_out),'/',trim(file_out_base),nyr
    open(40,file=file_riv_ann,form='unformatted',access='direct',recl=4)
    annual = annual / real(yr(nt),8)
    write(40,rec=1) real(annual,4)
    write(6,*) nyr, ' annual mean = ', real(annual,4)
    close(40)

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
