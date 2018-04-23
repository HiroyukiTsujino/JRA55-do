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

  integer(4) :: mo(12,2), yr(2)
  integer(4) :: ys, ye

  integer(4) :: nt, nyr, nm, nd, irec, i, j, tday, ii, jj
  
  integer(4) :: irec1, irec2, irec3

  data ys, ye / 1981, 2013 /
  data yr / 365, 366 /
  data mo / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, &
          & 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
  data undef / 1e20 /

  cfriv='/worke/htsujino/RUNOFF_YOSHIMURA/nextxy_wo_lake.bin'
  open(10,file=cfriv,form='unformatted',status='old',access='direct',recl=4*nx*ny)
  read(10,rec=1) tmp(1:nx,1:ny)
  close(10)
  tmp(1:nx,0) = -9999
  tmp(1:nx,ny+1) = 1
  tmp(0,1:ny) = tmp(nx,1:ny)
  tmp(nx+1,1:ny) = tmp(1,1:ny)
  do j = 1, ny
    do i = 1, nx
      if ( tmp(i,j) == -9 ) then
        !do jj = j-1, j+1                              !
        !  do ii = i-1, i+1                            ! only in 
        !    if ( tmp(ii,jj) == -9999 ) mask(i,j) = 1. !  river mouths
        !  end do                                      ! adjacent to ocean grids
        !end do                                        !
        mask(i,j) = 1.    !-- all river mouths
      else
        mask(i,j) = 0.
      end if
    end do
  end do

  open(20,file='result/daily.gd',form='unformatted',access='direct',recl=4)
  irec1 = 0

  open(50,file='result/clim.txt')

  clim = 0.d0
  tday = 0
  do nyr = ys, ye
    write(*,*) nyr
    annual = 0.d0
    write(file_river_in,'(1a,i4.4,1a)') &
         & '/worke/htsujino/RUNOFF_YOSHIMURA/rivout',nyr,'.bin'
    open(10,file=file_river_in,form='unformatted',status='old',access='direct',recl=4*nx*ny)
    irec = 0

    write(file_riv_ann,'(1a,i4.4)') 'result/runoff.',nyr
    open(40,file=file_riv_ann,form='unformatted',access='direct',recl=4)

    if ( lleap(nyr) ) then
      nt = 2
    else
      nt = 1
    end if
    do nm = 1, 12
      monthly = 0.d0

      write(file_riv_mon,'(1a,i4.4,i2.2)') 'result/runoff.',nyr,nm
      open(30,file=file_riv_mon,form='unformatted',access='direct',recl=4)

      do nd = 1, mo(nm,nt)
        daily = 0.d0
        irec = irec + 1
        read(10,rec=irec) roff
        do j = 1, ny
          do i = 1, nx
            if ( roff(i,j) == undef ) roff(i,j) = 0.
            daily = daily + real(roff(i,j)*mask(i,j),8)
          end do
        end do
        write(cdate,'(I4.4,I2.2,I2.2)') nyr,nm,nd
        irec1 = irec1 + 1
        write(20,rec=irec1) real(daily,4)
        monthly = monthly + daily
      end do
      annual = annual + monthly
      monthly = monthly / real(mo(nm,nt),8)
      write(cdate,'(I4.4,I2.2)') nyr,nm
      irec2 = irec2 + 1
      write(30,rec=1) real(monthly,4)
      close(30)
    end do
    clim = clim + annual
    annual = annual / real(yr(nt),8)
    irec3 = irec3 + 1
    write(40,rec=1) real(annual,4)
    close(40)
    tday = tday + irec
    close(10)
  end do
  clim = clim / real(tday,8)
  write(50,*) clim
    
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
