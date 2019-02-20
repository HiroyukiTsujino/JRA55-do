!-*-F90-*-
program mk_GrnFreshFlux_Bamber
  implicit none

  character(len=128) :: cfile_in  = 'data_Grn_Bamber_2018_org/grads_monthly/Bamber_2018_025x025.YYYYMM'
  character(len=128) :: cfile_out_lqd = 'data_Grn_Bamber_2018_new/grads_daily/Bamber_lqd_2018_025x025.YYYY'
  character(len=128) :: cfile_out_sld = 'data_Grn_Bamber_2018_new/grads_daily/Bamber_sld_2018_025x025.YYYY'
  real(4), parameter :: undef_in = -9.99e33

  integer(4), parameter :: nx = 360 * 4, ny = 180 * 4
  real(4) :: dx = 0.25e0, dy = 0.25e0
  real(8) :: lqd_monthly(nx,ny,0:13), lqd_daily(nx,ny)
  real(8) :: sld_monthly(nx,ny,0:13), sld_daily(nx,ny)
  real(8) :: fwf_monthly(nx,ny,0:13), fwf_daily(nx,ny)
  real(4) :: tundra(nx,ny), icemlt(nx,ny), sldice(nx,ny)

  integer(4) :: nyr
  real(8) :: month_days(0:13) 
  real(8) :: days_sum(12) 

  logical :: l_fwf_exist(nx,ny)
  
  character(len=128) :: cfile_nextxy_org = &
       & 'data_etc/nextxy_wo_lake_big_endian_noyrev_lon0strt_rm_GA.bin'
  character(len=128) :: cfile_nextxy_new = &
       & 'data_etc/nextxy_wo_lake_big_endian_noyrev_lon0strt_rm_GA_add_Bamber_2018.bin'  
  logical :: l_leap

  !------------------------------------------------------------

  l_fwf_exist(:,:) = .false.

  do nyr = 1958, 2018

    if ( mod(nyr,4) == 0 ) then
      l_leap = .true. 
    else
      l_leap = .false.
    end if

    call set_calendar  (l_leap)
    call mk_daily_main (nyr)

  end do

  call modify_nextxy

contains
!---------------------------------------------------------------    
subroutine modify_nextxy
  implicit none
  integer(4) :: nextx_org(nx,ny), nexty_org(nx,ny)
  integer(4) :: nextx_new(nx,ny), nexty_new(nx,ny)

  integer(4) :: i, j, n, index_mn
  integer(4), parameter :: next_Grn = -888
  character(len=128) :: cfile

  write(6,*) ' modify_nextxy '

  open(11,file=cfile_nextxy_org,form='unformatted',access='direct',&
       & action='read',recl=4*nx*ny)
  read(11,rec=1) nextx_org
  read(11,rec=2) nexty_org
  close(11)

  do j = 1, ny
    do i = 1, nx
      nextx_new(i,j) = nextx_org(i,j)
      nexty_new(i,j) = nexty_org(i,j)
      if ( nextx_org(i,j) == next_Grn ) then
        nextx_new(i,j) = 0
        nexty_new(i,j) = 0        
      end if
      if ( l_fwf_exist(i,j) ) then
        nextx_new(i,j) = next_Grn
        nexty_new(i,j) = next_Grn
      end if
    end do
  end do

  open(11,file=cfile_nextxy_new,form='unformatted',access='direct', &
       & action='write',recl=4*nx*ny)
  write(11,rec=1) nextx_new
  write(11,rec=2) nexty_new
  close(11)
  
end subroutine modify_nextxy
!---------------------------------------------------------------    
subroutine set_calendar     (l_leap)

  logical,intent(in) :: l_leap
  integer(4) :: mn

  !--------------------------

  month_days( 1) = 31.d0
  if (l_leap) then
    month_days( 2) = 29.d0
  else
    month_days( 2) = 28.d0
  end if
  month_days( 3) = 31.d0
  month_days( 4) = 30.d0
  month_days( 5) = 31.d0
  month_days( 6) = 30.d0
  month_days( 7) = 31.d0
  month_days( 8) = 31.d0
  month_days( 9) = 30.d0
  month_days(10) = 31.d0
  month_days(11) = 30.d0
  month_days(12) = 31.d0

  month_days( 0) = month_days(12)
  month_days(13) = month_days( 1)

  days_sum(1) = month_days(1)
  do mn = 2, 12
    days_sum(mn) = days_sum(mn-1) + month_days(mn)
  end do    
  
end subroutine set_calendar
!--------------------------------------------------------
subroutine mk_daily_main (nyear)

  integer(4), intent(in) :: nyear
  real(8) :: now
  real(8) :: total 
  integer(4) :: ndylast, index_dy, index_yr
  integer(4) :: i, j, mn, ndy, ndy_tmp
  character(len=128) :: cfile
  integer(4) :: nrec
  integer(4) :: ny_prev, ny_now, ny_next

  !------------------------------------------------------

  write(6,*) ' mk_daily_main '

  ny_prev = nyear - 1
  ny_now  = nyear
  ny_next = nyear + 1

  ndy_tmp = int(days_sum(12))

  write(6,*) ' Year = ', nyear
  write(6,*) ' Days = ', ndy_tmp

  ! December of the previous year

  mn = 12
  cfile = cfile_in
  index_yr = index(cfile,"YYYYMM")
  write(cfile(index_yr:index_yr+5),'(i4.4,i2.2)') ny_prev, mn
  write(6,*) 'Read from ', trim(cfile)
  open(11,file=cfile, form='unformatted',access='direct',action='read',recl=4*nx*ny)
  read(11,rec=1) tundra
  read(11,rec=2) icemlt
  read(11,rec=3) sldice
  close(11)

  do j = 1, ny
    do i = 1, nx
      if ((tundra(i,j) /= undef_in) .and. (icemlt(i,j) /= undef_in) .and. (sldice(i,j) /= undef_in)) then
        lqd_monthly(i,j,0) = real(tundra(i,j) + icemlt(i,j),8)
        sld_monthly(i,j,0) = real(sldice(i,j),8)
        fwf_monthly(i,j,0) = lqd_monthly(i,j,0) + sld_monthly(i,j,0)
      end if
    end do
  end do

  ! main year

  do mn = 1, 12

    cfile = cfile_in
    index_yr = index(cfile,"YYYYMM")
    write(cfile(index_yr:index_yr+5),'(i4.4,i2.2)') ny_now, mn
    write(6,*) 'Read from ', trim(cfile)
    open(11,file=cfile, form='unformatted',access='direct',action='read',recl=4*nx*ny)
    read(11,rec=1) tundra
    read(11,rec=2) icemlt
    read(11,rec=3) sldice
    close(11)

    do j = 1, ny
      do i = 1, nx
        if ((tundra(i,j) /= undef_in) .and. (icemlt(i,j) /= undef_in) .and. (sldice(i,j) /= undef_in)) then
          lqd_monthly(i,j,mn) = real(tundra(i,j) + icemlt(i,j),8)
          sld_monthly(i,j,mn) = real(sldice(i,j),8)
          fwf_monthly(i,j,mn) = lqd_monthly(i,j,mn) + sld_monthly(i,j,mn)
          if (fwf_monthly(i,j,mn) > 0.0d0) then
            l_fwf_exist(i,j) = .true.
          end if
        end if
      end do
    end do

  end do

  ! January of the next year

  mn = 1
  cfile = cfile_in
  index_yr = index(cfile,"YYYYMM")
  write(cfile(index_yr:index_yr+5),'(i4.4,i2.2)') ny_next, mn
  write(6,*) 'Read from ', trim(cfile)
  open(11,file=cfile, form='unformatted',access='direct',action='read',recl=4*nx*ny)
  read(11,rec=1) tundra
  read(11,rec=2) icemlt
  read(11,rec=3) sldice
  close(11)

  do j = 1, ny
    do i = 1, nx
      if ((tundra(i,j) /= undef_in) .and. (icemlt(i,j) /= undef_in) .and. (sldice(i,j) /= undef_in)) then
        lqd_monthly(i,j,13) = real(tundra(i,j) + icemlt(i,j),8)
        sld_monthly(i,j,13) = real(sldice(i,j),8)
        fwf_monthly(i,j,13) = lqd_monthly(i,j,13) + sld_monthly(i,j,13)
      end if
    end do
  end do

  ! daily file

  cfile = cfile_out_lqd
  index_yr = index(cfile,"YYYY")
  write(cfile(index_yr:index_yr+3),'(I4.4)') nyear
  write(6,*) 'Written to ', trim(cfile)
  open(11,file=cfile, form='unformatted',access='direct',recl=4*nx*ny)
  write(6,*) 'Output :', trim(cfile)

  do ndy = 1, ndy_tmp
    now = dble(ndy) - 0.5d0
    call mk_daily_now(lqd_daily, now, lqd_monthly) ! linear interpolation
    write(11,rec=ndy) real(lqd_daily,4)
  end do
  
  close(11)

  cfile = cfile_out_sld
  index_yr = index(cfile,"YYYY")
  write(cfile(index_yr:index_yr+3),'(I4.4)') nyear
  write(6,*) 'Written to ', trim(cfile)
  open(11,file=cfile, form='unformatted',access='direct',recl=4*nx*ny)
  write(6,*) 'Output :', trim(cfile)

  do ndy = 1, ndy_tmp
    now = dble(ndy) - 0.5d0
    call mk_daily_now(sld_daily, now, sld_monthly) ! linear interpolation
    write(11,rec=ndy) real(sld_daily,4)
  end do
  
  close(11)

end subroutine mk_daily_main
!--------------------------------------------------------
subroutine mk_daily_now (r2d_now, now, r2d_monthly)

  real(8),intent(in)    :: now
  real(8),intent(out)   :: r2d_now(nx,ny)
  real(8),intent(in)    :: r2d_monthly(nx,ny,0:13)

  integer(4) :: mn, n1, n2
  real(8) :: f1, f2

  if ( now <= 0.5d0 * month_days(1) ) then
    n1 = 0
    n2 = 1
    f1 = - 0.5d0 * month_days(12)
    f2 =   0.5d0 * month_days(1)
  else if ( now >= days_sum(12) - 0.5d0 * month_days(12) ) then
    n1 = 12
    n2 = 13
    f1 =  days_sum(12) - 0.5d0 * month_days(12)
    f2 =  days_sum(12) + 0.5d0 * month_days(1)
  else
    do mn = 1, 11
      if ( now <= 0.5d0 * (days_sum(mn) + days_sum(mn+1)) ) then
        n1 = mn
        n2 = mn + 1
        f1 =  days_sum(mn) - 0.5d0 * month_days(mn)
        f2 =  days_sum(mn) + 0.5d0 * month_days(mn+1)
        exit
      end if
    end do
  end if  
  
!  write(6,*) 'Weights'
!  write(6,*) n1, (f2-now)/(f2-f1)
!  write(6,*) n2, (now-f1)/(f2-f1)

  r2d_now(:,:) =  r2d_monthly(:,:,n1) * (f2-now)/(f2-f1) &
              & + r2d_monthly(:,:,n2) * (now-f1)/(f2-f1)      

end subroutine mk_daily_now
!=======================================================================
end program mk_GrnFreshFlux_Bamber
