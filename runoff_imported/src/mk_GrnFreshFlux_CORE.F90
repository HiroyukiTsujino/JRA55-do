!-*-F90-*-
program mk_GrnFreshFlux_CORE
  implicit none

  character(len=128) :: cfile_clm             = 'data_Grn_CORE_org/monclim_025x025/fwf_green_025x025.MM'
  character(len=128) :: cfile_clm_daily       = 'data_Grn_CORE_new/dayclim_025x025/fwf_green_025x025_DDDdy'
  character(len=128) :: cfile_clim_total      = 'data_Grn_CORE_new/monclim_025x025/total_fwf_green.MM'
  character(len=128) :: cfile_clm_daily_total = 'data_Grn_CORE_new/dayclim_025x025/total_fwf_green_DDDdy'

  character(len=128) :: cfile_total

  integer(4), parameter :: nx = 360 * 4, ny = 180 * 4
  real(4) :: dx = 0.25e0, dy = 0.25e0
  real(8) :: fwf_clim(nx,ny,12), fwf_clim_f(nx,ny,12), fwf_daily(nx,ny)
  real(4) :: work4(nx,ny)

  integer(4),parameter :: clim_yr_str = 1961, clim_yr_end = 1990

  integer(4) :: nyr, nmn
  real(8) :: month_days(0:13) 
  real(8) :: days_sum(12) 

  logical :: l_fwf_exist(nx,ny)
  
  character(len=128) :: cfile_nextxy_org = &
       & 'data_etc/nextxy_wo_lake_big_endian_noyrev_lon0strt_rm_GA.bin'
  character(len=128) :: cfile_nextxy_new = &
       & 'data_etc/nextxy_wo_lake_big_endian_noyrev_lon0strt_rm_GA_add_CORE.bin'  
  logical :: l_leap
  integer(4) :: i_check, j_check

  real(8),save :: days_Feb_mean
  real(8) :: annual_mean

  l_leap = .false.
  call set_calendar  (l_leap)
  call main
  call modify_nextxy

  i_check = 1347; j_check = 637

  ! Create daily data for convenience.

!------- Simple version ---------------
  l_leap = .false.
  call set_calendar  (l_leap)
  call killworth_filter  
  call mk_daily_main (i_check, j_check)

contains
!---------------------------------------------------------------    
subroutine main 

  integer(4) :: nmn, i, j
  character(len=128) :: cfile
  real(8) :: total, total_mon
  integer(4) :: index_mn

  total = 0.d0
  do nmn = 1, 12
    cfile = cfile_clm
    index_mn = index(cfile,"MM")
    write(cfile(index_mn:index_mn+1),'(I2.2)') nmn
    write(6,*) 'Read from ...', trim(cfile)
    open(11,file=cfile,form='unformatted',access='direct',recl = 4 * nx * ny)
    read(11,rec=1) work4
    fwf_clim(:,:,nmn) = real(work4(:,:),8)
    close(11)
    total_mon = 0.d0
    do j = 1, ny
      do i = 1, nx
        total = total + fwf_clim(i,j,nmn) * month_days(nmn) 
        total_mon = total_mon + fwf_clim(i,j,nmn)
      end do
    end do

    total_mon = total_mon * 1.d-6                   ! m^3/s   -> Sv
    cfile = cfile_clim_total
    index_mn = index(cfile,"MM")
    write(cfile(index_mn:index_mn+1),'(I2.2)') nmn
    open (11,file=cfile,form='unformatted',access='direct',recl = 4)
    write(11,rec=1) real(total_mon,4)
    close(11)

  end do
  total = total / 365.d0
  annual_mean = total
  total = total * 1.d-6                   ! m^3/s   -> Sv
  write(6,'(A,F15.7,A)') 'total (clim)=', total*1000.d0,  ' mSv'

end subroutine main
!---------------------------------------------------------------    
subroutine modify_nextxy
  implicit none
  integer(4) :: nextx_org(nx,ny), nexty_org(nx,ny)
  integer(4) :: nextx_new(nx,ny), nexty_new(nx,ny)

  integer(4) :: i, j, n, index_mn
  integer(4), parameter :: next_Grn = -888
  character(len=128) :: cfile

  write(6,*) ' modify_nextxy '

  l_fwf_exist(:,:) = .false.
  
  do n = 1, 12
    cfile = cfile_clm
    index_mn = index(cfile,"MM")
    write(cfile(index_mn:index_mn+1),'(I2.2)') n
    write(6,*) 'Reading from ', trim(cfile)
    open(11,file=cfile,form='unformatted',access='direct',recl = 4 * nx * ny)
    read(11,rec=1) work4
    fwf_clim(:,:,n) = real(work4(:,:),8)
    do j = 1, ny
      do i = 1, nx
        if ( fwf_clim(i,j,n) > 0.d0 ) l_fwf_exist(i,j) = .true.
      end do
    end do
    close(11)
  end do

  open(11,file=cfile_nextxy_org,form='unformatted',access='direct',recl=4*nx*ny)
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

  open(11,file=cfile_nextxy_new,form='unformatted',access='direct',recl=4*nx*ny)
  write(11,rec=1) nextx_new
  write(11,rec=2) nexty_new
  close(11)
  
end subroutine modify_nextxy
!---------------------------------------------------------------    
subroutine set_calendar     (l_leap)
  logical,intent(in) :: l_leap
  integer(4) :: mn


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
!---------------------------------------------------------------    
subroutine killworth_filter  
  implicit none
!  logical,intent(in) :: cfile_clm_f

  ! Killworth (1996), JPO, (26), 136-143
  real(8) :: a(12), b(12), c(12), r(12), x(12), beta, alpha
  integer(4) :: n, i, j, ndy
  character(len=128) :: cfile
  integer(4) :: index_mn, ndylast, index_dy
  logical :: l_negative (nx,ny)

  write(6,*) ' Killworth filter '

  do n = 1, 12
    cfile = cfile_clm
    index_mn = index(cfile,"MM")
    write(cfile(index_mn:index_mn+1),'(I2.2)') n
    write(6,*) 'reading from ', trim(cfile)
    open(11,file=cfile,form='unformatted',access='direct',recl = 4 * nx * ny)
    read(11,rec=1) work4
    fwf_clim(:,:,n) = real(work4(:,:),8)
    close(11)
  end do

  do n = 1, 12
    a(n) = month_days(n)/(4.d0 * (month_days(n-1) + month_days(n  )))
    c(n) = month_days(n)/(4.d0 * (month_days(n)   + month_days(n+1)))
    b(n) = 1.d0 - a(n) - c(n)
  end do
  beta  = a(1)
  alpha = c(12)

  l_negative(:,:) = .false.
  do j = 1, ny
    do i = 1, nx
      do n = 1, 12
        r(n) = fwf_clim(i,j,n)
      end do
      call cyclic(a,b,c,alpha,beta,r,x,12)
      do n = 1, 12
        if ( x(n) < 0.0d0 ) l_negative(i,j) = .true.
        fwf_clim_f(i,j,n) = x(n)
      end do
    end do
  end do

  ! if  l_negative then fwf_clim_f = fwf_clim
  do j = 1, ny
    do i = 1, nx
      if ( l_negative(i,j) ) then
        do n = 1, 12
          fwf_clim_f(i,j,n) = fwf_clim(i,j,n)
        end do
      end if
    end do
  end do

end subroutine killworth_filter

!--------------------------------------------------------
subroutine mk_daily_main (i_check, j_check)
  integer(4),intent(in) :: i_check, j_check
  real(8) :: now
  real(8) :: tmp1(366), tmp2(366), tmp1_mon(12), tmp2_mon(12)
  real(8) :: total, total_sum, adjust_factor
  real(8) :: total_sum_leap
  integer(4) :: ndylast, index_dy, index_yr
  integer(4) :: i, j, mn, ndy, ndy_tmp 
  character(len=128) :: cfile, cfile_total
  integer(4) :: nrec

  real(8) :: fwf_daily_28Feb(nx,ny)
  real(8) :: fwf_daily_01Mar(nx,ny)
  real(8) :: fwf_daily_29Feb(nx,ny)

  write(6,*) ' mk_daily_main '

  ndy_tmp = int(days_sum(12))

  cfile = cfile_clm_daily
  index_dy = index(cfile,"DDD")
  write(cfile(index_dy:index_dy+2),'(I3.3)') ndy_tmp
  write(6,*) 'Written to ', trim(cfile)
  open(11,file=cfile, form='unformatted',access='direct',recl=4*nx*ny)

  cfile = cfile_clm_daily
  index_dy = index(cfile,"DDD")
  write(cfile(index_dy:index_dy+2),'(I3.3)') 366
  write(6,*) 'Written to ', trim(cfile)
  open(12,file=cfile, form='unformatted',access='direct',recl=4*nx*ny)

  cfile_total = cfile_clm_daily_total
  index_dy = index(cfile_total,"DDD")
  write(cfile_total(index_dy:index_dy+2),'(I3.3)') ndy_tmp
  write(6,*) 'Written to ', trim(cfile_total)
  open(21,file=cfile_total, form='unformatted',access='direct',recl=4)
    
  total_sum = 0.d0
  do ndy = 1, ndy_tmp
    now = dble(ndy) - 0.5d0
    call mk_daily_now(fwf_daily, now, fwf_clim_f) ! apply Killworth filter
!    write(11,rec=ndy) fwf_daily
    tmp1(ndy) = fwf_daily(i_check,j_check)
    total = sum(fwf_daily)
    total_sum = total_sum + total
    total = total * 1.d-6  ! m^3/s -> Sv
!    write(21,rec=ndy) real(total,4)

    call mk_daily_now(fwf_daily, now, fwf_clim  ) ! linear interpolation
    tmp2(ndy) = fwf_daily(i_check,j_check)
  end do
  adjust_factor = annual_mean / (total_sum / 365.d0)
  write(6,*) 'annual mean (mSv) (killworth : no adjust)'
  write(6,*) 'original data             ',  'from daily (mSv: normal year) '
  write(6,*) annual_mean*1.d-3, total_sum/365.d0*1.d-3
  write(6,*) 'adjusted factor=', adjust_factor
  
  total_sum      = 0.d0
  total_sum_leap = 0.d0
  nrec = 0
  do ndy = 1, ndy_tmp
    now = dble(ndy) - 0.5d0
    call mk_daily_now(fwf_daily, now, fwf_clim_f)
    fwf_daily(:,:) = fwf_daily(:,:) * adjust_factor
    write(11,rec=ndy) real(fwf_daily,4)

    if ( ndy == 31 + 28     ) fwf_daily_28Feb(:,:) = fwf_daily(:,:)
    if ( ndy == 31 + 28 + 1 ) then
      fwf_daily_01Mar(:,:) = fwf_daily(:,:)
      fwf_daily_29Feb(:,:) = 0.5d0 * (fwf_daily_28Feb(:,:) + fwf_daily_01Mar(:,:))
      nrec = nrec + 1
      write(12,rec=nrec) real(fwf_daily_29Feb,4)
      total_sum_leap = total_sum_leap + sum(fwf_daily_29Feb)
    end if

    nrec = nrec + 1
    write(12,rec=nrec) real(fwf_daily,4)

    tmp1(ndy) = fwf_daily(i_check,j_check)
    total = sum(fwf_daily)
    total_sum      = total_sum      + total
    total_sum_leap = total_sum_leap + total
    total = total * 1.d-6  ! m^3/s -> Sv
    write(21,rec=ndy) real(total,4)

    call mk_daily_now(fwf_daily, now, fwf_clim  )
    tmp2(ndy) = fwf_daily(i_check,j_check)
  end do
  adjust_factor = annual_mean  / (total_sum / 365.d0)
  write(6,*) 'annual_mean (mSv) (killworth: adjusted)' 
  write(6,*) 'original data           ',  'from daily (normal year) ','  from daily (leap year) '
  write(6,*) annual_mean*1.d-3, total_sum/365.d0*1.d-3, total_sum_leap/366.d0*1.d-3

  close(11)
  close(12)
  close(21)
  

  ! Check

  i = i_check
  j = j_check

  mn = 1
  tmp1_mon(:) = 0.d0
  tmp2_mon(:) = 0.d0
  open(11,file='check_CORE_green.txt')
  write(6,*) 'check at ',i_check, j_check
  write(6,*) '        filtered, not filtered.  [relative error (%)] '
  do ndy = 1, ndy_tmp
    tmp1_mon(mn) = tmp1_mon(mn) + tmp1(ndy)
    tmp2_mon(mn) = tmp2_mon(mn) + tmp2(ndy)
    write(11,*) ndy-0.5d0, tmp1(ndy), fwf_clim(i,j,mn)
    if ( ndy >= days_sum(mn) ) then
      tmp1_mon(mn) = tmp1_mon(mn) / month_days(mn)
      tmp2_mon(mn) = tmp2_mon(mn) / month_days(mn)
      write(6,'(A,2F14.7)') '        ',  &
           & (tmp1_mon(mn)-fwf_clim(i_check,j_check,mn))/abs(fwf_clim(i_check,j_check,mn))*100.d0, &
           & (tmp2_mon(mn)-fwf_clim(i_check,j_check,mn))/abs(fwf_clim(i_check,j_check,mn))*100.d0
      mn = mn + 1
    end if
  end do
  close(11)

  open(11,file='check_CORE_green_2.txt')
  do mn = 1, 12
    if (mn == 1) then
      write(11,*) days_sum(1)*0.5d0, fwf_clim_f(i,j,mn)
    else
      write(11,*) (days_sum(mn)+days_sum(mn-1))*0.5d0, fwf_clim_f(i,j,mn)
    end if
  end do
  close(11)

end subroutine mk_daily_main
!--------------------------------------------------------
subroutine mk_daily_now (r2d_now, now, r2d_monthly)
  real(8),intent(in)    :: now
  real(8),intent(out)   :: r2d_now(nx,ny)
  real(8),intent(in)    :: r2d_monthly(nx,ny,12)

  integer(4) :: mn, n1, n2
  real(8) :: f1, f2

  if ( now <= 0.5d0 * month_days(1) ) then
    n1 = 12
    n2 = 1
    f1 = - 0.5d0 * month_days(12)
    f2 =   0.5d0 * month_days(1)
  else if ( now >= days_sum(12) - 0.5d0 * month_days(12) ) then
    n1 = 12
    n2 = 1
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
  
  r2d_now(:,:) =  r2d_monthly(:,:,n1) * (f2-now)/(f2-f1) &
              & + r2d_monthly(:,:,n2) * (now-f1)/(f2-f1)      
end subroutine mk_daily_now
!=======================================================================
end program mk_GrnFreshFlux_CORE

! following subroutines are taken from Numerical recipes
subroutine tridag(a,b,c,r,u,n)
  implicit none
  integer(4),intent(in) :: n
  real(8),intent(in)  :: a(n),b(n),c(n),r(n)
  real(8),intent(out) :: u(n)
  integer(4),parameter :: nmax = 500

  integer(4) :: j
  real(8) :: bet,gam(nmax)
  
  bet = b(1)
  u(1) = r(1)/bet
  do j = 2, n
    gam(j) = c(j-1)/bet
    bet = b(j) - a(j) * gam(j)
    u(j) = (r(j)-a(j)*u(j-1))/bet
  end do
  do j = n-1,1,-1
    u(j) = u(j)-gam(j+1)*u(j+1)
  end do
end subroutine tridag

subroutine cyclic(a,b,c,alpha,beta,r,x,n)
  implicit none
  integer(4),intent(in) :: n
  real(8),intent(in)  :: alpha,beta,a(n),b(n),c(n),r(n)
  real(8),intent(out) :: x(n)
  integer(4),parameter :: nmax = 500
  
  integer(4) :: i
  real(8) fact, gamma, bb(nmax),u(nmax), z(nmax)
  
  gamma=-b(1)
  bb(1)=b(1)-gamma
  bb(n)=b(n)-alpha*beta/gamma
  do i = 2, n-1
    bb(i) = b(i)
  end do
  call tridag(a,bb,c,r,x,n)
  u(1) = gamma
  u(n) = alpha
  do i = 2,n-1
    u(i) = 0
  end do
  call tridag(a,bb,c,u,z,n)
  fact=(x(1)+beta*x(n)/gamma)/(1.d0+z(1)+beta*z(n)/gamma)
  do i = 1, n
    x(i) = x(i)-fact*z(i)
  end do
end subroutine cyclic
