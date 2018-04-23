!-*-F90-*-
program mk_GrnFreshFlux_Bamber
  implicit none
  character(len=128) :: cfile_clm           = 'data_Grn_CORE_org/runoff_greenland.MM'
  character(len=128) :: cfile_clm_daily     = 'data_Grn_CORE_new/fwf_green_025x025_DDDdy'
  character(len=128) :: cfile_clm_daily_4yr = 'data_Grn_CORE_new/fwf_green_025x025_DDDdy_X'

  integer(4), parameter :: nx = 360 * 4, ny = 180 * 4
  real(4) :: dx = 0.25e0, dy = 0.25e0
  real(4) :: fwf(nx,ny), fwf_clim(nx,ny,12), fwf_clim_f(nx,ny,12), fwf_clim_f_4yr(nx,ny,48), fwf_daily(nx,ny)

  integer(4),parameter :: clim_yr_str = 1961, clim_yr_end = 1990

  integer(4) :: nyr, nmn
  real(8) :: month_days(0:13) 
  real(8) :: days_sum(12) 

  real(8) :: month_days_4yr(0:49) 
  real(8) :: days_sum_4yr(48) 

  logical :: l_fwf_exist(nx,ny)
  logical :: l_leap
  integer(4) :: i_check, j_check

  real(8) :: total_org

  l_leap = .false.
  call set_calendar  (l_leap)
  call main

  ! Create daily data for convenience.

!------- Simple version ---------------
  l_leap = .false.
  call set_calendar  (l_leap)
  call killworth_filter  
  call mk_daily_main 

  l_leap = .true.
  call set_calendar  (l_leap)
  call killworth_filter ! filter should be created again.
  call mk_daily_main 
  
!------- Strict version ---------------------
  ! 4-year cycle 
  call set_calendar_4yr
  call killworth_filter_4yr
  call mk_daily_main_4yr 
!---------------------------------------------
  
contains
!---------------------------------------------------------------    
subroutine main 
  integer(4) :: nmn, i, j
  real(4) :: fwf_clim(nx,ny,12)
  character(len=128) :: cfile
  real(8) :: total
  integer(4) :: index_mn

  total = 0.d0
  do nmn = 1, 12
    cfile = cfile_clm
    index_mn = index(cfile,"MM")
    write(cfile(index_mn:index_mn+1),'(I2.2)') nmn
    write(*,*) trim(cfile)
    open(11,file=cfile,form='unformatted',access='direct',recl = 4 * nx * ny)
    read(11,rec=1) fwf_clim(:,:,nmn)
    close(11)
    do j = 1, ny
      do i = 1, nx
        total = total + fwf_clim(i,j,nmn) * month_days(nmn) 
      end do
    end do
  end do
  total = total / 365.d0 * 1.d-6                   ! m^3/s   -> Sv
  total_org = total
  write(*,'(A,F15.7,A)') 'total (clim)=', total*100.d0,  ' mSv'

end subroutine main
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

subroutine set_calendar_4yr
  integer(4) :: mn

  
  do nyr = 1, 4
    if ( nyr <= 3 ) call set_calendar ( .false. )
    if ( nyr == 4 ) call set_calendar ( .true. )
    do mn = 1, 12
      month_days_4yr(12*(nyr-1)+mn) = month_days(mn)
    end do
  end do
  month_days_4yr( 0) = month_days(48)
  month_days_4yr(49) = month_days( 1)

  days_sum_4yr(1) = month_days_4yr(1)
  do mn = 2, 48
    days_sum_4yr(mn) = days_sum_4yr(mn-1) + month_days_4yr(mn)
  end do    
  
end subroutine set_calendar_4yr
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

  do n = 1, 12
    cfile = cfile_clm
    index_mn = index(cfile,"MM")
    write(cfile(index_mn:index_mn+1),'(I2.2)') n
    write(*,*) trim(cfile)
    open(11,file=cfile,form='unformatted',access='direct',recl = 4 * nx * ny)
    read(11,rec=1) fwf_clim(:,:,n)
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
        if ( x(n) < 0 ) l_negative(i,j) = .true.
        fwf_clim_f(i,j,n) = x(n)
      end do
    end do
  end do

  ! if  l_negative then fwf_clim_f_4yr = fwf_clim
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
!---------------------------------------------------------------    
subroutine killworth_filter_4yr
  implicit none
!  logical,intent(in) :: cfile_clm_f
  ! 4-yr cycle of killworth_filter

  ! Killworth (1996), JPO, (26), 136-143
  real(8) :: a(48), b(48), c(48), r(48), x(48), beta, alpha
  integer(4) :: n, i, j, ndy
  character(len=128) :: cfile
  integer(4) :: index_mn, ndylast, index_dy, nyr
  logical :: l_negative(nx,ny)

  do n = 1, 12
    cfile = cfile_clm
    index_mn = index(cfile,"MM")
    write(cfile(index_mn:index_mn+1),'(I2.2)') n
    write(*,*) trim(cfile)
    open(11,file=cfile,form='unformatted',access='direct',recl = 4 * nx * ny)
    read(11,rec=1) fwf_clim(:,:,n)
    close(11)
  end do

  do n = 1, 48
    a(n) = month_days_4yr(n)/(4.d0 * (month_days_4yr(n-1) + month_days_4yr(n  )))
    c(n) = month_days_4yr(n)/(4.d0 * (month_days_4yr(n)   + month_days_4yr(n+1)))
    b(n) = 1.d0 - a(n) - c(n)
  end do
  beta  = a(1)
  alpha = c(48)

  l_negative(:,:) = .false.
  do j = 1, ny
    do i = 1, nx
      do nyr = 1, 4
        do n = 1, 12
          r(12*(nyr-1)+n) = fwf_clim(i,j,n)
        end do
      end do
      call cyclic(a,b,c,alpha,beta,r,x,48)
      do n = 1, 48
        if ( x(n) < 0 ) l_negative(i,j) = .true.
        fwf_clim_f_4yr(i,j,n) = x(n)
      end do
    end do
  end do

  ! if  l_negative then fwf_clim_f_4yr = fwf_clim
  do j = 1, ny
    do i = 1, nx
      if ( l_negative(i,j) ) then
        do nyr = 1, 4
          do n = 1, 12
            fwf_clim_f_4yr(i,j,12*(nyr-1)+n) = fwf_clim(i,j,n)
          end do
        end do
      end if
    end do
  end do

end subroutine killworth_filter_4yr
!--------------------------------------------------------
subroutine mk_daily_main 
  real(8) :: now
  real(8) :: total
  integer(4) :: ndylast, index_dy, index_yr
  integer(4) :: i, j, mn, ndy, ndy_tmp 
  character(len=128) :: cfile, cfile_total
  real(8) :: adjust_factor

  ndy_tmp = int(days_sum(12))

  cfile = cfile_clm_daily
  index_dy = index(cfile,"DDD")
  write(cfile(index_dy:index_dy+2),'(I3.3)') ndy_tmp
  write(*,*) trim(cfile)
  open(11,file=cfile, form='unformatted',access='direct',recl=4*nx*ny)

  total = 0.d0
  do ndy = 1, ndy_tmp
    now = dble(ndy) - 0.5d0
    call mk_daily_now(fwf_daily, now, fwf_clim_f)
    do j = 1, ny
      do i = 1, nx
        total = total + fwf_daily(i,j)
      end do
    end do
  end do
  total = total / dble(ndy_tmp) * 1.d-6  ! m^3/s -> Sv
  write(*,'(A,I3.3,AF15.7,A)') 'total (',ndy_tmp,')=', total*100.d0,  ' mSv'
  adjust_factor = total_org / total
  write(*,'(A,I3.3,AF15.7)') 'adjust_factor (',ndy_tmp,')=', adjust_factor

  total = 0.d0
  do ndy = 1, ndy_tmp
    now = dble(ndy) - 0.5d0
    call mk_daily_now(fwf_daily, now, fwf_clim_f)
    fwf_daily = fwf_daily * adjust_factor
    write(11,rec=ndy) fwf_daily
    do j = 1, ny
      do i = 1, nx
        total = total + fwf_daily(i,j)
      end do
    end do
  end do
  close(11)
  total = total / dble(ndy_tmp) * 1.d-6  ! m^3/s -> Sv
  write(*,'(A,I3.3,AF15.7,A)') 'total (',ndy_tmp,')=', total*100.d0,  ' mSv (adjusted)'

end subroutine mk_daily_main
!--------------------------------------------------------
subroutine mk_daily_now (r2d_now, now, r2d_monthly)
  real(8),intent(in)    :: now
  real(4),intent(out)   :: r2d_now(nx,ny)
  real(4),intent(in)    :: r2d_monthly(nx,ny,12)

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
!================================================================--
subroutine mk_daily_main_4yr 
  real(8) :: now, now_4yr
  real(8) :: total
  integer(4) :: ndylast, index_dy, index_yr
  integer(4) :: i, j, mn, ndy, ndy_tmp, nyr, mn_1yr
  character(len=128) :: cfile, cfile_total
  logical :: l_leap 
  real(8) :: adjust_factor(4)

  do nyr = 1, 4
    if (nyr <= 3) ndy_tmp = 365
    if (nyr == 4) ndy_tmp = 366
  
    cfile = cfile_clm_daily_4yr
    index_dy = index(cfile,"DDD")
    write(cfile(index_dy:index_dy+2),'(I3.3)') ndy_tmp
    index_yr = index(cfile,"X")
    write(cfile(index_yr:index_yr),  '(I1.1)') nyr 
    write(*,*) trim(cfile)
    open(11,file=cfile, form='unformatted',access='direct',recl=4*nx*ny)

    total = 0.d0
    do ndy = 1, ndy_tmp
      now_4yr = dble(ndy) - 0.5d0 + 365.d0 * dble(nyr-1)
      call mk_daily_now_4yr(fwf_daily, now_4yr, fwf_clim_f_4yr)
      do j = 1, ny
        do i = 1, nx
          total = total + fwf_daily(i,j)
        end do
      end do
    end do
    total = total /dble(ndy_tmp) * 1.d-6  ! m^3/s -> Sv
    write(*,'(A,F15.7,A)') 'total (killworth 4yr)=', total*100.d0,  ' mSv', nyr
    adjust_factor(nyr) = total_org / total
    write(*,'(A,I3.3,AF15.7)') 'adjust_factor (',ndy_tmp,')=', adjust_factor(nyr)
  end do

  do nyr = 1, 4
    if (nyr <= 3) ndy_tmp = 365
    if (nyr == 4) ndy_tmp = 366
  
    cfile = cfile_clm_daily_4yr
    index_dy = index(cfile,"DDD")
    write(cfile(index_dy:index_dy+2),'(I3.3)') ndy_tmp
    index_yr = index(cfile,"X")
    write(cfile(index_yr:index_yr),  '(I1.1)') nyr 
    write(*,*) trim(cfile)
    open(11,file=cfile, form='unformatted',access='direct',recl=4*nx*ny)

    total = 0.d0
    do ndy = 1, ndy_tmp
      now_4yr = dble(ndy) - 0.5d0 + 365.d0 * dble(nyr-1)
      call mk_daily_now_4yr(fwf_daily, now_4yr, fwf_clim_f_4yr)
      fwf_daily = fwf_daily * adjust_factor(nyr)
      write(11,rec=ndy) fwf_daily
      do j = 1, ny
        do i = 1, nx
          total = total + fwf_daily(i,j)
        end do
      end do
      ! conventional, but wrong
      now = dble(ndy) - 0.5d0
      call mk_daily_now(fwf_daily, now, fwf_clim  )
    end do
    total = total /dble(ndy_tmp) * 1.d-6  ! m^3/s -> Sv
    write(*,'(A,F15.7,A)') 'total (killworth 4yr)=', total*100.d0,  ' mSv (adjusted)', nyr
  end do
    


end subroutine mk_daily_main_4yr
!--------------------------------------------------------
subroutine mk_daily_now_4yr (r2d_now, now, r2d_monthly)
  real(8),intent(in)    :: now
  real(4),intent(out)   :: r2d_now(nx,ny)
  real(4),intent(in)    :: r2d_monthly(nx,ny,48)

  integer(4) :: mn, n1, n2
  real(8) :: f1, f2

  if ( now <= 0.5d0 * month_days_4yr(1) ) then
    n1 = 48
    n2 = 1
    f1 = - 0.5d0 * month_days_4yr(48)
    f2 =   0.5d0 * month_days_4yr(1)
  else if ( now >= days_sum_4yr(48) - 0.5d0 * month_days_4yr(48) ) then
    n1 = 48
    n2 = 1
    f1 =  days_sum_4yr(48) - 0.5d0 * month_days_4yr(48)
    f2 =  days_sum_4yr(48) + 0.5d0 * month_days_4yr(1)
  else
    do mn = 1, 47
      if ( now <= 0.5d0 * (days_sum_4yr(mn) + days_sum_4yr(mn+1)) ) then
        n1 = mn
        n2 = mn + 1
        f1 =  days_sum_4yr(mn) - 0.5d0 * month_days_4yr(mn)
        f2 =  days_sum_4yr(mn) + 0.5d0 * month_days_4yr(mn+1)
        exit
      end if
    end do
  end if  
  
  r2d_now(:,:) =  r2d_monthly(:,:,n1) * (f2-now)/(f2-f1) &
              & + r2d_monthly(:,:,n2) * (now-f1)/(f2-f1)      
end subroutine mk_daily_now_4yr

!=======================================================================
end program mk_GrnFreshFlux_Bamber

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

  
