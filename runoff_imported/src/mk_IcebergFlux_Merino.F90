program mk_Iceberg_Flux_Merino
  implicit none

  integer(4), parameter :: nxo = 360 * 4, nyo = 180 * 4
  real(4) :: dxo = 0.25e0, dyo = 0.25e0
  real(4) :: lato(nyo+1), lono(nxo)
  real(8) :: areao(nyo)
  real(4) :: icbo(nxo,nyo,12)
  real(4) :: icbo_daily(nxo,nyo)
  integer(4) :: jstr, jend

  integer(4), parameter :: nxi = 1442, nyi = 400

  real(4) :: icbi(nxi,nyi,12)
  real(4) :: loni(nxi), lati(nyi)
  real(8) :: areai(nyi) 
  integer(4) :: nt
  character(len=128) :: fileo       = 'data_Iceberg/Merino_icb_025x025.XX'
  character(len=128) :: fileo_daily = 'data_Iceberg/Merino_icb_025x025_DDDdy'

  real(8),parameter :: total_Calving_Flux = 1321.d0 ! 10^12 kg/yr = Gt/yr 
!           Depoorter et al(2013,Nature)

  integer(4) :: river_index(nxo,nyo)
  integer(4), parameter :: rivmouth_Iceberg = -666
  character(len=128) :: file_riverindex = 'data_Iceberg/iceberg_index'

  real(8) :: month_days(0:13) 
  real(8) :: days_sum(12) 
  logical :: l_leap
  integer(4) :: i, j

  call read_data
  call set_grid

  do nt = 1, 12
    call linear_interpo(icbi(:,:,nt), icbo(:,:,nt))
  end do
  call adjust_flux

  river_index(:,:) = 0

  do nt = 1, 12
    write(fileo(33:34),'(I2.2)') nt
    open (31,file=fileo,form='unformatted',access='direct',&
       & recl = 4 * nxo * nyo)
    write(31,rec=1) icbo(:,:,nt)
    do j = 1, nyo
      do i = 1, nxo
        if ( icbo(i,j,nt) > 0 ) river_index(i,j) = rivmouth_Iceberg
      end do
    end do
    close(31)
  end do
  open (31,file=file_riverindex,form='unformatted',access='direct',&
       & recl = 4 * nxo * nyo)
  write(31,rec=1) river_index
  close(31)

  l_leap = .false.
  call set_calendar ( l_leap )
  call mk_daily_main

  l_leap = .true.
  call set_calendar ( l_leap )
  call mk_daily_main


contains
!===============================================================
subroutine read_data
  integer(4) :: nt
  real(4) :: icbi_tmp(nxi,nyi,12)
  real(4) :: loni_tmp(nxi,nyi), lati_tmp(nxi,nyi)
  integer(4) :: ii, jj, ii0, iii
  open (21,file="data_Iceberg_org/mmc2.dat",form='unformatted',access='direct',&
       & recl = 4 * nxi * nyi )
  do nt = 1, 12
    read (21, rec=nt) icbi_tmp(:,:,nt)
  end do
  read(21,rec=13) loni_tmp
  read(21,rec=14) lati_tmp
  close(21)

  do ii = 1, nxi
    if ( abs(loni_tmp(ii,1)) <= 0.01e0) then
      ii0 = ii
      exit
    end if
  end do
!  write(*,*) loni_tmp(ii0,1), ii0

  iii = ii0
  do ii = 1, nxi
    loni(ii) = loni_tmp(iii,1)
    icbi(ii,1:nyi,1:12) = icbi_tmp(iii,1:nyi,1:12)
    iii = iii + 1
    if (iii > nxi) iii = 1
    if (loni(ii)      <= 0.0e0 ) loni(ii) = loni(ii) + 360.e0
    if (abs(loni(ii)) <= 0.01e0) loni(ii) = 0.e0
  end do

  do jj = 1, nyi
    lati(jj) = lati_tmp(1,jj)
  end do
  lati(1) = -77.0105
      
end subroutine read_data
!===============================================================
subroutine set_grid
  integer(4) :: i, j, ii, jj
  real(8) :: ttng, tanfi,  cst
  real(8) :: lat, lon, dyi, dxi
  real(8),parameter :: pi     = 3.1415926535897932384626433832795d0
  real(8),parameter :: radius = 6375.d3, deg1 = radius * pi / 180.d0

  real(4) :: anhfto(nyo+1), ashfto(nyo+1)
  real(4) :: anhfti(nyi+1), ashfti(nyi+1)
  
  do i = 1, nxo
    lono(i) = 0.125 + dxo*dble(i-1)
  end do
  do j = 1, nyo + 1
    lato(j)  = -90.d0 + (j-1) * dyo
    ttng  = dtan(pi / 180.d0 * lato(j) )
    tanfi = dtan(0.25d0 * dyo / 180.d0 * pi) 
    cst   = cos(pi / 180.d0 * lato(j) )
    ashfto(j) = 0.25d0 * (1.d0+ttng*tanfi)*dxo*dyo*cst * deg1**2
    anhfto(j) = 0.25d0 * (1.d0-ttng*tanfi)*dxo*dyo*cst * deg1**2
  end do
  do j = 1, nyo
    areao(j) = 2.d0 * (anhfto(j) + ashfto(j+1))
  end do

  dxi = 0.25d0
  do j = 2, nyi -1
    ttng  = dtan(pi / 180.d0 * lati(j) )
    cst   =  cos(pi / 180.d0 * lati(j) )
    dyi   = lati(j) - lati(j-1)
    tanfi = dtan(0.25d0 * dyi / 180.d0 * pi) 
    ashfti(j) = 0.25d0 * (1.d0+ttng*tanfi)*dxi*dyi*cst * deg1**2

    dyi   = lati(j+1) - lati(j)
    tanfi = dtan(0.25d0 * dyi / 180.d0 * pi) 
    anhfti(j) = 0.25d0 * (1.d0-ttng*tanfi)*dxi*dyi*cst * deg1**2
  end do
  do j = 2, nyi - 2
    areai(j) = 2.d0 * (anhfti(j) + ashfti(j+1))
  end do
  
  do j = 1, nyo
    if ( lato(j) >= lati(1) ) then
      jstr = j
      exit
    end if
  end do
  do j = jstr, nyo
    if ( lato(j) > lati(nyi) ) then
      jend = j - 1
      exit
    end if
  end do

end subroutine set_grid  
!===============================================================
subroutine linear_interpo (datin, datout)
  implicit none
  real(4),intent( in) :: datin (nxi, nyi)
  real(4),intent(out) :: datout(nxo, nyo)
  integer(4) :: i, j, ii, jj
  integer(4) ::   iss(nxo), jss(nyo), issp(nxo), jssp(nyo)
  real(8)    ::   ssi(nxo), ssj(nyo), ssic(nxo), ssjc(nyo)

  jss(:) = 0
  iss(:) = 0

  do jj = 1, nyi
    where (lato(1:nyo) > lati(jj)) jss(1:nyo) = jj
  end do

  do ii = 1, nxi
    where (lono(1:nxo) > loni(ii)) iss(1:nxo) = ii
  end do

  jssp(1:nyo) = jss (1:nyo) + 1
  jss (1:nyo) = max(jss (1:nyo),1  )
  jssp(1:nyo) = min(jssp(1:nyo),nyi)

  issp(1:nxo) = iss(1:nxo) + 1
  iss (1:nxo) = mod(iss (1:nxo)+nxi-1,nxi)+1
  issp(1:nxo) = mod(issp(1:nxo)+nxi-1,nxi)+1
  
  do j = jstr, jend
    do i = 1, nxo
      if ( jss(j) == jssp(j) ) then
        ssj(j) = 1.0d0
      else
        ssj(j) = (lato(j)     - lati(jss(j))) &
           &   / (lati(jssp(j))-lati(jss(j)))
      end if
      
      if ( iss(i) == issp(i) ) then
        ssi(i) = 1.0d0
      else if ( lono(i) < loni(1) ) then
        ssi(i) = (lono(i)      +360.0d0-loni(iss(i))) &
             & / (loni(issp(i))+360.0d0-loni(iss(i)))
      else if ( lono(i) > loni(nxi) ) then
        ssi(i) = (lono(i)              -loni(iss(i))) &
             & / (loni(issp(i))+360.0d0-loni(iss(i)))
      else
        ssi(i) = (lono(i)     - loni(iss(i))) &
             & / (loni(issp(i))-loni(iss(i)))
      end if
    end do
  end do
  
  ssjc(1:nyo) = 1.0d0 - ssj(1:nyo)
  ssic(1:nxo) = 1.0d0 - ssi(1:nxo)
  
  datout(:,:) = 0.e0
  do j = jstr, jend
    do i = 1, nxo
      datout(i,j) = ssic(i) * ssjc(j) * datin(iss (i),jss (j)) &
           &      + ssi (i) * ssjc(j) * datin(issp(i),jss (j)) &
           &      + ssic(i) * ssj (j) * datin(iss (i),jssp(j)) &
           &      + ssi (i) * ssj (j) * datin(issp(i),jssp(j))
    end do
  end do

end subroutine linear_interpo
!===============================================================
!===============================================================
subroutine adjust_flux
  integer(4) :: i, j
  real(8) :: total, adjust_fact
  real(8) :: days(12) = (/31.d0, 28.24d0, 31.d0, 30.d0, 31.d0, 30.d0, &
                      &   31.d0, 31.d0, 30.d0, 31.d0, 30.d0, 31.d0/)

  total = 0.d0
  do nt = 1, 12
    do j = 1, nyi
      do i = 1, nxi
        total = total + icbi(i,j,nt) * areai(j) * days(nt) * 86400.d0
      end do
    end do
  end do
  write(*,*) total * 1.e-12, 'Gt/yr (original data)'

  total = 0.d0
  do nt = 1, 12
    do j = 1, nyo
      do i = 1, nxo
        total = total + icbo(i,j,nt) * areao(j) * days(nt) * 86400.d0
      end do
    end do
  end do
  write(*,*) total * 1.e-12, 'Gt/yr (interpolated data)'

  adjust_fact = total_calving_flux / (total * 1.d-12)
  write(*,*) 'adjust_factor=', adjust_fact

! kg/m^2/s --> m^3/s * adjust_fact  

! weight of m^3 of water is 1t.

  do nt = 1, 12
    do j = 1, nyo
      do i = 1, nxo
        icbo(i,j,nt) = icbo(i,j,nt) * areao(j) * 1.d-3 * adjust_fact
      end do
    end do
  end do

  total = 0.d0
  do nt = 1, 12
    do j = 1, nyo
      do i = 1, nxo
        total = total + icbo(i,j,nt) * days(nt) * 86400.d0
      end do
    end do
  end do
  write(*,*) total * 1.d-9,      'Gt/yr (adjusted data )' ! 1.d-9 for Giga
  write(*,*) total_calving_flux, 'Gt/yr (Depoorter data)'
    
end subroutine adjust_flux  
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
subroutine mk_daily_main 
  real(8) :: now
  real(8) :: tmp1(366), tmp2(366), tmp1_mon(12), tmp2_mon(12)
  real(8) :: total, adjust_fact
  integer(4) :: ndylast, index_dy, index_yr
  integer(4) :: i, j, mn, ndy, ndy_tmp 
  character(len=128) :: cfile

  integer(4) :: i1 = 488, j1 = 96

  ndy_tmp = int(days_sum(12))

  total = 0.d0
  do ndy = 1, ndy_tmp
    now = dble(ndy) - 0.5d0
    call mk_daily_now(icbo_daily, now, icbo)
    do j = 1, nyo
      do i = 1, nxo
        total = total + icbo_daily(i,j)
      end do
    end do
  end do
  close(11)
  total = total * 86400.d0
  write(*,*) total * 1.d-9, 'Gt/yr (Daily)'

  adjust_fact = total_calving_flux / (total * 1.d-9) 
  write(*,*) 'adjut_fact (due to the coarse linear interpolation)= ', adjust_fact


  cfile = fileo_daily
  index_dy = index(cfile,"DDD")
  write(cfile(index_dy:index_dy+2),'(I3.3)') ndy_tmp
  write(*,*) trim(cfile)
  open(11,file=cfile, form='unformatted',access='direct',recl=4*nxo*nyo)
  total = 0.d0
  do ndy = 1, ndy_tmp
    now = dble(ndy) - 0.5d0
    call mk_daily_now(icbo_daily, now, icbo)
    icbo_daily = icbo_daily * adjust_fact
    write(11,rec=ndy) icbo_daily
    do j = 1, nyo
      do i = 1, nxo
        total = total + icbo_daily(i,j)
      end do
    end do
  end do
  close(11)
  total = total * 86400.d0
  write(*,*) total * 1.d-9, 'Gt/yr (Daily, adjusted)'



end subroutine mk_daily_main
!--------------------------------------------------------
subroutine mk_daily_now (r2d_now, now, r2d_monthly)
  real(8),intent(in)    :: now
  real(4),intent(out)   :: r2d_now(nxo,nyo)
  real(4),intent(in)    :: r2d_monthly(nxo,nyo,12)

  integer(4) :: mn, n1, n2
  real(8) :: f1, f2

  integer(4) :: i1 = 488, j1 = 96

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
end program mk_Iceberg_Flux_Merino
