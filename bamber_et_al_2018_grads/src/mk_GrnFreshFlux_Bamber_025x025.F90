!-*-F90-*-
program mk_GrnFreshFlux_Bamber_025
  implicit none

  character(len=128) :: cfile_in    = 'data_Grn_Bamber_org/grads_monthly/Bamber_2018.YYYYMM'
  character(len=128) :: cfile_out   = 'data_Grn_Bamber_new/grads_monthly/Bamber_2018_025x025.YYYYMM'
  character(len=128) :: cfile_total = 'data_Grn_Bamber_new/grads_monthly/total_fwf_Bamber_2018.YYYYMM'

  integer(4), parameter :: nx = 360 * 4, ny = 180 * 4
  real(4) :: dx = 0.25e0, dy = 0.25e0

  integer(4) :: nyr, nmn
  real(8) :: month_days(12) 
  real(8) :: days_sum(12) 

  logical :: l_leap

  !--------------------------------------------------

  do nyr = 1958, 2016
    if ( mod(nyr,4) == 0 ) then
      l_leap = .true. 
    else
      l_leap = .false.
    end if

    call set_calendar(l_leap)
    write(6,*) nyr, ' Calling Main program '
    do nmn = 1, 12
      call main(nyr, nmn)
    end do
  end do

contains
!---------------------------------------------------------------    
subroutine main (nyr, nmn)
  integer(4),intent(in) :: nyr, nmn
  integer(4),parameter :: nxi = 752, nyi = 785
  real(4) :: tuni (nxi,nyi)
  real(4) :: mlti (nxi,nyi)
  real(4) :: icei (nxi,nyi)
  real(4) :: loni (nxi,nyi)
  real(4) :: lati (nxi,nyi)
  real(4) :: grnm (nxi,nyi)
  character(len=128) :: cfile
  integer(4) :: ii, jj, i, j

  real(8) :: tun(nx,ny)
  real(8) :: mlt(nx,ny)
  real(8) :: ice(nx,ny)

  real(8) :: total_tun, total_mlt, total_ice

  integer(4) :: index_yr,index_mn
  integer(4) :: n
  
  cfile = cfile_in
  index_yr = index(cfile,"YYYY")
  index_mn = index(cfile,"MM")
  write(cfile(index_yr:index_yr+3),'(I4.4)') nyr
  write(cfile(index_mn:index_mn+1),'(I2.2)') nmn
!  write(*,*) trim(cfile)

  open(11,file=cfile,form='unformatted',access='direct',recl = 4 * nxi * nyi)
  read(11,rec=1) tuni
  read(11,rec=2) mlti
  read(11,rec=3) icei
  read(11,rec=4) loni
  read(11,rec=5) lati
  read(11,rec=6) grnm
  close(11)

  ! original  (km)^3 / month
  ! final       m ^3 / s
  tuni(:,:) = max(tuni(:,:),0.0) * grnm(:,:) * 1.d9     ! (km)^3  -> m^3
  tuni(:,:) = tuni(:,:) / (month_days(nmn) * 86400.d0 ) ! 1/month -> 1/s
  mlti(:,:) = max(mlti(:,:),0.0) * grnm(:,:) * 1.d9     ! (km)^3  -> m^3
  mlti(:,:) = mlti(:,:) / (month_days(nmn) * 86400.d0 ) ! 1/month -> 1/s
  icei(:,:) = max(icei(:,:),0.0) * grnm(:,:) * 1.d9     ! (km)^3  -> m^3
  icei(:,:) = icei(:,:) / (month_days(nmn) * 86400.d0 ) ! 1/month -> 1/s
 
  do jj = 1, nyi
    do ii = 1, nxi
      if (loni(ii,jj) < 0.0d0) then
        loni(ii,jj) = loni(ii,jj) + 360.e0
      end if
    end do
  end do

  ! binning

  total_tun = 0.d0
  total_mlt = 0.d0
  total_ice = 0.d0
  tun(:,:) = 0.d0
  mlt(:,:) = 0.d0
  ice(:,:) = 0.d0

  do jj = 1, nyi
    do ii = 1, nxi
      i = loni(ii,jj) / dx + 1
      j = (lati(ii,jj) + 90.e0) / dy + 1
      tun(i,j) = tun(i,j) + real(tuni(ii,jj),8)
      mlt(i,j) = mlt(i,j) + real(mlti(ii,jj),8)
      ice(i,j) = ice(i,j) + real(icei(ii,jj),8)
      total_tun = total_tun + real(tuni(ii,jj),8)
      total_mlt = total_mlt + real(mlti(ii,jj),8)
      total_ice = total_ice + real(icei(ii,jj),8)
    end do
  end do

  total_tun = total_tun * 1.d-6                   ! m^3/s   -> Sv
  total_mlt = total_mlt * 1.d-6                   ! m^3/s   -> Sv
  total_ice = total_ice * 1.d-6                   ! m^3/s   -> Sv
!  write(*,'(A,F15.7,A)') 'total=', total*1000.d0,  ' mSv'

  cfile = cfile_out
  index_yr = index(cfile,"YYYY")
  index_mn = index(cfile,"MM")
  write(cfile(index_yr:index_yr+3),'(I4.4)') nyr
  write(cfile(index_mn:index_mn+1),'(I2.2)') nmn
!  write(*,*) trim(cfile)
  open (11,file=cfile,form='unformatted',access='direct',recl = 4 * nx * ny)
  write(11,rec=1) real(tun(:,:),4)
  write(11,rec=2) real(mlt(:,:),4)
  write(11,rec=3) real(ice(:,:),4)
  close(11)

  cfile = cfile_total
  index_yr = index(cfile,"YYYY")
  index_mn = index(cfile,"MM")
  write(cfile(index_yr:index_yr+3),'(I4.4)') nyr
  write(cfile(index_mn:index_mn+1),'(I2.2)') nmn
!  write(*,*) trim(cfile)
  open (11,file=cfile,form='unformatted',access='direct',recl = 4)
  write(11,rec=1) real(total_tun,4)
  write(11,rec=2) real(total_mlt,4)
  write(11,rec=3) real(total_ice,4)
  close(11)

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

  days_sum(1) = month_days(1)
  do mn = 2, 12
    days_sum(mn) = days_sum(mn-1) + month_days(mn)
  end do    
  
end subroutine set_calendar

end program mk_GrnFreshFlux_Bamber_025
