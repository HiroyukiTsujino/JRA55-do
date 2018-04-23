!-*-F90-*-
!sigma2mld.F90
!====================================================
!
!   Calculate Mixed Layer Depth from GrADS file
!     the depth at which the density difference
!     from the sea surface is 0.125 sigma units
!     (Monterey and Levitus, 1997)
!
!====================================================
program sigma2mld
  !
  use oc_mod_param, only :  &
  & imut, jmut, km, dz
  !
  use oc_structure, only :  &
  & read_topo,              &
  & dp, dep,                &
  & atexl
  !
  implicit none
  !
  !
  real(4), save :: dsigma0 = 0.125
  real(4), save :: undef   = 0.0e0
  !
  character(len=256)    :: flin_sigma0
  character(len=256)    :: flout_mld
  character(len=256)    :: fltopo
  !
  namelist /nml_mld/ flin_sigma0, flout_mld, dsigma0, undef, fltopo
  !
  character(len=256)    :: flin_sigma0_tmp
  character(len=256)    :: flout_mld_tmp
  !
  real(4) :: sigma0(imut, jmut, km)
  real(4) :: dzf(km)
  real(4) :: dpf(km)
  real(4) :: depf(km)
  real(4) :: mldp(imut, jmut)
  real(4) :: dat2out(imut,jmut)
  !
  integer(4), parameter :: mtin_sigma0   = 81
  integer(4), parameter :: mtout_mld     = 82
  !
  integer(4) :: m
  integer(4) :: i, j, k, n
  real(4) :: hl0, hl1, hl2
  !
  !==========================================
  !
  read(unit=5, nml_mld)
  print *,'flin_sigma0  :', flin_sigma0
  print *,'flin_mld     :', flout_mld
  print *,'fltopo       :', fltopo
  print *,'dsigma0      :', dsigma0
  print *,'undef        :', undef
  !
  !------------------------------------------
  !
  call read_topo(fltopo)
  !
  dzf(1:km) = real(dz(1:km), 4)
  depf(1:km) = real(dep(1:km), 4)
  dpf(1:km) = real(dp(1:km), 4)
  !
  !------------------------------------------------------
  open(mtin_sigma0, file=flin_sigma0, form='unformatted', &
    &           access='direct', recl=4*imut*jmut*km)
  read(mtin_sigma0, rec=1) sigma0(1:imut, 1:jmut, 1:km)
  close(mtin_sigma0)
  !
  !------------------------------------------------------
  mldp(1:imut, 1:jmut) = 0.0
  do j = 1, jmut
    do i = 1, imut
      if(atexl(i, j, 1) /= 0.d0) then
        hl0 = sigma0(i, j, 1) + dsigma0

        LOOP_K : do k = km-1, 2, -1
          hl1 = sigma0(i, j, k)   
          hl2 = sigma0(i, j, k-1) 
          if(hl1 <= hl0 .and. atexl(i,j,k) == 1.d0 ) then
            mldp(i, j) = depf(k+1) 
            BBL_check : if ( atexl(i,j,km) == 1.d0 ) then
              hl2 = sigma0(i, j, km) 
              if (hl2 <= hl0) mldp(i,j) = mldp(i,j) + dzf(km)
            end if BBL_check
            exit LOOP_K
          else if (hl2 < hl0 .and. hl0 <= hl1) then
            mldp(i, j) = dpf(k-1)                                 &
              &  +(dpf(k)-dpf(k-1))*(hl0-hl2)/(hl1 -hl2 + 1.e-30)
            exit LOOP_K
          end if
        end do LOOP_K
        mldp(i, j) = mldp(i, j) * 1.e-2  !  cm to m
      else
        mldp(i, j) = undef
      end if
    end do
  end do
  !
  open(mtout_mld, file=flout_mld, form='unformatted', &
    &           access='direct', recl=4*imut*jmut)
  write(mtout_mld, rec=1) mldp(1:imut, 1:jmut)
  close(mtout_mld)
end program sigma2mld
!====================================================
