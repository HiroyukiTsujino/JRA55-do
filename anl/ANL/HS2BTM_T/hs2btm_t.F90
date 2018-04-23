!-*-F90-*-
program hs2btm_t
  !
  use oc_mod_param, only : &
  &  imut, jmut, km
  !
  use oc_structure, only : &
  &  read_topo,            &
  &  ktbtm,                &
  &  ho4,                  &
  &  atexl
  !
  !----------------------------------------------------------
  implicit none
  !
  !
  real(4),    parameter :: UNDEF = -9.99e33
  !
  integer(4), parameter :: mtin  = 61
  integer(4), parameter :: mtout = 65
  integer(4), parameter :: mttmp  = 68
  !
  character(len=256), save :: flin
  character(len=256), save :: flout
  character(len=256), save :: fltopo
  !
  namelist /nml_hs2btm_t/ flin, flout, fltopo
  !
  real(4) :: tt(imut, jmut, km)
  !
  real(4) :: tbtm(imut, jmut)
  real(4) :: dbtm(imut, jmut)
  real(4) :: dbtmt(imut, jmut)
  !
  integer(4) :: i, j, k, n
  !
  !=========================================
  !
  flin  = 'hs_t.yyyymm'
  flout = 'hs_t_btm.yyyymm'
  fltopo= 'topo.d'
  !
  !
  read(unit=5, nml_hs2btm_t)
  print *,'flin   :', trim(flin)
  print *,'flout  :', trim(flout)
  print *,'fltopo :', trim(fltopo)
  !-------------------------------------
  !
  call read_topo(fltopo)
  !
  dbtm(1:imut, 1:jmut) = real(ho4(1:imut, 1:jmut), 4)/100.e0
  !
  !------------------------------------------
  !
  open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*km)
  read ( unit=mtin, rec=1 ) tt(1:imut,1:jmut,1:km)
  close ( mtin )
  !------------------------------------------
  !
  dbtmt(1:imut, 1) = 0.e0
  do j = 2, jmut
    do i = 2, imut
      dbtmt(i, j) = max(dbtm(i-1, j  ), dbtm(i, j  ), &
        &               dbtm(i-1, j-1), dbtm(i, j-1) )
    end do
  end do
  dbtmt(1, 2:jmut) = dbtmt(imut-3, 2:jmut)
  !
  do j = 1, jmut
    do i = 1, imut
      k = ktbtm(i, j)
#ifdef OGCM_BBL
      tbtm(i, j) = atexl(i, j, k) * tt(i, j, k)  &
        &  +(1.d0 -atexl(i, j, k))* tt(i, j, km)
#else /* OGCM_BBL */
      tbtm(i, j) = tt(i, j, k)
#endif /* OGCM_BBL */
    end do
  end do
  !
  where(atexl(1:imut, 1:jmut, 1) == 0.d0)
    tbtm(1:imut, 1:jmut)  = UNDEF
    dbtmt(1:imut, 1:jmut) = UNDEF
  end where
  !------------------------------------------
  open ( unit=mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut)
  write ( unit=mtout, rec=1 ) tbtm(1:imut, 1:jmut)
  write ( unit=mtout, rec=2 ) dbtmt(1:imut, 1:jmut)
  close ( mtout )
  !------------------------------------------
  !
end program hs2btm_t
