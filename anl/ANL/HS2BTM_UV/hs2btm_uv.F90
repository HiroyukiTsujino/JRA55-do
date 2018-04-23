program hs2btm_uv
  !
  use oc_mod_param, only : &
  &  imut, jmut, km
  !
  use oc_structure, only : &
  &  read_topo,            &
  &  kbtm,                 &
  &  ho4,                  &
  &  aexl
  !
  !----------------------------------------------------------
  implicit none
  !
  !
  real(4),    parameter :: UNDEF = -9.99e33
  !
  integer(4), parameter :: mtinu = 61
  integer(4), parameter :: mtinv = 63
  integer(4), parameter :: mtout = 65
  !
  character(len=256) :: flinu, flinv
  character(len=256) :: flout
  character(len=256) :: fltopo
  !
  namelist /nml_hs2btm_uv/ flinu, flinv, flout, fltopo
  !
  real(4) :: u(imut, jmut, km)
  real(4) :: v(imut, jmut, km)
  !
  real(4) :: ubtm(imut, jmut)
  real(4) :: vbtm(imut, jmut)
  real(4) :: sbtm(imut, jmut)
  real(4) :: dbtm(imut, jmut)
  !
  integer(4) :: i, j, k, n
  !
  !=========================================
  !
  flinu = 'hs_u.yyyymm'
  flinv = 'hs_v.yyyymm'
  flout = 'hs_t_btm.yyyymm'
  fltopo= 'topo.d'
  !
  read(unit=5, nml_hs2btm_uv)
  print *,'flinu  :', trim(flinu)
  print *,'flinv  :', trim(flinv)
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
  open(mtinu, file=flinu, form='unformatted', access='direct', recl=4*imut*jmut*km)
  read ( unit=mtinu, rec=1 ) u(1:imut,1:jmut,1:km)
  close ( mtinu )
  open(mtinv, file=flinv, form='unformatted', access='direct', recl=4*imut*jmut*km)
  read ( unit=mtinv, rec=1 ) v(1:imut,1:jmut,1:km)
  close ( mtinv )
  !------------------------------------------
  !
  do j = 1, jmut
    do i = 1, imut
      k = kbtm(i, j)
#ifdef OGCM_BBL
      ubtm(i, j) = real(aexl(i, j, k) , 4) * u(i, j, k)  &
        &  +real((1.d0 -aexl(i, j, k)), 4) * u(i, j, km)
      vbtm(i, j) = real(aexl(i, j, k) , 4) * v(i, j, k)  &
        &  +real((1.d0 -aexl(i, j, k)), 4) * v(i, j, km)
#else /* OGCM_BBL */
      ubtm(i, j) = u(i, j, k)
      vbtm(i, j) = v(i, j, k)
#endif /* OGCM_BBL */
    end do
  end do
  !
  do j = 1, jmut
    do i = 1, imut
      sbtm(i, j) = sqrt(ubtm(i, j)**2 + vbtm(i, j)**2)
    end do
  end do
  !
  where(aexl(1:imut, 1:jmut, 1) == 0.d0)
    ubtm(1:imut, 1:jmut) = UNDEF
    vbtm(1:imut, 1:jmut) = UNDEF
    sbtm(1:imut, 1:jmut) = UNDEF
    dbtm(1:imut, 1:jmut) = UNDEF
  end where
  !------------------------------------------
  open ( unit=mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut)
  write ( unit=mtout, rec=1 ) ubtm(1:imut, 1:jmut)
  write ( unit=mtout, rec=2 ) vbtm(1:imut, 1:jmut)
  write ( unit=mtout, rec=3 ) sbtm(1:imut, 1:jmut)
  write ( unit=mtout, rec=4 ) dbtm(1:imut, 1:jmut)
  close ( mtout )
  !------------------------------------------
  !
end program hs2btm_uv
