!-*-F90-*-
program bbl2btm_t
  !
  use oc_mod_param, only : &
  &  imut, jmut, km, ksgm
  !
  use oc_structure, only : &
  &  read_topo,            &
  &  ktbtm,                &
  &  ho4,                  &
  &  atexl
#ifdef OGCM_BBL
  use oc_structure, only : &
  &  atexlbbl
#endif /* OGCM_BBL */

  !
  !----------------------------------------------------------
  implicit none
  !
  real(4) :: UNDEF_IN  = -9.99e33
  real(4) :: UNDEF_OUT = -9.99e33
  !
  integer(4), parameter :: mtin  = 61
  integer(4), parameter :: mtout = 65
  !
  character(len=256), save :: flin
  character(len=256), save :: flout
  character(len=256), save :: fltopo
  !
  namelist /nml_bbl2btm_t/ flin, flout, fltopo, undef_in, undef_out
  !
  real(4) :: tt(imut, jmut, km)
  !
  integer(4) :: i, j, k, n
  !
  !=========================================
  !
  flin  = 'hs_t.yyyymm'
  flout = 'hs_t_new.yyyymm'
  fltopo= 'topo.d'

  read(unit=5, nml_bbl2btm_t)
  print *,'flin   :', trim(flin)
  print *,'flout  :', trim(flout)
  print *,'fltopo :', trim(fltopo)
  print *,'undef_in  :', undef_in
  print *,'undef_out :', undef_out
  !-------------------------------------

  call read_topo(fltopo)

  !------------------------------------------
  open(mtin, file=flin, form='unformatted', access='direct', &
       & recl=4*imut*jmut*km)
  read ( unit=mtin, rec=1 ) tt(1:imut,1:jmut,1:km)
  close ( mtin )
  !------------------------------------------

#ifdef OGCM_BBL
  do j = 1, jmut
    do i = 1, imut
      k = ktbtm(i, j)
      if (k > ksgm .and. atexlbbl(i,j,1) == 1.d0) then
        if (tt(i,j,k) == undef_in .and. tt(i,j,km) /= undef_in) then
          tt(i, j, k) = tt(i, j, km)
          tt(i, j, km) = undef_in
        else if (tt(i,j,k) == tt(i,j,km)) then
          tt(i, j, km) = undef_in
        else
          write(6,*) 'possibly inconsistent, please check :', i,j,k
        end if
      end if
    end do
  end do
#endif /* OGCM_BBL */

  do k = 1, km
    do j = 1, jmut
      do i = 1, imut
        if (tt(i,j,k) == undef_in) then
          tt(i,j,k) = undef_out
        end if
      end do
    end do
  end do

  !------------------------------------------

  open ( unit=mtout, file=flout, form='unformatted', access='direct', &
       & recl=4*imut*jmut*km)
  write ( unit=mtout, rec=1 ) tt(1:imut, 1:jmut, 1:km)
  close ( mtout )

  !------------------------------------------

end program bbl2btm_t
