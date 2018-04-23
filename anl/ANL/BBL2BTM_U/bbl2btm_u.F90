!-*-F90-*-
program bbl2btm_u
  !
  use oc_mod_param, only : &
  &  imut, jmut, km, ksgm
  !
  use oc_structure, only : &
  &  read_topo,            &
  &  kbtm,                 &
  &  ho4,                  &
  &  aexl
#ifdef OGCM_BBL
  use oc_structure, only : &
  &  aexlbbl
#endif /* OGCM_BBL */
  !
  !----------------------------------------------------------
  implicit none
  !
  !
  real(4) :: UNDEF_IN  = -9.99e33
  real(4) :: UNDEF_OUT = -9.99e33
  !
  integer(4), parameter :: mtinu = 61
  integer(4), parameter :: mtout = 65
  !
  character(len=256) :: flinu
  character(len=256) :: flout
  character(len=256) :: fltopo
  !
  namelist /nml_bbl2btm_u/ flinu, flout, fltopo, undef_in, undef_out

  real(4) :: u(imut, jmut, km)

  integer(4) :: i, j, k, n

  !=========================================
  !
  flinu = 'hs_u.yyyymm'
  flout = 'hs_u_new.yyyymm'
  fltopo= 'topo.d'
  !
  read(unit=5, nml_bbl2btm_u)
  print *,'flinu  :', trim(flinu)
  print *,'flout  :', trim(flout)
  print *,'fltopo :', trim(fltopo)
  print *,'undef_in  :', undef_in
  print *,'undef_out :', undef_out
  !-------------------------------------
  !
  call read_topo(fltopo)
  !
  !------------------------------------------
  !
  open(mtinu, file=flinu, form='unformatted', access='direct', &
       & recl=4*imut*jmut*km)
  read ( unit=mtinu, rec=1 ) u(1:imut,1:jmut,1:km)
  close ( mtinu )

  !------------------------------------------
  !
#ifdef OGCM_BBL
  do j = 1, jmut
    do i = 1, imut
      k = kbtm(i, j)
      if (k > ksgm .and. aexlbbl(i,j,1) == 1.d0) then
        if (u(i,j,k) == undef_in .and. u(i,j,km) /= undef_in) then
          u(i, j, k) = u(i, j, km)
          u(i, j, km) = undef_in
        else if (u(i,j,k) == u(i,j,km)) then
          u(i, j, km) = undef_in
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
        if (u(i,j,k) == undef_in) then
          u(i,j,k) = undef_out
        end if
      end do
    end do
  end do

  !------------------------------------------
  open ( unit=mtout, file=flout, form='unformatted', access='direct', &
       & recl=4*imut*jmut*km)
  write ( unit=mtout, rec=1 ) u(1:imut, 1:jmut, 1:km)
  close ( mtout )
  !------------------------------------------

end program bbl2btm_u
