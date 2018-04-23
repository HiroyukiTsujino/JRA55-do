!-*-F90-*-
!ts2density.F90
!====================================================
!
!   Make GrADS data from OGCM restart file
!
!====================================================
program ts2density
  !
  use oc_mod_param, only :   &
  &  imut, jmut, km, grav, ro
  !
  use oc_mod_density, only : &
  &  dens
  !
  use oc_structure, only : &
  &  read_topo,            &
  &  atexl,                &
  &  dp

#ifdef OGCM_BBL
  use oc_structure, only : &
  &  ktbtm,                &
  &  texnnbbl
#endif /* OGCM_BBL */

  !
  implicit none
  !
  !character(len=256), parameter :: flmaskt = '../../data/maskt.gd'
  !
  real(4), parameter :: UNDEF = -9.99e33
  !
  real(4) :: dat4(imut, jmut, km)
  !
  real(8) :: ttd(imut, jmut, km)
  real(8) :: ssd(imut, jmut, km)
  !
  logical :: l_insitu = .false.
  real(8), save :: plev = 0.d0  !  [bar]

  real(8) :: p_ref_c(km)        !  [bar]
  real(8) :: sigmad(imut, jmut, km)
  !
  character(len=256)    :: fpath
  character(len=256)    :: flin_t
  character(len=256)    :: flin_s
  character(len=256)    :: flout
  character(len=256)    :: fltopo
  !
  integer(4), parameter :: mtin    = 82
  integer(4), parameter :: mtin_t  = 83
  integer(4), parameter :: mtin_s  = 84
  integer(4), parameter :: mtout   = 85
  !
  integer(4) :: irec
  integer(4) :: i, j, k, n, ktmp
  !
  namelist /nml_density/ l_insitu, plev, flin_t, flin_s, flout, fltopo
  !
  !==========================================

  write(6,*) 'This is ts2density.'
  !
  flin_t = 'hs_t.grd'
  flin_s = 'hs_s.grd'
  flout  = 'hs_sigma.grd'
  fltopo = 'topo.d'
  !
  write(6,*) 'reading namelist ......'
  read(unit=5, nml=nml_density)
  if (l_insitu) then
    write(6,*) ' Calculate in-situ density'
  else
    print *,'plev     :', plev, ' [bar]'
  end if
  print *,'flin_t   :', trim(flin_t)
  print *,'flin_s   :', trim(flin_s)
  print *,'flout    :', trim(flout)
  print *,'fltopo   :', trim(fltopo)
  !
  call read_topo(fltopo)
  !
  !------------------------------------------
  !
  if (l_insitu) then
    p_ref_c(1:km) = (grav * 1.0d-2) * (dp(1:km) * 1.0d-2) * (ro * 1.0d3) * 1.0d-5
  else
    p_ref_c(1:km) = plev
  end if
  !
  !--------------

  open(mtin_t, file=flin_t, form='unformatted', access='direct', recl=imut*jmut*km*4)
  open(mtin_s, file=flin_s, form='unformatted', access='direct', recl=imut*jmut*km*4)
  open(mtout,  file=flout,  form='unformatted', access='direct', recl=imut*jmut*km*4)

  !------------------------------------------------------
  !
  read (mtin_t, rec=1) dat4(1:imut, 1:jmut, 1:km)  !  Temperature
  do k = 1, km
    where(atexl(1:imut, 1:jmut, k) == 0.d0)
      dat4(1:imut, 1:jmut, k) = 0.e0
    end where
  end do
  ttd(1:imut, 1:jmut, 1:km) = real(dat4(1:imut, 1:jmut, 1:km), 8)

  read (mtin_s, rec=1) dat4(1:imut, 1:jmut, 1:km)  !  Salinity
  do k = 1, km
    where(atexl(1:imut, 1:jmut, k) == 0.d0)
      dat4(1:imut, 1:jmut, k) = 0.e0
    end where
  end do
  ssd(1:imut, 1:jmut, 1:km) = real(dat4(1:imut, 1:jmut, 1:km), 8)
  !
#ifdef OGCM_BBL
  if (l_insitu) then
    ! manipulate data for BBL
    do j = 1, jmut
      do i = 1, imut
        ktmp = ktbtm(i,j)
        if ((texnnbbl(i,j) > 0) .and. (atexl(i,j,ktmp) == 0.0d0)) then
          ttd(i,j,ktmp) = ttd(i,j,km)
          ssd(i,j,ktmp) = ssd(i,j,km)
        end if
      end do
    end do
  end if
#endif /* OGCM_BBL */
  !
  !          Potential Density
  !
  ! p_ref_c: [bar]
  !
  call dens(imut, jmut, km, ttd, ssd, p_ref_c, sigmad)
  !
#ifdef OGCM_BBL
  if (l_insitu) then
    ! restore data for BBL
    do j = 1, jmut
      do i = 1, imut
        ktmp = ktbtm(i,j)
        if ((texnnbbl(i,j) > 0) .and. (atexl(i,j,ktmp) == 0.0d0)) then
          sigmad(i,j,km) = sigmad(i,j,ktmp)
          sigmad(i,j,ktmp) = 0.0d0
        end if
      end do
    end do
  end if
#endif /* OGCM_BBL */

  dat4(1:imut, 1:jmut, 1:km) = real(sigmad(1:imut, 1:jmut, 1:km), 4) * 1.d3

  do k = 1, km
    where(atexl(1:imut, 1:jmut, 1:km) == 0.d0)
      dat4(1:imut, 1:jmut, 1:km) = UNDEF
    end where
  end do
  !
  write(mtout, rec=1) dat4(1:imut, 1:jmut, 1:km)  !  Potential Density
  !
  close(mtout)
  close(mtin_t)
  close(mtin_s)
  !
end program ts2density
!====================================================
