! -*-F90-*-
!
!======================================================================
program surface_water_flux_diag

  use basin_param
  use grid_common
  use oc_mod_trnsfrm

  implicit none

  integer(4), parameter :: nmon = 12

  integer(4), parameter :: ibu = 3, ieu = imut - 2  ! cyclic
  integer(4), parameter :: jbu = 2, jeu = jmut - 3  ! tripolar

  integer(4), parameter :: ibt = 3, iet = imut - 2  ! cyclic
  integer(4), parameter :: jbt = 2, jet = jmut - 2  ! tripolar

  integer(4) :: iettmp

  real(4) :: work4(imut,jmut)
  real(4) :: ibas4(imut,jmut)

  real(8) :: wtot(imut,jmut)
  real(8) :: ind_pac_mask(imut,jmut)
  real(8) :: atl_arc_mask(imut,jmut)

  real(8) :: wtotal
  real(8) :: areag

  !-----

  integer(4) :: jmug = 361
  real(8) :: delt_latg, strt_latg

  integer(4) :: jj, jg
  real(8) :: alatg_s, alatg_n
  real(8) :: lambda, phi
  real(8) :: mu, psi

  real(8) :: wtotg(jmug), wtotp(jmug), wtota(jmug)
  real(8) :: mwtrg(jmug), mwtrp(jmug), mwtra(jmug)

  real(8) :: areaz(jmug)

  real(8) :: area_earth, area_this

  !-----

  integer(4), parameter :: mtin1 = 71, mtin2 = 72, mtin3 = 73
  integer(4), parameter :: mtot1 = 81, mtot2 = 82, mtot3 = 83
  character(256) :: flnin_org1, flnin_org2, flnin_org3
  character(256) :: flnot_base1, flnot_base2, flnot_base3
  character(256) :: flnin1, flnin2, flnin3
  character(256) :: flnot1, flnot2, flnot3
  character(256) :: flnav
  character(256) :: ftopo, fgrid, fscale, fbasmap, fdz

  integer(4) :: nday, month, nyear, nd
  integer(4), parameter :: ndata = 12
  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  integer(4) :: irec1, irec2, irec3
  integer(4) :: i, j, k, m
  integer(4) :: ibyr, ieyr

  !-----------------------------------------------------------------------

  namelist /nml_water_diag/ ftopo, fgrid, fscale, fdz, fbasmap, &
       & flnin_org1, flnot_base2, flnot_base3, &
       & ibyr, ieyr, &
       & jmug, delt_latg, strt_latg

  open (11,file='namelist_water_diag')
  read (11,nml=nml_water_diag)
  close(11)

  !-----------------------------------------------------------------------
  ! set grid points

  call setgrd(ftopo, fgrid, fscale, fdz)

  call set_abc(nplat, nplon, splat, splon)

  !----

  open (mtin1, file=fbasmap, access='direct', recl=4*imut*jmut)
  read (mtin1,rec=1) ((ibas4(i,j),i=1,imut),j=1,jmut)
  close(mtin1)

  atl_arc_mask(1:imut,1:jmut) = 0.0d0
  ind_pac_mask(1:imut,1:jmut) = 0.0d0

  do j = jbu, jeu
    do i = ibu, ieu
      if (ibas4(i,j) == 2.0 .or. ibas4(i,j) == 3.0) then
        if ((alatu(j) < 68.0) .and. (alatu(j) > -34.0)) then
          ind_pac_mask(i,j) = 1.0d0
        end if
      end if
      if (ibas4(i,j) == 1.0) then
        atl_arc_mask(i,j) = 1.0d0
      end if
    end do
  end do

  !-----

  areag   = 0.0d0
  area_this = 0.0d0

  do j = jbu, jeu
    do i = ibu, ieu
      areag = areag + aexl(i,j,1) * areauu(i,j)
      area_this = area_this + areauu(i,j)
    end do
  end do

  area_earth = 4.0d0 * pi * radius * radius

  write(6,'(1a,E20.10)') ' Global Ocean area  = ', areag

  write(6,'(1a,E20.10)') ' area earth = ', area_earth
  write(6,'(1a,E20.10)') ' area this  = ', area_this

  ! read temperature and salinity

  do nyear = ibyr, ieyr

    write(6,*) nyear

    wtot(1:imut,1:jmut) = 0.0d0

    write(flnin1,'(1a,i4.4)') trim(flnin_org1),nyear
    open(mtin1, file=flnin1, access='direct', recl=4*imut*jmut)
    write(*,*) ' GrADs data read from... ', trim(flnin1)

    read(mtin1,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    wtot(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

    close(mtin1)

    !-------

    write(flnot2,'(1a,i4.4)') trim(flnot_base2),nyear
    write(*,*) ' GrADs data written to... ', trim(flnot2)
    open(mtot2,file=flnot2,form='unformatted',access='direct',recl=4*jmug)
    irec2 = 0

    write(flnot3,'(1a,i4.4)') trim(flnot_base3),nyear
    write(*,*) ' GrADs data written to... ', trim(flnot3)
    open(mtot3,file=flnot3,form='unformatted',access='direct',recl=4)
    irec3 = 0

    !-------

    areaz(1:jmug) = 0.0d0
    wtotg(1:jmug) = 0.0d0
    wtotp(1:jmug) = 0.0d0
    wtota(1:jmug) = 0.0d0

    areag = 0.0d0
    wtotal = 0.0d0

    do j = jbt, jet

      if (j == jet) then
        iettmp = imut / 2
      else
        iettmp = iet
      end if

      do i = ibt, iettmp

        mu  = alont(i) * radian_r
        psi = alatt(j) * radian_r
        call mp2lp(lambda, phi, mu, psi)
        lambda = lambda * radian
        phi = phi * radian

        do jj = 1, jmug
          alatg_s = strt_latg + delt_latg * real(jj-1,8)
          alatg_n = strt_latg + delt_latg * real(jj  ,8)
          if ((alatg_s < phi) .and. (phi <= alatg_n)) then
            jg = jj
            exit
          end if
        end do

        if (atexl(i,j,1) > 0.0d0) then
          areag = areag + &
               &    (aexl(i-1,j  ,1) * a_br(i-1,j  ) + aexl(i,j  ,1) * a_bl(i,j  ) &
               &   + aexl(i-1,j-1,1) * a_tr(i-1,j-1) + aexl(i,j-1,1) * a_tl(i,j-1))
          areaz(jg) = areaz(jg) + &
               &    (aexl(i-1,j  ,1) * a_br(i-1,j  ) + aexl(i,j  ,1) * a_bl(i,j  ) &
               &   + aexl(i-1,j-1,1) * a_tr(i-1,j-1) + aexl(i,j-1,1) * a_tl(i,j-1))
          wtotal = wtotal + &
               &    (aexl(i-1,j  ,1) * a_br(i-1,j  ) + aexl(i,j  ,1) * a_bl(i,j  )  &
               &   + aexl(i-1,j-1,1) * a_tr(i-1,j-1) + aexl(i,j-1,1) * a_tl(i,j-1)) &
               &   * wtot(i,j)
          wtotg(jg) = wtotg(jg) + &
               &    (aexl(i-1,j  ,1) * a_br(i-1,j  ) + aexl(i,j  ,1) * a_bl(i,j  ) &
               &   + aexl(i-1,j-1,1) * a_tr(i-1,j-1) + aexl(i,j-1,1) * a_tl(i,j-1)) &
               &   * wtot(i,j)
          wtotp(jg) = wtotp(jg) + &
               &    (ind_pac_mask(i-1,j) * aexl(i-1,j,1) * a_br(i-1,j) &
               &   + ind_pac_mask(i,j) * aexl(i,j,1) * a_bl(i,j)     &
               &   + ind_pac_mask(i-1,j-1) * aexl(i-1,j-1,1) * a_tr(i-1,j-1) &
               &   + ind_pac_mask(i,j-1) * aexl(i,j-1,1) * a_tl(i,j-1)) &
               &   * wtot(i,j)
          wtota(jg) = wtota(jg) + &
               &    (atl_arc_mask(i-1,j) * aexl(i-1,j,1) * a_br(i-1,j) &
               &   + atl_arc_mask(i,j) * aexl(i,j,1) * a_bl(i,j)     &
               &   + atl_arc_mask(i-1,j-1) * aexl(i-1,j-1,1) * a_tr(i-1,j-1) &
               &   + atl_arc_mask(i,j-1) * aexl(i,j-1,1) * a_tl(i,j-1)) &
               &   * wtot(i,j)
        end if
      end do

    end do

    !-----
    mwtrg(1:jmug) = 0.0d0
    mwtrp(1:jmug) = 0.0d0
    mwtra(1:jmug) = 0.0d0

    mwtrg(jmug) = wtotg(jmug)
    mwtrp(jmug) = wtotp(jmug) 
    mwtra(jmug) = wtota(jmug)

    if (areag > 0.0d0) then
      wtotal = wtotal / areag
    else
      wtotal = 0.0d0
    end if

    write(6,'(i8,1a,e22.15)') nyear, ' Global imbalance = ', wtotal

    irec3 = irec3 + 1
    write(mtot3,rec=irec3) real(wtotal,4)

    !-----
    ! Indo-Pacific

    do j = jmug - 1, 1, -1
      mwtrp(j) = mwtrp(j+1) - wtotp(j)
    end do

!    do j = 1, jmug
!      if (wtotp(j) == 0.0d0) then
!        mwtrp(j) = -9.99e33
!      end if
!    end do

    ! Atlantic

    do j = jmug - 1, 1, -1
      mwtra(j) = mwtra(j+1) - wtota(j)
    end do

!    do j = 1, jmug
!      if (wtota(j) == 0.0d0) then
!        mwtra(j) = -9.99e33
!      end if
!    end do

    ! Global

!    do j = 2, jmug
!      mwtrg(j) = mwtrg(j-1) + wtotg(j)
!    end do

    do j = jmug - 1, 1, -1
      mwtrg(j) = mwtrg(j+1) - wtotg(j)
    end do

    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(mwtrg(1:jmug),4)
    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(mwtra(1:jmug),4)
    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(mwtrp(1:jmug),4)

    close(mtot3)
    close(mtot2)

  end do

  !----------

end program surface_water_flux_diag
