! -*-F90-*-
!
!======================================================================
! Information:
!     calculate ice area and mean thickness
!----------------------------------------------------------------------
program surface_heat_flux_diag

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

  real(8) :: qot(imut,jmut), qsw(imut,jmut), qfr(imut,jmut), qtot(imut,jmut)
  real(8) :: ind_pac_mask(imut,jmut)
  real(8) :: atl_arc_mask(imut,jmut)

  real(8) :: qtotal
  real(8) :: areag

  !-----

  integer(4) :: jmug = 361
  real(8) :: delt_latg, strt_latg

  integer(4) :: jj, jg
  real(8) :: alatg_s, alatg_n
  real(8) :: lambda, phi
  real(8) :: mu, psi

  real(8) :: qtotg(jmug), qtotp(jmug), qtota(jmug)
  real(8) :: mhtrg(jmug), mhtrp(jmug), mhtra(jmug)

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
  character(256) :: ftopo, fgrid, fscale, fbasmap

  integer(4) :: nday, month, nyear, nd
  integer(4), parameter :: ndata = 12
  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  integer(4) :: irec1, irec2, irec3
  integer(4) :: i, j, k, m
  integer(4) :: ibyr, ieyr

  !-----------------------------------------------------------------------

  namelist /nml_heat_diag/ ftopo, fgrid, fscale, fbasmap, &
       & flnin_org1, flnot_base2, flnot_base3, &
       & ibyr, ieyr, &
       & jmug, delt_latg, strt_latg

  open (11,file='namelist_heat_diag')
  read (11,nml=nml_heat_diag)
  close(11)

  !-----------------------------------------------------------------------
  ! set grid points

  call setgrd(ftopo, fgrid, fscale)

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

    qtot(1:imut,1:jmut) = 0.0d0

    write(flnin1,'(1a,i4.4)') trim(flnin_org1),nyear
    open(mtin1, file=flnin1, access='direct', recl=4*imut*jmut)

    read(mtin1,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    qtot(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

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
    qtotg(1:jmug) = 0.0d0
    qtotp(1:jmug) = 0.0d0
    qtota(1:jmug) = 0.0d0

    areag = 0.0d0
    qtotal = 0.0d0

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
          qtotal = qtotal + &
               &    (aexl(i-1,j  ,1) * a_br(i-1,j  ) + aexl(i,j  ,1) * a_bl(i,j  )  &
               &   + aexl(i-1,j-1,1) * a_tr(i-1,j-1) + aexl(i,j-1,1) * a_tl(i,j-1)) &
               &   * qtot(i,j)
          qtotg(jg) = qtotg(jg) + &
               &    (aexl(i-1,j  ,1) * a_br(i-1,j  ) + aexl(i,j  ,1) * a_bl(i,j  ) &
               &   + aexl(i-1,j-1,1) * a_tr(i-1,j-1) + aexl(i,j-1,1) * a_tl(i,j-1)) &
               &   * qtot(i,j)
          qtotp(jg) = qtotp(jg) + &
               &    (ind_pac_mask(i-1,j) * aexl(i-1,j,1) * a_br(i-1,j) &
               &   + ind_pac_mask(i,j) * aexl(i,j,1) * a_bl(i,j)     &
               &   + ind_pac_mask(i-1,j-1) * aexl(i-1,j-1,1) * a_tr(i-1,j-1) &
               &   + ind_pac_mask(i,j-1) * aexl(i,j-1,1) * a_tl(i,j-1)) &
               &   * qtot(i,j)
          qtota(jg) = qtota(jg) + &
               &    (atl_arc_mask(i-1,j) * aexl(i-1,j,1) * a_br(i-1,j) &
               &   + atl_arc_mask(i,j) * aexl(i,j,1) * a_bl(i,j)     &
               &   + atl_arc_mask(i-1,j-1) * aexl(i-1,j-1,1) * a_tr(i-1,j-1) &
               &   + atl_arc_mask(i,j-1) * aexl(i,j-1,1) * a_tl(i,j-1)) &
               &   * qtot(i,j)
        end if
      end do

    end do

    !-----
    mhtrg(1:jmug) = 0.0d0
    mhtrp(1:jmug) = 0.0d0
    mhtra(1:jmug) = 0.0d0

    mhtrg(jmug) = qtotg(jmug)
    mhtrp(jmug) = qtotp(jmug) 
    mhtra(jmug) = qtota(jmug)

    if (areag > 0.0d0) then
      qtotal = qtotal / areag
    else
      qtotal = 0.0d0
    end if

    write(6,'(i8,1a,f10.5)') nyear, ' Global imbalance = ', qtotal

    irec3 = irec3 + 1
    write(mtot3,rec=irec3) real(qtotal,4)

    !-----
    ! Indo-Pacific

    do j = jmug - 1, 1, -1
      mhtrp(j) = mhtrp(j+1) - qtotp(j)
    end do

!    do j = 1, jmug
!      if (qtotp(j) == 0.0d0) then
!        mhtrp(j) = -9.99e33
!      end if
!    end do

    ! Atlantic

    do j = jmug - 1, 1, -1
      mhtra(j) = mhtra(j+1) - qtota(j)
    end do

!    do j = 1, jmug
!      if (qtota(j) == 0.0d0) then
!        mhtra(j) = -9.99e33
!      end if
!    end do

    ! Global

!    do j = 2, jmug
!      mhtrg(j) = mhtrg(j-1) + qtotg(j)
!    end do

    do j = jmug - 1, 1, -1
      mhtrg(j) = mhtrg(j+1) - qtotg(j)
    end do

    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(mhtrg(1:jmug),4)
    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(mhtra(1:jmug),4)
    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(mhtrp(1:jmug),4)

    close(mtot3)
    close(mtot2)

  end do

  !----------

end program surface_heat_flux_diag
