! -*-F90-*-
!
!======================================================================
! Information:
!     calculate total water flux
!----------------------------------------------------------------------
program surface_water_flux_all_annual

  use libmxe_para, only: libmxe_para__register, clen, rho &
                     & , pi, radius, radian, radian_r, type_libmxe_para
  use libmxe_grid, only: libmxe_grid__register  &
                     & , type_libmxe_grid
  use libmxe_topo, only: libmxe_topo__register, libmxe_topo__aexl &
                     & , type_libmxe_topo
  use libmxe_io, only: libmxe_io__register, type_libmxe_io
  use libmxe_grads, only: libmxe_grads__make, type_grads
  use libmxe_stmrgn, only: libmxe_stmrgn__var2_n  &
                     & , libmxe_stmrgn__var2_x
  use libmxe_trnsfrm

  use file_open_close_manager

  implicit none

  integer(4), parameter :: nmon = 12

  integer(4) :: imut, jmut
  integer(4) :: ibu, ieu, jbu, jeu
  integer(4) :: ibt, iet, jbt, jet

!  integer(4) :: iettmp

  real(4),allocatable :: work4(:,:)
  real(4),allocatable :: ibas4(:,:)

  real(8),allocatable :: evp(:,:), pcp(:,:), pcp_all(:,:), rof(:,:)
  real(8),allocatable :: wtot(:,:)
  real(8),allocatable :: ind_pac_mask(:,:)
  real(8),allocatable :: atl_arc_mask(:,:)

  real(8) :: wtotal
  real(8) :: prcp_land, prcp_ocean, prcp_land_except_antarctica
  real(8) :: runoff_integ, runoff_mean
  real(8) :: areag, area_ocean, area_land, area_tmp

  real(8) :: conv_runoff

  !----------------------------------------------------
  ! 地理座標パラメタ
  ! 
  real(8)    :: slatg, elatg, dlatg
  integer(4) :: jmgeo
  integer(4) :: jj, jg
  real(8) :: alatg_s, alatg_n
  real(8) :: lambda, phi
  real(8) :: mu, psi

  real(8),allocatable :: wtotg(:), wtotp(:), wtota(:)
  real(8),allocatable :: mwtrg(:), mwtrp(:), mwtra(:)

  real(8),allocatable :: areazg(:), areazp(:), areaza(:)

  real(8) :: area_earth, area_this

  real(8) :: water_adjust, water_adjust_bering
  real(8),parameter :: water_adjust_default = 0.0d0
  logical :: l_med_atl
  logical :: l_bering_atl
  logical :: l_bering_pac

  real(4) :: undef_in, undef_out

  !-----

  integer(4), parameter :: mtin1 = 71, mtin2 = 72, mtin3 = 73, mtin4 = 74
  integer(4), parameter :: mtot1 = 81, mtot2 = 82, mtot3 = 83, mtot4 = 84, mtot5 = 85
  character(256) :: flnin_org1, flnin_org2, flnin_org3, flnin_org4
  character(256) :: flnot_base1, flnot_base2, flnot_base3, flnot_base4, flnot_base5
  character(256) :: flnin1, flnin2, flnin3, flnin4
  character(256) :: flnot1, flnot2, flnot3, flnot4, flnot5

  character(256) :: fbasmap

  integer(4) :: nday, month, nyear, nd
  integer(4), parameter :: ndata = 12
  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  integer(4) :: irec1, irec2, irec3, irec4, irec5
  integer(4) :: i, j, k, m
  integer(4) :: ibyr, ieyr

  type(type_libmxe_para) :: para
  type(type_libmxe_grid) :: grid
  type(type_libmxe_topo) :: topo
  type(type_libmxe_io) :: io
  type(type_grads) :: grads

  !-----------------------------------------------------------------------

  namelist /nml_waterall/ fbasmap, &
       & flnin_org1, flnin_org2, flnin_org3, flnin_org4, &
       & conv_runoff, &
       & flnot_base1, flnot_base2, flnot_base3, flnot_base4, flnot_base5, &
       & undef_in, undef_out, &
       & ibyr, ieyr, &
       & slatg, elatg, dlatg, &
       & water_adjust, water_adjust_bering, &
       & l_med_atl, l_bering_atl, l_bering_pac

  !-----------------------------------------------------------------------

  l_med_atl = .false.
  l_bering_atl = .false.
  l_bering_pac = .false.

  water_adjust = water_adjust_default
  water_adjust_bering = water_adjust_default
  open (11,file='namelist.waterall')
  read (11,nml=nml_waterall)
  close(11)

  !-----------------------------------------------------------------------
  ! set grid points

  !-- model settings --

  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)
  call libmxe_topo__aexl(topo,para)

  call set_abc(90.d0,0.0d0,-90.0d0,0.0d0)

  imut = para%imut
  jmut = para%jmut

  ibu = 1
  ieu = imut
  jbu = 1
  jeu = jmut

  allocate(work4(1:imut,1:jmut))
  allocate(ibas4(1:imut,1:jmut))

  allocate(evp(1:imut,1:jmut), pcp(1:imut,1:jmut))
  allocate(pcp_all(1:imut,1:jmut))
  allocate(rof(1:imut,1:jmut))
  allocate(wtot(1:imut,1:jmut))
  allocate(ind_pac_mask(1:imut,1:jmut))
  allocate(atl_arc_mask(1:imut,1:jmut))

  !----
  !  mask
  !
  open(mtin1, file=fbasmap, access='direct', recl=4*imut*jmut)
  read(mtin1,rec=1) ((ibas4(i,j),i=1,imut),j=1,jmut)
  close(mtin1)
  !
  atl_arc_mask(1:imut,1:jmut) = 0.0d0
  ind_pac_mask(1:imut,1:jmut) = 0.0d0
  do j = jbu, jeu
    do i = ibu, ieu
      if (ibas4(i,j) == 2.0 .or. ibas4(i,j) == 3.0) then
!        if ((alatu(j) < 68.0) .and. (alatu(j) > -34.0)) then
        ind_pac_mask(i,j) = 1.0d0
!        end if
      end if
      if (l_med_atl) then
        if ((ibas4(i,j) == 1.0) .or. (ibas4(i,j) == 4.0)) then
          atl_arc_mask(i,j) = 1.0d0
        end if
      else
        if (ibas4(i,j) == 1.0) then
          atl_arc_mask(i,j) = 1.0d0
        end if
      end if
    end do
  end do

  !---------------------------------------------------

  jmgeo = int((elatg - slatg + 1.0d-6) / dlatg)

  write(6,*) ' jmgeo = ', jmgeo

  allocate(wtotg(1:jmgeo), wtotp(1:jmgeo), wtota(1:jmgeo))
  allocate(mwtrg(1:jmgeo+1), mwtrp(1:jmgeo+1), mwtra(1:jmgeo+1))
  allocate(areazg(1:jmgeo),areazp(1:jmgeo),areaza(1:jmgeo))

  !---------------------------------------------------

  areag     = 0.0d0
  area_this = 0.0d0
  area_land = 0.0d0

  do j = jbu, jeu
    do i = ibu, ieu
      area_tmp = grid%a_br(i,j) + grid%a_bl(i,j) + grid%a_tr(i,j) + grid%a_tl(i,j)
      areag = areag + topo%aexl(i,j,1) * area_tmp
      area_land = area_land + (1.0d0 - topo%aexl(i,j,1)) * area_tmp
      area_this = area_this + area_tmp
    end do
  end do

  area_earth = 4.0d0 * pi * radius * radius

  write(6,'(1a,E20.10)') ' Global Ocean area  = ', areag
  write(6,'(1a,E20.10)') ' Global Land area   = ', area_land

  write(6,'(1a,E20.10)') ' area earth = ', area_earth
  write(6,'(1a,E20.10)') ' area this  = ', area_this

  ! read temperature and salinity

  do nyear = ibyr, ieyr

    write(6,*) nyear

    wtot(1:imut,1:jmut) = 0.0d0

    write(flnin1,'(1a,i4.4)') trim(flnin_org1),nyear
    open(mtin1, file=flnin1, access='direct', recl=4*imut*jmut)

    write(flnin2,'(1a,i4.4)') trim(flnin_org2),nyear
    open(mtin2, file=flnin2, access='direct', recl=4*imut*jmut)

!    write(flnin3,'(1a,i4.4)') trim(flnin_org3),nyear
!    open(mtin3, file=flnin3, access='direct', recl=4*imut*jmut)

    write(flnin4,'(1a,i4.4)') trim(flnin_org4),nyear
    open(mtin4, file=flnin4, access='direct', recl=4*imut*jmut)

    read(mtin1,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    evp(1:imut,1:jmut) = real(work4(1:imut,1:jmut),8)

    read(mtin2,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    pcp(1:imut,1:jmut) = real(work4(1:imut,1:jmut),8)

    pcp_all(1:imut,1:jmut) = pcp(1:imut,1:jmut)
!    read(mtin3,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
!    pcp_all(1:imut,1:jmut) = real(work4(1:imut,1:jmut),8)

!    rof(1:imut,1:jmut) = 0.0d0
    read(mtin4,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    rof(1:imut,1:jmut) = real(work4(1:imut,1:jmut),8) * conv_runoff

    close(mtin4)
!    close(mtin3)
    close(mtin2)
    close(mtin1)

    !-------

    write(flnot1,'(1a,i4.4)') trim(flnot_base1),nyear
    write(*,*) ' GrADs data written to... ', trim(flnot1)
    open(mtot1,file=flnot1,form='unformatted',access='direct',recl=4*imut*jmut)
    irec1 = 0

    write(flnot2,'(1a,i4.4)') trim(flnot_base2),nyear
    write(*,*) ' GrADs data written to... ', trim(flnot2)
    open(mtot2,file=flnot2,form='unformatted',access='direct',recl=4*(jmgeo+1))
    irec2 = 0

    write(flnot3,'(1a,i4.4)') trim(flnot_base3),nyear
    write(*,*) ' GrADs data written to... ', trim(flnot3)
    open(mtot3,file=flnot3,form='unformatted',access='direct',recl=4)
    irec3 = 0

    write(flnot4,'(1a,i4.4)') trim(flnot_base4),nyear
    write(*,*) ' GrADs data written to... ', trim(flnot4)
    open(mtot4,file=flnot4,form='unformatted',access='direct',recl=4)
    irec4 = 0

    write(flnot5,'(1a,i4.4)') trim(flnot_base5),nyear
    write(*,*) ' GrADs data written to... ', trim(flnot5)
    open(mtot5,file=flnot5,form='unformatted',access='direct',recl=4*jmgeo)
    irec5 = 0

    do j = 1, jmut
      do i = 1, imut
        wtot(i,j) = topo%aexl(i,j,1) * (pcp(i,j) - evp(i,j)) + rof(i,j)
      end do
    end do

    irec1 = irec1 + 1
    write(mtot1,rec=irec1) real(wtot(1:imut,1:jmut),4)
    write(6,*) ' total water flux written '

    !-----------------

    areazg(1:jmgeo) = 0.0d0
    areaza(1:jmgeo) = 0.0d0
    areazp(1:jmgeo) = 0.0d0
    wtotg(1:jmgeo) = 0.0d0
    wtotp(1:jmgeo) = 0.0d0
    wtota(1:jmgeo) = 0.0d0
    mwtrg(1:jmgeo+1) = 0.0d0
    mwtrp(1:jmgeo+1) = 0.0d0
    mwtra(1:jmgeo+1) = 0.0d0

    wtotal = 0.0d0

    area_ocean = 0.0d0
    area_land  = 0.0d0
    prcp_ocean = 0.0d0
    prcp_land  = 0.0d0
    prcp_land_except_antarctica = 0.0d0
    runoff_integ = 0.0d0

    do j = jbu, jeu
      do i = ibu, ieu

        mu  = grid%lonu(i) * radian_r
        psi = grid%latu(j) * radian_r
        call mp2lp(lambda, phi, mu, psi)
        lambda = lambda * radian
        phi = phi * radian

        do jj = 1, jmgeo
          alatg_s = slatg + dlatg * real(jj-1,8)
          alatg_n = slatg + dlatg * real(jj  ,8)
          if ((alatg_s <= phi) .and. (phi < alatg_n)) then
            jg = jj
            !write(6,*) alatg_s, phi, alatg_n
            exit
          end if
        end do

        !write(6,*) i, j, jg

        area_tmp   = (grid%a_br(i,j) + grid%a_bl(i,j) + grid%a_tr(i,j) + grid%a_tl(i,j)) * 1.0d-4
        area_ocean = area_ocean + topo%aexl(i,j,1) * area_tmp
        prcp_ocean = prcp_ocean + topo%aexl(i,j,1) * area_tmp * pcp_all(i,j)
!        area_land  = area_land  + (1.0d0 - topo%aexl(i,j,1)) * area_tmp
!        prcp_land  = prcp_land  + (1.0d0 - topo%aexl(i,j,1)) * area_tmp * pcp_all(i,j)
!        if (phi > -60.0d0) then
!          prcp_land_except_antarctica = prcp_land_except_antarctica + (1.0d0 - topo%aexl(i,j,1)) * area_tmp * pcp_all(i,j)
!        end if
        wtotal     = wtotal    + topo%aexl(i,j,1) * area_tmp * (pcp(i,j)-evp(i,j)) &
             &                 + area_tmp * rof(i,j)
        wtotg(jg)  = wtotg(jg) + topo%aexl(i,j,1) * area_tmp * (pcp(i,j)-evp(i,j) + water_adjust) &
             &                 + area_tmp * rof(i,j)
        areazg(jg) = areazg(jg) + area_tmp
        wtotp(jg) = wtotp(jg) + ind_pac_mask(i,j) * area_tmp * (wtot(i,j) + water_adjust)
        areazp(jg) = areazp(jg) + ind_pac_mask(i,j) * area_tmp
        wtota(jg) = wtota(jg) + atl_arc_mask(i,j) * area_tmp * (wtot(i,j) + water_adjust)
        areaza(jg) = areaza(jg) + atl_arc_mask(i,j) * area_tmp
        runoff_integ = runoff_integ + area_tmp * rof(i,j)

      end do
    end do

    !-----

    if (area_ocean > 0.0d0) then
      wtotal = wtotal / area_ocean
      runoff_mean = runoff_integ / area_ocean
    else
      wtotal = 0.0d0
    end if

    write(6,'(i8,1a,e12.6)') nyear, ' Global imbalance = ', wtotal

    irec3 = irec3 + 1
    write(mtot3,rec=irec3) real(wtotal,4)

    !if (area_ocean > 0.0d0) then
    !  prcp_ocean = prcp_ocean / area_ocean
    !else
    !  prcp_ocean = 0.0d0
    !end if

    !if (area_land > 0.0d0) then
    !  prcp_land = prcp_land / area_land
    !else
    !  prcp_land = 0.0d0
    !end if

    write(6,'(i8,1a,f12.6)') nyear, ' Ocean precipitation (10^9 kg m-2 s-2)      = ', prcp_ocean * 1.0d-9
!    write(6,'(i8,1a,f12.6)') nyear, ' Land precipitation (10^9 kg m-2 s-2)       = ', prcp_land * 1.0d-9
!    write(6,'(i8,1a,f12.6)') nyear, ' Land precipitation (except for Antarctica) = ', prcp_land_except_antarctica * 1.0d-9
    write(6,'(i8,1a,f12.6)') nyear, ' River runoff (10^9 kg m-2 s-2)             = ', runoff_integ * 1.0d-9
    write(6,'(i8,1a,e12.6)') nyear, ' River runoff (kg/m2/s) = ', runoff_mean

    irec4 = irec4 + 1
    write(mtot4,rec=irec4) real(prcp_ocean,4)
    irec4 = irec4 + 1
    write(mtot4,rec=irec4) real(prcp_land,4)
    irec4 = irec4 + 1
    write(mtot4,rec=irec4) real(prcp_land_except_antarctica,4)
    irec4 = irec4 + 1
    write(mtot4,rec=irec4) real(runoff_integ,4)
    irec4 = irec4 + 1
    write(mtot4,rec=irec4) real(runoff_mean,4)

    !-----
    ! Indo-Pacific
    !
    if (l_bering_pac) then
      do j = jmgeo, 1, -1
        if (areazp(j) /= 0.0d0) then
          wtotp(j) = wtotp(j) - water_adjust_bering
          exit
        end if
      end do
    end if

    do j = jmgeo - 1, 1, -1
      mwtrp(j) = mwtrp(j+1) - wtotp(j)
    end do
    if (areazp(jmgeo) == 0.0d0) then
      mwtrp(jmgeo+1) = real(undef_out,8)
    end if
    do j = jmgeo, 1, -1
      if (areazp(j) == 0.0d0) then
        mwtrp(j) = real(undef_out,8)
      end if
    end do

    !
    ! Atlantic
    !
    if (l_bering_atl) then
      do jj = 1, jmgeo
        alatg_s = slatg + dlatg * real(jj-1,8)
        alatg_n = slatg + dlatg * real(jj  ,8)
        if ((alatg_s < 67.5d0) .and. (67.5d0 < alatg_n)) then
          wtota(jj) = wtota(jj) + water_adjust_bering
          exit
        end if
      end do
    end if

    do j = jmgeo - 1, 1, -1
      mwtra(j) = mwtra(j+1) - wtota(j)
    end do
    if (areaza(jmgeo) == 0.0d0) then
      mwtra(jmgeo+1) = real(undef_out,8)
    end if
    do j = jmgeo, 1, -1
      if (areaza(j) == 0.0d0) then
        mwtra(j) = real(undef_out,8)
      end if
    end do
    !
    ! Global
    !
    do j = jmgeo - 1, 1, -1
      mwtrg(j) = mwtrg(j+1) - wtotg(j)
      !write(6,*) mwtrg(j), wtotg(j)
    end do
    if (areazg(jmgeo) == 0.0d0) then
      mwtrg(jmgeo+1) = real(undef_out,8)
    end if
    do j = jmgeo, 1, -1
      if (areazg(j) == 0.0d0) then
        mwtrg(j) = real(undef_out,8)
      end if
    end do

    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(mwtrg(1:jmgeo+1),4)
    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(mwtra(1:jmgeo+1),4)
    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(mwtrp(1:jmgeo+1),4)

    irec5 = irec5 + 1
    write(mtot5,rec=irec5) real(wtotg(1:jmgeo),4)
    irec5 = irec5 + 1
    write(mtot5,rec=irec5) real(wtota(1:jmgeo),4)
    irec5 = irec5 + 1
    write(mtot5,rec=irec5) real(wtotp(1:jmgeo),4)

    close(mtot5)
    close(mtot4)
    close(mtot3)
    close(mtot2)
    close(mtot1)

  end do

  !----------

end program surface_water_flux_all_annual
