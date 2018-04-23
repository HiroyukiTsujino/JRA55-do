! -*-F90-*-
!
!======================================================================
! Information:
!     calculate ice area and mean thickness
!----------------------------------------------------------------------
program surface_heat_flux_all_annual

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

  real(8),allocatable :: qlw(:,:), qsw(:,:), qsn(:,:), qla(:,:)
  real(8),allocatable :: qtot(:,:)
  real(8),allocatable :: ind_pac_mask(:,:)
  real(8),allocatable :: atl_arc_mask(:,:)

  real(8) :: qtotal
  real(8) :: areag, area_tmp

  !----------------------------------------------------
  ! 地理座標パラメタ
  ! 
  real(8)    :: slatg, elatg, dlatg
  integer(4) :: jmgeo
  integer(4) :: jj, jg
  real(8) :: alatg_s, alatg_n
  real(8) :: lambda, phi
  real(8) :: mu, psi

  real(8),allocatable :: qtotg(:), qtotp(:), qtota(:)
  real(8),allocatable :: mhtrg(:), mhtrp(:), mhtra(:)

  real(8),allocatable :: areazg(:), areazp(:), areaza(:)

  real(8) :: area_earth, area_this
  real(8) :: heat_adjust, heat_adjust_noatl
  real(8),parameter :: heat_adjust_default = 0.0d0
  real(8),parameter :: heat_adjust_noatl_default = 0.0d0
  logical :: l_med_atl

  real(4) :: undef_in, undef_out

  !-----

  integer(4), parameter :: mtin1 = 71, mtin2 = 72, mtin3 = 73, mtin4 = 74
  integer(4), parameter :: mtot1 = 81, mtot2 = 82, mtot3 = 83, mtot4 = 84
  character(256) :: flnin_org1, flnin_org2, flnin_org3, flnin_org4
  character(256) :: flnot_base1, flnot_base2, flnot_base3, flnot_base4
  character(256) :: flnin1, flnin2, flnin3, flnin4
  character(256) :: flnot1, flnot2, flnot3, flnot4

  character(256) :: fbasmap

  integer(4) :: nday, month, nyear, nd
  integer(4), parameter :: ndata = 12
  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  integer(4) :: irec1, irec2, irec3, irec4
  integer(4) :: i, j, k, m
  integer(4) :: ibyr, ieyr

  type(type_libmxe_para) :: para
  type(type_libmxe_grid) :: grid
  type(type_libmxe_topo) :: topo
  type(type_libmxe_io) :: io
  type(type_grads) :: grads

  !-----------------------------------------------------------------------

  namelist /nml_heatall/ fbasmap, &
       & flnin_org1, flnin_org2, flnin_org3, flnin_org4, &
       & flnot_base1, flnot_base2, flnot_base3, flnot_base4, &
       & undef_in, undef_out, &
       & ibyr, ieyr, &
       & slatg, elatg, dlatg, &
       & l_med_atl, &
       & heat_adjust, heat_adjust_noatl

  !-----------------------------------------------------------------------

  l_med_atl = .false.
  heat_adjust = heat_adjust_default
  heat_adjust_noatl = heat_adjust_noatl_default
  open (11,file='namelist.heatall')
  read (11,nml=nml_heatall)
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

  allocate(qlw(1:imut,1:jmut), qsw(1:imut,1:jmut))
  allocate(qsn(1:imut,1:jmut), qla(1:imut,1:jmut))
  allocate(qtot(1:imut,1:jmut))
  allocate(ind_pac_mask(1:imut,1:jmut))
  allocate(atl_arc_mask(1:imut,1:jmut))

  !----------
  !  mask

  open(mtin1, file=fbasmap, access='direct', recl=4*imut*jmut)
  read(mtin1,rec=1) ((ibas4(i,j),i=1,imut),j=1,jmut)
  close(mtin1)

  atl_arc_mask(1:imut,1:jmut) = 0.0d0
  ind_pac_mask(1:imut,1:jmut) = 0.0d0
  do j = jbu, jeu
    do i = ibu, ieu
      if (ibas4(i,j) == 2.0 .or. ibas4(i,j) == 3.0) then
!        if ((grid%latu(j) < 68.0) .and. (grid%latu(j) > -34.0)) then
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

  allocate(qtotg(1:jmgeo), qtotp(1:jmgeo), qtota(1:jmgeo))
  allocate(mhtrg(1:jmgeo+1), mhtrp(1:jmgeo+1), mhtra(1:jmgeo+1))
  allocate(areazg(1:jmgeo),areazp(1:jmgeo),areaza(1:jmgeo))

  !---------------------------------------------------

  areag   = 0.0d0
  area_this = 0.0d0

  do j = jbu, jeu
    do i = ibu, ieu
      area_tmp = grid%a_br(i,j) + grid%a_bl(i,j) + grid%a_tr(i,j) + grid%a_tl(i,j)
      areag = areag + topo%aexl(i,j,1) * area_tmp
      area_this = area_this + area_tmp
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

    write(flnin2,'(1a,i4.4)') trim(flnin_org2),nyear
    open(mtin2, file=flnin2, access='direct', recl=4*imut*jmut)

    write(flnin3,'(1a,i4.4)') trim(flnin_org3),nyear
    open(mtin3, file=flnin3, access='direct', recl=4*imut*jmut)

    write(flnin4,'(1a,i4.4)') trim(flnin_org4),nyear
    open(mtin4, file=flnin4, access='direct', recl=4*imut*jmut)

    read(mtin1,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
!    qsw(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))
    do j = 1, jmut
      do i = 1, imut
        if (abs(work4(i,j)) < 9.0e33) then
          qsw(i,j) = real(work4(i,j),8)
        else
          qsw(i,j) = 0.0d0
        end if
      end do
    end do

    read(mtin2,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
!    qlw(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))
    do j = 1, jmut
      do i = 1, imut
        if (abs(work4(i,j)) < 9.0e33) then
          qlw(i,j) = real(work4(i,j),8)
        else
          qlw(i,j) = 0.0d0
        end if
      end do
    end do

    read(mtin3,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
!    qsn(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))
    do j = 1, jmut
      do i = 1, imut
        if (abs(work4(i,j)) < 9.0e33) then
          qsn(i,j) = real(work4(i,j),8)
        else
          qsn(i,j) = 0.0d0
        end if
      end do
    end do

    read(mtin4,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
!    qla(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))
    do j = 1, jmut
      do i = 1, imut
        if (abs(work4(i,j)) < 9.0e33) then
          qla(i,j) = real(work4(i,j),8)
        else
          qla(i,j) = 0.0d0
        end if
      end do
    end do

    close(mtin4)
    close(mtin3)
    close(mtin2)
    close(mtin1)

    !-------

    write(flnot1,'(1a,i4.4)') trim(flnot_base1),nyear
    write(6,*) ' GrADs data written to... ', trim(flnot1)
    open(mtot1,file=flnot1,form='unformatted',access='direct',recl=4*imut*jmut)
    irec1 = 0

    write(flnot2,'(1a,i4.4)') trim(flnot_base2),nyear
    write(6,*) ' GrADs data written to... ', trim(flnot2)
    open(mtot2,file=flnot2,form='unformatted',access='direct',recl=4*(jmgeo+1))
    irec2 = 0

    write(flnot3,'(1a,i4.4)') trim(flnot_base3),nyear
    write(6,*) ' GrADs data written to... ', trim(flnot3)
    open(mtot3,file=flnot3,form='unformatted',access='direct',recl=4)
    irec3 = 0

    write(flnot4,'(1a,i4.4)') trim(flnot_base4),nyear
    write(6,*) ' GrADs data written to... ', trim(flnot4)
    open(mtot4,file=flnot4,form='unformatted',access='direct',recl=4*jmgeo)
    irec4 = 0

    do j = 1, jmut
      do i = 1, imut
        if (topo%aexl(i,j,1) /= 0.0d0) then
          qtot(i,j) = qsw(i,j) + qlw(i,j) + qla(i,j) + qsn(i,j)
        else
          qtot(i,j) = real(undef_out,8)
        end if
      end do
    end do

    irec1 = irec1 + 1
    write(mtot1,rec=irec1) real(qtot(1:imut,1:jmut),4)
    write(6,*) ' total heat flux written '

    !-----------------

    areazg(1:jmgeo) = 0.0d0
    areaza(1:jmgeo) = 0.0d0
    areazp(1:jmgeo) = 0.0d0
    qtotg(1:jmgeo) = 0.0d0
    qtotp(1:jmgeo) = 0.0d0
    qtota(1:jmgeo) = 0.0d0
    mhtrg(1:jmgeo+1) = 0.0d0
    mhtrp(1:jmgeo+1) = 0.0d0
    mhtra(1:jmgeo+1) = 0.0d0

    areag = 0.0d0
    qtotal = 0.0d0

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

        !if (i==1) then
        !  write(6,*) j, jg, phi, grid%latu(j),grid%lonu(i)
        !end if

        if (topo%aexl(i,j,1) > 0.0d0) then
          area_tmp  = topo%aexl(i,j,1) * &
               &     (grid%a_br(i,j) + grid%a_bl(i,j) + grid%a_tr(i,j) + grid%a_tl(i,j)) * 1.0d-4
          areag     = areag     + area_tmp
          qtotal    = qtotal    + area_tmp * qtot(i,j)
          qtotg(jg) = qtotg(jg) + area_tmp * &
               & (qtot(i,j) + heat_adjust + (1.0d0 - atl_arc_mask(i,j)) * heat_adjust_noatl)
          areazg(jg) = areazg(jg) + area_tmp
          qtotp(jg) = qtotp(jg) + ind_pac_mask(i,j) * area_tmp * (qtot(i,j) + heat_adjust_noatl)
          areazp(jg) = areazp(jg) + ind_pac_mask(i,j) * area_tmp
          qtota(jg) = qtota(jg) + atl_arc_mask(i,j) * area_tmp * qtot(i,j)
          areaza(jg) = areaza(jg) + atl_arc_mask(i,j) * area_tmp
        end if

      end do
      write(6,*) j, jg, qtotg(jg)
    end do

    !-----

    if (areag > 0.0d0) then
      qtotal = qtotal / areag
    else
      qtotal = 0.0d0
    end if

!    write(6,'(i8,1a,f10.5)') nyear, ' Global imbalance = ', qtotal
    write(6,*) nyear, ' Global imbalance = ', qtotal

    irec3 = irec3 + 1
    write(mtot3,rec=irec3) real(qtotal,4)

    !-----
    ! Indo-Pacific
    !
    do j = jmgeo - 1, 1, -1
      mhtrp(j) = mhtrp(j+1) - qtotp(j)
    end do

!    write(6,*) 'indo-pacific area', areazp(jmgeo)
    if (areazp(jmgeo) == 0.0d0) then
      mhtrp(jmgeo+1) = real(undef_out,8)
    end if
    do j = jmgeo, 1, -1
      if (areazp(j) == 0.0d0) then
        mhtrp(j) = real(undef_out,8)
      end if
    end do
    !
    ! Atlantic
    !
    do j = jmgeo - 1, 1, -1
      mhtra(j) = mhtra(j+1) - qtota(j)
    end do
    if (areaza(jmgeo) == 0.0d0) then
      mhtra(jmgeo+1) = real(undef_out,8)
    end if
    do j = jmgeo, 1, -1
      if (areaza(j) == 0.0d0) then
        mhtra(j) = real(undef_out,8)
      end if
    end do
    !
    ! Global
    !
    do j = jmgeo - 1, 1, -1
      mhtrg(j) = mhtrg(j+1) - qtotg(j)
    end do
    if (areazg(jmgeo) == 0.0d0) then
      mhtrg(jmgeo+1) = real(undef_out,8)
    end if
    do j = jmgeo, 1, -1
      if (areazg(j) == 0.0d0) then
        mhtrg(j) = real(undef_out,8)
      end if
    end do

    !

    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(mhtrg(1:jmgeo+1),4)
    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(mhtra(1:jmgeo+1),4)
    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(mhtrp(1:jmgeo+1),4)

    irec4 = irec4 + 1
    write(mtot4,rec=irec4) real(qtotg(1:jmgeo),4)
    irec4 = irec4 + 1
    write(mtot4,rec=irec4) real(qtota(1:jmgeo),4)
    irec4 = irec4 + 1
    write(mtot4,rec=irec4) real(qtotp(1:jmgeo),4)

    close(mtot4)
    close(mtot3)
    close(mtot2)
    close(mtot1)

  end do

  write(6,*) 'Area global   : ', areag
  write(6,*) 'Area global   : ', sum(areazg(1:jmgeo))
  write(6,*) 'Area Atlantic : ', sum(areaza(1:jmgeo))
  write(6,*) 'Area Indo-Pac : ', sum(areazp(1:jmgeo))
  !----------

end program surface_heat_flux_all_annual
