! -*-F90-*-
!
!======================================================================
! Information:
!     calculate ice area and mean thickness
!----------------------------------------------------------------------
program ice_area_climatology

  use basin_param
  use grid_common

  implicit none

  integer(4), parameter :: nmon = 12

  integer(4), parameter :: ibu = 3, ieu = imut - 2  ! cyclic
  integer(4), parameter :: jbu = 2, jeu = jmut - 3  ! tripolar

  integer(4), parameter :: ibt = 3, iet = imut - 2  ! cyclic
  integer(4), parameter :: jbt = 2, jet = jmut - 2  ! tripolar

  integer(4) :: iettmp

  real(4) :: work4(imut,jmut)

  real(8) :: ar(imut,jmut), hi(imut,jmut), hs(imut,jmut), ti(imut,jmut)
  real(8) :: mask(imut,jmut)

  real(8) :: areag, areaarc
  real(8) :: area_earth, area_this

  !-----

  real(8) :: arcl(imut,jmut,nmon), hicl(imut,jmut,nmon), hscl(imut,jmut,nmon)
  real(8) :: ticl(imut,jmut,nmon)

  real(8) :: volice_n,  volsnw_n
  real(8) :: areaice_n, extentice_n
  real(8) :: aveice_n,  avesnw_n

  real(8) :: volice_s,  volsnw_s
  real(8) :: areaice_s, extentice_s
  real(8) :: aveice_s,  avesnw_s

  real(8) :: volice_mcl_n(nmon),  volsnw_mcl_n(nmon)
  real(8) :: areaice_mcl_n(nmon), extentice_mcl_n(nmon)
  real(8) :: aveice_mcl_n(nmon),  avesnw_mcl_n(nmon)

  real(8) :: volice_mcl_s(nmon),  volsnw_mcl_s(nmon)
  real(8) :: areaice_mcl_s(nmon), extentice_mcl_s(nmon)
  real(8) :: aveice_mcl_s(nmon),  avesnw_mcl_s(nmon)

  !-----

  integer(4), parameter :: mtin1 = 71, mtin2 = 72, mtot1 = 81, mtot2 = 82
  character(256) :: flnin_base
  character(256) :: flnin, flnot, flnav
  character(256) :: ftopo, fgrid, fscale

  integer(4) :: nday, month, nyear, nd
  integer(4), parameter :: ndata = 12
  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  real(8), parameter :: alats = -10.0d0, alatn = 10.d0
  real(8), parameter :: alonw =   0.0d0, alone = 360.d0

  integer(4) :: irec1, irec2, irec3
  integer(4) :: i, j, k, m
  integer(4) :: ibyr, ieyr

  logical :: l_create_mask = .true.
  real(8), parameter :: bad = -9.0d0
!  real(8) :: undef

  logical :: l_percent

  !-----------------------------------------------------------------------

  namelist /ioinficl/ flnin_base, flnot, flnav,  &
       & ftopo, fgrid, fscale, ibyr, ieyr, l_percent
  open (11,file='ioinficl.dat')
  read (11,nml=ioinficl)
  close(11)

  !-----------------------------------------------------------------------
  ! set grid points

  call setgrd(ftopo, fgrid, fscale)

  areag   = 0.0d0
  areaarc = 0.0d0
  area_this = 0.0d0

  do j = jbu, jeu
    do i = ibu, ieu
      areag = areag + aexl(i,j,1) * areauu(i,j)
      area_this = area_this + areauu(i,j)
      if (alatn < alatu(j)) then
        areaarc = areaarc + aexl(i,j,1) * areauu(i,j)
      end if
    end do
  end do

  area_earth = 4.0d0 * pi * radius * radius

  write(6,'(1a,E20.10)') ' Global Ocean area  = ', areag
  write(6,'(1a,E20.10)') ' Arctic area        = ', areaarc

  write(6,'(1a,E20.10)') ' area earth = ', area_earth
  write(6,'(1a,E20.10)') ' area this  = ', area_this

  ! read temperature and salinity

  arcl(1:imut,1:jmut,1:nmon) = 0.0d0
  hicl(1:imut,1:jmut,1:nmon) = 0.0d0
  hscl(1:imut,1:jmut,1:nmon) = 0.0d0
  ticl(1:imut,1:jmut,1:nmon) = 0.0d0

  volice_mcl_n(1:nmon)    = 0.0d0
  volsnw_mcl_n(1:nmon)    = 0.0d0
  areaice_mcl_n(1:nmon)   = 0.0d0
  extentice_mcl_n(1:nmon) = 0.0d0
  aveice_mcl_n(1:nmon)    = 0.0d0
  avesnw_mcl_n(1:nmon)    = 0.0d0

  volice_mcl_s(1:nmon)    = 0.0d0
  volsnw_mcl_s(1:nmon)    = 0.0d0
  areaice_mcl_s(1:nmon)   = 0.0d0
  extentice_mcl_s(1:nmon) = 0.0d0
  aveice_mcl_s(1:nmon)    = 0.0d0
  avesnw_mcl_s(1:nmon)    = 0.0d0

  do nyear = ibyr, ieyr

    do nd = 1, nmon

      month = nd

      write(6,*) ' month = ', month
      write(flnin,'(1a,i4.4,i2.2)') trim(flnin_base),nyear,month
      open(mtin1, file=flnin, access='direct', recl=4*imut*jmut)
      write(*,'(1a,1a)') 'data read from ... ', trim(flnin)

      read(mtin1,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
      hi(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

      read(mtin1,rec=2) ((work4(i,j),i=1,imut),j=1,jmut)
      hs(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

      read(mtin1,rec=3) ((work4(i,j),i=1,imut),j=1,jmut)
      if (l_percent) then
        ar(1:imut,1:jmut) = dble(work4(1:imut,1:jmut)) * 1.0d-2
      else
        ar(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))
      end if

      read(mtin1,rec=4) ((work4(i,j),i=1,imut),j=1,jmut)
      ti(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

      close(mtin1)

      !-----------------

      if (l_create_mask) then
        do j = 1, jmut
          do i = 1, imut
            if (ar(i,j) < bad) then
              mask(i,j) = 0.0d0 ! land
            else
              mask(i,j) = 1.0d0 ! ocean
            end if
          end do
        end do
        l_create_mask = .false.
      end if

      !-----------------

      volice_n    = 0.0d0
      volsnw_n    = 0.0d0
      areaice_n   = 0.0d0
      extentice_n = 0.0d0

      volice_s    = 0.0d0
      volsnw_s    = 0.0d0
      areaice_s   = 0.0d0
      extentice_s = 0.0d0

      do j = 1, jmut
        do i = 1, imut
          if (ar(i,j) > 0.0d0) then
            arcl(i,j,nd) = arcl(i,j,nd) + ar(i,j)
            hicl(i,j,nd) = hicl(i,j,nd) + hi(i,j)
            hscl(i,j,nd) = hscl(i,j,nd) + hs(i,j)
            ticl(i,j,nd) = ticl(i,j,nd) + ti(i,j) * ar(i,j)
          end if
        end do
      end do

      do j = jbt, jet
        if (alatt(j) >= alatn) then
          if (j == jet) then
            iettmp = imut / 2
          else
            iettmp = iet
          end if
          do i = ibt, iettmp
            if (ar(i,j) >= 0.15d0) then
              areaice_n = areaice_n + &
                   &    (aexl(i-1,j  ,1) * a_br(i-1,j  ) + aexl(i,j  ,1) * a_bl(i,j  ) &
                   &   + aexl(i-1,j-1,1) * a_tr(i-1,j-1) + aexl(i,j-1,1) * a_tl(i,j-1)) &
                   &   * ar(i,j)
              extentice_n = extentice_n + &
                   &    (aexl(i-1,j  ,1) * a_br(i-1,j  ) + aexl(i,j  ,1) * a_bl(i,j  ) &
                   &   + aexl(i-1,j-1,1) * a_tr(i-1,j-1) + aexl(i,j-1,1) * a_tl(i,j-1))
              volice_n = volice_n + &
                   &    (aexl(i-1,j  ,1) * a_br(i-1,j  ) + aexl(i,j  ,1) * a_bl(i,j  ) &
                   &   + aexl(i-1,j-1,1) * a_tr(i-1,j-1) + aexl(i,j-1,1) * a_tl(i,j-1)) &
                   &   * hi(i,j)
              volsnw_n = volsnw_n + &
                   &    (aexl(i-1,j  ,1) * a_br(i-1,j  ) + aexl(i,j  ,1) * a_bl(i,j  ) &
                   &   + aexl(i-1,j-1,1) * a_tr(i-1,j-1) + aexl(i,j-1,1) * a_tl(i,j-1)) &
                   &   * hs(i,j)
            end if
          end do
        end if

        if (alatt(j) <= alats) then
          if (j == jet) then
            iettmp = imut / 2
          else
            iettmp = iet
          end if
          do i = ibt, iettmp
            if (ar(i,j) >= 0.15d0) then
              areaice_s = areaice_s + &
                   &    (aexl(i-1,j  ,1) * a_br(i-1,j  ) + aexl(i,j  ,1) * a_bl(i,j  ) &
                   &   + aexl(i-1,j-1,1) * a_tr(i-1,j-1) + aexl(i,j-1,1) * a_tl(i,j-1)) &
                   &   * ar(i,j)
              extentice_s = extentice_s + &
                   &    (aexl(i-1,j  ,1) * a_br(i-1,j  ) + aexl(i,j  ,1) * a_bl(i,j  ) &
                   &   + aexl(i-1,j-1,1) * a_tr(i-1,j-1) + aexl(i,j-1,1) * a_tl(i,j-1))
              volice_s = volice_s + &
                   &    (aexl(i-1,j  ,1) * a_br(i-1,j  ) + aexl(i,j  ,1) * a_bl(i,j  ) &
                   &   + aexl(i-1,j-1,1) * a_tr(i-1,j-1) + aexl(i,j-1,1) * a_tl(i,j-1)) &
                   &   * hi(i,j)
              volsnw_s = volsnw_s + &
                   &    (aexl(i-1,j  ,1) * a_br(i-1,j  ) + aexl(i,j  ,1) * a_bl(i,j  ) &
                   &   + aexl(i-1,j-1,1) * a_tr(i-1,j-1) + aexl(i,j-1,1) * a_tl(i,j-1)) &
                   &   * hs(i,j)
            end if
          end do
        end if
      end do

      if (areaice_n > 0.0d0) then
        aveice_n = volice_n / areaice_n
        avesnw_n = volsnw_n / areaice_n
      else
        aveice_n = 0.0d0
        avesnw_n = 0.0d0
      end if

      if (areaice_s > 0.0d0) then
        aveice_s = volice_s / areaice_s
        avesnw_s = volsnw_s / areaice_s
      else
        aveice_s = 0.0d0
        avesnw_s = 0.0d0
      end if

      volice_mcl_n(nd)    = volice_mcl_n(nd)    + volice_n
      volsnw_mcl_n(nd)    = volsnw_mcl_n(nd)    + volsnw_n
      areaice_mcl_n(nd)   = areaice_mcl_n(nd)   + areaice_n
      extentice_mcl_n(nd) = extentice_mcl_n(nd) + extentice_n
      aveice_mcl_n(nd)    = aveice_mcl_n(nd)    + aveice_n
      avesnw_mcl_n(nd)    = avesnw_mcl_n(nd)    + avesnw_n

      volice_mcl_s(nd)    = volice_mcl_s(nd)    + volice_s
      volsnw_mcl_s(nd)    = volsnw_mcl_s(nd)    + volsnw_s
      areaice_mcl_s(nd)   = areaice_mcl_s(nd)   + areaice_s
      extentice_mcl_s(nd) = extentice_mcl_s(nd) + extentice_s
      aveice_mcl_s(nd)    = aveice_mcl_s(nd)    + aveice_s
      avesnw_mcl_s(nd)    = avesnw_mcl_s(nd)    + avesnw_s

    end do

  end do

  !----------

  write(*,*) ' GrADs data written to... ', trim(flnot)
  open(mtot1,file=flnot,form='unformatted',access='direct',recl=imut*jmut*4)
  irec1 = 0

  write(*,*) ' GrADs data written to... ', trim(flnav)
  open(mtot2,file=flnav,form='unformatted',access='direct',recl=4)
  irec2 = 0

  do nd = 1, nmon

    do j = 1, jmut
      do i = 1, imut
        if (arcl(i,j,nd) > 0.0d0) then
          ticl(i,j,nd) = ticl(i,j,nd) / arcl(i,j,nd)
        else if (mask(i,j) == 1) then
          ticl(i,j,nd) = 0.0d0
        else
          ticl(i,j,nd) = real(undef,8)
        end if
      end do
    end do

    arcl(1:imut,1:jmut,nd) = arcl(1:imut,1:jmut,nd) / dble(ieyr-ibyr+1)
    hicl(1:imut,1:jmut,nd) = hicl(1:imut,1:jmut,nd) / dble(ieyr-ibyr+1)
    hscl(1:imut,1:jmut,nd) = hscl(1:imut,1:jmut,nd) / dble(ieyr-ibyr+1)

    do j = 1, jmut
      do i = 1, imut
        if (mask(i,j) == 0) then
          arcl(i,j,nd) = real(undef,8)
          hicl(i,j,nd) = real(undef,8)
          hscl(i,j,nd) = real(undef,8)
        end if
      end do
    end do

    irec1 = irec1 + 1
    write(mtot1,rec=irec1) real(hicl(1:imut,1:jmut,nd),4)
    irec1 = irec1 + 1
    write(mtot1,rec=irec1) real(hscl(1:imut,1:jmut,nd),4)
    irec1 = irec1 + 1
    write(mtot1,rec=irec1) real(arcl(1:imut,1:jmut,nd),4)
    irec1 = irec1 + 1
    write(mtot1,rec=irec1) real(ticl(1:imut,1:jmut,nd),4)

    volice_mcl_n(nd)    = volice_mcl_n(nd)    / dble(ieyr-ibyr+1)
    volsnw_mcl_n(nd)    = volsnw_mcl_n(nd)    / dble(ieyr-ibyr+1)
    areaice_mcl_n(nd)   = areaice_mcl_n(nd)   / dble(ieyr-ibyr+1)
    extentice_mcl_n(nd) = extentice_mcl_n(nd) / dble(ieyr-ibyr+1)
    aveice_mcl_n(nd)    = aveice_mcl_n(nd)    / dble(ieyr-ibyr+1)
    avesnw_mcl_n(nd)    = avesnw_mcl_n(nd)    / dble(ieyr-ibyr+1)

    volice_mcl_s(nd)    = volice_mcl_s(nd)    / dble(ieyr-ibyr+1)
    volsnw_mcl_s(nd)    = volsnw_mcl_s(nd)    / dble(ieyr-ibyr+1)
    areaice_mcl_s(nd)   = areaice_mcl_s(nd)   / dble(ieyr-ibyr+1)
    extentice_mcl_s(nd) = extentice_mcl_s(nd) / dble(ieyr-ibyr+1)
    aveice_mcl_s(nd)    = aveice_mcl_s(nd)    / dble(ieyr-ibyr+1)
    avesnw_mcl_s(nd)    = avesnw_mcl_s(nd)    / dble(ieyr-ibyr+1)

    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(areaice_mcl_n(nd),4)
    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(extentice_mcl_n(nd),4)
    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(volice_mcl_n(nd),4)
    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(volsnw_mcl_n(nd),4)
    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(aveice_mcl_n(nd),4)
    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(avesnw_mcl_n(nd),4)

    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(areaice_mcl_s(nd),4)
    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(extentice_mcl_s(nd),4)
    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(volice_mcl_s(nd),4)
    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(volsnw_mcl_s(nd),4)
    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(aveice_mcl_s(nd),4)
    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(avesnw_mcl_s(nd),4)

  end do

  close(mtot2)
  close(mtot1)

end program ice_area_climatology
