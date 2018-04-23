! -*-F90-*-
!
!======================================================================
! Information:
!     calculate ice area and mean thickness
!----------------------------------------------------------------------
program surface_heat_flux_component_annual

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

  real(8) :: qtot(imut,jmut)

  real(8) :: qot(imut,jmut), qsw(imut,jmut), qfr(imut,jmut)
  real(8) :: qlw(imut,jmut), qsn(imut,jmut), qlt(imut,jmut)
  real(8) :: qpr(imut,jmut), qev(imut,jmut), qrv(imut,jmut), qit(imut,jmut)

  real(8) :: qtotal
  real(8) :: q_sw, q_lw, q_sn, q_lt
  real(8) :: q_pr, q_ev, q_rv, q_it
  real(8) :: areag

  !-----

  real(8) :: area_earth, area_this, area_loc

  !-----

  integer(4), parameter :: mtin1 = 71, mtin2 = 72, mtin3 = 73, mtin4 = 74, mtin5 = 75
  integer(4), parameter :: mtin6 = 76, mtin7 = 77, mtin8 = 78, mtin9 = 79, mtin10 = 80

  integer(4), parameter :: mtot1 = 81, mtot2 = 82, mtot3 = 83

  character(256) :: flnin_org1, flnin_org2, flnin_org3
  character(256) :: flnin_org4, flnin_org5, flnin_org6
  character(256) :: flnin_org7, flnin_org8, flnin_org9, flnin_org10

  character(256) :: flnin1, flnin2, flnin3
  character(256) :: flnin4, flnin5, flnin6
  character(256) :: flnin7, flnin8, flnin9, flnin10

  character(256) :: flnot_base1, flnot_base2, flnot_base3
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

  namelist /ioinfhcmp/ ftopo, fgrid, fscale, &
       & flnin_org1, flnin_org2, flnin_org3, &
       & flnin_org4, flnin_org5, flnin_org6, &
       & flnin_org7, flnin_org8, flnin_org9, flnin_org10, &
       & flnot_base1, &
       & ibyr, ieyr

  open (11,file='ioinfhcmp.dat')
  read (11,nml=ioinfhcmp)
  close(11)

  !-----------------------------------------------------------------------
  ! set grid points

  call setgrd(ftopo, fgrid, fscale)

  !----

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

    write(flnin2,'(1a,i4.4)') trim(flnin_org2),nyear
    open(mtin2, file=flnin2, access='direct', recl=4*imut*jmut)

    write(flnin3,'(1a,i4.4)') trim(flnin_org3),nyear
    open(mtin3, file=flnin3, access='direct', recl=4*imut*jmut)

    read(mtin1,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    qot(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

    read(mtin2,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    qsw(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

    read(mtin3,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    qfr(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

    close(mtin3)
    close(mtin2)
    close(mtin1)

    do j = 1, jmut
      do i = 1, imut
        if (atexl(i,j,1) /= 0.0d0) then
          qtot(i,j) = qsw(i,j) + qot(i,j) + qfr(i,j)
        else
          qtot(i,j) = -9.99e33
        end if
      end do
    end do

    write(flnin4,'(1a,i4.4)') trim(flnin_org4),nyear
    open(mtin4, file=flnin4, access='direct', recl=4*imut*jmut)

    write(flnin5,'(1a,i4.4)') trim(flnin_org5),nyear
    open(mtin5, file=flnin5, access='direct', recl=4*imut*jmut)

    write(flnin6,'(1a,i4.4)') trim(flnin_org6),nyear
    open(mtin6, file=flnin6, access='direct', recl=4*imut*jmut)

    write(flnin7,'(1a,i4.4)') trim(flnin_org7),nyear
    open(mtin7, file=flnin7, access='direct', recl=4*imut*jmut)

    write(flnin8,'(1a,i4.4)') trim(flnin_org8),nyear
    open(mtin8, file=flnin8, access='direct', recl=4*imut*jmut)

    write(flnin9,'(1a,i4.4)') trim(flnin_org9),nyear
    open(mtin9, file=flnin9, access='direct', recl=4*imut*jmut)

    write(flnin10,'(1a,i4.4)') trim(flnin_org10),nyear
    open(mtin10, file=flnin10, access='direct', recl=4*imut*jmut)

    !-----

    read(mtin4,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    qlw(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

    read(mtin5,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    qsn(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

    read(mtin6,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    qlt(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

    read(mtin7,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    qpr(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

    read(mtin8,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    qev(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

    read(mtin9,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    qrv(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

    read(mtin10,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    qit(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

    close(mtin10)
    close(mtin9)
    close(mtin8)
    close(mtin7)
    close(mtin6)
    close(mtin5)
    close(mtin4)

    !-----------------

    areag = 0.0d0

    qtotal = 0.0d0

    q_sw = 0.0d0
    q_lw = 0.0d0
    q_sn = 0.0d0
    q_lt = 0.0d0
    q_pr = 0.0d0
    q_ev = 0.0d0
    q_rv = 0.0d0
    q_it = 0.0d0

    do j = jbt, jet

      if (j == jet) then
        iettmp = imut / 2
      else
        iettmp = iet
      end if

      do i = ibt, iettmp

        if (atexl(i,j,1) > 0.0d0) then
          area_loc = aexl(i-1,j  ,1) * a_br(i-1,j  ) + aexl(i,j  ,1) * a_bl(i,j  ) &
               &   + aexl(i-1,j-1,1) * a_tr(i-1,j-1) + aexl(i,j-1,1) * a_tl(i,j-1)

          areag = areag + area_loc
          qtotal = qtotal + area_loc * qtot(i,j)
          q_sw = q_sw + area_loc * qsw(i,j)
          q_lw = q_lw + area_loc * qlw(i,j)
          q_sn = q_sn + area_loc * qsn(i,j)
          q_lt = q_lt + area_loc * qlt(i,j)
          q_pr = q_pr + area_loc * qpr(i,j)
          q_ev = q_ev + area_loc * qev(i,j)
          q_rv = q_rv + area_loc * qrv(i,j)
          q_it = q_it + area_loc * qit(i,j)
        end if
      end do

    end do

    !-----

    if (areag > 0.0d0) then
      qtotal = qtotal / areag
      q_sw = q_sw / areag
      q_lw = q_lw / areag
      q_sn = q_sn / areag
      q_lt = q_lt / areag
      q_pr = q_pr / areag
      q_ev = q_ev / areag
      q_rv = q_rv / areag
      q_it = q_it / areag
    else
      qtotal = 0.0d0
      q_sw = 0.0d0
      q_lw = 0.0d0
      q_sn = 0.0d0
      q_lt = 0.0d0
      q_pr = 0.0d0
      q_ev = 0.0d0
      q_rv = 0.0d0
      q_it = 0.0d0

    end if

    write(6,'(i8,1a,f10.5)') nyear, ' Global imbalance = ', qtotal

    !-------

    write(flnot1,'(1a,i4.4)') trim(flnot_base1),nyear
    write(*,*) ' GrADs data written to... ', trim(flnot1)
    open(mtot1,file=flnot1,form='unformatted',access='direct',recl=4)
    irec1 = 0

    irec1 = irec1 + 1
    write(mtot1,rec=irec1) real(qtotal,4)
    irec1 = irec1 + 1
    write(mtot1,rec=irec1) real(q_sw,4)
    irec1 = irec1 + 1
    write(mtot1,rec=irec1) real(q_lw,4)
    irec1 = irec1 + 1
    write(mtot1,rec=irec1) real(q_sn,4)
    irec1 = irec1 + 1
    write(mtot1,rec=irec1) real(q_lt,4)
    irec1 = irec1 + 1
    write(mtot1,rec=irec1) real(q_pr,4)
    irec1 = irec1 + 1
    write(mtot1,rec=irec1) real(q_ev,4)
    irec1 = irec1 + 1
    write(mtot1,rec=irec1) real(q_rv,4)
    irec1 = irec1 + 1
    write(mtot1,rec=irec1) real(q_it,4)

    close(mtot1)

  end do

  !----------

end program surface_heat_flux_component_annual
