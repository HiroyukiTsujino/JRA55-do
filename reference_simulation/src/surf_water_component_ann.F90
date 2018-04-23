! -*-F90-*-
!
!======================================================================
! Information:
!     calculate ice area and mean thickness
!----------------------------------------------------------------------
program surface_water_flux_component_annual

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

  real(8) :: wtot(imut,jmut)
  real(8) :: wpr(imut,jmut), wev(imut,jmut), wrv(imut,jmut), wic(imut,jmut)

  real(8) :: wtotal
  real(8) :: w_pr, w_ev, w_rv, w_ic
  real(8) :: areag

  !-----

  real(8) :: area_earth, area_this, area_loc

  !-----

  integer(4), parameter :: mtin1 = 71, mtin2 = 72, mtin3 = 73, mtin4 = 74, mtin5 = 75
  integer(4), parameter :: mtin6 = 76, mtin7 = 77, mtin8 = 78, mtin9 = 79, mtin10 = 80

  integer(4), parameter :: mtot1 = 81, mtot2 = 82, mtot3 = 83

  character(256) :: flnin_org1, flnin_org2, flnin_org3
  character(256) :: flnin_org4, flnin_org5, flnin_org6

  character(256) :: flnin1, flnin2, flnin3
  character(256) :: flnin4, flnin5, flnin6

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

  namelist /ioinfwcmp/ ftopo, fgrid, fscale, &
       & flnin_org1, flnin_org2, flnin_org3, &
       & flnin_org4, flnin_org5, &
       & flnot_base1, &
       & ibyr, ieyr

  open (11,file='ioinfwcmp.dat')
  read (11,nml=ioinfwcmp)
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

    write(flnin1,'(1a,i4.4)') trim(flnin_org1),nyear
    open(mtin1, file=flnin1, access='direct', recl=4*imut*jmut)

    write(flnin2,'(1a,i4.4)') trim(flnin_org2),nyear
    open(mtin2, file=flnin2, access='direct', recl=4*imut*jmut)

    write(flnin3,'(1a,i4.4)') trim(flnin_org3),nyear
    open(mtin3, file=flnin3, access='direct', recl=4*imut*jmut)

    write(flnin4,'(1a,i4.4)') trim(flnin_org4),nyear
    open(mtin4, file=flnin4, access='direct', recl=4*imut*jmut)

    write(flnin5,'(1a,i4.4)') trim(flnin_org5),nyear
    open(mtin5, file=flnin5, access='direct', recl=4*imut*jmut)

    !-----

    read(mtin1,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    wtot(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

    read(mtin2,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    wpr(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

    read(mtin3,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    wev(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

    read(mtin4,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    wrv(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

    read(mtin5,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    wic(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

    close(mtin5)
    close(mtin4)
    close(mtin3)
    close(mtin2)
    close(mtin1)

    !-----------------

    areag = 0.0d0

    wtotal = 0.0d0
    w_pr = 0.0d0
    w_ev = 0.0d0
    w_rv = 0.0d0
    w_ic = 0.0d0

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
          wtotal = wtotal + area_loc * wtot(i,j)
          w_pr = w_pr + area_loc * wpr(i,j)
          w_ev = w_ev + area_loc * wev(i,j)
          w_rv = w_rv + area_loc * wrv(i,j)
          w_ic = w_ic + area_loc * wic(i,j)
        end if
      end do

    end do

    !-----

    if (areag > 0.0d0) then
      wtotal = wtotal / areag
      w_pr = w_pr / areag
      w_ev = w_ev / areag
      w_rv = w_rv / areag
      w_ic = w_ic / areag
    else
      wtotal = 0.0d0
      w_pr = 0.0d0
      w_ev = 0.0d0
      w_rv = 0.0d0
      w_ic = 0.0d0

    end if

    write(6,'(i8,1a,d12.5)') nyear, ' Global imbalance = ', wtotal

    !-------

    write(flnot1,'(1a,i4.4)') trim(flnot_base1),nyear
    write(*,*) ' GrADs data written to... ', trim(flnot1)
    open(mtot1,file=flnot1,form='unformatted',access='direct',recl=4)
    irec1 = 0

    irec1 = irec1 + 1
    write(mtot1,rec=irec1) real(wtotal,4)
    irec1 = irec1 + 1
    write(mtot1,rec=irec1) real(w_pr,4)
    irec1 = irec1 + 1
    write(mtot1,rec=irec1) real(w_ev,4)
    irec1 = irec1 + 1
    write(mtot1,rec=irec1) real(w_rv,4)
    irec1 = irec1 + 1
    write(mtot1,rec=irec1) real(w_ic,4)

    close(mtot1)

  end do

  !----------

end program surface_water_flux_component_annual
