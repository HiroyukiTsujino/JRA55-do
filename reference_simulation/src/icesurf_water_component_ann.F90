! -*-F90-*-
!
!======================================================================
! Information:
!     calculate ice area and mean thickness
!----------------------------------------------------------------------
program icesurf_water_flux_component_annual

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

  real(8) :: wpr(imut,jmut), wsb(imut,jmut)
  real(8) :: wpr_tmp(imut,jmut), wsb_tmp(imut,jmut)

  real(8) :: w_pr, w_sb
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
  integer(4), parameter :: monyr = 12
  integer(4), parameter :: ndyr = 365
  integer(4) :: ndmon(monyr) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  integer(4) :: irec1, irec2, irec3
  integer(4) :: i, j, k, m
  integer(4) :: ibyr, ieyr

  integer(4) :: irec_pr, irec_sb

  !-----------------------------------------------------------------------

  namelist /ioinfiwcmp/ ftopo, fgrid, fscale, &
       & flnin_org1, irec_pr, irec_sb, &
       & flnot_base1, &
       & ibyr, ieyr

  open (11,file='ioinfiwcmp.dat')
  read (11,nml=ioinfiwcmp)
  close(11)

  !-----------------------------------------------------------------------
  ! set grid points

  call setgrd(ftopo, fgrid, fscale)

  !----

  areag = 0.0d0
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

    wpr(:,:) = 0.0d0
    wsb(:,:) = 0.0d0

    do month = 1, monyr

      write(flnin1,'(1a,i4.4,i2.2)') trim(flnin_org1),nyear,month
      open(mtin1, file=flnin1, access='direct', recl=4*imut*jmut)

      read(mtin1,rec=irec_pr) ((work4(i,j),i=1,imut),j=1,jmut)
      wpr_tmp(1:imut,1:jmut) = real(work4(1:imut,1:jmut),4) ! [kg/m2/s]
!      wpr_tmp(1:imut,1:jmut) = real(work4(1:imut,1:jmut),4) / ro * 1.0d2 ! [kg/m2/s] -> [cm/s] of sea water

      read(mtin1,rec=irec_sb) ((work4(i,j),i=1,imut),j=1,jmut)
      wsb_tmp(1:imut,1:jmut) = real(work4(1:imut,1:jmut),4) ! [kg/m2/s]
!      wsb_tmp(1:imut,1:jmut) = real(work4(1:imut,1:jmut),4) / ro * 1.0d2 ! [kg/m2/s] -> [cm/s] of sea water

      close(mtin1)

      wpr(1:imut,1:jmut) = wpr(1:imut,1:jmut) + atexl(1:imut,1:jmut,1) * wpr_tmp(1:imut,1:jmut) * real(ndmon(month),8)
      wsb(1:imut,1:jmut) = wsb(1:imut,1:jmut) + atexl(1:imut,1:jmut,1) * wsb_tmp(1:imut,1:jmut) * real(ndmon(month),8)

    end do

    wpr(1:imut,1:jmut) = wpr(1:imut,1:jmut) / real(ndyr,8)
    wsb(1:imut,1:jmut) = wsb(1:imut,1:jmut) / real(ndyr,8)

    !-----------------

    areag = 0.0d0

    w_pr = 0.0d0
    w_sb = 0.0d0

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
          w_pr = w_pr + area_loc * wpr(i,j)
          w_sb = w_sb + area_loc * wsb(i,j)
        end if
      end do

    end do

    !-----

    if (areag > 0.0d0) then
      w_pr = w_pr / areag
      w_sb = w_sb / areag
    else
      w_pr = 0.0d0
      w_sb = 0.0d0
    end if

    !-------

    write(flnot1,'(1a,i4.4)') trim(flnot_base1),nyear
    write(*,*) ' GrADs data written to... ', trim(flnot1)
    open(mtot1,file=flnot1,form='unformatted',access='direct',recl=4)
    irec1 = 0

    irec1 = irec1 + 1
    write(mtot1,rec=irec1) real(w_pr,4)
    irec1 = irec1 + 1
    write(mtot1,rec=irec1) real(w_sb,4)

    close(mtot1)

  end do

  !----------

end program icesurf_water_flux_component_annual
