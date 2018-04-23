! -*-F90-*-
!
!======================================================================
! Information:
!     calculate ice area and mean thickness
!----------------------------------------------------------------------
program ice_area_thickness

  use basin_param
  use grid_common

  implicit none

  integer(4), parameter :: ibu = 3, ieu = imut - 2  ! cyclic
  integer(4), parameter :: jbu = 2, jeu = jmut - 3  ! tripolar

  integer(4), parameter :: ibt = 3, iet = imut - 2  ! cyclic
  integer(4), parameter :: jbt = 2, jet = jmut - 2  ! tripolar

  integer(4) :: iettmp

  real(4) :: work4(imut,jmut)

  real(8) :: ar(imut,jmut), hi(imut,jmut), hs(imut,jmut)

  real(8) :: areag, areaarc
  real(8) :: area_earth, area_this

  !-----

  real(8) :: volice_n,  volsnw_n
  real(8) :: areaice_n, extentice_n
  real(8) :: aveice_n,  avesnw_n

  real(8) :: volice_s,  volsnw_s
  real(8) :: areaice_s, extentice_s
  real(8) :: aveice_s,  avesnw_s

  real(8) :: volice_ann_n,  volsnw_ann_n
  real(8) :: areaice_ann_n, extentice_ann_n
  real(8) :: aveice_ann_n,  avesnw_ann_n

  real(8) :: volice_ann_s,  volsnw_ann_s
  real(8) :: areaice_ann_s, extentice_ann_s
  real(8) :: aveice_ann_s,  avesnw_ann_s

  !-----

  integer(4), parameter :: mtin1 = 71, mtin2 = 72, mtot1 = 81, mtot2 = 82
  character(256) :: flnin, flnot, flnav
  character(256) :: flnin_base, flnot_base, flnav_base
  character(256) :: ftopo, fgrid, fscale

  integer(4) :: nday, month, nyear, nd
  integer(4), parameter :: ndata = 12
  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  real(8), parameter :: alats = -10.0d0, alatn = 10.d0
  real(8), parameter :: alonw =   0.0d0, alone = 360.d0
  real(8), parameter :: amin = 0.15d0

  integer(4) :: irec1, irec2, irec3
  integer(4) :: i, j, k, m
  integer(4) :: nbyr, neyr

  logical :: l_percent = .true.

  !-----------------------------------------------------------------------

  namelist /ioinfic/ flnin_base, flnot_base, flnav_base, ftopo, fgrid, fscale, nbyr, neyr, l_percent
  open (11,file='ioinfic.dat')
  read (11,nml=ioinfic)
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

!  write(*,*) ' GrADs data written to... ', trim(flnot)
!  open(mtot1,file=flnot,form='unformatted',access='direct',recl=4)
!  irec1 = 0

!  write(*,*) ' GrADs data written to... ', trim(flnav)
!  open(mtot2,file=flnav,form='unformatted',access='direct',recl=4)
!  irec2 = 0

  ! read temperature and salinity

  do nyear = nbyr, neyr

    volice_ann_n    = 0.0d0
    volsnw_ann_n    = 0.0d0
    areaice_ann_n   = 0.0d0
    extentice_ann_n = 0.0d0
    aveice_ann_n    = 0.0d0
    avesnw_ann_n    = 0.0d0

    volice_ann_s    = 0.0d0
    volsnw_ann_s    = 0.0d0
    areaice_ann_s   = 0.0d0
    extentice_ann_s = 0.0d0
    aveice_ann_s    = 0.0d0
    avesnw_ann_s    = 0.0d0

    do nd = 1, 12

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

      close(mtin1)

      !-----------------

      volice_n    = 0.0d0
      volsnw_n    = 0.0d0
      areaice_n   = 0.0d0
      extentice_n = 0.0d0

      volice_s    = 0.0d0
      volsnw_s    = 0.0d0
      areaice_s   = 0.0d0
      extentice_s = 0.0d0

      do j = jbt, jet
        if (alatt(j) >= alatn) then
          if (j == jet) then
            iettmp = imut / 2
          else
            iettmp = iet
          end if
          do i = ibt, iettmp
            if (ar(i,j) >= amin) then
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
            if (ar(i,j) >= amin) then
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

      volice_ann_n    = volice_ann_n    + volice_n * real(ndmon(month),8)
      volsnw_ann_n    = volsnw_ann_n    + volsnw_n * real(ndmon(month),8)
      areaice_ann_n   = areaice_ann_n   + areaice_n * real(ndmon(month),8)
      extentice_ann_n = extentice_ann_n + extentice_n * real(ndmon(month),8)
      aveice_ann_n    = aveice_ann_n    + aveice_n * real(ndmon(month),8)
      avesnw_ann_n    = avesnw_ann_n    + avesnw_n * real(ndmon(month),8)

      volice_ann_s    = volice_ann_s    + volice_s * real(ndmon(month),8)
      volsnw_ann_s    = volsnw_ann_s    + volsnw_s * real(ndmon(month),8)
      areaice_ann_s   = areaice_ann_s   + areaice_s * real(ndmon(month),8)
      extentice_ann_s = extentice_ann_s + extentice_s * real(ndmon(month),8)
      aveice_ann_s    = aveice_ann_s    + aveice_s * real(ndmon(month),8)
      avesnw_ann_s    = avesnw_ann_s    + avesnw_s * real(ndmon(month),8)

      write(flnot,'(1a,i4.4,i2.2)') trim(flnot_base),nyear,month
      open(mtot1, file=flnot, access='direct', recl=4)
      write(*,*) ' GrADs data written to... ', trim(flnot)
      irec1 = 0

      irec1 = irec1 + 1
      write(mtot1,rec=irec1) real(areaice_n,4)
      irec1 = irec1 + 1
      write(mtot1,rec=irec1) real(extentice_n,4)
      irec1 = irec1 + 1
      write(mtot1,rec=irec1) real(volice_n,4)
      irec1 = irec1 + 1
      write(mtot1,rec=irec1) real(volsnw_n,4)
      irec1 = irec1 + 1
      write(mtot1,rec=irec1) real(aveice_n,4)
      irec1 = irec1 + 1
      write(mtot1,rec=irec1) real(avesnw_n,4)

      irec1 = irec1 + 1
      write(mtot1,rec=irec1) real(areaice_s,4)
      irec1 = irec1 + 1
      write(mtot1,rec=irec1) real(extentice_s,4)
      irec1 = irec1 + 1
      write(mtot1,rec=irec1) real(volice_s,4)
      irec1 = irec1 + 1
      write(mtot1,rec=irec1) real(volsnw_s,4)
      irec1 = irec1 + 1
      write(mtot1,rec=irec1) real(aveice_s,4)
      irec1 = irec1 + 1
      write(mtot1,rec=irec1) real(avesnw_s,4)

      close(mtot1)

      !write(6,'(i4,i2,E20.10,E20.10,E20.10,F12.6,F12.6)') &
      !     & nyear, month, areaice, volice, volsnw, aveice, avesnw

    end do

    volice_ann_n    = volice_ann_n    / 365.0d0
    volsnw_ann_n    = volsnw_ann_n    / 365.0d0
    areaice_ann_n   = areaice_ann_n   / 365.0d0
    extentice_ann_n = extentice_ann_n / 365.0d0
    aveice_ann_n    = aveice_ann_n    / 365.0d0
    avesnw_ann_n    = avesnw_ann_n    / 365.0d0

    volice_ann_s    = volice_ann_s    / 365.0d0
    volsnw_ann_s    = volsnw_ann_s    / 365.0d0
    areaice_ann_s   = areaice_ann_s   / 365.0d0
    extentice_ann_s = extentice_ann_s / 365.0d0
    aveice_ann_s    = aveice_ann_s    / 365.0d0
    avesnw_ann_s    = avesnw_ann_s    / 365.0d0

    write(flnav,'(1a,i4.4)') trim(flnav_base),nyear
    open(mtot2, file=flnav, access='direct', recl=4)
    write(*,*) ' GrADs data written to... ', trim(flnav)
    irec2 = 0

    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(areaice_ann_n,4)
    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(extentice_ann_n,4)
    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(volice_ann_n,4)
    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(volsnw_ann_n,4)
    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(aveice_ann_n,4)
    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(avesnw_ann_n,4)

    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(areaice_ann_s,4)
    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(extentice_ann_s,4)
    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(volice_ann_s,4)
    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(volsnw_ann_s,4)
    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(aveice_ann_s,4)
    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(avesnw_ann_s,4)

    close(mtot2)

  end do

end program ice_area_thickness
