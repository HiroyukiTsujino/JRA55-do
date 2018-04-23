! -*-F90-*-
!
!======================================================================
! Information:
!     calculate ice area and mean thickness
!----------------------------------------------------------------------
program surface_effective_heat_flux_monthly

  use basin_param
  use grid_common

  implicit none

  integer(4), parameter :: nmon = 12

  integer(4), parameter :: ibu = 3, ieu = imut - 2  ! cyclic
  integer(4), parameter :: jbu = 2, jeu = jmut - 3  ! tripolar

  integer(4), parameter :: ibt = 3, iet = imut - 2  ! cyclic
  integer(4), parameter :: jbt = 2, jet = jmut - 2  ! tripolar

  real(4) :: work4(imut,jmut)

  real(8) :: qot(imut,jmut), qsw(imut,jmut), qfr(imut,jmut)
  real(8) :: qtot(imut,jmut), qwfl(imut,jmut)
  real(8) :: wtot(imut,jmut)

  real(8) :: sst(imut,jmut)

  !-----

  integer(4), parameter :: mtin1 = 71, mtin2 = 72, mtin3 = 73
  integer(4), parameter :: mtin4 = 74, mtin5 = 75, mtin6 = 76
  integer(4), parameter :: mtin7 = 77
  integer(4), parameter :: mtot1 = 81, mtot2 = 82, mtot3 = 83
  character(256) :: flnin_org1, flnin_org2, flnin_org3
  character(256) :: flnin_org4, flnin_org5, flnin_org6
  character(256) :: flnin_org7
  character(256) :: flnot_base1, flnot_base2, flnot_base3
  character(256) :: flnin1, flnin2, flnin3
  character(256) :: flnin4, flnin5, flnin6, flnin7
  character(256) :: flnot1, flnot2, flnot3
  character(256) :: ftopo, fgrid, fscale, fbasmap

  integer(4) :: nday, month, nyear, nd, mon
  integer(4), parameter :: ndata = 12
  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  integer(4) :: irec1, irec2, irec3
  integer(4) :: i, j, k, m
  integer(4) :: ibyr, ieyr, mon

  real(8) :: undef_out = 0.0

  !-----------------------------------------------------------------------

  namelist /ioinfehflx/ ftopo, fgrid, fscale,  &
       & flnin_org1, flnin_org2, flnin_org3,   &
       & flnin_org4, flnin_org5,               &
       & flnot_base1, flnot_base2, undef_out,  &
       & ibyr, ieyr

  open (11,file='ioinfehflx.dat')
  read (11,nml=ioinfehflx)
  close(11)

  !-----------------------------------------------------------------------
  ! set grid points

  call setgrd(ftopo, fgrid, fscale)

  !-----------------------------------------------------------------------
  ! read temperature and salinity

  do nyear = ibyr, ieyr
  do mon = 1, 12

    write(6,*) nyear, mon

    write(flnin1,'(1a,i4.4,i2.2)') trim(flnin_org1),nyear,mon
    open(mtin1, file=flnin1, access='direct', recl=4*imut*jmut)

    write(flnin2,'(1a,i4.4,i2.2)') trim(flnin_org2),nyear,mon
    open(mtin2, file=flnin2, access='direct', recl=4*imut*jmut)

! for yr 1241-1300
    write(flnin3,'(1a,i4.4,i2.2)') trim(flnin_org3),nyear,mon
    open(mtin3, file=flnin3, access='direct', recl=4*imut*jmut)

! for yr 1001-1240
!    write(flnin3,'(1a,i4.4)') trim(flnin_org3),nyear
!    open(mtin3, file=flnin3, access='direct', recl=4*imut*jmut)

    write(flnin4,'(1a,i4.4,i2.2)') trim(flnin_org4),nyear,mon
    open(mtin4, file=flnin4, access='direct', recl=4*imut*jmut)

    write(flnin5,'(1a,i4.4,i2.2)') trim(flnin_org5),nyear,mon
    open(mtin5, file=flnin5, access='direct', recl=4*imut*jmut)

    read(mtin1,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    qot(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

    read(mtin2,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    qsw(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

    read(mtin3,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    qfr(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

    read(mtin4,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    wtot(1:imut,1:jmut) = dble(work4(1:imut,1:jmut)) * 1.0d-2

    read(mtin5,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    sst(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

    close(mtin5)
    close(mtin4)
    close(mtin3)
    close(mtin2)
    close(mtin1)

    !------------------------------------------------

    write(flnot1,'(1a,i4.4,i2.2)') trim(flnot_base1),nyear,mon
    write(*,*) ' GrADs data written to... ', trim(flnot1)
    open(mtot1,file=flnot1,form='unformatted',access='direct',recl=4*imut*jmut)
    irec1 = 0

    write(flnot2,'(1a,i4.4,i2.2)') trim(flnot_base2),nyear,mon
    write(*,*) ' GrADs data written to... ', trim(flnot2)
    open(mtot2,file=flnot2,form='unformatted',access='direct',recl=4*imut*jmut)
    irec2 = 0

    !------------------------------------------------

    do j = 1, jmut
      do i = 1, imut
        if (atexl(i,j,1) /= 0.0d0) then
          qtot(i,j) = qsw(i,j) + qot(i,j) + qfr(i,j) - wtot(i,j) * sst(i,j) * ro * cp
          qwfl(i,j) = wtot(i,j) * sst(i,j) * ro * cp
        else
          qtot(i,j) = undef_out
          qwfl(i,j) = undef_out
        end if
      end do
    end do

    irec1 = irec1 + 1
    write(mtot1,rec=irec1) real(qtot(1:imut,1:jmut),4)

    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(qwfl(1:imut,1:jmut),4)

  end do
  end do

  !----------

end program surface_effective_heat_flux_monthly
