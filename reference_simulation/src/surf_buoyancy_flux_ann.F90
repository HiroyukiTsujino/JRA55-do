! -*-F90-*-
!
!======================================================================
! Information:
!     calculate ice area and mean thickness
!----------------------------------------------------------------------
program surface_buoyancy_flux_annual

  use basin_param
  use grid_common
  use state_equations

  implicit none

  integer(4), parameter :: nmon = 12

  integer(4), parameter :: ibu = 3, ieu = imut - 2  ! cyclic
  integer(4), parameter :: jbu = 2, jeu = jmut - 3  ! tripolar

  integer(4), parameter :: ibt = 3, iet = imut - 2  ! cyclic
  integer(4), parameter :: jbt = 2, jet = jmut - 2  ! tripolar

  real(4) :: work4(imut,jmut)

  real(8) :: qlw(imut,jmut), qsw(imut,jmut)
  real(8) :: qla(imut,jmut), qse(imut,jmut), qic(imut,jmut)
  real(8) :: qtot(imut,jmut)
  real(8) :: wtot(imut,jmut), stot(imut,jmut)

  real(8) :: sst(imut,jmut), sss(imut,jmut)

  real(8) :: tflx(imut,jmut)
  real(8) :: sflx(imut,jmut)
  real(8) :: bflx(imut,jmut)

  real(8) :: drdt, drds, tc, sc, pc

  !-----

  integer(4), parameter :: mtin1 = 71, mtin2 = 72, mtin3 = 73
  integer(4), parameter :: mtin4 = 74, mtin5 = 75, mtin6 = 76
  integer(4), parameter :: mtin7 = 77, mtin8 = 78, mtin9 = 79
  integer(4), parameter :: mtot1 = 81, mtot2 = 82, mtot3 = 83

  character(256) :: flnin_org1, flnin_org2, flnin_org3
  character(256) :: flnin_org4, flnin_org5, flnin_org6
  character(256) :: flnin_org7, flnin_org8, flnin_org9

  character(256) :: flnot_base1, flnot_base2, flnot_base3

  character(256) :: flnin1, flnin2, flnin3
  character(256) :: flnin4, flnin5, flnin6
  character(256) :: flnin7, flnin8, flnin9
  character(256) :: flnot1, flnot2, flnot3
  character(256) :: ftopo, fgrid, fscale, fbasmap

  integer(4), parameter :: mteff = 91
  character(256) :: flnin_eff_org, flnin_eff
  logical :: l_use_eff
  real(8) :: qeff(imut,jmut)

  integer(4) :: nday, month, nyear, nd
  integer(4), parameter :: ndata = 12
  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  integer(4) :: irec1, irec2, irec3
  integer(4) :: i, j, k, m
  integer(4) :: ibyr, ieyr

  real(8) :: undef_out = 0.0

  !-----------------------------------------------------------------------

  namelist /ioinfbflx/ ftopo, fgrid, fscale,  &
       & flnin_org1, flnin_org2, flnin_org3,  &
       & flnin_org4, flnin_org5, flnin_org6,  &
       & flnin_org7, flnin_org8, flnin_org9,  &
       & flnin_eff_org, l_use_eff,   &
       & flnot_base1, undef_out, &
       & ibyr, ieyr

  open (11,file='ioinfbflx.dat')
  read (11,nml=ioinfbflx)
  close(11)

  !-----------------------------------------------------------------------
  ! set grid points

  call setgrd(ftopo, fgrid, fscale)

  !-----------------------------------------------------------------------
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

    write(flnin6,'(1a,i4.4)') trim(flnin_org6),nyear
    open(mtin6, file=flnin6, access='direct', recl=4*imut*jmut)

    write(flnin7,'(1a,i4.4)') trim(flnin_org7),nyear
    open(mtin7, file=flnin7, access='direct', recl=4*imut*jmut)

    write(flnin8,'(1a,i4.4)') trim(flnin_org8),nyear
    open(mtin8, file=flnin8, access='direct', recl=4*imut*jmut)

    write(flnin9,'(1a,i4.4)') trim(flnin_org9),nyear
    open(mtin9, file=flnin9, access='direct', recl=4*imut*jmut)

    read(mtin1,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    qsw(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

    read(mtin2,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    qlw(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

    read(mtin3,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    qla(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

    read(mtin4,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    qse(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

    read(mtin5,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    qic(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

    read(mtin6,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    do j = 1, jmut
      do i = 1, imut
        if (atexl(i,j,1) /= 0.0d0) then
          wtot(i,j) = work4(i,j) * 1.0d-2
        else
          wtot(i,j) = undef_out
        end if
      end do
    end do

    read(mtin7,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    do j = 1, jmut
      do i = 1, imut
        if (atexl(i,j,1) /= 0.0d0) then
          stot(i,j) = work4(i,j) * 1.0d-2
        else
          stot(i,j) = undef_out
        end if
      end do
    end do

    read(mtin8,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    sst(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

    read(mtin9,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    sss(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))

    close(mtin9)
    close(mtin8)
    close(mtin7)
    close(mtin6)
    close(mtin5)
    close(mtin4)
    close(mtin3)
    close(mtin2)
    close(mtin1)

    if (l_use_eff) then
      write(flnin_eff,'(1a,i4.4)') trim(flnin_eff_org),nyear
      open(mteff, file=flnin_eff, access='direct', recl=4*imut*jmut)
      read(mteff,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
      qeff(1:imut,1:jmut) = dble(work4(1:imut,1:jmut))
    end if

    !-------

    write(flnot1,'(1a,i4.4)') trim(flnot_base1),nyear
    write(*,*) ' GrADs data written to... ', trim(flnot1)
    open(mtot1,file=flnot1,form='unformatted',access='direct',recl=4*imut*jmut)
    irec1 = 0

!    write(flnot2,'(1a,i4.4)') trim(flnot_base2),nyear
!    write(*,*) ' GrADs data written to... ', trim(flnot2)
!    open(mtot2,file=flnot2,form='unformatted',access='direct',recl=4*imut*jmut)
!    irec2 = 0

    !------------------------------------------------

    do j = 1, jmut
      do i = 1, imut
        if (atexl(i,j,1) /= 0.0d0) then
          if (l_use_eff) then
            qtot(i,j) = qeff(i,j)
          else
            qtot(i,j) = qsw(i,j) + qlw(i,j) + qla(i,j) + qse(i,j) + qic(i,j)
          end if
          tflx(i,j) = qtot(i,j) / (ro * cp)
          sflx(i,j) = stot(i,j) - wtot(i,j) * sss(i,j)
        else
          qtot(i,j) = undef_out
          tflx(i,j) = undef_out
          sflx(i,j) = undef_out
        end if
      end do
    end do

    do j = 1, jmut
      do i = 1, imut
        if (atexl(i,j,1) /= 0.0d0) then
          tc = sst(i,j)
          sc = sss(i,j)
          drdt = alpha_state(tc,sc)
          drds = beta_state (tc,sc)
          bflx(i,j) = - grav / ro * (drdt * tflx(i,j) + drds * sflx(i,j))
        else
          bflx(i,j) = undef_out
        end if
      end do
    end do

    irec1 = irec1 + 1
    write(mtot1,rec=irec1) real(bflx(1:imut,1:jmut),4)

    irec1 = irec1 + 1
    write(mtot1,rec=irec1) real(qtot(1:imut,1:jmut),4)

    irec1 = irec1 + 1
    write(mtot1,rec=irec1) real(sflx(1:imut,1:jmut),4)

  end do

  !----------

end program surface_buoyancy_flux_annual
