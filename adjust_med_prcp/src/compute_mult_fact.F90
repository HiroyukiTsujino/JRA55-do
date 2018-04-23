!-*-F90-*-
!======================================================================
! Information:
!     calculate total water flux
!----------------------------------------------------------------------
program compute_multiplication_factor

  use file_open_close_manager

  implicit none

  integer(4), parameter :: nmon = 12

  integer(4) :: imut, jmut

  integer(4) :: ibu, ieu, jbu, jeu
  integer(4) :: ibt, iet, jbt, jet

  real(8) :: wtotal
  real(8) :: prcp_integ
  real(8) :: prcp_annual
  real(8) :: area_region

  real(8) :: prcp_land_clim
  real(8) :: prcp_region_clim

  real(8),allocatable :: prcp_land(:)
  real(8),allocatable :: prcp_core(:)
  real(8),allocatable :: prcp_estimate(:)
  real(8),allocatable :: prcp_jra(:)
  real(8),allocatable :: prcp_fac(:)

  real(4) :: work_tmp
  real(4) :: undef_in, undef_out

  !-----

  integer(4), parameter :: mtin1 = 71, mtin2 = 72, mtin3 = 73, mtin4 = 74
  integer(4), parameter :: mtot1 = 81, mtot2 = 82, mtot3 = 83, mtot4 = 84
  character(256) :: flnin_base1, flnin_base2, flnin_base3, flnin_base4
  character(256) :: flnot_base1, flnot_base2, flnot_base3, flnot_base4
  character(256) :: flnin1, flnin2, flnin3, flnin4
  character(256) :: flnot1, flnot2, flnot3, flnot4

  integer(4) :: nday, month, nyear, nd, day_mon, total_day
  logical :: l_leap

  integer(4) :: irec1, irec2, irec3, irec4
  integer(4) :: i, j, k, m
  integer(4) :: ibyr, ieyr
  integer(4) :: ibyr_cl, ieyr_cl
  integer(4) :: ibyr_fac, ieyr_fac

  !-----------------------------------------------------------------------

  namelist /nml_precip_mult_fac/ &
       & flnin_base1,  &
       & flnin_base2,  &
       & flnin_base3,  &
       & flnot_base1, &
       & area_region,   &
       & ibyr, ieyr,  &
       & ibyr_cl, ieyr_cl, &
       & ibyr_fac, ieyr_fac

  !-----------------------------------------------------------------------

  open (11,file='namelist.precip_mult_fac')
  read (11,nml=nml_precip_mult_fac)
  close(11)

  !-----------------------------------------------------------------------
  ! set grid points

  allocate (prcp_land(ibyr:ieyr))
  allocate (prcp_core(ibyr:ieyr))
  allocate (prcp_estimate(ibyr:ieyr))
  allocate (prcp_jra(ibyr:ieyr))
  allocate (prcp_fac(ibyr-1:ieyr+1))

  !---------------------------------------------------

  write(6,'(1a,E20.10)') ' area region = ', area_region

  !-----------------------------------------------------------------------

  write(flnin2,'(1a)') trim(flnin_base2)
  write(*,*) ' GrADs data read from... ', trim(flnin2)
  open(mtin2,file=flnin2,form='unformatted',access='direct', &
       & action='read',recl=4)
  irec2 = 0

  write(flnin3,'(1a)') trim(flnin_base3)
  write(*,*) ' GrADs data read from... ', trim(flnin3)
  open(mtin3,file=flnin3,form='unformatted',access='direct', &
       & action='read',recl=4)
  irec3 = 0

  do nyear = ibyr, ieyr

    write(6,*) nyear

    prcp_annual = 0.0d0
    total_day = 0

    write(flnin1,'(1a,i4.4)') trim(flnin_base1),nyear
    write(*,*) ' GrADs data read from... ', trim(flnin1)
    open(mtin1,file=flnin1,form='unformatted',access='direct', &
         & action='read',recl=4)
    irec1 = 0
    irec1 = irec1 + 1
    read(mtin1,rec=irec1) work_tmp
    prcp_land(nyear) = real(work_tmp,4)
    close(mtin1)

    irec2 = irec2 + 1
    read(mtin2,rec=irec2) work_tmp
    prcp_core(nyear) = real(work_tmp,4) * area_region

    irec3 = irec3 + 1
    read(mtin3,rec=irec3) work_tmp
    prcp_jra(nyear) = real(work_tmp,4) * area_region

  end do

  prcp_land_clim = 0.0d0
  prcp_region_clim = 0.0d0

  do nyear = ibyr_cl, ieyr_cl
    prcp_land_clim = prcp_land_clim + prcp_land(nyear)
    prcp_region_clim = prcp_region_clim + prcp_core(nyear)
  end do

  prcp_land_clim = prcp_land_clim / real(ieyr_cl - ibyr_cl + 1,4)
  prcp_region_clim = prcp_region_clim / real(ieyr_cl - ibyr_cl + 1,4)

  prcp_fac(:) = 1.0d0
  do nyear = ibyr - 1, ieyr + 1
    prcp_estimate(nyear) = prcp_land(nyear)*prcp_region_clim/prcp_land_clim

    if ((ibyr_fac <= nyear) .and. (nyear <= ieyr_fac)) then
      prcp_fac(nyear) = prcp_estimate(nyear) / prcp_jra(nyear)
    end if

  end do

  prcp_fac(ibyr_fac-1) = prcp_fac(ibyr_fac)

  !------

  write(flnot1,'(1a)') trim(flnot_base1)
  write(*,*) ' GrADs data written to... ', trim(flnot1)
  open(mtot1,file=flnot1,form='unformatted',access='direct', &
       & action='write',recl=4)
  irec4 = 0

  do nyear = ibyr-1, ieyr+1

    write(6,'(i6,4f12.5)') nyear, &
         & prcp_estimate(nyear)*1.0d-9, &
         & prcp_core(nyear)*1.0d-9, prcp_jra(nyear)*1.0d-9, prcp_fac(nyear)
    irec4 = irec4 + 1
    write(mtot1,rec=irec4) real(prcp_fac(nyear),4)

  end do

  !----------

end program compute_multiplication_factor
