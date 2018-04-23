!-*-F90-*-
!
!======================================================================
! Information:
!     calculate total water flux
!----------------------------------------------------------------------
program surface_water_flux_monthly

  use libmxe_para, only: libmxe_para__register, clen, rho &
                     & , pi, radius, radian, radian_r, type_libmxe_para
  use libmxe_grid, only: libmxe_grid__register  &
                     & , type_libmxe_grid

  use file_open_close_manager

  implicit none

  integer(4), parameter :: nmon = 12

  integer(4) :: imut, jmut

  integer(4) :: ibu, ieu, jbu, jeu
  integer(4) :: ibt, iet, jbt, jet

  real(4),allocatable :: work4(:,:)
  real(4),allocatable :: ibas4(:,:)

  real(8),allocatable :: pcp(:,:), pcp_all(:,:)
  real(8),allocatable :: wtot(:,:)
  real(8),allocatable :: mask(:,:)
  real(8),allocatable :: mask_tmp(:,:)
  real(8),allocatable :: mask_wide(:,:)
  real(8),allocatable :: favail(:,:)

  real(8) :: wtotal
  real(8) :: prcp_integ
  real(8) :: prcp_annual
  real(8) :: area_ocean, area_tmp
  real(8) :: area_earth, area_this, hl1

  real(4) :: undef_in, undef_out

  !-----

  integer(4), parameter :: mtin1 = 71, mtin2 = 72, mtin3 = 73, mtin4 = 74
  integer(4), parameter :: mtot1 = 81, mtot2 = 82, mtot3 = 83, mtot4 = 84
  character(256) :: flnin_org1, flnin_org2, flnin_org3, flnin_org4
  character(256) :: flnot_base1, flnot_base2, flnot_base3, flnot_base4
  character(256) :: flnin1, flnin2, flnin3, flnin4
  character(256) :: flnot1, flnot2, flnot3, flnot4

  character(256) :: file_mask, file_avail

  integer(4) :: nday, month, nyear, nd, day_mon, total_day
  integer(4), parameter :: ndata = 12
  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)
  integer(4), parameter :: nsday = 86400
  logical :: l_leap

  integer(4) :: irec1, irec2, irec3, irec4
  integer(4) :: i, j, k, m
  integer(4) :: ibyr, ieyr

  type(type_libmxe_para) :: para
  type(type_libmxe_grid) :: grid

  !-----------------------------------------------------------------------

  namelist /nml_precip_integ/ &
       & flnin_org1,  &
       & flnot_base1, &
       & flnot_base2, &
       & file_mask, file_avail, &
       & undef_in, undef_out, &
       & ibyr, ieyr

  !-----------------------------------------------------------------------

  open (11,file='namelist.precip_integ')
  read (11,nml=nml_precip_integ)
  close(11)

  !-----------------------------------------------------------------------
  ! set grid points

  !-- model settings --

  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)

  imut = para%imut
  jmut = para%jmut

  ibu = 1
  ieu = imut
  jbu = 1
  jeu = jmut

  allocate(work4(1:imut,1:jmut))
  allocate(ibas4(1:imut,1:jmut))

  allocate(pcp(1:imut,1:jmut))
  allocate(pcp_all(1:imut,1:jmut))
  allocate(wtot(1:imut,1:jmut))

  allocate(mask(1:imut,1:jmut))
  allocate(mask_wide(1:imut,1:jmut))
  allocate(favail(1:imut,1:jmut))

  !----------------------------------------------------------
  !  mask

  open(mtin1, file=file_mask, access='direct', recl=4*imut*jmut)
  read(mtin1,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
  close(mtin1)

  mask(1:imut,1:jmut) = real(work4(1:imut,1:jmut),8)

  allocate(mask_tmp(0:imut+1,0:jmut+1))

  mask_tmp(1:imut,1:jmut) = mask(1:imut,1:jmut)
  mask_tmp(0     ,1:jmut) = mask(imut,1:jmut)
  mask_tmp(imut+1,1:jmut) = mask(   1,1:jmut)
  mask_tmp(0:imut+1,0)      = mask(0:imut+1,1)
  mask_tmp(0:imut+1,jmut+1) = mask(0:imut+1,jmut)

  do j = jbu, jeu
    do i = ibu, ieu
      hl1 = mask_tmp(i,j) + mask_tmp(i-1,j) + mask_tmp(i+1,j) &
         & + mask_tmp(i,j-1) + mask_tmp(i-1,j-1) + mask_tmp(i+1,j-1) &
         & + mask_tmp(i,j+1) + mask_tmp(i-1,j+1) + mask_tmp(i+1,j+1)
      if (hl1 > 0.0) then
        mask_wide(i,j) = 1.0d0
      else
        mask_wide(i,j) = 0.0d0
      end if
    end do
  end do

  open(mtin2, file=file_avail, access='direct', recl=4*imut*jmut)
  read(mtin2,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
  close(mtin2)

  favail(1:imut,1:jmut) = real(work4(1:imut,1:jmut),8)

  !---------------------------------------------------

  area_this = 0.0d0

  do j = jbu, jeu
    do i = ibu, ieu
      area_tmp = grid%a_br(i,j) + grid%a_bl(i,j) + grid%a_tr(i,j) + grid%a_tl(i,j)
      area_this = area_this + favail(i,j) * mask_wide(i,j) * area_tmp
    end do
  end do

  area_earth = 4.0d0 * pi * radius * radius

  write(6,'(1a,E20.10)') ' area earth = ', area_earth
  write(6,'(1a,E20.10)') ' area this  = ', area_this

  !-----------------------------------------------------------------------

  do nyear = ibyr, ieyr

    write(6,*) nyear

    prcp_annual = 0.0d0
    total_day = 0

    write(flnot2,'(1a,i4.4)') trim(flnot_base2),nyear
    write(*,*) ' GrADs data written to... ', trim(flnot2)
    open(mtot2,file=flnot2,form='unformatted',access='direct',recl=4)
    irec2 = 0

    if (mod(nyear,4) == 0) then
      l_leap = .true.
      if ((mod(nyear,100) == 0) .and. (mod(nyear,400) /= 0)) then
        l_leap = .false.
      end if
    else
      l_leap = .false.
    end if

    do month = 1, 12

      write(flnot1,'(1a,i4.4,i2.2)') trim(flnot_base1),nyear,month
      !write(*,*) ' GrADs data written to... ', trim(flnot1)
      open(mtot1,file=flnot1,form='unformatted',access='direct',recl=4)
      irec1 = 0

      day_mon = ndmon(month)
      if ((month == 2) .and. l_leap) then
        day_mon = day_mon + 1
      end if

      write(flnin1,'(1a,i4.4,i2.2)') trim(flnin_org1),nyear,month
      open(mtin1, file=flnin1, access='direct', recl=4*imut*jmut)

      read(mtin1,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
      pcp(1:imut,1:jmut) = real(work4(1:imut,1:jmut),8) * 1.d-3 ! mm -> m

      close(mtin1)
    
      prcp_integ = 0.0d0

      do j = jbu, jeu
        do i = ibu, ieu
          area_tmp   = (grid%a_br(i,j) + grid%a_bl(i,j) + grid%a_tr(i,j) + grid%a_tl(i,j)) * 1.0d-4
          prcp_integ = prcp_integ + favail(i,j) * mask_wide(i,j) * area_tmp * pcp(i,j) / real(day_mon * nsday,8) * 1.d3
        end do
      end do

      total_day = total_day + day_mon
      prcp_annual = prcp_annual + prcp_integ * real(day_mon,8)

      !write(6,'(2i6,1a,f12.6)') nyear, month, ' Ocean precipitation (10^9 kg s-1)      = ', prcp_integ * 1.0d-9

      irec1 = irec1 + 1
      write(mtot1,rec=irec1) real(prcp_integ,4)
      close(mtot1)

    end do

    write(6,'(2i6,1a,f12.6)') nyear, total_day, ' Annual mean ocean precipitation (10^9 kg s-1)      = ', prcp_integ * 1.0d-9
    prcp_annual = prcp_annual / real(total_day,8)
    irec2 = irec2 + 1
    write(mtot2,rec=irec2) real(prcp_annual,4)
    close(mtot2)

  end do

  !----------

end program surface_water_flux_monthly
