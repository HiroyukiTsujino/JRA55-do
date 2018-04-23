! -*-F90-*-
!==============================================================
program make_correction_for_surface_temperature
!==============================================================

  use libmxe_para, only: libmxe_para__register, clen, rho &
                     & , pi, radius, type_libmxe_para
  use libmxe_grid, only: libmxe_grid__register  &
                     & , type_libmxe_grid
  use libmxe_topo, only: libmxe_topo__register, libmxe_topo__aexl &
                     & , type_libmxe_topo

  implicit none

  integer(4) :: imax
  integer(4) :: jmax

  integer(4) :: i, j, k, nyear, month, klev, mday
  integer(4) :: nbyr, neyr, ibmn, iemn

  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)
  integer(4),parameter :: nday_year = 365

  integer(4),parameter :: mtin1 = 31, mtin2 = 32, mtin3 = 33
  integer(4),parameter :: mtot1 = 51, mtot2 = 52

  integer(4) :: lrec

  real(4) :: undef_jra
  real(4) :: undef_core
  real(4) :: undef_ice

  real(4),allocatable :: dat2(:,:)
  real(8),allocatable :: tmp10m_obs(:,:)
  real(8),allocatable :: tmp10m_jra(:,:)
  real(8),allocatable :: mask_jra(:,:)
  real(8),allocatable :: tmp10m_crr(:,:)

  real(8) :: lattrn_north, lattrn_south
  real(8) :: taper_fac

  character(len=256) :: flnin1, flnin2, flnin3
  character(len=256) :: flnin1_base, flnin2_base
  character(len=256) :: flnot1, flnot2

  type(type_libmxe_para) :: para
  type(type_libmxe_grid) :: grid
  type(type_libmxe_topo) :: topo

!-----------------------------------------------------------------

  namelist /nml_tmpcorrec/ &
       &  flnin1_base, &
       &  flnin2_base, &
       &  flnin3, &
       &  undef_jra, undef_core, &
       &  lattrn_north, lattrn_south, &
       &  flnot1

!-----------------------------------------------------------------

  open (10,file='namelist.tmp10mcorrec_no_antarc')
  read (10,nml_tmpcorrec) 
  close(10)

!-----------------------------------------------------------------
  ! set grid points

  !-- model settings --

  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)
  call libmxe_topo__aexl(topo,para)

  imax = para%imut
  jmax = para%jmut

  allocate (dat2(1:imax,1:jmax))
  allocate (tmp10m_obs(1:imax,1:jmax))
  allocate (tmp10m_jra(1:imax,1:jmax))
  allocate (tmp10m_crr(1:imax,1:jmax))
  allocate (mask_jra(1:imax,1:jmax))

  lrec=imax*jmax*4

  !-----------------------------------------------------------------------


  mask_jra(:,:) = 0.0d0
  open(mtin3, file=flnin3, form='unformatted', access='direct', recl=lrec)
  write(6,*) ' file : ', trim(flnin3), ' opened '
  read(mtin3,rec=1) dat2
  do j = 1, jmax
    do i = 1, imax
      mask_jra(i,j) = real(dat2(i,j),8)
    end do
  end do
  close(mtin3)

 !---------------------------------------------------------------

  tmp10m_obs(:,:) = 0.0d0
  tmp10m_jra(:,:) = 0.0d0

  write(6,*) ' Correction factor of temperature on ocean = ',trim(flnot1)
  open (mtot1,file=flnot1,access='direct',form='unformatted',recl=lrec)

  do month = 1, 12

    write(flnin1,'(1a,i2.2)') trim(flnin1_base),month
    open(mtin1, file=flnin1, form='unformatted', access='direct', recl=lrec)
    write(6,*) ' file : ', trim(flnin1), ' opened '
    read(mtin1,rec=1) dat2
    close(mtin1)

    do j = 1, jmax
      do i = 1, imax
        if (dat2(i,j) /= undef_core) then
          tmp10m_obs(i,j) = real(dat2(i,j),8)
        else
          tmp10m_obs(i,j) = 0.0d0
        end if
      end do
    end do

    write(flnin2,'(1a,i2.2)') trim(flnin2_base),month
    open(mtin2, file=flnin2, form='unformatted', access='direct', recl=lrec)
    write(6,*) ' file : ', trim(flnin2), ' opened '
    read(mtin2,rec=1) dat2
    close(mtin2)

    do j = 1, jmax
      do i = 1, imax
        if (dat2(i,j) /= undef_jra) then
          tmp10m_jra(i,j) = real(dat2(i,j),8)
        else
          tmp10m_jra(i,j) = 0.0d0
        end if
      end do
    end do
    
    do j = 1, jmax
      do i = 1, imax
        if (mask_jra(i,j) == 1.0d0) then
          if ((tmp10m_obs(i,j) /= 0.0d0) .and. (tmp10m_jra(i,j) /= 0.0d0)) then
            taper_fac = (grid%latu(j) - lattrn_south) / (lattrn_north - lattrn_south)
            taper_fac = min(1.0d0,max(taper_fac,0.0d0))
            tmp10m_crr(i,j) = (tmp10m_obs(i,j) - tmp10m_jra(i,j)) * taper_fac
          else
            tmp10m_crr(i,j) = 0.0d0
          end if
        else
          tmp10m_crr(i,j) = real(undef_jra,8)
        end if
      end do
    end do

    write(mtot1,rec=month) real(tmp10m_crr(1:imax,1:jmax),4)

  end do

  close(mtot1)

end program make_correction_for_surface_temperature
