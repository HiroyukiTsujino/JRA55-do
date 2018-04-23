! -*-F90-*-
!==============================================================
program fill_correction_factor_for_temperature
!==============================================================

  use libmxe_para, only: libmxe_para__register, clen, rho &
                     & , pi, radius, radian, radian_r, type_libmxe_para
  use libmxe_grid, only: libmxe_grid__register  &
                     & , type_libmxe_grid
  use libmxe_topo, only: libmxe_topo__register, libmxe_topo__aexl &
                     & , type_libmxe_topo

  use file_open_close_manager

  implicit none

  integer(4) :: imax
  integer(4) :: jmax
  integer(4) :: kmax

  integer(4) :: i, j, k, n, nyear, month, klev, mday
  integer(4) :: nbyr, neyr, ibmn, iemn

  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  integer(4),parameter :: mtin1 = 31, mtin2 = 32, mtin3 = 33
  integer(4),parameter :: mtin4 = 34, mtin5 = 35, mtin6 = 36
  integer(4),parameter :: mtot1 = 51, mtot2 = 52

  integer(4) :: lrec

  real(4) :: undef_jra
  real(8) :: undef8_jra

  real(4),allocatable :: dat2(:,:)
  real(8),allocatable :: multply_fac(:,:)
  real(8),allocatable :: multply_fac_filt(:,:)
  real(8),allocatable :: multply_fac_tmp(:,:)
  real(8),allocatable :: fill_offset(:,:)
!  real(8),allocatable :: fill_offset_tmp(:,:)
  real(8),allocatable :: mask_land(:,:)
  real(8),allocatable :: mask_land_tmp(:,:)

  integer(4) :: num_pass ! pass of 5-point filter
  
  real(8) :: hl1, hl2, hl3

  character(len=256) :: flnin1, flnin2, flnin3
  character(len=256) :: flnin4, flnin5, flnin6
  character(len=256) :: flnot1, flnot2
  character(len=256) :: file_mask

  type(type_libmxe_para) :: para
  type(type_libmxe_grid) :: grid
  type(type_libmxe_topo) :: topo

  integer(4) :: nd, num_data, nitem1, nitem2, nth1, nth2

!-----------------------------------------------------------------

  namelist /nml_fill_mult_correc/ &
       &  flnin1, &
       &  num_data, nitem1, nth1, &
       &  undef_jra,      &
       &  file_mask, &
       &  flnot1, &
       &  num_pass

!-----------------------------------------------------------------

  open (10,file='namelist.fill_mult_correc')
  read (10,nml_fill_mult_correc)
  close(10)

  undef8_jra = real(undef_jra,8)

!-----------------------------------------------------------------
  ! set grid points

  !-- model settings --

  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)
  call libmxe_topo__aexl(topo,para)

  imax = para%imut
  jmax = para%jmut

  lrec = imax*jmax*4

  allocate(dat2(1:imax,1:jmax))

  allocate(multply_fac(1:imax,1:jmax))
  allocate(multply_fac_filt(1:imax,1:jmax))
  allocate(multply_fac_tmp(0:imax+1,1:jmax))

  allocate(fill_offset(1:imax,1:jmax))
!  allocate(fill_offset_tmp(0:imax+1,1:jmax))

  allocate(mask_land(1:imax,1:jmax))
  allocate(mask_land_tmp(0:imax+1,1:jmax))

  !-----------------------------------------------------------------------
  ! MASK data

  open(mtin1, file=file_mask, form='unformatted', access='direct', recl=lrec)
  write(6,*) ' file : ', trim(file_mask), ' opened '
  read(mtin1,rec=1) dat2
  do j = 1, jmax
    do i = 1, imax
      mask_land(i,j) = real(dat2(i,j),8)
    end do
  end do
  close(mtin1)

  mask_land_tmp(1:imax,1:jmax) = mask_land(1:imax,1:jmax)
  mask_land_tmp(     0,1:jmax) = mask_land(  imax,1:jmax)
  mask_land_tmp(imax+1,1:jmax) = mask_land(     1,1:jmax)

  !-----------------------------------------------------------------------

  write(6,*) ' Filled correction factor output to ',trim(flnot1)
  open (mtot1,file=flnot1,access='direct',form='unformatted',recl=lrec)

  !-----------------------------------------------------------------------

  LOOP_NDATA: do nd = 1, num_data

  !-----------------------------------------------------------------------

    open (mtin1, file=flnin1, form='unformatted', access='direct', recl=lrec)
    write(6,*) ' Raw correction factor read from file ', trim(flnin1)
    write(6,'(1a,i4.4)') '  Record #',nitem1*(nd-1)+nth1
    read (mtin1,rec=nitem1*(nd-1)+nth1) dat2
    close(mtin1)

    do j = 1, jmax
      do i = 1, imax
        if (mask_land(i,j) == 1.0d0) then
          multply_fac(i,j) = real(dat2(i,j),8)
        else
          multply_fac(i,j) = undef8_jra
        end if
      end do
    end do

    multply_fac_tmp(1:imax,1:jmax) = multply_fac(1:imax,1:jmax)
    multply_fac_tmp(0     ,1:jmax) = multply_fac(  imax,1:jmax)
    multply_fac_tmp(imax+1,1:jmax) = multply_fac(     1,1:jmax)
  !------------------------------------------------------------------------

    fill_offset(:,:) = 0.0d0

    do j = 2, jmax - 1
      do i = 1, imax
        if ((multply_fac_tmp(i,j) == 1.0d0) .or. (multply_fac_tmp(i-1,j) == 1.0d0) &
             & .or. (multply_fac_tmp(i+1,j) == 1.0d0) .or. (multply_fac_tmp(i,j-1) == 1.0d0) &
             & .or. (multply_fac_tmp(i-1,j-1) == 1.0d0) .or. (multply_fac_tmp(i+1,j-1) == 1.0d0) &
             & .or. (multply_fac_tmp(i,j+1) == 1.0d0) .or. (multply_fac_tmp(i-1,j+1) == 1.0d0) &
             & .or. (multply_fac_tmp(i+1,j+1) == 1.0d0)) then
          fill_offset(i,j) = 1.0d0 * mask_land(i,j)
        end if
      end do
    end do

    j = 1
    do i = 1, imax
      if ((multply_fac_tmp(i,j) == 1.0d0) .or. (multply_fac_tmp(i-1,j) == 1.0d0) &
           & .or. (multply_fac_tmp(i+1,j) == 1.0d0) .or. (multply_fac_tmp(i,j+1) == 1.0d0) &
           & .or. (multply_fac_tmp(i-1,j+1) == 1.0d0) .or. (multply_fac_tmp(i+1,j+1) == 1.0d0)) then
        fill_offset(i,j) = 1.0d0 * mask_land(i,j)
      end if
    end do

    j = jmax
    do i = 1, imax
      if ((multply_fac_tmp(i,j) == 1.0d0) .or. (multply_fac_tmp(i-1,j) == 1.0d0) &
           & .or. (multply_fac_tmp(i+1,j) == 1.0d0) .or. (multply_fac_tmp(i,j-1) == 1.0d0) &
           & .or. (multply_fac_tmp(i-1,j-1) == 1.0d0) .or. (multply_fac_tmp(i+1,j-1) == 1.0d0)) then
        fill_offset(i,j) = 1.0d0 * mask_land(i,j)
      end if
    end do

!    do j = 1, jmax
!      do i = 1, imax
!        if (multply_fac(i,j) == 1.0d0) then
!          fill_offset(i,j) = 1.0d0
!        end if
!      end do
!    end do

!    fill_offset_tmp(1:imax,1:jmax) = fill_offset(1:imax,1:jmax)
!    fill_offset_tmp(     0,1:jmax) = fill_offset(  imax,1:jmax)
!    fill_offset_tmp(imax+1,1:jmax) = fill_offset(     1,1:jmax)

  !------------------------------------------------------------------------
  ! 9-point filter
  
    multply_fac_filt(:,:) = multply_fac(:,:)
  
    do n = 1, num_pass
  
      write(6,'(1a,i2.2)') 'filter pass #', n
  
      multply_fac_tmp(1:imax,1:jmax) = multply_fac_filt(1:imax,1:jmax)
      multply_fac_tmp(0     ,1:jmax) = multply_fac_filt(  imax,1:jmax)
      multply_fac_tmp(imax+1,1:jmax) = multply_fac_filt(     1,1:jmax)
      
      do j = 2, jmax - 1
        do i = 1, imax
          if (fill_offset(i,j) == 1.0d0) then
            hl2 =    multply_fac_tmp(i  ,j  ) * 12.0d0 * mask_land_tmp(i  ,j  ) &
                 & + multply_fac_tmp(i+1,j+1) *  1.0d0 * mask_land_tmp(i+1,j+1) &
                 & + multply_fac_tmp(i  ,j+1) *  2.0d0 * mask_land_tmp(i  ,j+1) &
                 & + multply_fac_tmp(i-1,j+1) *  1.0d0 * mask_land_tmp(i-1,j+1) &
                 & + multply_fac_tmp(i+1,j)   *  2.0d0 * mask_land_tmp(i+1,j  ) &
                 & + multply_fac_tmp(i-1,j)   *  2.0d0 * mask_land_tmp(i-1,j  ) &
                 & + multply_fac_tmp(i+1,j-1) *  1.0d0 * mask_land_tmp(i+1,j-1) &
                 & + multply_fac_tmp(i  ,j-1) *  2.0d0 * mask_land_tmp(i  ,j-1) &
                 & + multply_fac_tmp(i-1,j-1) *  1.0d0 * mask_land_tmp(i-1,j-1)
            hl3 =   12.0d0 * mask_land_tmp(i  ,j  ) &
                 & + 1.0d0 * mask_land_tmp(i+1,j+1) &
                 & + 2.0d0 * mask_land_tmp(i  ,j+1) &
                 & + 1.0d0 * mask_land_tmp(i-1,j+1) &
                 & + 2.0d0 * mask_land_tmp(i+1,j  ) &
                 & + 2.0d0 * mask_land_tmp(i-1,j  ) &
                 & + 1.0d0 * mask_land_tmp(i+1,j-1) &
                 & + 2.0d0 * mask_land_tmp(i  ,j-1) &
                 & + 1.0d0 * mask_land_tmp(i-1,j-1)
            if (hl3 /= 0.0d0) then
              multply_fac_filt(i,j) = hl2 / hl3
            end if
          end if
        end do
      end do

    end do
  
    multply_fac(:,:) = multply_fac_filt(:,:)
    
  !------------------------------------------------------------------------

    write(mtot1,rec=nd) real(multply_fac(1:imax,1:jmax),4)

  !------------------------------------------------------------------------

  end do LOOP_NDATA

  close(mtot1)

end program fill_correction_factor_for_temperature
