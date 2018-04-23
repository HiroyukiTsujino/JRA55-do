! -*-F90-*-
!==============================================================
program fill_correction_factor_for_wind_vector
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
  real(8),allocatable :: corr_mag(:,:)
  real(8),allocatable :: corr_rot(:,:)
  real(8),allocatable :: corr_mag_filt(:,:)
  real(8),allocatable :: corr_rot_filt(:,:)
  real(8),allocatable :: swind_vec(:,:)
  real(8),allocatable :: mask_wind(:,:)
  real(8),allocatable :: fill_wind(:,:)
  real(8),allocatable :: fill_wind_org(:,:)
  real(8),allocatable :: corr_mag_tmp(:,:)
  real(8),allocatable :: corr_rot_tmp(:,:)
  real(8),allocatable :: mask_wind_tmp(:,:)
  real(8),allocatable :: alonu_tmp(:)

  integer(4),parameter :: n_search = 5
  integer(4) :: ib, ie, jb, je, ii, jj

  integer(4) :: num_pass ! pass of 5-point filter
  
  real(8) :: distance, hl1, hl2, hl3
  real(8),parameter :: half_width = 0.5d0
  real(8) :: lonc, latc, weight
  real(8) :: value_mag, value_rot

  character(len=256) :: flnin1, flnin2, flnin3
  character(len=256) :: flnin4, flnin5, flnin6
  character(len=256) :: flnot1, flnot2
  character(len=256) :: file_fill_wind

  type(type_libmxe_para) :: para
  type(type_libmxe_grid) :: grid
  type(type_libmxe_topo) :: topo

  integer(4) :: nd, num_data, nitem1, nitem2, nth1, nth2

!-----------------------------------------------------------------

  namelist /nml_fillwindcorr_antarc_v2/ &
       &  flnin1, flnin2, &
       &  num_data, nitem1, nth1, nitem2, nth2, &
       &  undef_jra,      &
       &  file_fill_wind, &
       &  flnot1, flnot2, &
       &  num_pass

!-----------------------------------------------------------------

  open (10,file='namelist.fillwindcorr_antarc_v2')
  read (10,nml_fillwindcorr_antarc_v2)
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
  allocate(corr_mag(1:imax,1:jmax))
  allocate(corr_rot(1:imax,1:jmax))
  allocate(corr_mag_filt(1:imax,1:jmax))
  allocate(corr_rot_filt(1:imax,1:jmax))
  allocate(swind_vec(1:imax,1:jmax))
  allocate(mask_wind(1:imax,1:jmax))
  allocate(fill_wind(1:imax,1:jmax))
  allocate(fill_wind_org(1:imax,1:jmax))
  allocate(corr_mag_tmp(-9:imax+10,1:jmax))
  allocate(corr_rot_tmp(-9:imax+10,1:jmax))
  allocate(mask_wind_tmp(-9:imax+10,1:jmax))
  allocate(alonu_tmp(-9:imax+10))

  alonu_tmp(1:imax) = grid%lonu(1:imax)
  alonu_tmp(-9:0) = alonu_tmp(imax-9:imax)
  alonu_tmp(imax+1:imax+10) = alonu_tmp(1:10)

  !-----------------------------------------------------------------------
  ! MASK data

  open(mtin1, file=file_fill_wind, form='unformatted', access='direct', recl=lrec)
  write(6,*) ' file : ', trim(file_fill_wind), ' opened '
  read(mtin1,rec=1) dat2
  do j = 1, jmax
    do i = 1, imax
      fill_wind_org(i,j) = real(dat2(i,j),8)
    end do
  end do
  close(mtin1)

  !-----------------------------------------------------------------------

  write(6,*) ' Correction factor of speed = ',trim(flnot1)
  open (mtot1,file=flnot1,access='direct',form='unformatted',recl=lrec)
  write(6,*) ' Rotation = ',trim(flnot2)
  open (mtot2,file=flnot2,access='direct',form='unformatted',recl=lrec)

  !-----------------------------------------------------------------------

  LOOP_NDATA: do nd = 1, num_data

  !-----------------------------------------------------------------------

    open(mtin1, file=flnin1, form='unformatted', access='direct', recl=lrec)
    write(6,*) ' file : ', trim(flnin1), ' opened '
    read(mtin1,rec=nitem1*(nd-1)+nth1) dat2
    close(mtin1)

    fill_wind(:,:) = fill_wind_org(:,:)
    do j = 1, jmax
      do i = 1, imax
        if (dat2(i,j) /= undef_jra) then
          fill_wind(i,j) = 1.0d0
        end if
      end do
    end do

    do j = 1, jmax
      do i = 1, imax
        if ((fill_wind(i,j) == 1.0d0) .or. (fill_wind(i,j) == 5.0d0)) then
          corr_mag(i,j) = real(dat2(i,j),8)
        end if
        if ((fill_wind(i,j) == 0.0d0) .or. (fill_wind(i,j) == 7.0d0)) then
          corr_mag(i,j) = 1.0d0
        end if
      end do
    end do

    corr_mag_tmp(1:imax,1:jmax) = corr_mag(1:imax,1:jmax)
    corr_mag_tmp(-9:0,1:jmax) = corr_mag(imax-9:imax,1:jmax)
    corr_mag_tmp(imax+1:imax+10,1:jmax) = corr_mag(1:10,1:jmax)

  !-----------------------------------------------------------------------

    open(mtin2, file=flnin2, form='unformatted', access='direct', recl=lrec)
    write(6,*) ' file : ', trim(flnin2), ' opened '
    read(mtin2,rec=nitem2*(nd-1)+nth2) dat2
    close(mtin2)

    do j = 1, jmax
      do i = 1, imax
        if ((fill_wind(i,j) == 1.0d0) .or. (fill_wind(i,j) == 5.0d0)) then
          corr_rot(i,j) = real(dat2(i,j),8)
        end if
        if ((fill_wind(i,j) == 0.0d0) .or. (fill_wind(i,j) == 7.0d0)) then
          corr_rot(i,j) = 0.0d0
        end if
      end do
    end do

    corr_rot_tmp(1:imax,1:jmax) = corr_rot(1:imax,1:jmax)
    corr_rot_tmp(-9:0,1:jmax) = corr_rot(imax-9:imax,1:jmax)
    corr_rot_tmp(imax+1:imax+10,1:jmax) = corr_rot(1:10,1:jmax)

  !------------------------------------------------------------------------

    mask_wind(:,:) = 0.0d0
    do j = 1, jmax
      do i = 1, imax
        !if ((fill_wind(i,j) == 0.0d0) .or.(fill_wind(i,j) == 1.0d0) .or. (fill_wind(i,j) == 7.0d0)) then
        if ((fill_wind(i,j) == 1.0d0) .or. (fill_wind(i,j) == 7.0d0)) then
          mask_wind(i,j) = 1.0d0
        end if
      end do
    end do

    mask_wind_tmp(1:imax,        1:jmax) = mask_wind(1:imax,     1:jmax)
    mask_wind_tmp(-9:0,          1:jmax) = mask_wind(imax-9:imax,1:jmax)
    mask_wind_tmp(imax+1:imax+10,1:jmax) = mask_wind(1:10,       1:jmax)

  !------------------------------------------------------------------------
  ! fill_wind = 5

    do j = 1, jmax
      do i = 1, imax
        if (fill_wind(i,j) == 5.0d0) then
          lonc = grid%lonu(i)
          latc = grid%latu(j)
          weight = 0.0d0
          value_mag = 0.0d0
          value_rot = 0.0d0
          ib = i - n_search
          ie = i + n_search
          jb = max(j - n_search, 1)
          je = min(j + n_search, jmax)
          do jj = jb, je
            do ii = ib, ie
              if (mask_wind_tmp(ii,jj) == 1.0d0) then
                distance = sqrt((lonc - alonu_tmp(ii))**2 + (latc - grid%latu(jj))**2)
                hl1 = exp(-(distance/half_width)**2)
                weight = weight + hl1
                value_mag = value_mag + corr_mag_tmp(ii,jj) * hl1
                value_rot = value_rot + corr_rot_tmp(ii,jj) * hl1
              end if
            end do
          end do
          if (value_mag == 0.0d0) then
            write(6,*) ' Could not find valid point for magnitude around ', &
                 & real(grid%lonu(i),4), real(grid%latu(j),4)
            stop
          else
            corr_mag(i,j) = value_mag / weight
            corr_rot(i,j) = value_rot / weight
          end if
        end if
      end do
    end do

  !------------------------------------------------------------------------
  ! fill_wind = 7 and Southern Ocean

    do j = 1, jmax
      do i = 1, imax
        if (fill_wind(i,j) == 7.0d0 .and. grid%latu(j) < -50.0d0) then
          lonc = grid%lonu(i)
          latc = grid%latu(j)
          weight = 0.0d0
          value_mag = 0.0d0
          value_rot = 0.0d0
          ib = i - n_search
          ie = i + n_search
          jb = max(j - n_search, 1)
          je = min(j + n_search, jmax)
          do jj = jb, je
            do ii = ib, ie
              if (mask_wind_tmp(ii,jj) == 1.0d0) then
                distance = sqrt((lonc - alonu_tmp(ii))**2 + (latc - grid%latu(jj))**2)
                hl1 = exp(-(distance/half_width)**2)
                weight = weight + hl1
                value_mag = value_mag + corr_mag_tmp(ii,jj) * hl1
                value_rot = value_rot + corr_rot_tmp(ii,jj) * hl1
              end if
            end do
          end do
          if (value_mag == 0.0d0) then
            write(6,*) ' Could not find valid point for magnitude around ', &
                 & real(grid%lonu(i),4), real(grid%latu(j),4)
            stop
          else
            corr_mag(i,j) = value_mag / weight
            corr_rot(i,j) = value_rot / weight
          end if
        end if
      end do
    end do

  !------------------------------------------------------------------------
  ! 9-point filter for southern ocean
  
    corr_mag_filt(:,:) = corr_mag(:,:)
    corr_rot_filt(:,:) = corr_rot(:,:)
  
    do n = 1, num_pass
  
      write(6,'(1a,i2.2)') 'filter pass #', n
      corr_mag_tmp(1:imax,1:jmax) = corr_mag_filt(1:imax,1:jmax)
      corr_mag_tmp(-9:0,1:jmax) = corr_mag_filt(imax-9:imax,1:jmax)
      corr_mag_tmp(imax+1:imax+10,1:jmax) = corr_mag_filt(1:10,1:jmax)
  
      corr_rot_tmp(1:imax,1:jmax) = corr_rot_filt(1:imax,1:jmax)
      corr_rot_tmp(-9:0,1:jmax) = corr_rot_filt(imax-9:imax,1:jmax)
      corr_rot_tmp(imax+1:imax+10,1:jmax) = corr_rot_filt(1:10,1:jmax)
      
      do j = 2, jmax - 1
        do i = 1, imax
          if (fill_wind(i,j) == 7.0d0 .and. grid%latu(j) < -50.0d0) then
            hl1 = 8.0d0 * corr_mag_tmp(i,j) &
                 & + corr_mag_tmp(i+1,j+1) + corr_mag_tmp(i,j+1) + corr_mag_tmp(i-1,j+1) &
                 & + corr_mag_tmp(i+1,j) + corr_mag_tmp(i-1,j) &
                 & + corr_mag_tmp(i+1,j-1) + corr_mag_tmp(i,j-1) + corr_mag_tmp(i-1,j-1)
            hl2 = 8.0d0 * corr_rot_tmp(i,j) &
                 & + corr_rot_tmp(i+1,j+1) + corr_rot_tmp(i,j+1) + corr_rot_tmp(i-1,j+1) &
                 & + corr_rot_tmp(i+1,j) + corr_rot_tmp(i-1,j) &
                 & + corr_rot_tmp(i+1,j-1) + corr_rot_tmp(i,j-1) + corr_rot_tmp(i-1,j-1)
            hl3 = fill_wind(i,j) &
                 & * fill_wind(i+1,j+1) * fill_wind(i,j+1) * fill_wind(i-1,j+1) &
                 & * fill_wind(i+1,j) * fill_wind(i-1,j) &
                 & * fill_wind(i+1,j-1) * fill_wind(i,j-1) * fill_wind(i-1,j-1)
            if (hl3 /= 0.0d0) then
              corr_mag_filt(i,j) = hl1 / 16.0d0
              corr_rot_filt(i,j) = hl2 / 16.0d0
            end if
          end if
        end do
      end do
  
    end do
  
    corr_mag(:,:) = corr_mag_filt(:,:)
    corr_rot(:,:) = corr_rot_filt(:,:)
    
  !------------------------------------------------------------------------

    write(mtot1,rec=nd) real(corr_mag(1:imax,1:jmax),4)
    write(mtot2,rec=nd) real(corr_rot(1:imax,1:jmax),4)

  !------------------------------------------------------------------------

  end do LOOP_NDATA

  close(mtot2)
  close(mtot1)

end program fill_correction_factor_for_wind_vector
