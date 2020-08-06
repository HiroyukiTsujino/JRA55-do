! -*-F90-*-
!==============================================================
program merge_20CRv3_and_jra55
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

  integer(4) :: i, j, k

  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  integer(4),parameter :: mtin1 = 31, mtin2 = 32, mtin3 = 33
  integer(4),parameter :: mtin4 = 34, mtin5 = 35, mtin6 = 36
  integer(4),parameter :: mtot1 = 51, mtot2 = 52

  integer(4) :: lrec

  real(4) :: undef1, undef2

  real(4),allocatable :: dat2(:,:)
  real(8),allocatable :: orgdata(:,:)
  real(8),allocatable :: refdata(:,:)
  real(8),allocatable :: newdata(:,:)
  real(8),allocatable :: weight(:,:)

  integer(4) :: ic, jc

  real(8) :: distance, hl1, lonabs, latabs
  real(8) :: dlon, dlat
  real(8),parameter :: half_width = 5.0d0  ! 5.0-degree
  real(8) :: lonc, latc

  character(len=256) :: dir1_in, dir2_in
  character(len=256) :: flnin1_base, flnin2_base
  character(len=256) :: flnin1, flnin2

  character(len=256) :: dir_out
  character(len=256) :: flnot_base
  character(len=256) :: flnot, flwgt

  integer(4) :: nyeara, nmona, ndaya, nhoura, ncont
  integer(4) :: nyear, nmon, nday, nhour
  integer(4) :: monday
  integer(4) :: nfh, max_nfh, nfh_replace
  integer(4) :: nhr_elapsed
  real(8) :: tv_weight

  logical :: l_leap
  logical :: l_out_weight

  type(type_libmxe_para) :: para
  type(type_libmxe_grid) :: grid
  type(type_libmxe_topo) :: topo

!-----------------------------------------------------------------

  namelist /nml_merge20CRv3jra55/ &
       &  flnin1_base, flnin2_base, &
       &  dir1_in, dir2_in, &
       &  undef1, undef2,   &
       &  nyeara, nmona, ndaya, nhoura, ncont, nfh_replace, &
       &  lonc, latc,       &
       &  dir_out, flnot_base, &
       &  l_out_weight

!-----------------------------------------------------------------

  l_out_weight = .false.
  nfh_replace = 2
  open (10,file='./namelist.merge20CRv3jra55')
  read (10,nml_merge20CRv3jra55)
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

  lrec = imax*jmax*4

  allocate(dat2(1:imax,1:jmax))
  allocate(orgdata(1:imax,1:jmax))
  allocate(refdata(1:imax,1:jmax))
  allocate(newdata(1:imax,1:jmax))
  allocate(weight (1:imax,1:jmax))

  !-----------------------------------------------------------------------

  nyear = nyeara
  nmon  = nmona
  nday  = ndaya
  nhour = nhoura

  l_leap = .false.
  if (mod(nyear,4) == 0) then
    if (mod(nyear,100) == 0) then
      if (mod(nyear,400) == 0) then
        l_leap = .true.
      else
        l_leap = .false.
      end if
    else
      l_leap = .true.
    end if
  else
    l_leap = .false.
  end if

  if (nmon == 2) then
    if (l_leap) then
      monday = ndmon(nmon) + 1
    else
      monday = ndmon(nmon)
    end if
  else
    monday = ndmon(nmon)
  end if

  if (ncont == 1) then
    max_nfh = nfh_replace
  else
    max_nfh = 26
  end if

  nhr_elapsed = 0
  LOOP_FCST: do nfh = 1, max_nfh

  nhr_elapsed = nhr_elapsed + 3

  tv_weight = 1.d0

  if (ncont == 0 .and. nhr_elapsed > 6) then
    tv_weight = 0.5d0 * (-tanh(real(nhr_elapsed-54, 8)/6.0d0) + 1.d0) ! 1.5-2.5 day
!    tv_weight = 0.5d0 * (-tanh(real(nhr_elapsed-42, 8)/6.0d0) + 1.d0) ! 1.0-2.0 day
  end if

  write(6,*) nhr_elapsed, tv_weight

  nhour = nhour + 3
  if (nhour == 24) then
    nday = nday + 1
    nhour = 0
    if (nday > monday) then
      nmon = nmon + 1
      nday = 1
    end if
    if (nmon == 13) then
      nyear = nyear + 1
      nmon = 1
    end if
  end if

  write(6,*) ' merging ', nyear, nmon, nday, nhour

  write(flnin1,'(1a,1a,i4.4,i2.2,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') &
       & trim(dir1_in),'/',nyear,nmon,'/',&
       & trim(flnin1_base),'.', nyear, nmon, nday, nhour
  open(mtin1, file=flnin1, form='unformatted', access='direct', recl=lrec)
  write(6,*) ' file : ', trim(flnin1), ' opened '
  read(mtin1,rec=1) dat2
  do j = 1, jmax
    do i = 1, imax
      orgdata(i,j) = real(dat2(i,j),8)
    end do
  end do
  close(mtin1)

  write(flnin2,'(1a,1a,i4.4,i2.2,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') &
       & trim(dir2_in),'/',nyear,nmon,'/',&
       & trim(flnin2_base),'.', nyear, nmon, nday, nhour
  open(mtin2, file=flnin2, form='unformatted', access='direct', recl=lrec)
  write(6,*) ' file : ', trim(flnin2), ' opened '
  read(mtin2,rec=1) dat2
  do j = 1, jmax
    do i = 1, imax
      refdata(i,j) = real(dat2(i,j),8)
    end do
  end do
  close(mtin2)

  !------------------------------------------------------------------------

  jc = 1
  latabs = 0.6d0
  do j = 1, jmax
    if (abs(grid%latu(j)-latc) < latabs) then
      jc = j
      latabs = abs(grid%latu(j)-latc)
    end if
  end do

  ic = 1
  lonabs = 0.6d0
  do i = 1, imax
    if (abs(grid%lonu(i)-lonc) < lonabs) then
      ic = i
      lonabs = abs(grid%lonu(i)-lonc)
    end if
  end do

  write(6,*) ' ic = ', ic, grid%lonu(ic), lonc
  write(6,*) ' jc = ', jc, grid%latu(jc), latc

  weight (:,:) = 0.0d0

  do j = 1, jmax
    do i = 1, imax
      dlon = lonc - grid%lonu(i)
      if (lonc - grid%lonu(i) > 180.d0) then
        dlon = lonc - 360.d0 - grid%lonu(i)
      end if
      if (lonc - grid%lonu(i) < -180.d0) then
        dlon = lonc + 360.d0 - grid%lonu(i)
      end if
      dlat = latc - grid%latu(j)
      distance = sqrt(dlon**2 + dlat**2)
      if (distance > 22.d0) then
        weight(i,j) = 0.d0
      else
        if (distance < 10.d0) then
          weight(i,j) = 1.d0 * tv_weight
        else
          weight(i,j) = exp(-((distance-10.d0)/half_width)**2) * tv_weight
        end if
      end if
      newdata(i,j) = weight(i,j) * refdata(i,j) + (1.0d0 - weight(i,j)) * orgdata(i,j)
    end do
  end do

  !------------------------------------------------------------------------

  write(flnot,'(1a,1a,i4.4,i2.2,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') &
       & trim(dir_out),'/',nyear,nmon,'/',&
       & trim(flnot_base),'.', nyear, nmon, nday, nhour
  open(mtot1, file=flnot, form='unformatted', access='direct', recl=lrec)
  write(6,*) ' file : ', trim(flnot), ' opened '
  write(mtot1,rec=1) real(newdata(:,:),4)
  close(mtot1)

  if (l_out_weight) then
    write(flwgt,'(1a,1a,i4.4,i2.2,1a,i4.4,i2.2,i2.2,i2.2)') &
         & trim(dir_out),'/',nyear,nmon,'/weight.', nyear, nmon, nday, nhour
    open(mtot2, file=flwgt, form='unformatted', access='direct', recl=lrec)
    write(6,*) ' file : ', trim(flwgt), ' opened '
    write(mtot2,rec=1) real(weight(:,:),4)
    close(mtot2)
  end if

  end do LOOP_FCST

end program merge_20CRv3_and_jra55
