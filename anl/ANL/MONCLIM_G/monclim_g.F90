!-*-F90-*-
!monclim_g.F90
!====================================================
!
!  Monthly Data to Annual Data
!
!====================================================
program monclim_g

  implicit none

  integer(4), save :: imax, jmax, kmax
  !
  real(4), save :: UNDEF = -9.99e33
  !
  integer(4) :: sttyear, endyear
  !
  character(len=256)    :: fpathin
  character(len=256)    :: fpathout
  !
  character(len=256)    :: flin
  character(len=256)    :: flout
  !
  integer(4), parameter :: mtfile = 80
  integer(4), parameter :: mtin   = 81
  integer(4), parameter :: mtout  = 82
  !
  real(4), allocatable :: dat3in(:,:,:)
  real(4), allocatable :: dat3out(:,:,:)
  real(8), allocatable :: cnt8(:,:,:,:)
  real(8), allocatable :: dat8(:,:,:,:)
  !
  real(4), allocatable :: dat3inorg(:,:,:)
  !
  integer(4) :: iyear, m
  integer(4) :: i, j, k, n
  !
  !==========================================
  namelist /nml_monclim_g/ imax, jmax, kmax, undef,     &
       & sttyear, endyear, fpathin, fpathout 
  !-----------------------------------------
  read(unit=5, nml=nml_monclim_g)
  print *,'imax       :', imax
  print *,'jmax       :', jmax
  print *,'kmax         :', kmax
  print *,'UNDEF      :', undef
  print *,'start year :', sttyear
  print *,'end   year :', endyear
  print *,'fpath in   :', trim(fpathin)
  print *,'fpath out  :', trim(fpathout)

  allocate(dat3in(1:imax, 1:jmax, 1:kmax))
  allocate(dat3inorg(1:imax, 1:jmax, 1:kmax))
  allocate(cnt8  (1:imax, 1:jmax, 1:kmax, 12))
  allocate(dat8  (1:imax, 1:jmax, 1:kmax, 12))
  allocate(dat3out(1:imax, 1:jmax, 1:kmax))

  !------------------------------------------

  cnt8(:,:,:,:) = 0.d0
  dat8(:,:,:,:) = 0.d0

  !------------------------------------------

  do iyear = sttyear, endyear
    write(flin, '(a, a, i4.4)' ) trim(fpathin), '.', iyear
    write(*,'(a, a)') 'file in :', trim(flin)
    open(mtin, file=flin, form='unformatted', access='direct', recl=4*imax*jmax*kmax)
    do m = 1, 12
      read (mtin, rec=m) dat3in(:,:,:)
      do k=1, kmax
        do j = 1, jmax
          do i = 1, imax
            if(dat3in(i, j, k) == UNDEF) then
              dat3in(i, j, k) = 0.0
            else
              cnt8(i, j, k, m) = cnt8(i, j, k, m) + 1.d0
            end if
          end do
        end do
      end do
      dat8(1:imax, 1:jmax, 1:kmax, m) = dat8(1:imax, 1:jmax, 1:kmax, m) &
        &                  + real(dat3in(1:imax, 1:jmax, 1:kmax), 8)
    end do
    close(mtin)
  end do

  do m = 1, 12
    do k=1, kmax
      do j = 1, jmax
        do i = 1, imax
          if(cnt8(i, j, k, m) > 0.0d0) then
            dat8(i, j, k, m) = dat8(i, j, k, m) / cnt8(i, j, k, m)
          else
            dat8(i, j, k, m) = real(UNDEF, 8)
          end if
        end do
      end do
    end do
  end do

  do m = 1, 12
    write(flout, '(a, a, i2.2)' ) trim(fpathout), '.', m
    write(6,*) 'Output : ', trim(flout)
    open(mtout, file=flout, form='unformatted', access='direct', recl=4*imax*jmax*kmax)
    dat3out(1:imax, 1:jmax, 1:kmax) = real(dat8(1:imax, 1:jmax, 1:kmax, m), 4)
    write(mtout, rec=1) dat3out(1:imax, 1:jmax, 1:kmax)
    close(mtout)
  end do

!====================================================
end program monclim_g
