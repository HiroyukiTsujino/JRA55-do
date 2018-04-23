!fourier.F90
!====================================================
!
! calculate relative vorticity
!
!====================================================
program fourier
  !
  !----------------------------------------------
  !
  implicit none
  !
  real(8), parameter :: pi = 3.1415926535d0
  !
  real(4), allocatable :: v4(:,:,:)
  real(8), allocatable :: v8(:,:,:)
  !
  real(8), allocatable :: c8(:,:,:)
  real(4), allocatable :: c4(:,:,:)
  !
  integer(4) :: imax, jmax, kmax
  logical, save :: l_on_kx = .true.
  logical, save :: l_on_ky = .false.
  integer(4), save :: i_offset = 2 ! default
  integer(4), save :: j_offset = 2 ! default
  integer(4), save :: nxmax = -1
  integer(4), save :: nymax = -1
  !
  integer(4) :: hmax
  !
  ! 入出力ファイル
  !
  character(len=256)    :: flin     ! 入出力ファイル
  character(len=256)    :: flxout   ! 出力ファイル
  character(len=256)    :: flyout   ! 出力ファイル
  !
  namelist /nml_fourier/ imax, jmax, kmax, flin,       &
    &                l_on_kx, i_offset, nxmax, flxout, &
    &                l_on_ky, j_offset, nymax, flyout
  !
  integer(4), parameter :: mtin     = 81
  integer(4), parameter :: mtout    = 84
  integer(4) :: ios       !  入出力エラーチェック用
  !
  !==============================================
  !
  ! 入力パラメタ既定値
  !
  flin  = 'hs_v.d'
  flxout = 'frxco.d'
  flyout = 'fryco.d'
  !
  ! 標準入力から読み込み
  !
  i_offset = 2
  read(unit=5, nml_fourier, iostat=ios)
  if(ios /= 0) write(*, *) 'reading error nml_fourier'
  write(*,*) 'imax     :', imax
  write(*,*) 'jmax     :', jmax
  write(*,*) 'kmax     :', kmax
  write(*,*) 'flin     :', trim(flin)
  !
  if(nxmax == -1) nxmax = imax-4 ! CYCLIC
  if(nymax == -1) nymax = jmax-2
  !
  write(*,*) 'l_on_kx  :', l_on_kx
  write(*,*) 'i_offset :', i_offset
  write(*,*) 'nxmax    :', nxmax
  write(*,*) 'flxout   :', trim(flxout)

  write(*,*) 'l_on_ky  :', l_on_ky
  write(*,*) 'j_offset :', j_offset
  write(*,*) 'nymax    :', nymax
  write(*,*) 'flyout   :', trim(flyout)
  !
  allocate(v4(imax,jmax,kmax))
  allocate(v8(imax,jmax,kmax))
  !
  !==============================================
  !
  ! 入力
  !
  open (mtin, file=flin, form='unformatted', &
    &     access='direct', recl=4*imax*jmax*kmax)
  read (mtin, rec=1, iostat=ios) v4
  if(ios /= 0) write(*, *) 'reading error in file: ', trim(flin), mtin
  close (mtin)
  !
  v8(:,:,:) = real(v4(:,:,:), 8)
  !
  !---------------------------------------------------
  if(l_on_kx) then
    !
    ! calculate relative vorticity
    !
    hmax = (nxmax-1)/2
    allocate(c8(hmax,jmax,kmax))
    c8(1:hmax, 1:jmax, 1:kmax) =0.0d0
    !
    call cal_coef_x
    !
    !---------------------------------------------------
    !
    ! 出力
    !
    allocate(c4(hmax,jmax,kmax))
    c4(1:hmax, 1:jmax, 1:kmax) = real(c8(1:hmax, 1:jmax, 1:kmax),4)
    deallocate(c8(:,:,:))
    !
    open (mtout, file=flxout, form='unformatted', &
      &     access='direct', recl=4*hmax*jmax*kmax)
    write (mtout, rec=1, iostat=ios) c4
    if(ios /= 0) write(*, *) 'writing error in file:', mtout
    close ( mtout )
    deallocate(c4(:,:,:))
    write(*,*) 'wave number max in x direction', hmax
  end if
  !
  if(l_on_ky) then
    !
    ! calculate relative vorticity
    !
    hmax = (nymax-1)/2
    allocate(c8(imax,hmax,kmax))
    c8(1:imax, 1:hmax, 1:kmax) =0.0d0
    !
    call cal_coef_y
    !
    !---------------------------------------------------
    !
    ! 出力
    !
    allocate(c4(imax,hmax,kmax))
    c4(1:imax, 1:hmax, 1:kmax) = real(c8(1:imax, 1:hmax, 1:kmax),4)
    deallocate(c8(:,:,:))
    !
    open (mtout, file=flyout, form='unformatted', &
      &     access='direct', recl=4*imax*hmax*kmax)
    write (mtout, rec=1, iostat=ios) c4
    if(ios /= 0) write(*, *) 'writing error in file:', mtout
    close ( mtout )
    deallocate(c4(:,:,:))
    write(*,*) 'wave number max in y direction', hmax
  end if
  !
contains
!====================================================
!
!  calculate fourier coef.
!
!====================================================
subroutine cal_coef_x
  !
  integer(4) :: h, i, j, k
  complex(8) :: b
  real(8) :: rnxmax
  real(8) :: d0, d
  complex(8) :: csum
  !
  rnxmax = 1.0d0 / real(nxmax, 8)
  !
  d0 = 2.0d0 * pi * rnxmax
  !
  do k = 1, kmax
    do j = 2, jmax-1
      do h = 1, hmax ! wave number
        csum = cmplx(0.0d0, 0.0d0)
        do i = 1, nxmax
          d = d0 * real(i-1, 8) * real(h-1, 8)
          b = cmplx( -cos(d), -sin(d) )
          csum = csum + v8(i+i_offset, j, k) * b
        end do
        c8(h, j, k) = (cabs(csum) * rnxmax)**2
      end do
    end do
  end do
end subroutine cal_coef_x
!====================================================
subroutine cal_coef_y
  !
  integer(4) :: h, i, j, k
  complex(8) :: b
  real(8) :: rnymax
  real(8) :: d0, d
  complex(8) :: csum
  !
  rnymax = 1.0d0 / real(nymax, 8)
  !
  d0 = 2.0d0 * pi * rnymax
  !
  do k = 1, kmax
    do i = 1, imax
      do h = 1, hmax ! wave number
        csum = cmplx(0.0d0, 0.0d0)
        do j = 1, nymax
          d = d0 * real(j-1, 8) * real(h-1, 8)
          b = cmplx( -cos(d), -sin(d) )
          csum = csum + v8(i, j+j_offset, k) * b
        end do
        c8(i, h, k) = (cabs(csum) * rnymax)**2
      end do
    end do
  end do
end subroutine cal_coef_y
!====================================================
end program fourier
