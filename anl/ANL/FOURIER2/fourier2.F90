!fourier2.F90
!====================================================
!
! fourier transformation
!
!====================================================
program fourier2
  !
  !----------------------------------------------
  !
  implicit none
  !
  real(8), parameter :: pi = 3.1415926535d0
  !
  real(4), allocatable :: v4(:,:,:)
  real(8), allocatable :: v8(:,:)
  !
  real(8), allocatable :: c8(:,:)
  real(4), allocatable :: c4(:,:)
  !
  integer(4) :: imax, jmax, kmax
  integer(4) :: k_ref
  integer(4), save :: i_stt = 3 ! default
  integer(4), save :: j_stt = 2 ! default
  integer(4), save :: nxmax = -1
  integer(4), save :: nymax = -1
  !
  integer(4) :: hxmax, hymax
  !
  ! 入出力ファイル
  !
  character(len=256)    :: flin   ! 入出力ファイル
  character(len=256)    :: flout  ! 出力ファイル
  !
  namelist /nml_fourier2/ imax, jmax, kmax, flin, k_ref, &
    &                i_stt, nxmax, j_stt, nymax, flout
  !
  integer(4), save :: i_offset
  integer(4), save :: j_offset
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
  flout = 'frco.d'
  !
  ! 標準入力から読み込み
  !
  read(unit=5, nml_fourier2, iostat=ios)
  if(ios /= 0) write(*, *) 'reading error nml_fourier2'
  write(*,*) 'imax     :', imax
  write(*,*) 'jmax     :', jmax
  write(*,*) 'kmax     :', kmax
  write(*,*) 'flin     :', trim(flin)
  write(*,*) 'k_ref    :', k_ref
  !
  if(nxmax == -1) nxmax = imax-4 ! CYCLIC
  if(nymax == -1) nymax = jmax-2
  !
  write(*,*) 'nxmax    :', nxmax
  write(*,*) 'nymax    :', nymax
  write(*,*) 'flout   :', trim(flout)
  write(*,*) '----------'

  i_offset = i_stt -1
  j_offset = j_stt -1
  write(*,*) 'i_offset :', i_offset
  write(*,*) 'j_offset :', j_offset
  !
  allocate(v4(imax,jmax,kmax))
  allocate(v8(imax,jmax))
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
  v8(:,:) = real(v4(:,:,k_ref), 8)
  !
  !---------------------------------------------------
  !
  hxmax = (nxmax-1)/2
  hymax = (nymax-1)/2
  allocate(c8(hxmax,hymax))
  c8(:,:) =CMPLX(0.0d0, 0.0d0)
  !
  call cal_coef
  !
  !---------------------------------------------------
  !
  ! 出力
  !
  allocate(c4(hxmax,hymax))
  c4(:,:) = real(c8(:,:),4)
  deallocate(c8(:,:))
  !
  open (mtout, file=flout, form='unformatted', &
    &     access='direct', recl=4*hxmax*hymax)
  write (mtout, rec=1, iostat=ios) c4
  if(ios /= 0) write(*, *) 'writing error in file:', mtout
  close ( mtout )
  deallocate(c4(:,:))
  write(*,*) 'wave number max in x direction', hxmax
  write(*,*) 'wave number max in y direction', hymax
  !
contains
!====================================================
!
!  calculate fourier2 coef.
!
!====================================================
subroutine cal_coef
  !
  integer(4) :: hx, hy, i, j
  complex(8) :: bx, by
  real(8) :: rnxmax, rnymax
  real(8) :: d0x, d0y, dx, dy
  complex(8) :: csum
  !
  rnxmax = 1.0d0 / real(nxmax, 8)
  rnymax = 1.0d0 / real(nymax, 8)
  !
  d0x = 2.0d0 * pi * rnxmax
  d0y = 2.0d0 * pi * rnymax
  !
  do hy = 1, hymax ! wave number
    do hx = 1, hxmax ! wave number
      csum = cmplx(0.0d0, 0.0d0)
      do j = 1, nymax
        do i = 1, nxmax
          dx = d0x * real(i-1, 8) * real(hx-1, 8)
          dy = d0y * real(j-1, 8) * real(hy-1, 8)
          bx = cmplx( -cos(dx), -sin(dx) )
          by = cmplx( -cos(dy), -sin(dy) )
          csum = csum + v8(i+i_offset, j+j_offset) * bx * by
        end do
      end do
      c8(hx, hy) = (cabs(csum) * rnxmax * rnymax)**2
    end do
  end do
end subroutine cal_coef
!====================================================
end program fourier2
