!-*-F90-*-
module mod_gaussgrid
  !
  !
  implicit none
  !
  integer(4), save :: imaxg ! => IMAXA
  integer(4), save :: jmaxg ! => JMAXA
  !
  real(8), allocatable :: long(:)               ! longitude of grid center
  real(8), allocatable :: latg(:)               ! latitude of grid center
  real(8), allocatable :: long_boundary(:)      ! longitude of grid boundary
  real(8), allocatable :: latg_boundary(:)      ! latitude of grid boundary
  real(8), allocatable :: gauss_wgt(:)          ! gaussian weight
  real(8), allocatable :: coscolat(:)           ! cosine of colatitude
  real(8), allocatable :: gw(:)                 ! gaussian weight
  real(8), allocatable :: gw_integ(:)
  !
  real(8), parameter :: zero = 0.0d0
  real(8), parameter :: one  = 1.0d0
  real(8), parameter :: half = 0.5d0

  real(8), parameter :: pi = 3.141592653589793d0    ! 円周率

  contains


  subroutine set_gaussgrid ( resol )
    !----------------------------------------------------------------
    ! Purpose:
    !    set centers and corners of gaussian grid cell 
    !      based on gaussian weight
    !      - The core part of calculation is written by R. Mizuta
    !----------------------------------------------------------------
    !
    character(len=16), intent(in) ::  resol
    !
    real(8) :: r2d
    real(8) :: dlon
    real(8) :: half_dlon
    integer :: i , j
    !
    ! Set dimension parameters
    !
    print *, ' set_gaussgrid: resol =',resol
    imaxg = 0
    imaxg = 0
    if ( resol=='T5' .or. resol=='T05' .or. resol=='T005' ) then
      imaxg =   16
      jmaxg =    8
    endif
    if ( resol=='T21' .or. resol=='T021' .or. resol=='t21' ) then
      imaxg =   64
      jmaxg =   32
    endif
    if ( resol=='T42' .or. resol=='T042' .or. resol=='TL63' ) then
      imaxg =  128
      jmaxg =   64
    endif
    if ( resol=='T63' .or. resol=='T063' .or. resol=='TL95' ) then
      imaxg =  192
      jmaxg =   96
    endif
    if ( resol=='T62' .or. resol=='T062' .or. resol=='TL93' ) then
      imaxg =  192
      jmaxg =   94
    endif
    if ( resol=='T106' .or. resol=='T0106' .or. resol=='TL159' ) then
      imaxg =  320
      jmaxg =  160
    endif
    if ( resol=='T213' .or. resol=='T0213' .or. resol=='TL319' ) then
      imaxg =  640
      jmaxg =  320
    endif
    if ( resol=='T319' .or. resol=='T0319' .or. resol=='TL479' ) then
      imaxg =  960
      jmaxg =  480
    endif
    if ( resol=='T426' .or. resol=='T0426' .or. resol=='TL639' ) then
      imaxg = 1280
      jmaxg =  640
    endif
    if ( resol=='T639' .or. resol=='T0639' .or. resol=='TL959' ) then
      imaxg = 1920
      jmaxg =  960
    endif
    if ( resol=='WOA1x1') then
      imaxg = 360
      jmaxg = 180
    endif
    if ( resol=='MRICOM1_0x0_5') then
      imaxg = 360
      jmaxg = 361
    endif
    if ( resol=='MRICOM1_0x0_5u') then
      imaxg = 360
      jmaxg = 360
    endif
    if ( imaxg == 0 ) then
      print *, 'Unknown resol =',resol
      stop 900
    endif
    !
    ! allocation
    !
    allocate ( long(imaxg) )
    allocate ( latg(jmaxg) )
    allocate ( long_boundary(0:imaxg+1) )
    allocate ( latg_boundary(0:jmaxg) )
    allocate ( gauss_wgt(jmaxg) )
    allocate ( coscolat(jmaxg) )
    allocate ( gw(jmaxg) )
    allocate ( gw_integ(0:jmaxg) )
    !
    ! calculation
    !
    ! pi = asin(one)*2
    r2d = 180.0d0 / pi
    dlon = 360.0d0/imaxg
    half_dlon = half*dlon
    !
    call gauss (coscolat, gw, jmaxg)
    !
    gauss_wgt = gw
    !
    latg(1:jmaxg) = -asin(coscolat(1:jmaxg))*r2d
    long(1:imaxg) = (/ (i,i=0,imaxg-1) /) *dlon
    !
    gw_integ(0) = 0.0d0
    do j=1,jmaxg
      gw_integ(j) = gw_integ(j-1) + gw(j)
    end do
    ! 
    latg_boundary(0)     = -90.0d0
    latg_boundary(jmaxg) = 90.0d0
    do j=1,jmaxg-1
      latg_boundary(j) = asin( gw_integ(j) - 1.0d0 )*r2d
    end do
    !
    long_boundary(0)      = long(1) - half_dlon
    do i=1,imaxg
      long_boundary(i) = long(i) + 360.d0/(imaxg*2.0d0)
    end do
    long_boundary(imaxg+1) = long_boundary(imaxg) + dlon
    !
    if ( resol=='WOA1x1' ) then
      do i = 1, imaxg
        long(i) =  0.5d0  + 1.d0 * dble(i-1)
      end do
      do j = 1, jmaxg
        latg(j) = -89.5d0 + 1.d0 * dble(j-1)
      end do
      long_boundary(0)       = 0.0d0
      long_boundary(imaxg+1) = 360.0d0
!      long_boundary(imaxg+1) = 361.0d0
      do i =1,imaxg
        long_boundary(i) = 1.d0 * dble(i)
      end do
      latg_boundary(0)     = -90.0d0
      latg_boundary(jmaxg) = 90.0d0
      do j =1,jmaxg-1
        latg_boundary(j) = -90.0d0 + 1.d0 * dble(j)
      end do
    end if
    !
    if ( resol=='MRICOM1_0x0_5' ) then
      do i = 1, imaxg
        long(i) =  0.0d0  + 1.0d0 * dble(i-1)
      end do
      do j = 1, jmaxg
        latg(j) = -90.0d0 + 0.5d0 * dble(j-1)
      end do
      long_boundary(0)       = -0.5d0
      long_boundary(imaxg+1) = 360.5d0

      do i =1,imaxg
        long_boundary(i) = 0.5d0 + 1.d0 * dble(i-1)
      end do

      latg_boundary(0)     = -90.0d0
      latg_boundary(jmaxg) = 90.0d0
      do j = 1, jmaxg-1
        latg_boundary(j) = -89.75d0 + 0.5d0 * dble(j-1)
      end do
    end if

    !-----

    if ( resol=='MRICOM1_0x0_5u' ) then
      do i = 1, imaxg
        long(i) =  0.5d0  + 1.0d0 * dble(i-1)
      end do
      long_boundary(0)     = 0.0d0

      do i = 1, imaxg
        long_boundary(i) = 1.d0 * dble(i)
      end do

      do j = 1, jmaxg
        latg(j) = -89.75d0 + 0.5d0 * dble(j-1)
      end do
      latg_boundary(0)   = -90.0d0
      do j = 1, jmaxg
        latg_boundary(j) = -90.0d0 + 0.5d0 * dble(j)
      end do

    end if
    !

!g     write(6,*) 'gw  =',gw
!g     write(6,*) 'gw_integ =',gw_integ
       write(6,*) 'latg=',latg
       write(6,*) 'latg_boundary = ',latg_boundary
!g     write(6,*) 'long=',long
!g     write(6,*) 'long_boundary=',long_boundary
    !
  end subroutine set_gaussgrid

  subroutine free_gaussgrid
    !
    deallocate ( long )
    deallocate ( latg )
    deallocate ( long_boundary )
    deallocate ( latg_boundary )
    deallocate ( gauss_wgt )
    deallocate ( coscolat )
    deallocate ( gw )
    deallocate ( gw_integ )
    !
  end subroutine free_gaussgrid

end module mod_gaussgrid
