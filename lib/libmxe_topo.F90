! -*-F90-*-
!- topography information
module libmxe_topo
  use libmxe_para, only: clen, type_libmxe_para
  implicit none
  private


  !-- structure --
  type,public :: type_libmxe_topo
    logical         :: ldef=.false.
    character(clen) :: file_topo

    integer,pointer :: exnn(:,:), texnn(:,:)
                   !- k of U, T-grid bottom ( 0 means land )
    integer,pointer :: exnnbbl(:,:), texnnbbl(:,:)
    integer,pointer :: ho4(:,:)  !- water depth at U-grid [cm]
    integer,pointer :: ho4bbl(:,:)  !- water depth at U-grid [cm]
    integer,pointer :: depth_t_cm(:,:)  !- at T-grid
    real(8),pointer :: dzu(:,:,:) !- box thickness at U grid
    real(8),pointer :: dzt(:,:,:) !- box thickness at T grid
    real(8),pointer :: dzu1c(:,:,:)
         !-   dzu in sigma layer when SSH=0 (only ksgm >= 1)
    real(8),pointer :: aexl(:,:,:), atexl(:,:,:)
         !- ocean/land index at U, T grids
    real(8),pointer :: mask_x(:,:,:), mask_y(:,:,:)
         !-                  at X, Y grids
    real(8),pointer :: dsgm(:)
         !- ratio of each sigma layer (1:ksgm)
    character(len=clen) :: namelist !- namelist file
  end type type_libmxe_topo


  !-- subroutine --
  public :: libmxe_topo__register
    !- register an object

  private :: libmxe_topo__clear
    !- clear an objcet

  public :: libmxe_topo__aexl
    !- Set aexl and atexl.

  public :: libmxe_topo__make_mask_xy


  public :: libmxe_topo__dz3d
    !- Set dzu, dzt and dzu1c.

  public :: libmxe_topo__updatedz
    !- Update dz considering SSH. (only in Sigma layer)

  integer,parameter,private :: lun = 87 


contains 
!-----------------------------------------------------------------


subroutine libmxe_topo__register(topo,para)
  implicit none

  type(type_libmxe_topo),intent(inout) :: topo
  type(type_libmxe_para),intent(in) :: para

  character(clen)   :: file_topo
  integer           :: i, j, ios, im, jm, km, ksgm

  namelist /nml_topo/ file_topo

  !-- check --
  if ( .not. para%ldef )  then
    write(*,*) 'Error at libmxe_topo__register'
    write(*,*) '  para is not registered.'
    stop
  endif
  im = para%imut
  jm = para%jmut
  km = para%km

  call libmxe_topo__clear( topo )
  
  open( lun, file=para%namelist, status='old' )
  read( lun, nml=nml_topo )
  close(lun)
  topo%file_topo = file_topo

  !---- read topography file ----
  allocate( topo%ho4(im,jm), topo%exnn(im,jm) )
  open( lun, file=trim(topo%file_topo), form='unformatted', &
       & status='old', iostat=ios )
  if ( ios /= 0 ) then
    write(*,*) 'Error at libmxe_topo__register'
    write(*,*) '  cannot find ',trim(topo%file_topo)
    stop
  endif
  read(lun) topo%ho4, topo%exnn

  if (para%lbbl) then
    allocate( topo%ho4bbl(im,jm), topo%exnnbbl(im,jm) )
    read(lun) topo%ho4bbl, topo%exnnbbl
  end if
  close(lun)

  topo%namelist = para%namelist

  
  !---- texnn (equivalent to atexl) ----
  allocate( topo%texnn(im,jm) )
  topo%texnn(:,:) = 0
  topo%texnn(1,1) = topo%exnn(1,1)
  do i = 2, im
    topo%texnn(i,1) = max( topo%exnn(i,1), topo%exnn(i-1,1) )
  enddo
  do j = 2, jm
    topo%texnn(1,j) = max( topo%exnn(1,j), topo%exnn(1,j-1) )
  enddo
  do j = 2, jm
    do i = 2, im
      topo%texnn(i,j) = maxval( topo%exnn(i-1:i,j-1:j) )
    enddo
  enddo
  if ( para%lcyclic ) then
    topo%texnn(1,2:jm)    = topo%texnn(im-3,2:jm)
    topo%texnn(2,2:jm)    = topo%texnn(im-2,2:jm)
    topo%texnn(im-1,2:jm) = topo%texnn(3,2:jm)
    topo%texnn(im,2:jm)   = topo%texnn(4,2:jm)
  endif

  if (para%lbbl) then
    allocate( topo%texnnbbl(im,jm) )
    topo%texnnbbl(:,:) = 0
    topo%texnnbbl(1,1) = topo%exnn(1,1)
    do i = 2, im
      topo%texnnbbl(i,1) = max( topo%exnnbbl(i,1), topo%exnnbbl(i-1,1) )
    end do
    do j = 2, jm
      topo%texnnbbl(1,j) = max( topo%exnnbbl(1,j), topo%exnnbbl(1,j-1) )
    end do
    do j = 2, jm
      do i = 2, im
        topo%texnnbbl(i,j) = maxval( topo%exnnbbl(i-1:i,j-1:j) )
      end do
    end do
    if ( para%lcyclic ) then
      topo%texnnbbl(1,2:jm)    = topo%texnnbbl(im-3,2:jm)
      topo%texnnbbl(2,2:jm)    = topo%texnnbbl(im-2,2:jm)
      topo%texnnbbl(im-1,2:jm) = topo%texnnbbl(3,2:jm)
      topo%texnnbbl(im,2:jm)   = topo%texnnbbl(4,2:jm)
    end if
  end if

  !---- depth at T-point ----
  allocate( topo%depth_t_cm(im,jm) )
  topo%depth_t_cm(:,:) = 0
  topo%depth_t_cm(1,1) = topo%ho4(1,1)
  do i = 2, im
    topo%depth_t_cm(i,1) = max( topo%ho4(i,1), topo%ho4(i-1,1) )
  enddo
  do j = 2, jm
    topo%depth_t_cm(1,j) = max( topo%ho4(1,j), topo%ho4(1,j-1) )
  enddo
  do j = 2, jm
    do i = 2, im
      topo%depth_t_cm(i,j) = maxval( topo%ho4(i-1:i,j-1:j) )
    enddo
  enddo
  if ( para%lcyclic ) then
    topo%depth_t_cm(1,2:jm)    = topo%depth_t_cm(im-3,2:jm)
    topo%depth_t_cm(2,2:jm)    = topo%depth_t_cm(im-2,2:jm)
    topo%depth_t_cm(im-1,2:jm) = topo%depth_t_cm(3,2:jm)
    topo%depth_t_cm(im,2:jm)   = topo%depth_t_cm(4,2:jm)
  endif


  topo%ldef = .true.


end subroutine libmxe_topo__register
!-----------------------------------------------------------------


subroutine libmxe_topo__dz3d(topo,para)
  implicit none


  type(type_libmxe_topo),intent(inout) :: topo
  type(type_libmxe_para),intent(in) :: para

  integer :: i, j, k, im, jm, km, ksgm


  !-- check --
  if ( .not. topo%ldef )  then
    write(*,*) 'Error at libmxe_topo__dz3d'
    write(*,*) '  topo is not registered.'
    stop
  endif
  im = para%imut
  jm = para%jmut
  km = para%km
  ksgm = para%ksgm


  !---- dzu ----
  if ( associated(topo%dzu) ) deallocate( topo%dzu )
  allocate( topo%dzu(im,jm,km) )
  topo%dzu = 0.d0
  do j = 1, jm
    do i = 1, im

      do k = 1, topo%exnn(i,j) - 1
        topo%dzu(i, j, k) = para%dz(k)
      enddo

      !- partial cell (bottom)
      k = topo%exnn(i,j)
      if ( k == 1 ) then
        topo%dzu(i, j, k) = dble(topo%ho4(i,j))
      endif
      if ( k >= 2 ) then
        topo%dzu(i, j, k) = dble(topo%ho4(i,j)) &
                        &  - sum( para%dz(1:k-1) )
      endif

    enddo
  enddo

  if (para%lbbl) then
    topo%dzu(1:im, 1:jm, km) = 0.d0
    do j = 1, jm
      do i = 1, im
        if (topo%exnn(i,j) > 0 .and. topo%exnnbbl(i,j) > 0) then
          topo%dzu(i,j,km) = real(topo%ho4bbl(i,j), 8)
        end if
      end do
    end do
  end if

  !---- dzu1c, dsgm (used in libmxe_topo_updatedz) ----
  if ( ksgm >= 1 ) then
    if ( associated(topo%dzu1c) ) deallocate( topo%dzu1c )
    if ( associated(topo%dsgm) ) deallocate( topo%dsgm )
    allocate( topo%dzu1c(im,jm,ksgm) , topo%dsgm(1:ksgm) )
    do k = 1, ksgm
      topo%dzu1c(:,:,k) = topo%dzu(:,:,k)
      topo%dsgm(k) = para%dz(k) / sum(para%dz(1:ksgm))
    enddo
  endif


  !---- dzt ----

  ! NOTE: dzt for interior BBL is zero (this is actually not zero in MRI.COM)

  if ( associated(topo%dzt) ) deallocate( topo%dzt )
  allocate( topo%dzt(im,jm,km) )
  topo%dzt(:,:,:) = 0.d0
  do k = 1, km
    do j = 2, jm
      do i = 2, im
        topo%dzt(i, j, k) = max ( &
            &   topo%dzu(i-1, j-1, k), topo%dzu(i-1, j, k)   &
            & , topo%dzu(i  , j-1, k), topo%dzu(i  , j, k)  )
      enddo
    enddo
  enddo


end subroutine libmxe_topo__dz3d
!-----------------------------------------------------------------


subroutine libmxe_topo__updatedz(ht,topo,para,grid)
  use libmxe_grid, only: type_libmxe_grid
  implicit none


  type(type_libmxe_topo),intent(inout) :: topo
  type(type_libmxe_para),intent(in) :: para
  type(type_libmxe_grid),intent(in) :: grid
  real(8),intent(in) :: ht(para%imut, para%jmut)

  real(8) :: htu
  integer :: i, j, k, im, jm, km, ksgm

  !-- check --
  if ( para%ksgm == 0 ) return

  if ( .not. topo%ldef )  then
    write(*,*) 'Error at libmxe_topo__updatedz'
    write(*,*) '  topo is not registered.'
    stop
  endif

  if ( .not. grid%ldef )  then
    write(*,*) 'Error at libmxe_topo__updatedz'
    write(*,*) '  grid is not registered.'
    stop
  endif

  im = para%imut
  jm = para%jmut
  km = para%km
  ksgm = para%ksgm

  !-- dzu --
  do j = 1, jm - 1
    do i = 1, im - 1
      if ( topo%exnn(i,j) >= 1 ) then
        htu = ( grid%a_bl(i,j) * ht(i,j) &
             & + grid%a_br(i,j) * ht(i+1,j) &
             & + grid%a_tl(i,j) * ht(i,j+1) &
             & + grid%a_tr(i,j) * ht(i+1,j+1) ) / grid%areau(i,j)
        do k = 1, ksgm
          topo%dzu(i,j,k) = topo%dzu1c(i,j,k) + htu * topo%dsgm(k)
        enddo
      endif
    enddo
  enddo


  !-- dzt --
  do k = 1, ksgm
    do j = 2, jm
      do i = 2, im
        topo%dzt(i, j, k) = max ( &
            &   topo%dzu(i-1, j-1, k), topo%dzu(i-1, j, k)   &
            & , topo%dzu(i  , j-1, k), topo%dzu(i  , j, k)  )
      enddo
    enddo
  enddo

end subroutine libmxe_topo__updatedz
!-----------------------------------------------------------------


subroutine libmxe_topo__aexl(topo,para)
  implicit none


  type(type_libmxe_topo),intent(inout) :: topo
  type(type_libmxe_para),intent(in) :: para

  integer :: i, j, k, im, jm, km


  !-- check --
  if ( .not. topo%ldef )  then
    write(*,*) 'Error at libmxe_topo__aexl'
    write(*,*) '  topo is not registered.'
    stop
  endif
  im = para%imut
  jm = para%jmut
  km = para%km


  !---- aexl ----
  if (associated(topo%aexl)) deallocate(topo%aexl)
  allocate( topo%aexl(im,jm,km) )
  topo%aexl = 1.d0
  do k = 1, km
    do j = 1, jm
      do i = 1, im
        if ( k > topo%exnn(i,j) ) topo%aexl(i,j,k) = 0.d0
      enddo
    enddo
  enddo

  if (para%lbbl) then
    topo%aexl(1:im, 1:jm, km) = 0.d0
    do j = 1, jm
      do i = 1, im
        if (topo%exnn(i,j) > 0 .and. topo%exnnbbl(i,j) > 0) then
          topo%aexl(i,j,km) = 1.d0
        end if
      end do
    end do
  end if

  !---- atexl ----
  if (associated(topo%atexl)) deallocate(topo%atexl)
  allocate( topo%atexl(im,jm,km) )
  topo%atexl = 0.d0

  ! NOTE: atexl for interior BBL is zero (this is actually unity in MRI.COM)

  do k = 1, km
    topo%atexl(1,1,k) = topo%aexl(1,1,k)
    do i = 2, im
      topo%atexl(i,1,k) = max( topo%aexl(i,1,k) , topo%aexl(i-1,1,k) )
    enddo
    do j = 2, jm
      topo%atexl(1,j,k) = max( topo%aexl(1,j,k) , topo%aexl(1,j-1,k) )
    enddo
    do j = 2, jm
      do i = 2, im
        topo%atexl(i,j,k) = max( topo%aexl(i-1, j-1, k) &
                           &   , topo%aexl(i-1, j, k) &
                           &   , topo%aexl(i  , j-1, k) &
                           &   , topo%aexl(i  , j, k) )
      enddo
    enddo
  enddo

  if ( para%lcyclic ) then
    topo%atexl(1, 1:jm, 1:km) = topo%atexl(im-3, 1:jm, 1:km)
  end if

end subroutine libmxe_topo__aexl
!-----------------------------------------------------------------


subroutine libmxe_topo__make_mask_xy( topo,para )
  implicit none


  type(type_libmxe_topo),intent(inout) :: topo
  type(type_libmxe_para),intent(in) :: para

  integer :: i, j, k, im, jm, km


  if ( associated(topo%aexl) ) then
    call libmxe_topo__aexl( topo, para )
  endif

  im = para%imut
  jm = para%jmut
  km = para%km

  if (associated(topo%mask_x)) deallocate(topo%mask_x)
  allocate( topo%mask_x(im,jm,km) )

  topo%mask_x(:,1,:) = 0.d0
  do k = 1, km
    do j = 2, jm
      do i = 1, im
        topo%mask_x(i,j,k) = max(topo%aexl(i,j,k),topo%aexl(i,j-1,k))
        !- same as coefx in MRI.COM
      enddo
    enddo
  enddo

  if (associated(topo%mask_y)) deallocate(topo%mask_y)
  allocate( topo%mask_y(im,jm,km) )

  topo%mask_y(1,:,:) = 0.d0
  do k = 1, km
    do j = 1, jm
      do i = 2, im
        topo%mask_y(i,j,k) = max(topo%aexl(i,j,k),topo%aexl(i-1,j,k))
        !- same as coefy in MRI.COM
      enddo
    enddo
  enddo

end subroutine libmxe_topo__make_mask_xy
!-----------------------------------------------------------------


subroutine libmxe_topo__clear(topo)
  implicit none

  type(type_libmxe_topo),intent(out) :: topo

  if ( .not. topo%ldef ) return

  deallocate(topo%exnn,topo%texnn)
  deallocate(topo%ho4)
  deallocate(topo%dzu,topo%dzt,topo%dzu1c)
  deallocate(topo%aexl,topo%atexl)
  deallocate(topo%dsgm)
  topo%ldef = .false.

end subroutine libmxe_topo__clear


end module libmxe_topo
