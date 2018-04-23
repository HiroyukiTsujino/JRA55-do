! -*-F90-*-
!- grid information
module libmxe_grid
  use libmxe_para, only: clen, type_libmxe_para
  implicit none
  private


  !-- structure --
  type,public :: type_libmxe_grid

    character(clen) :: namelist
    logical         :: ldef=.false.

    real(8)         :: lon_west_end_of_core
    real(8)         :: lat_south_end_of_core
    real(8)         :: north_pole_lon
    real(8)         :: north_pole_lat
    real(8)         :: south_pole_lon
    real(8)         :: south_pole_lat

    character(clen) :: file_scale

    real(8),pointer :: lont(:), latt(:)
                      !- longitude and latitude at T-grid
    real(8),pointer :: lonu(:), latu(:)   !- at U-grid
    real(8),pointer :: glont(:,:), glatt(:,:) 
               !- geographical longitude and latitude at T-grid
    real(8),pointer :: glonu(:,:), glatu(:,:)  !- at U-grid
    real(8),pointer :: cor(:,:)       !- Coriolis parameter
    real(8),pointer :: dxtdeg(:), dytdeg(:)
               !- longitude and latitude grid spacing

    real(8),pointer :: dz_cm(:)
    real(8),pointer :: dep(:)  !- box-top depth (1:km+1)
    real(8),pointer :: depm(:) !- box-center depth (1:km)
    real(8),pointer :: dzm(:)  !- box-center interval (1:km)

    real(8),pointer :: a_bl(:,:), a_br(:,:), a_tl(:,:), a_tr(:,:)
                               !- area of quarter 
                               !                  T----X----T
                               !                  | tl | tr |
                               !             U----Y----U----Y
                               !             |    | bl | br |
                               !             X----T----X----T
                               !             |    |    |
                               !             U----Y----U
                               !
    real(8),pointer :: areau(:,:) !- area of U-box
    real(8),pointer :: dx_bl(:,:), dx_br(:,:), dx_tl(:,:), dx_tr(:,:)
    real(8),pointer :: dy_bl(:,:), dy_br(:,:), dy_tl(:,:), dy_tr(:,:)
                               !- length
    real(8),pointer :: nendidx(:) !- north end index
               !- nendidx(para%jet)=0.5 if para%lfoldnp=.true.

  end type type_libmxe_grid


  !-- subroutine --
  public :: libmxe_grid__register
    !- Register an object.

  private :: libmxe_grid__clear
    !- Clear an objcet.

  private :: libmxe_grid__metric_spherical
    !- Calculate metric for a spherical coordinate.

  integer,parameter,private :: lun = 88


contains
!-----------------------------------------------------------------


subroutine libmxe_grid__register(grid,para)
  use libmxe_para, only: pi, radius, radian, radian_r, omega, file_default
  use libmxe_trnsfrm
  implicit none


  type(type_libmxe_grid),intent(inout) :: grid
  type(type_libmxe_para),intent(inout) :: para

  integer :: i, j, k, ios, im, jm, km
  real(8) :: lat, lon, rlat, rlon, rot_cos, rot_sin

  character(clen) :: file_scale
  real(8)         :: lon_west_end_of_core
  real(8)         :: lat_south_end_of_core
  real(8)         :: dx_const_deg
  real(8)         :: dy_const_deg
  character(256)  :: file_dxdy_tbox_deg
  real(8)         :: north_pole_lon, north_pole_lat
  real(8)         :: south_pole_lon, south_pole_lat

  namelist /nml_horz_grid/ lon_west_end_of_core, lat_south_end_of_core,&
                         & dx_const_deg, dy_const_deg, &
                         & file_dxdy_tbox_deg
  namelist /nml_poles/     north_pole_lon, north_pole_lat, &
                         & south_pole_lon, south_pole_lat
  namelist /nml_grid_scale/ file_scale


  !-- check --
  if ( .not. para%ldef )  then
    write(*,*) 'Error at libmxe_grid__register'
    write(*,*) '  para is not registered.'
    stop
  endif
  im = para%imut
  jm = para%jmut
  km = para%km

  call libmxe_grid__clear(grid)
  grid%namelist = para%namelist

  allocate( grid%dxtdeg(1:im) )
  allocate( grid%dytdeg(1:jm) )


  !-- vertical grid --
  allocate( grid%dz_cm(1:km) )
  grid%dz_cm(:) = para%dz(:)

  allocate( grid%dzm(km), grid%dep(km+1), grid%depm(km) )

  grid%dep(1) = 0.D0
  do k = 2, km+1
    grid%dep(k) = grid%dep(k-1) + grid%dz_cm(k-1)   !- box-top depth
  enddo
   
  grid%dzm(1) = 0.5D0 * grid%dz_cm(1)
  do k = 2, km
    grid%dzm(k) = 0.5D0 * ( grid%dz_cm(k) + grid%dz_cm(k-1) )
  end do

  grid%depm(1) = grid%dzm(1)
  do k = 2, km
    grid%depm(k) = grid%depm(k-1) + grid%dzm(k)  !- box-center depth
  enddo


  !----  longitude/latitude ----

  file_dxdy_tbox_deg = file_default

  open( lun, file=trim(para%namelist), status='old' )
  read( lun, nml=nml_horz_grid )

  rewind( lun )
  north_pole_lon = 0.d0
  north_pole_lat = 90.d0
  read( lun, nml=nml_poles, iostat=ios )  !- optional
  if ( ios > 0 ) then
    write(*,*) 'ERROR: Reading nml_poles fail.'
    stop
  endif
  close( lun )

  grid%lon_west_end_of_core = lon_west_end_of_core
  grid%lat_south_end_of_core = lat_south_end_of_core

  grid%north_pole_lon = north_pole_lon
  grid%north_pole_lat = north_pole_lat
  if ( para%lspherical ) then
    grid%south_pole_lon = north_pole_lon + 180.d0
    grid%south_pole_lat = - north_pole_lat
  else
    grid%south_pole_lon = south_pole_lon
    grid%south_pole_lat = south_pole_lat
  endif

  if ( trim(file_dxdy_tbox_deg) /= trim(file_default) ) then

    open( lun, file=trim(file_dxdy_tbox_deg), form='unformatted', &
         & status='old' )
    read(lun) i, j
    if ( ( i == im ).and.( j == jm ) ) then
      read(lun) grid%dxtdeg
      read(lun) grid%dytdeg
    else
      write(*,*) 'ERROR: inconsistent size of ',trim(file_dxdy_tbox_deg)
      write(*,*) '  array size: ',i,j
      stop
    endif
    close( lun )

  else
    grid%dxtdeg(1:im) = dx_const_deg
    grid%dytdeg(1:jm) = dy_const_deg
  endif

  allocate( grid%latt(1:jm), grid%latu(1:jm) )
  if ( para%lsub ) then
    !grid%latu(1) = grid%lat_south_end_of_core &
    !             & - 0.5d0 * grid%dytdeg(1) - grid%dytdeg(2)
    !  This is different from original libmxe, but use this for the case of dytdeg(1) /= dytdeg(2) /= dytdeg(3)
    grid%latu(1) = grid%lat_south_end_of_core &
                 & - grid%dytdeg(2) - 0.5d0 * grid%dytdeg(3)
  else
    !grid%latu(1) = grid%lat_south_end_of_core - 0.5d0 * grid%dytdeg(1)
    !  This is different from original libmxe, but use this for the case of dytdeg(1) /= dytdeg(2)
    grid%latu(1) = grid%lat_south_end_of_core - 0.5d0 * grid%dytdeg(2)
  endif
  do j = 2, jm
    grid%latu(j) = grid%latu(j-1) + grid%dytdeg(j) 
  enddo
  do j = 1, jm
    grid%latt(j) = grid%latu(j) - 0.5d0 * grid%dytdeg(j)
  enddo
  if (para%ltripolar) then  
    do j = 2, jm-1
      if ( ( grid%latu(j+1) > grid%north_pole_lat ) &
          & .and.( grid%latu(j) < grid%north_pole_lat ) ) then
        grid%latt(j+1)=grid%north_pole_lat
        exit
      endif
    enddo
  endif

  write(6,*) ' list of latitude from libmxe '
  do j = 1, jm
    write(6,*) j, grid%latt(j),grid%latu(j)
  enddo

  allocate( grid%lont(1:im), grid%lonu(1:im) )
  if ( para%lsub .or. para%lcyclic ) then
    !grid%lonu(1) = grid%lon_west_end_of_core &
    !             & - 0.5d0 * grid%dxtdeg(1) - grid%dxtdeg(2)
    !  This is different from original libmxe, but use this for the case of dxtdeg(1) /= dxtdeg(2) / = dxtdeg(3)
    grid%lonu(1) = grid%lon_west_end_of_core &
                 & - grid%dxtdeg(2) - 0.5d0 * grid%dxtdeg(3)
  else
    !grid%lonu(1) = grid%lon_west_end_of_core - 0.5d0 * grid%dxtdeg(1)
    !  This is different from original libmxe, but use this for the case of dxtdeg(1) /= dxtdeg(2)
    grid%lonu(1) = grid%lon_west_end_of_core - 0.5d0 * grid%dxtdeg(2)
  endif
  do i = 2, im
    grid%lonu(i) = grid%lonu(i-1) + grid%dxtdeg(i)
  enddo
  do i = 1, im
    grid%lont(i) = grid%lonu(i) - 0.5d0 * grid%dxtdeg(i)
  enddo

  write(6,*) ' list of longitude from libmxe '
  do i = 1, im
    write(6,*) i, grid%lont(i),grid%lonu(i)
  enddo


  !---- geographical latitude/longitude ----
  !write(6,*) ' setting geographical longitude/latitude '
  allocate( grid%glatu(im,jm), grid%glatt(im,jm) )
  allocate( grid%glonu(im,jm), grid%glont(im,jm) )
  allocate( grid%cor(im,jm) )

  do j = 1, jm
    do i = 1, im
      grid%glatu(i,j) = grid%latu(j)
      grid%glonu(i,j) = grid%lonu(i)
      grid%glatt(i,j) = grid%latt(j)
      grid%glont(i,j) = grid%lont(i)
    enddo
  enddo

  if ( para%lspherical ) then

    !- Coriolis parameter
    do j = 1, jm
      do i = 1, im
        grid%cor(i,j) = 2.d0 * omega * sin(grid%glatu(i,j)/radian)
      enddo
    enddo

  else

    call set_abc( grid%north_pole_lat, grid%north_pole_lon, &
                & grid%south_pole_lat, grid%south_pole_lon )
    do j = 1, jm
      do i = 1, im

        !- U-points
        rlat = grid%glatu(i,j) * radian_r
        rlon = grid%glonu(i,j) * radian_r
        call mp2lp(lon, lat, rlon, rlat)
        call rot_mp2lp(rot_cos, rot_sin, lon, lat, rlon, rlat)
        grid%cor(i,j) = 2.d0 * omega * sin(lat)
        grid%glatu(i,j) = lat * radian
        lon = lon * radian
        if ( lon < 0.d0 ) then
          grid%glonu(i,j) = lon + 360.d0
        else if ( lon >= 360.d0) then
          grid%glonu(i,j) = lon - 360.d0
        else
          grid%glonu(i,j) = lon
        endif

        !- T-points
        rlat = grid%glatt(i,j) * radian_r
        rlon = grid%glont(i,j) * radian_r
        call mp2lp(lon, lat, rlon, rlat)
        call rot_mp2lp(rot_cos, rot_sin, lon, lat, rlon, rlat)
        grid%glatt(i,j) = lat * radian
        lon = lon * radian
        if ( lon < 0.d0 ) then
          grid%glont(i,j) = lon + 360.d0
        else if ( lon >= 360.d0) then
          grid%glont(i,j) = lon - 360.d0
        else
          grid%glont(i,j) = lon
        endif

      enddo
    enddo

    if ( para%lfoldnp ) then
      do i = 1, im
        grid%cor(i,jm-2) = grid%cor(im-i+1,jm-3)
        grid%cor(i,jm-1) = grid%cor(im-i+1,jm-4)
        grid%cor(i,jm  ) = grid%cor(im-i+1,jm-5)
      enddo
    endif

  endif


  !------ scale factors (unit area and length) ------
  !write(6,*) ' determine scale factors (unit area and length) '
  allocate( grid%a_bl(im, jm) )
  allocate( grid%a_br(im, jm) )
  allocate( grid%a_tl(im, jm) )
  allocate( grid%a_tr(im, jm) )
  allocate( grid%dx_bl(im, jm) )
  allocate( grid%dx_br(im, jm) )
  allocate( grid%dx_tl(im, jm) )
  allocate( grid%dx_tr(im, jm) )
  allocate( grid%dy_bl(im, jm) )
  allocate( grid%dy_br(im, jm) )
  allocate( grid%dy_tl(im, jm) )
  allocate( grid%dy_tr(im, jm) )


  if ( para%lspherical ) then
    call libmxe_grid__metric_spherical(grid,para)

  else

    open( lun, file=trim(para%namelist), status='old')
    read( lun, nml=nml_grid_scale )
    close( lun )
    grid%file_scale = file_scale

    open( lun, file=trim(grid%file_scale), form='unformatted', &
         & access='sequential' )
      !write(6,*) 'libmxe_grid__register:'
      !write(6,*) '  Reading from ....',trim(fscale)
      read(lun)   grid%a_bl  ! area
      read(lun)   grid%a_br
      read(lun)   grid%a_tl
      read(lun)   grid%a_tr
      read(lun)   grid%dx_bl ! X-ward length
      read(lun)   grid%dx_br
      read(lun)   grid%dx_tl
      read(lun)   grid%dx_tr
      read(lun)   grid%dy_bl ! Y-ward length
      read(lun)   grid%dy_br
      read(lun)   grid%dy_tl
      read(lun)   grid%dy_tr
    close(lun)

  endif


  !-- cyclic condition --
  if ( para%lcyclic ) then
    grid%dx_br(im,1:jm) = grid%dx_br(4,1:jm)
    grid%dx_tr(im,1:jm) = grid%dx_tr(4,1:jm)
  else
    if (para%l_globe) then
      grid%dx_br( im, 1:jm ) = grid%dx_bl( 1, 1:jm )
      grid%dx_tr( im, 1:jm ) = grid%dx_tl( 1, 1:jm )
      grid%dy_tr( 1:im, jm ) = grid%dy_br( 1:im, 1 )
      grid%dy_tl( 1:im, jm ) = grid%dy_bl( 1:im ,1 )
    else
      grid%dx_br(im,1:jm) = grid%dx_br(im-1,1:jm)
      grid%dx_tr(im,1:jm) = grid%dx_tr(im-1,1:jm)
    end if
  endif

  if ( para%lcyclic ) then
    grid%a_br( im, 1:jm   ) = grid%a_br( 4 , 1:jm   )
    grid%a_tr( im, 1:jm-1 ) = grid%a_tr( 4 , 1:jm-1 )
  else
    if (para%l_globe) then
      grid%a_br( im, 1:jm   ) = grid%a_bl( 1 , 1:jm   )
      grid%a_tr( im, 1:jm-1 ) = grid%a_tl( 1 , 1:jm-1 )
      grid%a_tr( 1:im, jm )   = grid%a_br( 1:im, 1 )
      grid%a_tl( 1:im, jm )   = grid%a_bl( 1:im ,1 )
    else
      grid%a_br( im, 1:jm   ) = grid%a_bl( im , 1:jm   )
      grid%a_tr( im, 1:jm-1 ) = grid%a_tl( im , 1:jm-1 )
    end if
  endif


  !-- areau ( Eq.(3.21) in MRI.COM manual ) --
  allocate( grid%areau(1:im,1:jm) )
  grid%areau = grid%a_bl + grid%a_br + grid%a_tl + grid%a_tr


  !-- nendidx

  allocate( grid%nendidx(1:jm) )
  grid%nendidx(1:jm) = 1.0d0
  if (para%lfoldnp) then
    grid%nendidx(para%jet) = 0.5d0
  endif

  grid%ldef = .true.


end subroutine libmxe_grid__register
!-----------------------------------------------------------------


subroutine libmxe_grid__metric_spherical(grid,para)
  use libmxe_para, only: pi, radius, radian_r
  implicit none

  
  type(type_libmxe_grid),intent(inout) :: grid
  type(type_libmxe_para),intent(in) :: para

  real(8),allocatable,dimension(:) :: lat, dlat, dlon
  real(8),allocatable,dimension(:,:) :: anhft, ashft
  real(8) :: temp1, temp2, deg1, ld
  integer :: i, j, im, jm


  im = para%imut
  jm = para%jmut


  !---- areas ----
  !
  !  T(i,j+1)                             T(i+1,j+1)
  !   +-------------------+-------------------+
  !   |                   |                   |
  !   |      tl(i,j)      |      tr(i,j)      |
  !   |                   |                   |
  !   +-----------------U(i,j)----------------+
  !   |                   |                   |
  !   |      bl(i,j)      |      br(i,j)      |
  !   |                   |                   |
  !   +-------------------+-------------------+
  ! T(i,j)                                 T(i+1,j)
  !  

  !-- anhft and ashft (see p.36 of  MRI.COM manual in Japanese)--
  !- anhft: north-eastern part of T-box
  !-  = (a^2/2) * dlon * cos(lat) * sin(dlat/2) * ( 1 - tan(lat) *tan(dlat/4) )
  !- ashft: south-eastern part
  !-  = (a^2/2) * dlon * cos(lat) * sin(dlat/2) * ( 1 + tan(lat) *tan(dlat/4) )
  allocate( lat(1:jm), dlon(1:im), dlat(1:jm) )  !- radian
  lat = grid%latt * radian_r
  dlon = 0.d0
  dlat = 0.d0
  do i = 2, im
    dlon(i) = ( grid%lonu(i) - grid%lonu(i-1) ) * radian_r
  enddo
  dlon(1) = dlon(2)
  do j = 2, jm
    dlat(j) = ( grid%latu(j) - grid%latu(j-1) ) * radian_r
  enddo
  dlat(1) = dlat(2)


  allocate( anhft(1:im,1:jm), ashft(1:im,1:jm) )
  do j = 1, jm
    do i = 1, im
      if (grid%latt(j) <= -90.D0) then
        temp1 = 0.5d0 * radius**2 * dlon(i) * sin( 0.5d0*dlat(j) )
        temp2 = tan( 0.25d0 * dlat(j) )
        ashft(i,j) = 0.d0
        anhft(i,j) = temp1 * temp2
      else if (grid%latt(j) >= 90.D0) then
        temp1 = 0.5d0 * radius**2 * dlon(i) * sin( 0.5d0*dlat(j) )
        temp2 = tan( 0.25d0 * dlat(j) )
        ashft(i,j) = temp1 * temp2
        anhft(i,j) = 0.d0
      else
        temp1 = 0.5d0 * radius**2 * dlon(i) * cos( lat(j) ) * sin( 0.5d0*dlat(j) )
        temp2 = tan( lat(j) ) * tan( 0.25d0 * dlat(j) )
        anhft(i,j) = temp1 * ( 1.d0 - temp2 )
        ashft(i,j) = temp1 * ( 1.d0 + temp2 )
      end if
    end do
    write(6,*) grid%latt(j), ashft(1,j), anhft(1,j) 
  end do
  deallocate(lat,dlon,dlat)


  !-- a_bl, a_br, a_tl, a_tr --
  grid%a_bl=anhft

  grid%a_br=0.d0
  do j = 1, jm
    do i = 1, im -1
      grid%a_br(i,j)=anhft(i+1,j)
    enddo
  enddo

  grid%a_tl=0.d0
  do j = 1, jm -1
    do i = 1, im
      grid%a_tl(i,j)=ashft(i,j+1)
    enddo
  enddo

  grid%a_tr=0.d0
  do j = 1, jm -1
    do i = 1, im -1
      grid%a_tr(i,j)=ashft(i+1,j+1)
    enddo
  enddo

  deallocate(anhft, ashft)


  !---- grid lengths ----
  !-
  !  T(i,j+1)                               T(i+1,j+1)
  !   +---------------------+---------------------+
  !   |                     |                     |
  !   |dy_tl(i,j)           |dy_tr(i,j)           |
  !   |                     |                     |
  !   |                     |                     |
  !   |      dx_tl(i,j)     |     dx_tr(i,j)      | 
  !   +-------------------U(i,j)------------------+
  !   |                     |                     |
  !   |dy_bl(i,j)           |dy_br(i,j)           |
  !   |                     |                     |
  !   |                     |                     |
  !   |      dx_bl(i,j)     |     dx_br(i,j)      | 
  !   +---------------------+---------------------+
  ! T(i,j)                                     T(i+1,j)

  deg1 = ( 2.d0 * pi * radius ) / 360.d0   !- length of 1 degree [cm]

  !-- dx --
  grid%dx_bl = 0.d0
  grid%dx_tl = 0.d0
  grid%dx_br = 0.d0
  grid%dx_tr = 0.d0

  do j = 1, jm
    do i = 1, im
!      ld = grid%lonu(i) - grid%lont(i)
      ld = 0.5d0 * grid%dxtdeg(i)
      grid%dx_bl(i,j) = deg1 * ld * cos( grid%latt(j) * radian_r )
      grid%dx_tl(i,j) = deg1 * ld * cos( grid%latu(j) * radian_r )
    enddo
  enddo
  do j = 1, jm
    do i = 1, im -1
!      ld = grid%lont(i+1) - grid%lonu(i)
      ld = 0.5d0 * grid%dxtdeg(i+1)
      grid%dx_br(i,j) = deg1 * ld * cos( grid%latt(j) * radian_r )
      grid%dx_tr(i,j) = deg1 * ld * cos( grid%latu(j) * radian_r )
    enddo
  enddo

  !-- dy --
  grid%dy_bl = 0.d0
  grid%dy_tl = 0.d0
  grid%dy_br = 0.d0
  grid%dy_tr = 0.d0

  do j = 1, jm
    do i = 1, im
      grid%dy_bl(i,j) = deg1 * ( grid%latu(j) - grid%latt(j) )
    enddo
  enddo
  grid%dy_br = grid%dy_bl

  do j = 1, jm -1
    do i = 1, im
      grid%dy_tl(i,j) = deg1 * ( grid%latt(j+1) - grid%latu(j) )
    enddo
  enddo
  grid%dy_tl(1:im,jm) = grid%dy_tl(1:im,jm-1)
  grid%dy_tr = grid%dy_tl


end subroutine libmxe_grid__metric_spherical
!-----------------------------------------------------------------


subroutine libmxe_grid__clear(grid)
  implicit none

  type(type_libmxe_grid),intent(out) :: grid

  if ( .not. grid%ldef ) return

  deallocate(grid%lont,grid%latt)
  deallocate(grid%lonu,grid%latu)
  deallocate(grid%glont,grid%glatt)
  deallocate(grid%glont,grid%glatt)
  deallocate(grid%cor)
  deallocate(grid%dxtdeg,grid%dytdeg)
  deallocate(grid%dep,grid%depm,grid%dzm)
  deallocate(grid%a_bl,grid%a_br,grid%a_tl,grid%a_tr)
  deallocate(grid%areau)
  deallocate(grid%dx_bl,grid%dx_br,grid%dx_tl,grid%dx_tr)
  deallocate(grid%dy_bl,grid%dy_br,grid%dy_tl,grid%dy_tr)
  deallocate(grid%nendidx)

  grid%ldef = .false.


end subroutine libmxe_grid__clear


end module libmxe_grid
