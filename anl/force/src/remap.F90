! -*-F90-*-
!-- Make remap table for interpolation.
!-   restriction: z grid is same
module remap
  use libmxe_para, only: type_libmxe_para, clen
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo
  use force_data,  only: type_force_data
  implicit none
  private


  public :: ini
  public :: calc
  public :: get_result
  public :: write_result


  character(*),parameter   :: file_out = 'remap.dat'
  integer, parameter       :: lun = 77

  integer,save             :: imdst, jmdst, imsrc, jmsrc
  integer,save             :: nlink, isrc_max, idst_max
  integer,allocatable,save :: isrc(:), idst(:)
  real(8),allocatable,save :: wgt(:)
  integer,allocatable,save :: sea_index(:,:,:)  !- 1:sea / 0:land (forcing)

  type(type_libmxe_para),save :: para
  type(type_libmxe_grid),save :: grid
  type(type_libmxe_topo),save :: topo
  type(type_force_data),save  :: force


contains


subroutine ini
  use libmxe_para, only: libmxe_para__register
  use libmxe_grid, only: libmxe_grid__register
  use libmxe_topo, only: libmxe_topo__register, libmxe_topo__aexl
  use force_data,  only: force_data__register, force_data__read_data
  implicit none

  integer :: i

  character(clen) :: file_data, file_data_grid
  integer :: im, jm, num_elm
  integer :: interval_ical(6), first_ical(6), last_ical(6)
  logical :: l_leap_year
  integer :: km

  real(4),parameter :: undef_default = 1234.5678e0
  real(4)           :: undef

  real(4),allocatable :: r(:,:,:,:)

  namelist /nml_force_data/ file_data, file_data_grid, &
                         &  im, jm, num_elm, interval_ical, &
                         &  first_ical, last_ical, &
                         &  l_leap_year, km 
  namelist /nml_remap/ undef


  l_leap_year = .false.
  km = 1
  read( 5, nml=nml_force_data, iostat=i )
  if ( i /= 0 ) then
    write(*,*) ' Read error : namelist'
    stop
  endif

  undef = undef_default
  rewind( 5 )
  read( 5, nml=nml_remap, iostat=i )

  call force_data__register( force, file_data, file_data_grid, &
                           & im, jm, num_elm, interval_ical, &
                           &  first_ical, last_ical, l_leap_year, km=km )

  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)
  call libmxe_topo__aexl(topo,para)

  imdst = para%imut
  jmdst = para%jmut
  imsrc = im
  jmsrc = jm

  allocate( isrc(imdst*jmdst*4*km) )
  allocate( idst(imdst*jmdst*4*km) )
  allocate( wgt(imdst*jmdst*4*km) )

  allocate( sea_index(imsrc,jmsrc,km) )
  sea_index(:,:,:) = 1
  if ( undef /= undef_default ) then
    allocate( r(imsrc,jmsrc,km,force%num_elm) )
    call force_data__read_data( force, 1, r )
    where ( r(:,:,:,1) == undef ) sea_index = 0
    deallocate( r )
  endif

end subroutine ini


subroutine calc
  use weight, only: weight_t_tw
  use coastline, only: coastline__seek_nearest_sea2
  implicit none

  real(8),parameter :: minim = 1.d-6

  integer,allocatable :: idx(:,:), jdx(:,:)
  integer :: i, j, k, i_src, j_src, nnolink
  real(8) :: x, y, x0, x1, y0, y1, w00, w10, w01, w11
  integer :: f00, f10, f01, f11

  !-- grid index: forcing grid southwest of model grid (i,j)
  allocate(idx(imdst,jmdst),jdx(imdst,jmdst))
  idx(:,:) = 0
  jdx(:,:) = 0

  do i = 1, imdst
    do i_src = 1, imsrc
      if ( abs( grid%lont(i) - force%lon(i_src) ) < minim ) then
        idx(i,:) = i_src
        exit
      endif
      if ( grid%lont(i) < force%lon(i_src) ) then
        idx(i,:) = i_src - 1
        exit
      endif
    enddo
  enddo

  do j = 1, jmdst
    do j_src = 1, jmsrc
      if ( abs( grid%latt(j) - force%lat(j_src) ) < minim ) then
        jdx(:,j) = j_src
        exit
      endif
      if ( grid%latt(j) < force%lat(j_src) ) then
        jdx(:,j) = j_src - 1
        exit
      endif
    enddo
  enddo

  !-- weight of forcing 4 grids.
  nnolink = 0
  do k = 1, force%km
    do j = 1, jmdst
      do i = 1, imdst

        x = grid%glont(i,j)
        y = grid%glatt(i,j)

        i_src = idx(i,j)
        j_src = jdx(i,j)

        x0 = force%lon(i_src)
        y0 = force%lat(j_src)
        x1 = force%lon(i_src+1)
        y1 = force%lat(j_src+1)


        if ( topo%atexl(i,j,k) == 0.d0 ) cycle

        !- sea index
        f00 = sea_index(i_src,j_src,k)
        f10 = sea_index(i_src+1,j_src,k)
        f01 = sea_index(i_src,j_src+1,k)
        f11 = sea_index(i_src+1,j_src+1,k)

        if ( f00+f10+f01+f11==0 ) then

          call coastline__seek_nearest_sea2( imsrc, jmsrc, force%lon, force%lat, sea_index(:,:,k), x, y, i_src, j_src )
          if ( ( i_src == 0 ).or.( j_src == 0 ) )then
            write(*,*) 'No link:',i,j,k
            nnolink = nnolink + 1
            cycle
          endif
          nlink = nlink + 1
          isrc(nlink) = imsrc*jmsrc*(k-1) + imsrc * (j_src - 1) + i_src
          idst(nlink) = imdst*jmdst*(k-1) + imdst * (j - 1) + i
          wgt(nlink)  = 1.d0

        else

          call weight_t_tw(x,y,x0,x1,y0,y1,f00,f10,f01,f11,w00,w10,w01,w11)

          if (w00 > 0.d0) then
            nlink = nlink + 1
            isrc(nlink) = imsrc*jmsrc*(k-1) + imsrc * (j_src - 1) + i_src
            idst(nlink) = imdst*jmdst*(k-1) + imdst * (j - 1) + i
            wgt(nlink) = w00
          endif
          if (w10 > 0.d0) then
            nlink = nlink + 1
            isrc(nlink) = imsrc*jmsrc*(k-1) + imsrc * (j_src - 1) + i_src + 1
            idst(nlink) = imdst*jmdst*(k-1) + imdst * (j - 1) + i
            wgt(nlink) = w10
          endif
          if (w01 > 0.d0) then
            nlink = nlink + 1
            isrc(nlink) = imsrc*jmsrc*(k-1) + imsrc *  j_src      + i_src
            idst(nlink) = imdst*jmdst*(k-1) + imdst * (j - 1) + i
            wgt(nlink) = w01
          endif
          if (w11 > 0.d0) then
            nlink = nlink + 1
            isrc(nlink) = imsrc*jmsrc*(k-1) + imsrc *  j_src      + i_src + 1
            idst(nlink) = imdst*jmdst*(k-1) + imdst * (j - 1) + i
            wgt(nlink) = w11
          endif
        endif

      enddo
    enddo
  enddo

  write(6,'(1a,i8)') 'nlink = ', nlink
  isrc_max = maxval(isrc(1:nlink))
  idst_max = maxval(idst(1:nlink))

  write(6,'(1a,i8)') 'no link point = ', nnolink

end subroutine calc


subroutine write_result
  implicit none

  integer,parameter :: num_wgts_all = 1

  write(*,*) 'Remapping table is output: ',file_out

  open(lun,file=file_out,form='unformatted',action='write')
    write(lun) isrc_max
    write(lun)
    write(lun)
    write(lun) idst_max
    write(lun)
    write(lun)
    write(lun) num_wgts_all, nlink
    write(lun) isrc(1:nlink)
    write(lun) idst(1:nlink)
    write(lun) wgt(1:nlink)
  close(lun)

end subroutine write_result


integer function get_result(i,cvar)
  implicit none

  integer,intent(in)      :: i
  character(*),intent(in) :: cvar

  select case ( cvar )
    case('isrc')
      get_result = isrc(i)
    case('idst')
      get_result = idst(i)
    case('nlink')
      get_result = nlink
    case default
      write(*,*) 'Error at remap__get_result'
      write(*,*) '  Wrong cvar =',cvar
      stop
  end select

end function get_result


end module remap
