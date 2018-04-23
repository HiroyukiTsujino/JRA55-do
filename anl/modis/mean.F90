! -*-F90-*-
!- Make a mean field of MODIS data.
module mean
  use libmxe_para, only: clen
  implicit none
  private


  integer,parameter :: mm = 12          !- region separate
  integer :: i

  !-- arguments --
  character(clen),save :: file_list     !- text of input data files 
  character(clen),save :: fileo         !- output file name
  integer,save :: mx_str = 1            !- start region (x)
  integer,save :: mx_end = 4            !- end
  integer,save :: my_str = 1            !- start region (y)
  integer,save :: my_end = 3            !- end
  logical,save :: lgrads = .false.      !- flag for grads ctl file

    !- MODIS sub regions
    !- my lat
    !-    50 +----+----+----+----+
    !-  3    |  0 |  1 |  2 |  3 |
    !-    40 +----+----+----+----+
    !-  2    |  4 |  5 |  6 |  7 |
    !-    30 +----+----+----+----+
    !-  1    |  8 |  9 | 10 | 11 |
    !-    20 +----+----+----+----+
    !-      115  125  135  145  155  : lon
    !-          1    2    3    4     : mx

  public :: ini
  public :: convert
  public :: get_result
  public :: write_result

  real,parameter :: rundef = -999.999e0
  integer,parameter :: i10 = 1000    !- grid per 10 degree
  integer,parameter :: im = i10*4, jm = i10*3
  integer,parameter :: lun = 77
  integer,parameter :: mfile = 10000  !- maximum number of input data files
  real,allocatable,save :: r(:,:)
  integer,save :: ir_str, ir_end, jr_str, jr_end
  integer,save :: nfile                 !- number of input data files
  logical,save :: lin(0:mm-1) = (/ (.false., i=0, mm-1) /)
                                        !- flag for input
  character(clen),save :: filei(mfile)

contains


subroutine ini
  implicit none

  integer :: i, j, m

  namelist /mean_lst/  file_list, fileo &
            & , lgrads &
            & , mx_str, mx_end, my_str, my_end

  read(5,nml=mean_lst,iostat=i)
  if ( i /= 0 ) then
    write(*,*) ' Read error : namelist'
    stop
  endif

  if ( ( mx_str < 1).or.( mx_str > mx_end ) &
       & .or.( mx_end > 4 ) ) then
    write(*,*) 'Error at snap__ini'
    write(*,*) '  Wrong mx_str, mx_end',mx_str,mx_end
    stop
  endif

  lin(:) = .false.
  do j = my_str, my_end
    do i = mx_str, mx_end
      m = (3-j)*4 + i-1
      lin(m) = .true.
    enddo
  enddo

  ir_str = i10*( mx_str - 1 ) + 1
  ir_end = i10*mx_end
  jr_str = i10*( my_str - 1 ) + 1
  jr_end = i10*my_end

  allocate( r(ir_str:ir_end,jr_str:jr_end) )
  r(:,:) = rundef
  
  write(*,*) 'Check ',trim(file_list)
  open(lun,file=trim(file_list),form='formatted',status='old')
  do m = 1, mfile
    read(lun,'(a)',iostat=i) filei(m)
    if ( i /= 0 ) then
      nfile = m - 1
      exit
    endif
  enddo
  close(lun)
  if ( nfile < 1 ) then
    write(*,*) 'STOP: Empty file_list:',trim(file_list)
    stop
  endif

  write(*,*) 'Reading ',nfile,' files...'

end subroutine ini


subroutine convert
  implicit none

  integer :: j, m, mx, my, i0, j0, ipixel, iline, n, j1
  real :: lon0, lat0, dx, slope, offset, r2d(i10,i10)
  integer(2) :: r1d(i10)
  character(2) :: ctemp
  real(8),allocatable :: d(:,:)
  integer,allocatable :: ns(:,:)

  allocate( d(ir_str:ir_end,jr_str:jr_end) )
  allocate( ns(ir_str:ir_end,jr_str:jr_end) )
  d(:,:) = 0.d0
  ns(:,:) = 0

  do n = 1, nfile

    if ( mod(n,100) == 0 ) write(*,*) n
    i = len_trim( filei(n) )
    ctemp = filei(n)(i-1:i)
    read(ctemp,'(i2)') m       !- Get area number from filename.

    if ( .not. lin(m) ) cycle  !- Pass file out of target region.

    open(lun,file=trim(filei(n)),form='formatted',status='old')
    read(lun,'(2i6,2f8.2,f8.2,2f9.4)') &
         & ipixel,iline,lon0,lat0,dx,slope,offset
    close(lun)

    r2d(:,:) = rundef
    open(lun,file=trim(filei(n)),form='unformatted',status='old' &
         &,access='direct',recl=i10*2)
    do j = 1, i10
      read(lun,rec=j+1) r1d
      do i = 1, i10
        if ( r1d(i)>0 ) then
          r2d(i,j) = slope*r1d(i) + offset - 273.e0
        endif
      enddo
    enddo
    close(lun)

    mx = mod(m,4) + 1
    my = 3 - m / 4
    i0 = ( mx - 1 ) * i10
    j0 = ( my - 1 ) * i10
    do j = 1, i10
      j1 = i10 - j + 1
      do i = 1, i10
        if  ( ( r2d(i,j1) >= -2.e0 ).and.( r2d(i,j1) <= 30.e0 ) ) then
          d( i+i0, j+j0 ) = d( i+i0, j+j0 ) + dble( r2d(i,j1) )
          ns( i+i0, j+j0 ) = ns( i+i0, j+j0 ) + 1
        endif
      enddo
    enddo
  enddo

  do j = jr_str, jr_end
    do i = ir_str, ir_end
      if  ( ns(i,j) >= 1 ) then
        r(i,j) = real( d(i,j) / dble( ns(i,j) ) )
      endif
    enddo
  enddo
  deallocate( d, ns )

end subroutine convert


subroutine write_result
  use libmxe_para, only: type_libmxe_para, libmxe_para__register
  use libmxe_grid, only: type_libmxe_grid, libmxe_grid__register
  use libmxe_io, only: type_libmxe_io, libmxe_io__register
  use libmxe_grads, only: type_grads, libmxe_grads__make
  implicit none

  type(type_libmxe_para),save :: para
  type(type_libmxe_grid),save :: grid
  type(type_libmxe_io),save :: io
  type(type_grads),save :: grads

  integer :: reclen

  reclen = 4*( ir_end - ir_str + 1 )*( jr_end - jr_str + 1 )

  open(lun,file=trim(fileo),form='unformatted' &
     & , access='direct',recl=reclen,action='write')
    write(lun,rec=1) r(ir_str:ir_end,jr_str:jr_end)
  close(lun)

  if ( lgrads ) then
    call libmxe_para__register(para,file_namelist='namelist.configure.in-modis')
    call libmxe_grid__register(grid,para)
    call libmxe_io__register(io,para)
    grads%file_base = trim(fileo)
    grads%title = 'MODIS SST'
    grads%istr = ir_str
    grads%iend = ir_end
    grads%jstr = jr_str
    grads%jend = jr_end
    grads%cgrid = 'U'
    grads%ztype = 'surface'
    grads%timemode = 'stationary'
    grads%nvar = 1
    grads%var(1) = 't 1 99 SST [degree]'
    grads%undef = rundef
    call libmxe_grads__make(grads,para,grid,io)
  endif

end subroutine write_result


real function get_result(i,j)
  implicit none

  integer,intent(in) :: i,j

  get_result = r(i,j)

end function get_result


end module mean
