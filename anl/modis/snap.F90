! -*-F90-*-
!- Convert 1 snap shot of MODIS data to grads data.
module snap
  use libmxe_para, only: clen
  implicit none
  private


  integer,parameter :: mm = 12          !- region separate
  integer :: i

  !-- arguments --
  character(clen),save :: file_base     !- input file base (without _??)
  character(clen),save :: fileo         !- output file name
  logical,save :: lin(0:mm-1) = (/ (.false., i=0, mm-1) /)
                                        !- flag for input
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
  real,save :: r(im,jm)
  integer,save :: ir_str, ir_end, jr_str, jr_end


contains


subroutine ini
  implicit none

  integer :: i

  namelist /snap_lst/  file_base, fileo &
            & , lin, lgrads &
            & , mx_str, mx_end, my_str, my_end

  read(5,nml=snap_lst,iostat=i)
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

  ir_str = i10*( mx_str - 1 ) + 1
  ir_end = i10*mx_end
  jr_str = i10*( my_str - 1 ) + 1
  jr_end = i10*my_end

  r(:,:) = rundef

end subroutine ini


subroutine convert
  implicit none

  character(clen) :: filename
  integer :: j, m, mx, my, i0, j0, ipixel, iline
  real :: lon0, lat0, dx, slope, offset, r2d(i10,i10)
  integer(2) :: r1d(i10)

  do m = 0, mm - 1
    if ( lin(m) ) then

      write(filename,'(i2.2)') m
      filename = trim(file_base)//'_'//trim(filename)
      write(*,*)'Read ',trim(filename)

      open(lun,file=trim(filename),form='formatted',status='old')
        read(lun,'(2i6,2f8.2,f8.2,2f9.4)') &
          & ipixel,iline,lon0,lat0,dx,slope,offset
      close(lun)

      r2d(:,:) = rundef
      open(lun,file=trim(filename),form='unformatted',status='old' &
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
        do i = 1, i10
          r( i + i0 , j + j0 ) = r2d( i, i10 - j + 1 )
        enddo
      enddo

    endif
  enddo

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


end module snap
