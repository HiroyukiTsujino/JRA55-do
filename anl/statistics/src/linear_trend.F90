!-*-F90-*-
program linear_trend_of_time_series

  use libmxe_para, only: libmxe_para__register, clen, rho &
                     & , pi, radius, radian, radian_r, type_libmxe_para
  use libmxe_grid, only: libmxe_grid__register  &
                     & , type_libmxe_grid
  use libmxe_topo, only: libmxe_topo__register, libmxe_topo__aexl &
                     & , type_libmxe_topo
  use libmxe_io, only: libmxe_io__register, type_libmxe_io
  use libmxe_grads, only: libmxe_grads__make, type_grads
  use libmxe_stmrgn, only: libmxe_stmrgn__var2_n  &
                     & , libmxe_stmrgn__var2_x
  use libmxe_trnsfrm

  use file_open_close_manager

  implicit none

  integer(4) :: imut, jmut
  integer(4) :: ibu, ieu, jbu, jeu
  integer(4) :: ibt, iet, jbt, jet

  integer(4) :: mtin1 = 31
  integer(4) :: iflnm = 11, iflout = 20

  integer(4), allocatable :: nsum(:,:)
  real(8), allocatable :: mean(:,:)
  real(8), allocatable :: stdv(:,:)
  real(8), allocatable :: slope(:,:)
  real(8), allocatable :: yzero(:,:)

  real(4), allocatable :: indt(:,:,:)
  real(4), allocatable :: work4(:,:)
  real(8), allocatable :: x(:), y(:)

  real(4) :: a, b

  integer(4) :: iys, iye, iy
  integer(4) :: i, j, k, m, n
  integer(4) :: ndt, ndu, nmx
  integer(4) :: ndt_tmp, ndu_tmp
  integer(4) :: ii, nn, kk
  integer(4) :: iundef = -99999
  integer(4) :: ireco

  character(len=256) :: flnin, flnin_base
  character(len=256) :: flout, flout_base

  real(4) :: rmiss_in, rmiss_out

  integer(4) :: intv, istrt
  integer(4) :: irec

  integer(4) :: nbyr, neyr ! data year
  integer(4) :: ibyr, ieyr ! analysis year

  logical :: lconv_in, lconv_out

  type(type_libmxe_para) :: para
  type(type_libmxe_grid) :: grid
  type(type_libmxe_topo) :: topo
  type(type_libmxe_io) :: io
  type(type_grads) :: grads

  !-----------------------------------------------------------------------

  namelist /nml_trend/ flnin_base, rmiss_in, lconv_in, nbyr, neyr, &
       & ibyr, ieyr, istrt, intv, flout, rmiss_out, lconv_out

  open (iflnm,file='namelist.linear_trend')
  read (iflnm,nml=nml_trend)
  close(iflnm)

  !-----------------------------------------------------------------------

  nmx = neyr - nbyr + 1

  write(6,*) ' max number of data = ', nmx

  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)
  call libmxe_topo__aexl(topo,para)

  imut = para%imut
  jmut = para%jmut

  ibu = 1
  ieu = imut
  jbu = 1
  jeu = jmut

  allocate(work4(1:imut,1:jmut))

  allocate(nsum(1:imut,1:jmut))
  allocate(mean(1:imut,1:jmut), stdv(1:imut,1:jmut))
  allocate(indt(1:imut,1:jmut,1:nmx))
  allocate(slope(1:imut,1:jmut))
  allocate(yzero(1:imut,1:jmut))

  allocate(x(nmx),y(nmx))

  ! -----------------------------------------------
  
  write(6,*) 'Read Historical Data from ',nbyr,' to ',neyr

  ndt = 0 ! total
  ndu = 0 ! to be used
  n = 0

  nsum(1:imut,1:jmut) = 0
  mean(1:imut,1:jmut) = 0.d0
  indt(1:imut,1:jmut,1:nmx) = 0.e0

  do iy = nbyr, neyr

    ndt = ndt + 1

    ! ============================
    !   Input File Settings
    ! ============================

    write(flnin,'(1a,i4.4)') trim(flnin_base), iy

    if (lconv_in) then
      open(mtin1,file=flnin,form='unformatted',access='direct',convert='big_endian',recl=4*imut*jmut)
    else
      open(mtin1,file=flnin,form='unformatted',access='direct',convert='little_endian',recl=4*imut*jmut)
    endif
    write(6,*) ' Reading from ', trim(flnin)

    ! ============================
    !   Read Data
    ! ============================

    read(mtin1,rec=1) work4

    if ((ibyr <= iy) .and. (iy <= ieyr))  then
      ndu = ndu + 1
      do j = 1, jmut
        do i = 1, imut
          indt(i,j,ndt) = work4(i,j)
          if ((topo%aexl(i,j,1) == 1.0d0) .and. (indt(i,j,ndt) /= rmiss_in)) then
            nsum(i,j)    = nsum(i,j) + 1
            mean(i,j)    = mean(i,j) + real(indt(i,j,ndt),8)
          end if
        end do
      end do
    end if

    close(mtin1)

  end do

  ! -----------------------------------------------
  !   Calculate Climatology
  ! -----------------------------------------------

  write(6,*) 'Calculate Climatology'

  do j = 1, jmut
    do i = 1, imut
      if (nsum(i,j) == ndu) then
        mean(i,j) = mean(i,j) / real(ndu,8)
      else
        mean(i,j) = real(rmiss_out,8)
      end if
    end do
  end do

  ndt = 0
  ndu = 0

  nsum(1:imut,1:jmut) = 0
  stdv(1:imut,1:jmut) = 0.0d0

  do iy = nbyr, neyr
    ndt = ndt + 1
    if ((ibyr <= iy) .and. (iy <= ieyr))  then
      ndu = ndu + 1
      do j = 1, jmut
        do i = 1, imut
          if ((topo%aexl(i,j,1) == 1.0d0) .and. (indt(i,j,ndt) /= rmiss_in)) then
            nsum(i,j) = nsum(i,j) + 1
            stdv(i,j) = stdv(i,j) + (real(indt(i,j,ndt),8) - mean(i,j))**2
          end if
        end do
      end do
    end if
  end do

  ! -----------------------------------------------
  !   Calculate Linear Trend
  ! -----------------------------------------------

  do j = 1, jmut
    do i = 1, imut
      if (nsum(i,j) == ndu) then
        ndt_tmp = 0
        ndu_tmp = 0
        do iy = nbyr, neyr
          ndt_tmp = ndt_tmp + 1
          if ((ibyr <= iy) .and. (iy <= ieyr))  then
            ndu_tmp = ndu_tmp + 1
            x(ndu_tmp) = 0.5d0 + real(ndu_tmp-1,8) 
            y(ndu_tmp) = real(indt(i,j,ndt_tmp),8) - mean(i,j)
          end if
        end do
        call lsr( x, y, ndu, nmx, a, b )
        slope(i,j) = a
        yzero(i,j) = b
        stdv (i,j) = sqrt(stdv(i,j) / real(ndu-1,8))
      else
        slope(i,j) = real(rmiss_out,8)
        yzero(i,j) = real(rmiss_out,8)
        stdv (i,j) = real(rmiss_out,8)
      end if
    end do
  end do

  if (lconv_out) then
    open(iflout,file=flout,form='unformatted',access='direct',convert='big_endian',recl=4*imut*jmut)
  else
    open(iflout,file=flout,form='unformatted',access='direct',convert='little_endian',recl=4*imut*jmut)
  end if

  write(6,*) ' writing data to ... ', trim(flout)

  work4(1:imut,1:jmut) = real(mean(1:imut,1:jmut),8)
  write(iflout,rec=1) work4
  work4(1:imut,1:jmut) = real(stdv(1:imut,1:jmut),8)
  write(iflout,rec=2) work4
  work4(1:imut,1:jmut) = real(slope(1:imut,1:jmut),8)
  write(iflout,rec=3) work4
  work4(1:imut,1:jmut) = real(yzero(1:imut,1:jmut),8)
  write(iflout,rec=4) work4

  close(iflout)

end program linear_trend_of_time_series

