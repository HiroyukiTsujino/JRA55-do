!-*-F90-*-
program main

  use libmxe_para, only: libmxe_para__register, clen &
                     & , pi, radius, radian, radian_r, type_libmxe_para
  use libmxe_grid, only: libmxe_grid__register  &
                     & , type_libmxe_grid
  use libmxe_topo, only: libmxe_topo__register, libmxe_topo__aexl &
                     & , type_libmxe_topo
  use libmxe_trnsfrm

  use file_open_close_manager

  implicit none

  integer(4) :: nx, ny
  real(4),allocatable :: work4(:,:)
  real(8),allocatable :: roforg(:,:)

  character(256) :: cfriv
  character(256) :: file_river_in
  character(256) :: file_river_out
  character(256) :: riv_in_base
  character(256) :: riv_out_base
  character(256) :: riv_area

  integer(4) :: ibyr, ieyr

  integer(4) :: nt, nyr, nm, nd, irec, i, j, ii, jj
  integer(4) :: itmp, jtmp, ni, nj
  
  integer(4) :: irec1, irec2, irec3

  type(type_libmxe_para) :: rivp
  type(type_libmxe_grid) :: rivg

  integer(4),parameter :: nmonyr = 12
  integer(4) :: ndmon(nmonyr) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  real(8),parameter :: dens_pw = 1.0d3

  !--------------------------------------------------------------------------

  namelist /nml_river_final/ ibyr, ieyr, riv_in_base, riv_out_base, riv_area

  !--------------------------------------------------------------------------

  open(10,file='namelist.river_final')
  read(10,nml=nml_river_final)
  close(10)

  !--------------------------------------------------------------------------
  ! set grid points

  call libmxe_para__register(rivp,file_namelist='NAMELIST.MXE.GLBRIVER')
  call libmxe_grid__register(rivg,rivp)

  nx = rivp%imut
  ny = rivp%jmut

  allocate(work4(1:nx,1:ny))
  allocate(roforg(1:nx,1:ny))

  write(6,*) ' original grid ', nx, ny

  !--------------------------------------------------------------------

  do nyr = ibyr, ieyr

    write(*,*) nyr

    write(file_river_in,'(1a,i4.4,1a)') trim(riv_in_base),nyr,'.bin'
    open(10,file=file_river_in,form='unformatted',status='old',access='direct',action='read',recl=4*nx*ny)

    write(file_river_out,'(1a,i4.4)') trim(riv_out_base),nyr
    write(6,*) ' data written to ....', trim(file_river_out)
    open (30,file=file_river_out,form='unformatted',access='direct',recl=4*nx*ny)
    irec = 0

    if ( lleap(nyr) ) then
      ndmon(2) = 29
    else
      ndmon(2) = 28
    end if

    do nm = 1, nmonyr
      do nd = 1, ndmon(nm)

        irec = irec + 1

        read(10,rec=irec) work4

        do j = 1, ny
          do i = 1, nx
            roforg(i,j) = real(work4(i,j),8)
          end do
        end do

        do j = 1, ny
          do i = 1, nx
            ! product: [m3/s] / [m2] * [kg/m3] = [kg/m2/s]
            roforg(i,j) = roforg(i,j) * (1.0d4 / rivg%areau(i,j)) * dens_pw ! area: [cm2] -> [m2]
          end do
        end do

        write(30,rec=irec) real(roforg,4)

      end do
    end do

    close(30)
    close(10)

  end do

  write(6,*) ' Grid cell area written to ....', trim(riv_area)

  open (30,file=riv_area,form='unformatted',access='direct',recl=4*nx*ny)
  write(30,rec=1) real(rivg%areau(1:nx,1:ny)*1.0d-4,4)
  close(30)

contains

  logical function lleap( year )
    implicit none
    integer(4) :: year
    logical :: lflg
    if ( mod(year,4) == 0 ) then
      if ( mod(year,100) == 0 ) then
        if ( mod(year,400) == 0 ) then
          lflg = .true.
        else
          lflg = .false.
        end if
      else
        lflg = .true.
      end if
    else
      lflg = .false.
    end if
    lleap = lflg
  end function lleap

end program main
