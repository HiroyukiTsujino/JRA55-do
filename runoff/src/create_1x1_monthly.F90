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

  real(4),allocatable :: work4 (:,:)
  real(8),allocatable :: roforg(:,:)

  integer(4) :: imut, jmut
  real(8),allocatable :: rof(:,:)

  character(256) :: cfriv
  character(256) :: file_river_in
  character(256) :: file_river_out_mon
  character(256) :: riv_in_base
  character(256) :: riv_out_base

  integer(4) :: ibyr, ieyr

  integer(4) :: nt, nyr, nm, nd, irec, i, j, ii, jj, iii, mi, mj
  integer(4) :: itmp, jtmp, ni, nj
  
  integer(4) :: irec1, irec2, irec3

  type(type_libmxe_para) :: rivp, newp
  type(type_libmxe_grid) :: rivg, newg
  type(type_libmxe_topo) :: rivt, newt

  integer(4),parameter :: nmonyr = 12
  integer(4) :: ndmon(nmonyr) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  logical :: l_transport
  real(8),parameter :: dens_pw = 1.0d3 ! [kg/m3]
  integer(4),parameter :: i_search = 5, j_search = 7

  !--------------------------------------------------------------------------

  namelist /nml_river2onedeg/ ibyr, ieyr, riv_in_base, riv_out_base, l_transport

  !--------------------------------------------------------------------------

  l_transport = .true.
  open (10, file='namelist.riv2onedeg')
  read (10, nml=nml_river2onedeg)
  close(10)

  !--------------------------------------------------------------------------
  ! set grid points

  !-- model settings --

  call libmxe_para__register(newp,file_namelist='NAMELIST.MXE.COBESST')
  call libmxe_grid__register(newg,newp)
  call libmxe_topo__register(newt,newp)
  call libmxe_topo__aexl(newt,newp)

  imut = newp%imut
  jmut = newp%jmut

  allocate(rof(1:imut,1:jmut))

  write(6,*) ' New grid ', imut, jmut
  !--------------------------------------------------------------------

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

    write(file_river_in,'(1a,i4.4)') trim(riv_in_base),nyr
    open(10,file=file_river_in,form='unformatted',status='old',access='direct',action='read',recl=4*nx*ny)

    irec = 0

    if ( lleap(nyr) ) then
      ndmon(2) = 29
    else
      ndmon(2) = 28
    end if

    do nm = 1, nmonyr

      roforg(:,:) = 0.0d0

      do nd = 1, ndmon(nm)
        irec = irec + 1
        read(10,rec=irec) work4

        !write(6,*) irec
        do j = 1, ny
          do i = 1, nx
            if (l_transport) then
              roforg(i,j) = roforg(i,j) + real(work4(i,j),8)
            else
              roforg(i,j) = roforg(i,j) + real(work4(i,j),8) * (rivg%areau(i,j) * 1.0d-4)
            end if
          end do
        end do
      end do

      roforg(:,:) = roforg(:,:) / real(ndmon(nm),8)

      rof(:,:) = 0.0d0

      do j = 1, jmut
        do i = 1, imut
          do nj = 1, 4
            do ni = 1, 4
              itmp = 4*(i-1) + ni
              jtmp = 4*(j-1) + nj
              if ((itmp > nx) .or. (jtmp > ny)) then
                write(6,*) itmp, jtmp
              endif
              rof(i,j) = rof(i,j) + roforg(itmp,jtmp)
            end do
          end do
          !if (j == 1) then
          !  write(6,*) i,j,rof(i,j)
          !end if
        end do
      end do

      !-----------------------------------------------
      ! search ocean grids

      do j = 1, jmut
        do i = 1, imut
          if ((rof(i,j) > 0.0d0) .and. (newt%aexl(i,j,1) == 0.0d0)) then
            loop_search: do mj = 1, j_search
              jj = j + mj - 1
              jj = min(jj,jmut)

              do mi = 1, i_search
                ii = i + mi - 1
                iii = ii
                if (ii > imut) iii = ii - imut
                if (newt%aexl(iii,jj,1) == 1.0d0) then
                  rof(iii,jj) = rof(iii,jj) + rof(i,j)
                  rof(i,j) = 0.0d0
                  exit loop_search
                end if

                if (mi == 1) cycle

                ii = i - mi + 1
                iii = ii
                if (ii < 1)    iii = imut - ii
                if (newt%aexl(iii,jj,1) == 1.0d0) then
                  rof(iii,jj) = rof(iii,jj) + rof(i,j)
                  rof(i,j) = 0.0d0
                  exit loop_search
                end if
              end do

              if (mj == 1) cycle loop_search

              jj = j - mj + 1
              jj = max(jj,1)

              do mi = 1, i_search
                ii = i + mi - 1
                iii = ii
                if (ii > imut) iii = ii - imut
                if (newt%aexl(iii,jj,1) == 1.0d0) then
                  rof(iii,jj) = rof(iii,jj) + rof(i,j)
                  rof(i,j) = 0.0d0
                  exit loop_search
                end if

                if (mi == 1) cycle

                ii = i - mi + 1
                iii = ii
                if (ii < 1)    iii = imut - ii
                if (newt%aexl(iii,jj,1) == 1.0d0) then
                  rof(iii,jj) = rof(iii,jj) + rof(i,j)
                  rof(i,j) = 0.0d0
                  exit loop_search
                end if
              end do

              if (mj == j_search) then
                write(6,*) ' ocean grid not found ', i,j,mj
                stop
              end if

            end do loop_search
          end if
        end do
      end do

      !-----------------------------------------------


      do j = 1, jmut
        do i = 1, imut
          !if (j == 1) then
          !  write(6,*) rof(i,j)
          !end if
          if ((rof(i,j) > 0.0d0) .and. (newt%aexl(i,j,1) == 0.0d0)) then
            write(6,*) ' There is still runoff on land at ', i, j
            stop
          else
            rof(i,j) = rof(i,j) * 1.0d4 / newg%areau(i,j) ! area [cm2] -> [m2], product kg/m2/s
          end if
        end do
      end do

      write(file_river_out_mon,'(1a,i4.4,i2.2)') trim(riv_out_base),nyr,nm
      write(6,*) ' data written to ....', trim(file_river_out_mon)
      open (30,file=file_river_out_mon,form='unformatted',access='direct',recl=4*imut*jmut)
      write(30,rec=1) real(rof,4)
      close(30)

    end do

    close(10)

  end do

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
