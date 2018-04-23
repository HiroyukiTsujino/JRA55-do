! -*-F90-*-
program runoff_area_integ

  use libmxe_para, only: libmxe_para__register, clen &
                     & , pi, radius, radian, radian_r, type_libmxe_para
  use libmxe_grid, only: libmxe_grid__register  &
                     & , type_libmxe_grid
  use libmxe_topo, only: libmxe_topo__register, libmxe_topo__aexl &
                     & , type_libmxe_topo
  use libmxe_trnsfrm

  use file_open_close_manager

  implicit none

  integer(4) :: imut, jmut
  integer(4) :: num_index

  real(4),allocatable :: work4(:,:)
  integer(4),allocatable :: indx(:,:)
  real(8),allocatable :: run_off(:,:)
  real(8),allocatable :: area_rad(:,:)
  real(8),allocatable :: integ_mon(:), integ_ann(:)

  real(8) :: areag, areaarc
  real(8) :: area_earth, area_this

  !-----

  integer(4), parameter :: mtin1 = 71, mtin2 = 72, mtot1 = 81, mtot2 = 82
  character(256) :: flnin, flnot, flnav
  character(256) :: flnin_base, flnot_base, flnav_base
  character(256) :: file_index

  integer(4) :: nday, month, nyear
  integer(4), parameter :: ndata = 12
  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  real(8), parameter :: alats = -10.0d0, alatn = 10.d0
  real(8), parameter :: alonw =   0.0d0, alone = 360.d0
  real(8), parameter :: amin = 0.15d0

  integer(4) :: irec1, irec2, irec3
  integer(4) :: i, j, k, m, n
  integer(4) :: nbyr, neyr

  type(type_libmxe_para) :: rivp
  type(type_libmxe_grid) :: rivg

  !-----------------------------------------------------------------------

  namelist /nml_runoff_integ/ &
       & flnin_base, flnot_base, flnav_base, file_index, &
       & nbyr, neyr, num_index

  open (11,file='namelist.runoff_integ')
  read (11,nml=nml_runoff_integ)
  close(11)

  !--------------------------------------------------------------------------
  ! set grid points

  !-- model settings --

  call libmxe_para__register(rivp,file_namelist='NAMELIST.MXE.RUNOFF_CORE')
  call libmxe_grid__register(rivg,rivp)

  imut = rivp%imut
  jmut = rivp%jmut

  allocate(run_off(1:imut,1:jmut))
  allocate(area_rad(1:imut,1:jmut))
  allocate(work4(1:imut,1:jmut))
  allocate(indx(1:imut,1:jmut))
  allocate(integ_mon(0:num_index), integ_ann(0:num_index))

  write(6,*) ' Grid ', imut, jmut

  open (mtin1, file=file_index, access='direct', recl=4*imut*jmut)
  write(6,'(1a,1a)') 'index data read from ... ', trim(file_index)
  read(mtin1,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
  indx(1:imut,1:jmut) = int(work4(1:imut,1:jmut)+1.0d-4)
  close(mtin1)

  ! read temperature and salinity

  do nyear = nbyr, neyr

    integ_ann(1:num_index) = 0.0d0

    write(flnin,'(1a,i4.4,1a)') trim(flnin_base),nyear,'.dat'
    open (mtin1, file=flnin, access='direct', recl=4*imut*jmut)
    write(6,'(1a,1a)') 'run off data read from ... ', trim(flnin)
    irec3 = 0

    do month = 1, 12

      integ_mon(0:num_index) = 0.0d0

      write(6,*) ' month = ', month

      irec3 = irec3 + 1
      read(mtin1,rec=irec3) ((work4(i,j),i=1,imut),j=1,jmut)
      run_off(1:imut,1:jmut) = real(work4(1:imut,1:jmut),8)

      irec3 = irec3 + 1
      read(mtin1,rec=irec3) ((work4(i,j),i=1,imut),j=1,jmut)
      area_rad(1:imut,1:jmut) = real(work4(1:imut,1:jmut),8)

      do j = 1, jmut
        do i = 1, imut
          integ_mon(0) = integ_mon(0) + run_off(i,j) * area_rad(i,j)
          do n = 1, num_index
            if (indx(i,j) == n) then
              integ_mon(n) = integ_mon(n) + run_off(i,j) * area_rad(i,j)
            end if
          end do
        end do
      end do

      do n = 0, num_index
        integ_ann(n) = integ_ann(n) + integ_mon(n) * real(ndmon(month),8)
      end do

      write(flnot,'(1a,i4.4,i2.2)') trim(flnot_base),nyear,month
      open(mtot1, file=flnot, access='direct', recl=4)
      write(*,*) ' GrADs data written to... ', trim(flnot)
      irec1 = 0

      do n = 0, num_index
        irec1 = irec1 + 1
        write(mtot1,rec=irec1) real(integ_mon(n),4)
      end do
      close(mtot1)

    end do

    do n = 0, num_index
      integ_ann(n) = integ_ann(n) / 365.0d0
      write(6,*) nyear, n, integ_ann(n) * 1.0d-9
    end do

    write(flnav,'(1a,i4.4)') trim(flnav_base),nyear
    open(mtot2, file=flnav, access='direct', recl=4)
    write(*,*) ' GrADs data written to... ', trim(flnav)
    irec2 = 0

    do n = 0, num_index
      irec2 = irec2 + 1
      write(mtot2,rec=irec2) real(integ_ann(n),4)
    end do

    close(mtot2)
    close(mtin1)

  end do

end program runoff_area_integ
