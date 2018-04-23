! -*-F90-*-
program runoff_area_integ_antarctica

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
  real(8) :: runoff_binned(36) ! 0-10, ..., 350-360

  real(4),allocatable :: work4(:,:)
  real(8),allocatable :: run_off(:,:)

  
  integer(4),allocatable :: indx_coast(:,:), indx_shelf(:,:)

  !-----

  integer(4), parameter :: mtin1 = 71, mtin2 = 72, mtot1 = 81, mtot2 = 82

  character(256) :: flnin, flnot

  character(256) :: file_index
  integer(4)     :: irec_coast, irec_shelf

  integer(4) :: irec1, irec2, irec3
  integer(4) :: i, j, k, m, n

  type(type_libmxe_para) :: rivp
  type(type_libmxe_grid) :: rivg

  logical :: l_done

  !-----------------------------------------------------------------------

  namelist /nml_runoff_integ_antarc/ &
       & flnin, flnot, &
       & file_index, irec_coast, irec_shelf

  open (11,file='namelist.runoff_integ_antarc')
  read (11,nml=nml_runoff_integ_antarc)
  close(11)

  !--------------------------------------------------------------------------
  ! set grid points

  !-- model settings --

  call libmxe_para__register(rivp,file_namelist='NAMELIST.MXE.GLBRIVER')
  call libmxe_grid__register(rivg,rivp)

  imut = rivp%imut
  jmut = rivp%jmut

  allocate(run_off(1:imut,1:jmut))
  allocate(work4(1:imut,1:jmut))

  allocate(indx_coast(1:imut,1:jmut))
  allocate(indx_shelf(1:imut,1:jmut))

  write(6,*) ' Grid ', imut, jmut

  open (mtin2, file=file_index, access='direct', recl=4*imut*jmut)
  write(6,'(1a,1a)') 'index data read from ... ', trim(file_index)
  read (mtin2, rec=irec_shelf) indx_shelf
  read (mtin2, rec=irec_coast) indx_coast
  close(mtin2)

  !------

  open (mtin1, file=flnin, access='direct', recl=4*imut*jmut)
  write(6,'(1a,1a)') 'run off data read from ... ', trim(flnin)

  read(mtin1,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
  run_off(1:imut,1:jmut) = real(work4(1:imut,1:jmut),8)

  LOOP_J: do j = 1, jmut
    LOOP_I: do i = 1, imut
      if (run_off(i,j) /= 0.0d0) then
        l_done = .false.
        if (294.5d0 < rivg%lonu(i) .and. rivg%lonu(i) < 305.d0) then
          if (indx_coast(i,j) == 5) then
            runoff_binned(30) = runoff_binned(30) + run_off(i,j)
            l_done = .true.
          end if
          if (indx_coast(i,j) == 6) then
            runoff_binned(31) = runoff_binned(31) + run_off(i,j)
            l_done = .true.
          end if
          if (indx_shelf(i,j) == 28 .or. indx_shelf(i,j) == 29) then
            runoff_binned(31) = runoff_binned(31) + run_off(i,j)
            l_done = .true.
          end if
          if (.not. l_done) then
            write(6,*) ' Binning imcomplete, please check data '
            stop
          else
            cycle LOOP_I
          end if
        end if
        do n = 1, 36
          if (10.0d0 * (n-1) < rivg%lonu(i) .and. rivg%lonu(i) < 10.0d0 * n) then
            runoff_binned(n) = runoff_binned(n) + run_off(i,j)
            l_done = .true.
          end if
        end do
        if (.not. l_done) then
          write(6,*) ' Cannot find appropriate bin '
          stop
        else
          cycle LOOP_I
        end if
      end if
    end do LOOP_I
  end do LOOP_J

  do n = 1, 36
    write(6,*) n, runoff_binned(n) * 1.d-3
  end do

  open (mtot1, file=flnot, access='direct', recl=4*36)
  write(*,*) ' GrADs data written to... ', trim(flnot)
  write(mtot1,rec=irec1) real(runoff_binned(1:36),4)
  close(mtot1)

end program runoff_area_integ_antarctica
