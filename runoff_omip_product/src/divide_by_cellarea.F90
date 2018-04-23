!-*-F90-*-
program divide_by_cell_area

  use libmxe_para, only: libmxe_para__register &
                     & , clen, pi, radian, radian_r, radius &
                     & , type_libmxe_para
  use libmxe_grid, only: libmxe_grid__register  &
                     & , type_libmxe_grid
  use libmxe_topo, only: libmxe_topo__register, libmxe_topo__aexl &
                     & , type_libmxe_topo
  use libmxe_io, only: libmxe_io__register, type_libmxe_io
  use libmxe_calendar

  use file_open_close_manager

  implicit none

  !------------------------------------------------------------

  type(type_libmxe_para) :: camap
  type(type_libmxe_grid) :: camag

  character(256) :: file_namelist_cama
  character(256) :: file_mask_cama
  character(256) :: file_river_in
  character(256) :: file_river_out
  character(256) :: file_area_out

  integer(4) :: num_data
  integer(4) :: mtinm
  integer(4) :: mtot, mtota
  integer(4) :: ireclen
  logical :: l_area_out

  integer(4) :: nxf, nyf
  real(8),allocatable :: rofa(:,:), rofm(:,:)
  real(4),allocatable :: workf(:,:)
  real(4) :: undef_in_cama
  real(8),allocatable :: mask_cama(:,:)
  real(8),allocatable :: area_cama(:,:)

  real(8) :: total_main

  !------------------------------------------------------------

  integer(4) :: nd, irec, i, j
  integer(4) :: irec1, irec2, irec3

  !------------------------------------------------------------

  real(8),parameter :: dens_pw = 1.0d3 ! density of pure water

  !--------------------------------------------------------------------------

  namelist /nml_divide_by_area/ &
       & file_namelist_cama,    &
       & file_mask_cama,        &
       & file_river_in,         &
       & file_river_out,        &
       & num_data,              &
       & l_area_out,            &
       & file_area_out

  !--------------------------------------------------------------------------

  open(10,file='namelist.divide_by_cellarea')
  read(10,nml=nml_divide_by_area)
  close(10)

  !--------------------------------------------------------------------------

  call libmxe_para__register(camap,file_namelist=file_namelist_cama)
  call libmxe_grid__register(camag,camap)

  nxf = camap%imut
  nyf = camap%jmut

  ireclen = 4*nxf*nyf

  allocate(workf(1:nxf,1:nyf))
  allocate(mask_cama(1:nxf,1:nyf))
  allocate(area_cama(1:nxf,1:nyf))
  allocate(rofa(1:nxf,1:nyf))
  allocate(rofm(1:nxf,1:nyf))

  area_cama(1:nxf,1:nyf) = camag%areau(1:nxf,1:nyf) * 1.0d-4 ! cgs => MKS

  !-------------------------------------------------------------

  if (l_area_out) then
    call open_file_direct(mtota,file_area_out,ireclen)
    write(6,*) ' Writing area to ', trim(file_area_out)
    write(mtota,rec=1) real(area_cama(1:nxf,1:nyf),4)
    call close_file(mtota)
  end if

  !-------------------------------------------------------------

  call open_file_direct(mtinm,file_river_in,ireclen)
  write(6,*) ' reading from ', trim(file_river_in)

  call open_file_direct(mtot,file_river_out,ireclen)
  write(6,*) ' writing to ', trim(file_river_out)

  do nd = 1, num_data

    total_main = 0.0d0

    read(mtinm,rec=nd) workf(1:nxf,1:nyf)
    rofa(1:nxf,1:nyf) = real(workf(1:nxf,1:nyf),8)

    do j = 1, nyf
      do i = 1, nxf
        rofm(i,j) = rofa(i,j) / area_cama(i,j) * dens_pw ! [kg/m2/s]
      end do
    end do

    write(mtot,rec=nd) real(rofm(1:nxf,1:nyf),4)

    total_main = total_main + sum(rofa)
    !write(6,*) sum(rofa)
  end do

  write(6,*) real(total_main/real(num_data,8),4)
  
  write(6,*) ' closing file, total record = ', num_data

  call close_file(mtot)
  call close_file(mtinm)

  !-------------------------------------------------------

  deallocate(workf)
  deallocate(rofa)
  deallocate(rofm)

  deallocate(mask_cama)

  write(6,*) ' Operation ended '

end program divide_by_cell_area
