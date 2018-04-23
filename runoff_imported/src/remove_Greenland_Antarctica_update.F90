!-*-F90-*-
program remote_Greenland_Antarctica

  implicit none

  integer(4),parameter :: nx = 1440, ny = 720

  character(len=64) :: cfile_org = 'data_org/runoff????.grd'
  character(len=64) :: cfile_ice='data_org/runoff_ice.grd'

  character(len=64) :: cfile_new = 'data_new/runoff.????' 
  character(len=64) :: cfile_Gld = 'data_new/runoff_Greenland.????' 

  character(len=64) :: cfile_new_total='data_new/total_runoff.????'
  character(len=64) :: cfile_GLD_total='data_new/total_runoff_Greenland.????'
  character(len=64) :: cfile_Antarctica_total='data_new/total_Antarctica.cnst'

  integer(4),parameter :: iunit1 = 11, iunit2 = 12
  integer(4),parameter :: ounit1 = 21, ounit2 = 22, ounit3 = 23, ounit4 = 24

  real(4) :: runoff_org(nx,ny), runoff_new(nx,ny), runoff_ice(nx,ny)
  real(4) :: runoff_Gld(nx,ny) 

  integer(4) :: nyr, irec

  real(8) :: total_runoff, total_runoff_Greenland         
  real(8) :: total_Greenland_ice, total_Greenland_liquid, total_Antarctic_ice

  real(4) :: rtotal
  integer(4) :: ileap

  character(len=64) :: cfile_CF = 'data_Antarctica/CF_025x025.dat'
  real(4) :: runoff_CF(nx,ny)
  character(len=64) :: cfile_BMF = 'data_Antarctica/BMF_025x025.dat'
  real(4) :: runoff_BMF(nx,ny)
  real(4) :: runoff_Ant_Total(nx,ny)

  integer(4), parameter :: next_Ant = -777
  integer(4), parameter :: next_Grn = -888

  integer(4) :: i, j
  integer(4) :: ibyr, ieyr, nd_last, id_last

  !-------------------------------------------

  namelist /nml_river_update/ ibyr, ieyr, nd_last

  !-------------------------------------------

  nd_last = -9
  open(11,file='namelist_runoff_update')
  read(11,nml=nml_river_update)
  close(11)

  !-------------------------------------------

  open(iunit2,file=cfile_ice,form='unformatted',access='direct',recl = 4*nx*ny)
  read(iunit2,rec=1) runoff_ice
  close(iunit2)
  
  open(iunit2,file=cfile_CF,form='unformatted',access='direct',recl = 4*nx*ny)
  read(iunit2,rec=1) runoff_CF
  close(iunit2)

  open(iunit2,file=cfile_BMF,form='unformatted',access='direct',recl = 4*nx*ny)
  read(iunit2,rec=1) runoff_BMF
  close(iunit2)

  runoff_Ant_Total(:,:) = runoff_CF(:,:)  + runoff_BMF(:,:)

  total_Antarctic_ice = 0.d0
  do j = 1, ny
    do i = 1, nx
      total_Antarctic_ice = total_Antarctic_ice + runoff_Ant_total(i,j)
    end do
  end do
  total_Antarctic_ice = total_Antarctic_ice * 1.d-6
  write(*,'(A,F12.5,A)') 'total_Antarctic_ice', total_Antarctic_ice, ' Sv'

  open (iunit2,file=cfile_Antarctica_total,form='unformatted',access='direct',recl=4)
  write(iunit2,rec=1) real(total_Antarctic_ice,4)
  close(iunit2)

  do nyr = ibyr, ieyr

    write(cfile_org(16:19),'(I4.4)') nyr
    write(cfile_new(17:20),'(I4.4)') nyr
    write(cfile_Gld(27:30),'(I4.4)') nyr
    
    write(cfile_new_total(23:26),'(I4.4)') nyr
    write(cfile_GLD_total(33:36),'(I4.4)') nyr
    write(*,*) trim(cfile_org)

    if ( mod(nyr,4) == 0 ) then
      ileap = 1
    else
      ileap = 0
    end if

    open(iunit1, file=cfile_org, form='unformatted', access='direct', recl = 4*nx*ny)

    open(ounit1, file=cfile_new, form='unformatted', access='direct', recl = 4*nx*ny)
    open(ounit2, file=cfile_Gld, form='unformatted', access='direct', recl = 4*nx*ny)

    open(ounit3, file=cfile_new_total, form='unformatted', access='direct', recl = 4)
    open(ounit4, file=cfile_Gld_total, form='unformatted', access='direct', recl = 4)

    id_last = 365 + ileap
    if ((nyr == ieyr) .and. (nd_last /= -9)) then
      id_last = nd_last
    end if

    do irec = 1, id_last
      read(iunit1,rec=irec) runoff_org
      call remove_Greenland
      call write_newrunoff
      call write_Greenland
    end do

    close(iunit1)
    close(ounit1)
    close(ounit2)
    close(ounit3)
    close(ounit4)

  end do

contains
subroutine remove_Greenland
  integer(4) :: i, j  

  total_Greenland_ice    = 0.d0
  total_Greenland_liquid = 0.d0
  do j = ny/2+1, ny
    do i = 1, nx
      if ( runoff_ice(i,j) > 0 ) then  
        total_Greenland_liquid = total_Greenland_liquid + runoff_org(i,j)
        total_Greenland_ice    = total_Greenland_ice    + runoff_ice(i,j)
        runoff_Gld(i,j)  = runoff_org(i,j) + runoff_ice(i,j)
        runoff_org(i,j)  = 0.d0
      end if
    end do
  end do
end subroutine remove_Greenland

subroutine write_newrunoff
  integer(4) :: i, j  
  real(4) :: runoff_tmp(nx,ny)


  do j = 1, ny
    do i = 1, nx
      if ( i <= nx/2 ) then
        runoff_tmp(i,j) = runoff_org(i+nx/2,j)
      else
        runoff_tmp(i,j) = runoff_org(i-nx/2,j)
      end if
    end do
  end do

  write(ounit1,rec=irec)  runoff_tmp

  total_runoff = 0.d0
  do j = 1, ny
    do i = 1, nx
      total_runoff = total_runoff + runoff_tmp(i,j)
    end do
  end do
  rtotal = total_runoff * 1.d-6
  write(ounit3,rec=irec) rtotal


end subroutine write_newrunoff


subroutine write_Greenland
  integer(4) :: i, j  
  real(4) :: runoff_tmp(nx,ny)

  do j = 1, ny
    do i = 1, nx
      if ( i <= nx/2 ) then
        runoff_tmp(i,j) = runoff_Gld(i+nx/2,j)
      else
        runoff_tmp(i,j) = runoff_Gld(i-nx/2,j)
      end if
    end do
  end do

  write(ounit2,rec=irec) runoff_tmp

  total_runoff_Greenland = 0.d0
  do j = 1, ny
    do i = 1, nx
      total_runoff_Greenland = total_runoff_Greenland + runoff_tmp(i,j)
    end do
  end do
  rtotal = total_runoff_Greenland*1.d-6
  write(ounit4,rec=irec) rtotal

end subroutine write_Greenland

end program remote_Greenland_Antarctica
