!-*-F90-*-
program cobe_to_jra55_mask

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

  type(type_libmxe_para) :: jrap
  type(type_libmxe_grid) :: jrag
  type(type_libmxe_topo) :: jrat

  character(256) :: file_namelist_jra
  character(256) :: file_mask_jra
  character(256) :: file_mask_out

  integer(4) :: mtinf, mtot
  integer(4) :: ireclen

  integer(4) :: nxf, nyf

  real(4),allocatable :: workf(:,:)
  real(4) :: undef_in_jra
  real(8),allocatable :: alonuf(:), alatuf(:)
  real(8),allocatable :: alontf(:), alattf(:)
  real(8),allocatable :: mask_jra(:,:)
  real(8),allocatable :: mask_cobe_jra(:,:)

  !------------------------------------------------------------

  type(type_libmxe_para) :: cobep
  type(type_libmxe_grid) :: cobeg

  character(256) :: file_namelist_cobe
  character(256) :: file_mask_cobe

  integer(4) :: mtinc

  integer(4) :: nxc, nyc
  real(4),allocatable :: workc(:,:)
  real(4) :: undef_in_cobe
  real(8),allocatable :: alonuc(:), alatuc(:)
  real(8),allocatable :: alontc(:), alattc(:)
  real(8),allocatable :: mask_cobe(:,:)

  !------------------------------------------------------------

  integer(4) :: i, j, ii, jj, iii, jjj
  
  character(1),allocatable :: indx(:,:)
  character(64) :: flout
  character(50) :: rform

  integer(4),allocatable :: idiv(:)
  integer(4),allocatable :: jdiv(:)
  integer(4) :: nidiv, njdiv
  integer(4) :: num_width = 250

  integer(4) :: k,mi,mj,n,length
  integer(4) :: i0,i1,j0,j1

  character(len=16),save :: cdir   !- diretory

  integer(4),parameter :: lun = 10

  logical :: l_read_index

  !--------------------------------------------------------------------------

  namelist /nml_cobe_to_jra/ &
       & file_namelist_jra, file_namelist_cobe, &
       & file_mask_jra, file_mask_cobe, &
       & file_mask_out, &
       & undef_in_jra, undef_in_cobe, &
       & cdir, l_read_index

  !--------------------------------------------------------------------------

  open(10,file='namelist.cobe_to_jra_mask')
  read(10,nml=nml_cobe_to_jra)
  close(10)

  !--------------------------------------------------------------------------

  call libmxe_para__register(jrap,file_namelist=file_namelist_jra)
  call libmxe_grid__register(jrag,jrap)
  call libmxe_topo__register(jrat,jrap)

  nxf = jrap%imut
  nyf = jrap%jmut
  allocate(alonuf(1:nxf), alatuf(1:nyf))
  allocate(alontf(1:nxf), alattf(1:nyf))
  alonuf(1:nxf) = jrag%lonu(1:nxf)
  alatuf(1:nyf) = jrag%latu(1:nyf)
  alontf(1:nxf) = jrag%lont(1:nxf)
  alattf(1:nyf) = jrag%latt(1:nyf)

  allocate(workf(1:nxf,1:nyf))
  allocate(mask_jra(1:nxf,1:nyf))
  allocate(mask_cobe_jra(1:nxf,1:nyf))

  !--------------------------------------------------------------------------

  call libmxe_para__register(cobep,file_namelist=file_namelist_cobe)
  call libmxe_grid__register(cobeg,cobep)

  nxc = cobep%imut
  nyc = cobep%jmut
  allocate(alonuc(1:nxc), alatuc(1:nyc))
  allocate(alontc(1:nxc), alattc(1:nyc))
  alonuc(1:nxc) = cobeg%lonu(1:nxc)
  alatuc(1:nyc) = cobeg%latu(1:nyc)
  alontc(1:nxc) = cobeg%lont(1:nxc)
  alattc(1:nyc) = cobeg%latt(1:nyc)

  allocate(workc(1:nxc,1:nyc))
  allocate(mask_cobe(1:nxc,1:nyc))

  !--------------------------------------------------------------------------
  IF_READ_INDEX: if (l_read_index) then

  ! JRA mask
  !
  !ireclen = 4*nxf*nyf
  !call open_file_direct(mtinf,file_mask_jra,ireclen)
  !write(6,*) ' Read MASK (Jra) from ', trim(file_mask_jra)
  !read(mtinf,rec=1) workf
  !call close_file(lun)
  !mask_jra(1:nxf,1:nyf) = real(workf(1:nxf,1:nyf),8)

  allocate(indx(1:nxf,1:nyf))

  nidiv = nxf / num_width  + 2
  njdiv = nyf / num_width  + 2

  allocate(idiv(1:nidiv))
  allocate(jdiv(1:njdiv))

  idiv(1) = 0
  jdiv(1) = 0

  do mi = 2, nidiv - 1
    idiv(mi) = idiv(mi-1) + num_width
  end do
  idiv(nidiv) = nxf

  do mj = 2, njdiv - 1
    jdiv(mj) = jdiv(mj-1) + num_width
  end do
  jdiv(njdiv) = nyf

  write(6,*) 'nidiv = ', nidiv
  write(6,*) 'njdiv = ', njdiv
  write(6,*) 'i = ',(idiv(mi),mi=1,nidiv)
  write(6,*) 'j = ',(jdiv(mj),mj=1,njdiv)

  n = 0
  do mj = 1, njdiv - 1
    do mi = 1, nidiv - 1
      n = n + 1
      write(flout,'(1a,1a,i2.2,a4)') trim(cdir),'/indx',n,'.dat'
      length = idiv(mi+1) - idiv(mi)
      write(rform,'(A4,I3,A3)') '(1X,',length,'A1)'
      write(6,*) 'format: ',rform
      open (lun,file=flout)
      do j = jdiv(mj+1),jdiv(mj)+1,-1
        read(lun,rform) (indx(i,j),i=idiv(mi)+1,idiv(mi+1))
      end do
      close(lun)
    end do
  end do

  do j = 1, nyf
    do i = 1, nxf
      select case(indx(i,j)) 
      case('@')
        mask_jra(i,j) = 0.0
      case('1')
        mask_jra(i,j) = 1.0
      case('2')
        mask_jra(i,j) = 2.0
      case('3')
        mask_jra(i,j) = 3.0
      case('4')
        mask_jra(i,j) = 4.0
      case('5')
        mask_jra(i,j) = 5.0
      case('6')
        mask_jra(i,j) = 6.0
      case('7')
        mask_jra(i,j) = 7.0
      case('8')
        mask_jra(i,j) = 8.0
      case('9')
        mask_jra(i,j) = 9.0
      end select
    end do
  end do

  deallocate(indx)

  else

  !--------------------------------------------------------------------------
  ! COBESST mask
  !
  ireclen = 4*nxc*nyc
  call open_file_direct(mtinc,file_mask_cobe,ireclen)
  write(6,*) ' Read MASK (Cobe) from ', trim(file_mask_cobe)
  read(mtinc,rec=1) workc
  call close_file(mtinc)
  mask_cobe(1:nxc,1:nyc) = real(workc(1:nxc,1:nyc),8)
  !
  !-------------------------------------------------------------------------

  do j = 1, nyf
    do i = 1, nxf
      do jj = 1, nyc - 1
        if ((alattc(jj) < alatuf(j)) .and. (alatuf(j) <= alattc(jj+1))) then
          do ii = 1, nxc - 1
            if ((alontc(ii) < alonuf(i)) .and. (alonuf(i) <= alontc(ii+1))) then
              mask_cobe_jra(i,j) = mask_cobe(ii,jj)
            end if
          end do
          if (alontc(nxc) < alonuf(i)) then
            mask_cobe_jra(i,j) = mask_cobe(nxc,jj)
          end if
        end if
      end do
      if (alattc(nyc) < alatuf(j)) then
        do ii = 1, nxc - 1
          if ((alontc(ii) < alonuf(i)) .and. (alonuf(i) <= alontc(ii+1))) then
            mask_cobe_jra(i,j) = mask_cobe(ii,nyc)
          end if
        end do
        if (alontc(nxc) < alonuf(i)) then
          mask_cobe_jra(i,j) = mask_cobe(nxc,nyc)
        end if
      end if
    end do
  end do

  !------

  do j = 1, nyf
    do i = 1, nxf
      if (jrat%ho4(i,j) > 0) then
        if (mask_cobe_jra(i,j) /= 0.0) then
          mask_jra(i,j) = mask_cobe_jra(i,j)
        else
          if (jrag%latu(j) < -60.0d0) then
            mask_jra(i,j) = 9
          else
            if (jrag%latu(j) > 70.0d0) then
              mask_jra(i,j) = 1
            else
              mask_jra(i,j) = -9
            end if
          end if
        end if
      else
        mask_jra(i,j) = 0
      end if
    end do
  end do

  !------

  allocate(indx(1:nxf,1:nyf))

  do j = 1, nyf
    do i = 1, nxf
      select case(nint(mask_jra(i,j))) 
      case(0)
        indx(i,j) = '@'
      case(1)
        indx(i,j) = '1'
      case(2)
        indx(i,j) = '2'
      case(3)
        indx(i,j) = '3'
      case(4)
        indx(i,j) = '4'
      case(5)
        indx(i,j) = '5'
      case(6)
        indx(i,j) = '6'
      case(7)
        indx(i,j) = '7'
      case(8)
        indx(i,j) = '8'
      case(9)
        indx(i,j) = '9'
      case(-9)
        indx(i,j) = 'f'
      end select
    end do
  end do

  nidiv = nxf / num_width  + 2
  njdiv = nyf / num_width  + 2

  allocate(idiv(1:nidiv))
  allocate(jdiv(1:njdiv))

  idiv(1) = 0
  jdiv(1) = 0

  do mi = 2, nidiv - 1
    idiv(mi) = idiv(mi-1) + num_width
  end do
  idiv(nidiv) = nxf

  do mj = 2, njdiv - 1
    jdiv(mj) = jdiv(mj-1) + num_width
  end do
  jdiv(njdiv) = nyf

  write(6,*) 'nidiv = ', nidiv
  write(6,*) 'njdiv = ', njdiv
  write(6,*) 'i = ',(idiv(mi),mi=1,nidiv)
  write(6,*) 'j = ',(jdiv(mj),mj=1,njdiv)

  n = 0
  do mj = 1, njdiv - 1
    do mi = 1, nidiv - 1
      n = n + 1
      write(flout,'(1a,1a,i2.2,a4)') trim(cdir),'/indx',n,'.dat'
      length = idiv(mi+1) - idiv(mi)
      write(rform,'(A4,I3,A3)') '(1X,',length,'A1)'
      write(6,*) 'format: ',rform
      open (lun,file=flout)
      do j = jdiv(mj+1),jdiv(mj)+1,-1
        write(lun,rform) (indx(i,j),i=idiv(mi)+1,idiv(mi+1))
      end do
      close(lun)
    end do
  end do

  deallocate(indx)

  end if IF_READ_INDEX

  !-------------------------------------------------------------

  do j = 1, nyf
    do i = 1, nxf
      if (jrat%ho4(i,j) > 0) then
        if (mask_jra(i,j) == 0.0) then
          write(6,*) ' index and mask is inconsistent at ', alonuf(i), alatuf(j), mask_jra(i,j)
        end if
      else
        if (mask_jra(i,j) > 0.0) then
          write(6,*) ' index and mask is inconsistent at ', alonuf(i), alatuf(j), mask_jra(i,j)
        end if
      end if
    end do
  end do

  ireclen = 4*nxf*nyf

  call open_file_direct(mtot,file_mask_out,ireclen)
  write(6,*) ' writing to ', trim(file_mask_out)

  write(mtot,rec=1) real(mask_jra(1:nxf,1:nyf),4)

  call close_file(mtot)

  deallocate(alonuf, alatuf)
  deallocate(alontf, alattf)
  deallocate(workf)

  deallocate(alonuc, alatuc)
  deallocate(alontc, alattc)
  deallocate(workc)

  !-------------------------------------------------------

  write(6,*) ' Operation ended '
  
end program cobe_to_jra55_mask
