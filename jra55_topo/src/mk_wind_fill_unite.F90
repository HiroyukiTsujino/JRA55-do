! -*-F90-*-
!==================================================================!
!                                                                  !
!                    Program mk_mask_unite                         !
!                                                                  !
!------------------------------------------------------------------!
program main

  use libmxe_para, only: libmxe_para__register, type_libmxe_para
  use libmxe_grid, only: libmxe_grid__register, type_libmxe_grid
  use libmxe_topo, only: libmxe_topo__register, type_libmxe_topo, &
       & libmxe_topo__aexl

  use libmxe_io, only: libmxe_io__register, type_libmxe_io
  use libmxe_grads, only: libmxe_grads__make, type_grads

  implicit none

  !-- arguments --

  integer(4),parameter :: lun = 91
  integer(4) :: imut, jmut

  type(type_libmxe_para) :: para
  type(type_libmxe_grid) :: grid
  type(type_libmxe_topo) :: topo
  type(type_libmxe_io) :: io
  type(type_grads) :: grads

  character(1),allocatable :: indx(:,:)
  real(4),allocatable :: mask(:,:)
  integer(4),allocatable :: imask(:,:)

  character(64) :: flnin
  character(64) :: flout = 'mask.gd'
  character(64) :: flout_base = 'mask'
  character(50) :: rform

  integer(4),allocatable :: idiv(:)
  integer(4),allocatable :: jdiv(:)
  integer(4) :: nidiv, njdiv
  integer(4) :: num_width = 150

  integer(4) :: i,j,k,mi,mj,n,length
  
  character(len=1),save :: cgrid  !- "T": T-grid / "U": U-grid
  character(len=16),save :: cdir   !- diretory

  !---------------------------------------------------

  namelist /nml_maskunite/ cgrid, cdir, flout_base

  !------ preprocessing: read settings and data ------

  read(5,nml=nml_maskunite,iostat=i)
  if ( i /= 0 ) then
    write(*,*) 'Read error from namelist : nml_maskunite'
    stop
  else
    write(6,*) trim(cgrid)
    write(6,*) trim(cdir)
  endif

  !---- settings ----
  call libmxe_para__register(para, file_namelist='NAMELIST.MXE')
  call libmxe_grid__register(grid, para)
  call libmxe_topo__register(topo, para)
  call libmxe_topo__aexl(topo, para)
  imut = para%imut
  jmut = para%jmut

  !---------------------------------------------------------

  allocate(indx (1:imut,1:jmut))
  allocate(mask (1:imut,1:jmut))
  allocate(imask(1:imut,1:jmut))

  !-------------------------------------------------------------

  nidiv = imut / num_width  + 2
  njdiv = jmut / num_width  + 2

  allocate(idiv(1:nidiv))
  allocate(jdiv(1:njdiv))

  idiv(1) = 0
  jdiv(1) = 0

  do mi = 2, nidiv - 1
    idiv(mi) = idiv(mi-1) + num_width
  end do
  idiv(nidiv) = imut

  do mj = 2, njdiv - 1
    jdiv(mj) = jdiv(mj-1) + num_width
  end do
  jdiv(njdiv) = jmut

  write(6,*) 'nidiv = ', nidiv
  write(6,*) 'njdiv = ', njdiv
  write(6,*) 'i=',(idiv(mi),mi=1,nidiv)
  write(6,*) 'j=',(jdiv(mj),mj=1,njdiv)

  !------

  n = 0
  do mj = 1, njdiv - 1
    do mi = 1, nidiv - 1
      n = n + 1
      k = idiv(mi+1) - idiv(mi)
      write(flnin,'(1a,1a,i2.2,a4)') trim(cdir),'/indx',n,'.dat'
      write(6,*) 'reading from... ',trim(flnin)
      write(rform,'(A4,I3,A3)') '(1X,',k,'A1)'
      write(6,*) 'format: ',rform
      open (lun,file=flnin)
      do j = jdiv(mj+1), jdiv(mj)+1, -1
        read(lun,rform) (indx(i,j),i=idiv(mi)+1,idiv(mi+1))
      enddo
      close(lun)
    end do
  end do

  mask(1:imut,1:jmut) = 0.0

  if (cgrid == 'U') then
    do j = 1, jmut
      do i = 1, imut
        if (indx(i,j)=='5') then
          if (topo%aexl(i,j,1) == 0.0d0) then
            write(6,*) 'inconsistency ? ', i,j,indx(i,j)
          else
            mask(i,j) = 5.0
          end if
        end if
        if (indx(i,j)=='7') then
          if (topo%aexl(i,j,1) == 0.0d0) then
            write(6,*) 'inconsistency ? ', i,j,indx(i,j)
          else
            mask(i,j) = 7.0
          end if
        end if
        if (indx(i,j)=='0') then
          if (topo%aexl(i,j,1) == 0.0d0) then
            write(6,*) 'inconsistency ? ', i,j,indx(i,j)
          else
            mask(i,j) = 1.0
          end if
        end if
      end do
    end do
  end if

  if (cgrid == 'T') then
    do j = 1, jmut
      do i = 1, imut
        if (indx(i,j)=='5') then
          if (topo%atexl(i,j,1) == 0.0d0) then
            write(6,*) 'inconsistency ? ', i,j,indx(i,j)
          else
            mask(i,j) = 1.0
          end if
        end if
      end do
    end do
  end if

  !---------------------------
  ! Output Data
  !
  write(flout,'(1a,1a)') trim(flout_base),'.gd'
  open(lun,file=flout,form='unformatted',access='direct',recl=4*imut*jmut)
  write(lun,rec=1) mask
  close(lun)
  !
  imask(1:imut,1:jmut) = nint(mask(1:imut,1:jmut))
  write(flout,'(1a,1a)') trim(flout_base),'.d'
  open(lun,file=flout,form='unformatted')
  write(lun) imask, imask
  close(lun)
  !
  call libmxe_io__register(io,para)
  grads%file_base=trim(flout_base)
  grads%title='target grid point'
  grads%cgrid=trim(cgrid)
  grads%ztype='surface'
  grads%timemode='stationary'
  grads%nvar=1
  grads%var(1)='index  0  99  index of target region (=1.0)'
  call libmxe_grads__make(grads,para,grid,io)

  !---------------------------------------------------------------------

end program main
