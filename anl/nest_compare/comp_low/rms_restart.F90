! -*-F90-*-
program rms_restart

  use libmxe_para, only: libmxe_para__register, type_libmxe_para &
                      & , clen
  use libmxe_grid, only: libmxe_grid__register, type_libmxe_grid
  use libmxe_topo, only: libmxe_topo__register, type_libmxe_topo &
                      & , libmxe_topo__aexl, libmxe_topo__dz3d

  implicit none

  real(8),parameter :: minim = 1.d-6

  integer,parameter :: mtin1=11, mtin2=12, mtin3=13, mtin4=14
  integer,parameter :: mtot1=21, mtot2=22
  integer :: lrec

  character(clen) :: filei1, filei2, filei3, filei4
  character(clen) :: fileo1, fileo2

  real(8),allocatable :: var_l(:,:,:)   ! low resolution model
  real(8),allocatable :: wkdat2d(:,:)

  real(8),allocatable :: volt(:,:,:)
  real(8),allocatable :: volu(:,:,:)

  character(len=1) :: TUXY
  integer :: klevel

  integer :: irec, istrt

  real(8) :: vol_total_inner, vol_total_all
  real(8) :: variance_inner, variance_all
  integer(4) :: ndata_inner, ndata_all

  integer :: i,j,k
  integer :: ibst, ibed, jbst, jbed
  integer :: ist, ied, jst, jed
  integer :: imall, jmall, km

  type(type_libmxe_para) :: allp
  type(type_libmxe_grid) :: allg
  type(type_libmxe_topo) :: allt

  !---------------------------

  namelist /nam_rms_restart/ filei1, &
       & fileo1, klevel, TUXY, ibst, ibed, jbst, jbed

  open(10,file='namelist.rms_restart')
  read(10,nml=nam_rms_restart)
  close(10)

  !------ preprocessing ------

  call libmxe_para__register(allp, file_namelist='NAMELIST.MXE.LOW')
  call libmxe_grid__register(allg, allp)
  call libmxe_topo__register(allt, allp)
  call libmxe_topo__aexl(allt, allp)
  call libmxe_topo__dz3d(allt, allp)

  imall = allp%imut
  jmall = allp%jmut
  km = allp%km

  allocate(var_l(imall,jmall,klevel))
  var_l(:,:,:) = 0.0d0

  !-------------------------------------------------

  lrec = 8 * imall * jmall
  allocate(wkdat2d(imall,jmall))

  open(mtin1,file=filei1,form='unformatted',access='direct',recl=lrec)
  irec = 1
  do k = 1, klevel
    read(mtin1,rec=irec) wkdat2d(:,:)
    istrt = istrt + 1
    do j = 1, jmall
      do i = 1, imall
        var_l(i,j,k) = wkdat2d(i,j)
      end do
    end do
  end do
  close(mtin1)

  deallocate(wkdat2d)

  write(6,*) ' finish reading data '

  !------- main --------

  variance_all = 0.0d0
  ndata_all = 0
  vol_total_all = 0.0d0

  variance_inner = 0.0d0
  ndata_inner = 0
  vol_total_inner = 0.0d0

  if (TUXY=='T') then

    if (allp%lcyclic) then
      ist = 3
      jst = 3
      ied = imall - 2
      jed = jmall - 2
    else
      ist = 2
      jst = 2
      ied = imall
      jed = jmall
    end if
!    if (lfoldnp) then
!      jed = jmall - 3
!    end if

    allocate(volt(1:imall,1:jmall,1:km))

    do k = 1, km
      do j = 2, jmall
        do i = 2, imall
          volt(i,j,k) = allg%a_tr(i-1,j-1) * allt%dzu(i-1,j-1,k) &
               & + allg%a_tl(i,j-1) * allt%dzu(i,j-1,k) &
               & + allg%a_br(i-1,j) * allt%dzu(i-1,j,k) &
               & + allg%a_bl(i,j) * allt%dzu(i,j,k)
        end do
      end do
    end do

    do k = 1, klevel
      do j = jst, jed
        do i = ist, ied
          if (allt%atexl(i,j,k) /= 0.0d0) then
            ndata_all = ndata_all + 1
            vol_total_all = vol_total_all + volt(i,j,k)
            variance_all = variance_all + volt(i,j,k) * ((var_l(i,j,k) - var_l(ist,jst,k))**2)
          end if
        end do
      end do
    end do

    do k = 1, klevel
      do j = jbst, jbed
        do i = ibst, ibed
          if (allt%atexl(i,j,k) /= 0.0d0) then
            ndata_inner = ndata_inner + 1
            vol_total_inner = vol_total_inner + volt(i,j,k)
            variance_inner = variance_inner  + volt(i,j,k) * ((var_l(i,j,k) - var_l(ist,jst,k))**2)
          end if
        end do
      end do
    end do

  end if

  !-------------------------------------------------------------------------

!  variance_all = sqrt(variance_all/real(ndata_all,8))
  variance_all = sqrt(variance_all/vol_total_all)
  write(6,*) variance_all

!  variance_inner = sqrt(variance_inner/real(ndata_inner,8))
  variance_inner = sqrt(variance_inner/vol_total_inner)
  write(6,*) variance_inner

  write(6,*) ' now writing data '

  lrec = 4

  open (mtot1,file=fileo1)
  write(mtot1,*) real(variance_all,4)
  write(mtot1,*) real(variance_inner,4)
  close(mtot1)

end program rms_restart
