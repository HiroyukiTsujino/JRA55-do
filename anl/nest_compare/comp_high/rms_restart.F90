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

  real(8),allocatable :: var_r(:,:,:)   ! reagional model 
  real(8),allocatable :: wkdat2d(:,:)

  real(8),allocatable :: volt(:,:,:)
  real(8),allocatable :: volu(:,:,:)

  character(len=1) :: TUXY
  integer :: klevel

  integer :: irec, istrt
  real(8) :: variance, vol_total
  integer(4) :: ndata

  integer,allocatable :: idxt(:),jdxt(:),idxu(:),jdxu(:)

  integer :: i,j,k,ip,jp,is,js
  integer :: nbw,nbe,nbs,nbn
  integer :: ist, ied, jst, jed
  integer :: imrgn, jmrgn, imall, jmall, km

  type(type_libmxe_para) :: rgnp
  type(type_libmxe_grid) :: rgng
  type(type_libmxe_topo) :: rgnt

  !---------------------------

  namelist /nam_rms_restart/ filei1, &
       & fileo1, klevel, TUXY, nbw, nbe, nbs, nbn

  open(10,file='namelist.rms_restart')
  read(10,nml=nam_rms_restart)
  close(10)

  !------ preprocessing ------

  call libmxe_para__register(rgnp, file_namelist='NAMELIST.MXE.SUB')
  call libmxe_grid__register(rgng, rgnp)
  call libmxe_topo__register(rgnt, rgnp)
  call libmxe_topo__aexl(rgnt, rgnp)
  call libmxe_topo__dz3d(rgnt, rgnp)

  imrgn = rgnp%imut
  jmrgn = rgnp%jmut
  km = rgnp%km

  allocate(var_r(imrgn,jmrgn,klevel))
  var_r(:,:,:) = 0.0d0

  !-------------------------------------------------

  lrec = 8 * imrgn * jmrgn
  allocate(wkdat2d(imrgn,jmrgn))

  open(mtin1,file=filei1,form='unformatted',access='direct',recl=lrec)
  irec = 1

  do k = 1, klevel
    read(mtin1,rec=irec) wkdat2d(:,:)
    irec = irec + 1
    do j = 1, jmrgn
      do i = 1, imrgn
        var_r(i,j,k) = wkdat2d(i,j)
      end do
    end do
  end do
  close(mtin1)

  deallocate(wkdat2d)

  !-------------------------------------------------

  variance = 0.0d0
  vol_total = 0.0d0
  ndata = 0

  if (TUXY=='T') then

    if (nbw /= 0) then
      ist = 4
    else
      ist = 3
    end if
    
    if (nbs /= 0) then
      jst = 4
    else
      jst = 3
    end if
    
    if (nbw /= 0) then
      ied = imrgn - 2
    else
      ied = imrgn
    end if
    
    if (nbn /= 0) then
      jed = jmrgn - 2
    else
      jed = jmrgn
    end if

    allocate(volt(1:imrgn,1:jmrgn,1:km))

    do k = 1, km
      do j = 2, jmrgn
        do i = 2, imrgn
          volt(i,j,k) = rgng%a_tr(i-1,j-1) * rgnt%dzu(i-1,j-1,k) &
               & + rgng%a_tl(i,j-1) * rgnt%dzu(i,j-1,k) &
               & + rgng%a_br(i-1,j) * rgnt%dzu(i-1,j,k) &
               & + rgng%a_bl(i,j) * rgnt%dzu(i,j,k)
        end do
      end do
    end do

    do k = 1, klevel
      do jp = jst, jed
        do ip = ist, ied
          if (rgnt%atexl(ip,jp,k) /= 0.0d0) then
            ndata = ndata + 1
            vol_total = vol_total + volt(ip,jp,k)
            variance = variance + volt(ip,jp,k) * ((var_r(ip,jp,k) - var_r(ist,jst,k))**2)
          end if
        end do
      end do
    end do
  end if

  !-------------------------------------------------------------------------

!  variance = sqrt(variance / real(ndata,8))
  variance = sqrt(variance / vol_total)
  write(6,*) variance

  write(6,*) ' now writing data '

  open (mtot1,file=fileo1)
  write(mtot1,*) real(variance,4)
  close(mtot1)

end program rms_restart
