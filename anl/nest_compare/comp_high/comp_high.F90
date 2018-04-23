! -*-F90-*-
program compare_high_resolution_grid

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

  real(8),allocatable :: var_r(:,:,:)   ! regional model
  real(8),allocatable :: var_a(:,:,:)   ! all reagion model 
  real(8),allocatable :: var_d(:,:,:)   ! difference
  real(4),allocatable :: wkdat3d(:,:,:)

  real(8),allocatable :: volt(:,:,:)
  real(8),allocatable :: volu(:,:,:)

  character(len=1) :: TUXY
  integer :: klevel

  real(8) :: variance, vol_total
  integer(4) :: ndata

  integer,allocatable :: idxt(:),jdxt(:),idxu(:),jdxu(:)

  integer :: i,j,k,ip,jp,is,js
  integer :: nbw,nbe,nbs,nbn
  integer :: ist, ied, jst, jed
  integer :: imrgn, jmrgn, imall, jmall, km

  type(type_libmxe_para) :: rgnp, allp
  type(type_libmxe_grid) :: rgng, allg
  type(type_libmxe_topo) :: rgnt, allt

  !---------------------------

  namelist /nam_comp_high/ filei1, filei2, &
       & fileo1, klevel, TUXY, nbw, nbe, nbs, nbn

  open(10,file='namelist.comp_high')
  read(10,nml=nam_comp_high)
  close(10)

  !------ preprocessing ------

  call libmxe_para__register(rgnp &
             & ,file_namelist='NAMELIST.MXE.SUB')
  call libmxe_grid__register(rgng, rgnp)
  call libmxe_topo__register(rgnt, rgnp)
  call libmxe_topo__aexl(rgnt, rgnp)
  call libmxe_topo__dz3d(rgnt, rgnp)

  imrgn = rgnp%imut
  jmrgn = rgnp%jmut
  km = rgnp%km

  allocate(var_r(imrgn,jmrgn,klevel))
  allocate(var_d(imrgn,jmrgn,klevel))
  var_r(:,:,:) = 0.0d0
  var_d(:,:,:) = 0.0d0

  !-----

  call libmxe_para__register(allp &
             & ,file_namelist='NAMELIST.MXE.ALL')
  call libmxe_grid__register(allg, allp)
  call libmxe_topo__register(allt, allp)
  call libmxe_topo__aexl(allt, allp)

  imall = allp%imut
  jmall = allp%jmut
  if ( rgnp%km /= allp%km ) then
    write(*,*) 'Error: different km of src and dst.'
    stop
  endif

  allocate(var_a(imall,jmall,klevel))

  var_a(:,:,:) = 0.0d0

  !---- grid index ----

  !-- T grid

  allocate(idxt(imrgn),jdxt(jmrgn))

  idxt(:) = 0
  jdxt(:) = 0

  do ip = 1, imrgn
    do i = 1, imall
      if ( abs(allg%lont(i) - rgng%lont(ip)) < minim ) then
        idxt(ip) = i
        exit
      endif
    enddo
  enddo

  do jp = 1, jmrgn
    do j = 1, jmall
      if ( abs(allg%latt(j) - rgng%latt(jp)) < minim ) then
        jdxt(jp) = j
        exit
      end if
    end do
  end do

  !-- U grid

  allocate(idxu(imrgn),jdxu(jmrgn))

  idxu(:) = 0
  jdxu(:) = 0

  do ip = 1, imrgn
    do i = 1, imall
      if ( abs( allg%lonu(i) - rgng%lonu(ip) ) < minim ) then
        idxu(ip) = i
        exit
      endif
    enddo
  enddo

  do jp = 1, jmrgn
    do j = 1, jmall
      if ( abs( allg%latu(j) - rgng%latu(jp) ) < minim ) then
        jdxu(jp) = j
        exit
      endif
    enddo
  enddo

  !-------------------------------------------------

  lrec = 4 * imrgn * jmrgn * klevel
  allocate(wkdat3d(imrgn,jmrgn,klevel))

  open(mtin1,file=filei1,form='unformatted',access='direct',recl=lrec)
  read(mtin1,rec=1) wkdat3d
  do k = 1, klevel
    do j = 1, jmrgn
      do i = 1, imrgn
        if (wkdat3d(i,j,k) > -9.e33) then
          var_r(i,j,k) = real(wkdat3d(i,j,k),8)
        else
          var_r(i,j,k) = 0.0d0
        end if
      end do
    end do
  end do
  close(mtin1)

  deallocate(wkdat3d)

  !-------------------------------------------------

  lrec = 4 * imall * jmall * klevel
  allocate(wkdat3d(imall,jmall,klevel))

  open(mtin2,file=filei2,form='unformatted',access='direct',recl=lrec)
  read(mtin2,rec=1) wkdat3d
  do k = 1, klevel
    do j = 1, jmall
      do i = 1, imall
        if (wkdat3d(i,j,k) > -9.e33) then
          var_a(i,j,k) = real(wkdat3d(i,j,k),8)
        else
          var_a(i,j,k) = 0.0d0
        end if
      end do
    end do
  end do
  close(mtin2)

  deallocate(wkdat3d)

  write(6,*) ' finish reading data '

  !------- main --------

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
          if ((idxt(ip) /= 0) .and. (jdxt(jp) /= 0)) then
            ndata = ndata + 1
            vol_total = vol_total + volt(ip,jp,k)
            variance = variance + volt(ip,jp,k) * ((var_r(ip,jp,k) - var_a(idxt(ip),jdxt(jp),k))**2)
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

  lrec = 4

  open (mtot1,file=fileo1,form='unformatted',access='direct',recl=lrec)
  write(mtot1,rec=1) real(variance,4)
  close(mtot1)

end program compare_high_resolution_grid
