! -*-F90-*-
!- Replace X-ward and Y-ward traer fluxes of the parent model
!-   with those of the sub model
program mabiki

  use libmxe_para, only: libmxe_para__register, type_libmxe_para &
                      & , clen
  use libmxe_grid, only: libmxe_grid__register, type_libmxe_grid
  use libmxe_topo, only: libmxe_topo__register, type_libmxe_topo &
                      & , libmxe_topo__aexl

  implicit none

  real(8),parameter :: minim = 1.d-6

  integer,parameter :: mtin1=11, mtin2=12, mtin3=13, mtin4=14
  integer,parameter :: mtot1=21, mtot2=22
  integer :: lrec

  character(clen) :: filei1, filei2, filei3, filei4
  character(clen) :: fileo1, fileo2

  real(8),allocatable :: var_l(:,:,:)   ! low-resolution model
  real(8),allocatable :: var_m(:,:,:)   ! reduced (mabiki) on low-resolution
  real(8),allocatable :: var_h(:,:,:)   ! high-resolution model
  real(4),allocatable :: wkdat3d(:,:,:)

  character(len=1) :: TUXY
  logical :: l_replace_with_zero
  integer :: klevel

  integer,allocatable :: idxt(:),jdxt(:),idxu(:),jdxu(:)

  integer :: i,j,k,ip,jp,is,js
  integer :: nbw,nbe,nbs,nbn
  integer :: impar, jmpar, imsub, jmsub, km

  type(type_libmxe_para) :: parp, subp
  type(type_libmxe_grid) :: parg, subg
  type(type_libmxe_topo) :: part, subt

  !---------------------------

  namelist /nam_mabiki/ filei1, filei2, &
       & fileo1, l_replace_with_zero, klevel, TUXY

  open(10,file='namelist.mabiki')
  read(10,nml=nam_mabiki)
  close(10)

  !------ preprocessing ------
  !-- parent model --
  call libmxe_para__register(parp &
             & ,file_namelist='NAMELIST.MXE.LOW')
  call libmxe_grid__register(parg, parp)
  call libmxe_topo__register(part, parp)
  call libmxe_topo__aexl(part, parp)
  impar = parp%imut
  jmpar = parp%jmut
  km = parp%km

  allocate(var_l(impar,jmpar,klevel))
  allocate(var_m(impar,jmpar,klevel))
  var_l(:,:,:) = 0.0d0
  var_m(:,:,:) = 0.0d0

  !-- sub model --
  call libmxe_para__register(subp &
             & ,file_namelist='NAMELIST.MXE.HIGH')
  call libmxe_grid__register(subg, subp)
  call libmxe_topo__register(subt, subp)
  call libmxe_topo__aexl(subt, subp)
  imsub = subp%imut
  jmsub = subp%jmut
  if ( parp%km /= subp%km ) then
    write(*,*) 'Error: different km of src and dst.'
    stop
  endif

  allocate(var_h(imsub,jmsub,klevel))

  var_h(:,:,:) = 0.0d0

  !---- grid index ----

  !-- T grid (sub model T grid that belongs to parent model T grid) --

  allocate(idxt(impar),jdxt(jmpar))

  idxt(:) = 0
  jdxt(:) = 0

  do ip = 1, impar
    do i = 1, imsub
      if ( abs(subg%lont(i) - parg%lont(ip)) < minim ) then
        idxt(ip) = i
        exit
      endif
    enddo
  enddo

  do jp = 1, jmpar
    do j = 1, jmsub
      if ( abs(subg%latt(j) - parg%latt(jp)) < minim ) then
        jdxt(jp) = j
        exit
      endif
    enddo
  enddo

  !-- U grid (parent model U grid that matches parent model U grid) --

  allocate(idxu(impar),jdxu(jmpar))

  idxu(:) = 0
  jdxu(:) = 0

  do ip = 1, impar
    do i = 1, imsub
      if ( abs( subg%lonu(i) - parg%lonu(ip) ) < minim ) then
        idxu(ip) = i
        exit
      endif
    enddo
  enddo

  do jp = 1, jmpar
    do j = 1, jmsub
      if ( abs( subg%latu(j) - parg%latu(jp) ) < minim ) then
        jdxu(jp) = j
        exit
      endif
    enddo
  enddo

  !-------------------------------------------------

  lrec = 4 * impar * jmpar * klevel
  allocate(wkdat3d(impar,jmpar,klevel))

  open(mtin1,file=filei1,form='unformatted',access='direct',recl=lrec)
  read(mtin1,rec=1) wkdat3d
  do k = 1, klevel
    do j = 1, jmpar
      do i = 1, impar
        if (wkdat3d(i,j,k) > -9.e33) then
          var_l(i,j,k) = real(wkdat3d(i,j,k),8)
        else
          var_l(i,j,k) = 0.0d0
        end if
      end do
    end do
  end do
  close(mtin1)

  deallocate(wkdat3d)

  !-------------------------------------------------

  lrec = 4 * imsub * jmsub * klevel
  allocate(wkdat3d(imsub,jmsub,klevel))

  open(mtin2,file=filei2,form='unformatted',access='direct',recl=lrec)
  read(mtin2,rec=1) wkdat3d
  do k = 1, klevel
    do j = 1, jmsub
      do i = 1, imsub
        if (wkdat3d(i,j,k) > -9.e33) then
          var_h(i,j,k) = real(wkdat3d(i,j,k),8)
        else
          var_h(i,j,k) = 0.0d0
        end if
      end do
    end do
  end do
  close(mtin2)

  deallocate(wkdat3d)

  write(6,*) ' finish reading data '

  !------- main --------

  do k = 1, klevel
    if (TUXY=='T') then
      do jp = 1, jmpar
        do ip = 1, impar
          if ((idxt(ip) /= 0) .and. (jdxt(jp) /= 0)) then
            var_m(ip,jp,k) = var_h(idxt(ip),jdxt(jp),k)
          else
            if (l_replace_with_zero) then
              var_m(ip,jp,k) = 0.0d0
            else  
              var_m(ip,jp,k) = var_l(ip,jp,k)
            end if
          end if
        end do
      end do
    end if
  end do

  !-------------------------------------------------------------------------

  write(6,*) ' now writing data '

  lrec = 4 * impar * jmpar * klevel

  open (mtot1,file=fileo1,form='unformatted',access='direct',recl=lrec)
  write(mtot1,rec=1) real(var_m(:,:,:),4)
  close(mtot1)

end program mabiki
