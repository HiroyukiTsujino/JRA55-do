! -*-F90-*-
!- Energy analysis of barotropic tide
!-   after Eq.(1) of Egbert and Ray (2003)
!-  D = W - nablaP 
!-    = rho g < U * nabla ( zeta_EQ + zeta_SAL ) >
!-      - rho g nabla * <U zeta>
!-    W : work by tidal forcing
!-    P : barotropic energy flux
!-  Output: W, Px, Py, nablaP
!-  This equation is applied to the tide scheme output.
!-    U => ( um, vm )
!-    zeta => ssh
!-    zeta_EQ => beta ht_tide
!-    zeta_SAL => (1-alpha) ssh 
!-      ht_tide: equilibrium tide potential
module energy
  use libmxe_para, only: clen, type_libmxe_para
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo
  use libmxe_io, only: type_libmxe_io
  use libmxe_grads, only: type_grads
  implicit none
  private


  !-- arguments --
  character(clen),save :: file_base_ssh  !- input file base (SSH)
  character(clen),save :: file_base_um   !-   (UM)
  character(clen),save :: file_base_vm   !-   (VM)
  character(clen),save :: file_base_tidept !- (tide potential)
  real(8),save :: alpha_tide = 0.88d0    ! coef. alpha
  real(8),save :: beta_tide = 0.7d0      ! coef. beta
  integer,save :: nstr = 1              !- start nrec [default: 1]
  integer,save :: nend = 0              !- end nrec [default: nm]


  public :: ini
  public :: calc
  public :: get_result
  public :: write_result


  character(*), parameter :: fileo = 'energy'
  integer, parameter :: lun = 77
  real(8),allocatable,save :: w(:,:),px(:,:),py(:,:),nablap(:,:)
  integer,save :: im, jm, reclen

  type(type_libmxe_para),save :: para
  type(type_libmxe_grid),save :: grid
  type(type_libmxe_topo),save :: topo
  type(type_libmxe_io),save :: io
  type(type_grads),save :: grads


contains


subroutine ini
  use libmxe_para, only: libmxe_para__register
  use libmxe_grid, only: libmxe_grid__register
  use libmxe_topo, only: libmxe_topo__register, &
                       & libmxe_topo__aexl
  use libmxe_io, only: libmxe_io__register
  implicit none

  integer :: i

  namelist /energy_lst/  file_base_ssh, file_base_um &
            & , file_base_vm, file_base_tidept &
            & , alpha_tide, beta_tide, nstr, nend

  !---- arguments ----
  read(5,nml=energy_lst,iostat=i)
  if ( i /= 0 ) then
    write(*,*) ' Read error : namelist'
    stop
  endif

  !---- experiment settings ----
  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)
  call libmxe_topo__aexl(topo,para)
  call libmxe_io__register(io,para)
  im = para%imut
  jm = para%jmut
  reclen = im*jm*4
  if ( nend == 0 ) nend = io%nm
  if ( ( nstr < 1).or.( nstr > io%nm ) &
       & .or.( nend < 1).or.( nend > io%nm ) ) then
    write(*,*) 'Error at energy__ini'
    write(*,*) '  Wrong nstr, nend',nstr,nend
    stop
  endif

  !-- grads control file --
  grads%file_base = trim(fileo)
  grads%title = trim(fileo)
  grads%cgrid = 'U'
  grads%ztype = 'surface'
  grads%timemode = 'stationary'
  grads%nvar = 4
  grads%var(1) = 'w 1 99 work of tidal forcing [mW/m2] (=[g/s3])'
  grads%var(2) = 'px 1 99 energy flux [cm g/s3]'
  grads%var(3) = 'py 1 99 energy flux [cm g/s3]'
  grads%var(4) = 'nablap 1 99 divergence of energy flux [g/s3]'

  allocate(w(im,jm),px(im,jm),py(im,jm),nablap(im,jm))

end subroutine ini


subroutine calc
  use libmxe_para, only: rho, grav
  use libmxe_io, only: libmxe_io__open
  use gradient, only: gradient__h2d
  use regrid, only: regrid__h2d
  use btro_vector, only: btro_vector__divergence
  implicit none

  integer :: i,j,n
  real(8),allocatable,dimension(:,:) :: um,vm,ssh,heq,hsal &
                               & ,eqsal_x,eqsal_y,tidept
  real,allocatable,dimension(:,:) :: r4

  allocate( r4(im,jm),um(im,jm),vm(im,jm),ssh(im,jm),tidept(im,jm))
  allocate( heq(im,jm), hsal(im,jm), eqsal_x(im,jm), eqsal_y(im,jm) )

  w(:,:) = 0.d0
  px(:,:) = 0.d0
  py(:,:) = 0.d0
  nablap(:,:) = 0.d0

  do n = nstr, nend

    if ( mod(n,100) == 0 ) write(*,*) n

    !-- input --
    call libmxe_io__open(io,trim(file_base_ssh),n,reclen &
         & ,lun,action='read')
    read(lun,rec=1) r4
    close(lun)
    ssh(:,:) = dble( r4(:,:) )
    call libmxe_io__open(io,trim(file_base_um),n,reclen &
         & ,lun,action='read')
    read(lun,rec=1) r4
    close(lun)
    um(:,:) = dble( r4(:,:) )
    call libmxe_io__open(io,trim(file_base_vm),n,reclen &
         & ,lun,action='read')
    read(lun,rec=1) r4
    close(lun)
    vm(:,:) = dble( r4(:,:) )
    call libmxe_io__open(io,trim(file_base_tidept),n,reclen &
         & ,lun,action='read')
    read(lun,rec=1) r4
    close(lun)
    tidept(:,:) = dble( r4(:,:) )

    !-- sum < U * nabla ( h_EQ + h_SAL ) > --
    heq(:,:) = beta_tide * tidept(:,:)
    hsal(:,:) = ( 1.d0 - alpha_tide ) * ssh(:,:)
    call gradient__h2d( heq(:,:)+hsal(:,:),1,'T','U','x' &
                      & ,para,grid,topo, eqsal_x(:,:), 0.d0 )
    call gradient__h2d( heq(:,:)+hsal(:,:),1,'T','U','y' &
                      & ,para,grid,topo, eqsal_y(:,:), 0.d0 )
    w(:,:) = w(:,:) + um(:,:)*eqsal_x(:,:) + vm(:,:)*eqsal_y(:,:)

    !-- sum  <U h> --
    call regrid__h2d(ssh(:,:),1,'T',para,grid,topo,heq(:,:),0.d0)
    px(:,:) = px(:,:) + um(:,:) * heq(:,:)  !- heq: regrided SSH
    py(:,:) = py(:,:) + vm(:,:) * heq(:,:)

  enddo

  !-- terms --
  w(:,:) = rho * grav * w(:,:) / dble(nend-nstr+1)
  px(:,:) = rho * grav * px(:,:) / dble(nend-nstr+1)
  py(:,:) = rho * grav * py(:,:) / dble(nend-nstr+1)
  call btro_vector__divergence( px(:,:), py(:,:), &
                              & para, grid, topo, nablap(:,:) )

  !-- land --
  do j = 1, jm
    do i = 1, im
      if ( topo%exnn(i,j) < 1 ) then
        w(i,j) = dble( para%rundefout )
        px(i,j) = dble( para%rundefout )
        py(i,j) = dble( para%rundefout )
        !-  nablap(i,j) is already set in btro_vector.
      endif
    enddo
  enddo

  deallocate(r4,um,vm,ssh,heq,hsal,eqsal_x,eqsal_y,tidept)

end subroutine calc


subroutine write_result
  use libmxe_grads, only: libmxe_grads__make
  implicit none

  call libmxe_grads__make(grads,para,grid,io)
  open(lun,file=trim(fileo)//'.gd',form='unformatted' &
     & , access='direct',recl=reclen,action='write')
    write(lun,rec=1) real(w)
    write(lun,rec=2) real(px)
    write(lun,rec=3) real(py)
    write(lun,rec=4) real(nablap)
  close(lun)

end subroutine write_result


real function get_result(i,j,cvar)
  implicit none

  integer,intent(in) :: i,j
  character(*),intent(in) :: cvar

  select case ( cvar )
    case('w')
      get_result = real(w(i,j))
    case('px')
      get_result = real(px(i,j))
    case('py')
      get_result = real(py(i,j))
    case('nablap')
      get_result = real(nablap(i,j))
    case default
      write(*,*) 'Error at energy__get_result'
      write(*,*) '  Wrong cvar =',cvar
      stop
  end select

end function get_result


end module energy
