!add.F90
!====================================================
!
!
!====================================================
program add
  !
  use oc_mod_param, only : &
  &   imut, jmut, km
  !
  use oc_structure, only  : &
  &   read_topo,            &
  &   aexl, atexl,          &
  &   coefx, coefy
  !
  !
  !
  real(4) :: dat3in1(imut, jmut, km)
  real(4) :: dat3in2(imut, jmut, km)
  !
  real(4) :: dat3out(imut,jmut,km)
  !
  integer(4), parameter :: mtfile = 80
  integer(4), parameter :: mtin1  = 81
  integer(4), parameter :: mtin2  = 82
  integer(4), parameter :: mtout  = 83
  !
  integer(4) :: i, j, k, n
  !
  character(len=256)    :: fltopo
  character(len=256)    :: flin1
  character(len=256)    :: flin2
  character(len=256)    :: flout
  character(len=  1)    :: tuxy
  integer(4), save      :: k_max = 0
  !
  real(4), save  :: UNDEF  =  0.0e0
  !
  namelist /nml_add/ fltopo, flin1, flin2, flout, tuxy, k_max, UNDEF
  !
  !==========================================
  !
  read(unit=5, nml=nml_add)
  print *,'fltopo   :', trim(fltopo)
  print *,'file in 1:', trim(flin1)
  print *,'file in 2:', trim(flin2)
  print *,'file out :', trim(flout)
  print *,'tuxy     :', tuxy
  print *,'k_max    :', k_max
  print *,'new UNDEF  :', UNDEF
  !
  call read_topo(fltopo)
  !
  !------------------------------------------
  select case (tuxy)
    case('T') !-------------------------------------------------
      dat3out(1:imut, 1:jmut, 1:km) = 0.0
      !
      open(mtin1, file=flin1, form='unformatted', access='direct', recl=4*imut*jmut*km)
      read (mtin1, rec=1) dat3in1(:,:,:)
      close(mtin1)
      open(mtin2, file=flin2, form='unformatted', access='direct', recl=4*imut*jmut*km)
      read (mtin2, rec=1) dat3in2(:,:,:)
      close(mtin2)
      !
      do k=1, km
        dat3out(1:imut, 1:jmut, k) = atexl(1:imut, 1:jmut, k) *          &
          &      (dat3in1(1:imut, 1:jmut, k) + dat3in2(1:imut, 1:jmut, k))
      end do
      !
      open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*km)
      do k=1, km
        where(atexl(1:imut, 1:jmut, k) == 0.d0)
          dat3out(1:imut, 1:jmut, k) = UNDEF
        end where
      end do
      write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:km)
      close(mtout)
    case('t') !-------------------------------------------------
      dat3out(1:imut, 1:jmut, 1:k_max) = 0.0
      !
      open(mtin1, file=flin1, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
      read (mtin1, rec=1) dat3in1(:,:,1:k_max)
      close(mtin1)
      open(mtin2, file=flin2, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
      read (mtin2, rec=1) dat3in2(:,:,1:k_max)
      close(mtin2)
      !
      do k=1, k_max
        dat3out(1:imut, 1:jmut, k) = atexl(1:imut, 1:jmut, 1) *         &
          &      (dat3in1(1:imut, 1:jmut, k) + dat3in2(1:imut, 1:jmut, k))
      end do
      !
      open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
      do k=1, k_max
        where(atexl(1:imut, 1:jmut, 1) == 0.d0)
          dat3out(1:imut, 1:jmut, k) = UNDEF
        end where
      end do
      write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:k_max)
      close(mtout)
    case('U') !-------------------------------------------------
      dat3out(1:imut, 1:jmut, 1:km) = 0.0
      !
      open(mtin1, file=flin1, form='unformatted', access='direct', recl=4*imut*jmut*km)
      read (mtin1, rec=1) dat3in1(:,:,:)
      close(mtin1)
      open(mtin2, file=flin2, form='unformatted', access='direct', recl=4*imut*jmut*km)
      read (mtin2, rec=1) dat3in2(:,:,:)
      close(mtin2)
      !
      do k=1, km
        dat3out(1:imut, 1:jmut, k) = aexl(1:imut, 1:jmut, k) *           &
          &      (dat3in1(1:imut, 1:jmut, k) + dat3in2(1:imut, 1:jmut, k))
      end do
      !
      open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*km)
      do k=1, km
        where(aexl(1:imut, 1:jmut, k) == 0.d0)
          dat3out(1:imut, 1:jmut, k) = UNDEF
        end where
      end do
      write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:km)
      close(mtout)
    case('u') !-------------------------------------------------
      dat3out(1:imut, 1:jmut, 1:k_max) = 0.0
      !
      open(mtin1, file=flin1, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
      read (mtin1, rec=1) dat3in1(:,:,1:k_max)
      close(mtin1)
      open(mtin2, file=flin2, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
      read (mtin2, rec=1) dat3in2(:,:,1:k_max)
      close(mtin2)
      !
      do k=1, k_max
        dat3out(1:imut, 1:jmut, k) = aexl(1:imut, 1:jmut, 1) *          &
          &      (dat3in1(1:imut, 1:jmut, k) + dat3in2(1:imut, 1:jmut, k))
      end do
      !
      open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
      do k=1, k_max
        where(aexl(1:imut, 1:jmut, 1) == 0.d0)
          dat3out(1:imut, 1:jmut, k) = UNDEF
        end where
      end do
      write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:k_max)
      close(mtout)
    case('X') !-------------------------------------------------
      dat3out(1:imut, 1:jmut, 1:km) = 0.0
      !
      open(mtin1, file=flin1, form='unformatted', access='direct', recl=4*imut*jmut*km)
      read (mtin1, rec=1) dat3in1(:,:,:)
      close(mtin1)
      open(mtin2, file=flin2, form='unformatted', access='direct', recl=4*imut*jmut*km)
      read (mtin2, rec=1) dat3in2(:,:,:)
      close(mtin2)
      !
      do k=1, km
        dat3out(1:imut, 1:jmut, k) = coefx(1:imut, 1:jmut, k) *         &
          &      (dat3in1(1:imut, 1:jmut, k) + dat3in2(1:imut, 1:jmut, k))
      end do
      !
      open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*km)
      do k=1, km
        where(coefx(1:imut, 1:jmut, k) == 0.d0)
          dat3out(1:imut, 1:jmut, k) = UNDEF
        end where
      end do
      write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:km)
      close(mtout)
    case('x') !-------------------------------------------------
      dat3out(1:imut, 1:jmut, 1:k_max) = 0.0
      !
      open(mtin1, file=flin1, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
      read (mtin1, rec=1) dat3in1(:,:,1:k_max)
      close(mtin1)
      open(mtin2, file=flin2, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
      read (mtin2, rec=1) dat3in2(:,:,1:k_max)
      close(mtin2)
      !
      do k=1, k_max
        dat3out(1:imut, 1:jmut, k) = coefx(1:imut, 1:jmut, 1) *           &
          &     (dat3in1(1:imut, 1:jmut, k) + dat3in2(1:imut, 1:jmut, k))
      end do
      !
      open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
      do k=1, k_max
        where(coefx(1:imut, 1:jmut, 1) == 0.d0)
          dat3out(1:imut, 1:jmut, k) = UNDEF
        end where
      end do
      write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:k_max)
      close(mtout)
    case('Y') !-------------------------------------------------
      dat3out(1:imut, 1:jmut, 1:km) = 0.0
      !
      open(mtin1, file=flin1, form='unformatted', access='direct', recl=4*imut*jmut*km)
      read (mtin1, rec=1) dat3in1(:,:,:)
      close(mtin1)
      open(mtin2, file=flin2, form='unformatted', access='direct', recl=4*imut*jmut*km)
      read (mtin2, rec=1) dat3in2(:,:,:)
      close(mtin2)
      !
      do k=1, km
        dat3out(1:imut, 1:jmut, k) = coefy(1:imut, 1:jmut, k) *         &
          &      (dat3in1(1:imut, 1:jmut, k) + dat3in2(1:imut, 1:jmut, k))
      end do
      !
      open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*km)
      do k=1, km
        where(coefy(1:imut, 1:jmut, k) == 0.d0)
          dat3out(1:imut, 1:jmut, k) = UNDEF
        end where
      end do
      write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:km)
      close(mtout)
    case('y') !-------------------------------------------------
      dat3out(1:imut, 1:jmut, 1:k_max) = 0.0
      !
      open(mtin1, file=flin1, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
      read (mtin1, rec=1) dat3in1(:,:,1:k_max)
      close(mtin1)
      open(mtin2, file=flin2, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
      read (mtin2, rec=1) dat3in2(:,:,1:k_max)
      close(mtin2)
      !
      do k=1, k_max
        dat3out(1:imut, 1:jmut, k) = coefy(1:imut, 1:jmut, 1) *           &
          &     (dat3in1(1:imut, 1:jmut, k) + dat3in2(1:imut, 1:jmut, k))
      end do
      !
      open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
      do k=1, k_max
        where(coefy(1:imut, 1:jmut, 1) == 0.d0)
          dat3out(1:imut, 1:jmut, k) = UNDEF
        end where
      end do
      write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:k_max)
      close(mtout)
    case('W') !-------------------------------------------------
      dat3out(1:imut, 1:jmut, 1:km) = 0.0
      !
      open(mtin1, file=flin1, form='unformatted', access='direct', recl=4*imut*jmut*km)
      read (mtin1, rec=1) dat3in1(:,:,:)
      close(mtin1)
      open(mtin2, file=flin2, form='unformatted', access='direct', recl=4*imut*jmut*km)
      read (mtin2, rec=1) dat3in2(:,:,:)
      close(mtin2)
      !
      dat3out(1:imut, 1:jmut, km) = 0.e0
      do k=1, km-1
        dat3out(1:imut, 1:jmut, k) = atexl(1:imut, 1:jmut, k+1) *            &
          &      (dat3in1(1:imut, 1:jmut, k) + dat3in2(1:imut, 1:jmut, k))
      end do
      !
      open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*km)
      dat3out(1:imut, 1:jmut, km) = UNDEF
      do k=1, km-1
        where(atexl(1:imut, 1:jmut, k+1) == 0.d0)
          dat3out(1:imut, 1:jmut, k) = UNDEF
        end where
      end do
      write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:km)
      close(mtout)
    case default !-----------------------------------------------
  end select
  !
!====================================================
end program add
