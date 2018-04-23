!replace.F90
!====================================================
!
!  Convert Unit and Set UNDEF
!
!====================================================
program replace
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
  real(4) :: dat3in(imut, jmut, km) ! 読み出し用3次元配列
  real(4) :: flag_in(imut,jmut,km)
  real(4) :: dat3out(imut,jmut,km)
  !
  integer(4), parameter :: mtfile = 80
  integer(4), parameter :: mtin   = 81
  integer(4), parameter :: mtout  =82
  !
  integer(4) :: i, j, k, n
  !
  character(len=256)    :: fltopo
  character(len=256)    :: flin
  character(len=256)    :: flout
  character(len=  1)    :: tuxy
  integer(4), save      :: k_max = 0
  !
  real(4), save  :: addval = 0.0e0
  real(4), save  :: factor = 1.0e0
  real(4), save  :: UNDEF  = 0.0e0
  !
  namelist /nml_replace/ fltopo, flin, flout, tuxy, k_max,  &
    &                    addval, factor, UNDEF
  !
  !==========================================
  !
  read(unit=5, nml=nml_replace)
  print *,'fltopo   :', trim(fltopo)
  print *,'file in  :', trim(flin)
  print *,'file out :', trim(flout)
  print *,'tuxy     :', tuxy
  print *,'k_max    :', k_max
  !
  print *,'addval     :', addval
  print *,'factor     :', factor
  print *,'new UNDEF  :', UNDEF
  !
  call read_topo(fltopo)
  !
  !------------------------------------------
  select case (tuxy)
    case('T') !-------------------------------------------------
      dat3out(1:imut, 1:jmut, 1:km) = 0.0
      flag_in(1:imut, 1:jmut, 1:km) = 1.0
      !
      open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*km)
      read (mtin, rec=1) dat3in(:,:,:)
      close(mtin)
      !
      where(dat3in(:,:,:) == UNDEF)
        flag_in(:,:,:) = 0.0
      end where
      do k=1, km
        dat3out(1:imut, 1:jmut, k) = factor * (addval +           &
          &  atexl(1:imut, 1:jmut, k) * dat3in(1:imut, 1:jmut, k))
      end do
      !
      do k=1, km
        where(flag_in(1:imut, 1:jmut, k) == 0.0)
          dat3out(1:imut, 1:jmut, k) = UNDEF
        end where
        where(atexl(1:imut, 1:jmut, k) == 0.d0)
          dat3out(1:imut, 1:jmut, k) = UNDEF
        end where
      end do
      !
      open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*km)
      write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:km)
      close(mtout)
    case('t') !-------------------------------------------------
      dat3out(1:imut, 1:jmut, 1:k_max) = 0.0
      flag_in(1:imut, 1:jmut, 1:k_max) = 1.0
      !
      open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
      read (mtin, rec=1) dat3in(:,:,1:k_max)
      close(mtin)
      !
      where(dat3in(:,:,:) == UNDEF)
        flag_in(:,:,:) = 0.0
      end where
      do k=1, k_max
        dat3out(1:imut, 1:jmut, k) = factor * (addval +           &
          &  atexl(1:imut, 1:jmut, 1) * dat3in(1:imut, 1:jmut, k))
      end do
      !
      do k=1, k_max
        where(flag_in(1:imut, 1:jmut, k) == 0.0)
          dat3out(1:imut, 1:jmut, k) = UNDEF
        end where
        where(atexl(1:imut, 1:jmut, 1) == 0.d0)
          dat3out(1:imut, 1:jmut, k) = UNDEF
        end where
      end do
      !
      open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
      write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:k_max)
      close(mtout)
    case('U') !-------------------------------------------------
      dat3out(1:imut, 1:jmut, 1:km) = 0.0
      flag_in(1:imut, 1:jmut, 1:km) = 1.0
      !
      open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*km)
      read (mtin, rec=1) dat3in(:,:,:)
      close(mtin)
      !
      where(dat3in(:,:,:) == UNDEF)
        flag_in(:,:,:) = 0.0
      end where
      do k=1, km
        dat3out(1:imut, 1:jmut, k) = factor * (addval +           &
          &  aexl(1:imut, 1:jmut, k) * dat3in(1:imut, 1:jmut, k))
      end do
      !
      do k=1, km
        where(flag_in(1:imut, 1:jmut, k) == 0.0)
          dat3out(1:imut, 1:jmut, k) = UNDEF
        end where
        where(aexl(1:imut, 1:jmut, k) == 0.d0)
          dat3out(1:imut, 1:jmut, k) = UNDEF
        end where
      end do
      !
      open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*km)
      write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:km)
      close(mtout)
    case('u') !-------------------------------------------------
      dat3out(1:imut, 1:jmut, 1:k_max) = 0.0
      flag_in(1:imut, 1:jmut, 1:k_max) = 1.0
      !
      open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
      read (mtin, rec=1) dat3in(:,:,1:k_max)
      close(mtin)
      !
      where(dat3in(:,:,:) == UNDEF)
        flag_in(:,:,:) = 0.0
      end where
      do k=1, k_max
        dat3out(1:imut, 1:jmut, k) = factor * (addval +           &
          &  aexl(1:imut, 1:jmut, 1) * dat3in(1:imut, 1:jmut, k))
      end do
      !
      do k=1, k_max
        where(flag_in(1:imut, 1:jmut, k) == 0.0)
          dat3out(1:imut, 1:jmut, k) = UNDEF
        end where
        where(aexl(1:imut, 1:jmut, 1) == 0.d0)
          dat3out(1:imut, 1:jmut, k) = UNDEF
        end where
      end do
      !
      open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
      write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:k_max)
      close(mtout)
    case('X') !-------------------------------------------------
      dat3out(1:imut, 1:jmut, 1:km) = 0.0
      flag_in(1:imut, 1:jmut, 1:km) = 1.0
      !
      open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*km)
      read (mtin, rec=1) dat3in(:,:,:)
      close(mtin)
      !
      where(dat3in(:,:,:) == UNDEF)
        flag_in(:,:,:) = 0.0
      end where
      do k=1, km
        dat3out(1:imut, 1:jmut, k) = factor * (addval +           &
          &  coefx(1:imut, 1:jmut, k) * dat3in(1:imut, 1:jmut, k))
      end do
      !
      do k=1, km
        where(flag_in(1:imut, 1:jmut, k) == 0.0)
          dat3out(1:imut, 1:jmut, k) = UNDEF
        end where
        where(coefx(1:imut, 1:jmut, k) == 0.d0)
          dat3out(1:imut, 1:jmut, k) = UNDEF
        end where
      end do
      !
      open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*km)
      write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:km)
      close(mtout)
    case('x') !-------------------------------------------------
      dat3out(1:imut, 1:jmut, 1:k_max) = 0.0
      flag_in(1:imut, 1:jmut, 1:k_max) = 1.0
      !
      open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
      read (mtin, rec=1) dat3in(:,:,1:k_max)
      close(mtin)
      !
      where(dat3in(:,:,:) == UNDEF)
        flag_in(:,:,:) = 0.0
      end where
      do k=1, k_max
        dat3out(1:imut, 1:jmut, k) = factor * (addval +           &
          &  coefx(1:imut, 1:jmut, k) * dat3in(1:imut, 1:jmut, k))
      end do
      !
      do k=1, k_max
        where(flag_in(1:imut, 1:jmut, k) == 0.0)
          dat3out(1:imut, 1:jmut, k) = UNDEF
        end where
        where(coefx(1:imut, 1:jmut, 1) == 0.d0)
          dat3out(1:imut, 1:jmut, k) = UNDEF
        end where
      end do
      !
      open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
      write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:k_max)
      close(mtout)
    case('Y') !-------------------------------------------------
      dat3out(1:imut, 1:jmut, 1:km) = 0.0
      flag_in(1:imut, 1:jmut, 1:km) = 1.0
      !
      open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*km)
      read (mtin, rec=1) dat3in(:,:,:)
      close(mtin)
      !
      where(dat3in(:,:,:) == UNDEF)
        flag_in(:,:,:) = 0.0
      end where
      do k=1, km
        dat3out(1:imut, 1:jmut, k) = factor * (addval +           &
          &  coefy(1:imut, 1:jmut, k) * dat3in(1:imut, 1:jmut, k))
      end do
      !
      do k=1, km
        where(flag_in(1:imut, 1:jmut, k) == 0.0)
          dat3out(1:imut, 1:jmut, k) = UNDEF
        end where
        where(coefy(1:imut, 1:jmut, k) == 0.d0)
          dat3out(1:imut, 1:jmut, k) = UNDEF
        end where
      end do
      !
      open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*km)
      write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:km)
      close(mtout)
    case('y') !-------------------------------------------------
      dat3out(1:imut, 1:jmut, 1:k_max) = 0.0
      flag_in(1:imut, 1:jmut, 1:k_max) = 1.0
      !
      open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
      read (mtin, rec=1) dat3in(:,:,1:k_max)
      close(mtin)
      !
      where(dat3in(:,:,:) == UNDEF)
        flag_in(:,:,:) = 0.0
      end where
      do k=1, k_max
        dat3out(1:imut, 1:jmut, k) = factor * (addval +           &
          &  coefy(1:imut, 1:jmut, 1) * dat3in(1:imut, 1:jmut, k))
      end do
      !
      do k=1, k_max
        where(flag_in(1:imut, 1:jmut, k) == 0.0)
          dat3out(1:imut, 1:jmut, k) = UNDEF
        end where
        where(coefy(1:imut, 1:jmut, 1) == 0.d0)
          dat3out(1:imut, 1:jmut, k) = UNDEF
        end where
      end do
      !
      open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*k_max)
      write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:k_max)
      close(mtout)
    case('W') !-------------------------------------------------
      dat3out(1:imut, 1:jmut, 1:km) = 0.0
      flag_in(1:imut, 1:jmut, 1:km) = 1.0
      !
      open(mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*km)
      read (mtin, rec=1) dat3in(:,:,:)
      close(mtin)
      !
      where(dat3in(:,:,:) == UNDEF)
        flag_in(:,:,:) = 0.0
      end where
      dat3out(1:imut, 1:jmut, km) = 0.e0
      do k=1, km-1
        dat3out(1:imut, 1:jmut, k) = factor * (addval +             &
          &  atexl(1:imut, 1:jmut, k+1) * dat3in(1:imut, 1:jmut, k))
      end do
      !
      dat3out(1:imut, 1:jmut, km) = UNDEF
      do k=1, km-1
        where(flag_in(1:imut, 1:jmut, k) == 0.d0)
          dat3out(1:imut, 1:jmut, k) = UNDEF
        end where
        where(atexl(1:imut, 1:jmut, k+1) == 0.d0)
          dat3out(1:imut, 1:jmut, k) = UNDEF
        end where
      end do
      !
      open(mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut*km)
      write(mtout, rec=1) dat3out(1:imut, 1:jmut, 1:km)
      close(mtout)
    case default !-----------------------------------------------
  end select
  !
!====================================================
end program replace
