!glb2rgn_ij.F90
!====================================================
!
!  Regional Data out of Global Data
!
!====================================================
program glb2rgn_ij
  !
  use oc_mod_param, only : &
  &   imut, jmut, km
  !
  use oc_structure, only : &
  &  read_topo,            &
  &  aexl, atexl
  !
  !----------------------------------------------
  !
  implicit none
  !
  character(len=256) :: fltopo
  !
  integer(4), save :: i_begin = 1
  integer(4), save :: i_end   = imut
  integer(4), save :: j_begin = 1
  integer(4), save :: j_end   = jmut
  integer(4), save :: k_2d    = 1
  !
  real(4), save :: UNDEF = -9.99e33
  !
  real(4), allocatable :: data4in(:, :, :)
  real(4), allocatable :: data4out(:, :, :)
  !
  character(len=256) :: flin
  character(len=256) :: flout
  character(len=256) :: flout2
  character(len=4)   :: tuxy
  !
  integer(4), parameter :: mtin  = 80
  integer(4), parameter :: mtout = 81
  !
  integer(4) :: i_range, j_range, k
  !
  namelist /nml_glb2rgn_ij/  flin,    flout, &
    &                        fltopo,  undef, &
    &                        tuxy,    k_2d,  &
    &                        i_begin, i_end, &
    &                        j_begin, j_end
  !
  !==============================================
  !
  !
#ifdef OGCM_CYCLIC
  i_begin = 3
  i_end   = imut -2
#endif /* OGCM_CYCLIC */
  !
  read(unit=5, nml_glb2rgn_ij)
  write(*,*) 'flin    :', trim(flin)
  write(*,*) 'flout   :', trim(flout)
  write(*,*) 'fltopo  :', trim(fltopo)
  write(*,*) 'UDEF    :', undef
  write(*,*) 'tuxy    :', trim(tuxy)
  write(*,*) 'k_2d    :', k_2d
  write(*,*) 'i_begin :', i_begin
  write(*,*) 'i_end   :', i_end
  write(*,*) 'j_begin :', j_begin
  write(*,*) 'j_end   :', j_end
  !
  i_range = i_end -i_begin +1
  j_range = j_end -j_begin +1
  write(*,*) 'j_range :', j_range
  ! 
  call read_topo(fltopo)
  !
  !==============================================
  select case(tuxy)
    !-----------------------------------------------------------------
    case('u') !  Surface Flux at UV grids
      allocate(data4in(imut, jmut, k_2d))
      allocate(data4out(i_range, j_range, k_2d))
      open (mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*k_2d)
      read (mtin, rec = 1) data4in(1:imut, 1:jmut, 1:k_2d)
      close(mtin)
      !
      do k = 1, k_2d
        where(aexl(1:imut, 1:jmut, 1) == 0.d0)
          data4in(1:imut, 1:jmut, k) = UNDEF
        end where
      end do
      !
      data4out(1:i_range, 1:j_range, 1:k_2d) = data4in(i_begin:i_end, j_begin:j_end, 1:k_2d)
      !
      do k = 1, k_2d
        write(flout2, '(a, a, i2.2)') trim(flout), '_', k
        open (mtout, file=flout2, form='unformatted', access='direct', recl=4*i_range*j_range)
        write (mtout, rec = 1) data4out(1:i_range, 1:j_range, k)
        close(mtout)
      end do
    !-----------------------------------------------------------------
    case('t') !  Surface Flux at TS grids
      allocate(data4in(imut, jmut, k_2d))
      allocate(data4out(i_range, j_range, k_2d))
      open (mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut)
      read (mtin, rec = 1) data4in(1:imut, 1:jmut, 1:1)
      close(mtin)
      !
      do k = 1, k_2d
        where(aexl(1:imut, 1:jmut, 1) == 0.d0)
          data4in(1:imut, 1:jmut, k) = UNDEF
        end where
      end do
      !
      data4out(1:i_range, 1:j_range, 1:k_2d) = data4in(i_begin:i_end, j_begin:j_end, 1:k_2d)
      !
      do k = 1, k_2d
        write(flout2, '(a, a, i2.2)') trim(flout), '_', k
        open (mtout, file=flout, form='unformatted', access='direct', recl=4*i_range*j_range)
        write (mtout, rec = 1) data4out(1:i_range, 1:j_range, k)
        close(mtout)
      end do
    !-----------------------------------------------------------------
    !-----------------------------------------------------------------
    case('U') !  3D data at UV grids
      allocate(data4in(imut, jmut, km))
      allocate(data4out(i_range, j_range, km))
      open (mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut*km)
      read (mtin, rec = 1) data4in(1:imut, 1:jmut, 1:km)
      close(mtin)
      !
      do k = 1, km
        where(aexl(1:imut, 1:jmut, k) == 0.d0)
          data4in(1:imut, 1:jmut, k) = UNDEF
        end where
      end do
      !
      data4out(1:i_range, 1:j_range, 1:km) = data4in(i_begin:i_end, j_begin:j_end, 1:km)
      !
      open (mtout, file=flout, form='unformatted', access='direct', recl=4*i_range*j_range*km)
      write (mtout, rec = 1) data4out(1:i_range, 1:j_range, 1:km)
      close(mtout)
    case('T') !  Surface Flux at TS grids
      allocate(data4in(imut, jmut, km))
      allocate(data4out(i_range, j_range, km))
      open (mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut)
      read (mtin, rec = 1) data4in(1:imut, 1:jmut, 1:km)
      close(mtin)
      !
      do k = 1, km
        where(aexl(1:imut, 1:jmut, k) == 0.d0)
          data4in(1:imut, 1:jmut, k) = UNDEF
        end where
      end do
      !
      data4out(1:i_range, 1:j_range, 1:km) = data4in(i_begin:i_end, j_begin:j_end, 1:km)
      !
      open (mtout, file=flout, form='unformatted', access='direct', recl=4*i_range*j_range*km)
      write (mtout, rec = 1) data4out(1:i_range, 1:j_range, 1:km)
      close(mtout)
    case default
      write(*,*) 'tuxy: u or t'
      stop
  end select
  !
!====================================================
end program glb2rgn_ij
