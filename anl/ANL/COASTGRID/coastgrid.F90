!coastgrid.F90
!====================================================
!
! make coastal T-grid information file
!    i, j, glon, glat, areat
!
!====================================================
program coastgrid
  !
  use oc_mod_param, only : &
  &   imut, jmut,          &
  &   radian_r
  !
  use oc_structure, only :    &
  &   set_hgrids,             &
  &   alatt, alont,           &
#ifdef OGCM_SPHERICAL
  &   calc_scale,             &
#else /* OGCM_SPHERICAL */
  &   read_scale,             &
#endif /* OGCM_SPHERICAL */
  &   read_topo, aexl,        &
  &   set_area, areat
  !
  !----------------------------------------------
  !
  implicit none
  !
  ! 入出力ファイル
  !
  character(len=256)    :: fout_core
  character(len=256)    :: fltopo
#ifdef OGCM_VARIABLE
  character(len=256)    :: file_vgrid
  namelist /nml_coastgrid/ fout_core, fltopo, file_vgrid
#else /* OGCM_VARIABLE */
  namelist /nml_coastgrid/ fout_core, fltopo
#endif /* OGCM_VARIABLE */
#ifndef OGCM_SPHERICAL
  character(len=256)    :: fscale
#endif /* !OGCM_SPHERICAL */
  !
  character(len=256)    :: fout
  integer(4), parameter :: mtout    = 83
  !
  integer(4), allocatable :: ii(:), jj(:)
  real(4),    allocatable :: glon(:), glat(:), area(:)
  integer(4), save :: n
  integer(4)       :: i, j, nmax
  real(8) :: numsea
  !
  !==============================================
  ! read_namelist
  !==============================================
  !
  fout_core  = 'seto'
#ifdef OGCM_VARIABLE
  file_vgrid = 'vgrid.d'
#endif /* OGCM_VARIABLE */
#ifndef OGCM_SPHERICAL
  fscale     = 'scale.d'
#endif /* !OGCM_SPHERICAL */
  fltopo     = 'topo.d'
  !
  write(*,*) '=== reading namelist: nml_coastgrid'
  read(unit=5, nml_coastgrid)
  write(*,*) 'output file : ', trim(fout_core)
#ifdef OGCM_VARIABLE
  write(*,*) 'file_vgrid  : ', trim(file_vgrid)
#endif /* OGCM_VARIABLE */
#ifndef OGCM_SPHERICAL
  write(*,*) 'scale file  : ', trim(fscale)
#endif /* !OGCM_SPHERICAL */
  write(*,*) 'topography  : ', trim(fltopo)
  !
  nmax = imut * jmut / 4
write(*,*) "nmax:", nmax
  allocate( ii(1:nmax) )
  allocate( jj(1:nmax) )
  allocate( glon(1:nmax))
  allocate( glat(1:nmax) )
  allocate( area(1:nmax) )
  !
  !----------------------------------------------
  !
#ifdef OGCM_VARIABLE
  call set_hgrids(file_vgrid)
#else /* OGCM_VARIABLE */
  call set_hgrids
#endif /* OGCM_VARIABLE */
  call read_topo(fltopo)
#ifdef OGCM_SPHERICAL
  call calc_scale
#else /* OGCM_SPHERICAL */
  call read_scale(fscale)
#endif /* OGCM_SPHERICAL */
  !
  call set_area
  !
  !----------------------------------------------
  !
  n = 0
#if defined OGCM_JOT || defined OGCM_TRIPOLAR
  do j = 2, jmut-2
#else /* defined OGCM_JOT || defined OGCM_TRIPOLAR */
  do j = 2, jmut-1
#endif /* defined OGCM_JOT || defined OGCM_TRIPOLAR */
#ifdef OGCM_CYCLIC
    do i = 3, imut-2
#else /* OGCM_CYCLIC */
    do i = 2, imut-1
#endif /* OGCM_CYCLIC */
      numsea = aexl(i-1,j  ,1) + aexl(i,j  ,1) &
             + aexl(i-1,j-1,1) + aexl(i,j-1,1)
      if(0.0d0 < numsea .and. numsea < 4.0d0) then
        n = n + 1
        ii(n) = i
        jj(n) = j
        glon(n) = real(alont(i),     4)
        glat(n) = real(alatt(j),     4)
        area(n) = real(areat(i,j,1), 4)
        write(*,'(i5, i5, f9.3, f9.3, e13.4)') ii(n), jj(n), glon(n), glat(n), area(n)
      end if
    end do
  end do
  !
  write(fout, '(a, a, i8.8, a)') trim(fout_core), "_", n, ".d"
  open(unit=mtout, file=fout, form='unformatted')
  do i = 1, n
    write(mtout) ii(i), jj(i), glon(i), glat(i), area(i)
  end do
  close(mtout)
  !
!====================================================
end program coastgrid
