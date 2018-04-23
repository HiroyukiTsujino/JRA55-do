! -*-F90-*-
!=========================================================================
program mkgrads_restart_ocean
  !-----------------------------------------------------------------------
  !     Make GrADS data from OGCM output
  !-----------------------------------------------------------------------

  use basin_param
  use grid_common

  implicit none

  real(8) :: dat3(imut,jmut,km)
  real(8) :: dat2(imut,jmut)
  real(4) :: dat4(imut,jmut)

  integer(4) :: i, j, k, n
  integer(4) :: nkai, month, iday, ihour, imin

#ifdef OGCM_VVDIMP
#if defined OGCM_MELYAM || defined OGCM_NOHKIM
#ifdef OGCM_MELYAM

#include "ocean_restart_item_melyam.F90"

#endif /* OGCM_MELYAM */
#ifdef OGCM_NOHKIM

#include "ocean_restart_item_nohkim.F90"

#endif /* OGCM_NOHKIM */
#else /* OGCM_MELYAM || OGCM_NOHKIM */

#include "ocean_restart_item_vvdimp.F90"

#endif /* OGCM_MELYAM || OGCM_NOHKIM */
#else /* OGCM_VVDIMP */

#include "ocean_restart_item.F90"

#endif /* OGCM_VVDIMP */


  integer(4), parameter :: mtin1=61, mtin2=62, mtin3=63, mtin4=64, mtin5=65
  integer(4), parameter :: mtin6=66, mtin7=67, mtin8=68, mtin9=69
  integer(4), parameter :: mtina=71, mtinb=72, mtinc=73, mtind=74, mtine=75

  integer(4), parameter :: mtot1=81, mtot2=82, mtot3=83, mtot4=84, mtot5=85
  integer(4), parameter :: mtot6=86, mtot7=87, mtot8=88, mtot9=89
  integer(4), parameter :: mtota=91, mtotb=92, mtotc=93, mtotd=94, mtote=75

  integer(4) :: irec1, irec2, irec3, irec4, irec5
  integer(4) :: irec6, irec7, irec8, irec9
  integer(4) :: ireca, irecb, irecc, irecd, irece

  integer(4) :: inum

  logical :: lsplit

  character(len=256) :: file_in
  character(len=256) :: file_grid, file_topo

  character(len=256) :: file_in_u, file_in_v, file_in_t, file_in_s, file_in_h
  character(len=256) :: file_out_u, file_out_v, file_out_t, file_out_s, file_out_h, file_out_um, file_out_vm
  character(len=256) :: file_ctl_u, file_ctl_v, file_ctl_t, file_ctl_s, file_ctl_h, file_ctl_um, file_ctl_vm

#ifdef OGCM_VVDIMP
  character(len=256) :: file_in_avd, file_in_avm
  character(len=256) :: file_out_avd, file_out_avm
  character(len=256) :: file_ctl_avd, file_ctl_avm
#ifdef OGCM_MELYAM
  character(len=256) :: file_in_avq, file_in_q, file_in_alo
  character(len=256) :: file_out_avq, file_out_q, file_out_alo
  character(len=256) :: file_ctl_avq, file_ctl_q, file_ctl_alo
#endif /* OGCM_MELYAM */
#ifdef OGCM_NOHKIM
  character(len=256) :: file_in_avq, file_in_eb
  character(len=256) :: file_out_avq, file_out_eb
  character(len=256) :: file_ctl_avq, file_ctl_eb
#endif /* OGCM_NOHKIM */
#endif /* OGCM_VVDIMP */

  real(4) :: undef = 0.0
  real(4) :: undef8 = 0.0d0

  logical :: lmakectl
  logical :: linear_x = .false.
  logical :: linear_y = .false.
  logical :: linear_z = .false.

  character(len=256) :: varname_tmp(1)
  integer(4) :: ndims_tmp(1)
  real(8) :: dep_tmp(km)

  character(len=256),parameter :: title = 'MRICOM SNAP SHOT (RESTART) DATA'

  character(len=256),parameter :: fmt_axis_h = 'f8.3'
  character(len=256),parameter :: fmt_axis_v = 'f8.2'
  character(len=256),parameter :: time_dim = 'TDEF 1 LINEAR  15JAN1001 1mo'

  character(len=256),parameter :: opt_general = ' '
!  character(len=256),parameter :: opt_general = 'template'

  character(len=256),parameter :: opt_hint_ts = ' '
  character(len=256),parameter :: opt_hint_uv = ' '
!  character(len=256),parameter :: opt_hint_ts = 'PDEF 364 368 BILIN STREAM BINARY-BIG ^pdef.masku.bilin.gd'
!  character(len=256),parameter :: opt_hint_uv = 'PDEF 364 368 BILIN STREAM BINARY-BIG ^pdef.maskt.bilin.gd'

  character(len=256) :: cundef

  !---------------------------------------------------------------------
  
#ifdef OGCM_VVDIMP
#if defined OGCM_MELYAM || defined OGCM_NOHKIM
#ifdef OGCM_MELYAM
  namelist /nocrst/ file_grid, file_topo, &
       & lsplit, &
       & file_in, &
       & file_in_u,  file_in_v,   file_in_t,  file_in_s, &
       & file_in_h,  &
       & file_in_avd, file_in_avm,  file_in_avq, file_in_q, &
       & file_in_alo, &
       & file_out_u, file_out_v,  file_out_t, file_out_s, &
       & file_out_h, file_out_um, file_out_vm, &
       & file_out_avd, file_out_avm,  file_out_avq, file_out_q, &
       & file_out_alo, &
       & file_ctl_u, file_ctl_v,  file_ctl_t, file_ctl_s, &
       & file_ctl_h, file_ctl_um, file_ctl_vm, &
       & file_ctl_avd, file_ctl_avm, file_ctl_avq, file_ctl_q, &
       & file_ctl_alo, &
       & inum, undef, lmakectl, linear_x, linear_y, linear_z
#endif /* OGCM_MELYAM */
#ifdef OGCM_NOHKIM
  namelist /nocrst/ file_grid, file_topo, &
       & lsplit, &
       & file_in, &
       & file_in_u,  file_in_v,   file_in_t,  file_in_s,  file_in_h,  &
       & file_in_avd, file_in_avm,  file_in_avq, file_in_eb, &
       & file_out_u, file_out_v,  file_out_t, file_out_s, &
       & file_out_h, file_out_um, file_out_vm, &
       & file_out_avd, file_out_avm,  file_out_avq, file_out_eb, &
       & file_ctl_u, file_ctl_v,  file_ctl_t, file_ctl_s, &
       & file_ctl_h, file_ctl_um, file_ctl_vm, &
       & file_ctl_avd, file_ctl_avm, file_ctl_avq, file_ctl_eb, &
       & inum, undef, lmakectl, linear_x, linear_y, linear_z
#endif /* OGCM_NOHKIM */
#else /* OGCM_MELYAM || OGCM_NOHKIM */
  namelist /nocrst/ file_grid, file_topo, &
       & lsplit, &
       & file_in, &
       & file_in_u,  file_in_v,   file_in_t,  file_in_s,  file_in_h,  &
       & file_in_avd, file_in_avm, &
       & file_out_u, file_out_v,  file_out_t, file_out_s, &
       & file_out_h, file_out_um, file_out_vm, &
       & file_out_avd, file_out_avm, &
       & file_ctl_u, file_ctl_v,  file_ctl_t, file_ctl_s, &
       & file_ctl_h, file_ctl_um, file_ctl_vm, &
       & file_ctl_avd, file_ctl_avm, &
       & inum, undef, lmakectl, linear_x, linear_y, linear_z
#endif /* OGCM_MELYAM || OGCM_NOHKIM */
#else /* OGCM_VVDIMP */
  namelist /nocrst/ file_grid, file_topo, &
       & lsplit, &
       & file_in, &
       & file_in_u,  file_in_v,   file_in_t,  file_in_s,  file_in_h,  &
       & file_out_u, file_out_v,  file_out_t, file_out_s, &
       & file_out_h, file_out_um, file_out_vm, &
       & file_ctl_u, file_ctl_v,  file_ctl_t, file_ctl_s, &
       & file_ctl_h, file_ctl_um, file_ctl_vm, &
       & inum, undef, lmakectl, linear_x, linear_y, linear_z
#endif /* OGCM_VVDIMP */

  read(5,nocrst) 

  !---------------------------------------------------------------------

  call setgrd(file_topo, file_grid)

  write(cundef,'(E12.5)') undef
  undef8 = undef

  write(6,*) 'Grid spacing from = ', trim(file_grid)
  write(6,*) 'Topography from   = ', trim(file_topo)
  write(6,*) 'Missing value     = ', trim(cundef)

  !----------------------------------------------------------------

  write(*,*) 'reading ', inum ,' data'

  !------------------------------------------------------------------------------

  if (lsplit) then

    open(mtin1, file=file_in_u,form='unformatted',action='read')
    write(*,*) 'input (U) from ... ', trim(file_in_u)

    open(mtin2, file=file_in_v,form='unformatted',action='read')
    write(*,*) 'input (V) from ... ', trim(file_in_v)

    open(mtin3, file=file_in_t,form='unformatted',action='read')
    write(*,*) 'input (T) from ... ', trim(file_in_t)

    open(mtin4, file=file_in_s,form='unformatted',action='read')
    write(*,*) 'input (S) from ... ', trim(file_in_s)

    open(mtin5, file=file_in_h,form='unformatted',action='read')
    write(*,*) 'input (SSH) from ... ', trim(file_in_h)

#ifdef OGCM_VVDIMP

    open(mtina, file=file_in_avd,form='unformatted',action='read')
    write(*,*) 'input (AVD) from ... ', trim(file_in_avd)

    open(mtinb, file=file_in_avm,form='unformatted',action='read')
    write(*,*) 'input (AVM) from ... ', trim(file_in_avm)

#if defined OGCM_MELYAM || defined OGCM_NOHKIM

    open(mtinc, file=file_in_avq,form='unformatted',action='read')
    write(*,*) 'input (AVQ) from ... ', trim(file_in_avq)

#ifdef OGCM_MELYAM

    open(mtind, file=file_in_q,form='unformatted',action='read')
    write(*,*) 'input (Q) from ... ', trim(file_in_q)

    open(mtine, file=file_in_alo,form='unformatted',action='read')
    write(*,*) 'input (ALO) from ... ', trim(file_in_alo)

#endif /* OGCM_MELYAM */
#ifdef OGCM_NOHKIM

    open(mtind, file=file_in_eb,form='unformatted',action='read')
    write(*,*) 'input (EB) from ... ', trim(file_in_eb)

#endif /* OGCM_NOHKIM */
#endif /* OGCM_MELYAM || OGCM_NOHKIM */
#endif /* OGCM_VVDIMP */

  else

    open(mtin1, file=file_in,form='unformatted',action='read')
    write(*,*) 'reading from ... ', trim(file_in)

  end if

  !-------

  open(mtot1, file=file_out_u,form='unformatted',access='direct',recl=4*imut*jmut)
  write(*,*) 'output (U) to ... ', trim(file_out_u)
  irec1 = 0

  open(mtot2, file=file_out_v,form='unformatted',access='direct',recl=4*imut*jmut)
  write(*,*) 'output (V) to ... ', trim(file_out_v)
  irec2 = 0

  open(mtot3, file=file_out_t,form='unformatted',access='direct',recl=4*imut*jmut)
  write(*,*) 'output (T) to ... ', trim(file_out_t)
  irec3 = 0

  open(mtot4, file=file_out_s,form='unformatted',access='direct',recl=4*imut*jmut)
  write(*,*) 'output (S) to ... ', trim(file_out_s)
  irec4 = 0

  open(mtot5, file=file_out_h,form='unformatted',access='direct',recl=4*imut*jmut)
  write(*,*) 'output (SSH) to ... ', trim(file_out_h)
  irec5 = 0

  open(mtot6, file=file_out_um,form='unformatted',access='direct',recl=4*imut*jmut)
  write(*,*) 'output (UM) to ... ', trim(file_out_um)
  irec6 = 0

  open(mtot7, file=file_out_vm,form='unformatted',access='direct',recl=4*imut*jmut)
  write(*,*) 'output (VM) to ... ', trim(file_out_vm)
  irec7 = 0

#ifdef OGCM_VVDIMP

  open(mtota, file=file_out_avd,form='unformatted',action='write',access='direct',recl=4*imut*jmut)
  write(*,*) 'output (AVD) to ... ', trim(file_out_avd)
  ireca = 0

  open(mtotb, file=file_out_avm,form='unformatted',action='write',access='direct',recl=4*imut*jmut)
  write(*,*) 'output (AVM) to ... ', trim(file_out_avm)
  irecb = 0

#if defined OGCM_MELYAM || defined OGCM_NOHKIM

  open(mtotc, file=file_out_avq,form='unformatted',action='write',access='direct',recl=4*imut*jmut)
  write(*,*) 'output (AVQ) to ... ', trim(file_out_avq)
  irecc = 0

#ifdef OGCM_MELYAM

  open(mtotd, file=file_out_q,form='unformatted',action='write',access='direct',recl=4*imut*jmut)
  write(*,*) 'output (Q) to ... ', trim(file_out_q)
  irecd = 0

  open(mtote, file=file_out_alo,form='unformatted',action='write',access='direct',recl=4*imut*jmut)
  write(*,*) 'output (ALO) to ... ', trim(file_out_alo)
  irece = 0

#endif /* OGCM_MELYAM */
#ifdef OGCM_NOHKIM

  open(mtotd, file=file_out_eb,form='unformatted',action='write',access='direct',recl=4*imut*jmut)
  write(*,*) 'output (EB) to ... ', trim(file_out_eb)
  irecd = 0

#endif /* OGCM_NOHKIM */
#endif /* OGCM_MELYAM || OGCM_NOHKIM */
#endif /* OGCM_VVDIMP */

  !------------------------------------------------------------------------

  do n = 1, inum

    ! U  

    read (mtin1,end=999) NKAI,MONTH,IDAY,IHOUR,IMIN
    write(*,*) NKAI,MONTH,IDAY,IHOUR,IMIN
    read (mtin1,end=999) dat3

    do k = 1, km

      do j = 1, jmut
        do i = 1, imut
          dat4(i,j) = aexl(i,j,k) * dat3(i,j,k) + (1.0d0 - aexl(i,j,k)) * undef8
        end do
      end do

      irec1 = irec1 + 1

      write(mtot1,rec=irec1) dat4

    end do

    ! V

    if (lsplit) then
      read (mtin2,end=999) NKAI,MONTH,IDAY,IHOUR,IMIN
      write(*,*) NKAI,MONTH,IDAY,IHOUR,IMIN
      read (mtin2,end=999) dat3
    else
      read (mtin1,end=999) dat3
    end if

    do k = 1, km

      do j = 1, jmut
        do i = 1, imut
          dat4(i,j) = aexl(i,j,k) * dat3(i,j,k) + (1.0d0 - aexl(i,j,k)) * undef8
        end do
      end do

      irec2 = irec2 + 1
      write(mtot2,rec=irec2) dat4

    end do

    ! T

    if (lsplit) then
      read (mtin3,end=999) NKAI,MONTH,IDAY,IHOUR,IMIN
      write(*,*) NKAI,MONTH,IDAY,IHOUR,IMIN
      read (mtin3,end=999) dat3
    else
      read (mtin1,end=999) dat3
    end if

    do k = 1, km

      do j = 1, jmut
        do i = 1, imut
          dat4(i,j) = atexl(i,j,k) * dat3(i,j,k) + (1.0d0 - atexl(i,j,k)) * undef8
        end do
      end do

      irec3 = irec3 + 1
      write(mtot3,rec=irec3) dat4

    end do

    ! S

    if (lsplit) then
      read (mtin4,end=999) NKAI,MONTH,IDAY,IHOUR,IMIN
      write(*,*) NKAI,MONTH,IDAY,IHOUR,IMIN
      read (mtin4,end=999) dat3
    else
      read (mtin1,end=999) dat3
    end if

    do k = 1, km

      do j = 1, jmut
        do i = 1, imut
          dat4(i,j) = atexl(i,j,k) * dat3(i,j,k) + (1.0d0 - atexl(i,j,k)) * undef8
        end do
      end do

      irec4 = irec4 + 1
      write(mtot4,rec=irec4) dat4
    end do

    ! Surface height

    if (lsplit) then
      read (mtin5,end=999) NKAI,MONTH,IDAY,IHOUR,IMIN
      write(*,*) NKAI,MONTH,IDAY,IHOUR,IMIN
      read(mtin5,end=999) dat3(1:imut,1:jmut,1:3)
    else
      read(mtin1,end=999) dat3(1:imut,1:jmut,1:3)
    end if

    do j = 1, jmut
      do i = 1, imut
        dat4(i,j) = atexl(i,j,1) * dat3(i,j,1) + (1.0d0 - atexl(i,j,1)) * undef8
      end do
    end do
    
    irec5 = irec5 + 1
    write(mtot5,rec=irec5) dat4

    !-----

    do j = 1, jmut
      do i = 1, imut
        dat4(i,j) = aexl(i,j,1) * dat3(i,j,2) + (1.0d0 - aexl(i,j,1)) * undef8
      end do
    end do
    
    irec6 = irec6 + 1
    write(mtot6,rec=irec6) dat4

    !-----

    do j = 1, jmut
      do i = 1, imut
        dat4(i,j) = aexl(i,j,1) * dat3(i,j,3) + (1.0d0 - aexl(i,j,1)) * undef8
      end do
    end do
    
    irec7 = irec7 + 1
    write(mtot7,rec=irec7) dat4

#ifdef OGCM_VVDIMP

    ! AVD

    if (lsplit) then
      read (mtina,end=999) NKAI,MONTH,IDAY,IHOUR,IMIN
      write(*,*) NKAI,MONTH,IDAY,IHOUR,IMIN
      read (mtina,end=999) dat3
    else
      read (mtin1,end=999) dat3
    end if

    do k = 1, km

      do j = 1, jmut
        do i = 1, imut
          dat4(i,j) = atexl(i,j,k) * dat3(i,j,k) + (1.0d0 - atexl(i,j,k)) * undef8
        end do
      end do

      ireca = ireca + 1
      write(mtota,rec=ireca) dat4

    end do

    ! AVM

    if (lsplit) then
      read (mtinb,end=999) NKAI,MONTH,IDAY,IHOUR,IMIN
      write(*,*) NKAI,MONTH,IDAY,IHOUR,IMIN
      read (mtinb,end=999) dat3
    else
      read (mtin1,end=999) dat3
    end if

    do k = 1, km

      do j = 1, jmut
        do i = 1, imut
          dat4(i,j) = atexl(i,j,k) * dat3(i,j,k) + (1.0d0 - atexl(i,j,k)) * undef8
        end do
      end do

      irecb = irecb + 1
      write(mtotb,rec=irecb) dat4

    end do

#if defined OGCM_MELYAM || defined OGCM_NOHKIM

    ! AVQ

    if (lsplit) then
      read (mtinc,end=999) NKAI,MONTH,IDAY,IHOUR,IMIN
      write(*,*) NKAI,MONTH,IDAY,IHOUR,IMIN
      read (mtinc,end=999) dat3
    else
      read (mtin1,end=999) dat3
    end if

    do k = 1, km

      do j = 1, jmut
        do i = 1, imut
          dat4(i,j) = atexl(i,j,k) * dat3(i,j,k) + (1.0d0 - atexl(i,j,k)) * undef8
        end do
      end do

      irecc = irecc + 1
      write(mtotc,rec=irecc) dat4

    end do

#ifdef OGCM_MELYAM

    ! Q

    if (lsplit) then
      read (mtind,end=999) NKAI,MONTH,IDAY,IHOUR,IMIN
      write(*,*) NKAI,MONTH,IDAY,IHOUR,IMIN
      read (mtind,end=999) dat3
    else
      read (mtin1,end=999) dat3
    end if

    do k = 1, km

      do j = 1, jmut
        do i = 1, imut
          dat4(i,j) = atexl(i,j,k) * dat3(i,j,k) + (1.0d0 - atexl(i,j,k)) * undef8
        end do
      end do

      irecd = irecd + 1
      write(mtotd,rec=irecd) dat4

    end do

    ! ALO

    if (lsplit) then
      read (mtin5,end=999) NKAI,MONTH,IDAY,IHOUR,IMIN
      write(*,*) NKAI,MONTH,IDAY,IHOUR,IMIN
      read(mtine,end=999) dat2
    else
      read(mtin1,end=999) dat2
    end if

    do j = 1, jmut
      do i = 1, imut
        dat4(i,j) = atexl(i,j,1) * dat3(i,j,1) + (1.0d0 - atexl(i,j,1)) * undef8
      end do
    end do

    irece = irece + 1
    write(mtote,rec=irece) dat4

#endif /* OGCM_MELYAM */
#ifdef OGCM_NOHKIM

    ! EB

    if (lsplit) then
      read (mtind,end=999) NKAI,MONTH,IDAY,IHOUR,IMIN
      write(*,*) NKAI,MONTH,IDAY,IHOUR,IMIN
      read (mtind,end=999) dat3
    else
      read (mtin1,end=999) dat3
    end if

    do k = 1, km

      do j = 1, jmut
        do i = 1, imut
          dat4(i,j) = atexl(i,j,k) * dat3(i,j,k) + (1.0d0 - atexl(i,j,k)) * undef8
        end do
      end do

      irecd = irecd + 1
      write(mtotd,rec=irecd) dat4

    end do

#endif /* OGCM_NOHKIM */
#endif /* OGCM_MELYAM || OGCM_NOHKIM */
#endif /* OGCM_VVDIMP */

  end do

999 continue

  if (lsplit) then
    close(mtin1)
    close(mtin2)
    close(mtin3)
    close(mtin4)
    close(mtin5)
#ifdef OGCM_VVDIMP
    close(mtina)
    close(mtinb)
#if defined OGCM_MELYAM || defined OGCM_NOHKIM
    close(mtinc)
    close(mtind)
#ifdef OGCM_MELYAM
    close(mtine)
#endif /* OGCM_MELYAM */
#endif /* OGCM_MELYAM || OGCM_NOHKIM */
#endif /* OGCM_VVDIMP */
  else
    close(mtin1)
  end if

#ifdef OGCM_VVDIMP
#if defined OGCM_MELYAM || defined OGCM_NOHKIM
#ifdef OGCM_MELYAM
  close(mtine)
#endif /* OGCM_MELYAM */
  close(mtind)
  close(mtinc)
#endif /* OGCM_MELYAM || OGCM_NOHKIM */
  close(mtota)
  close(mtotb)
#endif /* OGCM_VVDIMP */
  close(mtot5)
  close(mtot4)
  close(mtot3)
  close(mtot2)
  close(mtot1)

  !--------------------------------------------------------------

  if (lmakectl) then

    varname_tmp(1) = varname(1)
    ndims_tmp(1) = ndims(1)

    call mkgrdctl ( &
         &   file_ctl_u, file_out_u,   title,              &
         &   imut, alonu, fmt_axis_h, linear_x,            &
         &   jmut, alatu, fmt_axis_h, linear_y,            &
         &   km  , dp   , fmt_axis_v, linear_z,            &
         &   time_dim   ,        1,        1,              &
         &   varname_tmp,  ndims_tmp,                      &
         &   cundef, opt_hint_uv, opt_general              &
         &   )

    varname_tmp(1) = varname(2)
    ndims_tmp(1) = ndims(2)

    call mkgrdctl ( &
         &   file_ctl_v, file_out_v,   title,              &
         &   imut, alonu, fmt_axis_h, linear_x,            &
         &   jmut, alatu, fmt_axis_h, linear_y,            &
         &   km  , dp   , fmt_axis_v, linear_z,            &
         &   time_dim   ,        1,        1,              &
         &   varname_tmp,  ndims_tmp,                      &
         &   cundef, opt_hint_uv, opt_general              &
         &   )

    varname_tmp(1) = varname(3)
    ndims_tmp(1) = ndims(3)

    call mkgrdctl ( &
         &   file_ctl_t, file_out_t,   title,              &
         &   imut, alont, fmt_axis_h, linear_x,            &
         &   jmut, alatt, fmt_axis_h, linear_y,            &
         &   km  , dp   , fmt_axis_v, linear_z,            &
         &   time_dim   ,        1,        1,              &
         &   varname_tmp,  ndims_tmp,                      &
         &   cundef, opt_hint_ts, opt_general              &
         &   )

    varname_tmp(1) = varname(4)
    ndims_tmp(1) = ndims(4)

    call mkgrdctl ( &
         &   file_ctl_s, file_out_s,   title,              &
         &   imut, alont, fmt_axis_h, linear_x,            &
         &   jmut, alatt, fmt_axis_h, linear_y,            &
         &   km  , dp   , fmt_axis_v, linear_z,            &
         &      time_dim,        1,        1,              &
         &   varname_tmp,  ndims_tmp,                      &
         &        cundef, opt_hint_ts, opt_general         &
         &   )

    varname_tmp(1) = varname(5)
    ndims_tmp(1) = ndims(5)

    call mkgrdctl ( &
         &   file_ctl_h,   file_out_h,    title,           &
         &   imut, alont,   fmt_axis_h,  linear_x,         &
         &   jmut, alatt,   fmt_axis_h,  linear_y,         &
         &     km,    dp,   fmt_axis_v,  linear_z,         &
         &      time_dim,          1,         1,           &
         &   varname_tmp,  ndims_tmp,                      &
         &        cundef, opt_hint_ts, opt_general         &
         &   )

    varname_tmp(1) = varname(6)
    ndims_tmp(1) = ndims(6)

    call mkgrdctl ( &
         &   file_ctl_um,   file_out_um,    title,         &
         &   imut, alonu,   fmt_axis_h,  linear_x,         &
         &   jmut, alatu,   fmt_axis_h,  linear_y,         &
         &     km,    dp,   fmt_axis_v,  linear_z,         &
         &      time_dim,          1,         1,           &
         &   varname_tmp,  ndims_tmp,                      &
         &        cundef, opt_hint_uv, opt_general         &
         &   )

    varname_tmp(1) = varname(7)
    ndims_tmp(1) = ndims(7)

    call mkgrdctl ( &
         &   file_ctl_vm,   file_out_vm,    title,         &
         &   imut, alonu,   fmt_axis_h,  linear_x,         &
         &   jmut, alatu,   fmt_axis_h,  linear_y,         &
         &     km,    dp,   fmt_axis_v,  linear_z,         &
         &      time_dim,          1,         1,           &
         &   varname_tmp,  ndims_tmp,                      &
         &        cundef, opt_hint_uv, opt_general         &
         &   )

#ifdef OGCM_VVDIMP

    varname_tmp(1) = varname(8)
    ndims_tmp(1) = ndims(8)
    dep_tmp(1:km) = dep(2:km+1)

    call mkgrdctl ( &
         &   file_ctl_avd, file_out_avd,   title,          &
         &   imut, alont, fmt_axis_h, linear_x,            &
         &   jmut, alatt, fmt_axis_h, linear_y,            &
         &     km, dep_tmp, fmt_axis_v, linear_z,          &
         &      time_dim,        1,        1,              &
         &   varname_tmp,  ndims_tmp,                      &
         &   cundef, opt_hint_ts, opt_general              &
         &   )

    varname_tmp(1) = varname(9)
    ndims_tmp(1) = ndims(9)
    dep_tmp(1:km) = dep(2:km+1)

    call mkgrdctl ( &
         &   file_ctl_avm, file_out_avm,   title,          &
         &   imut, alont, fmt_axis_h, linear_x,            &
         &   jmut, alatt, fmt_axis_h, linear_y,            &
         &     km, dep_tmp, fmt_axis_v, linear_z,          &
         &      time_dim,        1,        1,              &
         &   varname_tmp,  ndims_tmp,                      &
         &   cundef, opt_hint_ts, opt_general              &
         &   )

#if defined OGCM_MELYAM || defined OGCM_NOHKIM

    varname_tmp(1) = varname(10)
    ndims_tmp(1) = ndims(10)
    dep_tmp(1:km) = dep(2:km+1)

    call mkgrdctl ( &
         &   file_ctl_avq, file_out_avq,   title,          &
         &   imut, alont, fmt_axis_h, linear_x,            &
         &   jmut, alatt, fmt_axis_h, linear_y,            &
         &     km, dep_tmp, fmt_axis_v, linear_z,          &
         &      time_dim,        1,        1,              &
         &   varname_tmp,  ndims_tmp,                      &
         &   cundef, opt_hint_ts, opt_general              &
         &   )

#ifdef OGCM_MELYAM

    varname_tmp(1) = varname(11)
    ndims_tmp(1) = ndims(11)
    dep_tmp(1:km) = dep(2:km+1)

    call mkgrdctl ( &
         &   file_ctl_q, file_out_q,   title,          &
         &   imut, alont, fmt_axis_h, linear_x,            &
         &   jmut, alatt, fmt_axis_h, linear_y,            &
         &     km, dep_tmp, fmt_axis_v, linear_z,          &
         &      time_dim,        1,        1,              &
         &   varname_tmp,  ndims_tmp,                      &
         &   cundef, opt_hint_ts, opt_general              &
         &   )

    varname_tmp(1) = varname(12)
    ndims_tmp(1) = ndims(12)

    call mkgrdctl ( &
         &   file_ctl_alo,   file_out_alo,    title,       &
         &   imut, alont,   fmt_axis_h,  linear_x,         &
         &   jmut, alatt,   fmt_axis_h,  linear_y,         &
         &     km,    dp,   fmt_axis_v,  linear_z,         &
         &      time_dim,          1,         1,           &
         &   varname_tmp,  ndims_tmp,                      &
         &        cundef, opt_hint_ts, opt_general         &
         &   )

#endif /* OGCM_MELYAM */

#ifdef OGCM_NOHKIM

    varname_tmp(1) = varname(11)
    ndims_tmp(1) = ndims(11)

    call mkgrdctl ( &
         &   file_ctl_eb, file_out_eb,   title,            &
         &   imut, alont, fmt_axis_h, linear_x,            &
         &   jmut, alatt, fmt_axis_h, linear_y,            &
         &     km,    dp, fmt_axis_v, linear_z,            &
         &      time_dim,        1,        1,              &
         &   varname_tmp,  ndims_tmp,                      &
         &   cundef, opt_hint_ts, opt_general              &
         &   )

#endif /* OGCM_NOHKIM */

#endif /* OGCM_MELYAM || OGCM_NOHKIM */
#endif /* OGCM_VVDIMP */

  end if

end program mkgrads_restart_ocean
