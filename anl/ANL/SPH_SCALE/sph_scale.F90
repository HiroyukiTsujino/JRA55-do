! -*-F90-*-
!
! make scale factor file for spherical coordiate
!
!        2012.03.02 -    M. Hirabara
!
!=====================================================================
!
program sph_scale
  !
  use oc_mod_param, only : &
  & imut, jmut,            &
  & radius, radian_r
  !
  use oc_structure, only : &
  & set_hgrids,            &
  & dxtdeg, dytdeg,        &
  & alatt, alatu
  !
  implicit none
  !
  real(8)  :: a_bl(imut, jmut)
  real(8)  :: a_br(imut, jmut)
  real(8)  :: a_tl(imut, jmut)
  real(8)  :: a_tr(imut, jmut)
  !
  real(8)  :: dx_bl(imut, jmut)
  real(8)  :: dx_br(imut, jmut)
  real(8)  :: dx_tl(imut, jmut)
  real(8)  :: dx_tr(imut, jmut)
  !
  real(8)  :: dy_bl(imut, jmut)
  real(8)  :: dy_br(imut, jmut)
  real(8)  :: dy_tl(imut, jmut)
  real(8)  :: dy_tr(imut, jmut)
  !
  integer(4) :: i, j
  integer(4) :: ios
  integer(4), parameter :: mttmp = 80
  !
  real(8)    :: hl1
  !
  character(len=256) :: flsclf
#ifdef OGCM_VARIABLE
  character(len=256) :: file_vgrid
  namelist /nml_sph_scale/ flsclf, file_vgrid
#else /* OGCM_VARIABLE */
  namelist /nml_sph_scale/ flsclf
#endif /* OGCM_VARIABLE */
  !
  !==================================================
  !
  read(unit=5, nml=nml_sph_scale)
  print *,'flsclf   :', trim(flsclf)
#ifdef OGCM_VARIABLE
  print *,'file_vgrid :', trim(file_vgrid)
#endif /* OGCM_VARIABLE */
  !
#ifdef OGCM_VARIABLE
  call set_hgrids(file_vgrid)
#else /* OGCM_VARIABLE */
  call set_hgrids
#endif /* OGCM_VARIABLE */
  !
  a_bl(1:imut, 1:jmut) = 0.d0
  a_br(1:imut, 1:jmut) = 0.d0
  a_tl(1:imut, 1:jmut) = 0.d0
  a_tr(1:imut, 1:jmut) = 0.d0
  !
  dx_bl(1:imut, 1:jmut) = 0.d0
  dx_br(1:imut, 1:jmut) = 0.d0
  dx_tl(1:imut, 1:jmut) = 0.d0
  dx_tr(1:imut, 1:jmut) = 0.d0
  !
  dy_bl(1:imut, 1:jmut) = 0.d0
  dy_br(1:imut, 1:jmut) = 0.d0
  dy_tl(1:imut, 1:jmut) = 0.d0
  dy_tr(1:imut, 1:jmut) = 0.d0
  !
  !--------------------------------------------------
  do j = 1, jmut-1
    do i = 1, imut-1
      hl1  = 0.5d0 * radius * dxtdeg(i) * radian_r * cos(alatt(j)*radian_r)
      dx_bl(i,j) = hl1
      dy_bl(i,j) = 0.5d0 * radius * dytdeg(j) * radian_r
      a_bl(i,j)  = hl1 * radius * sin(0.5d0*dytdeg(j)*radian_r) *            &
        &   (1.d0 - tan(alatt(j)*radian_r) * tan(0.25d0*dytdeg(j)*radian_r))
      !
      hl1  = 0.5d0 * radius * dxtdeg(i+1) * radian_r * cos(alatt(j)*radian_r)
      dx_br(i,j) = hl1
      dy_br(i,j) = 0.5d0 * radius * dytdeg(j) * radian_r
      a_br(i,j)  = hl1 * radius * sin(0.5d0*dytdeg(j)*radian_r) *            &
        &   (1.d0 - tan(alatt(j)*radian_r) * tan(0.25d0*dytdeg(j)*radian_r))
      !
      dx_tl(i,j) = 0.5d0 * radius * dxtdeg(i) * radian_r * cos(alatu(j  )*radian_r)
      hl1        = 0.5d0 * radius * dxtdeg(i) * radian_r * cos(alatt(j+1)*radian_r)
      dy_tl(i,j) = 0.5d0 * radius * dytdeg(j+1) * radian_r
      a_tl(i,j)  = hl1 * radius * sin(0.5d0*dytdeg(j+1)*radian_r) *            &
        &   (1.d0 + tan(alatt(j+1)*radian_r) * tan(0.25d0*dytdeg(j+1)*radian_r))
      !
      dx_tr(i,j) = 0.5d0 * radius * dxtdeg(i+1) * radian_r * cos(alatu(j  )*radian_r)
      hl1        = 0.5d0 * radius * dxtdeg(i+1) * radian_r * cos(alatt(j+1)*radian_r)
      dy_tr(i,j) = 0.5d0 * radius * dytdeg(j+1) * radian_r
      a_tr(i,j)  = hl1 * radius * sin(0.5d0*dytdeg(j+1)*radian_r) *            &
        &   (1.d0 + tan(alatt(j+1)*radian_r) * tan(0.25d0*dytdeg(j+1)*radian_r))
    end do
#ifdef OGCM_CYCLIC
    dx_bl(imut,j) = dx_bl(4,j)
    dx_br(imut,j) = dx_br(4,j)
    dx_tl(imut,j) = dx_tl(4,j)
    dx_tr(imut,j) = dx_tr(4,j)
    dy_bl(imut,j) = dy_bl(4,j)
    dy_br(imut,j) = dy_br(4,j)
    dy_tl(imut,j) = dy_tl(4,j)
    dy_tr(imut,j) = dy_tr(4,j)
    a_bl(imut,j)  = a_bl(4,j)
    a_br(imut,j)  = a_br(4,j)
    a_tl(imut,j)  = a_tl(4,j)
    a_tr(imut,j)  = a_tr(4,j)
#endif /* OGCM_CYCLIC */
  end do
  write(*,*) 'max dx_bl: ', maxval(dx_bl(:,:))
  write(*,*) 'min dx_bl: ', minval(dx_bl(:,:))
  write(*,*) 'max dx_br: ', maxval(dx_br(:,:))
  write(*,*) 'min dx_br: ', minval(dx_br(:,:))
  write(*,*) 'max dx_tl: ', maxval(dx_tl(:,:))
  write(*,*) 'min dx_tl: ', minval(dx_tl(:,:))
  write(*,*) 'max dx_tr: ', maxval(dx_tr(:,:))
  write(*,*) 'min dx_tr: ', minval(dx_tr(:,:))

  write(*,*) 'max dy_bl: ', maxval(dy_bl(:,:))
  write(*,*) 'min dy_bl: ', minval(dy_bl(:,:))
  write(*,*) 'max dy_br: ', maxval(dy_br(:,:))
  write(*,*) 'min dy_br: ', minval(dy_br(:,:))
  write(*,*) 'max dy_tl: ', maxval(dy_tl(:,:))
  write(*,*) 'min dy_tl: ', minval(dy_tl(:,:))
  write(*,*) 'max dy_tr: ', maxval(dy_tr(:,:))
  write(*,*) 'min dy_tr: ', minval(dy_tr(:,:))

  write(*,*) 'max a_bl: ', maxval(a_bl(:,:))
  write(*,*) 'min a_bl: ', minval(a_bl(:,:))
  write(*,*) 'max a_br: ', maxval(a_br(:,:))
  write(*,*) 'min a_br: ', minval(a_br(:,:))
  write(*,*) 'max a_tl: ', maxval(a_tl(:,:))
  write(*,*) 'min a_tl: ', minval(a_tl(:,:))
  write(*,*) 'max a_tr: ', maxval(a_tr(:,:))
  write(*,*) 'min a_tr: ', minval(a_tr(:,:))
  !
  !--------------------------------------------------
  !
  open(mttmp, file=flsclf, form="unformatted", access="sequential")
  write(mttmp, iostat=ios)   a_bl
  write(mttmp, iostat=ios)   a_br
  write(mttmp, iostat=ios)   a_tl
  write(mttmp, iostat=ios)   a_tr
  write(mttmp, iostat=ios)   dx_bl
  write(mttmp, iostat=ios)   dx_br
  write(mttmp, iostat=ios)   dx_tl
  write(mttmp, iostat=ios)   dx_tr
  write(mttmp, iostat=ios)   dy_bl
  write(mttmp, iostat=ios)   dy_br
  write(mttmp, iostat=ios)   dy_tl
  write(mttmp, iostat=ios)   dy_tr
  if(ios /= 0) write(*, *) 'writing error in file:', trim(flsclf)
  close(mttmp)
  !
!=====================================================================
end program sph_scale
