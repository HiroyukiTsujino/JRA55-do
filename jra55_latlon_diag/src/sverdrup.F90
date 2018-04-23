! -*-F90-*-
!===================================================================
!               PROGRAM SVERDRUP_TRANSPORT
!-------------------------------------------------------------------
! Information:
!     Calculate Sverdrup transport from wind stress
!-------------------------------------------------------------------
!
program sverdrup_transport


  use libmxe_para, only: libmxe_para__register, clen, rho, omega &
                     & , pi, radius, radian, radian_r, type_libmxe_para
  use libmxe_grid, only: libmxe_grid__register  &
                     & , type_libmxe_grid
  use libmxe_topo, only: libmxe_topo__register, libmxe_topo__aexl &
                     & , type_libmxe_topo
  use libmxe_io, only: libmxe_io__register, type_libmxe_io
  use libmxe_grads, only: libmxe_grads__make, type_grads

  use file_open_close_manager

  integer(4) :: imut, jmut, km
  integer(4) :: lreclen_in
  integer(4) :: lreclen_out

  real(8),allocatable :: taux(:,:), tauy(:,:)
  real(8),allocatable :: curlt(:,:), curlt_mask(:,:)
  real(8),allocatable :: psi(:,:), psi_mask(:,:)
  real(8),allocatable :: areat(:,:)
  real(8),allocatable :: beta(:,:)

  real(4),allocatable :: wdat(:,:)

  integer(4) :: mtin1, mtin2
  integer(4) :: mtot1
  character(256) :: file_mask
  character(256) :: file_u_base, file_v_base, file_out_base
  character(256) :: file_u, file_v, file_out

  integer(4) :: i, j, m, n
  integer(4) :: irec1, irec2

  real(8)    :: lon_start
  real(8)    :: lat_start, lat_end
  integer(4) :: integ_xstart

  logical :: l_nohalo_cyclic
  logical :: l_out_latlon

  ! MXE structure variables

  type(type_libmxe_para) :: para
  type(type_libmxe_grid) :: grid
  type(type_libmxe_topo) :: topo
  type(type_libmxe_io) :: io
  type(type_grads) :: grads

  !-----------------------------------------------------------------------

  namelist /nml_sverdrup_function/ &
       & file_mask, &
       & file_u_base, file_v_base, &
       & file_out_base, &
       & undef_in, undef_out, &
       & nbyr, neyr, &
       & lon_start, &
       & lat_start, lat_end, &
       & l_nohalo_cyclic, l_out_latlon

  !------------------------------------------------------------------

  l_nohalo_cyclic = .false.
  l_out_latlon = .false.

  open (11,file='namelist.sverdrup_function')
  read (11,nml=nml_sverdrup_function)
  close(11)

  !------------------------------------------------------------------
  !-- model settings --

  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)
  call libmxe_topo__aexl(topo,para)

  imut = para%imut
  jmut = para%jmut

  lreclen_in = 4*imut*jmut
  lreclen_out = 4*imut*jmut

  allocate(taux(0:imut+1,1:jmut), tauy(0:imut+1,1:jmut))
  allocate(curlt(0:imut+1,1:jmut))
  allocate(curlt_mask(0:imut+1,1:jmut))
  allocate(psi (0:imut+1,1:jmut))
  allocate(psi_mask(0:imut+1,1:jmut))
  allocate(wdat(1:imut,1:jmut))
  allocate(areat(1:imut,1:jmut))
  allocate(beta(1:imut,1:jmut))

  areat(:,:) = 0.0d0

  do j = 2, jmut
    do i = 2, imut
      areat(i,j) = topo%atexl(i,j,1) *                                &
           & ( topo%aexl(i-1,j  ,1) * grid%a_br(i-1,j  ) + topo%aexl(i,j  ,1) * grid%a_bl(i,j  ) &
           &  +topo%aexl(i-1,j-1,1) * grid%a_tr(i-1,j-1) + topo%aexl(i,j-1,1) * grid%a_tl(i,j-1))
    end do
  end do

  if (l_nohalo_cyclic) then
    do j = 2, jmut
      topo%atexl(1,j,1) = max(topo%aexl(imut,j,1), topo%aexl(1,j,1), topo%aexl(imut,j-1,1), topo%aexl(1,j-1,1))
    end do
    do j = 2, jmut
      areat(1,j) = topo%atexl(1,j,1) *                               &
           & ( topo%aexl(imut,j  ,1) * grid%a_br(imut,j  ) + topo%aexl(1,j  ,1) * grid%a_bl(1,j  ) &
           &  +topo%aexl(imut,j-1,1) * grid%a_tr(imut,j-1) + topo%aexl(1,j-1,1) * grid%a_tl(1,j-1))
    end do
  end if

  do j = 1, jmut
    do i = 1, imut
      beta(i,j) = 2.d0 * omega * cos(grid%glatu(i,j)/radian) / radius
    enddo
  enddo

  do i = 1, imut
    if (grid%lont(i) > lon_start) then
      integ_xstart = i - 1
      exit
    end if
  end do
  
  write(6,*) ' integ start = ', integ_xstart, grid%lont(integ_xstart)

  write(10,*) imut
  write(10,*) (grid%lont(i),i=1,imut)
  write(10,*) jmut
  write(10,*) (grid%latt(j),j=1,jmut)

  !-------------------------------------------------------------------

  call open_file_direct(mtin1,file_mask,lreclen_in)
  read(mtin1,rec=irec1) wdat
  call close_file(mtin1)

  curlt_mask(:,:) = 0.0d0
  psi_mask  (:,:) = 0.0d0

  do j = 2, jmut
    do i = 2, imut
      if (wdat(i-1,j-1)>0.0 .and. wdat(i-1,j)>0.0 &
           .and. wdat(i,j-1)>0.0 .and. wdat(i,j)>0.0) then
        psi_mask  (i,j) = 1.0d0
        curlt_mask(i,j) = 1.0d0
      end if
      if (wdat(i-1,j-1) == 9.0 .or. wdat(i-1,j) == 9.0 &
           .or. wdat(i,j-1) == 9.0 .or. wdat(i,j) == 9.0) then
        curlt_mask(i,j) = 0.0d0
      end if
    end do
  end do

  if (l_nohalo_cyclic) then
    do j = 2, jmut
      if (wdat(1,j-1)>0.0 .and. wdat(imut,j)>0.0 &
           .and. wdat(1,j)>0.0 .and. wdat(imut,j)>0.0) then
        psi_mask  (1,j) = 1.0d0
        curlt_mask(1,j) = 1.0d0
      end if
      if (wdat(imut,j-1) == 9.0 .or. wdat(imut,j) == 9.0 &
           .or. wdat(1,j-1) == 9.0 .or. wdat(1,j) == 9.0) then
        curlt_mask(1,j) = 0.0d0
      end if
    end do
  end if


  curlt(:,:) = curlt_mask(:,:)

  if (l_nohalo_cyclic) then
     curlt(0,:) = curlt(imut,:)
     curlt(imut+1,:) = curlt(1,:)
   end if

  do j = 2, jmut - 1
    do i = 1, imut
      if (curlt(i-1,j) == 0.0d0 .or. curlt(i+1,j) == 0.0d0 &
           .or. curlt(i,j-1) == 0.0d0 .or. curlt(i,j+1) == 0.0d0) then
        curlt_mask(i,j) = 0.0d0
      end if
    end do
  end do

  if (l_nohalo_cyclic) then
    curlt_mask(0,:) = curlt_mask(imut,:)
    psi_mask  (0,:) = psi_mask  (imut,:)
    curlt_mask(imut+1,:) = curlt_mask(1,:)
    psi_mask  (imut+1,:) = psi_mask  (1,:)
  end if

  !-------------------------------------------------------------------------

  do n = nbyr, neyr

    do m = 1, 12

      write(file_u,'(1a,i4.4,i2.2)') trim(file_u_base),n,m
      call open_file_direct(mtin1,file_u,lreclen_in)
      read(mtin1,rec=1) wdat
      do j = 1, jmut
        do i = 1, imut
          taux(i,j) = topo%aexl(i,j,1) * real(wdat(i,j),8) * 1.0d1 ! [cgs]
        end do
      end do
      call close_file(mtin1)

      write(file_v,'(1a,i4.4,i2.2)') trim(file_v_base),n,m
      call open_file_direct(mtin2,file_v,lreclen_in)
      read(mtin2,rec=irec1) wdat
      do j = 1, jmut
        do i = 1, imut
          tauy(i,j) = topo%aexl(i,j,1) * real(wdat(i,j),8) * 1.0d1 ! [cgs]
        end do
      end do
      call close_file(mtin2)

      curlt(:,:) = 0.0d0

      call cal_curl_tau

      if (l_nohalo_cyclic) then
        curlt(0,:)      = curlt(imut,:)
        curlt(imut+1,:) = curlt(1,:)
      end if

      psi(:,:) = 0.0d0

      do j = 2, jmut

        if ((lat_start < grid%latt(j)) .and. (grid%latt(j) < lat_end)) then
          if (l_nohalo_cyclic) then
            do i = integ_xstart - 1, 1, -1
              psi(i,j) = psi(i+1,j) &
                   & - (1.0d0 - (1.0d0 - topo%aexl(i,j,1)) * (1.0d0 - topo%aexl(i,j-1,1))) &
                   & * 0.5d0 * (curlt(i,j) + curlt(i+1,j)) &
                   & / rho / beta(i,j) * (grid%dx_br(i,j) + grid%dx_bl(i,j))
              psi(i,j) = psi_mask(i,j) * psi(i,j)
            end do
            psi(imut+1,j) = psi(1,j)
            do i = imut, integ_xstart, -1
              psi(i,j) = psi(i+1,j) &
                   & - (1.0d0 - (1.0d0 - topo%aexl(i,j,1)) * (1.0d0 - topo%aexl(i,j-1,1))) &
                   & * 0.5d0 * (curlt(i,j) + curlt(i+1,j)) &
                   & / rho / beta(i,j) * (grid%dx_br(i,j) + grid%dx_bl(i,j))
              psi(i,j) = psi_mask(i,j) * psi(i,j)
            end do
          else
            do i = imut-1, 1, -1
              psi(i,j) = psi(i+1,j) &
                   & - (1.0d0 - (1.0d0 - topo%aexl(i,j,1)) * (1.0d0 - topo%aexl(i,j-1,1))) &
                   & * 0.5d0 * (curlt(i,j) + curlt(i+1,j)) &
                   & / rho / beta(i,j) * (grid%dx_br(i,j) + grid%dx_bl(i,j))
              psi(i,j) = psi_mask(i,j) * psi(i,j)
            end do
          end if
        end if

      end do

      write(file_out,'(1a,i4.4,i2.2)') trim(file_out_base),n,m
      call open_file_direct(mtot1,file_out,lreclen_out)
      write(mtot1,rec=1) real(psi(1:imut,1:jmut),4)
      write(mtot1,rec=2) real(curlt(1:imut,1:jmut),4)
      call close_file(mtot1)

    enddo
  enddo

contains

!====================================================
subroutine cal_curl_tau
  !
  integer(4) :: i, j
  !
  do j = 2, jmut
    do i = 2, imut
      curlt(i, j) = curlt_mask(i,j) *                                             &
        & (-taux(i-1, j  )*grid%dx_tr(i-1, j  ) -taux(i, j  )*grid%dx_tl(i, j  )  &
        &  -tauy(i-1, j  )*grid%dy_br(i-1, j  ) +tauy(i, j  )*grid%dy_br(i, j  )  &
        &  -tauy(i-1, j-1)*grid%dy_tr(i-1, j-1) +tauy(i, j-1)*grid%dy_tr(i, j-1)  &
        &  +taux(i-1, j-1)*grid%dx_tr(i-1, j-1) +taux(i, j-1)*grid%dx_tl(i, j-1)) &
        & / (areat(i,j) + 1.0d0 - topo%atexl(i,j,1))
    end do
  end do

  if (l_nohalo_cyclic) then
    do j = 2, jmut
      curlt(1,j) = curlt_mask(1,j) *                                                &
        & (-taux(imut, j  )*grid%dx_tr(imut, j  ) -taux(1, j  )*grid%dx_tl(1, j  )  &
        &  -tauy(imut, j  )*grid%dy_br(imut, j  ) +tauy(1, j  )*grid%dy_br(1, j  )  &
        &  -tauy(imut, j-1)*grid%dy_tr(imut, j-1) +tauy(1, j-1)*grid%dy_tr(1, j-1)  &
        &  +taux(imut, j-1)*grid%dx_tr(imut, j-1) +taux(1, j-1)*grid%dx_tl(1, j-1)) &
        & / (areat(1,j) +1.0d0 - topo%atexl(1,j,1))
    end do
  end if
  !
end subroutine cal_curl_tau
!====================================================

end program sverdrup_transport
