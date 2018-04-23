!-*-F90-*-
program calc_wind_ceof

  use libmxe_para, only: libmxe_para__register, clen, rho &
                     & , pi, radius, radian, radian_r, type_libmxe_para
  use libmxe_grid, only: libmxe_grid__register  &
                     & , type_libmxe_grid
  use libmxe_topo, only: libmxe_topo__register, libmxe_topo__aexl &
                     & , type_libmxe_topo
  use libmxe_io, only: libmxe_io__register, type_libmxe_io
  use libmxe_grads, only: libmxe_grads__make, type_grads

  use file_open_close_manager

  implicit none

  integer(4) :: imut, jmut, km
  integer(4) :: lreclen_in
  integer(4) :: lreclen_out

  integer(4) :: ibu, ieu, jbu, jeu

  integer(4) :: i1, i2, j1, j2
  integer(4) :: ibyr, ieyr
  integer(4) :: ibmn, iemn
  integer(4) :: iy1, iy2
  integer(4) :: idn, mst, med

  integer(4),allocatable :: n_valid(:,:)

  complex(8),allocatable :: model_wind(:,:,:), obs_wind(:,:,:)

  real(8),allocatable :: eigen_mu(:,:), eigen_ou(:,:)
  real(8),allocatable :: eigen_mv(:,:), eigen_ov(:,:)
  real(8),allocatable :: corr_mag(:,:)
  real(8),allocatable :: corr_rot(:,:)
  real(8),allocatable :: angle_model(:,:)
  real(8),allocatable :: angle_obs(:,:)

  real(8),allocatable :: aexl(:,:)
  real(4),allocatable :: worku(:,:), workv(:,:)

  integer(4) :: irecu_obs
  integer(4) :: irecv_obs

  real(4) :: undef_model, undef_obs, undef_out

  integer(4) :: m, n, i, j, k, iy, im

  integer(4),parameter :: nmonyr = 12
  integer(4) :: ndy(nmonyr) = (/31,29,31,30,31,30,31,31,30,31,30,31/)
  
  real(8) :: total_area, sumh, ave_tarea
  real(8) :: slon, elon, slat, elat

  integer(4) :: mtin1 = 71, mtin2 = 72, mtin3 = 73, mtin4 = 74
  integer(4) :: mtot1 = 81, mtot2 = 82, mtot3 = 83, mtot4 = 84
  character(256) :: flnin_base1, flnin_base2, flnin_base3, flnin_base4
  character(256) :: flnot_base1, flnot_base2, flnot_base3, flnot_base4
  character(256) :: flnin1, flnin2, flnin3, flnin4
  character(256) :: flnot1, flnot2, flnot3, flnot4

  type(type_libmxe_para) :: para
  type(type_libmxe_grid) :: grid
  type(type_libmxe_topo) :: topo
  type(type_libmxe_io) :: io
  type(type_grads) :: grads

  ! for lapack

  complex(8) :: a(2,2), b(2,2), c(2,2) ! covariance matrix
  complex(8) :: check_orth

  integer(4) :: itype, ndim, lda, ldb, lwork, info
  character(1) :: jobz, uplo
  real(8) :: w(2)
  complex(8),allocatable :: work(:)
  real(8),allocatable :: rwork(:)
  logical :: l_check

  !----------------------------------------------

  namelist /nml_wind_ceof/ &
       & flnin_base1, &
       & flnin_base2, &
       & flnin_base3, &
       & irecu_obs, &
       & flnin_base4, &
       & irecv_obs, &
       & undef_model, undef_obs, &
       & flnot1, flnot2, flnot3, &
       & undef_out, &
       & slon, elon, slat, elat, &
       & ibyr, ieyr, ibmn, iemn, l_check

  !-----------------------------------------------------------------------

  l_check = .false.
  open (11,file='namelist.wind_ceof')
  read (11,nml=nml_wind_ceof)
  close(11)

  !-----------------------------------------------------------------------
  ! set grid points

  !-- model settings --

  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)
  call libmxe_topo__aexl(topo,para)

  imut = para%imut
  jmut = para%jmut

  ibu = 1
  ieu = imut
  jbu = 1
  jeu = jmut

  allocate(worku(1:imut,1:jmut), workv(1:imut,1:jmut))
  allocate(aexl(1:imut,1:jmut))

  aexl(:,:) = 1.0d0

  !--------------------------------------------------------
  ! set data size

  iy1 = ibyr
  iy2 = ieyr

  idn = 0
  do n = ibyr, ieyr
    mst = 1
    med = 12
    if (n == ibyr) mst = ibmn
    if (n == ieyr) med = iemn
    do m = mst, med
      idn = idn + 1
    end do
  end do

  write(6,*) ' idn = ', idn

  allocate(n_valid(imut,jmut))

  allocate(model_wind(imut,jmut,idn), obs_wind(imut,jmut,idn))
  allocate(eigen_mu(imut,jmut), eigen_mv(imut,jmut))
  allocate(eigen_ou(imut,jmut), eigen_ov(imut,jmut))
  allocate(corr_mag(imut,jmut), corr_rot(imut,jmut))
  allocate(angle_model(imut,jmut), angle_obs(imut,jmut))

  write(6,*) ' Data allocation O.K. '

  !--------------------------------------------------------

  lreclen_in = 4*imut*jmut

  n = 0

  do iy = ibyr, ieyr

    mst = 1
    med = 12

    if (iy == ibyr) mst = ibmn
    if (iy == ieyr) med = iemn

    do im = mst, med

      n = n + 1

      ! reanalysis

      write(flnin1,'(1a,i4.4,i2.2)') trim(flnin_base1),iy,im
      write(6,*) 'opening ',trim(flnin1)
      call open_file_direct(mtin1,flnin1,lreclen_in)
      read(mtin1,rec=1) worku
      call close_file(mtin1)

      write(flnin2,'(1a,i4.4,i2.2)') trim(flnin_base2),iy,im
      write(6,*) 'opening ',trim(flnin2)
      call open_file_direct(mtin2,flnin2,lreclen_in)
      read(mtin2,rec=1) workv
      call close_file(mtin2)

      do j = 1, jmut
        do i = 1, imut
          if ((worku(i,j) /= undef_model) .and. (workv(i,j) /= undef_model)) then
            model_wind(i,j,n) = &
                 & cmplx(real(worku(i,j),8),real(workv(i,j),8))
          else
            model_wind(i,j,n) = cmplx(0.0d0,0.0d0)
          end if
        end do
      end do

      ! observation

      write(flnin3,'(1a,i4.4,i2.2)') trim(flnin_base3),iy,im
      write(6,*) 'opening ',trim(flnin3)
      call open_file_direct(mtin3,flnin3,lreclen_in)
      read(mtin3,rec=irecu_obs) worku
      call close_file(mtin3)

      write(flnin4,'(1a,i4.4,i2.2)') trim(flnin_base4),iy,im
      write(6,*) 'opening ',trim(flnin4)
      call open_file_direct(mtin4,flnin4,lreclen_in)
      read(mtin4,rec=irecv_obs) workv
      call close_file(mtin4)

      do j = 1, jmut
        do i = 1, imut
          if ((worku(i,j) /= undef_obs) .and. (workv(i,j) /= undef_obs)) then
            obs_wind(i,j,n) = cmplx(real(worku(i,j),8),real(workv(i,j),8))
          else
            obs_wind(i,j,n) = cmplx(0.0d0,0.0d0)
          end if
        end do
      end do

    end do
  end do

  !-----------------------------------------------------

  write(6,*) ' calculation of co-variance matrix '

  a(1:2,1:2) = cmplx(0.d0, 0.d0)
  b(1:2,1:2) = cmplx(0.d0, 0.d0)
  b(1,1) = cmplx(1.d0, 0.d0)
  b(2,2) = cmplx(1.d0, 0.d0)

  itype = 1
  ndim = 2
  lda = 2
  ldb = 2
  lwork = 2*ndim - 1
  jobz='V'
  uplo='U'
  allocate(work(1:lwork))
  allocate(rwork(1:3*ndim-2))

  n_valid(:,:) = 0

  do j = 1, jmut
    do i = 1, imut
!  do j = 75, 75
!    do i = 286, 286

      do n = 1, idn
        !write(6,*) model_wind(i,j,n)
        !write(6,*) obs_wind(i,j,n)
        if ((abs(model_wind(i,j,n)) > 0.0d0) .and. (abs(obs_wind(i,j,n)) > 0.0d0)) then
          n_valid(i,j) = n_valid(i,j) + 1
          a(1,1) = a(1,1) + conjg(model_wind(i,j,n)) * model_wind(i,j,n)
          a(1,2) = a(1,2) + conjg(obs_wind(i,j,n)) * model_wind(i,j,n)
          a(2,1) = a(2,1) + conjg(model_wind(i,j,n)) * obs_wind(i,j,n)
          a(2,2) = a(2,2) + conjg(obs_wind(i,j,n)) * obs_wind(i,j,n)
        end if
      end do

      if (n_valid(i,j) > 0) then
        a(1,1) = a(1,1) / real(n_valid(i,j),8)
        a(2,1) = a(2,1) / real(n_valid(i,j),8)
        a(1,2) = a(1,2) / real(n_valid(i,j),8)
        a(2,2) = a(2,2) / real(n_valid(i,j),8)

        if (l_check) then
          write(6,*) ' a(1,1) = ', a(1,1)
          write(6,*) ' a(2,1) = ', a(2,1)
          write(6,*) ' a(1,2) = ', a(1,2)
          write(6,*) ' a(2,2) = ', a(2,2)
        end if

        c(1:2,1:2) = a(1:2,1:2)

        call zhegv(itype,jobz,uplo,ndim,a,lda,b,ldb,w,work,lwork,rwork,info)

        if (l_check) then
          write(6,*) info
          write(6,*) ' MODE 1'
          write(6,*) ' w(1) = ', w(1)
          write(6,*) ' a(1,1) = ', a(1,1)
          write(6,*) ' a(2,1) = ', a(2,1)
          write(6,*) ' MODE 2'
          write(6,*) ' w(2) = ', w(2)
          write(6,*) ' a(1,2) = ', a(1,2)
          write(6,*) ' a(2,2) = ', a(2,2)
     
          write(6,*) ' orthogonality check '
          check_orth = conjg(a(1,1)) * a(1,2) + conjg(a(2,1)) * a(2,2)
          write(6,*) check_orth

          !---

          write(6,*) ' MODE 1'
          check_orth = c(1,1) * a(1,1) + c(1,2) * a(2,1)
          write(6,*) check_orth / w(1)

          check_orth = c(2,1) * a(1,1) + c(2,2) * a(2,1)
          write(6,*) check_orth / w(1)

          !---

          write(6,*) ' MODE 2'
          check_orth = c(1,1) * a(1,2) + c(1,2) * a(2,2)
          write(6,*) check_orth / w(2)

          check_orth = c(2,1) * a(1,2) + c(2,2) * a(2,2)
          write(6,*) check_orth / w(2)

        end if

        eigen_mu(i,j) = real (a(1,2))
        eigen_mv(i,j) = aimag(a(1,2))
        eigen_ou(i,j) = real (a(2,2))
        eigen_ov(i,j) = aimag(a(2,2))

        corr_mag(i,j) = abs(a(2,2)) / abs(a(1,2))

        if (eigen_ou(i,j) /= 0.0d0) then
          angle_obs(i,j) = 180.d0 / pi * atan(eigen_ov(i,j)/eigen_ou(i,j)) ! [-90, 90]
          if ((eigen_ou(i,j) < 0.0d0) .and. (eigen_ov(i,j) > 0.0d0)) then
            angle_obs(i,j) = angle_obs(i,j) + 180.0d0
          end if
          if ((eigen_ou(i,j) < 0.0d0) .and. (eigen_ov(i,j) < 0.0d0)) then
            angle_obs(i,j) = angle_obs(i,j) - 180.0d0
          end if
        else
          if (eigen_ov(i,j) > 0.0d0) then
            angle_obs(i,j) = 90.0d0
          else
            angle_obs(i,j) = -90.0d0
          end if
        end if

        if (eigen_mu(i,j) /= 0.0d0) then
          angle_model(i,j) = 180.d0 / pi * atan(eigen_mv(i,j)/eigen_mu(i,j))
          if ((eigen_mu(i,j) < 0.0d0) .and. (eigen_mv(i,j) > 0.0d0)) then
            angle_model(i,j) = angle_model(i,j) + 180.0d0
          end if
          if ((eigen_mu(i,j) < 0.0d0) .and. (eigen_mv(i,j) < 0.0d0)) then
            angle_model(i,j) = angle_model(i,j) - 180.0d0
          end if
        else
          if (eigen_mv(i,j) > 0.0d0) then
            angle_model(i,j) = 90.0d0
          else
            angle_model(i,j) = -90.0d0
          end if
        end if

        corr_rot(i,j) = angle_obs(i,j) - angle_model(i,j)
        if (corr_rot(i,j) > 180.0d0) then
          corr_rot(i,j) = corr_rot(i,j) - 360.0d0
        end if
        if (corr_rot(i,j) < -180.0d0) then
          corr_rot(i,j) = 360.0d0 + corr_rot(i,j)
        end if

        if (l_check) then
          write(6,'(1a,2f8.2,2f10.3)') ' location mag ang', grid%glonu(i,j), grid%glatu(i,j), corr_mag(i,j), corr_rot(i,j)
        end if
      else
        corr_mag(i,j) = undef_out
        corr_rot(i,j) = undef_out
      end if

    end do
  end do

  if (l_check) stop

  lreclen_out = 4*imut*jmut

  write(6,*) ' Correction factor of speed = ',trim(flnot1)
  open (mtot1,file=flnot1,access='direct',form='unformatted',recl=lreclen_out)
  write(mtot1,rec=1) real(corr_mag(1:imut,1:jmut),4)
  close(mtot1)

  write(6,*) ' Correction factor of direction = ',trim(flnot2)
  open (mtot2,file=flnot2,access='direct',form='unformatted',recl=lreclen_out)
  write(mtot2,rec=1) real(corr_rot(1:imut,1:jmut),4)
  close(mtot2)

end program calc_wind_ceof
