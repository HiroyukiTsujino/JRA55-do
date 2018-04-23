! -*-F90-*-
!
!======================================================================
! Information:
!     calculate vertically averaged tracer
!----------------------------------------------------------------------
program vertically_averaged_tracer

  use basin_param
  use grid_common

  implicit none

#ifdef OGCM_CYCLIC
  integer(4), parameter :: ibu = 3, ieu = imut - 2
  integer(4), parameter :: ibt = 3, iet = imut - 2
#else /* OGCM_CYCLIC */
#ifdef OGCM_SUB
  integer(4), parameter :: ibu = 3, ieu = imut - 2
  integer(4), parameter :: ibt = 3, iet = imut - 1
#else /* OGCM_SUB */
  integer(4), parameter :: ibu = 2, ieu = imut - 1
  integer(4), parameter :: ibt = 2, iet = imut
#endif /* OGCM_SUB */
#endif /* OGCM_CYCLIC */

#ifdef OGCM_TRIPOLAR
  integer(4), parameter :: jbu = 2, jeu = jmut - 3
  integer(4), parameter :: jbt = 2, jet = jmut - 2
#else /* OGCM_TRIPOLAR */
#ifdef OGCM_SUB
  integer(4), parameter :: jbu = 3, jeu = jmut - 2
  integer(4), parameter :: jbt = 3, jet = jmut - 1
#else /* OGCM_SUB */
  integer(4), parameter :: jbu = 2, jeu = jmut - 1
  integer(4), parameter :: jbt = 2, jet = jmut
#endif /* OGCM_SUB */
#endif /* OGCM_TRIPOLAR */

  real(4) :: work4(imut,jmut)

  real(8) :: t(imut,jmut,km)
  real(8) :: ht(imut,jmut)

  real(8) :: d_ref
  real(8) :: volt_tmp, volt_total
  real(8) :: vat(imut,jmut)
  real(8), parameter :: bad = -9.0d30

  !-----

  integer(4), parameter :: mtin1 = 71, mtin2 = 72, mtin3 = 73
  integer(4), parameter :: mtot1 = 81, mtot2 = 82
  character(256) :: flnin1, flnin1_base
  character(256) :: flnin2, flnin2_base
  character(256) :: flnin3, flnin3_base
  character(256) :: flout, flout_base
  character(256) :: ftopo, fgrid, fscale

  integer(4) :: nday, month, nyear, nd
  integer(4) :: ndyear

  integer(4), parameter :: ndata = 73
  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  integer(4) :: irec1, irec2, irec3
  integer(4) :: i, j, k, m, ii, jj
  integer(4) :: nbyr, neyr
  integer(4) :: k_ref

  real(8), parameter :: teps = 1.0d-6

  logical :: l_stop
  !-----------------------------------------------------------------------

  namelist /ioinfvta/ flnin1_base, flnin3_base, flout_base, &
       & ftopo, fgrid, fscale, &
       & nbyr, neyr, d_ref
  open (11,file='ioinfvta.dat')
  read (11,nml=ioinfvta)
  close(11)

  !-----------------------------------------------------------------------
  ! set grid points

  call setgrd(ftopo, fgrid, fscale)

  do k = 1, km
    if (dep(k+1) >= d_ref) then
      k_ref = k
      exit
    end if
  end do

  write(6,*) 'vertically integrated to ', dep(k_ref+1)

  !------------------------------------------------------------------
  ! read tracer

  do nyear = nbyr, neyr

    write(6,*) 'Year = ', nyear

    write(flnin1,'(1a,i4.4)') trim(flnin1_base),nyear
    open(mtin1, file=flnin1, access='direct', recl=4*imut*jmut)
    write(*,'(1a,1a)') 'data read from ... ', trim(flnin1)

    do k = 1, km
      read(mtin1,rec=k) ((work4(i,j),i=1,imut),j=1,jmut)
      do j = 1, jmut
        do i = 1, imut
!          t(i,j,k) = atexl(i,j,k) * real(work4(i,j),8)
          t(i,j,k) = real(work4(i,j),8)
        end do
      end do
    end do
    close(mtin1)

    write(flnin3,'(1a,i4.4)') trim(flnin3_base),nyear
    open(mtin3, file=flnin3, access='direct', recl=4*imut*jmut)
    write(*,'(1a,1a)') 'data read from ... ', trim(flnin3)

    read(mtin3,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
    do j = 1, jmut
      do i = 1, imut
        ht(i,j) = atexl(i,j,1) * real(work4(i,j),8) * 1.0d-2
      end do
    end do
    close(mtin3)

    ! consistency check

    do k = 1, km
      do j = jbt, jet
        do i = ibt, iet
          if ((atexl(i,j,k) /= 0.0d0) .and. (volt(i,j,k) == 0.0d0)) then
            write(6,*) ' inconsistent topography', i,j,k,atexl(i,j,k),volt(i,j,k)
          end if
          if ((atexl(i,j,k) /= 0.0d0) .and. (t(i,j,k) < bad)) then
            write(6,*) ' tracer data missing', i,j,k,t(i,j,km),t(i,j,k)
            l_stop = .true.
          end if
        end do
      end do
    end do

    if (l_stop) stop

    !-----------------

    vat(1:imut,1:jmut) = 0.0d0

    do j = 1, jmut
      do i = 1, imut
        if (atexl(i,j,1) > 0.0d0) then

          volt_total = 0.0d0

          do k = 1, k_ref

            if (k <= ksgm) then
              volt_tmp = atexl(i,j,k) * (dz(k) + ht(i,j) * dsgm(k))  &
                   &   * (aexl(i-1, j  , k)* a_br(i-1, j  ) +aexl(i, j  , k)*a_bl(i, j  ) &
                   &     +aexl(i-1, j-1, k)* a_tr(i-1, j-1) +aexl(i, j-1, k)*a_tl(i, j-1))
            else
              volt_tmp = volt(i,j,k)
            end if

            vat(i,j) = vat(i,j) + volt_tmp * t(i,j,k) * atexl(i,j,k)
            volt_total = volt_total + volt_tmp
          
          end do

          if ((ktbtm(i,j) > 1) .and. (ktbtm(i,j) <= k_ref)) then
            if (atexl(i,j,ktbtm(i,j)) == 0.0d0) then ! This is BBL
              !write(6,*) 'BBL is included ',i,j,ktbtm(i,j),dp(ktbtm(i,j))
              volt_tmp = volt(i,j,km) * atexl(i,j,km)
              vat(i,j) = vat(i,j) + volt_tmp * t(i,j,km)
              volt_total = volt_total + volt_tmp
            end if
          end if

          if (ktbtm(i,j) >= k_ref) then
            vat(i,j) = vat(i,j) / volt_total
          else
            !write(6,*) 'set to missing value', undef
            vat(i,j) = undef
          end if

        else
          vat(i,j) = undef
        end if
      end do
    end do

    write(flout,'(1a,i4.4,i2.2,i2.2)') trim(flout_base),nyear
    write(6,*) ' GrADs data written to... ', trim(flout)
    open (mtot1,file=flout,form='unformatted',access='direct',recl=4*imut*jmut)
    write(mtot1,rec=1) real(vat(1:imut,1:jmut),4)
    close(mtot1)
    
  end do

end program vertically_averaged_tracer
