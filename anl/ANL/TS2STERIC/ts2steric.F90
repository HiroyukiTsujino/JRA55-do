! -*-F90-*-
!ts2steric.F90
!====================================================
!
!   Make GrADS data from OGCM restart file
!
!====================================================
program ts2steric

  use oc_mod_param
  use oc_structure
  use oc_mod_density

  implicit none

  integer(4), parameter :: ibu = 3, ieu = imut - 2  ! cyclic
  integer(4), parameter :: jbu = 2, jeu = jmut - 3  ! tripolar

  integer(4), parameter :: ibt = 3, iet = imut - 2  ! cyclic
  integer(4), parameter :: jbt = 2, jet = jmut - 2  ! tripolar

  real(8) :: ratio_nend(jmut)

  real(4), parameter :: UNDEF_grd = -9.99e33
  real(4), parameter :: bad = -9.0e33

  real(4) :: tt01(imut, jmut, km)
  real(4) :: ss01(imut, jmut, km)
  real(4) :: tt12(imut, jmut, km)
  real(4) :: ss12(imut, jmut, km)
  real(4) :: dummy
  real(4) :: tt1(imut, jmut, km) ! reference
  real(4) :: ss1(imut, jmut, km) ! reference
  real(4) :: tts(imut, jmut, km) ! this
  real(4) :: sss(imut, jmut, km) ! this
  real(4) :: hm (imut, jmut)

  real(8) :: tsd(imut, jmut, km, 2) ! temporal
  real(8) :: rho_ref(imut, jmut, km)
  real(8) :: rho_str(imut, jmut, km)
  real(8) :: rho_thm(imut, jmut, km)

  real(8) :: p_ref_c(km), p_ref_t(km+1)

  real(8) :: depth_ref, depth_this
  real(8) :: fac_ref, fac_this
  real(8) :: areag

  real(8) :: rhom_ref
  real(8) :: rhom_thm, rhom_str
  real(8) :: dyn_height
  real(8) :: vol_ref, vol_this
  real(8) :: steric_anom
  real(8) :: thermosteric_anom
  
  integer(4) :: maskt(imut, jmut, km)

  character(len=256) :: flin_tsclim
  character(len=256) :: flin_t
  character(len=256) :: flin_s
  character(len=256) :: flin_ssh
  character(len=256) :: flout0
  character(len=256) :: flout1, flout2, flout3, flout4
  character(len=256) :: flmaskt
  character(len=256) :: flgrid, fltopo, flsclf

  integer(4), parameter :: mtin   = 80
  integer(4), parameter :: mtin_c = 81
  integer(4), parameter :: mtin_t = 82
  integer(4), parameter :: mtin_s = 83
  integer(4), parameter :: mtin_h = 84
  integer(4), parameter :: mtout0 = 85
  integer(4), parameter :: mtout1 = 86
  integer(4), parameter :: mtout2 = 87
  integer(4), parameter :: mtout3 = 88
  integer(4), parameter :: mtout4 = 89
  !
  integer(4) :: irec
  integer(4) :: i, j, k, n
  integer(4) :: ktmp
  !
  !==========================================

  namelist /nml_steric/ flin_t, flin_s, flin_ssh, flin_tsclim, &
       & flout0, flout1, flout2, flout3, flout4, &
       & flmaskt, fltopo, flgrid, flsclf

  !==========================================

  flin_t  = '../result/hs_t.d'
  flin_s  = '../result/hs_s.d'
  flin_ssh= '../result/hs_ssh.d'
  flin_tsclim = '../../data/tsclim.d'
  flout0  = '../logs/heiht.txt'
  flout1  = '../logs/hs_steric.d'
  flout2  = '../logs/hs_thermo_steric.d'
  flout3  = '../logs/hs_surface_height.d'
  flout4  = '../logs/hs_density.d'
  flmaskt = '../../data/maskt.gd'
  fltopo  = '../../data/topo.d'
  flgrid  = '../../data/vgrid.d'
  flsclf  = '../../data/scale_factor.d'

  read(unit=5, nml_steric)

  print *,'flin_t   :', trim(flin_t)
  print *,'flin_s   :', trim(flin_s)
  print *,'flin_ssh :', trim(flin_ssh)
  print *,'flin_tsclim :', trim(flin_tsclim)
  print *,'flout0   :', trim(flout0)
  print *,'flout1   :', trim(flout1)
  print *,'flout2   :', trim(flout2)
  print *,'flout3   :', trim(flout3)
  print *,'flout4   :', trim(flout4)
  print *,'flmask   :', trim(flmaskt)
  print *,'fltopo   :', trim(fltopo)
  print *,'flgrid   :', trim(flgrid)
  print *,'flsclf   :', trim(flsclf)

  !----------------------------------------------
  !  地形の読み込み
  !----------------------------------------------
  call read_topo(fltopo)
  !
  !----------------------------------------------
  !  スケールファクタの読み込み
  !----------------------------------------------
  call read_scale(flsclf)

  !------------------------------------------
  ! set grid points

  call set_area_t
  call set_area_u
  call set_volt

  ! 1.0 bar = 10^5 [Pa]

  p_ref_c(1:km) = (grav * 1.0d-2) * (dp(1:km) * 1.0d-2) * (ro * 1.0d3) * 1.0d-5
  p_ref_t(1:km+1) = (grav * 1.0d-2) * (dep(1:km+1) * 1.0d-2) * (ro *1.0d3) * 1.0d-5

  do k = 1, km
    write(6,'(F8.2,F8.2)') p_ref_t(k), p_ref_c(k)
  enddo
  write(6,'(F8.2)') p_ref_t(km+1)

  areag  = 0.0d0

  do j = jbu, jeu
    do i = ibu, ieu
      areag = areag + areau(i,j,1)
    end do
  end do

  write(*,*) ' Global Ocean Area  = ', areag * 1.0d-4

  ratio_nend(1:jmut) = 1.0d0
  ratio_nend(jet) = 0.5d0

  !------------------------------------------
  ! mask

  open(mtin, file=flmaskt, form='unformatted',  &
    & access='direct', action='read', recl=imut*jmut*km*4)
  read(mtin, rec=1) maskt(1:imut, 1:jmut, 1:km)
  close(mtin)

  write(6,*) ' Check consisitency of topography '

#ifdef OGCM_BBL
  do k = 1, km - 1
#else /* OGCM_BBL */
  do k = 1, km
#endif /* OGCM_BBL */
    do j = 1, jmut
      do i = 1, imut
        if ((maskt(i,j,k) == 0) .and. (atexl(i,j,k) == 1.0d0)) then
          write(6,*) ' inconsistent topography (a) at ', i,j,k
        end if
        if ((maskt(i,j,k) == 1) .and. (atexl(i,j,k) == 0.0d0)) then
          write(6,*) ' inconsistent topography (b) at ', i,j,k
        end if
        if (real(maskt(i,j,k),8) /= atexl(i,j,k)) then
          write(6,*) ' inconsistent topography (c) at ', i,j,k
        end if
      end do
    end do
  end do

  write(6,*) ' done '

#ifdef OGCM_BBL
  do j = 1, jmut
    do i = 1, imut
      if (atexl(i,j,km) == 1.0d0) then
        atexl(i,j,km) = 0.0d0
      end if
      ktmp =  ktbtm(i,j)
      if ((ktmp > 1) .and. (texnnbbl(i,j) > 0)) then
        if (atexl(i,j,ktmp) == 0.0d0) then
          atexl(i,j,ktmp) = 1.0d0
        else
          write(6,*) 'BBL has been already occupied ', i, j, ktmp
        end if
      end if
    end do
  end do

  do j = 2, jmut
    do i = 2, imut
      ktmp =  ktbtm(i,j)
      if ((ktmp > 1) .and. (texnnbbl(i,j) > 0)) then
        if (volt(i,j,ktbtm(i,j)) == 0.0d0) then
          volt(i,j,ktbtm(i,j)) = volt(i,j,km)
          volt(i,j,km) = 0.0d0
        else
          write(6,*) 'inconsistent volt (1)', i,j,ktbtm(i,j)
        end if
      end if
    end do
  end do

  do j = 2, jmut
    do i = 2, imut
      if (kbtm(i,j) /= km) then
        if (volt(i,j,km) /= 0.0d0) then
          write(6,*) 'inconsistent volt (2)', i,j,ktbtm(i,j)
        end if
      end if
    end do
  end do

#endif /* OGCM_BBL */

  !------------------------------------------
  ! reference

  open(mtin_c, file=flin_tsclim, form='unformatted', access='sequential')

  read(mtin_c) tt01, ss01
  do j = 2, 11
    read(mtin_c) tt12, ss12
  end do
  read(mtin_c) tt12, ss12

  close(mtin_c)

  tt1(1:imut, 1:jmut, 1:km)=0.5e0*(tt01(1:imut, 1:jmut, 1:km)+tt12(1:imut, 1:jmut, 1:km))
  ss1(1:imut, 1:jmut, 1:km)=0.5e0*(ss01(1:imut, 1:jmut, 1:km)+ss12(1:imut, 1:jmut, 1:km))

  write(6,*) ' Check consisitency of climatology '
  do k = 1, km
    do j = 1, jmut
      do i = 1, imut
        if ((maskt(i,j,k) == 0) .and. (tt1(i,j,k) > -10.0d0)) then
          write(6,*) ' inconsistent data (a) at ', i,j,k,tt1(i,j,k)
        end if
        if ((maskt(i,j,k) == 1) .and. (tt1(i,j,k) < -10.0d0)) then
          write(6,*) ' inconsistent data (b) at ', i,j,k
        end if
      end do
    end do
  end do
  write(6,*) ' done '

  !------
  ! manipulate data for BBL

#ifdef OGCM_BBL
  do j = jbt, jet
    do i = ibt, iet
      ktmp = ktbtm(i,j)
      if ((texnnbbl(i,j) > 0) .and. (atexl(i,j,ktmp) /= 0.0d0)) then
        if (tt1(i,j,ktmp) < bad) then
          if (tt1(i,j,km) > bad) then
            tt1(i,j,ktmp) = tt1(i,j,km)
          else
            write(6,*) ' temperature data missing', i,j,k,tt1(i,j,km)
          end if
        else
          write(6,*) ' valid data already exists (climT) ',i,j,tt1(i,j,ktmp),maskt(i,j,ktmp)
        end if

        if (ss1(i,j,ktmp) < bad) then
          if (ss1(i,j,km) > bad) then
            ss1(i,j,ktmp) = ss1(i,j,km)
          else
            write(6,*) ' salinity data missing', i,j,k,ss1(i,j,km)
          end if
        else
          write(6,*) ' valid data already exists (climS)',i,j
        end if
      end if
    end do
  end do
#endif /* OGCM_BBL */

  where(atexl(1:imut, 1:jmut, 1:km) == 0.0d0)
    tt1(1:imut, 1:jmut, 1:km) = 0.e0
    ss1(1:imut, 1:jmut, 1:km) = 0.e0
  end where

  tsd(1:imut, 1:jmut, 1:km, 1) = real(tt1(1:imut, 1:jmut, 1:km), 8)
  tsd(1:imut, 1:jmut, 1:km, 2) = real(ss1(1:imut, 1:jmut, 1:km), 8)

  call dens(imut,jmut,km,tsd(1,1,1,1),tsd(1,1,1,2),p_ref_c,rho_ref)

  !-------------------------------------------
  ! this

  open(mtin_t,  file=flin_t,   form='unformatted', access='direct', recl=imut*jmut*km*4)
  open(mtin_s,  file=flin_s,   form='unformatted', access='direct', recl=imut*jmut*km*4)
  open(mtin_h,  file=flin_ssh, form='unformatted', access='direct', recl=imut*jmut*4)

  read(mtin_t,  rec=1) tts(1:imut, 1:jmut, 1:km)  !  Temperature
  read(mtin_s,  rec=1) sss(1:imut, 1:jmut, 1:km)  !  Salinity
  read(mtin_h,  rec=1) hm (1:imut, 1:jmut)        !  Surface Height

  close(mtin_t)
  close(mtin_s)
  close(mtin_h)

  write(6,*) ' Check consisitency of snap shot data'
  do k = 1, km
    do j = 1, jmut
      do i = 1, imut
        if ((maskt(i,j,k) == 0) .and. (tts(i,j,k) > -10.0d0)) then
          if (tts(i,j,km) < -10.0d0) then
            write(6,*) ' inconsistent data (a) at ', i,j,k
          end if
        end if
        if ((maskt(i,j,k) == 1) .and. (tts(i,j,k) < -10.0d0)) then
          write(6,*) ' inconsistent data (b) at ', i,j,k
        end if
      end do
    end do
  end do
  write(6,*) ' done '

#ifdef OGCM_BBL
  do j = jbt, jet
    do i = ibt, iet
      ktmp = ktbtm(i,j)
      if ((texnnbbl(i,j) > 0) .and. (atexl(i,j,ktmp) /= 0.0d0)) then
        if (tts(i,j,ktmp) < bad) then
          if (tts(i,j,km) > bad) then
            tts(i,j,ktmp) = tts(i,j,km)
          else
            write(6,*) ' temperature data missing', i,j,k,tts(i,j,km)
          end if
        else
!          write(6,*) ' valid data already exists (snapT)',i,j
        end if

        if (sss(i,j,ktmp) < bad) then
          if (sss(i,j,km) > bad) then
            sss(i,j,ktmp) = sss(i,j,km)
          else
            write(6,*) ' salinity data missing', i,j,k,sss(i,j,km)
          end if
        else
!          write(6,*) ' valid data already exists (snapS)',i,j
        end if

      end if
    end do
  end do
#endif /* OGCM_BBL */

  where(atexl(1:imut, 1:jmut, 1:km) == 0)
    tts(1:imut, 1:jmut, 1:km) = 0.e0
    sss(1:imut, 1:jmut, 1:km) = 0.e0
  end where
  where(atexl(1:imut, 1:jmut, 1) == 0)
    hm(1:imut, 1:jmut) = 0.e0
  end where

  hm(1:imut,1:jmut) = hm(1:imut,1:jmut) * 1.0e-2 ! cgs -> MKS

  tsd(1:imut, 1:jmut, 1:km, 1) = real(tts(1:imut, 1:jmut, 1:km), 8)
  tsd(1:imut, 1:jmut, 1:km, 2) = real(sss(1:imut, 1:jmut, 1:km), 8)

  call dens(imut,jmut,km,tsd(1,1,1,1),tsd(1,1,1,2),p_ref_c,rho_str)

  !------------------------------------------------------

  tsd(1:imut, 1:jmut, 1:km, 1) = real(tts(1:imut, 1:jmut, 1:km), 8)
  tsd(1:imut, 1:jmut, 1:km, 2) = real(ss1(1:imut, 1:jmut, 1:km), 8)

  call dens(imut,jmut,km,tsd(1,1,1,1),tsd(1,1,1,2),p_ref_c,rho_thm)

  vol_ref  = 0.0d0
  vol_this = 0.0d0
  rhom_ref = 0.0d0
  rhom_str = 0.0d0
  rhom_thm = 0.0d0
  
  do j = jbt, jet
    do i = ibt, iet
      fac_ref = (hm(i-1,j  ) * a_br(i-1,j  )  &
           &   + hm(i  ,j  ) * a_bl(i  ,j  )  &
           &   + hm(i-1,j-1) * a_tr(i-1,j-1)  &
           &   + hm(i  ,j-1) * a_tl(i  ,j-1))
      dyn_height  = dyn_height  + ratio_nend(j) * fac_ref
    end do
  end do

#ifdef OGCM_BBL
  do k = 1, km - 1
#else /* OGCM_BBL */
  do k = 1, km
#endif /* OGCM_BBL */
    do j = jbt, jet
      do i = ibt, iet
        fac_ref = volt(i,j,k)
        if (k <= ksgm) then
          fac_this = ((dzu(i-1,j  ,k) + dsgm(k) * hm(i,j)) * a_br(i-1,j  )  &
               &    + (dzu(i  ,j  ,k) + dsgm(k) * hm(i,j)) * a_bl(i  ,j  )  &
               &    + (dzu(i-1,j-1,k) + dsgm(k) * hm(i,j)) * a_tr(i-1,j-1)  &
               &    + (dzu(i  ,j-1,k) + dsgm(k) * hm(i,j)) * a_tl(i  ,j-1))
        else
          fac_this = fac_ref
        end if

        vol_ref  = vol_ref  + ratio_nend(j) * fac_ref
        vol_this = vol_this + ratio_nend(j) * fac_this

        rhom_ref = rhom_ref + ratio_nend(j) * fac_ref * rho_ref(i,j,k)
        rhom_str = rhom_str + ratio_nend(j) * fac_this * rho_str(i,j,k)
        rhom_thm = rhom_thm + ratio_nend(j) * fac_this * rho_thm(i,j,k)

      end do
    end do
  end do

  !------------------------------------------------
  ! report product in MKS

  dyn_height  = dyn_height / areag * 1.0d-2

  depth_ref  = vol_ref  / areag * 1.0d-2
  depth_this = vol_this / areag * 1.0d-2

  write(6,*) ' reference depth ', depth_ref
  write(6,*) ' this year       ', depth_this

  rhom_ref = rhom_ref / vol_ref  * 1.0d3  !  cgs -> MKS
  rhom_str = rhom_str / vol_this * 1.0d3  !  cgs -> MKS
  rhom_thm = rhom_thm / vol_this * 1.0d3  !  cgs -> MKS
  
  write(6,*) ' reference density    ', rhom_ref
  write(6,*) ' steric density       ', rhom_str
  write(6,*) ' thermostreic density ', rhom_thm

  steric_anom       = vol_ref / areag * (rhom_ref - rhom_str) / (1.0d3 + rhom_ref) * 1.0d-2
  thermosteric_anom = vol_ref / areag * (rhom_ref - rhom_thm) / (1.0d3 + rhom_ref) * 1.0d-2

  ! output

  open(mtout0, file=flout0, position='append')
  open(mtout1, file=flout1, form='unformatted', access='direct', recl=4)
  open(mtout2, file=flout2, form='unformatted', access='direct', recl=4)
  open(mtout3, file=flout3, form='unformatted', access='direct', recl=4)
  open(mtout4, file=flout4, form='unformatted', access='direct', recl=4)

  write(mtout0,*) '---', trim(flout1), '---'
  write(mtout0,*) ' Boussinesq height    ', dyn_height        ,'[m]'
  write(mtout0,*) ' steric height        ', steric_anom       ,'[m]'
  write(mtout0,*) ' thermosteric height  ', thermosteric_anom ,'[m]'
  write(mtout0,*) '------------------------'

  write(6,*) ' Boussinesq height    ', dyn_height        ,'[m]'
  write(6,*) ' steric height        ', steric_anom       ,'[m]'
  write(6,*) ' thermosteric height  ', thermosteric_anom ,'[m]'

  write(mtout1, rec=1) real(steric_anom, 4)
  write(mtout2, rec=1) real(thermosteric_anom, 4)
  write(mtout3, rec=1) real(dyn_height, 4)
  write(mtout4, rec=1) real(rhom_str, 4)

  close(mtout4)
  close(mtout3)
  close(mtout2)
  close(mtout1)
  close(mtout0)

end program ts2steric
!====================================================
