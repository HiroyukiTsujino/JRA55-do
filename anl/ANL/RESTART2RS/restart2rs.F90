!restart2rs.F90
!====================================================
!
! Decompose a sequential restart file into direct access files
!
!====================================================
program restart2rs
  !
  implicit none
  !
  character(len=256)    :: flin
  character(len=256)    :: flout
  character(len=256)    :: floutpath
  character(len= 32)    :: ext_str
  !
  integer(4), save :: imut = 1
  integer(4), save :: jmut = 1
  integer(4), save :: km   = 1
  !
  integer(4), save :: ext_year   = 0
  integer(4), save :: ext_month  = 1
  integer(4), save :: ext_day    = 1
  integer(4), save :: ext_hour   = 0
  integer(4), save :: ext_minute = 0
  integer(4), save :: ext_second = 0
  !
  logical, save :: l_oflt   = .false.
  logical, save :: l_vvdimp = .false.
  logical, save :: l_melyam = .false.
  logical, save :: l_nohkim = .false.
  !
  namelist /nml_restart2rs/  flin, floutpath,      &
    &    imut, jmut, km,                           &
    &    ext_year, ext_month,  ext_day,            &
    &    ext_hour, ext_minute, ext_second,         &
    &    l_oflt, l_vvdimp, l_melyam, l_nohkim
  !
  integer(4), parameter :: mtin     = 81
  integer(4), parameter :: mtout    = 83
  !
  integer(4) :: last, month, iday, ihour, imin          !  common.F90
  real(8)    :: over                                    !  rdjobp.F90
  real(8), allocatable  :: ahour(:), aday(:), ayear(:)  !  common.F90
  real(8), allocatable  :: pd(:)       !-+
  real(8), allocatable  :: pm(:)       ! | density.F90
  real(8), allocatable  :: dmn(:)      ! |
  real(8)    :: ddmna                  !-+
  real(8), allocatable  :: d3(:, :, :)
  real(8), allocatable  :: ds(:, :, :)
  !
  !==============================================
  !
  read(unit=5, nml_restart2rs)
  write(*,*) 'flin     :', trim(flin)
  write(*,*) 'floutpath:', trim(floutpath)
  write(*,*) 'year     :', ext_year
  write(*,*) 'month    :', ext_month
  write(*,*) 'day      :', ext_day
  write(*,*) 'hour     :', ext_hour
  write(*,*) 'minute   :', ext_minute
  write(*,*) 'second   :', ext_second
  write(*,*) 'OFLT     :', l_oflt
  write(*,*) 'VVDIMP   :', l_vvdimp
  write(*,*) 'MELYAM   :', l_melyam
  write(*,*) 'NOHKIM   :', l_nohkim
  !
  allocate(ahour(km))
  allocate(aday(km))
  allocate(ayear(km))
  allocate(pd(km))
  allocate(pm(km+1))
  allocate(dmn(km))
  allocate(d3(imut, jmut, km))
  allocate(ds(imut, jmut, 3))
  !----------------------------------------------
  !
  write(ext_str, '(i4.4, i2.2, i2.2, i2.2, i2.2, i2.2)')             &
    &  ext_year, ext_month, ext_day, ext_hour, ext_minute, ext_second
  open (mtin, file=flin, form='unformatted')
  !
  read (mtin) last, month, iday, ihour, imin, over, ahour, aday, ayear, pd, pm, ddmna, dmn
  write(*,*) 'last, month, iday, ihour, imin'
  write(*,*)  last, month, iday, ihour, imin
  write(*,*) 'over'
  write(*,*) over
  write(*,*) 'ahour(1), aday(1), ayear(1)'
  write(*,*) ahour(1), aday(1), ayear(1)
  !
  write(flout, '(a, a, a)') trim(floutpath), 'rs_pd.', trim(ext_str)
  write(*, *) trim(flout)
  open (mtout, file=flout, form='unformatted', access='direct', recl=8*km)
  write(mtout, rec=1) pd(1:km)
  close(mtout)
  !
  write(flout, '(a, a, a)') trim(floutpath), 'rs_pm.', trim(ext_str)
  write(*, *) trim(flout)
  open (mtout, file=flout, form='unformatted', access='direct', recl=8*(km+1))
  write(mtout, rec=1) pm(1:km+1)
  close(mtout)
  !
  write(flout, '(a, a, a)') trim(floutpath), 'rs_dmn.', trim(ext_str)
  write(*, *) trim(flout)
  open (mtout, file=flout, form='unformatted', access='direct', recl=8*km)
  write(mtout, rec=1) dmn(1:km)
  close(mtout)
  !
  write(flout, '(a, a, a)') trim(floutpath), 'rs_ddmna.', trim(ext_str)
  write(*, *) trim(flout)
  open (mtout, file=flout, form='unformatted', access='direct', recl=8)
  write(mtout, rec=1) ddmna
  close(mtout)
  !
  read (mtin) d3
  write(flout, '(a, a, a)') trim(floutpath), 'rs_u.', trim(ext_str)
  write(*, *) trim(flout)
  open (mtout, file=flout, form='unformatted', access='direct', recl=8*imut*jmut*km)
  write(mtout, rec=1) d3(1:imut, 1:jmut, 1:km)
  close(mtout)
  !
  read (mtin) d3
  write(flout, '(a, a, a)') trim(floutpath), 'rs_v.', trim(ext_str)
  write(*, *) trim(flout)
  open (mtout, file=flout, form='unformatted', access='direct', recl=8*imut*jmut*km)
  write(mtout, rec=1) d3(1:imut, 1:jmut, 1:km)
  close(mtout)
  !
  read (mtin) d3
  write(flout, '(a, a, a)') trim(floutpath), 'rs_t.', trim(ext_str)
  write(*, *) trim(flout)
  open (mtout, file=flout, form='unformatted', access='direct', recl=8*imut*jmut*km)
  write(mtout, rec=1) d3(1:imut, 1:jmut, 1:km)
  close(mtout)
  !
  read (mtin) d3
  write(flout, '(a, a, a)') trim(floutpath), 'rs_s.', trim(ext_str)
  write(*, *) trim(flout)
  open (mtout, file=flout, form='unformatted', access='direct', recl=8*imut*jmut*km)
  write(mtout, rec=1) d3(1:imut, 1:jmut, 1:km)
  close(mtout)
  !
  if(.not. l_oflt) then
    read (mtin) ds
    !
    write(flout, '(a, a, a)') trim(floutpath), 'rs_ssh.', trim(ext_str)
    write(*, *) trim(flout)
    open (mtout, file=flout, form='unformatted', access='direct', recl=8*imut*jmut)
    write(mtout, rec=1) ds(1:imut, 1:jmut, 1)
    close(mtout)
    !
    write(flout, '(a, a, a)') trim(floutpath), 'rs_uml.', trim(ext_str)
    write(*, *) trim(flout)
    open (mtout, file=flout, form='unformatted', access='direct', recl=8*imut*jmut)
    write(mtout, rec=1) ds(1:imut, 1:jmut, 2)
    close(mtout)
    !
    write(flout, '(a, a, a)') trim(floutpath), 'rs_vml.', trim(ext_str)
    write(*, *) trim(flout)
    open (mtout, file=flout, form='unformatted', access='direct', recl=8*imut*jmut)
    write(mtout, rec=1) ds(1:imut, 1:jmut, 3)
    close(mtout)
  end if
  !
  if(l_vvdimp) then
    read (mtin) d3
    write(flout, '(a, a, a)') trim(floutpath), 'rs_avdml.', trim(ext_str)
    write(*, *) trim(flout)
    open (mtout, file=flout, form='unformatted', access='direct', recl=8*imut*jmut*km)
    write(mtout, rec=1) d3(1:imut, 1:jmut, 1:km)
    close(mtout)
    !
    read (mtin) d3
    write(flout, '(a, a, a)') trim(floutpath), 'rs_avmml.', trim(ext_str)
    write(*, *) trim(flout)
    open (mtout, file=flout, form='unformatted', access='direct', recl=8*imut*jmut*km)
    write(mtout, rec=1) d3(1:imut, 1:jmut, 1:km)
    close(mtout)
  end if
  !
  if(l_melyam .or. l_nohkim) then
    read (mtin) d3
    write(flout, '(a, a, a)') trim(floutpath), 'rs_ml_avq.', trim(ext_str)
    write(*, *) trim(flout)
    open (mtout, file=flout, form='unformatted', access='direct', recl=8*imut*jmut*km)
    write(mtout, rec=1) d3(1:imut, 1:jmut, 1:km)
    close(mtout)
  end if
  !
  if(l_melyam) then
    read (mtin) d3
    write(flout, '(a, a, a)') trim(floutpath), 'rs_melyam_q.', trim(ext_str)
    write(*, *) trim(flout)
    open (mtout, file=flout, form='unformatted', access='direct', recl=8*imut*jmut*km)
    write(mtout, rec=1) d3(1:imut, 1:jmut, 1:km)
    close(mtout)
    !
    read (mtin) d3
    write(flout, '(a, a, a)') trim(floutpath), 'rs_melyam_alo.', trim(ext_str)
    write(*, *) trim(flout)
    open (mtout, file=flout, form='unformatted', access='direct', recl=8*imut*jmut*km)
    write(mtout, rec=1) d3(1:imut, 1:jmut, 1:km)
    close(mtout)
  end if
  !
  if(l_nohkim) then
    read (mtin) d3
    write(flout, '(a, a, a)') trim(floutpath), 'rs_nohkim_eb.', trim(ext_str)
    write(*, *) trim(flout)
    open (mtout, file=flout, form='unformatted', access='direct', recl=8*imut*jmut*km)
    write(mtout, rec=1) d3(1:imut, 1:jmut, 1:km)
    close(mtout)
  end if
  close(mtin)
!====================================================
end program restart2rs
