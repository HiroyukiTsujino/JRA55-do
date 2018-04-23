! -*-F90-*-
!- EXP basic parameters
module libmxe_para
  implicit none
  private


  !-- constant --
  integer,parameter,public :: clen = 256
    !- default length for character variables

  real(8),parameter,public :: dundef = -9.99d33 &
                          & , radius = 6375.d5 &  !- earth radius [cm]
                          & , grav = 981.d0 & 
                          & , rho = 1.036d0 &
                          & , pi = 3.141592653589793d0
  real(8),parameter,public :: &
    &   radian   = 180.D0/pi &
    & , radian_r = 1.d0/radian &
    & , omega    = pi/43082.D0 ! angular velocity of rotating Earth

  real,parameter,public :: rundef = -9.99e33  !- default undef

  integer,parameter,public :: iuvpnt = 0, itspnt = 1
    !-  flag of U/T grid (U: iuvts=iuvpnt, T: iuvts=itspnt)

  character(*),parameter,public  :: file_default = 'no_file'


  !-- structure --
  type,public :: type_libmxe_para
    character(clen) :: namelist      !- namelist file

    integer :: imut, jmut, km        !- MRI.COM configure.in
    integer :: ksgm    !- number of vertical grid in sigma layer
    integer :: kbbl    !- number of vertical grid in BBL

    logical :: lsub      !- .true. : sub model of nest EXP
    logical :: lcyclic   !- .true. : cyclic zonal condition
    logical :: lspherical  !- .true. : spherical coordinate
    logical :: ljot        !- .false. : jot coordinate
    logical :: ltripolar   !- .false. : tripolar coordinate
    logical :: lfoldnp     !- .true. : tripolar/jot
    logical :: l_force_no_foldnp  ! force no folding condition for northern boundary
    logical :: l_globe     ! spherical and no-halo (such as observational data set)
    logical :: lbbl        !- .true. : BBL is included
    logical :: lzstar      !- .true. : zstar vertical coordinate
    logical :: ldef=.false. !- .true. : this object is registered
    real(4) :: rundefin   !- undef values for input
    real(4) :: rundefout  !- undef values for output
    integer :: ibt, iet, jbt, jet !- core-region (T-points)
    integer :: ibu, ieu, jbu, jeu !- core-region (U-points)
    integer :: nmc    !- core-region width, used for tripolar/jot
    real(8),pointer :: dz(:)         !- same as grid%dz_cm

  end type type_libmxe_para


  !-- subroutine --
  public :: libmxe_para__register
    !- register an object following namelist file

  private :: libmxe_para__clear
    !- clear an objcet

  integer,parameter,private :: lun = 89
  integer,parameter,private :: lun2 = 90


contains 
!-----------------------------------------------------------------


subroutine libmxe_para__register(para,file_namelist)
  implicit none

  type(type_libmxe_para),intent(out) :: para
  character(*),intent(in),optional     :: file_namelist

  character(clen) :: namelist_open
  integer         :: ios

  integer         :: imut, jmut, km, ksgm, kbbl
  integer         :: k
  logical         :: lsub, lcyclic, lspherical, ljot, ltripolar, lzstar
  logical         :: l_force_no_foldnp, l_globe
  real(4)         :: rundefin, rundefout

  character(clen) :: file_dz_cm
  real(8)         :: dz_const_cm

  namelist /nml_grid_size/ imut, jmut, km, ksgm, kbbl
  namelist /nml_coordinate/ lsub, lcyclic, lspherical, ljot, ltripolar, lzstar, l_force_no_foldnp, l_globe
  namelist /nml_vert_grid/ file_dz_cm, dz_const_cm
  namelist /nml_undef/ rundefin, rundefout

  call libmxe_para__clear(para)

  if ( present( file_namelist ) ) then
    namelist_open = file_namelist
  else
    namelist_open = 'NAMELIST.MXE'
  endif

  imut = 0
  jmut = 0
  km   = 0
  ksgm = 0
  kbbl = 0
  lsub       = .false.
  lcyclic    = .false.
  lspherical = .false.
  ljot       = .false.
  ltripolar  = .false.
  lzstar     = .false.
  l_force_no_foldnp = .false.
  l_globe    = .false.
  rundefin   = rundef
  rundefout  = rundef

  open( lun, file=trim(namelist_open), status='old' )

  read( lun, nml=nml_grid_size )

  write(*,*) 'Reading ',trim(namelist_open),' ...'

  para%imut = imut
  para%jmut = jmut
  para%km = km
  para%ksgm = ksgm
  para%kbbl = kbbl

  rewind(lun)
  read( lun, nml=nml_coordinate )
  para%lsub       = lsub
  para%lcyclic    = lcyclic
  para%lspherical = lspherical
  para%ljot       = ljot
  para%ltripolar  = ltripolar
  para%lzstar     = lzstar
  para%l_force_no_foldnp = l_force_no_foldnp
  para%l_globe    = l_globe


  !-- Set dependent parameters. --
  if (para%ljot .or. para%ltripolar) then
    para%lfoldnp = .true.
    if (para%l_force_no_foldnp) then
      para%lfoldnp = .false.
    end if
  else
    para%lfoldnp = .false.
  end if
  if (para%lfoldnp .and. para%lspherical) then
    write(*,*) 'ERROR: Inconsistent settings.'
    write(*,*) ' lspherical = ', para%lspherical
    write(*,*) ' ltripolar  = ', para%ltripolar
    write(*,*) ' ljot       = ', para%ljot
    stop
  end if

  if (para%l_globe .and. .not. para%lspherical) then
    write(*,*) 'ERROR: Inconsistent settings.'
    write(*,*) ' l_globe    = ', para%l_globe
    write(*,*) ' lspherical = ', para%lspherical
    stop
  end if

  if (para%lcyclic) then
    para%ibt = 3
    para%ibu = 3
    para%iet = para%imut - 2
    para%ieu = para%imut - 2
  else
    para%ibt = 2
    para%ibu = 2
    para%iet = para%imut
    para%ieu = para%imut - 1
  end if

  para%nmc = para%iet - para%ibt + 1 ! width of the core region

  para%jbt = 2
  para%jbu = 2
  if (para%lfoldnp) then
    para%jet = para%jmut - 2
    para%jeu = para%jmut - 2
  else
    para%jet = para%jmut
    para%jeu = para%jmut - 1
  end if

  if (kbbl > 0) then
    para%lbbl = .true.
  else
    para%lbbl = .false.
  end if


  !-- vertical grid --
  file_dz_cm  = file_default
  dz_const_cm = 0.d0
  read( lun, nml=nml_vert_grid )
  allocate( para%dz(1:km) )

  if ( trim(file_dz_cm) /= trim(file_default) ) then

    if ( dz_const_cm /= 0.d0 ) then
      write(6,*) 'ERROR: Redundant specification:'
      write(6,*) '    file_dz_cm'
      write(6,*) '    dz_const_cm'
      write(6,*) '  Only one of them should be specified.'
      stop
    endif

    open( lun2, file=trim(file_dz_cm), form='unformatted', &
         & status='old' )
    read( lun2 ) k
    if ( k == km ) then
      read( lun2 ) para%dz
    else
      write(6,*) 'ERROR: inconsistent size of ',trim(file_dz_cm)
      write(6,*) '  layer number: ',k
      stop
    endif
    close( lun2 )

  else
    para%dz(1:km) = dz_const_cm
  endif

  !- Though reading dz should be in libmxe_grid.F90,
  !-   it is placed here to make libmxe_topo independent of libmxe_grid.
  !- Other vertical variables (depth etc) are
  !-   given by libmxe_grid__register.


  !-- undef value --
  rewind(lun)
  read( lun, nml=nml_undef, iostat=ios )  !- optional
  para%rundefin   = rundefin
  para%rundefout  = rundefout


  close(lun)

  para%namelist = namelist_open
  para%ldef = .true.

end subroutine libmxe_para__register
!-----------------------------------------------------------------


subroutine libmxe_para__clear(para)
  implicit none

  type(type_libmxe_para),intent(out) :: para

  para%namelist = ''
  para%imut = 0
  para%jmut = 0
  para%km = 0
  para%ksgm = 0
  para%kbbl = 0
  para%lsub = .false.
  para%lcyclic = .false.
  para%lspherical = .false.
  para%ltripolar = .false.
  para%ljot = .false.
  para%lfoldnp = .false.
  para%lzstar = .false.
  para%l_force_no_foldnp = .false.
  para%rundefin = 0.d0
  para%rundefout = 0.d0
  para%ldef = .false.
  para%lbbl = .false.

  para%ibt = 0
  para%ibu = 0
  para%iet = 0
  para%ieu = 0
  para%jbt = 0
  para%jbu = 0
  para%jet = 0
  para%jeu = 0
  para%nmc = 0

end subroutine libmxe_para__clear


end module libmxe_para
