! -*-F90-*-
!- Make grads control file for MRI.COM experiment output.
module libmxe_grads
  use libmxe_para, only: clen, type_libmxe_para, rundef
  implicit none
  private


  !-- Structure for settings of grads control file
  type,public :: type_grads
    character(clen) :: file_base
      !- grads data filename without suffix
    character(clen) :: title
      !- text for TITLE
    integer :: istr=0 ,iend=0, jstr=0, jend=0
      !- horizontal grid ranges (istr:iend, jstr:jend) 
      !-  [default: (1:imut, 1:jmut)]
    character(1) :: cgrid
      !- default (or 'U') : use U-grid (same as iuvts=0 in MRI.COM)
      !- 'T' : use T-grid (same as iuvts=1)
    integer :: km=0
      !- number of vertical grids [default:km]
    character(clen) :: ztype='center'
      !- 'center' :: box center
      !- 'bottom' :: box bottom
      !- 'surface' :: sea surface (ignore km)
      !- 'specify' :: specified by array z
    real :: z(100)
      !- used when ztype='specify'
    character(clen) :: timemode=''
      !- default : use time mode of libmxe_io.
      !-  'stationary' : stationary data named 'file_base.gd'
      !-  'plain     ' : data named            'file_base' 
    character(clen) :: dset_suffix=''
      !-  specify data suffix, otherwise data suffix is created from time mode
    integer :: nrec_first=1, nrec_last=0
      !- time record range [default: (1:io%nm)]
    integer :: nvar
      !- number of variables (<=20)
    character(clen) :: var(20)
      !- variable settings (e.g. 't 50 99 temperature')
    real :: undef = rundef
      !- missing value (default: para%rundefout)
  end type type_grads

    
  !-- subroutine --
  public :: libmxe_grads__make
  public :: libmxe_grads__clear


contains 
!---------------------------------------------------------------------


subroutine libmxe_grads__make(grads,para,grid,io)
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_io, only: type_libmxe_io
  use libmxe_calendar, only: libmxe_calendar__cdate
  implicit none


  type(type_grads),intent(in)  :: grads
  type(type_libmxe_para),intent(in) :: para
  type(type_libmxe_grid),intent(in) :: grid
  type(type_libmxe_io),intent(in) :: io

  integer,parameter :: lun = 86
  integer :: i,k,istr,iend,jstr,jend,imut,jmut,km
  real :: undef
  character(clen) :: ctemp, tdef, ctemp2, ctemp3 &
                  & , dset_suffix, cdate, timemode
  integer :: nrec_last


  !---- check ----
  if ( .not. grid%ldef )  then
    write(*,*) 'Error at libmxe_grads__make'
    write(*,*) '  grid is not registered.'
    stop
  endif
  if ( .not. io%ldef )  then
    write(*,*) 'Error at libmxe_grads__make'
    write(*,*) '  io is not registered.'
    stop
  endif
  imut = para%imut
  jmut = para%jmut
  km = para%km


  !--  dset_suffix, tdef --
  timemode = grads%timemode
  if ( timemode == '' ) then
    timemode = io%timemode
  endif
  cdate = libmxe_calendar__cdate(io%calrec(grads%nrec_first))
          !- HH:MMzDDMMMYYYY
  if ( grads%nrec_last == 0 ) then
    nrec_last = io%nm
  else
    nrec_last = grads%nrec_last
  endif
  write(ctemp,'(i5)') nrec_last - grads%nrec_first + 1

  select case (timemode)
    case ('second')

    case ('minute')
      dset_suffix = '.%y4%m2%d2%h2%n2'
      ctemp2 = cdate
      write(ctemp3,'(i2)') io%calint%minute 
      tdef = trim(ctemp)//' LINEAR  '//trim(ctemp2)//' '//trim(ctemp3)//'MN'
    case ('hour')
      dset_suffix = '.%y4%m2%d2%h2'
      ctemp2 = cdate(1:2)//cdate(6:15)
      write(ctemp3,'(i2)') io%calint%hour
      tdef = trim(ctemp)//' LINEAR  '//trim(ctemp2)//' '//trim(ctemp3)//'HR'
    case ('day')
      dset_suffix = '.%y4%m2%d2'
      ctemp2 = cdate(7:15)
      write(ctemp3,'(i2)') io%calint%day
      tdef = trim(ctemp)//' LINEAR  '//trim(ctemp2)//' '//trim(ctemp3)//'DY'
    case ('month')
      dset_suffix = '.%y4%m2'
      ctemp2 = cdate(9:15)
      write(ctemp3,'(i2)') io%calint%month
      tdef = trim(ctemp)//' LINEAR  '//trim(ctemp2)//' '//trim(ctemp3)//'MO'
    case ('year')
      dset_suffix = '.%y4'
      ctemp2 = cdate(9:15)
      write(ctemp3,'(i2)') io%calint%year
      tdef = trim(ctemp)//' LINEAR  '//trim(ctemp2)//' '//trim(ctemp3)//'YR'
    case ('stationary')
      dset_suffix = '.gd'
      ctemp2 = cdate(7:15)
      tdef = '1 LINEAR  '//trim(ctemp2)//' 1YR'
    case ('plain')
      dset_suffix = ''
      ctemp2 = cdate(7:15)
      tdef = trim(ctemp)//' LINEAR  '//trim(ctemp2)//' 1YR'
    case default
      write(*,*) 'Error at libmxe_grads__make'
      write(*,*) '  timemode=',trim(timemode)
      stop
  end select

  if (.not.grads%dset_suffix=='') dset_suffix=grads%dset_suffix

  !---- Write control file ----
  ctemp = trim(grads%file_base)//'.ctl'
  write(*,*) 'Write GRADS control file: ',trim(ctemp)
  open(lun,file=ctemp,form='formatted')


  !-- dset --
  i = scan(grads%file_base,'/',back=.true.)
  write(lun,'(a)') 'DSET ^'//trim(grads%file_base(i+1:))//trim(dset_suffix)
  

  !-- option --
  ctemp='OPTIONS big_endian'
  if ( .not. io%l_leap_year ) then
    ctemp=trim(ctemp)//' 365_day_calendar'
  endif
  if ( ( timemode /= 'stationary' ).and.( timemode /= 'plain' ) ) then
    ctemp=trim(ctemp)//' template'
  endif
  write(lun,'(a)') trim(ctemp)


  !-- title --
  write(lun,'(a)') 'TITLE '//trim(grads%title)


  !-- undef --
  undef = grads%undef
  if ( undef == rundef ) then
    undef = para%rundefout
  endif
    !- Undef is overwritten by rundefout unless specified.
  write(ctemp,'(e14.7)') undef
  write(lun,'(a)') 'UNDEF '//trim(ctemp)


  !-- xdef --
  istr = grads%istr
  iend = grads%iend
  if (istr==0) istr = 1
  if (iend==0) iend = imut
  if (istr==iend) then
    write(lun,'(a,f11.5,a)') 'XDEF 1 LINEAR ',grid%lont(istr),' 1.0'
  else
    write(lun,'(a,i4,a)') 'XDEF ', iend-istr+1,' LEVELS'
    if ( grads%cgrid=='T' ) then
      write(lun,'(5f11.5)') grid%lont(istr:iend)
    else
      write(lun,'(5f11.5)') grid%lonu(istr:iend)
    endif
  endif


  !-- ydef --
  jstr = grads%jstr
  jend = grads%jend
  if (jstr==0) jstr = 1
  if (jend==0) jend = jmut
  if (jstr==jend) then
    write(lun,'(a,f11.5,a)') 'YDEF 1 LINEAR ',grid%latt(jstr),' 1.0'
  else
    write(lun,'(a,i4,a)') 'YDEF ', jend-jstr+1,' LEVELS'
    if ( grads%cgrid=='T' ) then
      write(lun,'(5f11.5)') grid%latt(jstr:jend)
    else
      write(lun,'(5f11.5)') grid%latu(jstr:jend)
    endif
  endif


  !-- zdef --
  k = km
  if (grads%km/=0) then
    k = grads%km
  endif  

  select case ( grads%ztype ) 
    case ('center')
      write(lun,'(a,i4,a)') 'ZDEF ', k,' LEVELS'
      write(lun,'(5f11.5)') grid%depm(1:k)*0.01d0  !- [cm] => [m]

    case ('bottom')
      write(lun,'(a,i4,a)') 'ZDEF ', k,' LEVELS'
      write(lun,'(5f11.5)') grid%dep(2:k+1)*0.01d0

    case ('surface')
!      write(ctemp,'(f4.1)') grid%depm(1)*0.01d0
      write(lun,'(a)') 'ZDEF 1 LEVELS 0.0'    

    case ('specify')
      if ( grads%km <= 1 ) then
        write(*,*) 'Error at libmxe_grads__make:'
        write(*,*) ' grads%km =',grads%km,' must be >= 2 when ztype=specify'
        stop
      endif
      write(lun,'(a,i4,a)') 'ZDEF ', k,' LEVELS'
      write(lun,'(5f11.5)') grads%z(1:k)

    case default
      write(*,*) 'Error at libmxe_grads__make'
      write(*,*) '  grads%ztype=',trim(grads%ztype)
      stop

  end select


  !-- tdef --
  write(lun,'(a)') 'TDEF '//trim(tdef)


  !-- variables --
  write(lun,'(a,i2)') 'VARS ',grads%nvar
  do i=1,grads%nvar
    write(lun,*) trim(grads%var(i))
  enddo
  write(lun,'(a)') 'ENDVARS'


  close(lun)


end subroutine libmxe_grads__make


subroutine libmxe_grads__clear( grads )
  use libmxe_para, only: rundef
  implicit none

  type(type_grads),intent(out)  :: grads

  grads%file_base   = ''
  grads%title       = ''
  grads%cgrid       = ''
  grads%ztype       = ''
  grads%timemode    = ''
  grads%dset_suffix = ''
  grads%km          = 0
  grads%nrec_first  = 1
  grads%nrec_last   = 0
  grads%nvar        = 0
  grads%var(:)      = ''
  grads%undef       = rundef
  grads%istr        = 0
  grads%iend        = 0
  grads%jstr        = 0
  grads%jend        = 0

end subroutine libmxe_grads__clear


end module libmxe_grads
