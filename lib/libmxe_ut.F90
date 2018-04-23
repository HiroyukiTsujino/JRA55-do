! -*-F90-*-
!- Unit Test tool
module libmxe_ut
  implicit none
  private

  integer,parameter :: lun_ut = 40
  integer,parameter :: clen = 256

  logical,public :: flag=.false.       !- .true.: use this check.
  real,public :: rval                  !- answer value (real)
  real(8),public :: dval               !-              (double)
  integer,public :: ival               !-              (integer)
  character(clen),public :: cval       !-              (character)
  logical,public :: lval               !-              (logical)
  real,public :: rrange = 0.e0         !- precision    (real)
  real(8),public :: drange = 0.d0      !-              (double)
  integer,public :: nx=0 , ny=0, nz=0  !- array index

  type,public :: type_answer
    integer         :: ival
    real(4)         :: rval
    real(8)         :: dval
    character(clen) :: cval
    logical         :: lval
    real(4)         :: rrange
    real(8)         :: drange
    integer         :: nx, ny, nz
    logical         :: lread
    character(clen) :: check

    logical :: flag=.false.  !???
  end type type_answer

  public :: libmxe_ut__ini
  public :: libmxe_ut__get

  public :: libmxe_ut__clear          !- Clear variables.
  public :: libmxe_ut__open_namelist  !- Open "namelist.test".
  public :: libmxe_ut__register       !- Register "type_answer".

  character(clen),save :: target_module
  character(clen),save :: last_question


contains


subroutine libmxe_ut__ini( testname )
  implicit none

  character(*),intent(in) :: testname

  character(*),parameter  :: namelist_file='NAMELIST.TEST'

  character(2) :: cnp
  integer :: i, ios

  open( lun_ut, file=namelist_file, status='old', iostat=ios )
  if ( ios /= 0 ) then
    write(*,*) 'Error at libmxe_ut__ini:'
    write(*,*) '  cannot find ',namelist_file
    stop
  endif

  target_module = testname
  last_question = ''

end subroutine libmxe_ut__ini


subroutine libmxe_ut__get( target_question, answer )
  implicit none

  character(*),intent(in)       :: target_question
  type(type_answer),intent(out) :: answer

  character(clen)     :: module_name
  character(clen)     :: question
!  integer         :: ival
!  real(4)         :: rval
!  real(8)         :: dval
!  character(clen) :: cval
!  logical         :: lval
!  real(4)         :: rrange
!  real(8)         :: drange
!  integer         :: nx, ny, nz

  integer             :: ios
  character(clen)     :: ctemp

  namelist /nml_answer/ module_name, question, &
                      & ival, rval, dval, cval, lval, &
                      & rrange, drange, nx, ny, nz

  if ( trim(target_question) /= trim(last_question) ) rewind( lun_ut )


  answer%lread = .false.
  ios          = 0
  do while ( ios >= 0 )

    module_name = ''
    question    = ''
    ival        = -999
    rval        = -9.99e33
    dval        = -9.99d33
    cval        = ''
    lval        = .false.
    nx          = -999
    ny          = -999
    nz          = -999
    rrange      = 0.e0
    drange      = 0.d0

    read( lun_ut, nml=nml_answer, iostat=ios )
    if ( ( trim(module_name) == trim(target_module) ) &
         & .and. ( trim(question) == trim(target_question) ) ) then
      answer%lread = .true.

      if ( ios > 0 ) then
        write(*,*) 'READ ERROR: question = ',trim(question)
        write(*,*) '            iostat   = ',ios
        stop
      endif

      exit

    endif

  enddo

  answer%ival = ival
  answer%rval = rval
  answer%dval = dval
  answer%cval = cval
  answer%lval = lval

  answer%rrange = rrange
  answer%drange = drange  

  answer%check = question

  if ( nx /= -999 ) then
    answer%nx    = nx
    write(ctemp,*) nx
    answer%check = trim(answer%check)//','//trim(adjustl(ctemp))
  endif

  if ( ny /= -999 ) then
    answer%ny    = ny
    write(ctemp,*) ny
    answer%check = trim(answer%check)//','//trim(adjustl(ctemp))
  endif

  if ( nz /= -999 ) then
    answer%nz    = nz
    write(ctemp,*) nz
    answer%check = trim(answer%check)//','//trim(adjustl(ctemp))
  endif

  last_question = target_question

end subroutine libmxe_ut__get


subroutine libmxe_ut__clear
  implicit none

  flag=.false.
  rval=0.e0
  dval=0.d0
  ival=0
  cval=""
  lval=.false.
  rrange = 0.e0
  drange = 0.d0
  nx=0
  ny=0
  nz=0

end subroutine libmxe_ut__clear


subroutine libmxe_ut__open_namelist( lun )
  implicit none

  integer,intent(in) :: lun
  character(*),parameter :: namelist_test='namelist.test'
  integer :: i

  open(lun,file=namelist_test,status='old',iostat=i)
  if ( i /= 0 ) then
    write(*,*) 'Error at libmxe_ut__open_namelist:'
    write(*,*) '  cannot find ',namelist_test
    stop
  endif

end subroutine libmxe_ut__open_namelist


subroutine libmxe_ut__register( answer, flag, rval, dval, ival, cval, lval &
                           , rrange, drange, nx, ny, nz )
  implicit none

  type(type_answer),intent(out) :: answer
  logical,intent(in) :: flag
  real,intent(in),optional :: rval, rrange
  real(8),intent(in),optional :: dval, drange
  integer,intent(in),optional :: ival, nx, ny, nz
  character(clen),intent(in),optional :: cval
  logical,intent(in),optional :: lval

  answer%flag = flag
  if ( present(rval) ) answer%rval = rval
  if ( present(dval) ) answer%dval = dval
  if ( present(ival) ) answer%ival = ival
  if ( present(cval) ) answer%cval = cval
  if ( present(lval) ) answer%lval = lval
  if ( present(rrange) ) answer%rrange = rrange
  if ( present(drange) ) answer%drange = drange
  if ( present(nx) ) answer%nx = nx
  if ( present(ny) ) answer%ny = ny
  if ( present(nz) ) answer%nz = nz

end subroutine libmxe_ut__register


end module libmxe_ut
