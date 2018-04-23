! -*-F90-*-
module libmxe_para_test
  use libmxe_para
  use libmxe_ut
  use fruit, only: assert_equals
  implicit none
  private


  character(*),parameter,public :: testname='libmxe_para'
  public :: ini_test
  public :: test_imut
  public :: test_rundefin
  public :: test_rundefout


  type(type_libmxe_para),save :: para

  integer,parameter :: lun=20
  integer :: i


contains


subroutine ini_test
  implicit none

  call libmxe_para__register(para)

end subroutine ini_test


subroutine test_imut
  implicit none

  namelist /para_imut/ ival

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
    read(lun,nml=para_imut,iostat=i)
  close(lun)
  if ( i/=0 ) return

  call assert_equals( ival, para%imut, 'imut' )

end subroutine test_imut


subroutine test_rundefin
  implicit none

  namelist /para_rundefin/ rval, rrange

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
    read(lun,nml=para_rundefin,iostat=i)
  close(lun)
  if ( i/=0 ) return

  call assert_equals( rval, para%rundefin, rrange, 'rundefin' )

end subroutine test_rundefin


subroutine test_rundefout
  implicit none

  namelist /para_rundefout/ rval, rrange

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
    read(lun,nml=para_rundefout,iostat=i)
  close(lun)
  if ( i/=0 ) return

  call assert_equals( rval, para%rundefout, rrange, 'rundefout' )

end subroutine test_rundefout


end module libmxe_para_test
