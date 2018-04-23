! -*-F90-*-
module tidehmap_test
  use tidehmap
  use fruit, only: assert_equals
  implicit none
  private


  character(*),parameter,public :: testname='tidehmap'
  public :: read_answer
  public :: test_calc


  logical,save :: lready = .false.
  real,save :: tideh_100_100, tideh_100_100_delta
  real,save :: tideh_land, tideh_land_delta


contains


subroutine read_answer
  implicit none

  character(*),parameter :: namelist_test='namelist.test'
  integer,parameter :: lun = 21
  integer :: i

  namelist /tidehmap_answer/ tideh_100_100, tideh_100_100_delta &
            & , tideh_land, tideh_land_delta

  lready = .false.

  open(lun,file=namelist_test,status='old',iostat=i)
    if ( i /= 0 ) then
      write(*,*) 'Error at tidehmap_test__read_answer:'
      write(*,*) '  cannot find ',namelist_test
      stop
    endif
    read(lun,nml=tidehmap_answer,iostat=i)
  close(lun)

  if ( i==0 ) lready = .true.

end subroutine read_answer


subroutine test_calc
  implicit none

  real :: d

  if ( .not. lready ) return
  call init
  call calc
  d = get_result(100,100)
  call assert_equals (tideh_100_100, d, tideh_100_100_delta &
                & , 'test_calc(100,100)')

  d = get_result(15,3)
  call assert_equals (tideh_land, d, tideh_land_delta, 'test_calc(land)')

end subroutine test_calc


end module tidehmap_test
