! -*-F90-*-
!-- Make difference between 2 outputs
module diff
  use libmxe_para, only: clen, type_libmxe_para
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo
  use libmxe_io, only: type_libmxe_io
  use libmxe_grads, only: type_grads
  implicit none
  private


  !-- arguments --
  character(clen) :: file_namelist1  !- EXP1 namelist
  character(clen) :: file_namelist2  !- EXP2 namelist
  character(clen),save :: file_base1  !- EXP1 input file base
  character(clen),save :: file_base2  !- EXP2 input file base
  character(clen),save :: diro  !- output directory
  character(clen),save :: fileo  !- output file name
  logical :: l2d  !- .true.: 2D data / .false.: 3D data
  character(1) :: cgrid  !- "T": T-grid / "U": U-grid
  real,save :: missing  !- missing value
  integer,save :: nstr = 1  !- start N  [default: 1]
  integer,save :: nend = 0  !- end N [default: nm]
  integer,save :: nstr2     !- EXP2 start [default: nstr]
  integer,save :: nstep_change_record2 = 1
                !- If EXP1 is hourly and EXP2 is daily,
                !-   set nstep_change_record2 = 24.

  public :: ini
  public :: calc
  public :: get_result
  public :: write_result
  public :: next
  public :: has_next


  integer, parameter :: lun = 77
  real, parameter :: missing_default = -999.999e10
  real,allocatable,save :: r(:,:,:)
  real,allocatable,save :: r1(:,:,:), r2(:,:,:)
  integer,allocatable,save :: exnn(:,:)
  integer,save :: im, jm, km, nm, reclen
  integer,save :: nrec    !- count record loop
  integer,save :: nrec2

  type(type_libmxe_para),save :: para1,para2
  type(type_libmxe_grid),save :: grid1
  type(type_libmxe_topo),save :: topo1
  type(type_libmxe_io),save :: io1,io2
  type(type_grads),save :: grads


contains


subroutine next
  implicit none

  nrec = nrec + 1
 
end subroutine next


logical function has_next
  implicit none

  if ( nrec > nend ) then
    has_next = .false.
  else
    has_next = .true.
  endif

end function has_next


subroutine ini
  use libmxe_para, only: libmxe_para__register
  use libmxe_grid, only: libmxe_grid__register
  use libmxe_topo, only: libmxe_topo__register
  use libmxe_io, only: libmxe_io__register
  use libmxe_grads, only: libmxe_grads__make
  implicit none

  integer :: i
  character(3) :: ctemp

  namelist /diff_lst/ file_namelist1, file_namelist2, &
                    & file_base1, file_base2, &
                    & diro, fileo, l2d, cgrid, missing, nstr, nend, &
                    & nstr2, nstep_change_record2

  !---- arguments ----
  missing = missing_default
  nstr2 = 0
  read(5,nml=diff_lst,iostat=i)
  if ( i /= 0 ) then
    write(*,*) ' Read error : namelist'
    stop
  endif
  if ( nstr2 == 0 ) nstr2 = nstr

  !---- experiment settings ----
  call libmxe_para__register(para1 &
             & ,file_namelist=trim(file_namelist1))
  call libmxe_grid__register(grid1,para1)
  call libmxe_topo__register(topo1,para1)
  call libmxe_io__register(io1,para1)
  im = para1%imut
  jm = para1%jmut
  km = para1%km
  if ( l2d ) km = 1
  reclen = im*jm*km*4
  if ( nend == 0 ) nend = io1%nm
  if ( ( nstr < 1).or.( nstr > io1%nm ) &
       & .or.( nend < 1).or.( nend > io1%nm ) ) then
    write(*,*) 'Error at ini'
    write(*,*) '  Wrong nstr, nend',nstr,nend
    stop
  endif

  call libmxe_para__register(para2 &
             & ,file_namelist=trim(file_namelist2))
  call libmxe_io__register(io2,para2)
  if ( nstr2 + (nend-nstr)/nstep_change_record2 > io2%nm ) then
    write(*,*) 'Error: Not enough EXP2 record'
    write(*,*) '  number of record: ',io2%nm
    write(*,*) '  need            : ',nstr2 + (nend-nstr)/nstep_change_record2
    stop
  endif

  !-- topography --
  allocate( exnn(im,jm) )
  if ( cgrid=='U' ) then
    exnn(1:im,1:jm) = topo1%exnn(1:im,1:jm)
  else
    exnn(1:im,1:jm) = topo1%texnn(1:im,1:jm)
  endif

  !-- grads control file --
  grads%file_base = trim(fileo)
  grads%title = 'Diff: '//trim(fileo)
  grads%cgrid = cgrid
  grads%ztype = 'center'
  if ( l2d ) grads%ztype = 'surface'
  grads%nvar = 1
  write(ctemp,'(i3)') km
  grads%var(1) = 'f '//trim(ctemp)//' 99 Diff'
  call libmxe_grads__make(grads,para1,grid1,io1)

  allocate(r(im,jm,km))
  allocate(r1(im,jm,km),r2(im,jm,km))

  nrec  = nstr
  nrec2 = nstr2 - 1  !- "-1": CACL add 1 to nrec2 and open file.

end subroutine ini


subroutine calc
  use libmxe_io, only: libmxe_io__open
  implicit none

  integer :: i,j,k


  if ( mod(nrec,100) == 0 ) write(*,*) nrec
  if ( mod(nrec-nstr,nstep_change_record2) == 0 ) nrec2 = nrec2 + 1

  !-- input --
  call libmxe_io__open(io1,trim(file_base1),nrec,reclen &
       & ,lun,action='read')
  read(lun,rec=1) r1
  close(lun)
  call libmxe_io__open(io2,trim(file_base2),nrec2,reclen &
       & ,lun,action='read')
  read(lun,rec=1) r2
  close(lun)

  r(:,:,:) = r1(:,:,:) - r2(:,:,:)

  !-- land --
  do j = 1, jm
    do i = 1, im
      do k = 1, km
        if ( exnn(i,j) < k ) r(i,j,k) = para1%rundefout
      enddo
    enddo
  enddo

  !-- missing --
  if ( missing /= missing_default ) then
    do k = 1, km
      do j = 1, jm
        do i = 1, im
          if ( ( r1(i,j,k) == missing ) &
               & .or.( r2(i,j,k) == missing )) then
            r(i,j,k) = para1%rundefout
          endif
        enddo
      enddo
    enddo
  endif

end subroutine calc


subroutine write_result
  use libmxe_io, only: libmxe_io__open
  implicit none

  call libmxe_io__open(io1,trim(diro)//'/'//trim(fileo) &
                  & , nrec, reclen, lun, action='write')
    write(lun,rec=1) r(:,:,:)
  close(lun)

end subroutine write_result


real function get_result(i,j,k)
  implicit none

  integer,intent(in) :: i,j,k

  get_result = r(i,j,k)

end function get_result


end module diff
