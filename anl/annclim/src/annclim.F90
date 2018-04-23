! -*-F90-*-
module annclim

  use libmxe_para, only: clen, type_libmxe_para
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo
  use libmxe_io, only: type_libmxe_io

  implicit none

  private

  !-- arguments --
  character(clen),save :: file_base      !- 
  character(clen),save :: dir_out        !- output directory
  character(clen),save :: file_out       !- output file name
  real(4),save         :: undef_out      !- undefined value
                                         !-  [default:rundefout or rundef]
  integer(4),save      :: yearstr
  integer(4),save      :: yearend
  integer(4),save      :: yoffset
  character(1),save    :: tuxy
  integer,save         :: kmax = -1

  public :: ini
  public :: calc
  public :: write_result
  public :: next
  public :: has_next

  integer,parameter :: lun = 80

  type(type_libmxe_para),save :: para
  type(type_libmxe_grid),save :: grid
  type(type_libmxe_topo),save :: topo
  type(type_libmxe_io),save :: io

  integer(4),parameter :: day_year = 365
  integer(4),save      :: day_total

  real(8),allocatable,save :: mask(:,:,:)
  
  integer(4),save :: im, jm, km, nrec, reclen, year, nrec_last

  real(4),allocatable,save :: dat3in(:,:,:)
  real(8),allocatable,save :: dat3out(:,:,:)

  logical,save :: l_one_record
  logical,save :: l_read_from_onefile
  logical,save :: l_write_to_text  

contains

subroutine next
  implicit none

  nrec = nrec + 1

end subroutine next


logical function has_next()
  implicit none

  if ( nrec > nrec_last ) then
    has_next = .false.
    if (l_read_from_onefile) then
      close(lun)
    end if
  else
    has_next = .true.
  end if

end function has_next


subroutine ini
  use libmxe_para,  only: libmxe_para__register
  use libmxe_grid,  only: libmxe_grid__register
  use libmxe_topo,  only: libmxe_topo__register, libmxe_topo__aexl
  use libmxe_io,    only: libmxe_io__register

  implicit none

  real(4),parameter :: undef_default = -9.87d33

  integer(4) :: i, j, k, ios

  namelist /nml_annclim/ file_base, dir_out, file_out, &
       & l_one_record, l_read_from_onefile, l_write_to_text, &
       & undef_out, yearstr, yearend, yoffset, &
       & tuxy, kmax

  undef_out = undef_default
  read(5,nml=nml_annclim,iostat=ios)
  if ( ios /= 0 ) then
    write(6,*) ' Read error : namelist'
    stop
  end if

  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)
  call libmxe_topo__aexl(topo,para)
  call libmxe_io__register(io,para)

  if ( io%timemode /= 'year' ) then
    write(6,*) 'Error at time interval of records'
    write(6,*) io%timemode
    write(6,*) 'We expect "year"'
    stop
  end if

  if ( undef_out == undef_default ) undef_out = para%rundefout

  if (l_one_record) then
    im = 1
    jm = 1
    km = kmax
  else
    im = para%imut
    jm = para%jmut
    km = para%km

    select case (tuxy)
    case('T','U','W')
      kmax = km
    case('t','u')
      if ( kmax == -1 ) kmax = 1
    case default
      write(6,*) 'tuxy is not correct !'
      stop
    end select

  end if

  reclen = im * jm * kmax * 4
  nrec_last = io%nm

  allocate(dat3in (im,jm,kmax))
  allocate(dat3out(im,jm,kmax))
  dat3out(:,:,:) = 0.d0

  nrec = 1

  if (l_read_from_onefile) then
    open(lun,file=trim(file_base)//'.gd',form='unformatted',access='direct',recl=reclen,action='read')
    write(6,*) 'Opening ....', trim(file_base)//'.gd'
    write(6,*) '   reclen = ', reclen
  end if

  allocate(mask(im,jm,kmax))

  if ((im > 1) .or. (jm > 1)) then
    select case (tuxy)
    case('T')
      mask(:,:,:) = topo%atexl(:,:,:)
    case('t')
      do k = 1, kmax
        mask(:,:,k) = topo%atexl(:,:,1)
      end do
    case('U')
      mask(:,:,:) = topo%aexl(:,:,:)
    case('u')
      do k = 1, kmax
        mask(:,:,k) = topo%aexl(:,:,1)
      end do
    case('W')
      mask(:,:,:) = topo%atexl(:,:,2:km+1)
    end select
  else
    mask(:,:,:) = 1.0d0
  end if

end subroutine ini


subroutine calc
  use libmxe_io, only: libmxe_io__open
  implicit none

  integer(4) :: k, month, year

  year = io%calrec(nrec)%year + yoffset

  if (l_read_from_onefile) then
    read (lun,rec=nrec) dat3in
  else
    call libmxe_io__open(io,trim(file_base),nrec,reclen,lun,action='read')
    write(6,'(1a,1a,1a,i4.4)') 'Opening ....', trim(file_base),'.',year
    read (lun,rec=1) dat3in
    close(lun)
  end if

  do k = 1, kmax
    where(mask(:,:,k) == 0.d0)
      dat3in(:,:,k) = 0.0
    end where
  end do

  if ((year >= yearstr) .and. (year <= yearend)) then
    dat3out(:,:,:) = dat3out(:,:,:) + dble(day_year+ileap(year))*dble(dat3in(:,:,:))
    day_total = day_total + day_year + ileap(year)
    if (l_write_to_text) then
      do k = 1, kmax
        write(6,*) k, dat3in(:,:,k)
      end do
      write(6,*) day_total, day_year + ileap(year)
    end if
  end if
  
end subroutine calc


subroutine write_result

  use libmxe_io, only: libmxe_io__open
  implicit none

  integer(4) :: k, n

  dat3out(:,:,:) = dat3out(:,:,:) / real(day_total,8)

  if (l_write_to_text) then
    open(lun,file=trim(dir_out)//'/'//trim(file_out),form='formatted',status='unknown')
    do k = 1, kmax
      write(lun,*) k, dat3out(:,:,k)
    end do
    close(lun)
  else
    open(lun,file=trim(dir_out)//'/'//trim(file_out), &
         & form='unformatted',status='unknown',access='direct',   &
         & recl=reclen)
    write(lun,rec=1) sngl(dat3out(:,:,:))
    close(lun)
  end if

end subroutine write_result


integer function ileap(yr)
  implicit none
  integer(4),intent(in) :: yr

  if (.not. io%l_leap_year) then
    ileap = 0
    return
  end if
  
  if (mod(yr,4) == 0) then
    if (mod(yr,100) == 0) then
      if (mod(yr,400) == 0) then
        ileap = 1
      else
        ileap = 0
      end if
    else
      ileap = 1
    end if
  else
    ileap = 0
  end if
end function ileap


end module annclim
