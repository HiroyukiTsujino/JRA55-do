! -*-F90-*-
!====================================================
!
!    GrADS の ctlファイルを作成する
!
!====================================================
subroutine mkgrdctl ( &
    &   file_ctl, file_gd, gd_title,  &
    &   xsize, xlevs, xfmt, linear_x, &
    &   ysize, ylevs, yfmt, linear_y, &
    &   zsize, zlevs, zfmt, linear_z, &
    &   tdef, nvar, max_items,  &
    &   vars, ndims,            &
    &   undef, hd_opts, options )
!----------------------------------------------------

  implicit none

  character(len=*), intent(in) :: file_gd     ! データファイル名
  character(len=*), intent(in) :: file_ctl    ! ctlファイル名
  character(len=*), intent(in) :: gd_title    ! タイトル

  integer(4), intent(in) :: xsize
  real(8), intent(in)    :: xlevs(xsize)
  character(len=*), intent(in) :: xfmt
  logical, intent(in)    :: linear_x

  integer(4), intent(in) :: ysize
  real(8), intent(in)    :: ylevs(ysize)
  character(len=*), intent(in) :: yfmt
  logical, intent(in)    :: linear_y

  integer(4), intent(in) :: zsize
  real(8), intent(in)    :: zlevs(zsize)
  character(len=*), intent(in) :: zfmt
  logical, intent(in)    :: linear_z

  character(len=*), intent(in) :: tdef
  integer(4), intent(in) :: nvar
  integer(4), intent(in) :: max_items
  integer(4), intent(in) :: ndims(max_items)
  character(len=*), intent(in) :: vars(max_items)
  character(len=*), intent(in) :: undef
  character(len=*), intent(in) :: hd_opts
  character(len=*), intent(in) :: options
  integer(4), parameter :: nfctl = 21
  integer(4) :: n, len_var
  character(256) :: vars_tmp

  !------------------------------------------------------------

  write(6,*) '     - output grads ctl file : ', trim(file_ctl)
  open(nfctl,file=file_ctl,form='formatted')

  write(nfctl,'(a)') 'DSET    ^'//trim(file_gd)
  write(nfctl,'(a)') 'OPTIONS big_endian '//trim(options)
  if ( hd_opts /= ' ' ) then
    write(nfctl,'(a)') trim(hd_opts)
  end if
  write(nfctl,'(a)') 'TITLE   '//trim(gd_title)
  write(nfctl,'(a)') 'UNDEF   '//trim(undef)
  call defdims ( 'XDEF', xsize, xlevs, xfmt, nfctl, linear_x )
  call defdims ( 'YDEF', ysize, ylevs, yfmt, nfctl, linear_y )
  call defdims ( 'ZDEF', zsize, zlevs, zfmt, nfctl, linear_z )
  write(nfctl,'(a)') trim(tdef)
  write(nfctl,'(a,i4)') 'VARS ',nvar
  do n = 1, nvar
    vars_tmp = vars(n)
    len_var = len_trim(vars_tmp)
    write(nfctl,'(a9,i4,1a)') vars_tmp(1:9),ndims(n),vars_tmp(14:len_var)
  end do
  write(nfctl,'(a)') 'ENDVARS'
  close(nfctl)
  
end subroutine mkgrdctl
!====================================================
!====================================================
subroutine defdims ( dim, size, levs, dfmt, nfile, linear )

  implicit none

  character(4), intent(in) :: dim
  integer(4),   intent(in) :: size
  real(8),      intent(in) :: levs(size)
  character(len=*), intent(in) :: dfmt
  integer(4), intent(in) :: nfile
  logical, intent(in) :: linear
  real(8) :: dlev1
  integer(4) :: i , j
  
  ! --- check dim name ---

  if ( dim /= 'XDEF' .and. dim /= 'YDEF' .and. dim /= 'ZDEF' ) then
    print *, ' invalid *DEF : ',dim
    stop
  endif

  ! --- check uniform grid spacing ---

  if (linear) then
    dlev1 = levs(2) - levs(1)
    write(nfile,'(1a,i6,1a,2'//dfmt//')') dim//' ',size,' LINEAR ',&
         levs(1),dlev1
  else
    if (size > 1) then
      write(nfile,'(1a,i6,1a)') dim//' ',size,' LEVELS'
      write(nfile,'(8'//dfmt//')')  levs(1:size)
    else
      write(nfile,'(1a,i6,1a,'//dfmt//')') dim//' ',size,' LEVELS ',levs(1)
    end if
  endif
  
end subroutine defdims
!====================================================
