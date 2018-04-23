!-*-F90-*-
program delete_lakes
  implicit none
  integer(4),parameter :: nx = 1440, ny = 720
  integer(4) :: orgx(nx,ny), orgy(nx,ny)
  integer(4),parameter :: lun = 21
  character(len=64) :: file_riverindex_org='nextxy.bin'
  character(len=64) :: file_riverindex_new='nextxy_wo_lake.bin'

  integer(4) :: ocean(nx,ny)
  integer(4) :: i, j
  logical :: is_diagonal = .true.
  logical :: is_end

  integer(4) :: locx(nx*ny), locy(nx*ny), lx, ly, nsize=0, nsize_tmp=0
  integer(4) :: ink = 0

  open(lun,file=file_riverindex_org,form='unformatted',action='read', &
       &    access='direct',recl=4*nx*ny)
  read(lun,rec=1) orgx
  read(lun,rec=2) orgy
  close(lun)

!  write(*,*) orgx(1203,249), orgx(1166,720-611)

  do j = 1, ny
    do i = 1, nx
      if ( orgx(i,j) > 0 ) then
        ocean(i,j) = -9999
      else if ( orgx(i,j) == -9999 .or. orgx(i,j) == -9 ) then
        ocean(i,j) = 9999
      end if
    end do
  end do

  lx = 1443
  ly =  357          ! Pacific
  ocean(lx,ly) = ink ! ink
  call spread( ocean, nx, ny, lx, ly, locx, locy, &
       & is_diagonal, nsize, nsize_tmp, is_end )

  lx =  867
  ly =  720-533      ! Black Sea
  ocean(lx,ly) = ink ! ink
  call spread( ocean, nx, ny, lx, ly, locx, locy, &
       & is_diagonal, nsize, nsize_tmp, is_end )

  do j = 1, ny
    do i = 1, nx
      if ( ocean(i,j) .ne. ink  .and. orgx(i,j) ==-9 ) then
        orgx(i,j) = -99
      end if
      if ( ocean(i,j) .ne. ink  .and. orgy(i,j) ==-9 ) then
        orgy(i,j) = -99
      end if
    end do
  end do


  ! For Yenisei River
  orgx(1058,ny+1-639) = -9
  orgx(1059,ny+1-639) = -9
  orgx(1059,ny+1-638) = -9

  orgy(1058,ny+1-639) = -9
  orgy(1059,ny+1-639) = -9
  orgy(1059,ny+1-638) = -9

  do j = 652, 655
    do i = 1142, 1146
      if (orgx(i,ny+1-j) == -99) then
        orgx(i,ny+1-j) = -9
        orgy(i,ny+1-j) = -9
      end if
    end do
  end do

  open(lun,file=file_riverindex_new,form='unformatted', &
       &    access='direct',recl=4*nx*ny)
  write(lun,rec=1) orgx
  write(lun,rec=2) orgy
  close(lun)


end program delete_lakes
!=====================================================================
recursive subroutine spread(islandtmp ,nx, ny, i, j, locx, locy, &
     & is_diagonal, nsize, nsize_tmp, is_end )
  implicit none

  integer, intent(in) :: nx, ny, i, j,  nsize_tmp
  logical, intent(in) :: is_diagonal
  integer, intent(inout) :: islandtmp(nx, ny), &
       & locx(nx*ny), locy(nx*ny), nsize
  logical, intent(out) :: is_end

  integer :: iw, ie, jn, js, n, dum, ink
  integer :: lx(8), ly(8), nsearch = 4

!  integer :: nsize_max = 100000   ! used when segmentation fault occurs 
                                   ! even if you chage the stack size by the command
                                   ! " % ulimit -s unlimited " .
  integer :: nsize_max = 0
  !
  if (is_diagonal) then 
    nsearch = 8
  else
    nsearch = 4
  end if
  !
  ISENDLOOP: if ( nsize - nsize_tmp < nsize_max &
       & .or. nsize_max == 0 ) then
    is_end = .true.
    nsize = nsize + 1
    if ( nsize > nsize_tmp ) then
      locx(nsize) = i
      locy(nsize) = j 
    end if
    !
    call news(i, j, nx, ny, iw, ie, js, jn)
    !
    ISDIAONALLOOP: if (is_diagonal) then
      lx( 1) = i ; ly( 1) = js
      lx( 2) = iw; ly( 2) = js
      lx( 3) = iw; ly( 3) = j

      lx( 4) = iw ; ly( 4) = jn
      lx( 5) = i  ; ly( 5) = jn
      lx( 6) = ie ; ly( 6) = jn
      lx( 7) = ie ; ly( 7) = j
      lx( 8) = ie ; ly( 8) = js
      !
    else ISDIAONALLOOP
      lx( 1) = i ; ly( 1) = js
      lx( 2) = iw; ly( 2) = j
      lx( 3) = i ; ly( 3) = jn
      lx( 4) = ie; ly( 4) = j
    end if ISDIAONALLOOP
    !
    ink = islandtmp( i, j)
    LOOP_N: do n = 1, nsearch
      if (islandtmp( lx(n), ly(n) ) > ink ) then
        islandtmp( lx(n), ly(n) ) = ink
        call spread( islandtmp, nx, ny, lx(n), ly(n), locx, locy, &
             & is_diagonal, nsize, nsize_tmp, is_end )
        if ( .not. is_end ) exit LOOP_N
      endif
    end do LOOP_N
  else ISENDLOOP
    nsize = nsize + 1
    locx(nsize) = i
    locy(nsize) = j 
    is_end = .false.
  end if ISENDLOOP
end subroutine spread
!====================================================
subroutine news( i, j,  nx, ny, iw, ie, js, jn)
  implicit none
  integer(4), intent(in)  :: i,  j,  nx, ny
  integer(4), intent(out) :: iw, ie, js, jn
  if ( i == 1 ) then
    iw = nx
  else
    iw = i - 1
  endif
  if ( i== nx ) then
    ie = 1
  else
    ie = i + 1
  endif
  js = max(j-1, 1  )
  jn = min(j+1,ny )
end subroutine news
!====================================================
