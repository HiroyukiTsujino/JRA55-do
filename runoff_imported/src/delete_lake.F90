!-*-F90-*-
program delete_lakes
  implicit none
  integer(4),parameter :: nx = 1440, ny = 720
  integer(4) :: orgx(nx,ny), orgy(nx,ny)
  integer(4),parameter :: lun = 21
  character(len=64) :: file_riverindex_org='data_etc/nextxy_big_endian_noyrev_lon0strt.bin'
  character(len=64) :: file_riverindex_new='data_etc/nextxy_wo_lake_big_endian_noyrev_lon0strt.bin'

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

  lx =  799
  ly =  409          ! Pacific
  ocean(lx,ly) = ink ! ink
  call spread( ocean, nx, ny, lx, ly, locx, locy, &
       & is_diagonal, nsize, nsize_tmp, is_end )

  lx =  147
  ly =  533      ! Black Sea
  ocean(lx,ly) = ink ! ink
  call spread( ocean, nx, ny, lx, ly, locx, locy, &
       & is_diagonal, nsize, nsize_tmp, is_end )

  open (99,file='data_etc/ocean.dat',form='unformatted',access='direct', &
       & recl = 4 * nx * ny)
  write(99,rec=1) ocean
  close(99)

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

!  shusei 
!   (following points are identified as lake for the previous routine,
!    but actually the coastal points)
  orgx(1160, 144) = -9
  orgy(1160, 144) = -9
  orgx(1159, 145) = -9
  orgy(1159, 145) = -9
  orgx(1160, 145) = -9
  orgy(1160, 145) = -9
  orgx(1159, 146) = -9
  orgy(1159, 146) = -9
  orgx(1161, 146) = -9
  orgy(1161, 146) = -9
  orgx(1157, 147) = -9
  orgy(1157, 147) = -9
  orgx(1158, 147) = -9
  orgy(1158, 147) = -9
  orgx(1159, 147) = -9
  orgy(1159, 147) = -9
  orgx(1162, 147) = -9
  orgy(1162, 147) = -9
  orgx(1176, 199) = -9
  orgy(1176, 199) = -9
  orgx(1173, 202) = -9
  orgy(1173, 202) = -9
  orgx( 168, 405) = -9
  orgy( 168, 405) = -9
  orgx( 162, 417) = -9
  orgy( 162, 417) = -9
  orgx( 163, 452) = -9
  orgy( 163, 452) = -9
  orgx( 236, 470) = -9
  orgy( 236, 470) = -9
  orgx( 145, 477) = -9
  orgy( 145, 477) = -9
  orgx( 193, 481) = -9
  orgy( 193, 481) = -9
  orgx(1439, 497) = -9
  orgy(1439, 497) = -9
  orgx( 968, 500) = -9
  orgy( 968, 500) = -9
  orgx( 134, 511) = -9
  orgy( 134, 511) = -9
  orgx(1183, 542) = -9
  orgy(1183, 542) = -9
  orgx( 948, 553) = -9
  orgy( 948, 553) = -9
  orgx( 948, 554) = -9
  orgy( 948, 554) = -9
  orgx( 949, 554) = -9
  orgy( 949, 554) = -9
  orgx( 947, 555) = -9
  orgy( 947, 555) = -9
  orgx( 948, 555) = -9
  orgy( 948, 555) = -9
  orgx( 951, 555) = -9
  orgy( 951, 555) = -9
  orgx( 947, 556) = -9
  orgy( 947, 556) = -9
  orgx( 948, 556) = -9
  orgy( 948, 556) = -9
  orgx( 949, 556) = -9
  orgy( 949, 556) = -9
  orgx( 950, 556) = -9
  orgy( 950, 556) = -9
  orgx( 946, 557) = -9
  orgy( 946, 557) = -9
  orgx( 948, 557) = -9
  orgy( 948, 557) = -9
  orgx( 949, 557) = -9
  orgy( 949, 557) = -9
  orgx( 942, 558) = -9
  orgy( 942, 558) = -9
  orgx( 943, 558) = -9
  orgy( 943, 558) = -9
  orgx( 945, 558) = -9
  orgy( 945, 558) = -9
  orgx( 946, 558) = -9
  orgy( 946, 558) = -9
  orgx( 947, 558) = -9
  orgy( 947, 558) = -9
  orgx( 948, 558) = -9
  orgy( 948, 558) = -9
  orgx( 941, 559) = -9
  orgy( 941, 559) = -9
  orgx( 943, 559) = -9
  orgy( 943, 559) = -9
  orgx( 944, 559) = -9
  orgy( 944, 559) = -9
  orgx( 947, 559) = -9
  orgy( 947, 559) = -9
  orgx( 940, 560) = -9
  orgy( 940, 560) = -9
  orgx( 941, 560) = -9
  orgy( 941, 560) = -9
  orgx( 942, 560) = -9
  orgy( 942, 560) = -9
  orgx( 943, 560) = -9
  orgy( 943, 560) = -9
  orgx( 940, 561) = -9
  orgy( 940, 561) = -9
  orgx( 941, 561) = -9
  orgy( 941, 561) = -9
  orgx(1084, 622) = -9
  orgy(1084, 622) = -9
  orgx(1082, 623) = -9
  orgy(1082, 623) = -9
  orgx(1086, 623) = -9
  orgy(1086, 623) = -9
  orgx(1082, 624) = -9
  orgy(1082, 624) = -9
  orgx(1083, 624) = -9
  orgy(1083, 624) = -9
  orgx( 283, 626) = -9
  orgy( 283, 626) = -9
  orgx( 284, 627) = -9
  orgy( 284, 627) = -9
  orgx( 307, 635) = -9
  orgy( 307, 635) = -9
  orgx( 309, 635) = -9
  orgy( 309, 635) = -9
  orgx( 311, 635) = -9
  orgy( 311, 635) = -9
  orgx( 310, 636) = -9
  orgy( 310, 636) = -9
  orgx( 306, 637) = -9
  orgy( 306, 637) = -9
  orgx( 308, 637) = -9
  orgy( 308, 637) = -9
  orgx( 971, 646) = -9
  orgy( 971, 646) = -9
  orgx( 597, 647) = -9
  orgy( 597, 647) = -9
  orgx(1118, 650) = -9
  orgy(1118, 650) = -9
  orgx(1119, 650) = -9
  orgy(1119, 650) = -9
  orgx(1126, 650) = -9
  orgy(1126, 650) = -9
  orgx(1127, 650) = -9
  orgy(1127, 650) = -9
  orgx(1128, 651) = -9
  orgy(1128, 651) = -9
  orgx(1119, 652) = -9
  orgy(1119, 652) = -9
  orgx(1121, 652) = -9
  orgy(1121, 652) = -9
  orgx(1126, 652) = -9
  orgy(1126, 652) = -9
  orgx(1128, 652) = -9
  orgy(1128, 652) = -9
  orgx(1118, 653) = -9
  orgy(1118, 653) = -9
  orgx(1120, 653) = -9
  orgy(1120, 653) = -9
  orgx(1098, 670) = -9
  orgy(1098, 670) = -9
  orgx(1099, 670) = -9
  orgy(1099, 670) = -9
  orgx(1096, 671) = -9
  orgy(1096, 671) = -9
  orgx(1099, 671) = -9
  orgy(1099, 671) = -9
  orgx(1100, 671) = -9
  orgy(1100, 671) = -9
  orgx(1102, 671) = -9
  orgy(1102, 671) = -9
  orgx(1095, 672) = -9
  orgy(1095, 672) = -9
  orgx(1100, 672) = -9
  orgy(1100, 672) = -9
  orgx(1099, 673) = -9
  orgy(1099, 673) = -9
  orgx(1100, 673) = -9
  orgy(1100, 673) = -9
  orgx(1101, 673) = -9
  orgy(1101, 673) = -9
  orgx(1102, 673) = -9
  orgy(1102, 673) = -9
  orgx(1097, 674) = -9
  orgy(1097, 674) = -9
  orgx(1100, 674) = -9
  orgy(1100, 674) = -9
  orgx(1153, 681) = -9
  orgy(1153, 681) = -9
  orgx(1195, 686) = -9
  orgy(1195, 686) = -9
  orgx(1327, 693) = -9
  orgy(1327, 693) = -9

  orgx(  71, 477) = -9
  orgy(  71, 477) = -9
  orgx(1086, 622) = -9
  orgy(1086, 622) = -9
  orgx(1083, 623) = -9
  orgy(1083, 623) = -9
  orgx(1084, 623) = -9
  orgy(1084, 623) = -9
  orgx(1085, 623) = -9
  orgy(1085, 623) = -9
  orgx( 284, 626) = -9
  orgy( 284, 626) = -9
  orgx( 310, 635) = -9
  orgy( 310, 635) = -9
  orgx(1123, 650) = -9
  orgy(1123, 650) = -9
  orgx(1124, 650) = -9
  orgy(1124, 650) = -9
  orgx(1128, 650) = -9
  orgy(1128, 650) = -9
  orgx(1118, 651) = -9
  orgy(1118, 651) = -9
  orgx(1119, 651) = -9
  orgy(1119, 651) = -9
  orgx(1118, 652) = -9
  orgy(1118, 652) = -9
  orgx(1119, 653) = -9
  orgy(1119, 653) = -9
  orgx(1097, 671) = -9
  orgy(1097, 671) = -9
  orgx(1096, 673) = -9
  orgy(1096, 673) = -9
  orgx(1096, 674) = -9
  orgy(1096, 674) = -9  

  orgx( 311, 636) = -9
  orgy( 311, 636) = -9
  orgx( 598, 647) = -9
  orgy( 598, 647) = -9
  orgx(1120, 650) = -9
  orgy(1120, 650) = -9
  orgx(1122, 650) = -9
  orgy(1122, 650) = -9
  orgx(1098, 671) = -9
  orgy(1098, 671) = -9


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
