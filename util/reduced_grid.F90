! -*-F90-*-
subroutine set_reduced_grid(gname,numx,jmut,num_grid_1d)

  character(len=*),intent(in) :: gname
  integer(4), intent(in)  :: jmut
  integer(4), intent(out) :: numx(jmut)
  integer(4), intent(out) :: num_grid_1d
  logical :: l_found
  integer(4) :: j
  integer(4) :: num_grid_1d_truth=157792

  l_found = .false.

  if (gname=='TL319') then
    l_found = .true.
    num_grid_1d_truth=157792
    numx(1)      = 48
    numx(2)      = 64
    numx(3:4)    = 80
    numx(5)      = 96
    numx(6:7)    = 112
    numx(8:9)    = 128
    numx(10:11)  = 144
    numx(12:13)  = 160
    numx(14:17)  = 192
    numx(18:21)  = 224
    numx(22:23)  = 240
    numx(24:25)  = 256
    numx(26:30)  = 288
    numx(31:35)  = 320
    numx(36:37)  = 336
    numx(38:45)  = 384
    numx(46:48)  = 400
    numx(49:54)  = 432
    numx(55:57)  = 448
    numx(58:63)  = 480
    numx(64:70)  = 512
    numx(71:81)  = 560
    numx(82:86)  = 576
    numx(87:160) = 640
    do j = 1, 160
      numx(jmut-j+1) = numx(j)
    end do
    num_grid_1d = 0
    do j = 1, jmut
      num_grid_1d = num_grid_1d + numx(j)
    end do
    if (num_grid_1d_truth /= num_grid_1d) then
      write(6,*) ' Grid name = ',trim(gname), ', 1d total grid number = ',num_grid_1d
      stop
    end if
  end if

  if (.not. l_found) then
    write(6,*) ' Name of the grid is not found in the table '
  end if

end subroutine set_reduced_grid
