! -*-F90-*-
!=========================================================================
program mkgrads_grid_defs
  !-----------------------------------------------------------------------
  !     Make GrADS data from OGCM output
  !-----------------------------------------------------------------------

  use basin_param
  use grid_common

  implicit none

  character(len=256) :: file_grid, file_topo

  integer(4), parameter :: mtgr1 = 21
  integer(4), parameter :: mtgr2 = 22
  integer(4), parameter :: mtgr3 = 23
  integer(4), parameter :: mtgr4 = 24
  integer(4), parameter :: mtgr5 = 25
  integer(4), parameter :: mtgr6 = 26
  integer(4), parameter :: mtgr7 = 27

  character(len=256),parameter :: file_xt = 'XDEF_T.txt'
  character(len=256),parameter :: file_xu = 'XDEF_U.txt'
  character(len=256),parameter :: file_yt = 'YDEF_T.txt'
  character(len=256),parameter :: file_yu = 'YDEF_U.txt'
  character(len=256),parameter :: file_zt = 'ZDEF_T.txt'
  character(len=256),parameter :: file_zc = 'ZDEF_C.txt'
  character(len=256),parameter :: file_zb = 'ZDEF_B.txt'

  logical :: linear_x, linear_y, linear_z

  integer(4) :: i, j, k

  !---------------------------------------------------------------------
  
  namelist /ngrdefs/ file_grid, file_topo, linear_x, linear_y, linear_z
  read(5,ngrdefs) 

  !---------------------------------------------------------------------

  call setgrd(file_topo, file_grid)
  
  !----------------------------------------------------------------

  open(mtgr1,file=file_xt)
  write(6,*) ' X-coordinate definition for (T) written to ', trim(file_xt)
  if (linear_x) then
    write(mtgr1,'(1a,I8,1a,F10.5,1a,F10.5)') 'XDEF ',imut,' LINEAR ',alont(1),' ',dxudeg(1)
  else
    write(mtgr1,'(1a,I8,1a)') 'XDEF ',imut,' LEVELS'
    write(mtgr1,'(5F10.3)') (alont(i),i=1,imut)
  end if
  close(mtgr1)

  open(mtgr2,file=file_xu)
  write(6,*) ' X-coordinate definition for (U) written to ', trim(file_xu)
  if (linear_x) then
    write(mtgr2,'(1a,I8,1a,F10.5,1a,F10.5)') 'XDEF ',imut,' LINEAR ',alonu(1),' ',dxtdeg(1)
  else
    write(mtgr2,'(1a,I8,1a)') 'XDEF ',imut,' LEVELS'
    write(mtgr2,'(5F10.3)') (alonu(i),i=1,imut)
  end if
  close(mtgr2)

  open(mtgr3,file=file_yt)
  write(6,*) ' Y-coordinate definition for (T) written to ', trim(file_yt)
  if (linear_y) then
    write(mtgr3,'(1a,I8,1a,F10.5,1a,F10.5)') 'YDEF ',jmut,' LINEAR ',alatt(1),' ',dyudeg(1)
  else
    write(mtgr3,'(1a,I8,1a)') 'YDEF ',jmut,' LEVELS'
    write(mtgr3,'(5F10.3)') (alatt(j),j=1,jmut)
  end if
  close(mtgr3)

  open(mtgr4,file=file_yu)
  write(6,*) ' Y-coordinate definition for (U) written to ', trim(file_yu)
  if (linear_y) then
    write(mtgr4,'(1a,I8,1a,F10.5,1a,F10.5)') 'YDEF ',jmut,' LINEAR ',alatu(1),' ',dytdeg(1)
  else
    write(mtgr4,'(1a,I8,1a)') 'YDEF ',jmut,' LEVELS'
    write(mtgr4,'(5F10.3)') (alatu(j),j=1,jmut)
  end if
  close(mtgr4)

  !

  open(mtgr5,file=file_zt)
  write(6,*) ' Z-coordinate definition for  (top)   written to ', trim(file_zt)
  if (linear_z) then
    write(mtgr5,'(1a,I8,1a,F10.5,1a,F10.5)') 'ZDEF ',km,' LINEAR ',dep(1),' ',dz(1)
  else
    write(mtgr5,'(1a,I8,1a)') 'ZDEF ',km,' LEVELS'
    write(mtgr5,'(5F10.3)') (dep(k),k=1,km)
  end if
  close(mtgr5)

  open(mtgr6,file=file_zc)
  write(6,*) ' Z-coordinate definition for (center) written to ', trim(file_zc)
  if (linear_z) then
    write(mtgr6,'(1a,I8,1a,F10.5,1a,F10.5)') 'ZDEF ',km,' LINEAR ',dp(1),' ',dz(1)
  else
    write(mtgr6,'(1a,I8,1a)') 'ZDEF ',km,' LEVELS'
    write(mtgr6,'(5F10.3)') (dp(k),k=1,km)
  end if
  close(mtgr6)

  open(mtgr7,file=file_zb)
  write(6,*) ' Z-coordinate definition for (bottom) written to ', trim(file_zb)
  if (linear_z) then
    write(mtgr7,'(1a,I8,1a,F10.5,1a,F10.5)') 'ZDEF ',km,' LINEAR ',dep(2),' ',dz(1)
  else
    write(mtgr7,'(1a,I8,1a)') 'ZDEF ',km,' LEVELS'
    write(mtgr7,'(5F10.3)') (dep(k),k=2,km+1)
  end if
  close(mtgr7)

end program mkgrads_grid_defs
