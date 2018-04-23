! -*-F90-*-
!--------------------------- mapping_table.F90 -------------------------------
! Information:
!
!     mapping_table : read transformation table and create data on new grid
!
!-----------------------------------------------------------------------------
module mapping_table

  implicit none

  type :: type_table
    character(256) :: name
    integer(4) :: nlink
    integer(4) :: isrc_max
    integer(4) :: idst_max
    integer(4),pointer :: isrc(:), idst(:)
    real(8),pointer :: wgt(:)
    integer(4) :: num_wgts_all
  end type type_table

  type :: type_table_vec
    character(256) :: name
    integer(4) :: nlink
    integer(4) :: isrc_max
    integer(4) :: idst_max
    integer(4),pointer :: isrc(:), idst(:)
    real(8),pointer :: wgt(:,:)
    integer(4) :: num_wgts_all
  end type type_table_vec

  integer(4), parameter :: mttbl = 151

contains

  subroutine mapping_table__ini(  &
       & name,                    &
       & file_remap, rmp_table,   &
       & imx_src,jmx_src,kmx_src, &
       & imx_dst,jmx_dst,kmx_dst  &
       & )

    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: file_remap
    type(type_table),intent(inout) :: rmp_table
    integer(4), intent(in) :: imx_src, jmx_src, kmx_src
    integer(4), intent(in) :: imx_dst, jmx_dst, kmx_dst

    !-------

    rmp_table%name = trim(name)
    write(6,*) ' Reading table from...',trim(file_remap),' for ', trim(rmp_table%name)
    open(mttbl,file=file_remap,form='unformatted',action='read')

    read(mttbl) rmp_table%isrc_max
    read(mttbl)
    read(mttbl)
    read(mttbl) rmp_table%idst_max
    read(mttbl)
    read(mttbl)

    write(6,*) '   isrc_max = ',rmp_table%isrc_max,' / ', imx_src * jmx_src, ' or ', imx_src * jmx_src * kmx_src
    write(6,*) '   idst_max = ',rmp_table%idst_max,' / ', imx_dst * jmx_dst, ' or ', imx_dst * jmx_dst * kmx_dst
  
    read(mttbl) rmp_table%num_wgts_all, rmp_table%nlink
    
    write(6,'(1a,I8)') 'nlink = ', rmp_table%nlink

    allocate(rmp_table%isrc(1:rmp_table%nlink))
    allocate(rmp_table%idst(1:rmp_table%nlink))
    allocate(rmp_table%wgt (1:rmp_table%nlink))
    
    read(mttbl) rmp_table%isrc(1:rmp_table%nlink)
    read(mttbl) rmp_table%idst(1:rmp_table%nlink)
    read(mttbl) rmp_table%wgt (1:rmp_table%nlink)

    close(mttbl)

  end subroutine mapping_table__ini

  subroutine mapping_table_vec__ini(  &
       & name,                        &
       & file_remap, rmp_table_vec,   &
       & imx_src,jmx_src,kmx_src,     &
       & imx_dst,jmx_dst,kmx_dst      &
       & )

    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: file_remap
    type(type_table_vec),intent(inout) :: rmp_table_vec
    integer(4), intent(in) :: imx_src, jmx_src, kmx_src
    integer(4), intent(in) :: imx_dst, jmx_dst, kmx_dst

    !-------

    rmp_table_vec%name = trim(name)
    write(6,*) ' Reading table from...',trim(file_remap),' for ', trim(rmp_table_vec%name)

    open(mttbl,file=file_remap,form='unformatted',action='read')

    read(mttbl) rmp_table_vec%isrc_max
    read(mttbl)
    read(mttbl)
    read(mttbl) rmp_table_vec%idst_max
    read(mttbl)
    read(mttbl)

    write(6,*) '   isrc_max = ',rmp_table_vec%isrc_max,' / ', imx_src * jmx_src, ' or ', imx_src * jmx_src * kmx_src
    write(6,*) '   idst_max = ',rmp_table_vec%idst_max,' / ', imx_dst * jmx_dst, ' or ', imx_dst * jmx_dst * kmx_dst
  
    read(mttbl) rmp_table_vec%num_wgts_all, rmp_table_vec%nlink
    
    write(6,'(1a,I8)') 'nlink = ', rmp_table_vec%nlink

    allocate(rmp_table_vec%isrc(1:rmp_table_vec%nlink))
    allocate(rmp_table_vec%idst(1:rmp_table_vec%nlink))
    allocate(rmp_table_vec%wgt (1:rmp_table_vec%num_wgts_all, 1:rmp_table_vec%nlink))
    
    read(mttbl) rmp_table_vec%isrc(1:rmp_table_vec%nlink)
    read(mttbl) rmp_table_vec%idst(1:rmp_table_vec%nlink)
    read(mttbl) rmp_table_vec%wgt (1:rmp_table_vec%num_wgts_all, 1:rmp_table_vec%nlink)

    close(mttbl)

  end subroutine mapping_table_vec__ini

  subroutine mapping_table__main( &
       & rmp_table,               &
       & datout,area_recv,imx,jmx,kmx, &
       & datin,imxi,jmxi,kmxi,undef_in, &
       & ist, ied, jst, jed             &
       & )

    type(type_table),intent(in) :: rmp_table
    integer(4), intent(in)  :: imx, jmx, kmx
    real(8),    intent(out) :: datout(imx,jmx,kmx)
    real(8),    intent(out) :: area_recv(imx,jmx,kmx)
    integer(4), intent(in)  :: imxi, jmxi, kmxi
    integer(4), intent(in)  :: ist, ied, jst, jed
    real(8),    intent(in)  :: datin(imxi,jmxi,kmxi)
    real(8),    intent(in)  :: undef_in
    
    integer(4) :: n

    integer(4) :: isrc_3d, isrc_2d, i_src, j_src, k_src
    integer(4) :: idst_3d, idst_2d, i_dst, j_dst, k_dst

    !---------------------------------

    datout(:,:,:) = 0.0d0
    area_recv(:,:,:) = 0.0d0

!$omp parallel
!$omp do private(idst_3d, idst_2d, k_dst, j_dst, i_dst, &
!$omp &          isrc_3d, isrc_2d, k_src, j_src, i_src)
    do n = 1, rmp_table%nlink

      idst_3d = rmp_table%idst(n)
      k_dst = int((idst_3d - 1) / (imx * jmx)) + 1
      idst_2d = idst_3d - (k_dst - 1) * imx * jmx
      j_dst = jmx - int((idst_2d - 1) / imx)
      i_dst = idst_2d - (jmx - j_dst) * imx

      isrc_3d = rmp_table%isrc(n)
      k_src = int((isrc_3d - 1) / (imxi * jmxi)) + 1
      isrc_2d = isrc_3d - (k_src - 1) * imxi * jmxi
      j_src = int((isrc_2d - 1) / imxi) + 1
      i_src = isrc_2d - (j_src - 1) * imxi

      if ((i_src < ist)  .or. (ied < i_src)) then
        write(6,'(1a,4i6)') ' i_src is out of core region : ', i_src, ist, ied, j_src
      end if
      if ((j_src < jst)  .or. (jed < j_src)) then
        write(6,'(1a,4i6)') ' j_src is out of core region : ', j_src, jst, jed, i_src
      end if

      if (datin(i_src,j_src,k_src) /= undef_in) then
        datout(i_dst,j_dst,k_dst) = datout(i_dst,j_dst,k_dst)  &
             & + rmp_table%wgt(n) * datin(i_src,j_src,k_src)
        area_recv(i_dst,j_dst,k_dst) = area_recv(i_dst,j_dst,k_dst) &
             & + rmp_table%wgt(n)
      else
        write(6,*) ' WARNING: Source data is undefined ', i_src,j_src,k_src
      end if

    end do
!$omp end parallel

  end subroutine mapping_table__main

  subroutine mapping_table_vec__main( &
       & rmp_table_vec,               &
       & datou,datov,area_recv,imx,jmx,kmx, &
       & datiu,dativ,imxi,jmxi,kmxi,undef_in, &
       & ist, ied, jst, jed             &
       & )

    type(type_table_vec),intent(in) :: rmp_table_vec
    integer(4), intent(in)  :: imx, jmx, kmx
    real(8),    intent(out) :: datou(imx,jmx,kmx)
    real(8),    intent(out) :: datov(imx,jmx,kmx)
    real(8),    intent(out) :: area_recv(imx,jmx,kmx)
    integer(4), intent(in)  :: imxi, jmxi, kmxi
    integer(4), intent(in)  :: ist, ied, jst, jed
    real(8),    intent(in)  :: datiu(imxi,jmxi,kmxi)
    real(8),    intent(in)  :: dativ(imxi,jmxi,kmxi)
    real(8),    intent(in)  :: undef_in
    
    integer(4) :: n

    integer(4) :: isrc_3d, isrc_2d, i_src, j_src, k_src
    integer(4) :: idst_3d, idst_2d, i_dst, j_dst, k_dst

    !---------------------------------

    datou(:,:,:) = 0.0d0
    datov(:,:,:) = 0.0d0
    area_recv(:,:,:) = 0.0d0

!$omp parallel
!$omp do private(idst_3d, idst_2d, k_dst, j_dst, i_dst, &
!$omp &          isrc_3d, isrc_2d, k_src, j_src, i_src)
    do n = 1, rmp_table_vec%nlink

      idst_3d = rmp_table_vec%idst(n)
      k_dst = int((idst_3d - 1) / (imx * jmx)) + 1
      idst_2d = idst_3d - (k_dst - 1) * imx * jmx
      j_dst = jmx - int((idst_2d - 1) / imx)
      i_dst = idst_2d - (jmx - j_dst) * imx

      isrc_3d = rmp_table_vec%isrc(n)
      k_src = int((isrc_3d - 1) / (imxi * jmxi)) + 1
      isrc_2d = isrc_3d - (k_src - 1) * imxi * jmxi
      j_src = int((isrc_2d - 1) / imxi) + 1
      i_src = isrc_2d - (j_src - 1) * imxi

      if ((i_src < ist)  .or. (ied < i_src)) then
        write(6,'(1a,4i6)') ' i_src is out of core region : ', i_src, ist, ied, j_src
      end if
      if ((j_src < jst)  .or. (jed < j_src)) then
        write(6,'(1a,4i6)') ' j_src is out of core region : ', j_src, jst, jed, i_src
      end if

      if ((datiu(i_src,j_src,k_src) /= undef_in) .and. (dativ(i_src,j_src,k_src) /= undef_in)) then
        datou(i_dst,j_dst,k_dst) = datou(i_dst,j_dst,k_dst) &
             & + (rmp_table_vec%wgt(2,n) * datiu(i_src,j_src,k_src) - rmp_table_vec%wgt(3,n) * dativ(i_src,j_src,k_src))
        datov(i_dst,j_dst,k_dst) = datov(i_dst,j_dst,k_dst) &
             & + (rmp_table_vec%wgt(3,n) * datiu(i_src,j_src,k_src) + rmp_table_vec%wgt(2,n) * dativ(i_src,j_src,k_src))
        area_recv(i_dst,j_dst,k_dst) = area_recv(i_dst,j_dst,k_dst) + rmp_table_vec%wgt(1,n)
      else
        write(6,*) ' WARNING: Source data is undefined ', i_src,j_src,k_src
      end if

    end do
!$omp end parallel

  end subroutine mapping_table_vec__main

end module mapping_table
