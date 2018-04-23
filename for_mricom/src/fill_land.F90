! -*-F90-*-
!- fill values in land 
module fill_land
  use libmxe_para, only: type_libmxe_para, clen 
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo

  implicit none
  private  

  integer,parameter :: izero = 0
  integer,save :: im, jm, km
  real(8) :: dundef_in = -9.99e33
  integer(4) :: max_loop = 1000

  logical :: l_cyclic = .true.
  logical :: l_silent = .false.
  character(len=clen) :: file_org     = 'file_org'
  character(len=clen) :: file_filled  = 'file_org.filled'
  real(4),allocatable :: r3d(:,:,:)


  ! work array for modify_data
  real(8),allocatable :: filled(:,:) ! filled: 1.d0, not filled: 0.d0
  real(8),allocatable :: filled_next(:,:) 
  real(8),allocatable :: work  (:,:), work_next(:,:)

  !-- subroutines --
  public :: ini
  public :: read_data
  public :: modify_data
  public :: write_data
  public :: deallocate_arrays

  type(type_libmxe_para),save :: para
  type(type_libmxe_grid),save :: grid
  type(type_libmxe_topo),save :: topo
  
contains

subroutine ini (nrec_str, nrec_end)
  use libmxe_para, only: libmxe_para__register
  use libmxe_grid, only: libmxe_grid__register
  use libmxe_topo, only: libmxe_topo__register
  use libmxe_topo, only: libmxe_topo__aexl
  implicit none

  integer(4),intent(out) :: nrec_str, nrec_end

  integer(4) :: i

  nrec_str = 1
  nrec_end = 1

  namelist /fill_land_lst/ file_org, file_filled, nrec_str, nrec_end, l_cyclic, dundef_in, max_loop, l_silent
  !---- Read ARG ----
  read(5,nml=fill_land_lst,iostat=i)

  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)
  call libmxe_topo__aexl(topo,para)

  im = para%imut
  jm = para%jmut
  km = para%km

  allocate(r3d(im,jm,km))

  allocate(filled(0:im+1,0:jm+1),filled_next(0:im+1,0:jm+1))
  allocate(work  (0:im+1,0:jm+1),work_next  (0:im+1,0:jm+1))
    
end subroutine ini

subroutine read_data ( nrec )
  implicit none
  integer(4),intent(in) :: nrec
  integer,parameter :: lun = 10

  open(lun,file=file_org,form='unformatted',access='direct',&
       & recl = 4 * im * jm * km)
  read(lun,rec=nrec) r3d
  close(lun)
  
end subroutine read_data

subroutine modify_data 
  implicit none

  integer(4) :: i, j, k, n
  integer(4) :: not_filled
  real(8) :: sum_filled
  real(8) :: slant_fact = 0.561d0, sum_limit = 1.d0
! slant_fact は円に外接する正方形を9等分した場合の
! (角の正方形のうち円に含まれる面積)/(辺の正方形のうち円に含まれる面積)
! で定義される。

! sum_limit とは、斜めが一つだけ接している場合には外挿しないことを意味する。
 
 
  kloop: do k = 1, km
    filled(:,:) = 0.d0
    work  (:,:) = dundef_in
    not_filled = 0

    do j = 1, jm
      do i = 1, im
        if ( topo%aexl(i,j,k) == 1.d0 ) then
          filled(i,j) = 1.d0
          work  (i,j) = r3d(i,j,k)
        end if
      end do
    end do

    if ( l_cyclic ) then
      call set_margin( work,   im, jm)
      call set_margin( filled, im, jm)
    end if
    filled_next(:,:) = filled(:,:)
    work_next  (:,:) = work(:,:)

    nloop: do n = 1, max_loop
      not_filled = 0
      do j = 1, jm
        do i = 1, im
          if (filled(i,j)==0.d0) then
            not_filled = not_filled + 1
            sum_filled = filled(i-1,j) &
                     & + filled(i+1,j) &
                     & + filled(i,j-1) &
                     & + filled(i,j+1) &
                     & + slant_fact * (  &
                     &   filled(i-1,j-1) &
                     & + filled(i+1,j-1) &
                     & + filled(i-1,j+1) &
                     & + filled(i+1,j+1) &
                     & )
            if ( sum_filled >= sum_limit ) then
              filled_next(i,j) = 1.d0
              work_next  (i,j) =  &
                   & ( filled(i-1,j)*work(i-1,j) &
                   & + filled(i+1,j)*work(i+1,j) &
                   & + filled(i,j-1)*work(i,j-1) &
                   & + filled(i,j+1)*work(i,j+1) &
                   & + slant_fact * (   &
                   &   filled(i-1,j-1)*work(i-1,j-1) &
                   & + filled(i+1,j-1)*work(i+1,j-1) &
                   & + filled(i-1,j+1)*work(i-1,j+1) &
                   & + filled(i+1,j+1)*work(i+1,j+1) &
                   &                ) &
                   & ) / sum_filled
            end if
          end if
        end do
      end do
      work  (:,:) = work_next  (:,:)
      filled(:,:) = filled_next(:,:)

      if ( l_cyclic ) then
        call set_margin( work,   im, jm)
        call set_margin( filled, im, jm)
      end if

      if (.not. l_silent) write(*,'(A, I4,I10)') 'not_filled=' , n,  not_filled
      if ( not_filled == 0 ) exit nloop
    end do nloop

    do j = 1, jm
      do i = 1, im
        r3d(i,j,k) = work(i,j)
      end do
    end do

  end do kloop
    
   
end subroutine modify_data

subroutine write_data ( nrec )
  implicit none
  integer(4),intent(in) :: nrec
  integer,parameter :: lun = 10

  open(lun,file=file_filled,form='unformatted',access='direct',&
       & recl = 4 * im * jm * km)
  write(lun,rec=nrec) r3d
  close(lun)
end subroutine write_data

subroutine deallocate_arrays
  deallocate(filled,filled_next,work,work_next)
end subroutine deallocate_arrays

subroutine set_margin( AA, il, jl )
  integer(4),intent(in)  :: il, jl
  real(8), intent(inout) :: AA(0:il+1,0:jl+1)
  integer(4) :: i, j 

  ! cyclic condition
  do j = 1, jl
    AA(0,j   ) = AA(il  ,j)
    AA(il+1,j) = AA(1   ,j)
  end do

end subroutine set_margin


end module fill_land
