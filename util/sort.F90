!-*-F90-*-
! Copied from Pkg of H.Nakano (2016.05.09)
!
subroutine hpsort(n,ra)
  implicit none
  integer(4),intent(in) :: n
  real(8),intent(inout) :: ra(n)

  real(8) :: rra

  integer(4) :: i, ir, j, l
  
  l = n/2+1
  ir = n
  ! The index L will be decremented from its initial value
  ! during the "hiring" (heap creation) phase. Once it reaches 1, 
  ! the index ir will be decremented from its initial value down to 1
  ! during the "retirement-and-promotion" (heap selection) phase.

  do
    if ( l > 1 ) then
      l = l-1
      rra=ra(l)
    else
      rra=ra(ir)
      ra(ir)=ra(1)
      ir = ir -1
      if (ir==1) then
        ra(1) = rra
        return
      end if
    end if
    i = l
    j = l + l

    do while ( j <= ir) 
      if ( j < ir ) then
        if (ra(j) < ra(j+1)) j = j+1
      end if
      if ( rra < ra(j)) then
        ra(i) = ra(j)
        i=j
        j = j+ j
      else
        j = ir+1
      end if
    end do
    ra(i) = rra
  end do
end subroutine hpsort

subroutine hpsort2(n,ra,rb)
  implicit none
  integer(4),intent(in) :: n
  real(8),intent(inout) :: ra(n),rb(n)

  real(8) :: rra,rrb

  integer(4) :: i, ir, j, l
  
  l = n/2+1
  ir = n
  ! The index L will be decremented from its initial value
  ! during the "hiring" (heap creation) phase. Once it reaches 1, 
  ! the index ir will be decremented from its initial value down to 1
  ! during the "retirement-and-promotion" (heap selection) phase.

  do
    if ( l > 1 ) then
      l = l-1
      rra=ra(l)
      rrb=rb(l)
    else
      rra=ra(ir)
      rrb=rb(ir)
      ra(ir)=ra(1)
      rb(ir)=rb(1)
      ir = ir -1
      if (ir==1) then
        ra(1) = rra
        rb(1) = rrb
        return
      end if
    end if
    i = l
    j = l + l

    do while ( j <= ir) 
      if ( j < ir ) then
        if (ra(j) < ra(j+1)) j = j+1
      end if
      if ( rra < ra(j)) then
        ra(i) = ra(j)
        rb(i) = rb(j)
        i=j
        j = j+ j
      else
        j = ir+1
      end if
    end do
    ra(i) = rra
    rb(i) = rrb
  end do
end subroutine hpsort2

subroutine iswap(i,j)
  implicit none
  integer(4),intent(inout) :: i, j
  integer(4) :: itmp
  itmp = i
  i = j
  j = itmp
end subroutine iswap

subroutine indexx(n,arr,index)
  implicit none
  integer(4),intent(in) :: n
  real(8) :: arr(n)
  integer(4),intent(out) :: index(n)

  integer(4) :: k, i, j, indext, jstack, l, r
  real(8) :: a

  integer(4), parameter :: nn = 15, nstack = 50
  integer(4) :: istack(nstack)

  do i = 1, n
    index(i) = i
  end do

  jstack = 0
  l = 1
  r = n
  do
    if ( r-l < NN ) then
      do j = l + 1, r
        indext = index(j)
        a = arr(indext)
        do i = j-1, l, -1
          if (arr(index(i)) <= a ) exit
          index(i+1) = index(i)
        end do
        index(i+1)=indext
      end do
      if (jstack== 0) RETURN
      r = istack(jstack)
      l = istack(jstack-1)
      jstack=jstack-2
    else
      k = (l+r)/2
      call iswap(index(k),index(l+1))
      call icomp_xchg(index(l),index(r))
      call icomp_xchg(index(l+1),index(r))
      call icomp_xchg(index(l),index(l+1))
      i=l+1
      j=r
      indext=index(l+1)
      a=arr(indext)
      do 
        do
          i = i+1
          if ( arr(index(i)) >= a) exit
        end do
        do 
          j = j -1
          if (arr(index(j)) <= a ) exit
        end do
        if ( j < i) exit
        call iswap(index(i),index(j))
      end do
      index(l+1)=index(j)
      index(j)=indext
      jstack=jstack+2
      if (jstack > NSTACK ) then
        write(*,*) ('indexx: NSTACK too small')
        stop
      end if
      if (r-i+1 >= j-l) then
        istack(jstack)=r
        istack(jstack-1)=i
        r=j-1
      else
        istack(jstack)=j-1
        istack(jstack-1)=l
        l=i
      end if
    end if
  end do
contains

  subroutine icomp_xchg(i,j)
    integer(4), intent(inout) :: i, j
    integer(4) :: swp
    if (arr(j) < arr(i) ) then
      swp=i
      i=j
      j=swp
    end if
  end subroutine icomp_xchg
end subroutine indexx


