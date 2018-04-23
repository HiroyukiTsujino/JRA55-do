! -*-F90-*-
!
!     MRI.COM(気象研究所統合海洋モデル) hintpl.F90
!         Copyright 2001-2003 Oceanographic Research Dept., MRI-JMA
!
!     2002-08-16 作成(石川)
!
!======================================================================
!
!     hintpl: 水平二次元格子点データをモデル格子に補間する
!  --- the two data set must be base on the same coordinate system ---
!
subroutine hintpl(datout,imut,jmut,lono,lato, &
     &            datin,imuti,jmuti,aloni,alati,&
     &            intmod)
  !
  implicit none
  !
  ! datout: モデル格子に補間したデータを格納する配列
  !
  integer(4), intent(in) :: imut, jmut
  real(8), intent(in) :: lono(imut), lato(jmut)
  !
  real(8), intent(out) :: datout(imut,jmut)
  !
  ! datin: 入力データ
  ! aloni, alati: 入力データの格子配置(経度・緯度)
  ! intmod: 補間の方法(1:線型、2:3次スプライン)
  !
  integer(4), intent(in) :: imuti, jmuti, intmod
  real(4), intent(in) :: datin(imuti,jmuti)
  real(8), intent(in) :: aloni(imuti), alati(jmuti)

  real(8) :: alono(imut,jmut), alato(imut,jmut) ! モデルの格子配置(経度・緯度)
  !
  real(8), allocatable :: datme(:), datmc(:)
  real(8), allocatable :: datz2(:), datm2(:,:)
  real(8), allocatable :: wk1(:), wk2(:), buf(:)
  real(8) :: xpos, ypos, yposo
  real(8) :: xdat, ydat
  real(8), parameter :: rbig=9.99d33
  !
  integer(4) :: iss, jss, issp, jssp
  real(8) :: ssi, ssj, ssic, ssjc
  integer(4) :: i, j, ii, jj, ic, jc
  !
  do i = 1, imut
    alato(i,1:jmut) = lato(1:jmut)
  end do
  do j = 1, jmut
    alono(1:imut,j) = lono(1:imut)
  end do
  !
  IF_LINEAR: if ( intmod == 1 ) then
    !
    ! 線型補間
    !
    do j = 1, jmut
      do i = 1, imut

        jss = jmuti
        do jj = 1, jmuti
          if ( alato(i,j)<=alati(jj) ) then
            jss = jj - 1
            exit
          end if
        end do
        jssp = jss + 1
        if ( jss < 1 ) jss = 1
        if ( jssp > jmuti ) jssp = jmuti
        if ( jss == jssp ) then
          ssj = 1.
        else
          ssj = (alato(i,j)-alati(jss)) / (alati(jssp)-alati(jss))
        end if
        ssjc = 1.D0 - ssj

        iss = imuti
        do ii = 1, imuti
          if ( alono(i,j)<=aloni(ii) ) then
            iss = ii - 1
            exit
          end if
        end do
        issp = iss + 1
#ifdef OGCM_CYCLIC
        if ( iss < 1 ) iss = iss + imuti
        if ( issp > imuti ) issp = issp - imuti
#else /* OGCM_CYCLIC */
        if ( iss < 1 ) iss = 1
        if ( issp > imuti ) issp = imuti
#endif /* OGCM_CYCLIC */
        if ( iss == issp ) then
          ssi = 1.
#ifdef OGCM_CYCLIC
        else if ( alono(i,j) < aloni(1) ) then
          ssi = (alono(i,j)+360.-aloni(iss)) / (aloni(issp)+360.-aloni(iss))
        else if ( alono(i,j) > aloni(imuti) ) then
          ssi = (alono(i,j)-aloni(iss)) / (aloni(issp)+360.-aloni(iss))
#endif /* OGCM_CYCLIC */
        else
          ssi = (alono(i,j)-aloni(iss)) / (aloni(issp)-aloni(iss))
        endif
        ssic = 1.D0 - ssi

        datout(i,j) = ssic * ssjc * dble(datin(iss,jss))&
             & + ssi * ssjc * dble(datin(issp,jss))&
             & + ssic * ssj * dble(datin(iss,jssp))&
             & + ssi * ssj * dble(datin(issp,jssp))

      end do
    end do

  else if ( intmod == 2 ) then ! IF_LINEAR
    !
    ! 3次スプライン関数による補間( by H. Tsujino )
    !
    allocate ( datme(1:imuti) )
    allocate ( datmc(1:jmuti) )
    allocate ( datz2(1:imuti) )
    allocate ( datm2(1:imuti,1:jmuti) )
    allocate ( buf(1:jmuti) )
    allocate ( wk1(1:jmuti) )
    allocate ( wk2(1:imuti) )
    !
    ! 1: interpolation in the meridional direction
    !
    do ic = 1, imuti
      do jc = 1, jmuti
        datmc(jc) = datin(ic,jc)
      end do
      call spline(alati,datmc,jmuti,rbig,rbig,buf,wk1)
      do jc = 1, jmuti
        datm2(ic,jc) = buf(jc)
      end do
    end do
    !
    ! 2: intperpolation in the zonal direction
    !
    yposo=rbig
    do j = 1, jmut
      do i = 1, imut
        ypos = alato(i,j)
        if ( ypos /= yposo ) then
          do ic = 1, imuti
            do jc = 1, jmuti
              buf(jc) = datm2(ic,jc)
              datmc(jc) = datin(ic,jc)
            end do
            call splint(alati,datmc,buf,jmuti,ypos,ydat)
            datme(ic) = ydat
          end do
          call spline(aloni,datme,imuti,rbig,rbig,datz2,wk2)
        end if
        xpos = alono(i,j)
        call splint(aloni,datme,datz2,imuti,xpos,xdat)
        datout(i,j) = xdat
        yposo = ypos
      end do
    end do

    deallocate ( datme )
    deallocate ( datmc )
    deallocate ( datz2 )
    deallocate ( datm2 )
    deallocate ( buf )
    deallocate ( wk1 )
    deallocate ( wk2 )

  else
    
    print *, 'hintpl: wrong value of intmod = ', intmod
    stop

  end if IF_LINEAR
  !
end subroutine hintpl
!
!======================================================================
!
!     spline: 入力データから3次のスプライン関数を求める
!
!  (C) Copr. 1986-92 Numerical Recipes Software -k.
!
subroutine spline(x,y,n,yp1,ypn,y2,u)
  !
  implicit none

  integer(4),intent(in) :: n
  !  y = f(x): 入力データ
  !  yp1, ypn: n=1, N での一階微係数
  !  y2 = f''(x) : 出力
  real(8),intent(in) :: x(n),y(n),yp1,ypn
  real(8),intent(out) :: y2(n),u(n)
  integer(4) :: i,k
  real(8)    :: p,qn,sig,un
  !
  !--------------------------------------------------------------------
  !

  if (yp1 > 0.99d30) then
    y2(1) = 0.d0
    u(1) = 0.d0
  else
    y2(1) = -0.5d0
    u(1) = (3.d0/(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
  end if
  do i = 2, n-1
    sig = (x(i) - x(i-1))/(x(i+1) - x(i-1))
    p = sig * y2(i-1) + 2.d0
    y2(i) = (sig - 1.d0) / p
    u(i) = (6.d0 * ((y(i+1) - y(i)) / (x(i+1) - x(i)) &
         &      - (y(i) - y(i-1)) / (x(i) - x(i-1))) &
         &       / (x(i+1) - x(i-1)) - sig * u(i-1)) / p
  end do
  if (ypn > 0.99d30) then
    qn = 0.d0
    un = 0.d0
  else
    qn = 0.5d0
    un = (3.d0 / (x(n) - x(n-1))) &
         & * (ypn - (y(n) - y(n-1)) / (x(n) - x(n-1)))
  end if
  y2(n) = (un - qn * u(n-1)) / (qn * y2(n-1) + 1.d0)
  do k = n-1, 1, -1
    y2(k) = y2(k) * y2(k+1) + u(k)
  end do
  !
end subroutine spline
!
!======================================================================
!
!     splint: 3次のスプライン関数を用いて格子点値を求める
!
!  (C) Copr. 1986-92 Numerical Recipes Software -k.
!
subroutine splint(xa,ya,y2a,n,x,y)
  !
  integer(4), intent(in) :: n
  !  ya = f(xa): 入力データ
  !  y2a = f''(xa) : spline の結果
  !  x : 求める格子点の位置
  !  y : 求める格子点値(出力)
  real(8), intent(in) :: xa(n),ya(n),y2a(n),x
  real(8), intent(out) :: y
  integer(4)  :: k,khi,klo
  real(8)     :: a,b,h
  !----------------------------------------------------------------
  klo = 1
  khi = n
1 if (khi - klo > 1) then
    k = (khi + klo) / 2
    if (xa(k) > x) then
      khi = k
    else
      klo = k
    endif
    goto 1
  end if
  h = xa(khi) - xa(klo)
  if (h == 0.d0) pause 'bad xa input in splint'
  a = (xa(khi) - x)/h
  b = (x - xa(klo))/h
  y = a * ya(klo) + b * ya(khi) &
       &     +((a**3 - a) * y2a(klo) + &
       &       (b**3 - b) * y2a(khi)) * (h**2) / 6.d0
end subroutine splint
