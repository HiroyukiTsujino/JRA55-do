! -*-F90-*-
!
!--------------------------- hintpl.F90 -------------------------------
! Information:
!
!     hintpl: 水平二次元格子点データをモデル格子に補間する
!
subroutine hintpl(datout,imx,jmx,alono,alato,&
     &            datin,imxi,jmxi,aloni,alati)

  implicit none

  ! datout: モデル格子に補間したデータを格納する配列
  ! モデルの格子配置(経度・緯度)

  integer(4), intent(in) :: imx, jmx
  real(8), intent(out) :: datout(imx,jmx)
  real(8), intent(in) :: alono(imx), alato(jmx)

  ! datin: 入力データ
  ! aloni, alati: 入力データの格子配置(経度・緯度)

  integer(4), intent(in) :: imxi, jmxi
  real(8), intent(in) :: datin(imxi,jmxi)
  real(8), intent(in) :: aloni(imxi), alati(jmxi)

  integer(4) :: iss, jss, issp, jssp
  real(8) :: ssi, ssj, ssic, ssjc
  integer(4) :: i, j, ii, jj

  !-------------------------------------------------------------------
  !
  do j = 1, jmx
    do i = 1, imx
      !
      jss = jmxi
      do jj = 1, jmxi
        if ( alato(j)<=alati(jj) ) then
          jss = jj - 1
          exit
        end if
      end do
      jssp = jss + 1
      if ( jss < 1 ) jss = 1
      if ( jssp > jmxi ) jssp = jmxi
      if ( jss == jssp ) then
        ssj = 1.0d0
      else
        ssj = (alato(j)-alati(jss)) / (alati(jssp)-alati(jss))
      end if
      ssjc = 1.0d0 - ssj
      !
      iss = imxi
      do ii = 1, imxi
        if ( alono(i) <= aloni(ii) ) then
          iss = ii - 1
          exit
        end if
      end do
      issp = iss + 1

      if ( iss < 1 ) then
        iss = imxi
        ssi = (alono(i)-(aloni(iss)-360.d0)) / (aloni(issp)-(aloni(iss)-360.d0))
      else if ( issp > imxi ) then
        issp = 1
        ssi = (alono(i)-aloni(iss)) / (aloni(issp)+360.d0-aloni(iss))
      else
        ssi = (alono(i)-aloni(iss)) / (aloni(issp)-aloni(iss))
      endif
      ssic = 1.0d0 - ssi
      !
      datout(i,j) = ssic * ssjc * datin(iss,jss)&
           & + ssi * ssjc * datin(issp,jss)&
           & + ssic * ssj * datin(iss,jssp)&
           & + ssi * ssj * datin(issp,jssp)
      !
    end do
  end do
  !
end subroutine hintpl
