!-*-F90-*-
real(8) function qsat(y)

  implicit none

  real(8),intent(in) :: y(2)
  real(8) :: x, p, es

  x=y(1) !temp
  p=y(2) !pressure
  es=6.112*exp(17.502*x/(x+241.0))*(1.0007+3.46e-6*p)
  qsat=es*622./(p-.378*es)

end function qsat
