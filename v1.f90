program main
  implicit none
  integer, parameter :: N = 30  
  real(8), parameter :: beta = 1.0d0
  real(8) :: h, t ,p
  integer :: i, j

  p = N/beta
  h = 0.1d0
  t = 0.0d0
  s = 0.0d0
  N=30

contains
  ! 解析解
  function Sexact(t)
    real(8), intent(in) :: t
    real(8) :: Sexact
    Sexact = N * (exp(-p*t)) 
  end function xexact

  ! 微分方程式の右辺
function f(t, S) result(dS)
  real(8), intent(in) :: t, S
  real(8), intent(out) :: dS
  dS = -p * S
end function f

  subroutine step(h, x)
    real(8), intent(in) :: h
    real(8), intent(inout) :: x
    real(8) :: k1, k2, k3, k4

    k1 = f(x) * h
    k2 = f(x + k1/2) * h
    k3 = f(x + k2/2) * h
    k4 = f(x + k3) * h
    x = x + (k1 + 2*k2 + 2*k3 + k4) / 6
  end subroutine step

  subroutine solve(h, x)
    real(8), intent(in) :: h
    real(8), intent(out) :: x
    integer :: i, n

    n = nint(1.0d0 / h)
    x = 1.0d0
    do i = 1, n
      call step(h, x)
    end do
  end subroutine solve

end program main
