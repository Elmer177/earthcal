program main
  implicit none
  integer, parameter :: N = 30  
  real(8), dimension(3) :: betaarray
  real(8) :: beta
  real(8) :: h, t ,p
  real(8) :: S
  integer :: i, j, nstep
  character(len=30) :: fname
  data betaarray /1.0d0, 2.0d0, 3.0d0/

  h = 0.1d0
  nstep = nint(50.0d0 / h)

  do j = 1, size(betaarray)
    beta = betaarray(j)
    p = beta / N
    t = 0.0d0
    S = N

    write(fname, '(A, F3.1, A)') 'v1_beta', beta, '.dat'
    open(10, file=trim(fname), status='replace')
    write(10,*) t, S, Sexact(t)
    do i = 1, nstep
      call step(t, h, S)
      t = i * h
      write(10,*) t, S, Sexact(t)
    end do
    close(10)

    write(*,*) 'beta =', beta, '  K(50) =', N - S, '  -> ', trim(fname)
  end do
    
contains
  ! 解析解
  function Sexact(t)
    real(8), intent(in) :: t
    real(8) :: Sexact
    Sexact = N * (exp(-p*t)) 
  end function Sexact

  ! 微分方程式の右辺
function f(t, S) result(dS)
  real(8), intent(in) :: t, S
  real(8) :: dS
  dS = -p * S
end function f

  subroutine step(t, h, S)
    real(8), intent(in)    :: t, h
    real(8), intent(inout) :: S
    real(8) :: k1, k2, k3, k4
    k1 = f(t,       S       ) * h
    k2 = f(t + h/2, S + k1/2) * h
    k3 = f(t + h/2, S + k2/2) * h
    k4 = f(t + h,   S + k3  ) * h
    S = S + (k1 + 2*k2 + 2*k3 + k4) / 6
  end subroutine step

  subroutine solve(h, S)
    real(8), intent(in) :: h
    real(8), intent(out) :: S
    integer :: i, m

    m = nint(50.0d0 / h)
    S = N
    do i = 1, m
      call step((i - 1) * h, h, S)
    end do
  end subroutine solve

end program main
