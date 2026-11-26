program main
  implicit none
  integer, parameter :: N = 30
  real(8), parameter :: beta = 1.0d0       ! しずかちゃんが1分あたりに話す人数
  real(8), dimension(3) :: gamarray        ! 飽きやすさγ
  real(8) :: gam
  real(8) :: h, t, p, q
  real(8) :: S, Z, K, K_old, tstar
  logical :: found
  integer :: i, j, nstep
  character(len=30) :: fname
  data gamarray /0.10d0, 0.17d0, 0.30d0/

  h = 0.1d0
  nstep = nint(50.0d0 / h)

  do j = 1, size(gamarray)
    gam = gamarray(j)
    p = beta / N
    q = gam / N
    t = 0.0d0
    S = N
    Z = 1.0d0
    found = .false.

    write(fname, '(A, F4.2, A)') 'v2_gamma', gam, '.dat'
    open(10, file=trim(fname), status='replace')
    write(10,*) t, S, Z, N - S
    do i = 1, nstep
      K_old = N - S
      call step(t, h, S, Z)
      t = i * h
      K = N - S
      if (.not. found .and. K_old < 15.0d0 .and. K >= 15.0d0) then
        tstar = (t - h) + h * (15.0d0 - K_old) / (K - K_old)
        found = .true.
      end if
      write(10,*) t, S, Z, K
    end do
    close(10)

    if (found) then
      write(*,'(A,F5.2,A,F7.3,A,F7.3,A)') 'gamma =', gam, '  K(50) =', N - S, &
           '  t* =', tstar, ' min  -> early leave'
    else
      write(*,'(A,F5.2,A,F7.3,A)') 'gamma =', gam, '  K(50) =', N - S, &
           '  t* = none  -> safe'
    end if
  end do

contains
  ! 微分方程式の右辺 dS/dt
  function f(t, S, Z) result(dS)
    real(8), intent(in) :: t, S, Z
    real(8) :: dS
    dS = -p * Z * S
  end function f

  ! 微分方程式の右辺 dZ/dt
  function g(t, S, Z) result(dZ)
    real(8), intent(in) :: t, S, Z
    real(8) :: dZ
    dZ = -q * Z * (N - S)
  end function g

  ! RK4 の1ステップ（k は S、l は Z の増分。同じ段階の S, Z を使う）
  subroutine step(t, h, S, Z)
    real(8), intent(in)    :: t, h
    real(8), intent(inout) :: S, Z
    real(8) :: k1, k2, k3, k4, l1, l2, l3, l4
    k1 = f(t,       S,        Z       ) * h
    l1 = g(t,       S,        Z       ) * h
    k2 = f(t + h/2, S + k1/2, Z + l1/2) * h
    l2 = g(t + h/2, S + k1/2, Z + l1/2) * h
    k3 = f(t + h/2, S + k2/2, Z + l2/2) * h
    l3 = g(t + h/2, S + k2/2, Z + l2/2) * h
    k4 = f(t + h,   S + k3,   Z + l3  ) * h
    l4 = g(t + h,   S + k3,   Z + l3  ) * h
    S = S + (k1 + 2*k2 + 2*k3 + k4) / 6
    Z = Z + (l1 + 2*l2 + 2*l3 + l4) / 6
  end subroutine step

  ! t = 0 から t = 50 まで刻み h で解き、t = 50 での S, Z を返す（刻み幅の確認用）
  subroutine solve(h, S, Z)
    real(8), intent(in)  :: h
    real(8), intent(out) :: S, Z
    integer :: i, m

    m = nint(50.0d0 / h)
    S = N
    Z = 1.0d0
    do i = 1, m
      call step((i - 1) * h, h, S, Z)
    end do
  end subroutine solve

end program main
