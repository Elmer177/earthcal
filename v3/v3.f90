program main
  implicit none
  integer, parameter :: N = 30
  integer, parameter :: M = 3              ! 変数の数：y(1)=S, y(2)=I, y(3)=R
  real(8), parameter :: beta = 1.0d0       ! 広める人が1分あたりに話す人数
  real(8), dimension(6) :: gamarray        ! 飽きやすさγ
  real(8) :: gam
  real(8) :: h, t, p, q
  real(8) :: y(M)
  real(8) :: K, K_old, tstar
  logical :: found
  integer :: i, j, nstep
  character(len=30) :: fname
  data gamarray /0.10d0, 0.17d0, 0.30d0, 1.00d0, 2.00d0, 3.00d0/

  h = 0.1d0
  nstep = nint(50.0d0 / h)

  do j = 1, size(gamarray)
    gam = gamarray(j)
    p = beta / N
    q = gam / N
    t = 0.0d0
    y = (/ dble(N), 1.0d0, 0.0d0 /)        ! S = 30, I = 1（しずかちゃん）, R = 0
    found = .false.

    write(fname, '(A, F4.2, A)') 'v3_gamma', gam, '.dat'
    open(10, file=trim(fname), status='replace')
    write(10,*) t, y, N - y(1), sum(y)
    do i = 1, nstep
      K_old = N - y(1)
      call step(t, h, y)
      t = i * h
      K = N - y(1)
      ! K が 15 をまたいだら線形補間で到達時刻を求める
      if (.not. found .and. K_old < 15.0d0 .and. K >= 15.0d0) then
        tstar = (t - h) + h * (15.0d0 - K_old) / (K - K_old)
        found = .true.
      end if
      write(10,*) t, y, K, sum(y)         ! 最後の列 S+I+R は 31 のままなら正しい
    end do
    close(10)

    if (found) then
      write(*,'(A,F5.2,A,F7.3,A,F7.3,A)') 'gamma =', gam, '  K(50) =', N - y(1), &
           '  t* =', tstar, ' min  -> early leave'
    else
      write(*,'(A,F5.2,A,F7.3,A)') 'gamma =', gam, '  K(50) =', N - y(1), &
           '  t* = none  -> safe'
    end if
  end do

contains
  ! 微分方程式の右辺 dy/dt（y = (S, I, R)）
  function f(t, y) result(dy)
    real(8), intent(in) :: t, y(M)
    real(8) :: dy(M)
    real(8) :: spread, bored
    spread = p * y(1) * y(2)               ! 聞いて広め始める（S → I）
    bored  = q * y(2) * (N - y(1))         ! 白けてやめる（I → R）
    dy(1) = -spread
    dy(2) =  spread - bored
    dy(3) =  bored
  end function f

  ! RK4 の1ステップ（配列なので変数がいくつでも同じ形）
  subroutine step(t, h, y)
    real(8), intent(in)    :: t, h
    real(8), intent(inout) :: y(M)
    real(8) :: k1(M), k2(M), k3(M), k4(M)
    k1 = f(t,       y       ) * h
    k2 = f(t + h/2, y + k1/2) * h
    k3 = f(t + h/2, y + k2/2) * h
    k4 = f(t + h,   y + k3  ) * h
    y = y + (k1 + 2*k2 + 2*k3 + k4) / 6
  end subroutine step

  ! t = 0 から t = 50 まで刻み h で解き、t = 50 での y を返す（刻み幅の確認用）
  subroutine solve(h, y)
    real(8), intent(in)  :: h
    real(8), intent(out) :: y(M)
    integer :: i, m2

    m2 = nint(50.0d0 / h)
    y = (/ dble(N), 1.0d0, 0.0d0 /)
    do i = 1, m2
      call step((i - 1) * h, h, y)
    end do
  end subroutine solve

end program main
