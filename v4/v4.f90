program main
  implicit none
  integer, parameter :: N = 30
  integer, parameter :: M = 5              ! y(1)=S, y(2)=I_T, y(3)=I_F, y(4)=R_T, y(5)=R_F
  real(8), parameter :: betaT = 1.0d0      ! 真実を広める人が1分あたりに話す人数
  real(8), parameter :: betaF = 1.5d0      ! デマを広める人が1分あたりに話す人数（盛った話は広まりやすい）
  real(8), parameter :: gam   = 1.0d0      ! 飽きやすさγ（真実・デマ共通）
  real(8), dimension(5) :: epsarray        ! 話が盛られる確率ε
  real(8) :: eps
  real(8) :: h, t
  real(8) :: y(M)
  real(8) :: KT, KF, KT_old, tstar   ! KT：真実を知る人 T、KF：デマを信じる人 F（Fortranは大文字小文字を区別しないので t, f と別名にする）
  logical :: found
  integer :: i, j, nstep
  character(len=30) :: fname
  data epsarray /0.00d0, 0.05d0, 0.10d0, 0.20d0, 0.30d0/

  h = 0.1d0
  nstep = nint(50.0d0 / h)

  do j = 1, size(epsarray)
    eps = epsarray(j)
    t = 0.0d0
    y = (/ dble(N), 1.0d0, 0.0d0, 0.0d0, 0.0d0 /)   ! S = 30, I_T = 1（しずかちゃん）
    found = .false.

    write(fname, '(A, F4.2, A)') 'v4_eps', eps, '.dat'
    open(10, file=trim(fname), status='replace')
    call count(y, KT, KF)
    write(10,*) t, y, KT, KF, N - y(1), sum(y)
    do i = 1, nstep
      KT_old = KT
      call step(t, h, y)
      t = i * h
      call count(y, KT, KF)
      ! 真実を知る人 T が 15 をまたいだら線形補間で到達時刻を求める
      if (.not. found .and. KT_old < 15.0d0 .and. KT >= 15.0d0) then
        tstar = (t - h) + h * (15.0d0 - KT_old) / (KT - KT_old)
        found = .true.
      end if
      write(10,*) t, y, KT, KF, N - y(1), sum(y)   ! 最後の列は 31 のままなら正しい
    end do
    close(10)

    if (found) then
      write(*,'(A,F5.2,A,F7.3,A,F7.3,A,F7.3,A)') 'eps =', eps, '  T(50) =', KT, &
           '  F(50) =', KF, '  t* =', tstar, ' min  -> early leave'
    else
      write(*,'(A,F5.2,A,F7.3,A,F7.3,A)') 'eps =', eps, '  T(50) =', KT, &
           '  F(50) =', KF, '  t* = none  -> safe'
    end if
  end do

contains
  ! クラスメイト30人のうち、真実を知る人 T とデマを信じる人 F
  ! （しずかちゃんは I_T か R_T に入っているので 1 を引く）
  subroutine count(y, KT, KF)
    real(8), intent(in)  :: y(M)
    real(8), intent(out) :: KT, KF
    KT = y(2) + y(4) - 1.0d0
    KF = y(3) + y(5)
  end subroutine count

  ! 微分方程式の右辺 dy/dt
  function f(t, y) result(dy)
    real(8), intent(in) :: t, y(M)
    real(8) :: dy(M)
    real(8) :: hearT, hearF, boredT, boredF
    hearT  = betaT / N * y(1) * y(2)        ! 真実を広める人から聞く
    hearF  = betaF / N * y(1) * y(3)        ! デマを広める人から聞く
    boredT = gam / N * y(2) * (N - y(1))    ! 真実を広める人が白ける（I_T → R_T）
    boredF = gam / N * y(3) * (N - y(1))    ! デマを広める人が白ける（I_F → R_F）
    dy(1) = -hearT - hearF
    dy(2) = (1.0d0 - eps) * hearT - boredT          ! 正しく伝わる
    dy(3) = eps * hearT + hearF - boredF            ! 盛られる＋デマのまま伝わる
    dy(4) = boredT
    dy(5) = boredF
  end function f

  ! RK4 の1ステップ（v3 と同じ）
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
    y = (/ dble(N), 1.0d0, 0.0d0, 0.0d0, 0.0d0 /)
    do i = 1, m2
      call step((i - 1) * h, h, y)
    end do
  end subroutine solve

end program main
