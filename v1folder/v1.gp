set terminal pngcairo size 800,600
set output "v1.png"

set xlabel "t [min]"
set ylabel "K (people who know)"
set xrange [0:50]
set yrange [0:30]
set grid
set key right bottom
set title "v1: Shizuka keeps talking"

# v1_betaX.X.dat : 1列目 t, 2列目 S (RK4), 3列目 S_exact
# K = 30 - S なので、($2) と ($3) から計算して描く
betas = "1.0 2.0 3.0"
plot for [b in betas] sprintf("v1_beta%s.dat", b) u 1:(30-$2) w l lw 3 t sprintf("{/Symbol b} = %s (RK4)", b), \
     for [b in betas] sprintf("v1_beta%s.dat", b) u 1:(30-$3) w l lw 1.5 dt 2 lc "black" notitle, \
     15 w l lw 1.5 lc "gray" t "K = 15"
