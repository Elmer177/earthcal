set terminal pngcairo size 800,600
set xlabel "t [min]"
set ylabel "K (people who know)"
set yrange [0:30]
set grid
set key right bottom

# v3_gammaX.XX.dat : 1列目 t, 2列目 S, 3列目 I, 4列目 R, 5列目 K, 6列目 S+I+R
gams = "0.10 0.17 0.30 1.00 2.00 3.00"

# 全体（0〜50分）
set output "v3.png"
set title "v3: everyone spreads ({/Symbol b} = 1)"
set xrange [0:50]
plot for [g in gams] sprintf("v3_gamma%s.dat", g) u 1:5 w l lw 3 t sprintf("{/Symbol g} = %s", g), \
     15 w l lw 1.5 lc "gray" t "K = 15"

# 立ち上がりの拡大（0〜10分）
set output "v3_zoom.png"
set title "v3: first 10 minutes ({/Symbol b} = 1)"
set xrange [0:10]
replot
