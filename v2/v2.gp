set terminal pngcairo size 800,600
set output "v2.png"

set xlabel "t [min]"
set ylabel "K (people who know)"
set xrange [0:50]
set yrange [0:30]
set grid
set key right bottom
set title "v2: Shizuka gets bored ({/Symbol b} = 1)"

# v2_gammaX.XX.dat : 1列目 t, 2列目 S, 3列目 Z, 4列目 K
gams = "0.10 0.17 0.30"
plot for [g in gams] sprintf("v2_gamma%s.dat", g) u 1:4 w l lw 3 t sprintf("{/Symbol g} = %s", g), \
     15 w l lw 1.5 lc "gray" t "K = 15"
