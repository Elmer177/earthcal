set terminal pngcairo size 800,600
set xlabel "t [min]"
set yrange [0:30]
set grid
set key right center

# v4_epsX.XX.dat : 1 t, 2 S, 3 I_T, 4 I_F, 5 R_T, 6 R_F, 7 T, 8 F, 9 K, 10 合計
epss = "0.00 0.05 0.10 0.20 0.30"

# 真実を知る人 T（のび太くんの判定に使う量）
set output "v4_T.png"
set title "v4: people who know the truth ({/Symbol b}_T = 1, {/Symbol b}_F = 1.5, {/Symbol g} = 1)"
set ylabel "T (people who know the truth)"
set xrange [0:50]
plot for [e in epss] sprintf("v4_eps%s.dat", e) u 1:7 w l lw 3 t sprintf("{/Symbol e} = %s", e), \
     15 w l lw 1.5 lc "gray" t "T = 15"

# 真実 T（実線）とデマ F（破線）の比較
set output "v4_TF.png"
set title "v4: truth T (solid) vs rumor F (dashed)"
set ylabel "people"
set xrange [0:20]
plot for [i=1:5] sprintf("v4_eps%s.dat", word(epss,i)) u 1:7 w l lw 3 lc i t sprintf("{/Symbol e} = %s", word(epss,i)), \
     for [i=1:5] sprintf("v4_eps%s.dat", word(epss,i)) u 1:8 w l lw 2 dt 2 lc i notitle, \
     15 w l lw 1.5 lc "gray" t "15"
