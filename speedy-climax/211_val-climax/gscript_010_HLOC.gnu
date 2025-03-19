 set terminal tgif
 set terminal postscript eps enhanced color solid "Helvetica" 32
#  set terminal postscript eps "Helvetica" 32
 set size 1.3
# set size ratio 0.8 1.5
 set size ratio 0.7 2.0

set size ratio 0.5 2.0
#################################################### 
# climax variables
# levels: 925 850 700 600 500 250 50
########################## 
#  01    ; header, yyyymmddhh
#  02    ; header, TIMER (integer: 1,2,3,)
#  03:09 ; u (925 850 700 600 500 250 50)
#  11:16 ; v (925 850 700 600 500 250 50)
#  17:23 ; t (925 850 700 600 500 250 50)
#  24:30 ; q (925 850 700 600 500 250 50) [g/kg] coverted in 
#  31:37 ; gph (925 850 700 600 500 250 50) height [m]
#  38    ; T2m
#  39    ; U10m
#  40    ; V10m
#  41    ; Ps

###########################################################################################################################################################
set xlabel "date" font "Helvetica,38" offset 0.5,0
###########################################################################################################################################################
set xrange [1:1460.0] 
set xtics ('2017/01/01'1, '03/01'239, '05/01'481, '07/01'725, '09/01'973, '11/01'1217, '12/31'1457 ) font "Helvetica,33"
###########################################################################################################################################################
set key right bottom
set title  "(a) [AN] RMSE (solid) & Spread (dashed) ; Temperature [K] at 500 hPa" font "Helvetica,38"
set ylabel "RMSE, Spread [K]"   font "Helvetica,38" offset 0.5,0
set yrange [0.3:3.0] ; set ytics 0, 0.3
set output "./fig_010_time/time_raob_wb_rmse_T500_def.eps"
plot \
  'gnu_000/anal_rmse_time_raob_wb_CLIMAXdef-0LETKF_M000020L0400IADP_RTPx0.00_woWSM_glb.txt'   u 1:21 w l lw 0 lt 1 lc rgb "gray20"           title "",\
  'gnu_000/anal_rmse_time_raob_wb_CLIMAXdef-0LETKF_M000020L0500IADP_RTPx0.00_woWSM_glb.txt'   u 1:21 w l lw 0 lt 1 lc rgb "dark-magenta"     title "",\
  'gnu_000/anal_rmse_time_raob_wb_CLIMAXdef-0LETKF_M000020L0600IADP_RTPx0.00_woWSM_glb.txt'   u 1:21 w l lw 0 lt 1 lc rgb "blue"             title "",\
  'gnu_000/anal_rmse_time_raob_wb_CLIMAXdef-0LETKF_M000020L0700IADP_RTPx0.00_woWSM_glb.txt'   u 1:21 w l lw 0 lt 1 lc rgb "dark-green"       title "",\
  'gnu_000/anal_rmse_time_raob_wb_CLIMAXdef-0LETKF_M000020L0800IADP_RTPx0.00_woWSM_glb.txt'   u 1:21 w l lw 0 lt 1 lc rgb "red"              title "",\
  'gnu_000/anal_sprd_time_raob_wb_CLIMAXdef-0LETKF_M000020L0400IADP_RTPx0.00_woWSM_glb.txt'   u 1:21 w l lw 3 lt 0 lc rgb "gray20"           title "",\
  'gnu_000/anal_sprd_time_raob_wb_CLIMAXdef-0LETKF_M000020L0500IADP_RTPx0.00_woWSM_glb.txt'   u 1:21 w l lw 3 lt 0 lc rgb "dark-magenta"     title "",\
  'gnu_000/anal_sprd_time_raob_wb_CLIMAXdef-0LETKF_M000020L0600IADP_RTPx0.00_woWSM_glb.txt'   u 1:21 w l lw 3 lt 0 lc rgb "blue"             title "",\
  'gnu_000/anal_sprd_time_raob_wb_CLIMAXdef-0LETKF_M000020L0700IADP_RTPx0.00_woWSM_glb.txt'   u 1:21 w l lw 3 lt 0 lc rgb "dark-green"       title "",\
  'gnu_000/anal_sprd_time_raob_wb_CLIMAXdef-0LETKF_M000020L0800IADP_RTPx0.00_woWSM_glb.txt'   u 1:21 w l lw 3 lt 0 lc rgb "red"              title "",\
  'gnu_000/anal_rmse_trun_raob_wb_CLIMAXdef-0LETKF_M000020L0400IADP_RTPx0.00_woWSM_glb.txt'   u 1:21 w l lw 5 lt 1 lc rgb "gray20"           title "L_h:400km",\
  'gnu_000/anal_rmse_trun_raob_wb_CLIMAXdef-0LETKF_M000020L0500IADP_RTPx0.00_woWSM_glb.txt'   u 1:21 w l lw 5 lt 1 lc rgb "dark-magenta"     title "L_h:500km",\
  'gnu_000/anal_rmse_trun_raob_wb_CLIMAXdef-0LETKF_M000020L0600IADP_RTPx0.00_woWSM_glb.txt'   u 1:21 w l lw 5 lt 1 lc rgb "blue"             title "L_h:600km",\
  'gnu_000/anal_rmse_trun_raob_wb_CLIMAXdef-0LETKF_M000020L0700IADP_RTPx0.00_woWSM_glb.txt'   u 1:21 w l lw 5 lt 1 lc rgb "dark-green"       title "L_h:700km",\
  'gnu_000/anal_rmse_trun_raob_wb_CLIMAXdef-0LETKF_M000020L0800IADP_RTPx0.00_woWSM_glb.txt'   u 1:21 w l lw 5 lt 1 lc rgb "red"              title "L_h:800km"
###########################################################################################################################################################
set key right bottom
set title  "(b) [AN] RMSE (solid) & Spread (dashed) ; Geopotential Height [m] at 500 hPa" font "Helvetica,38"
set ylabel "RMSE, Spread [m]"   font "Helvetica,38" offset 0.5,0
set yrange [5:70.0]; set ytics 0, 10
set output "./fig_010_time/time_raob_wb_rmse_Z500_def.eps"
plot \
  'gnu_000/anal_rmse_time_raob_wb_CLIMAXdef-0LETKF_M000020L0400IADP_RTPx0.00_woWSM_glb.txt'   u 1:($35/9.81) w l lw 0 lt 1 lc rgb "gray20"           title "",\
  'gnu_000/anal_rmse_time_raob_wb_CLIMAXdef-0LETKF_M000020L0500IADP_RTPx0.00_woWSM_glb.txt'   u 1:($35/9.81) w l lw 0 lt 1 lc rgb "dark-magenta"     title "",\
  'gnu_000/anal_rmse_time_raob_wb_CLIMAXdef-0LETKF_M000020L0600IADP_RTPx0.00_woWSM_glb.txt'   u 1:($35/9.81) w l lw 0 lt 1 lc rgb "blue"             title "",\
  'gnu_000/anal_rmse_time_raob_wb_CLIMAXdef-0LETKF_M000020L0700IADP_RTPx0.00_woWSM_glb.txt'   u 1:($35/9.81) w l lw 0 lt 1 lc rgb "dark-green"       title "",\
  'gnu_000/anal_rmse_time_raob_wb_CLIMAXdef-0LETKF_M000020L0800IADP_RTPx0.00_woWSM_glb.txt'   u 1:($35/9.81) w l lw 0 lt 1 lc rgb "red"              title "",\
  'gnu_000/anal_sprd_time_raob_wb_CLIMAXdef-0LETKF_M000020L0400IADP_RTPx0.00_woWSM_glb.txt'   u 1:($35/9.81) w l lw 3 lt 0 lc rgb "gray20"           title "",\
  'gnu_000/anal_sprd_time_raob_wb_CLIMAXdef-0LETKF_M000020L0500IADP_RTPx0.00_woWSM_glb.txt'   u 1:($35/9.81) w l lw 3 lt 0 lc rgb "dark-magenta"     title "",\
  'gnu_000/anal_sprd_time_raob_wb_CLIMAXdef-0LETKF_M000020L0600IADP_RTPx0.00_woWSM_glb.txt'   u 1:($35/9.81) w l lw 3 lt 0 lc rgb "blue"             title "",\
  'gnu_000/anal_sprd_time_raob_wb_CLIMAXdef-0LETKF_M000020L0700IADP_RTPx0.00_woWSM_glb.txt'   u 1:($35/9.81) w l lw 3 lt 0 lc rgb "dark-green"       title "",\
  'gnu_000/anal_sprd_time_raob_wb_CLIMAXdef-0LETKF_M000020L0800IADP_RTPx0.00_woWSM_glb.txt'   u 1:($35/9.81) w l lw 3 lt 0 lc rgb "red"              title "",\
  'gnu_000/anal_rmse_trun_raob_wb_CLIMAXdef-0LETKF_M000020L0400IADP_RTPx0.00_woWSM_glb.txt'   u 1:($35/9.81) w l lw 5 lt 1 lc rgb "gray20"           title "L_h:400km",\
  'gnu_000/anal_rmse_trun_raob_wb_CLIMAXdef-0LETKF_M000020L0500IADP_RTPx0.00_woWSM_glb.txt'   u 1:($35/9.81) w l lw 5 lt 1 lc rgb "dark-magenta"     title "L_h:500km",\
  'gnu_000/anal_rmse_trun_raob_wb_CLIMAXdef-0LETKF_M000020L0600IADP_RTPx0.00_woWSM_glb.txt'   u 1:($35/9.81) w l lw 5 lt 1 lc rgb "blue"             title "L_h:600km",\
  'gnu_000/anal_rmse_trun_raob_wb_CLIMAXdef-0LETKF_M000020L0700IADP_RTPx0.00_woWSM_glb.txt'   u 1:($35/9.81) w l lw 5 lt 1 lc rgb "dark-green"       title "L_h:700km",\
  'gnu_000/anal_rmse_trun_raob_wb_CLIMAXdef-0LETKF_M000020L0800IADP_RTPx0.00_woWSM_glb.txt'   u 1:($35/9.81) w l lw 5 lt 1 lc rgb "red"              title "L_h:800km"
