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
set xtics ('2016/01/01'1, '03/01'241, '05/01'485, '07/01'729, '09/01'977, '11/01'1221, '2017/01/01'1465 ) font "Helvetica,33"
set xrange [1:2924.0] 
set xtics ('2016/01/01'1, '05/01'485, '09/01'977, '2017/01/01'1465, '05/01'1945, '09/01'2437, '01/01'2925 ) font "Helvetica,33"
###########################################################################################################################################################
set key right bottom
set title  "(a) [AN] RMSE (solid) & Spread (dashed) ; Temperature [K] at 500 hPa" font "Helvetica,38"
set ylabel "RMSE, Spread [K]"   font "Helvetica,38" offset 0.5,0
###set yrange [0.7:2.6] ; set ytics 0, 0.3
###
set output "./fig_010_time/time_prepbufr_rmse_T500_RTPP_int.eps"
plot \
  'gnu_000/anal_sprd_time_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP0.80_ad0.00_po0.00_glb.txt'   u 1:21 w l lw 2 lt 0 lc rgb "gray20"           title "",\
  'gnu_000/anal_sprd_time_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP0.85_ad0.00_po0.00_glb.txt'   u 1:21 w l lw 2 lt 0 lc rgb "dark-magenta"     title "",\
  'gnu_000/anal_sprd_time_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP0.90_ad0.00_po0.00_glb.txt'   u 1:21 w l lw 2 lt 0 lc rgb "blue"             title "",\
  'gnu_000/anal_sprd_time_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP0.95_ad0.00_po0.00_glb.txt'   u 1:21 w l lw 2 lt 0 lc rgb "dark-green"       title "",\
  'gnu_000/anal_sprd_time_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP1.00_ad0.00_po0.00_glb.txt'   u 1:21 w l lw 2 lt 0 lc rgb "red"              title "",\
  'gnu_000/anal_rmse_trun_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP0.80_ad0.00_po0.00_glb.txt'   u 1:21 w l lw 5 lt 1 lc rgb "gray20"           title "RTPP:0.80",\
  'gnu_000/anal_rmse_trun_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP0.85_ad0.00_po0.00_glb.txt'   u 1:21 w l lw 5 lt 1 lc rgb "dark-magenta"     title "RTPP:0.85",\
  'gnu_000/anal_rmse_trun_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP0.90_ad0.00_po0.00_glb.txt'   u 1:21 w l lw 5 lt 1 lc rgb "blue"             title "RTPP:0.90",\
  'gnu_000/anal_rmse_trun_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP0.95_ad0.00_po0.00_glb.txt'   u 1:21 w l lw 5 lt 1 lc rgb "dark-green"       title "RTPP:0.95",\
  'gnu_000/anal_rmse_trun_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP1.00_ad0.00_po0.00_glb.txt'   u 1:21 w l lw 5 lt 1 lc rgb "red"              title "RTPP:1.00"
###
set output "./fig_010_time/time_prepbufr_rmse_T500_RTPS_int.eps"
plot \
  'gnu_000/anal_sprd_time_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS0.90_ad0.00_po0.00_glb.txt'   u 1:21 w l lw 2 lt 0 lc rgb "gray20"           title "",\
  'gnu_000/anal_sprd_time_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS1.00_ad0.00_po0.00_glb.txt'   u 1:21 w l lw 2 lt 0 lc rgb "dark-magenta"     title "",\
  'gnu_000/anal_sprd_time_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS1.10_ad0.00_po0.00_glb.txt'   u 1:21 w l lw 2 lt 0 lc rgb "blue"             title "",\
  'gnu_000/anal_sprd_time_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS1.20_ad0.00_po0.00_glb.txt'   u 1:21 w l lw 2 lt 0 lc rgb "dark-green"       title "",\
  'gnu_000/anal_sprd_time_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS1.30_ad0.00_po0.00_glb.txt'   u 1:21 w l lw 2 lt 0 lc rgb "red"              title "",\
  'gnu_000/anal_rmse_trun_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS0.90_ad0.00_po0.00_glb.txt'   u 1:21 w l lw 5 lt 1 lc rgb "gray20"           title "RTPS:0.90",\
  'gnu_000/anal_rmse_trun_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS1.00_ad0.00_po0.00_glb.txt'   u 1:21 w l lw 5 lt 1 lc rgb "dark-magenta"     title "RTPS:1.00",\
  'gnu_000/anal_rmse_trun_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS1.10_ad0.00_po0.00_glb.txt'   u 1:21 w l lw 5 lt 1 lc rgb "blue"             title "RTPS:1.10",\
  'gnu_000/anal_rmse_trun_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS1.20_ad0.00_po0.00_glb.txt'   u 1:21 w l lw 5 lt 1 lc rgb "dark-green"       title "RTPS:1.20",\
  'gnu_000/anal_rmse_trun_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS1.30_ad0.00_po0.00_glb.txt'   u 1:21 w l lw 5 lt 1 lc rgb "red"              title "RTPP:1.30"

###########################################################################################################################################################
###
set title  "(a) [AN] RMSE (solid) & Spread (dashed) ; Temperature [K] at 500 hPa" font "Helvetica,38"
set ylabel "RMSE, Spread [K]"   font "Helvetica,38" offset 0.5,0
set output "./fig_010_time/time_prepbufr_rmse_T500_RTPx_int.eps"
plot \
  'gnu_000/anal_sprd_time_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS1.20_ad0.00_po0.00_glb.txt'   u 1:21 w l lw 2 lt 0 lc rgb "blue"            title "",\
  'gnu_000/anal_sprd_time_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP0.90_ad0.00_po0.00_glb.txt'   u 1:21 w l lw 2 lt 0 lc rgb "red"             title "",\
  'gnu_000/anal_rmse_trun_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS1.20_ad0.00_po0.00_glb.txt'   u 1:21 w l lw 5 lt 1 lc rgb "blue"            title "RTPS:1.20",\
  'gnu_000/anal_rmse_trun_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP0.90_ad0.00_po0.00_glb.txt'   u 1:21 w l lw 5 lt 1 lc rgb "red"             title "RTPP:0.90"
###
set title  "(b) [AN] RMSE (solid) & Spread (dashed) ; Zonal Wind [m/s]" font "Helvetica,38"
set ylabel "RMSE, Spread [m/s]"   font "Helvetica,38" offset 0.5,0
set output "./fig_010_time/time_prepbufr_rmse_U700_RTPx_int.eps"
plot \
  'gnu_000/anal_sprd_time_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS1.20_ad0.00_po0.00_glb.txt'   u 1:5  w l lw 2 lt 0 lc rgb "blue"            title "",\
  'gnu_000/anal_sprd_time_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP0.90_ad0.00_po0.00_glb.txt'   u 1:5  w l lw 2 lt 0 lc rgb "red"             title "",\
  'gnu_000/anal_rmse_trun_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS1.20_ad0.00_po0.00_glb.txt'   u 1:5  w l lw 5 lt 1 lc rgb "blue"            title "RTPS:1.20",\
  'gnu_000/anal_rmse_trun_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP0.90_ad0.00_po0.00_glb.txt'   u 1:5  w l lw 5 lt 1 lc rgb "red"             title "RTPP:0.90"
###
set title  "(c) [AN] RMSE (solid) & Spread (dashed) ; Meridional Wind [m/s]" font "Helvetica,38"
set ylabel "RMSE, Spread [m/s]"   font "Helvetica,38" offset 0.5,0
set output "./fig_010_time/time_prepbufr_rmse_V850_RTPx_int.eps"
plot \
  'gnu_000/anal_sprd_time_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS1.20_ad0.00_po0.00_glb.txt'   u 1:12 w l lw 2 lt 0 lc rgb "blue"            title "",\
  'gnu_000/anal_sprd_time_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP0.90_ad0.00_po0.00_glb.txt'   u 1:12 w l lw 2 lt 0 lc rgb "red"             title "",\
  'gnu_000/anal_rmse_trun_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS1.20_ad0.00_po0.00_glb.txt'   u 1:12 w l lw 5 lt 1 lc rgb "blue"            title "RTPS:1.20",\
  'gnu_000/anal_rmse_trun_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP0.90_ad0.00_po0.00_glb.txt'   u 1:12 w l lw 5 lt 1 lc rgb "red"             title "RTPP:0.90"
###
set title  "(d) [AN] RMSE (solid) & Spread (dashed) ; Surface Pressure [hPa]" font "Helvetica,38"
set ylabel "RMSE, Spread [hPa]"   font "Helvetica,38" offset 0.5,0
set output "./fig_010_time/time_prepbufr_rmse_Psrf_RTPx_int.eps"
plot \
  'gnu_000/anal_sprd_time_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS1.20_ad0.00_po0.00_glb.txt'   u 1:($41/100) w l lw 2 lt 0 lc rgb "blue"     title "",\
  'gnu_000/anal_sprd_time_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP0.90_ad0.00_po0.00_glb.txt'   u 1:($41/100) w l lw 2 lt 0 lc rgb "red"      title "",\
  'gnu_000/anal_rmse_trun_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS1.20_ad0.00_po0.00_glb.txt'   u 1:($41/100) w l lw 5 lt 1 lc rgb "blue"     title "RTPS:1.20",\
  'gnu_000/anal_rmse_trun_prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP0.90_ad0.00_po0.00_glb.txt'   u 1:($41/100) w l lw 5 lt 1 lc rgb "red"      title "RTPP:0.90"