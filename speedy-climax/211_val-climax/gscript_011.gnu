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
#  01    ; header, FT (0,1,2,3)
#  02    ; header, yyyymmddhh
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
set xlabel "forecast lead time (hr)" font "Helvetica,38" offset 0.5,0
###########################################################################################################################################################
set xrange [0:20.0] 
set xtics ('0'0, '24'4, '48'8, '72'12, '96'16, '120'20 ) font "Helvetica,33"
###########################################################################################################################################################
set key right bottom
set title  "(a) RMSE (solid) & Spread (dashed) ; T [K] at 500 hPa, Lh:600km" font "Helvetica,38"
set ylabel "RMSE, Spread [K]"   font "Helvetica,38" offset 0.5,0
set yrange [1.1:3.0] ; set ytics 0, 0.3
###
set output "./fig_011_fcst/time_prepbufr-rtpp0.90_rmse_T500_def_L0600.eps"
plot \
  'gnu_cat/prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP0.90_ad0.00_po0.00_SMP2017010100-2017013118_fcst_glb_CRSP.txt' u 1:21 w l lw 5 lt 0 lc rgb "blue"   title "CRPS Spread",\
  'gnu_cat/prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP0.90_ad0.00_po0.00_SMP2017010100-2017013118_fcst_glb_CRSK.txt' u 1:21 w l lw 5 lt 1 lc rgb "blue"   title "CRPS Skill",\
  'gnu_cat/prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP0.90_ad0.00_po0.00_SMP2017010100-2017013118_fcst_glb_SPRD.txt' u 1:21 w l lw 5 lt 0 lc rgb "red"    title "Ensemble Spread",\
  'gnu_cat/prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP0.90_ad0.00_po0.00_SMP2017010100-2017013118_fcst_glb_RMSE.txt' u 1:21 w l lw 5 lt 1 lc rgb "red"    title "RMSE"
###
set output "./fig_011_fcst/time_prepbufr-rtps1.20_rmse_T500_def_L0600.eps"
plot \
  'gnu_cat/prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS1.20_ad0.00_po0.00_SMP2017010100-2017013118_fcst_glb_CRSP.txt' u 1:21 w l lw 5 lt 0 lc rgb "blue"   title "CRPS Spread",\
  'gnu_cat/prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS1.20_ad0.00_po0.00_SMP2017010100-2017013118_fcst_glb_CRSK.txt' u 1:21 w l lw 5 lt 1 lc rgb "blue"   title "CRPS Skill",\
  'gnu_cat/prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS1.20_ad0.00_po0.00_SMP2017010100-2017013118_fcst_glb_SPRD.txt' u 1:21 w l lw 5 lt 0 lc rgb "red"    title "Ensemble Spread",\
  'gnu_cat/prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS1.20_ad0.00_po0.00_SMP2017010100-2017013118_fcst_glb_RMSE.txt' u 1:21 w l lw 5 lt 1 lc rgb "red"    title "RMSE"
###
set output "./fig_011_fcst/time_prepbufr-rtpxcomp_rmse_T500_def_L0600.eps"
plot \
  'gnu_cat/prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS1.20_ad0.00_po0.00_SMP2017010100-2017013118_fcst_glb_SPRD.txt' u 1:21 w l lw 5 lt 0 lc rgb "blue"   title "Spread (RTPS:1.20)",\
  'gnu_cat/prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS1.20_ad0.00_po0.00_SMP2017010100-2017013118_fcst_glb_RMSE.txt' u 1:21 w l lw 5 lt 1 lc rgb "blue"   title "RMSE (RTPS:1.20)",\
  'gnu_cat/prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP0.90_ad0.00_po0.00_SMP2017010100-2017013118_fcst_glb_SPRD.txt' u 1:21 w l lw 5 lt 0 lc rgb "red"    title "Spread (RTPP:0.90)",\
  'gnu_cat/prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP0.90_ad0.00_po0.00_SMP2017010100-2017013118_fcst_glb_RMSE.txt' u 1:21 w l lw 5 lt 1 lc rgb "red"    title "RMSE (RTPP:0.90)"

quit
###########################################################################################################################################################
set key right bottom
set title  "(a) RMSE (solid) & Spread (dashed) ; Temperature [K] at 500 hPa" font "Helvetica,38"
set ylabel "RMSE, Spread [K]"   font "Helvetica,38" offset 0.5,0
set yrange [1.0:3.5] ; set ytics 0, 0.3
set output "./fig_011_fcst/time_raob_wb_rmse_T500_def.eps"
plot \
  'gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020L0500_RTPxx.xx_woWSM_SMP2017120100-2017123118_fcst_glb_SPRD.txt' u 1:21 w l lw 5 lt 0 lc rgb "dark-magenta"     title "",\
  'gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020L0600_RTPxx.xx_woWSM_SMP2017120100-2017123118_fcst_glb_SPRD.txt' u 1:21 w l lw 5 lt 0 lc rgb "blue"             title "",\
  'gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020L0700_RTPxx.xx_woWSM_SMP2017120100-2017123118_fcst_glb_SPRD.txt' u 1:21 w l lw 5 lt 0 lc rgb "dark-green"       title "",\
  'gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020L0800_RTPxx.xx_woWSM_SMP2017120100-2017123118_fcst_glb_SPRD.txt' u 1:21 w l lw 5 lt 0 lc rgb "red"              title "",\
  'gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020L0500_RTPxx.xx_woWSM_SMP2017120100-2017123118_fcst_glb_RMSE.txt' u 1:21 w l lw 5 lt 1 lc rgb "dark-magenta"     title "L_h:500km",\
  'gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020L0600_RTPxx.xx_woWSM_SMP2017120100-2017123118_fcst_glb_RMSE.txt' u 1:21 w l lw 5 lt 1 lc rgb "blue"             title "L_h:600km",\
  'gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020L0700_RTPxx.xx_woWSM_SMP2017120100-2017123118_fcst_glb_RMSE.txt' u 1:21 w l lw 5 lt 1 lc rgb "dark-green"       title "L_h:700km",\
  'gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020L0800_RTPxx.xx_woWSM_SMP2017120100-2017123118_fcst_glb_RMSE.txt' u 1:21 w l lw 5 lt 1 lc rgb "red"              title "L_h:800km"
###########################################################################################################################################################
set key right bottom
set title  "(b) RMSE (solid) & Spread (dashed) ; Geopotential Height [m] at 500 hPa" font "Helvetica,38"
set ylabel "RMSE, Spread [m]"   font "Helvetica,38" offset 0.5,0
set yrange [15:80.0]; set ytics 0, 10
set output "./fig_011_fcst/time_raob_wb_rmse_Z500_def.eps"
plot \
  'gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020L0500_RTPxx.xx_woWSM_SMP2017120100-2017123118_fcst_glb_SPRD.txt' u 1:($35/9.81) w l lw 5 lt 0 lc rgb "dark-magenta"     title "",\
  'gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020L0600_RTPxx.xx_woWSM_SMP2017120100-2017123118_fcst_glb_SPRD.txt' u 1:($35/9.81) w l lw 5 lt 0 lc rgb "blue"             title "",\
  'gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020L0700_RTPxx.xx_woWSM_SMP2017120100-2017123118_fcst_glb_SPRD.txt' u 1:($35/9.81) w l lw 5 lt 0 lc rgb "dark-green"       title "",\
  'gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020L0800_RTPxx.xx_woWSM_SMP2017120100-2017123118_fcst_glb_SPRD.txt' u 1:($35/9.81) w l lw 5 lt 0 lc rgb "red"              title "",\
  'gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020L0500_RTPxx.xx_woWSM_SMP2017120100-2017123118_fcst_glb_RMSE.txt' u 1:($35/9.81) w l lw 5 lt 1 lc rgb "dark-magenta"     title "L_h:500km",\
  'gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020L0600_RTPxx.xx_woWSM_SMP2017120100-2017123118_fcst_glb_RMSE.txt' u 1:($35/9.81) w l lw 5 lt 1 lc rgb "blue"             title "L_h:600km",\
  'gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020L0700_RTPxx.xx_woWSM_SMP2017120100-2017123118_fcst_glb_RMSE.txt' u 1:($35/9.81) w l lw 5 lt 1 lc rgb "dark-green"       title "L_h:700km",\
  'gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020L0800_RTPxx.xx_woWSM_SMP2017120100-2017123118_fcst_glb_RMSE.txt' u 1:($35/9.81) w l lw 5 lt 1 lc rgb "red"              title "L_h:800km"