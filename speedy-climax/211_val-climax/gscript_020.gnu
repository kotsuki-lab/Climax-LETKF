 set terminal tgif
 set terminal postscript eps enhanced color solid "Helvetica" 32
#  set terminal postscript eps "Helvetica" 32
 set size 1.3
# set size ratio 0.8 1.5
 set size ratio 0.7 2.0

#################################################### 
# climax variables
# levels: 925 850 700 600 500 250 50
########################## 
#  01    ; header, loc scale (km)
#  02    ; header, ave of data lines
#  03    ; header, ave of date
#  04:10 ; u (925 850 700 600 500 250 50)
#  12:17 ; v (925 850 700 600 500 250 50)
#  18:24 ; t (925 850 700 600 500 250 50)
#  25:31 ; q (925 850 700 600 500 250 50) [g/kg] coverted in 
#  32:38 ; gph (925 850 700 600 500 250 50) height [m]
#  39    ; T2m
#  40    ; U10m
#  41    ; V10m
#  42    ; Ps
####################################################

###########################################################################################################################################################
 set key center top
 set grid
###################################################################
set xrange [330:880]
set xlabel "Horizontal Localization Scale (L_h) [km]"            font "Helvetica,42" offset 0.5,0

set style fill solid 0.2  border lc rgb "black" 
set boxwidth 0.2 relative

###################################################################
set ylabel "RMSE [m/s]"                           font "Helvetica,42" offset 0.5,0
set title  "(a) ClimaX-LETKF ; RMSE vs. WB, U-Wind [m/s]" font "Helvetica,42"
set output "./fig_020_box/M000020_a_Uxxx.eps"
plot \
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_anal_glb_RMSE.txt'   u ($1-30):5  w boxes   lw 5 lc rgb "light-blue"  fill pattern 2  title "AN RMSE (850hPa)",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_anal_glb_RMSE.txt'   u ($1-10):6  w boxes   lw 5 lc rgb "light-green" fill pattern 3  title "AN RMSE (700hPa)",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_anal_glb_RMSE.txt'   u ($1+10):8  w boxes   lw 5 lc rgb "pink"        fill pattern 3  title "AN RMSE (500hPa)",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_anal_glb_RMSE.txt'   u ($1+30):9  w boxes   lw 5 lc rgb "plum"        fill pattern 3  title "AN RMSE (250hPa)",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_gues_glb_RMSE.txt'   u ($1-30):5  w p pt 13 ps 4 lc rgb "black"                       title "FG RMSE",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_gues_glb_RMSE.txt'   u ($1-10):6  w p pt 13 ps 4 lc rgb "black"                       title "",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_gues_glb_RMSE.txt'   u ($1+10):8  w p pt 13 ps 4 lc rgb "black"                       title "",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_gues_glb_RMSE.txt'   u ($1+30):9  w p pt 13 ps 4 lc rgb "black"                       title ""

###################################################################
set ylabel "RMSE [m/s]"                           font "Helvetica,42" offset 0.5,0
set title  "(b) ClimaX-LETKF ; RMSE vs. WB, V-Wind [m/s]" font "Helvetica,42"
set output "./fig_020_box/M000020_b_Vxxx.eps"
plot \
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_anal_glb_RMSE.txt'   u ($1-30):12 w boxes   lw 5 lc rgb "light-blue"  fill pattern 2  title "AN RMSE (850hPa)",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_anal_glb_RMSE.txt'   u ($1-10):13 w boxes   lw 5 lc rgb "light-green" fill pattern 3  title "AN RMSE (700hPa)",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_anal_glb_RMSE.txt'   u ($1+10):15 w boxes   lw 5 lc rgb "pink"        fill pattern 3  title "AN RMSE (500hPa)",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_anal_glb_RMSE.txt'   u ($1+30):16 w boxes   lw 5 lc rgb "plum"        fill pattern 3  title "AN RMSE (250hPa)",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_gues_glb_RMSE.txt'   u ($1-30):12 w p pt 13 ps 4 lc rgb "black"                       title "FG RMSE",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_gues_glb_RMSE.txt'   u ($1-10):13 w p pt 13 ps 4 lc rgb "black"                       title "",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_gues_glb_RMSE.txt'   u ($1+10):15 w p pt 13 ps 4 lc rgb "black"                       title "",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_gues_glb_RMSE.txt'   u ($1+30):16 w p pt 13 ps 4 lc rgb "black"                       title ""

######################################################################################################################################
#set yrange [1.20:2.60] ; set ytics 0, 0.40
set ylabel "RMSE [K]"                           font "Helvetica,42" offset 0.5,0
set title  "(c) ClimaX-LETKF ; RMSE vs. WB, Temperature [K]" font "Helvetica,42"
set output "./fig_020_box/M000020_c_Txxx.eps"
plot \
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_anal_glb_RMSE.txt'   u ($1-30):19 w boxes   lw 5 lc rgb "light-blue"  fill pattern 2  title "AN RMSE (850hPa)",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_anal_glb_RMSE.txt'   u ($1-10):20 w boxes   lw 5 lc rgb "light-green" fill pattern 3  title "AN RMSE (700hPa)",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_anal_glb_RMSE.txt'   u ($1+10):22 w boxes   lw 5 lc rgb "pink"        fill pattern 3  title "AN RMSE (500hPa)",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_anal_glb_RMSE.txt'   u ($1+30):23 w boxes   lw 5 lc rgb "plum"        fill pattern 3  title "AN RMSE (250hPa)",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_gues_glb_RMSE.txt'   u ($1-30):19 w p pt 13 ps 4 lc rgb "black"                       title "FG RMSE",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_gues_glb_RMSE.txt'   u ($1-10):20 w p pt 13 ps 4 lc rgb "black"                       title "",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_gues_glb_RMSE.txt'   u ($1+10):22 w p pt 13 ps 4 lc rgb "black"                       title "",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_gues_glb_RMSE.txt'   u ($1+30):23 w p pt 13 ps 4 lc rgb "black"                       title ""


###################################################################
set ylabel "RMSE [g/kg]"                           font "Helvetica,42" offset 0.5,0
set title  "(d) ClimaX-LETKF ; RMSE vs. WB, Specific Humidity [g/kg]" font "Helvetica,42"
set output "./fig_020_box/M000020_d_Qxxx.eps"
plot \
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_anal_glb_RMSE.txt'   u ($1-30):($26*1000) w boxes   lw 5 lc rgb "light-blue"  fill pattern 2  title "AN RMSE (850hPa)",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_anal_glb_RMSE.txt'   u ($1-10):($27*1000) w boxes   lw 5 lc rgb "light-green" fill pattern 3  title "AN RMSE (700hPa)",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_anal_glb_RMSE.txt'   u ($1+10):($29*1000) w boxes   lw 5 lc rgb "pink"        fill pattern 3  title "AN RMSE (500hPa)",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_gues_glb_RMSE.txt'   u ($1-30):($26*1000) w p pt 13 ps 4 lc rgb "black"                       title "FG RMSE",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_gues_glb_RMSE.txt'   u ($1-10):($27*1000) w p pt 13 ps 4 lc rgb "black"                       title "",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_gues_glb_RMSE.txt'   u ($1+10):($29*1000) w p pt 13 ps 4 lc rgb "black"                       title ""

###################################################################
set ylabel "RMSE [m]"                           font "Helvetica,42" offset 0.5,0
set title  "(e) ClimaX-LETKF ; RMSE vs. WB, Geopotential Height [m]" font "Helvetica,42"
set output "./fig_020_box/M000020_e_Zxxx.eps"
plot \
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_anal_glb_RMSE.txt'   u ($1-30):($33/9.81) w boxes   lw 5 lc rgb "light-blue"  fill pattern 2  title "AN RMSE (850hPa)",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_anal_glb_RMSE.txt'   u ($1-10):($34/9.81) w boxes   lw 5 lc rgb "light-green" fill pattern 3  title "AN RMSE (700hPa)",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_anal_glb_RMSE.txt'   u ($1+10):($36/9.81) w boxes   lw 5 lc rgb "pink"        fill pattern 3  title "AN RMSE (500hPa)",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_anal_glb_RMSE.txt'   u ($1+30):($37/9.81) w boxes   lw 5 lc rgb "plum"        fill pattern 3  title "AN RMSE (250hPa)",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_gues_glb_RMSE.txt'   u ($1-30):($33/9.81) w p pt 13 ps 4 lc rgb "black"                       title "FG RMSE",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_gues_glb_RMSE.txt'   u ($1-10):($34/9.81) w p pt 13 ps 4 lc rgb "black"                       title "",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_gues_glb_RMSE.txt'   u ($1+10):($36/9.81) w p pt 13 ps 4 lc rgb "black"                       title "",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_gues_glb_RMSE.txt'   u ($1+30):($37/9.81) w p pt 13 ps 4 lc rgb "black"                       title ""

###################################################################
set ylabel "RMSE [Pa]"                           font "Helvetica,42" offset 0.5,0
set title  "(f) ClimaX-LETKF ; RMSE vs. WB, Surface Pressure [Pa]" font "Helvetica,42"
set output "./fig_020_box/M000020_f_Psrf.eps"
plot \
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_anal_glb_RMSE.txt'   u ($1+0):42 w boxes  lw 5  lc rgb "gray"    fill pattern 2  title "AN RMSE (sfc)",\
  './gnu_cat/raob_wb_CLIMAXdef-0LETKF_M000020_RTPxx.xx_woWSM_SMP2017070100-2017123118_gues_glb_RMSE.txt'   u ($1+0):42 w p pt 13 ps 4 lc rgb "black"                   title "FG RMSE",\





