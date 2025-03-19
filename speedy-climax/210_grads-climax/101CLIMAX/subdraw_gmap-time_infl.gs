'reinit'
*******************************************************
*** setting of control file
'open ./infl-mul_exp1_yyyymmddhh.ctl'

*** setting of time 
stime=00z01Jan2017 ; str="2017010100"
etime=18z31Dec2017 ; end="2017123118"
*******************************************************
'set display color white'
'reset'

*===> strings
'set strsiz 0.23 0.26'
'draw string 0.6 8.0 ClimaX-LETKF : Inflation Factor (5th level; 500 hPa)'


'set strsiz 0.17 0.20'
*'draw string 5.20 7.60 From 'str''
*'draw string 8.60 7.60 To 'end''

*===> constant parameters
'./gsfile/presen.gs -alabel 0.12'
'./gsfile/define_colors.gs'

*===> global map
'set gxout grfill'
'set grads off'
'infl=ave(T,time='etime',time='etime'))'

'set z 5'
'set parea 1.0 9.4 2.7 8.0'

'set clevs 1.05 1.10 1.20 1.30 1.50 2.00 3.00 4.00 5.00'
*'set ccols 14 4 5 3 10 7 8 2 6'

'd infl'
'./gsfile/xcbar.gs 10.05 10.20 2.7 7.6 -dir v  -line on -edge triangle -fwidth 0.11 -fheight 0.12'

'set strsiz 0.23 0.26'
'set string 1 c 1'
*'draw string 5.3 7.15 (a) Inflation Field'


*===> time mean
'set z 4'
'set x 1'
'set y 1'
'set gxout line'
'set time 'stime' 'etime''

'glb=aave(T,lon=0,lon=360,lat=-90,lat=90)'
'nph=aave(T,lon=0,lon=360,lat=20, lat=90)'
'trp=aave(T,lon=0,lon=360,lat=-20,lat=20)'
'sph=aave(T,lon=0,lon=360,lat=-90,lat=-20)'


'set parea 1.0 10.8 0.6 2.5'
'set ylint 0.5'
'set vrange 0.98 2.5'

'set cthick 3'
'set digsiz 0.05'
'set ccolor 1' ; 'set cmark 2' ; 'd glb'
***'set ccolor 4' ; 'set cmark 2' ; 'd nph'
***'set ccolor 3' ; 'set cmark 2' ; 'd trp'
***'set ccolor 2' ; 'set cmark 2' ; 'd sph'
***'draw string 5.3 3.05 (b) Time Series of Mean Inflation'

***'./gsfile/cbar_line -x 1.35 -y 0.75 -c 1 -m 2 -l 1 -sz 0.27 -t "GLOBAL"'
***'./gsfile/cbar_line -x 3.35 -y 0.75 -c 4 -m 2 -l 1 -sz 0.27 -t "NH(20:90)"'
***'./gsfile/cbar_line -x 5.75 -y 0.75 -c 3 -m 2 -l 1 -sz 0.27 -t "TR(-20:20)"'
***'./gsfile/cbar_line -x 8.35 -y 0.75 -c 2 -m 2 -l 1 -sz 0.27 -t "SH(-90:20)"'

'draw string 0.5 7.6 (a)'
'draw string 0.5 2.2 (b)'
'./gsfile/cbar_line -x 7.70 -y 0.90 -c 1 -m 2 -l 1 -sz 0.27 -t "Global Average"'

'printim PNGfile/time_infl-mul.png'


***===> output txt file
**txtfile='out/time_infl.txt'
**'!echo TIME GL NH TR SH > 'txtfile
**'q dims'
**
**tmp=sublin(result,5)
**tstr=subwrd(tmp,11)
**tend=subwrd(tmp,13)
**tt=tstr
**while(tt<=tend)
**  'set t 'tt
**  'd glb' ; tglb=subwrd(result,4)
**  'd nph' ; tnph=subwrd(result,4)
**  'd trp' ; ttrp=subwrd(result,4)
**  'd sph' ; tsph=subwrd(result,4)
**
**  rc=write(txtfile,tt' 'tglb' 'tnph' 'ttrp' 'tsph,append)
**  rc=close(txtfile)
**
**tt=tt+1
**endwhile


return

