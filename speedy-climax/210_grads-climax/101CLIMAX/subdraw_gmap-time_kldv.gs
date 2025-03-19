'reinit'
*******************************************************
*** setting of control file
'open ./gues-kldv_exp1_yyyymmddhh.ctl'

*** setting of time 
stime=00z01Jan2017 ; str="2017010100"
etime=18z31Dec2017 ; end="2017123118"
*******************************************************
'set display color white'
'reset'

*===> strings
'set strsiz 0.24 0.27'
'draw string 0.6 8.0 ClimaX-LETKF : T [K] (5th model level ; 500 hPa)'


'set strsiz 0.17 0.20'
'draw string 5.20 7.60 From 'str''
'draw string 8.60 7.60 To 'end''

*===> constant parameters
'./gsfile/presen.gs -alabel 0.15'
'./gsfile/define_colors.gs'

*===> global map
'set gxout shaded'
'set grads off'
'infl=ave(T,time='stime',time='etime'))'

'set z 5'
'set parea 1.6 9.6 3.7 6.9'

'set clevs 0.05 0.06 0.07 0.08 0.09 0.10 0.12 0.15'
'set ccols 14 4 5 3 10 7 8 2 6'

'd infl'
'./gsfile/xcbar.gs 9.0 9.2 3.7 6.9 -dir v'

'set strsiz 0.23 0.26'
'set string 1 c 1'
'draw string 5.3 7.15 (a) KL Divergence'


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


'set parea 0.8 10.8 0.5 2.8'
'set ylint 0.01'
'set vrange 0.05 0.10'

'set cthick 3'
'set digsiz 0.05'
'set ccolor 1' ; 'set cmark 2' ; 'd glb'
'set ccolor 4' ; 'set cmark 2' ; 'd nph'
'set ccolor 3' ; 'set cmark 2' ; 'd trp'
'set ccolor 2' ; 'set cmark 2' ; 'd sph'
'draw string 5.3 3.05 (b) Time Series of KL Divergence'

'./gsfile/cbar_line -x 1.35 -y 0.75 -c 1 -m 2 -l 1 -sz 0.27 -t "GLOBAL"'
'./gsfile/cbar_line -x 3.35 -y 0.75 -c 4 -m 2 -l 1 -sz 0.27 -t "NH(20:90)"'
'./gsfile/cbar_line -x 5.75 -y 0.75 -c 3 -m 2 -l 1 -sz 0.27 -t "TR(-20:20)"'
'./gsfile/cbar_line -x 8.35 -y 0.75 -c 2 -m 2 -l 1 -sz 0.27 -t "SH(-90:20)"'

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

