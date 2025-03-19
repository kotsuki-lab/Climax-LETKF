'reinit'
*******************************************************
*** setting of control file
'open weatherbc_yyyymmddhh.ctl' 
'open gues-mean_exp1_yyyymmddhh.ctl'
'open anal-mean_exp1_yyyymmddhh.ctl'

*** setting of time
stime=00z01Jan2017 ; str="2017010100"
etime=00z01Jan2017 ; end="2017010100"

stime=00z01Jul2017 ; str="2017070100"
etime=18z31Dec2017 ; end="2017123118"
*******************************************************
'set display color white'
'reset'

*===> strings
'set strsiz 0.24 0.27'
'draw string 0.6 8.20 ClimaX-LETKF : (U, T, Z, Ps)'


'set strsiz 0.15 0.18'
'draw string 8.60 8.31 From 'str''
'draw string 8.94 8.09 To 'end''

'set strsiz 0.18 0.20'
'set string 1 c 1'

'draw string 3.05 7.94 (a): MAE (AN-FG), U850 [m]'
'draw string 8.40 7.94 (b): MAE (AN-FG), T700 [K]'

'draw string 3.05 3.89 (c): MAE (AN-FG), Z500 [m]'
'draw string 8.40 3.89 (d): MAE (AN-FG), Ps [hPa]'


*===> constant parameters
'set gxout grfill'
'set grads off'

'./gsfile/presen.gs -alabel 0.12'
'./gsfile/define_colors.gs'

*** 850 hPa U-wind
  'set z 2'
  'guesU=ave(abs(U.2-U.1),time='stime',time='etime'))'
  'analU=ave(abs(U.3-U.1),time='stime',time='etime'))'
*** 700 hPa Temperature
  'set z 3'
  'guesT=ave(abs(T.2-T.1),time='stime',time='etime'))'
  'analT=ave(abs(T.3-T.1),time='stime',time='etime'))'
*** 500 hPa GPH
  'set z 5'
  'guesZ=ave(abs(GEO.2-GEO.1),time='stime',time='etime'))/9.81'
  'analZ=ave(abs(GEO.3-GEO.1),time='stime',time='etime'))/9.81'
*** 500 hPa GPH
  'set z 1'
  'guesP=ave(abs(PS.2-PS.1),time='stime',time='etime'))/100.0'
  'analP=ave(abs(PS.3-PS.1),time='stime',time='etime'))/100.0'

****
ivar=1
while(ivar<=4)
  'set lon 0 359.99'
  'set lat -90 90'
  'set ylint 30'
  if(ivar=1); 'set parea 0.60   5.60 4.47 7.97' ; endif
  if(ivar=2); 'set parea 5.90  10.90 4.47 7.97' ; endif

  if(ivar=3); 'set parea 0.60   5.60 0.42 3.92' ; endif
  if(ivar=4); 'set parea 5.90  10.90 0.42 3.92' ; endif

  'set ylab off'
  if(ivar=1|ivar=3); 'set ylab on' ; endif

  if(ivar=1)        ; 'set clevs -0.8 -0.5 -0.3 -0.15 0.15 0.3 0.5 0.8'   ; endif
  if(ivar=2)        ; 'set clevs -0.2 -0.15 -0.1 -0.05 0.05 0.1 0.15 0.2' ; endif
  if(ivar=3)        ; 'set clevs -4 -3 -2 -1 1 2 3 4'                     ; endif
  if(ivar=4)        ; 'set clevs -2 -1 -0.5 -0.2 0.2 0.5 1 2'             ; endif

  'set ccols 69 98 95 91 21 81 85 88 59'



  if(ivar=1); 'd analU-guesU' ; endif
  if(ivar=2); 'd analT-guesT' ; endif
  if(ivar=3); 'd analZ-guesZ' ; endif
  if(ivar=4); 'd analP-guesP' ; endif

  'set strsiz 0.18 0.20'
  if(ivar=1); './gsfile/xcbar.gs 0.6   5.6  4.30 4.42 -dir h  -line on -edge triangle -fwidth 0.11 -fheight 0.12' ; endif
  if(ivar=2); './gsfile/xcbar.gs 5.9  10.9  4.30 4.42 -dir h  -line on -edge triangle -fwidth 0.11 -fheight 0.12' ; endif

  if(ivar=3); './gsfile/xcbar.gs 0.6   5.6  0.25 0.37 -dir h  -line on -edge triangle -fwidth 0.11 -fheight 0.12' ; endif
  if(ivar=4); './gsfile/xcbar.gs 5.9  10.9  0.25 0.37 -dir h  -line on -edge triangle -fwidth 0.11 -fheight 0.12' ; endif

  '../../obs/FIGrads/markplot_statsml_raob.gs'
ivar=ivar+1
endwhile

*'print temp.eps'
'printim EXP1figs/gmap_mae-4map_'str'-'end'.png'
return

