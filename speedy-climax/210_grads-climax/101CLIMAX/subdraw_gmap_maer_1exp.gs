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
'draw string 0.6 8.1 Climax-LETKF : (5th model level ; 500 hPa)'


'set strsiz 0.17 0.20'
'draw string 5.20 7.70 From 'str''
'draw string 8.60 7.70 To 'end''

'set strsiz 0.18 0.20'
'set string 1 c 1'
'draw string 2.1 7.34 (a): FG MAD, T500 [K]'
'draw string 5.6 7.34 (b): AN MAD, T500 [K]'
'draw string 8.5 7.34 (c): (b) - (a)'

'draw string 2.1 4.04 (d): FG MAD, Z500 [m]'
'draw string 5.6 4.04 (e): AN MAD, Z500 [m]'
'draw string 8.5 4.04 (f): (e) - (d)'


*===> constant parameters
'set gxout grfill'
'set grads off'

'./gsfile/presen.gs -alabel 0.12'
'./gsfile/define_colors.gs'

*** 500 hPa Temperature
  'set z 5'
  'guesT=ave(abs(T.2-T.1),time='stime',time='etime'))'
  'analT=ave(abs(T.3-T.1),time='stime',time='etime'))'
*** 500 hPa GPH
  'set z 5'
  'guesZ=ave(abs(GEO.2-GEO.1),time='stime',time='etime'))/9.81'
  'analZ=ave(abs(GEO.3-GEO.1),time='stime',time='etime'))/9.81'

****
ivar=1
while(ivar<=6)
  'set lon 0 359.99'
  'set lat -90 90'
  'set ylint 30'
  if(ivar=1); 'set parea 0.50   3.95 4.30 7.80' ; endif
  if(ivar=2); 'set parea 3.95   7.40 4.30 7.80' ; endif
  if(ivar=3); 'set parea 7.45  10.90 4.30 7.80' ; endif

  if(ivar=4); 'set parea 0.50   3.95 1.00 4.50' ; endif
  if(ivar=5); 'set parea 3.95   7.40 1.00 4.50' ; endif
  if(ivar=6); 'set parea 7.45  10.90 1.00 4.50' ; endif

  'set ylab off'
  if(ivar=1|ivar=4); 'set ylab on' ; endif

*** temperature
  if(ivar=1|ivar=2) ; 'set clevs 0.5 0.6 0.8 1.0 1.2 1.4 1.6 2.0'             ; endif
  if(ivar=3)        ; 'set clevs -0.2 -0.15 -0.1 -0.05 0.05 0.1 0.15 0.2' ; endif
*** geopotential
  if(ivar=4|ivar=5) ; 'set clevs  6 8 12 16 20 30 40 50 '    ; endif
  if(ivar=6)        ; 'set clevs -4 -3 -2 -1 1 2 3 4'     ; endif

  if(ivar=1|ivar=2|ivar=4|ivar=5) ; 'set ccols 14 4 5 3 10 7 8 2 6'        ; endif
  if(ivar=3|ivar=6)               ; 'set ccols 69 98 95 91 21 81 85 88 59' ; endif





  if(ivar=1); 'd guesT'       ; endif
  if(ivar=2); 'd analT'       ; endif
  if(ivar=3); 'd analT-guesT' ; endif
  if(ivar=4); 'd guesZ'       ; endif
  if(ivar=5); 'd analZ'       ; endif
  if(ivar=6); 'd analZ-guesZ' ; endif

  'set strsiz 0.18 0.20'
  if(ivar=2); './gsfile/xcbar.gs 0.5   6.9  4.55 4.67 -dir h  -line on -edge triangle -fwidth 0.11 -fheight 0.12' ; endif
  if(ivar=3); './gsfile/xcbar.gs 7.0  10.9  4.55 4.67 -dir h  -line on -edge triangle -fwidth 0.11 -fheight 0.12' ; endif
  if(ivar=5); './gsfile/xcbar.gs 0.5   6.9  1.25 1.37 -dir h  -line on -edge triangle -fwidth 0.11 -fheight 0.12' ; endif
  if(ivar=6); './gsfile/xcbar.gs 7.0  10.9  1.25 1.37 -dir h  -line on -edge triangle -fwidth 0.11 -fheight 0.12' ; endif
ivar=ivar+1
endwhile

*'print temp.eps'
'printim EXP1figs/gmap_rmse-sprd_'str'-'end'.png'
return

