***************************************************************************
EXP1DIR="/export/home/kotsuki/climax-letkf/weatherbench/v20240703/" 
EXP2DIR="/export/home/kotsuki/climax-letkf/climax/nature_run/nature/000001/"
***************************************************************************
'!rm exp1dir exp2dir'
'!ln -s  'EXP1DIR'  exp1dir'
'!ln -s  'EXP2DIR'  exp2dir'

'reinit'
*******************************************************
*** setting of control file
'open simple_exp1_yyyymmddhh.ctl' 
'open simple_exp2_yyyymmddhh.ctl' 


*** setting of time
timea=00z03Jan2017 ; stra="2017010300 UTC"
timeb=00z01Feb2017 ; strb="2017020100 UTC"
timec=00z01May2017 ; strc="2017050100 UTC"
*******************************************************
'set display color white'
'reset'

*===> strings
'set strsiz 0.24 0.27'
'draw string 0.6 8.1 Weather Bench : (5th level T [K]; 500 hPa)'
'draw string 0.6 4.4 ClimaX Forecast : (5th level T [K]; 500 hPa)'

'set strsiz 0.17 0.20'
'draw string 7.20 4.02 Initialized at 2017010100'

'set strsiz 0.18 0.20'
'set string 1 c 1'
'draw string 2.1 7.64 (a): 'stra''
'draw string 5.6 7.64 (b): 'strb''
'draw string 9.1 7.64 (c): 'strc''

'draw string 2.1 3.74 (d): 'stra''
'draw string 5.6 3.74 (e): 'strb''
'draw string 9.1 3.74 (f): 'strc''


*===> constant parameters
'set gxout grfill'
'set grads off'

'./gsfile/presen.gs -alabel 0.12'
'./gsfile/define_colors.gs'


****
ivar=1
while(ivar<=6)
  'set lon 0 359.99'
  'set lat -90 90'
  'set ylint 30'
  if(ivar=1); 'set parea 0.50   3.95 4.60 8.10' ; endif
  if(ivar=2); 'set parea 3.98   7.43 4.60 8.10' ; endif
  if(ivar=3); 'set parea 7.46  10.91 4.60 8.10' ; endif

  if(ivar=4); 'set parea 0.50   3.95 0.70 4.20' ; endif
  if(ivar=5); 'set parea 3.98   7.43 0.70 4.20' ; endif
  if(ivar=6); 'set parea 7.46  10.91 0.70 4.20' ; endif

  'set ylab off'
  if(ivar=1|ivar=4); 'set ylab on' ; endif

  'set z 5'
  'set clevs 230 235 240 245 250 255 260 265 270'



  if(ivar=1|ivar=4); 'set time 'timea'' ; endif
  if(ivar=2|ivar=5); 'set time 'timeb'' ; endif
  if(ivar=3|ivar=6); 'set time 'timec'' ; endif

  if(ivar=1|ivar=2|ivar=3); 'd T.1' ; endif
  if(ivar=4|ivar=5|ivar=6); 'd T.2' ; endif

  'set strsiz 0.18 0.20'
  if(ivar=6); './gsfile/xcbar.gs 0.50  10.91  0.85 0.97 -dir h  -line on -edge triangle -fwidth 0.11 -fheight 0.12' ; endif
ivar=ivar+1
endwhile

*'print temp.eps'
'printim EXP2figs/gmap_collaps.png'
return
