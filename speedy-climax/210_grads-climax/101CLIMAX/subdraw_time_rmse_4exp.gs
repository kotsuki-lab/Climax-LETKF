'reinit'
*******************************************************
*** setting of control file
'open ./nature_yyyymmddhh.ctl' 
'open ./anal-mean_exp1_yyyymmddhh.ctl' ; 'open ./anal-sprd_exp1_yyyymmddhh.ctl'
'open ./anal-mean_exp2_yyyymmddhh.ctl' ; 'open ./anal-sprd_exp2_yyyymmddhh.ctl'
'open ./anal-mean_exp3_yyyymmddhh.ctl' ; 'open ./anal-sprd_exp3_yyyymmddhh.ctl'
'open ./anal-mean_exp4_yyyymmddhh.ctl' ; 'open ./anal-sprd_exp4_yyyymmddhh.ctl'

*** setting of time 
stime=00z01Jan2017 ; str="2017010100"
etime=18z31Jan2017 ; end="2017013118"
etime=18z30Apr2017 ; end="2017043018"
etime=18z31Dec2017 ; end="2017123118"
*******************************************************
'set display color white'
'reset'

*===> strings
'set strsiz 0.23 0.26'
'draw string 0.6 7.1 [AN] ClimaX: RMSE T [K] (5th model level; 500 hPa)'

'set strsiz 0.17 0.20'
'draw string 5.20 6.70 From 'str''
'draw string 8.60 6.70 To 'end''

*===> constant parameters
'set gxout line'
'set grads off'

'./gsfile/presen.gs -alabel 0.15'
'./gsfile/define_colors.gs'

*'set tlsupp year'
'set z 5' ; 'set x 1' ; 'set y 1'

'set time 'stime' 'etime''
'rms1=sqrt(aave(pow((T.1-T.2),2),globe))' ; 'spr1=sqrt(aave(pow((    T.3),2),globe))'
'rms2=sqrt(aave(pow((T.1-T.4),2),globe))' ; 'spr2=sqrt(aave(pow((    T.5),2),globe))'
'rms3=sqrt(aave(pow((T.1-T.6),2),globe))' ; 'spr3=sqrt(aave(pow((    T.7),2),globe))'
'rms4=sqrt(aave(pow((T.1-T.8),2),globe))' ; 'spr4=sqrt(aave(pow((    T.9),2),globe))'

'set parea 0.8 10.8 2.7 6.5'
'set ylab on'
'set ylint 0.5'
'set vrange 0.0 3.0'
'set digsiz 0.00'

****
ivar=1
while(ivar<=8)  
  if(ivar=1|ivar=2|ivar=3|ivar=4); 'set cstyle 1' ; 'set cthick 10' ; endif
  if(ivar=5|ivar=6|ivar=7|ivar=8); 'set cstyle 2' ; 'set cthick  3' ; endif
  if(ivar=1|ivar=5); 'set ccolor 1' ; endif
  if(ivar=2|ivar=6); 'set ccolor 4' ; endif
  if(ivar=3|ivar=7); 'set ccolor 2' ; endif
  if(ivar=4|ivar=8); 'set ccolor 3' ; endif

  'set cmark 0'
  if(ivar=1); 'd rms1' ; endif
  if(ivar=2); 'd rms2' ; endif
  if(ivar=3); 'd rms3' ; endif
  if(ivar=4); 'd rms4' ; endif
  if(ivar=5); 'd spr1' ; endif
  if(ivar=6); 'd spr2' ; endif
  if(ivar=7); 'd spr3' ; endif
  if(ivar=8); 'd spr4' ; endif
  
ivar=ivar+1
endwhile


*'set strsiz 0.20 0.23'
'./gsfile/cbar_line -x 1.00 -y 2.95 -c 1 -m 2 -l 1 -sz 0.27 -t "EXP1"'
'./gsfile/cbar_line -x 2.75 -y 2.95 -c 4 -m 2 -l 1 -sz 0.27 -t "EXP2"'
'./gsfile/cbar_line -x 4.50 -y 2.95 -c 2 -m 2 -l 1 -sz 0.27 -t "EXP3"'
'./gsfile/cbar_line -x 6.25 -y 2.95 -c 3 -m 2 -l 1 -sz 0.27 -t "EXP4"'



*'print temp.eps'
*'printim PNGfile/time_rmse-sprd_'str'-'end'.png'
'printim PNGfile/time_rmse.png'


****===> output txt file
***txtfile='out/time_rmse-sprd.txt'
***'!echo TIME RMSE SPREAD > 'txtfile
***'q dims'
***tmp=sublin(result,5)
***tstr=subwrd(tmp,11)
***tend=subwrd(tmp,13)
***tt=tstr
***while(tt<=tend)
***  'set t 'tt
***  'd rmse'
***  rmse=subwrd(result,4)
***  'd sprd'
***  sprd=subwrd(result,4)
***  rc=write(txtfile,tt' 'rmse' 'sprd,append)
***  rc=close(txtfile)
***
***tt=tt+1
***endwhile

return

