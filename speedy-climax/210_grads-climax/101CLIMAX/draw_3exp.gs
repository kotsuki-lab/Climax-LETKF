basedir="/export/home/kotsuki/climax-letkf/SYS20240704_SPEEDY_LPFv1.00_LAB/climax/DATA/" 
****
imem=1 ; while(imem<=1)
  if(imem=1); cmem="M000016" ; endif

iobs=1 ; while(iobs<=1)
  if(iobs=1); cobs="raob" ; endif

  cloc=Lxxxx
  if(imem=1&iobs=1) ; cloc="L0700" ; endif 

iexp=2 ; while(iexp<=2)
  cexp="0"iexp 
*******************************************************************************************************************************
   EXP1DIR=basedir "/adpu/SYS00000003_0LETKF_M000020L0400IADP_RTPx0.00_woWSM"
   EXP2DIR=basedir "/adpu/SYS00000003_0LETKF_M000040L0400IADP_RTPx0.00_woWSM"
   EXP3DIR=basedir "/adpu/SYS00000003_0LETKF_M000080L0400IADP_RTPx0.00_woWSM"
*******************************************************************************************************************************

  '!rm exp1dir exp2dir exp3dir'
  '!ln -s  'EXP1DIR'  exp1dir'
  '!ln -s  'EXP2DIR'  exp2dir'
  '!ln -s  'EXP3DIR'  exp3dir'

  'subdraw_time_rmse_3exp.gs'
  '!mv ./PNGfile/time_rmse.png ./EXP3figs/time_rmse_'EXPNAM'.png'

*  'subdraw_gmap_rmse-comp_2exp.gs'
*  '!mv ./PNGfile/gmap_rmse.png ./EXP2figs/gmap_rmse_comp_'EXPNAM'.png'

iexp=iexp+1  ;  endwhile
iobs=iobs+1  ;  endwhile
imem=imem+1  ;  endwhile

return
