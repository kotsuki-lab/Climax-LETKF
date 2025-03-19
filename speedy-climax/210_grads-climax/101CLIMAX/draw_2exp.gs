basedir="/export/home/kotsuki/climax-letkf/SYS20250121_SPEEDY-ClimaX_LETKFv1.13/speedy-climax/DATA"  
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
    EXP1DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-0LETKF_M000020L0600_inf1.00_RTPP0.90_ad0.00_po0.00"
    EXP2DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-0LETKF_M000020L0600_inf1.00_RTPS1.20_ad0.00_po0.00"
*******************************************************************************************************************************

  '!rm exp1dir exp2dir'
  '!ln -s  'EXP1DIR'  exp1dir'
  '!ln -s  'EXP2DIR'  exp2dir'

*  'subdraw_time_rmse_2exp.gs'
*  '!mv ./PNGfile/time_rmse.png ./EXP2figs/time_rmse_'EXPNAM'.png'

  'subdraw_time_rmse-peff_2exp.gs'
  '!mv ./PNGfile/time_rmse.png ./EXP2figs/time_rmse_'EXPNAM'.png'

*  'subdraw_gmap_rmse-comp_2exp.gs'
*  '!mv ./PNGfile/gmap_rmse.png ./EXP2figs/gmap_rmse_comp_'EXPNAM'.png'

iexp=iexp+1  ;  endwhile
iobs=iobs+1  ;  endwhile
imem=imem+1  ;  endwhile

return
