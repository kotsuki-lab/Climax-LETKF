basedir="/export/home/kotsuki/climax-letkf/SYS20241002_SPEEDY-ClimaX_LETKFv1.09_CLIMAX/speedy-climax/DATA/raob_wb" 
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
   EXP1DIR=basedir "/V240930_CLIMAXdef-0LETKF_M000020L0500_RTPx0.00_ad0.00_po0.00"
   EXP2DIR=basedir "/V240930_CLIMAXdef-0LETKF_M000020L0600_RTPx0.00_ad0.00_po0.00"
   EXP3DIR=basedir "/V240930_CLIMAXdef-0LETKF_M000020L0700_RTPx0.00_ad0.00_po0.00"
   EXP4DIR=basedir "/V240930_CLIMAXdef-0LETKF_M000020L0800_RTPx0.00_ad0.00_po0.00"

*   EXP1DIR=basedir "/SYS20240805_CLIMAXres-0LETKF_M000020L0500IADP_RTPx0.00_woWSM"
*   EXP2DIR=basedir "/SYS20240805_CLIMAXres-0LETKF_M000020L0600IADP_RTPx0.00_woWSM"
*   EXP3DIR=basedir "/SYS20240805_CLIMAXres-0LETKF_M000020L0700IADP_RTPx0.00_woWSM"
*   EXP4DIR=basedir "/SYS20240805_CLIMAXres-0LETKF_M000020L0800IADP_RTPx0.00_woWSM"
*******************************************************************************************************************************

  '!rm exp1dir exp2dir exp3dir exp4dir'
  '!ln -s  'EXP1DIR'  exp1dir'
  '!ln -s  'EXP2DIR'  exp2dir'
  '!ln -s  'EXP3DIR'  exp3dir'
  '!ln -s  'EXP4DIR'  exp4dir'

  'subdraw_time_rmse_4exp.gs'
  '!mv ./PNGfile/time_rmse.png ./EXP4figs/time_rmse_'EXPNAM'.png'


iexp=iexp+1  ;  endwhile
iobs=iobs+1  ;  endwhile
imem=imem+1  ;  endwhile

return
