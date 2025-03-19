basedir="/export/home/kotsuki/climax-letkf/SYS20250121_SPEEDY-ClimaX_LETKFv1.13/speedy-climax/DATA" 
****
imem=1 ; while(imem<=1)
  if(imem=1); cmem="M000020" ; endif
  if(imem=2); cmem="M000040" ; endif
  if(imem=3); cmem="M000080" ; endif

iobs=1 ; while(iobs<=1)
  if(iobs=1); cobs="raob" ; endif

  cloc=Lxxxx
  if(imem=1&iobs=1) ; cloc="L0700" ; endif 

iexp=3 ; while(iexp<=3)
  cexp="0"iexp 
******************************************************************************************************************************* adpu **
*** 20 mem ***
  if(imem=1)
*    EXP1DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-0LETKF_M000020L0600_inf1.00_RTPP0.70_ad0.00_po0.00"
*    EXP2DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-0LETKF_M000020L0600_inf1.00_RTPP0.75_ad0.00_po0.00"
*    EXP3DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-0LETKF_M000020L0600_inf1.00_RTPP0.80_ad0.00_po0.00"
*    EXP4DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-0LETKF_M000020L0600_inf1.00_RTPP0.85_ad0.00_po0.00"
*    EXP5DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-0LETKF_M000020L0600_inf1.00_RTPP0.90_ad0.00_po0.00"
*    EXP6DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-0LETKF_M000020L0600_inf1.00_RTPP0.95_ad0.00_po0.00"

*    EXP1DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP0.70_ad0.00_po0.00"
*    EXP2DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP0.75_ad0.00_po0.00"
*    EXP3DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP0.80_ad0.00_po0.00"
*    EXP4DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP0.85_ad0.00_po0.00"
*    EXP5DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP0.90_ad0.00_po0.00"
*    EXP6DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP0.95_ad0.00_po0.00"

*    EXP1DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-0LETKF_M000020L0600_inf1.00_RTPS0.90_ad0.00_po0.00"
*    EXP2DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-0LETKF_M000020L0600_inf1.00_RTPS1.00_ad0.00_po0.00"
*    EXP3DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-0LETKF_M000020L0600_inf1.00_RTPS1.10_ad0.00_po0.00"
*    EXP4DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-0LETKF_M000020L0600_inf1.00_RTPS1.20_ad0.00_po0.00"
*    EXP5DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-0LETKF_M000020L0600_inf1.00_RTPS1.30_ad0.00_po0.00"
*    EXP6DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-0LETKF_M000020L0600_inf1.00_RTPS1.40_ad0.00_po0.00"

*    EXP1DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS0.90_ad0.00_po0.00"
*    EXP2DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS1.00_ad0.00_po0.00"
*    EXP3DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS1.10_ad0.00_po0.00"
*    EXP4DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS1.20_ad0.00_po0.00"
*    EXP5DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS1.30_ad0.00_po0.00"
*    EXP6DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS1.40_ad0.00_po0.00"

*    EXP1DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-0LETKF_M000040L0600_inf1.00_RTPP0.70_ad0.00_po0.00"
*    EXP2DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-0LETKF_M000040L0600_inf1.00_RTPP0.75_ad0.00_po0.00"
*    EXP3DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-0LETKF_M000040L0600_inf1.00_RTPP0.80_ad0.00_po0.00"
*    EXP4DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-0LETKF_M000040L0600_inf1.00_RTPP0.85_ad0.00_po0.00"
*    EXP5DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-0LETKF_M000040L0600_inf1.00_RTPP0.90_ad0.00_po0.00"
*    EXP6DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-0LETKF_M000040L0600_inf1.00_RTPP0.95_ad0.00_po0.00"

    EXP1DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-0LETKF_M000040L0600_inf1.00_RTPS0.90_ad0.00_po0.00"
    EXP2DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-0LETKF_M000040L0600_inf1.00_RTPS1.00_ad0.00_po0.00"
    EXP3DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-0LETKF_M000040L0600_inf1.00_RTPS1.10_ad0.00_po0.00"
    EXP4DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-0LETKF_M000040L0600_inf1.00_RTPS1.20_ad0.00_po0.00"
    EXP5DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-0LETKF_M000040L0600_inf1.00_RTPS1.30_ad0.00_po0.00"
    EXP6DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-0LETKF_M000040L0600_inf1.00_RTPS1.40_ad0.00_po0.00"

*    EXP1DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-3LPFGM_M000040L0600_inf1.00_RTPP0.70_ad0.00_po0.00_rmxKK_nrmPF_gma1.5_fgt1.00_tau0.00_nzo000_lmd040"
*    EXP2DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-3LPFGM_M000040L0600_inf1.00_RTPP0.75_ad0.00_po0.00_rmxKK_nrmPF_gma1.5_fgt1.00_tau0.00_nzo000_lmd040"
*    EXP3DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-3LPFGM_M000040L0600_inf1.00_RTPP0.80_ad0.00_po0.00_rmxKK_nrmPF_gma1.5_fgt1.00_tau0.00_nzo000_lmd040"
*    EXP4DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-3LPFGM_M000040L0600_inf1.00_RTPP0.85_ad0.00_po0.00_rmxKK_nrmPF_gma1.5_fgt1.00_tau0.00_nzo000_lmd040"
*    EXP5DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-3LPFGM_M000040L0600_inf1.00_RTPP0.90_ad0.00_po0.00_rmxKK_nrmPF_gma1.5_fgt1.00_tau0.00_nzo000_lmd040"
*    EXP6DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-3LPFGM_M000040L0600_inf1.00_RTPP0.95_ad0.00_po0.00_rmxKK_nrmPF_gma1.5_fgt1.00_tau0.00_nzo000_lmd040"

*    EXP1DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-3LPFGM_M000040L0600_inf1.00_RTPS0.90_ad0.00_po0.00_rmxKK_nrmPF_gma1.5_fgt1.00_tau0.00_nzo000_lmd040"
*    EXP2DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-3LPFGM_M000040L0600_inf1.00_RTPS1.00_ad0.00_po0.00_rmxKK_nrmPF_gma1.5_fgt1.00_tau0.00_nzo000_lmd040"
*    EXP3DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-3LPFGM_M000040L0600_inf1.00_RTPS1.10_ad0.00_po0.00_rmxKK_nrmPF_gma1.5_fgt1.00_tau0.00_nzo000_lmd040"
*    EXP4DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-3LPFGM_M000040L0600_inf1.00_RTPS1.20_ad0.00_po0.00_rmxKK_nrmPF_gma1.5_fgt1.00_tau0.00_nzo000_lmd040"
*    EXP5DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-3LPFGM_M000040L0600_inf1.00_RTPS1.30_ad0.00_po0.00_rmxKK_nrmPF_gma1.5_fgt1.00_tau0.00_nzo000_lmd040"
*    EXP6DIR=basedir "/prepbufr_dnst1_sfc1_Tv12_n0/V250122_CLIMAXdef-3LPFGM_M000040L0600_inf1.00_RTPS1.40_ad0.00_po0.00_rmxKK_nrmPF_gma1.5_fgt1.00_tau0.00_nzo000_lmd040"

  endif

*******************************************************************************************************************************

  '!rm exp1dir exp2dir exp3dir exp4dir exp5dir exp6dir'
  '!ln -s  'EXP1DIR'  exp1dir'
  '!ln -s  'EXP2DIR'  exp2dir'
  '!ln -s  'EXP3DIR'  exp3dir'
  '!ln -s  'EXP4DIR'  exp4dir'
  '!ln -s  'EXP5DIR'  exp5dir'
  '!ln -s  'EXP6DIR'  exp6dir'

  'subdraw_time_rmse_6exp.gs'
  '!mv ./PNGfile/time_rmse.png ./EXP6figs/time_rmse_'EXPNAM'.png'

iexp=iexp+1  ;  endwhile
iobs=iobs+1  ;  endwhile
imem=imem+1  ;  endwhile

return
