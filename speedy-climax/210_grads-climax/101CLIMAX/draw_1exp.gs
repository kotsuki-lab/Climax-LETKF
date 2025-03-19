basedir="/export/home/kotsuki/climax-letkf/SYS20240704_SPEEDY_LPFv1.00_LAB/climax/DATA" 
****
imem=1 ; while(imem<=1)
  if(imem=1); cmem="M000040" ; endif

iobs=1 ; while(iobs<=1)
  if(iobs=1); cobs="raob" ; endif

iexp=2 ; while(iexp<=2) 
  if(iexp=2) 
    EXP1DIR=basedir "/adpu/SYS00000003_0LETKF_M000020L0600IADP_RTPx0.00_woWSM"
    EXP1DIR=basedir "/adpu/SYS00000003_0LETKF_M000020L0500IADP_RTPx0.00_woWSM"
*    EXP1DIR=basedir "/adpu/SYS00000003_0LETKF_M000020L0700IADP_RTPx0.00_woWSM"
  endif 


  EXPNAM=cmem cobs cexp 

  say EXP1NAM


  '!rm exp1dir'
  '!ln -s  'EXP1DIR'  exp1dir'

*  'subdraw_rmse.gs'                ; '!mv ./PNGfile/time_rmse-sprd.png ./PNGfile/time_rmse-sprd_'EXPNAM'.png'
*  'subdraw_gmap-time_infl.gs'       ; '!mv ./PNGfile/time_infl-mul.png  ./PNGfile/time_infl-mul_'EXPNAM'.png'
*  'subdraw_gmap-time_kldv.gs'       ; '!mv ./PNGfile/time_infl-mul.png  ./PNGfile/time_infl-mul_'EXPNAM'.png'

*  'subdraw_time_rmse_1exp.gs'       ; '!mv ./PNGfile/time_rmse-sprd.png ./ANDREWfile/time_rmse-sprd_'EXPNAM'.png'
*  'subdraw_gmap_wvec_1exp.gs'      ; '!mv ./PNGfile/gmap_wvec.png      ./EXP1figs/gmap_wvec_'EXPNAM'.png'
*  'subdraw_gmap_Peff-Pnum_1exp.gs' ; 
*  'subdraw_gmap_maer_1exp.gs'
  'subdraw_gmap_4mae_1exp.gs'
*  'subdraw_gmap_kldv_1exp.gs'

*iexp=iexp+1  ;  endwhile
iexp=iexp+1  ;  endwhile
iobs=iobs+1  ;  endwhile
imem=imem+1  ;  endwhile

return
