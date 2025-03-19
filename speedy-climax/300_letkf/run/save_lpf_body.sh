SYS="SYS20200416_" # REWRITE(0)
SYS="SYS20200418_" # RESAMPLE OR NOT INTRODUCED

#===> WI options (fixed)
SMPTBL="1"       # sampling interval
LUTTBL="BLLUT"   # LUT of WI
STHTBL="0"       # 0: no weight smoothing (default), 1: weight smoothing

#===> LPF OPTION (usually fixed)
RTPTBL="2"    # 0: no relaxation  ;  1: RTPP  ;  2:RTPS (default)
LFACTOR=2.5   # (usually fixed) factor of background error covariance (only for dastype=3 or 4)
RSMTBL="1"    # 0: shared resampling noise (Roland et al. 2019; MWR)
              # 1: KK's Monte-Carlo Resampling (repeat M-times)
              #   (This parameter is currently used only for DASTYPE 2 or 3)

#===> DA  OPTION (fixed)
MEMTBL="40" ; OBSTBL="reg2" ; HSGTBL="500"  # Kotsuki et al. (2020; QJRMS)
#MEMTBL="40" ; OBSTBL="raob" ; HSGTBL="800"  # Kotsuki et al. (2020; QJRMS)


#===> LPF OPTION (tunable)
DASTYPE=3     # (usually fixed) 
  # 0 :: LETKF    ;                    mean update :: LETKF,   ptb update :: LETKF
  # 1 :: DEBUG    ;                    mean update :: LPF,     ptb update :: LETKF                 # just for debug
  # 2 :: LAPF     ;                    mean update :: LPF  ,   ptb update :: LAPF      resampling
  # 3 :: LPFGM1   ; LETKF_particle  +  mean update :: N/A  ,   ptb update :: LPFGM w   resampling

FGTTBL="1.00" # 0.00: no forget, 1.00: perfect forget (default for LPFGM of raob)
SIRPEF=2      # effective particle size for resampling (SIR)
RLXTBL="0.65" # RTPP ; Relaxation to Prior Perturbation
              # RTPP ==> RTPS
              # 0: no relaxation   1: perfect relaxation i.e., dXa=dXb
              # forced to be 0.00 if 
#------------------------------------------------------------------------------------------------#
SYYYY=1982  ;  SMM=02  ;  SDD=01  ;  SHH=00 ### initial date setting
EYYYY=1982  ;  EMM=02  ;  EDD=01  ;  EHH=00 ### final   date setting
EYYYY=1982  ;  EMM=02  ;  EDD=01  ;  EHH=18 ### final   date setting
EYYYY=1982  ;  EMM=02  ;  EDD=15  ;  EHH=18
EYYYY=1982  ;  EMM=02  ;  EDD=28  ;  EHH=18 ### final   date setting
EYYYY=1982  ;  EMM=05  ;  EDD=31  ;  EHH=18 ### final   date setting
#EYYYY=1982  ;  EMM=12  ;  EDD=31  ;  EHH=18 ### final   date setting
#EYYYY=1983  ;  EMM=02  ;  EDD=28  ;  EHH=18 ### final   date setting
#EYYYY=1983  ;  EMM=12  ;  EDD=31  ;  EHH=18 ### final   date setting
#EYYYY=1982  ;  EMM=06  ;  EDD=30  ;  EHH=18 ### final   date setting
