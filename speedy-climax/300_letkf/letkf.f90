PROGRAM letkf
!=======================================================================
!
! [PURPOSE:] Main program of LETKF
!
! [HISTORY:]
!   01/16/2009 Takemasa Miyoshi  created
!   01/16/2009 Takemasa Miyoshi  created
!   06/29/2024 Shunji   Kotsuki  updated for speedy/climax
!
!=======================================================================
!$USE OMP_LIB
  USE common
  USE common_mpi
  USE common_time
  USE common_XXXXXX
  USE common_mpi_XXXXXX
  USE common_letkf
  USE common_lpf
  USE letkf_obs
  USE letkf_tools
  USE interpolate

  IMPLICIT NONE
  REAL(r_size),ALLOCATABLE :: gues3d(:,:,:,:)
  REAL(r_size),ALLOCATABLE :: gues2d(:,:,:)
  REAL(r_size),ALLOCATABLE :: anal3d(:,:,:,:)
  REAL(r_size),ALLOCATABLE :: anal2d(:,:,:)
  !!REAL(r_size) :: rtimer00,rtimer
  INTEGER :: ierr
  CHARACTER(11) :: stdoutf='NOUT-000000'
  CHARACTER(4)  :: guesf='gs00'

  namelist / letkf_param / sigma_obs, nbv, pymdh, ymdh,                    &
    logic_wout, logic_wsth, logic_wint, dastype, cov_infl_mul, s_additive, hpo_weight,   &
    resample_m, type_wnorm, type_pfmtx, type_relax, alph_relax, fgt_factor, tau_nmlize, snk_lambda, gamma_gmpf
!-----------------------------------------------------------------------
! Initial settings
!-----------------------------------------------------------------------
  CALL initialize_mpi
  CALL set_timer
  rtimer00 = MPI_WTIME() ; rtimer01 = rtimer00

 
 !==> SK 20180607 for exp.
  open(1,file='letkf.cnf')
    read(1,nml=letkf_param)
  close(1)
!
  WRITE(stdoutf(9:11), '(I3.3)') myrank
  WRITE(6,'(3A,I3.3)') 'STDOUT goes to ',stdoutf,' for MYRANK ', myrank
  OPEN(6,FILE=stdoutf)
  WRITE(6,'(A,I3.3,2A)') 'MYRANK=',myrank,', STDOUTF=',stdoutf
!
  WRITE(6,'(A)') '============================================='
  WRITE(6,'(A)') '  LOCAL ENSEMBLE TRANSFORM KALMAN FILTERING  '
  WRITE(6,'(A)') '                                             '
  WRITE(6,'(A)') '   LL      EEEEEE  TTTTTT  KK  KK  FFFFFF    '
  WRITE(6,'(A)') '   LL      EE        TT    KK KK   FF        '
  WRITE(6,'(A)') '   LL      EEEEE     TT    KKK     FFFFF     '
  WRITE(6,'(A)') '   LL      EE        TT    KK KK   FF        '
  WRITE(6,'(A)') '   LLLLLL  EEEEEE    TT    KK  KK  FF        '
  WRITE(6,'(A)') '                                             '
  WRITE(6,'(A)') '             WITHOUT LOCAL PATCH             '
  WRITE(6,'(A)') '                                             '
  WRITE(6,'(A)') '          Coded by Takemasa Miyoshi          '
  WRITE(6,'(A)') '  Based on Ott et al (2004) and Hunt (2005)  '
  WRITE(6,'(A)') '  Tested by Miyoshi and Yamane (2006)        '
  WRITE(6,'(A)') '============================================='
  WRITE(6,'(A)') '              LETKF PARAMETERS               '
  WRITE(6,'(A)') '============================================='
  WRITE(6,'(A,I15,  A)') '   nbv             :',nbv,         "   : # of ensemble size"
  WRITE(6,'(A,I15,  A)') '   nslots          :',nslots,      "   : # of obs slots "
  WRITE(6,'(A,I15,  A)') '   nbslot          :',nbslot,      "   : base obs slots "
  WRITE(6,'(A,F15.5,A)') '   sigma_obs       :',sigma_obs,   "   : horizontal loc scale (m) "
  WRITE(6,'(A,F15.5,A)') '   sigma_obsv      :',sigma_obsv,  "   : vertical   loc scale (logp) "
  WRITE(6,'(A,F15.5,A)') '   sigma_obst      :',sigma_obst,  "   : temporal   loc scale"
  WRITE(6,'(A,I15,  A)') '   cymdh           :',ymdh,        "   : present date"
  WRITE(6,'(A,I15,  A)') '   pymdh           :',pymdh,       "   : last date"   
  WRITE(6,'(A,L15,  A)') '   logic_wout      :',logic_wout,  "   : weight output"
  WRITE(6,'(A,L15,  A)') '   logic_wsth      :',logic_wsth,  "   : weight smoothing"
  WRITE(6,'(A,L15,  A)') '   logic_wint      :',logic_wint,  "   : weight interpolation"
  WRITE(6,'(A,F15.5,A)') '   cov_infl_mul    :',cov_infl_mul,"   : covariance inflation factor"
  WRITE(6,'(A,I15,  A)') '   type_relax      :',type_relax,  "   : 0:no, 1:RTPP, 2:RTPS, 3:ATAL, 4:ATAE, 5:RTPL, 6:RTPE"
  WRITE(6,'(A,F15.5,A)') '   alph_relax      :',alph_relax,  "   : relaxation alpha"
  WRITE(6,'(A,F15.5,A)') '   s_additive      :',s_additive,  "   : additive inflation factor"
  WRITE(6,'(A,F15.5,A)') '   hpo_weight      :',hpo_weight,  "   : Hybrid LETKF-PO, "
  WRITE(6,'(A)') '============================================='
  WRITE(6,'(A)') '              LPF PARAMETERS                 '
  WRITE(6,'(A)') '============================================='
  WRITE(6,'(A,I15,  A)') '   LPF,resample_m  :',resample_m,  "   : resampling threshold (N0)"
  WRITE(6,'(A,F15.5,A)') '   LPF,fgt_factor  :',fgt_factor,  "   : forgetting factor of SIS"
  WRITE(6,'(A,F15.5,A)') '   LPF,tau_nmlize  :',tau_nmlize,  "   : tau inflation for weight"
  WRITE(6,'(A,F15.5,A)') '   LPF,gamma_gmpf  :',gamma_gmpf,  "   : gamma of LPFGM" 
  WRITE(6,'(A,F15.5,A)') '   LPF,snk_lambda  :',snk_lambda,  "   : lambda of Sinkhorn Resampling"    
  WRITE(6,'(A,I15,  A)') '   LPF,type_wnorm  :',type_wnorm,  "   : 0: R-1, Classical PF    1: (R+HPfHT)-1  Hoteit+(2008)"
  WRITE(6,'(A,I15,  A)') '   LPF,type_pfmtx  :',type_pfmtx,  "   : 0: Potthast+(2019;MWR)  1: Kondo+(2020)   2:MPF   3:OT"
  WRITE(6,'(A)') '============================================='
  WRITE(6,'(A)') '              DA SCHEME                      '
  WRITE(6,'(A)') '============================================='
  WRITE(6,'(A,I15,  A)') '   LPF,dastype     :',dastype
  IF( dastype==0 ) WRITE(6,'(A)') '   dastype=0 ::   mean update :: LETKF,   ptb update :: LETKF'
  IF( dastype==1 ) WRITE(6,'(A)') '   dastype=1 ::   mean update :: LETKF,   ptb update :: DEBUG, LPF only for mean update'
  IF( dastype==2 ) WRITE(6,'(A)') '   dastype=2 ::   mean update :: LPF  ,   ptb update :: LAPF  resampling'
  IF( dastype==3 ) WRITE(6,'(A)') '   dastype=3 ::   mean update :: LPF  ,   ptb update :: LPFGM resampling'

  WRITE(6,'(A)') '============================================='
!  CALL set_common_climax
!  CALL set_common_mpi_climax
  CALL set_common_XXXXXX
  CALL set_common_mpi_XXXXXX
  ALLOCATE(gues3d(nij1,nlev,nbv,nv3d), gues2d(nij1,nbv,nv2d))
  ALLOCATE(anal3d(nij1,nlev,nbv,nv3d), anal2d(nij1,nbv,nv2d))
!
  rtimer = MPI_WTIME()
!  WRITE(6,'(A,2F10.2)') '### TIMER(INITIALIZE):',rtimer-rtimer01,rtimer-rtimer00
  rtimerl(1) = rtimer-rtimer01
  rtimer01 = rtimer
!-----------------------------------------------------------------------
! Observations
!-----------------------------------------------------------------------
  !
  ! CONVENTIONAL OBS
  !
  CALL set_letkf_obs
!
  rtimer = MPI_WTIME()
!  WRITE(6,'(A,2F10.2)') '### TIMER(READ_OBS):',rtimer-rtimer01,rtimer-rtimer00
  rtimerl(2) = rtimer-rtimer01
  rtimer01=rtimer
!-----------------------------------------------------------------------
! First guess ensemble
!-----------------------------------------------------------------------
  !
  ! READ GUES
  !
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  WRITE(guesf(3:4),'(I2.2)') nbslot
  !CALL read_ens_mpi(guesf,nbv,gues3d,gues2d)
  CALL read_ens_mpi("gues",nbv,gues3d,gues2d)  ! 20240804 SK
  !
  ! WRITE ENS MEAN and SPRD
  !
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  CALL write_ensmspr_mpi('gues',nbv,gues3d,gues2d)
!
  rtimer = MPI_WTIME()
!  WRITE(6,'(A,2F10.2)') '### TIMER(READ_GUES):',rtimer-rtimer01,rtimer-rtimer00
  rtimerl(3) = rtimer-rtimer01
  rtimer01=rtimer
!-----------------------------------------------------------------------
! Data Assimilation
!-----------------------------------------------------------------------
  !
  ! LETKF
  !

  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  CALL das_letkf(gues3d,gues2d,anal3d,anal2d)

  rtimer = MPI_WTIME()
!  WRITE(6,'(A,2F10.2)') '### TIMER(DAS_LETKF):',rtimer-rtimer01,rtimer-rtimer00
  rtimerl(4) = rtimer-rtimer01
  rtimer01=rtimer
!-----------------------------------------------------------------------
! Analysis ensemble
!-----------------------------------------------------------------------
  !
  ! WRITE ANAL
  !
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  CALL write_ens_mpi('anal',nbv,anal3d,anal2d)
  !
  ! WRITE ENS MEAN and SPRD
  !
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  CALL write_ensmspr_mpi('anal',nbv,anal3d,anal2d)
!
  rtimer = MPI_WTIME()
!  WRITE(6,'(A,2F10.2)') '### TIMER(WRITE_ANAL):',rtimer-rtimer01,rtimer-rtimer00
  rtimerl(5) = rtimer-rtimer01
  rtimer01=rtimer
!-----------------------------------------------------------------------
! Monitor
!-----------------------------------------------------------------------
  !SK follows KK!CALL monit_mean('gues')
  !SK follows KK!CALL monit_mean('anal')
!
  rtimer = MPI_WTIME()
! WRITE(6,'(A,2F10.2)') '### TIMER(MONIT_MEAN):',rtimer-rtimer01,rtimer-rtimer00
  rtimerl(6) = rtimer-rtimer01
  rtimer01=rtimer

  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  CALL MPI_Gather(rtimerl,slot,MPI_REAL8,rtimer_all,slot,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
  !KK!CALL MPI_Gather(ltimerl,slot+1,MPI_REAL8,ltimer_all,slot+1,MPI_REAL8,0,MPI_COMM_WORLD,ierr)

  WRITE(6,*) ''
  WRITE(6,'(A,2F10.2,A)') '### TIMER(INITIALIZE):', rtimerl(1), rtimerl(1)/sum(rtimerl(:))*100, ' %'
  WRITE(6,'(A,2F10.2,A)') '### TIMER(READ_OBS)  :', rtimerl(2), rtimerl(2)/sum(rtimerl(:))*100, ' %'
  WRITE(6,'(A,2F10.2,A)') '### TIMER(READ_GUES) :', rtimerl(3), rtimerl(3)/sum(rtimerl(:))*100, ' %'
  WRITE(6,'(A,2F10.2,A)') '### TIMER(DAS_LETKF) :', rtimerl(4), rtimerl(4)/sum(rtimerl(:))*100, ' %'
  WRITE(6,'(A,2F10.2,A)') '### TIMER(WRITE_ANAL):', rtimerl(5), rtimerl(5)/sum(rtimerl(:))*100, ' %'
  WRITE(6,'(A,2F10.2,A)') '### TIMER(MONIT_MEAN):', rtimerl(6), rtimerl(6)/sum(rtimerl(:))*100, ' %'
  WRITE(6,'(A,2F10.2,A)') '### TIMER(TOTAL)     :', sum(rtimerl(:)), sum(rtimerl(:))/sum(rtimerl(:))*100, ' %'

!-----------------------------------------------------------------------
! Finalize
!-----------------------------------------------------------------------
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  CALL finalize_mpi
  STOP
END PROGRAM letkf
