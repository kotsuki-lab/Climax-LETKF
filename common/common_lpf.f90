MODULE common_lpf
!=======================================================================
!
! [PURPOSE:] Local Particle Filter (LPF)
!            Model Independent Core Module
!
! [REFERENCES:]
!  [1] Kotsuki et al.,   2022 (GMD)  :: basic implementation
!  [2] Oishi and Kotsuki 2023 (SOLA) :: Sinkhorn 
!
! [HISTORY:]
!  09/03/2024 Shunji Kotsuki  Modified at Chiba U., Chiba
!  09/04/2024 Shunji Kotsuki  Shinkhorn Resampling (Oishi and Kotsuki 2023; SOLA)
!
!=======================================================================
!$USE OMP_LIB
  USE common
  USE common_mtx
!  USE ot_resampling_mod

  IMPLICIT NONE

  PUBLIC
!=======================================================================
!  LEKF Model Independent Parameters
!=======================================================================
  INTEGER,     SAVE :: type_pfmtx = 0      ! 0: Multinomial w/ shared noise ; Potthast et al. (2019;MWR)
                                           ! 1: Monte-Carlo Resampling      ; Kotsuki+2022 (GMD)
                                           ! 2: Marginal particle filter    ; not supported as of 20240904 (from textbook)
                                           ! 3: Sinkhorn obs   space        ; Oishi and Kotsuki 2024 (SOLA)
                                           ! 4: Sinkhorn state space        ; Oishi and Kotsuki 2024 (SOLA)
  INTEGER,     SAVE :: type_wnorm = 0      ! 0: R-1 (classical PF),       1: (R+HPfHT)-1  :: Hoteit+(2008), Stordal+(2011)
  REAL(r_size),SAVE :: tau_nmlize  = 0.0d0 ! SK 20240903 ; 0:w=PF, 1: w=1/m
  REAL(r_size),SAVE :: snk_lambda = 40.0d0 ! lambda of sinkhorn algorithm
                                           ! lambda=40 --> epsilon=0.025 Oishi and Kotsuki (2023; SOLA)
  REAL(r_size),SAVE :: gamma_gmpf = 1.5d0  ! gamma of LPFGM
CONTAINS
!=======================================================================
!  Main Subroutine of LPF Core
!   INPUT
!     nobs             : array size, but only first nobsl elements are used
!     nobsl            : total number of observation assimilated at the point
!     hdxb(nobs,nbv)   : obs operator times fcst ens perturbations
!     rdiag(nobs)      : observation error variance
!     rloc(nobs)       : localization weighting function
!     dep(nobs)        : observation departure (yo-Hxb)
!   OUTPUT
!     trans(nbv,nbv) : transformation matrix
!=======================================================================
SUBROUTINE lpf_core(nobs,nobsl,hdxb,rdiag,rloc,dep,parm_infl,gr_common,dastype,cst,asis,peff,pvec,pmat,DBG_REPLACE)
  IMPLICIT NONE
  INTEGER,     INTENT(IN)    :: nobs
  INTEGER,     INTENT(IN)    :: nobsl
  INTEGER,     INTENT(IN)    :: dastype    ! 1: DEBUG, 2: LPF, 3:LPFGM
  LOGICAL,     INTENT(IN)    :: DBG_REPLACE
  REAL(r_size),INTENT(IN)    :: hdxb(1:nobs,1:nbv)
  REAL(r_size),INTENT(IN)    :: rdiag(1:nobs)
  REAL(r_size),INTENT(IN)    :: rloc(1:nobs)
  REAL(r_size),INTENT(IN)    :: dep(1:nobs)
  REAL(r_size),INTENT(IN)    :: gr_common(1:nbv,1:nbv)
  REAL(r_size),INTENT(INOUT) :: parm_infl
  REAL(r_size) :: hdxb_rinv(nobsl,nbv)
  REAL(r_size) :: eivec(nbv,nbv)
  REAL(r_size) :: eival(nbv)
  REAL(r_size) :: pa(nbv,nbv)
  REAL(r_size) :: work1(nbv,nbv)
  REAL(r_size) :: work2(nbv,nobsl)
  REAL(r_size) :: rho
  REAL(r_size) :: parm(4),sigma_o,gain
  REAL(r_size),PARAMETER :: sigma_b = 0.04d0 !error stdev of parm_infl

  REAL(r_size),INTENT(IN)    :: cst(nbv,nbv)   ! cost matrix state-space distance b/w particles (x(i)-x(j))**2.0d0
  REAL(r_size),INTENT(INOUT) :: asis(1:nbv)
  REAL(r_size),INTENT(OUT)   :: peff
  REAL(r_size),INTENT(OUT)   :: pvec(nbv)      ! LPF when updating only ensemble mean
  REAL(r_size),INTENT(OUT)   :: pmat(nbv,nbv)  ! LPF when resampling (mean and ptb)
  REAL(r_size) :: sqpf, qtmp, swgh
  REAL(r_size) :: qpf(1:nbv), acc(1:nbv), pfwgh(1:nbv), rand(1:nbv), grnd(1:nbv), dep2_Ri(1:nbv)
  REAL(r_size) :: pmat_one(nbv,nbv), pmat_su(nbv,nbv), pmat_kk(nbv,nbv)

  !==> KK's Monte-Carlo Resampling (Kotsuki+2022, GMD)
  INTEGER                :: nmcsmp

  !==> Marginal Particle Filter ; parameters based on personal communication w/ K.Kondo of MRI ; M=040
  INTEGER,PARAMETER :: nsu          = 100
  INTEGER,PARAMETER :: a_sample     = 100
  INTEGER,PARAMETER :: b_sample     =  98
  !REAL(8),PARAMETER :: a_factor     =  0.088d0
  !REAL(8),PARAMETER :: b_factor     = -0.088d0
  REAL(8),PARAMETER :: a_factor     =  0.040d0
  REAL(8),PARAMETER :: b_factor     = -0.040d0
  REAL(8)           :: c_factor, d_factor, sss1, sss2, ppp, qqq

  !==> optimal transport (or Sinkhorn)
  LOGICAL, PARAMETER  :: snk_monit = .false.
  REAL(r_size) :: pfcst(nbv,nbv)    ! cost matrix (obs space)
  REAL(r_size) :: snk_cost(nbv,nbv) ! cost matrix (used in Sinkhorn)
  REAL(r_size) :: costSU, costKK, costOT, erroSU, erroKK, erroOT

  !==> adaptive resampling amplitudes ;;  Potthast et al. (2019; MWR)
  REAL(r_size) :: resample_factor
  REAL(r_size),PARAMETER :: alpha_smooth = 0.05d0 
  REAL(r_size),PARAMETER :: rho0         = 1.0d0
  REAL(r_size),PARAMETER :: rho1         = 1.4d0
  REAL(r_size),PARAMETER :: ccc0         = 0.02d0
  REAL(r_size),PARAMETER :: ccc1         = 0.20d0

  INTEGER :: i,j,k
  IF(nobsl == 0) THEN 
    pvec(:)   = 1.0d0 / dble( nbv )
    peff      = DBLE(nbv)
    pmat(:,:) = 0.0d0
    DO j=1,nbv ; pmat(j,j) = 1.0d0 ; END DO ! no resampling 
    RETURN
  ELSE
!-----------------------------------------------------------------------
!  hdxb Rinv
!-----------------------------------------------------------------------
  DO j=1,nbv
    DO i=1,nobsl
      hdxb_rinv(i,j) = hdxb(i,j) / rdiag(i) * rloc(i)
    END DO
  END DO
!-----------------------------------------------------------------------
!  transport cost in obs space
!-----------------------------------------------------------------------
  IF( type_pfmtx==3 .or. type_pfmtx==4 )THEN
    pfcst=0.0d0
    DO j=1,nbv
      DO i=j+1,nbv
        DO k=1,nobsl
          pfcst(i,j)=pfcst(i,j)+ (abs(hdxb_rinv(k,j)-hdxb(k,i)/DBLE(nobsl)))**2.0d0
        END DO
        pfcst(j,i) = pfcst(i,j)
      END DO
    END DO
    !!DO i=1,nbv
    !!  PRINT '(I3,25F10.2)',i,pfcst(i,1:25) 
    !!END DO
  ENDIF
!-----------------------------------------------------------------------    
! Likelihood Computation based on Gaussian likelihood
!-----------------------------------------------------------------------
  ! dep   :: yo     - Hxf(mean)
  ! hdxb  :: Hxf(i) - Hxf(mean)
  ! rdiag :: err*err (i.e., variance)
  ! rloc  :: 0-1

  !CALL calc_pfwgh_norml(nobs,nobsl,nbv,dep,hdxb,rloc,rdiag,pfwgh)
  CALL   calc_pfwgh_kkver(nobs,nobsl,nbv,dep,hdxb,rloc,rdiag,asis,pfwgh)
  IF( dastype==3 .and. type_wnorm==1 )THEN
  	CALL calc_pfwgh_hphtr(nobs,nobsl,nbv,dep,hdxb,rloc,rdiag,asis,pfwgh,GAMMA_GMPF)
  ENDIF

  swgh     = sum( pfwgh(:) )
  pfwgh(:) = pfwgh(:) / swgh                                                   ! weight of particles
  pfwgh(:) = (1.0d0-tau_nmlize)*pfwgh(:) + tau_nmlize*(1.0d0/REAL(nbv,r_size)) ! tau inflation

  asis(:)  = pfwgh(:)
  peff     = 1.0d0  / sum( pfwgh(:)**2.0d0 ) ! effective particle size

  acc(:)   = 0.0d0
  acc(1)   = pfwgh(1)
  DO j=2,nbv
    acc(j) = acc(j-1) + pfwgh(j)
  END DO
  pvec(:)   = pfwgh(:)

!-----------------------------------------------------------------------    
! adaptive inflation (Potthast et al. 2018)
!-----------------------------------------------------------------------
  parm = 0.0d0
  DO i=1,nobsl
    parm(1) = parm(1) + dep(i)*dep(i)/rdiag(i) * rloc(i)
  END DO
  DO j=1,nbv
    DO i=1,nobsl
      parm(2) = parm(2) + hdxb_rinv(i,j) * hdxb(i,j)
    END DO
  END DO
  parm(2) = parm(2) / REAL(nbv-1,r_size)
  parm(3) = SUM(rloc(1:nobsl))
!Pottast2018!  parm(4) = (parm(1)-parm(3))/parm(2) ! diagnosed inflation
!Pottast2018!  print '(3f10.4)', parm_infl, parm(4), ( 1.0d0 - alpha_smooth )*parm_infl + alpha_smooth*parm(4)
!Pottast2018!  parm_infl = ( 1.0d0 - alpha_smooth )*parm_infl + alpha_smooth*parm(4)

  parm(4) = (parm(1)-parm(3))/parm(2) - parm_infl
!  sigma_o = 1.0d0/REAL(nobsl,r_size)/MAXVAL(rloc(1:nobsl))
  sigma_o = 2.0d0/parm(3)*((parm_infl*parm(2)+parm(3))/parm(2))**2
  gain = sigma_b**2 / (sigma_o + sigma_b**2)
!CHECK!  print '(4f10.4)', parm_infl, parm(4), gain, parm_infl + gain * parm(4)
  parm_infl = parm_infl + gain * parm(4)

  !==> calc resampling factor
  IF( parm_infl < rho0 )THEN
    resample_factor = ccc0
  ELSE IF( parm_infl > rho1 )THEN
    resample_factor = ccc1
  ELSE
    resample_factor = ccc0 + (ccc1-ccc0)*(parm_infl-rho0)/(rho1-rho0)
  END IF
!CHECK!  print '(a, 4f10.4)', "  resample_factor :: ", rho0, parm_infl, rho1, resample_factor 

!-----------------------------------------------------------------------    
! Resampling with random numbers
!-----------------------------------------------------------------------
  IF( type_pfmtx==0 )THEN ! 0: Potthast et al. (2019;MWR), MR-based resampling
    CALL get_resampling_mtx('MR','ON',nbv,acc,pmat)
    !==> inflation by adding noise
    DO j=1,nbv
      grnd(1:nbv) = gr_common(1:nbv,j) * resample_factor
      swgh        = sum( grnd(1:nbv) ) / dble( nbv )
      grnd(1:nbv) = grnd(1:nbv) - swgh                ! so that mean is zero
      pmat(1:nbv,j) =      pmat(1:nbv,j) + grnd(1:nbv)
    END  DO
  ELSE IF( type_pfmtx==1 )THEN ! 1: KK's Monte-Carlo Resampling
    pmat(:,:) = 0.0d0
    !nmcsmp = nbv    ! 20200528 based on discussion w/ KK 
    nmcsmp = nbv*5 ! SK's basic test (appendix)
    DO j=1,nmcsmp ! Monte-Carlo Resampling M times
      CALL get_resampling_mtx('MR','ON',nbv,acc,pmat_one)
      pmat(:,:) = pmat(:,:) + pmat_one(:,:) / dble(nmcsmp)
    END DO
  ELSE IF( type_pfmtx==2 )THEN ! 2: Marginal Particle Filter
    !==> get coefficients of MPF
    sss1 = 0.0d0  ;  sss2 = 0.0d0
    do i=1,a_sample
      sss1 = sss1 + a_factor
      sss2 = sss2 + a_factor**2.0d0  ;  end do
    do i=1,b_sample
      sss1 = sss1 + b_factor
      sss2 = sss2 + b_factor**2.0d0  ;  end do

    ppp = 1.0d0 - sss1  ;  qqq = 1.0d0 - sss2
    c_factor = 0.5d0 * ( ppp - dsqrt(  ppp**2.0d0 - 2.0d0*(ppp**2.0d0 - qqq) ) )
    d_factor = 0.5d0 * ( ppp + dsqrt(  ppp**2.0d0 - 2.0d0*(ppp**2.0d0 - qqq) ) )

    !CHECK!sss1 = sss1 + c_factor        + d_factor 
    !CHECK!sss2 = sss2 + c_factor**2.0d0 + d_factor**2.0d0
    !CHECK!print *, sss1, sss2, a_factor, b_factor, c_factor, d_factor
    
    pmat(:,:) = 0.0d0
    DO j=1,a_sample ! MONTE-Carlo Resampling M times
      CALL get_resampling_mtx('MR','OF',nbv,acc,pmat_one)
      pmat(:,:) = pmat(:,:) + pmat_one(:,:)*a_factor ; END DO
    DO j=1,b_sample ! MONTE-Carlo Resampling M times
      CALL get_resampling_mtx('MR','OF',nbv,acc,pmat_one)
      pmat(:,:) = pmat(:,:) + pmat_one(:,:)*b_factor ; END DO
      
      CALL get_resampling_mtx('MR','OF',nbv,acc,pmat_one)
      pmat(:,:) = pmat(:,:) + pmat_one(:,:)*c_factor

      !!!CALL get_resampling_mtx('SU','OF',nbv,acc,pmat_one)
      !!!!CALL get_resampling_mtx('MR','OF',nbv,acc,pmat_one)
      pmat_su(:,:) = 0.0d0
      DO j=1,nsu
        CALL get_resampling_mtx('SU','OF',nbv,acc,pmat_one)
        pmat_su(:,:) = pmat_su(:,:) + pmat_one(:,:) / dble(nsu) ; END DO
        
        pmat(:,:) = pmat(:,:) + pmat_su(:,:)*d_factor

    !!!print '(a)', "========================ave time ============================="
    !!!do i=1,nbv@  
    !!!  print '(I3,x,40i1,x,40i1,x,40i1,x,5f8.3)', i, int(pmat_one(i,:)+0.00001), int(pmat_su(i,:)+0.1), int((pmat(i,:)+0.1)), &
    !!!        sum(pmat(:,i)),     sum(pmat(i,:)), sum(pmat_su(i,:)), pfwgh(i)*dble(nbv), acc(i)
    !!!end do
    !!!print '(7f)', sss1, sss2, a_factor, b_factor, c_factor, d_factor, a_factor*a_sample+b_factor*b_sample+c_factor+d_factor
    !!!stop
  ELSE IF( type_pfmtx==3 .or. type_pfmtx==4 )THEN !3-4: Sinkhorn
    CALL get_resampling_mtx('SU','OF',nbv,acc,pmat_su) 
    IF( ((peff+0.00001)>dble(nbv)) .or. (peff<1.00001) )THEN ! when peff is sufficiently large/small
      pmat=pmat_su
    ELSE
      IF( type_pfmtx==3 ) snk_cost(1:nbv,1:nbv) = pfcst(1:nbv,1:nbv) ! obs-space distance   : (Yb(i)-Yb(j) w/ R-1)
      IF( type_pfmtx==4 ) snk_cost(1:nbv,1:nbv) = cst(1:nbv,1:nbv)   ! state-space distance : (X(i) -X(j) of the first var)


      CALL get_snkresampl_mtx(nbv,pfwgh,snk_cost,pmat) 
      IF( snk_monit )THEN
        
        ! for comparison :: KK
        pmat_kk(:,:) = 0.0d0
        nmcsmp = nbv*5 ! SK's basic test (appendix)
        DO j=1,nmcsmp ! Monte-Carlo Resampling M times
          CALL get_resampling_mtx('MR','ON',nbv,acc,pmat_one)
          pmat_kk(:,:) = pmat_kk(:,:) + pmat_one(:,:) / dble(nmcsmp)
        END DO

        erroSU = error_resamp_mat_column_sum(nbv,nbv,pmat_su, pfwgh, .false.)
        erroKK = error_resamp_mat_column_sum(nbv,nbv,pmat_kk, pfwgh, .false.)
        erroOT = error_resamp_mat_column_sum(nbv,nbv,pmat   , pfwgh, .false.)

        PRINT '(a)', "========wlpf====wrsm====sum1====================== SU =============================="
        DO i=1,nbv
          PRINT '(i4,3f8.2,a,40f5.2)',i,pfwgh(i)*sngl(nbv),sum(pmat_su(i,:)),sum(pmat_su(:,i))," || ",pmat_su(i,:)
        ENDDO
        PRINT '(a)', "========wlpf====wrsm====sum1====================== KK =============================="
        DO i=1,nbv
          PRINT '(i4,3f8.2,a,40f5.2)',i,pfwgh(i)*sngl(nbv),sum(pmat_kk(i,:)),sum(pmat_kk(:,i))," || ",pmat_kk(i,:)
        END DO
        PRINT '(a)', "========wlpf====wrsm====sum1====================== OT =============================="
        DO i=1,nbv
          PRINT '(i4,3f8.2,a,40f5.2)',i,pfwgh(i)*sngl(nbv),sum(pmat(i,:)),   sum(pmat(:,i)),   " || ",pmat(i,:)
        END DO

        costSU=0.0d0 ; costKK=0.0d0 ; costOT=0.0d0
        DO j=1,nbv ; DO i=1,nbv
          costSU = costSU + snk_cost(i,j)*pmat_su(i,j)
          costKK = costKK + snk_cost(i,j)*pmat_kk(i,j)
          costOT = costOT + snk_cost(i,j)*pmat(i,j)
        END DO     ; END DO
        PRINT '(a)', "============================== Comparison =============================="
        PRINT '(I,f6.2a,3f10.3,a,3f20.3,a,3f20.3)',nobsl,peff, "  Error (SU,KK,OT) :: ", erroSU, erroKK,erroOT, &
          "  Cost (SU,KK,OT) :: ", costSU,costKK,costOT, &
          "  || (SU-OT,KK-OT)%, +/- =>good/bad ",100.d0*(costSU-costOT)/costOT,100.d0*(costKK-costOT)/costOT,sum(abs(pmat_kk(:,:)-pmat(:,:)))
        PRINT '(a)', "========================================================================"
      ENDIF ! monit

      costSU=0.0d0 ; costOT=0.0d0
      DO j=1,nbv ; DO i=1,nbv
        costSU = costSU + pfcst(i,j)*pmat_su(i,j)
        costOT = costOT + pfcst(i,j)*pmat(i,j)
      END DO     ; END DO
      IF( costSU<costOT ) THEN ! can happen
        !!PRINT '(a,i,3f20.5)', "hey, pmat is replaced by KK ::", nobsl,peff, costSU, costOT
        pmat_kk(:,:) = 0.0d0
        nmcsmp = nbv*5 ! SK's basic test (appendix)
        DO j=1,nmcsmp ! Monte-Carlo Resampling M times
          CALL get_resampling_mtx('MR','ON',nbv,acc,pmat_one)
          pmat_kk(:,:) = pmat_kk(:,:) + pmat_one(:,:) / dble(nmcsmp)
        END DO
        pmat(:,:)=pmat_kk(:,:)
      ENDIF

      ! for debug option
      !!IF( DBG_REPLACE ) THEN
      !!  pmat_kk(:,:) = 0.0d0
      !!  nmcsmp = nbv*5 ! SK's basic test (appendix)
      !!  DO j=1,nmcsmp ! Monte-Carlo Resampling M times
      !!    CALL get_resampling_mtx('MR','ON',nbv,acc,pmat_one)
      !!    pmat_kk(:,:) = pmat_kk(:,:) + pmat_one(:,:) / dble(nmcsmp)
      !!  END DO
      !!  pmat(:,:)=pmat_kk(:,:)
      !!ENDIF
    ENDIF ! peff

    !!!!==> inflation by adding noise
    !!!IF( type_pfmtx==5 )THEN 
    !!!  resample_factor = 0.05d0
    !!!  DO j=1,nbv
    !!!    grnd(1:nbv) = gr_common(1:nbv,j) * resample_factor
    !!!    swgh        = sum( grnd(1:nbv) ) / dble( nbv )
    !!!    grnd(1:nbv) = grnd(1:nbv) - swgh                ! so that mean is zero
    !!!    pmat(1:nbv,j) =      pmat(1:nbv,j) + grnd(1:nbv)
    !!!  END  DO
    !!!ENDIF


  ENDIF ! resampling type
!-----------------------------------------------------------------------
    RETURN
  END IF ! nobsl
END SUBROUTINE lpf_core

!-----------------------------------------------------------------------    
SUBROUTINE get_resampling_mtx(CC,DG,nbv,acc,pmat)
!-----------------------------------------------------------------------
IMPLICIT NONE
  INTEGER     , INTENT(in)  :: nbv
  CHARACTER(2), INTENT(in)  :: CC         ! SU or MR
  CHARACTER(2), INTENT(in)  :: DG         ! ON or OF (ON-->diagonal prioritized)
  REAL(r_size), INTENT(in)  :: acc(1:nbv)
  REAL(r_size), INTENT(out) :: pmat(nbv,nbv)

  INTEGER      :: i, j, k, init(nbv), inum(nbv)
  REAL(r_size) :: rand(nbv), temp

  DO j=1,nbv ; init(j) = j  ; END DO
  !CALL com_rand(nbv,rand) ! [0-1]

  IF( CC == 'SU' )THEN
    CALL com_rand_seed(nbv,0,rand) ! [0-1]
    temp = rand(1)
    DO i=1,nbv
      rand(i) = ( dble(i) +  temp - 1.0d0 ) / dble(nbv)
    END DO
  ELSE IF( CC == 'MR' )THEN
    CALL com_rand_seed(nbv,0,rand) ! [0-1]
    CALL quick_sort_asnd(rand,init,1,nbv)
  ELSE
    PRINT *, "  ERROR&SROP :: NOT SUCH OPTION in get_resampling_mtx for CC ", CC
    STOP
  END IF

  !==> generate resampling mxm matrix (KK's diagonal-oriented; str)
IF( DG =='ON' )THEN
  inum(:)   = 0
  !(1) :: gen selected particle
  DO j=1,nbv ! jth column
    DO i=1,nbv-1
      IF( rand(j)<=acc(i) )THEN
        inum(j) = i
        GOTO 10
      ENDIF
    END DO
    inum(j) = nbv
10  CONTINUE
  END DO
  !!print *, inum(:)

  pmat(:,:) = 0.0d0
  DO i=1,nbv ! (1) diagonal component
    k = inum(i)
    IF( k/=0 .and. pmat(k,k)<0.5d0 )THEN
      pmat(k,k) = 1.0d0
      inum(i)   = 0
    END IF 
  END DO

  DO i=1,nbv ! (2) off-diagonal component
    k = inum(i)
    IF( k/=0 )THEN
      DO j=1,nbv
        IF( sum(pmat(:,j))<0.5 )THEN
          pmat(k,j) = 1.0d0
          GOTO 20
        END IF
      END DO
    END IF 
20  CONTINUE
  END DO

ELSE IF( DG =='OF' )THEN  
  !==> generate resampling mxm matrix (non-diagonal-prioritized; str, SK's trial)
  pmat(:,:) = 0.0d0
  DO j=1,nbv ! jth column
    DO i=1,nbv-1
      IF( rand(j)<=acc(i) )THEN
        pmat(i,j) = 1.0d0
        GOTO 30
      END IF
    END DO
    pmat(nbv,j) = 1.0d0
30  CONTINUE
  END DO
ENDIF

END SUBROUTINE get_resampling_mtx
!-----------------------------------------------------------------------    
SUBROUTINE get_snkresampl_mtx(nbv,pfwgh,pfcst,pmat) ! Sinkhorn-based resampling (Cuturi 2013, NIPS)
                                                    ! coded by Takeshima and Kotsuki 
!-----------------------------------------------------------------------
IMPLICIT NONE
  INTEGER     , INTENT(in)  :: nbv
  REAL(r_size), INTENT(in)  :: pfwgh(nbv)     ! weight of particles
  REAL(r_size), INTENT(in)  :: pfcst(nbv,nbv) ! cost matrix i.e., distance b/w particles
  REAL(r_size), INTENT(out) :: pmat(nbv,nbv)

  ! Sinkhorn Algorithm
  LOGICAL,      PARAMETER   :: snkitr_monit  = .false.
  INTEGER,      PARAMETER   :: thresh_nLoop  = 150     ! Oishi and Kotsuki (2023; SOLA) 
  REAL(r_size), PARAMETER   :: thresh_convg  = 0.0001  !
  REAL(r_size), PARAMETER   :: thresh_weight = 0.00001 ! threshold of weight = 0 (to avoid NAN by 1./w )   
  REAL(r_size), PARAMETER   :: thresh_explim = 12.0d0  ! treatment on underflow of (exp(-thresh_explim)/=0.0d0) 
  REAL(r_size), ALLOCATABLE :: weight(:)     ! reduced weight vector
  REAL(r_size), ALLOCATABLE :: otmtx(:,:)    ! transport matrix
  REAL(r_size), ALLOCATABLE :: snk_C(:,:)    ! Cost Matrix
  REAL(r_size), ALLOCATABLE :: snk_K(:,:)    ! Gibbs Kernel
  REAL(r_size), ALLOCATABLE :: snk_Kc(:,:)   ! Gibbs Kernel weighted
  REAL(r_size), ALLOCATABLE :: snk_u(:)      ! u vector
  REAL(r_size), ALLOCATABLE :: snk_u_prev(:) ! u vector, previous
  REAL(r_size) :: snk_v(nbv)       ! v vector
  REAL(r_size) :: snk_error        ! errors in transform matrix
  REAL(r_size) :: snk_convg        ! monitor convergence

  ! global
  INTEGER :: i, j, nLoop, npost
  INTEGER :: i2post(nbv)

  ! (1) :: check non-zero particles, npost = # of posterior particles that can be resampled 
  npost=0 ; i2post(:)=0
  DO i=1,nbv
    IF( pfwgh(i)>thresh_weight ) THEN
      npost = npost + 1
      i2post(i)=npost
    ENDIF
  END DO
  ALLOCATE( otmtx(npost,nbv), snk_C(npost,nbv), snk_K(npost,nbv), snk_Kc(npost,nbv) )
  ALLOCATE( weight(npost)   , snk_u(npost)    , snk_u_prev(npost)                   )

  ! (2) :: cost matrix
  DO i=1,nbv
    IF( i2post(i)/=0 ) snk_C(i2post(i),1:nbv) = pfcst(i,1:nbv)
  END DO
  ! nomalization followed by clipping by thresh_explim
  snk_C(1:npost,1:nbv) = thresh_explim * snk_C(1:npost,1:nbv) / maxval(snk_C(1:npost,1:nbv)) 
    ! so that maxval(snk_C) = thresh_explim (=12.0) following Oishi and Kotsuki (2023)
    ! ex. lambda=40.0d -->  maxval(lambda*snk_C) ~ 480
    ! exp(-x) can be handled up to about 700

  ! Gibbs Kernel ; K
  snk_K(1:npost,1:nbv) = exp(-snk_C(1:npost,1:nbv)*snk_lambda)

  ! Gibbs Kernel ; K tilda
  DO i=1,nbv
    IF( i2post(i)/=0 ) weight(i2post(i)) = pfwgh(i)
  END DO
  snk_Kc = matmul(make_diag(1.d0/(weight*DBLE(nbv))), snk_K)

  IF( snkitr_monit )THEN
    PRINT '(a,I,2F)', "weight", npost, minval( weight ),  maxval( weight )
    PRINT '(a,I,2F)', "snk_c ", npost, minval( snk_c ),   maxval( snk_c )
    PRINT '(a,I,2F)', "snk_K ", npost, minval( snk_K ),   maxval( snk_K )
    PRINT '(a,I,2F)', "snk_Kc", npost, minval( snk_Kc ),  maxval( snk_Kc )
  ENDIF

  ! main loop
  snk_u_prev(:) = 1.d0 / DBLE(nbv) ! can be nbv
  nLoop         = 0
  DO WHILE( nLoop < thresh_nLoop )
    nLoop = nLoop + 1
    snk_u = 1.d0 / (matmul(snk_Kc, (1.d0 / matmul(transpose(snk_K),snk_u_prev))))
      ! u(t+1) = 1 ./ (G v(t+1) ) where v(t+1) = w./(G^T u(t))

    snk_convg  = maxval(abs(snk_u - snk_u_prev) / sqrt(sum(snk_u_prev**2)))
    snk_u_prev = snk_u ! sinkhorn update    

    IF( snkitr_monit )THEN
      snk_v = 1.d0 / matmul(transpose(snk_K), snk_u)
      DO j=1,nbv ; DO i=1,npost
        otmtx(i,j) = snk_u(i) * snk_v(j) * snk_K(i,j)
      END DO     ; END DO
      snk_error = error_resamp_mat_column_sum(npost,nbv,otmtx,weight, .false.)
      PRINT '(A,2I,F15.5,A,2F15.5)', "monit snk_error ::", npost, nloop, snk_error, &
                                    " convetrgence (now,prm)::", snk_convg, thresh_convg
    ENDIF
    IF( snk_convg < thresh_convg ) EXIT
  END DO
  snk_v = 1.d0 / matmul(transpose(snk_K), snk_u)

  DO j=1,nbv ; DO i=1,npost
    otmtx(i,j) = snk_u(i) * snk_v(j) * snk_K(i,j)
  END DO     ; END DO

  pmat(:,:) = 0.0d0
  DO i=1,nbv
    IF( i2post(i)/=0 ) pmat(i,1:nbv) = otmtx(i2post(i),1:nbv)
  END DO

  ! adjustment, sum should be 1.0 exactly
  DO j=1,nbv 
    pmat(:,j)=pmat(:,j)/sum(pmat(:,j))
  END DO
  DEALLOCATE( otmtx, snk_C, snk_K, snk_Kc, weight, snk_u, snk_u_prev )
END SUBROUTINE get_snkresampl_mtx
!-----------------------------------------------------------------------    
SUBROUTINE calc_pfwgh_kkver(nobs,nobsl,nbv,dep,hdxb,rloc,rdiag,asis,pfwgh)
!-----------------------------------------------------------------------
IMPLICIT NONE
  INTEGER     , INTENT(IN)    :: nobs,nobsl, nbv
  REAL(r_size), INTENT(IN)    :: dep(1:nobs),hdxb(1:nobs,1:nbv), rloc(1:nobs), rdiag(1:nobs), asis(1:nbv)
  REAL(r_size), INTENT(OUT)   :: pfwgh(1:nbv)
  INTEGER :: i, j, k
  REAL(r_size) :: sqpf, qtmp, qpf(1:nbv), dep2_Ri(1:nbv)

  dep2_Ri(:) = 0.0d0
  DO j=1,nbv
    DO i=1,nobsl
      dep2_Ri(j) = dep2_Ri(j) - 0.5d0*((hdxb(i,j)-dep(i))**2.0d0) * rloc(i) / rdiag(i)
    END DO
  END DO

  sqpf     = 0.0d0
  qpf(:)   = 0.0d0
  DO j=1,nbv
    qtmp = 0.0d0
    DO k=1,nbv
      qtmp = qtmp + dexp( dep2_Ri(k) - dep2_Ri(j) ) * asis(k)
      !print '(2i,5f)', j, k, dep2_Ri(j), dep2_Ri(k), dexp( dep2_Ri(j) ), qtmp, dexp( dep2_Ri(k) - dep2_Ri(j) )
    END DO
    qpf(j) = asis(j) / qtmp
    sqpf   = sqpf + qpf(j)
    !print *, j, qpf(j), dexp( dep2_Ri(j) )
  END DO
  pfwgh(:) = qpf(:) / sqpf
  RETURN
END SUBROUTINE calc_pfwgh_kkver

!-----------------------------------------------------------------------    
SUBROUTINE calc_pfwgh_norml(nobs,nobsl,nbv,dep,hdxb,rloc,rdiag,pfwgh)
!-----------------------------------------------------------------------
IMPLICIT NONE
  INTEGER     , INTENT(IN)  :: nobs,nobsl, nbv
  REAL(r_size), INTENT(IN)  :: dep(1:nobs),hdxb(1:nobs,1:nbv), rloc(1:nobs), rdiag(1:nobs)
  REAL(r_size), INTENT(OUT) :: pfwgh(1:nbv) 
  INTEGER :: i, j, k
  REAL(r_size) :: sqpf, qtmp, qpf(1:nbv), dep2_Ri(1:nbv)

  dep2_Ri(:) = 0.0d0
  DO j=1,nbv
    DO i=1,nobsl
      dep2_Ri(j) = dep2_Ri(j) - 0.5d0*((hdxb(i,j)-dep(i))**2.0d0) * rloc(i) / rdiag(i)
    END DO
  END DO

  sqpf     = 0.0d0
  qpf(:)   = 0.0d0
  DO j=1,nbv
    qpf(j) = dexp( dep2_Ri(j) )
    sqpf   = sqpf + qpf(j)
    !print *, j, qpf(j), dexp( dep2_Ri(j) )
  END DO
  pfwgh(:) = qpf(:) / sqpf
  RETURN
END SUBROUTINE calc_pfwgh_norml

!-----------------------------------------------------------------------    
SUBROUTINE calc_pfwgh_hphtr(nobs,nobsl,nbv,dep,hdxb,rloc,rdiag,asis,pfwgh,GAMMA_GMPF)
!-----------------------------------------------------------------------
IMPLICIT NONE
  INTEGER     , INTENT(IN)    :: nobs,nobsl, nbv
  REAL(r_size), INTENT(IN)    :: GAMMA_GMPF
  REAL(r_size), INTENT(IN)    :: dep(1:nobs),hdxb(1:nobs,1:nbv), rloc(1:nobs), rdiag(1:nobs), asis(1:nbv)
  REAL(r_size), INTENT(OUT)   :: pfwgh(1:nbv)
  INTEGER :: i, j, k, irank
  REAL(r_size) :: sqpf, qtmp, qpf(1:nbv), dep2_Ri(1:nbv)

  REAL(r_size) :: HPfHT(nobsl,nobsl), HPfHT_R(nobsl,nobsl), RiHPfHT(nobsl,nobsl), RiHPfHT_I(nobsl,nobsl), HPfHT_R_i(nobsl,nobsl)
  REAL(r_size) :: work0(nobsl,nobsl), work1(nobsl,nobsl)  , work2(nobsl,nobsl)
  REAL(r_size) :: pa(nobsl,nobsl),    pa_i(nobsl,nobsl),    pa_d(nobsl,nobsl)
  REAL(r_size) :: eival(nobsl),       eivec(nobsl,nobsl)
  REAL(r_size) :: chk0(nobsl,nobsl),  chk1(nobsl,nobsl),  chk2(nobsl,nobsl),  tmp(nobsl,nobsl)
  REAL(r_size) :: dep_tmp(nobsl)
  REAL(r_size) :: rsqrt(nobsl)
  REAL         :: ptimer0, ptimer

  !==> LU-BASED INVERSE COMPUTATION
  INTEGER :: LWORK, INFO, IPIV(nobsl)
  REAL(r_size) :: WVEC(nobsl)

  rsqrt(:) = UNDEF
  DO i=1,nobsl
    rsqrt(i) = dsqrt( rdiag(i) )
  END DO
!-----------------------------------------------------------------------
!  HPfHT + R = GAMMA*hdxb^T hdxb/(m-1) + R 
!-----------------------------------------------------------------------
  HPfHT(:,:) = 0.0d0
  DO j=1,nobsl
    DO i=1,nobsl
      DO k=1,nbv
       HPfHT(i,j) =HPfHT(i,j) + hdxb(i,k)*hdxb(j,k)*GAMMA_GMPF/dble(nbv-1.0d0)
      END DO
    END DO
    HPfHT_R(:,j) = HPfHT(:,j)
    HPfHT_R(j,j) = HPfHT_R(j,j) + rdiag(j)
  END DO

!!RANK!!!-----------------------------------------------------------------------
!!RANK!!!  eigenvalues and eigenvectors of [ HPfHT+R ]
!!RANK!!!-----------------------------------------------------------------------
!!RANK!!  work2(:,:) = work1(:,:)
!!RANK!!  CALL mtx_eigen(1,nobsl,work1,eival,eivec,irank)
!!RANK!!!-----------------------------------------------------------------------
!!RANK!!!  Pa = [ HPfHT+R ]inv
!!RANK!!!-----------------------------------------------------------------------
!!RANK!!  DO j=1,nobsl
!!RANK!!    DO i=1,nobsl
!!RANK!!      work1(i,j) = eivec(i,j) / eival(j)
!!RANK!!    END DO
!!RANK!!  END DO
!!RANK!!  DO j=1,nobsl
!!RANK!!    DO i=1,nobsl
!!RANK!!      pa(i,j) = work1(i,1) * eivec(j,1)
!!RANK!!      DO k=2,nbv
!!RANK!!        pa(i,j) = pa(i,j) + work1(i,k) * eivec(j,k)
!!RANK!!      END DO
!!RANK!!    END DO
!!RANK!!  END DO

!-----------------------------------------------------------------------
!  R-1/2(HPfHT)R-1/2 + I where HPfHT = GAMMA*hdxb^T hdxb/(m-1)
!-----------------------------------------------------------------------
  !R-1/2(HPfHT)R-1/2  + I
  DO j=1,nobsl
    DO i=1,nobsl
      RiHPfHT(i,j) = HPfHT(i,j)/rsqrt(i)/rsqrt(j)
    END DO
    RiHPfHT_I(:,j) = RiHPfHT(:,j)
    RiHPfHT_I(j,j) = RiHPfHT_I(j,j) + 1.0d0
  END DO
  work1(:,:) = RiHPfHT_I(:,:)
  !now! work1 :: R-1/2(HPfHT)R-1/2 + I
!!NO!!!-----------------------------------------------------------------------
!!NO!!!  eigenvalues and eigenvectors of [ R-1/2(HPfHT)R-1/2 + I ]
!!NO!!!-----------------------------------------------------------------------
!!NO!!  CALL cpu_time(ptimer0)
!!NO!!  CALL mtx_eigen(1,nobsl,work1,eival,eivec,irank)
!!NO!!!-----------------------------------------------------------------------
!!NO!!!  Pa = [ R-1/2(HPfHT)R-1/2 + I ] = UDU
!!NO!!!-----------------------------------------------------------------------
!!NO!!  !==> check eigenvalue decomposition
!!NO!!  pa(:,:) = 0.0d0    ; pa_i(:,:) = 0.0d0  ; pa_d(:,:) = 0.0d0
!!NO!!  work0(:,:) = 0.0d0 ; work1(:,:) = 0.0d0 ; work2(:,:) = 0.0d0
!!NO!!  DO j=1,nobsl ; DO i=1,nobsl
!!NO!!    work0(i,j) = eivec(i,j) / eival(j)
!!NO!!  END DO       ; END DO
!!NO!!  CALL dgemm('n','t',nobsl,nobsl,nobsl,1.0d0,work0,nobsl,eivec,nobsl,0.0d0,pa_i,nobsl)
!!NO!!!!CHK!!  DO j=1,nobsl ; DO i=1,nobsl
!!NO!!!!CHK!!    work1(i,j) = eivec(i,j) * eival(j)
!!NO!!!!CHK!!    work2(i,j) = eivec(i,j)
!!NO!!!!CHK!!  END DO       ; END DO
!!NO!!!!CHK!!  CALL dgemm('n','t',nobsl,nobsl,nobsl,1.0d0,work1,nobsl,eivec,nobsl,0.0d0,pa  ,nobsl)
!!NO!!!!CHK!!  CALL dgemm('n','t',nobsl,nobsl,nobsl,1.0d0,work2,nobsl,eivec,nobsl,0.0d0,pa_d,nobsl)
!!NO!!!!CHK!!  chk0 = matmul( pa_i, RiHPfHT_I )
!!NO!!  CALL cpu_time(ptimer) ; print *, ptimer-ptimer0 ; ptimer0=ptimer
!-----------------------------------------------------------------------
!  HPfHT_R_i = [ HPfHT+R ]inv  :: LU-BASED INVERSE COMPUTATION
!-----------------------------------------------------------------------
  CALL mtx_inv(nobsl,RiHPfHT_I,work1)
!!CHK!!  CALL cpu_time(ptimer) ; print *, ptimer-ptimer0 ; ptimer0=ptimer
!-----------------------------------------------------------------------
!  HPfHT_R_i = [ HPfHT+R ]inv
!-----------------------------------------------------------------------
  DO j=1,nobsl
    DO i=1,nobsl
!!CHK!!      tmp(i,j)       = pa_i(i,j) /rsqrt(i)/rsqrt(j)
      HPfHT_R_i(i,j) = work1(i,j)/rsqrt(i)/rsqrt(j)
    END DO
  END DO

!!CHK!!  chk1 = matmul( HPfHT_R, HPfHT_R_i ) !!CHK!!
!!CHK!!  chk2 = matmul( HPfHT_R, HPfHT_R_i ) !!CHK!!
!!CHK!!  !==> FOR CHECK ALGORITHM <==!
!!CHK!!  PRINT '(a)',"================================ (HPfHT) ==============================================================="
!!CHK!!                DO i=1,15  ;  PRINT '(20F15.8)', HPfHT(i,1:15)        ;       END DO
!!CHK!!  PRINT '(a)',"================================ (RiHPfHT) ::   (R-1/2(HPfHT)R-1/2) ===================================="
!!CHK!!                DO i=1,15  ;  PRINT '(20F15.8)', RiHPfHT(i,1:15)      ;       END DO
!!CHK!!  PRINT '(a)',"================================ (RiHPfHT_I) :: (R-1/2(HPfHT)R-1/2 + I) ================================"
!!CHK!!                DO i=1,15  ;  PRINT '(20F15.8)', RiHPfHT_I(i,1:15)    ;       END DO
!!CHK!!  PRINT '(a)',"================================ (Pa=RiHPfHT_I) UDUT ==================================================="
!!CHK!!                DO i=1,15  ;  PRINT '(20F15.8)', pa(i,1:15)           ;       END DO
!!CHK!!  PRINT '(a)',"================================ (Pa_d=         UUT  ==================================================="
!!CHK!!                DO i=1,15  ;  PRINT '(20F15.8)', pa_d(i,1:15)         ;       END DO
!!CHK!!  PRINT '(a)',"================================ (Pa_i= (RiHPfHT_I UDUT)-1 ============================================="
!!CHK!!                DO i=1,15  ;  PRINT '(20F15.8)', pa_i(i,1:15)         ;       END DO
!!CHK!!  PRINT '(a)',"================================ (Pa_i= (RiHPfHT_I UDUT)-1 ============================================="
!!CHK!!                DO i=1,15  ;  PRINT '(20F15.8)', work1(i,1:15)         ;      END DO
!!CHK!!  PRINT '(a)',"================================ EIVEC UDUT ============================================================"
!!CHK!!                DO i=1,15  ;  PRINT '(15F15.5,a,F15.5)', eivec(i,1:15), " ||sum|| ", sum(eivec(i,:)**2.0d0) ;  END DO
!!CHK!!  PRINT '(a)',"================================ HPfHT_R_i  ============================================================"
!!CHK!!                DO i=1,15  ;  PRINT '(15F15.5,a,F15.5)', HPfHT_R_i(i,1:15) ;  END DO
!!CHK!!  PRINT '(a)',"================================ chk0      ============================================================="
!!CHK!!                DO i=1,15  ;  PRINT '(15F15.5,a,F15.5)', chk0(i,1:15) ;       END DO
!!CHK!!  PRINT '(a)',"================================ chk1      ============================================================="
!!CHK!!                DO i=1,15  ;  PRINT '(15F15.5,a,F15.5)', chk1(i,1:15) ;       END DO
!!CHK!!  PRINT '(a)',"================================ chk2      ============================================================="
!!CHK!!                DO i=1,15  ;  PRINT '(15F15.5,a,F15.5)', chk2(i,1:15) ;       END DO
!!CHK!!  STOP

!!R!!  dep2_Ri(:) = 0.0d0
!!R!!  DO j=1,nbv
!!R!!    DO i=1,nobsl
!!R!!      dep2_Ri(j) = dep2_Ri(j) - 0.5d0*((hdxb(i,j)-dep(i))**2.0d0) * rloc(i) / rdiag(i)
!!R!!    END DO
!!R!!  END DO
  dep2_Ri(:) = 0.0d0
  DO k=1,nbv
    dep_tmp(:) = 0.0d0
    DO j=1,nobsl
      DO i=1,nobsl
        dep_tmp(j) = dep_tmp(j) + (hdxb(i,k)-dep(i))*HPfHT_R_i(i,j)
      END DO
    END DO
    DO i=1,nobsl
      dep2_Ri(k) = dep2_Ri(k) - 0.5d0*dep_tmp(i)*(hdxb(i,k)-dep(i))*rloc(i)
    END DO
  END DO

  sqpf     = 0.0d0
  qpf(:)   = 0.0d0
  DO j=1,nbv
    qtmp = 0.0d0
    DO k=1,nbv
      qtmp = qtmp + dexp( dep2_Ri(k) - dep2_Ri(j) ) * asis(k)
      !print '(2i,5f)', j, k, dep2_Ri(j), dep2_Ri(k), dexp( dep2_Ri(j) ), qtmp, dexp( dep2_Ri(k) - dep2_Ri(j) )
    END DO
    qpf(j) = asis(j) / qtmp
    sqpf   = sqpf + qpf(j)
    !print *, j, qpf(j), dexp( dep2_Ri(j) )
  END DO
  pfwgh(:) = qpf(:) / sqpf
  RETURN
END SUBROUTINE calc_pfwgh_hphtr

!===============================================================
! functions for Sinkhorn :: A. Takeshima 20240904
!===============================================================
function make_diag(L) result(A) 
  implicit none
  real(r_size), intent(in) :: L(:)
  real(r_size) :: A(size(L),size(L))

  integer :: i

  A(:,:) = 0.d0
  do i = 1, size(L)
    A(i,i) = L(i)
  enddo
end function make_diag
!===============================================================
function get_diag(A) result(L)
  implicit none
  real(r_size), intent(in) :: A(:,:)
  real(r_size) :: L(size(A,1))

  integer :: i
  do i = 1, size(A,1)
    L(i) = A(i,i)
  enddo
end function get_diag
!===============================================================
real(r_size) function error_resamp_mat_column_sum(npost,nbv,T, wa, is_transposed) result(ret)
  implicit none
  INTEGER, INTENT(in) :: npost, nbv
  real(8), intent(in) :: T(npost,nbv)  !(m',m) 
  real(8), intent(in) :: wa(npost)     !(m')
  logical, intent(in) :: is_transposed

  if( is_transposed )then
    ret = sum(abs(sum(T,1)/size(T,1) - wa))
  else
    ret = sum(abs(sum(T,2)/size(T,2) - wa))
  endif
end function error_resamp_mat_column_sum
END MODULE common_lpf
