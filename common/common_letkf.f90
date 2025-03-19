MODULE common_letkf
!=======================================================================
!
! [PURPOSE:] Local Ensemble Transform Kalman Filtering (LETKF)
!            Model Independent Core Module
!
! [REFERENCES:]
!  [1] Ott et al., 2004: A local ensemble Kalman filter for atmospheric
!    data assimilation. Tellus, 56A, 415-428.
!  [2] Hunt et al., 2007: Efficient Data Assimilation for Spatiotemporal
!    Chaos: A Local Ensemble Transform Kalman Filter. Physica D, 230,
!    112-126.
!
! [HISTORY:]
!  01/21/2009 Takemasa Miyoshi  Created at U. of Maryland, College Park
!
!=======================================================================
!$USE OMP_LIB
  USE common
  USE common_mtx

  IMPLICIT NONE

  PUBLIC
!=======================================================================
!  LEKF Model Independent Parameters
!=======================================================================
!  INTEGER,PARAMETER :: nbv=16    ! ensemble size
  !INTEGER,SAVE :: nbv=02
  REAL(r_size),PARAMETER :: relax_alpha = 0.0d0  ! relaxation parameter     !GYL
  REAL(r_size),PARAMETER :: min_infl    = 0.0d0  ! minimum inlfation factor !GYL
  INTEGER,     SAVE :: type_relax = 0            ! 0:RTPx, 1:RTPP, 2:RTPS, 3:RTPL, 4:RTPE
  INTEGER,     SAVE :: option_aloc = 0           ! 0:normal 1:adaptive localization
  REAL(r_size),SAVE :: hpo_weight  = 0.0d0       ! weight parameter of PO-LETKF (Tsuyuki 2024 JMSJ) 
                                                 !   w=0.0->LETKF, w=1.0-->PO
  REAL(r_size),SAVE :: s_additive = 0.0d0        ! scale of additive inflation
CONTAINS
!=======================================================================
!  Main Subroutine of LETKF Core
!   INPUT
!     nobs             : array size, but only first nobsl elements are used
!     nobsl            : total number of observation assimilated at the point
!     hdxb(nobs,nbv)   : obs operator times fcst ens perturbations
!     rdiag(nobs)      : observation error variance
!     rloc(nobs)       : localization weighting function
!     dep(nobs)        : observation departure (yo-Hxb)
!     parm_infl        : covariance inflation parameter
!   OUTPUT
!     trans(nbv,nbv) : transformation matrix
!=======================================================================
SUBROUTINE letkf_core(nobs,nobsl,hdxb,rdiag,rloc,dep,pob,parm_infl,LOGIC_LPFGM,work3,work4,workS,workQ,peff)
  IMPLICIT NONE
  INTEGER,INTENT(IN) :: nobs
  INTEGER,INTENT(IN) :: nobsl
  REAL(r_size),INTENT(IN) :: hdxb(1:nobs,1:nbv)
  REAL(r_size),INTENT(IN) :: rdiag(1:nobs)
  REAL(r_size),INTENT(IN) :: rloc(1:nobs)
  REAL(r_size),INTENT(IN) :: dep(1:nobs)
  REAL(r_size),INTENT(IN) :: pob(1:nobs,1:nbv)
  REAL(r_size),INTENT(INOUT) :: parm_infl
  REAL(r_size),INTENT(OUT)   :: peff
  REAL(r_size),INTENT(OUT) :: work3(nbv)         ! mean update (w of LETKF)
  REAL(r_size),INTENT(OUT) :: work4(nbv,nbv)     ! ptb  update (W of LETKF)
  REAL(r_size),INTENT(OUT) :: workQ(nbv,nbv)     ! ptb  update (W of PO-EnKF)
  REAL(r_size),INTENT(OUT) :: workS(nbv,nbv)     ! ptb  update (W of stochastic part of PO-EnKF)
  REAL(r_size) :: trans(nbv,nbv)                 ! ens  update (w.1 + W) 
  REAL(r_size) :: hdxb_rinv(nobsl,nbv)
  REAL(r_size) :: eivec(nbv,nbv)
  REAL(r_size) :: eival(nbv)
  REAL(r_size) :: pa(nbv,nbv)
  REAL(r_size) :: work1(nbv,nbv), work0(nbv,nbv), workP(nbv,nbv)
  REAL(r_size) :: work2(nbv,nobsl)
  REAL(r_size) :: rho, phi
  REAL(r_size) :: parm(4),sigma_o,gain
  REAL(r_size),PARAMETER :: sigma_b = 0.04d0 !error stdev of parm_infl
  INTEGER :: i,j,k
  LOGICAL, INTENT(IN) :: LOGIC_LPFGM
  LOGICAL             :: LOGIC_SEnKF = .false.
  LOGICAL             :: LOGIC_ADDIT = .false.
  REAL(r_size)        :: work5(nbv,nbv)  ! defined for LPFGM ; SK20191114
! LOGICAL :: exist
  IF(nobsl == 0) THEN
    trans = 0.0d0
    DO i=1,nbv
      trans(i,i) = SQRT(parm_infl)
    END DO
    IF (relax_alpha /= 0.0d0) THEN                                    !GYL
      DO i=1,nbv                                                      !GYL
        trans(i,i) = relax_alpha + (1.0d0 - relax_alpha) * trans(i,i) !GYL
      END DO                                                          !GYL
    END IF                                                            !GYL

    work3(:)   = 0.0d0
    work4(:,:) = SQRT(parm_infl)
    workS(:,:) = 0.0d0
    workQ(:,:) = 0.0d0
    peff      = DBLE(nbv)
    RETURN
  ELSE
    LOGIC_SEnKF = .false.
    LOGIC_ADDIT = .false.
    IF( hpo_weight>0.001 .or. type_relax==4 ) LOGIC_SEnKF = .true.
    IF( s_additive>0.001 ) THEN
      LOGIC_ADDIT = .true.
    ELSE
      workS(:,:) = 0.0d0
    ENDIF
!-----------------------------------------------------------------------
!  hdxb Rinv
!-----------------------------------------------------------------------
  DO j=1,nbv
    DO i=1,nobsl
      hdxb_rinv(i,j) = hdxb(i,j) / rdiag(i) * rloc(i)
    END DO
  END DO
!-----------------------------------------------------------------------
!  hdxb^T Rinv hdxb
!-----------------------------------------------------------------------
  CALL dgemm('t','n',nbv,nbv,nobsl,1.0d0,hdxb_rinv,nobsl,hdxb(1:nobsl,:),nobsl,0.0d0,work1,nbv)
!  DO j=1,nbv
!    DO i=1,nbv
!      work1(i,j) = hdxb_rinv(1,i) * hdxb(1,j)
!      DO k=2,nobsl
!        work1(i,j) = work1(i,j) + hdxb_rinv(k,i) * hdxb(k,j)
!      END DO
!    END DO
!  END DO
!-----------------------------------------------------------------------
!  hdxb^T Rinv Epo          :: workP
!-----------------------------------------------------------------------
  IF( LOGIC_SEnKF .or. LOGIC_ADDIT )THEN
    CALL dgemm('t','n',nbv,nbv,nobsl,1.0d0,hdxb_rinv,nobsl,pob(1:nobsl,:),nobsl,0.0d0,workP,nbv)
  ENDIF
!-----------------------------------------------------------------------
!  hdxb^T Rinv hdxb + (m-1) I / rho (covariance inflation)
!-----------------------------------------------------------------------
  IF (min_infl /= 0.0d0 .AND. parm_infl < min_infl) THEN !GYL
    parm_infl = min_infl                                 !GYL
  END IF                                                 !GYL
  rho = 1.0d0 / parm_infl
  DO i=1,nbv
    work1(i,i) = work1(i,i) + REAL(nbv-1,r_size) * rho
  END DO
!-----------------------------------------------------------------------
!  eigenvalues and eigenvectors of [ hdxb^T Rinv hdxb + (m-1) I ]
!-----------------------------------------------------------------------
  CALL mtx_eigen(1,nbv,work1,eival,eivec,i)
!-----------------------------------------------------------------------
!  Pa = [ hdxb^T Rinv hdxb + (m-1) I ]inv
!-----------------------------------------------------------------------
  DO j=1,nbv
    DO i=1,nbv
      work1(i,j) = eivec(i,j) / eival(j)
    END DO
  END DO
  CALL dgemm('n','t',nbv,nbv,nbv,1.0d0,work1,nbv,eivec,nbv,0.0d0,pa,nbv)
!  DO j=1,nbv
!    DO i=1,nbv
!      pa(i,j) = work1(i,1) * eivec(j,1)
!      DO k=2,nbv
!        pa(i,j) = pa(i,j) + work1(i,k) * eivec(j,k)
!      END DO
!    END DO
!  END DO
!-----------------------------------------------------------------------
!  Pa hdxb_rinv^T
!-----------------------------------------------------------------------
  CALL dgemm('n','t',nbv,nobsl,nbv,1.0d0,pa,nbv,hdxb_rinv,nobsl,0.0d0,work2,nbv)
!  DO j=1,nobsl
!    DO i=1,nbv
!      work2(i,j) = pa(i,1) * hdxb_rinv(j,1)
!      DO k=2,nbv
!        work2(i,j) = work2(i,j) + pa(i,k) * hdxb_rinv(j,k)
!      END DO
!    END DO
!  END DO
!-----------------------------------------------------------------------
!  Pa hdxb_rinv^T dep
!-----------------------------------------------------------------------
  DO i=1,nbv
    work3(i) = work2(i,1) * dep(1)
    DO j=2,nobsl
      work3(i) = work3(i) + work2(i,j) * dep(j)
    END DO
  END DO

  !===> SHIFT MATRIX FOR LPFGM
  work5(:,:) = 0.0d0
  DO k=1,nbv
    DO i=1,nbv
      work5(i,k) = work2(i,1) * ( dep(1) - hdxb(1,k) )
      DO j=2,nobsl
        work5(i,k) = work5(i,k) + work2(i,j) * ( dep(j) - hdxb(j,k) )
      END DO
    END DO
  END DO

  !!CHECKED;SK!!DO k=1,nbv
  !!CHECKED;SK!!  PRINT '(F6.3,a,40F6.3)', work3(k), " || ", work5(k,1:20)
  !!CHECKED;SK!!END DO
!-----------------------------------------------------------------------
!  T = sqrt[(m-1)Pa]
!-----------------------------------------------------------------------
  DO j=1,nbv
    rho = SQRT( REAL(nbv-1,r_size) / eival(j) )
    DO i=1,nbv
      work1(i,j) = eivec(i,j) * rho
    END DO
  END DO
  CALL dgemm('n','t',nbv,nbv,nbv,1.0d0,work1,nbv,eivec,nbv,0.0d0,trans,   nbv) ! ETKF
!  DO j=1,nbv
!    DO i=1,nbv
!      trans(i,j) = work1(i,1) * eivec(j,1)
!      DO k=2,nbv
!        trans(i,j) = trans(i,j) + work1(i,k) * eivec(j,k)
!      END DO
!    END DO
!  END DO
!-----------------------------------------------------------------------
!             (hdxb^T Rinv Epo)          ! workP (input)
!  Tpob  = pa (hdxb^T Rinv Epo)          ! workS (output)
!  Tenkf = pa (hdxb^T Rinv Epo + (m-1)I) ! workQ (output)
!-----------------------------------------------------------------------
  IF( LOGIC_SEnKF .or. LOGIC_ADDIT ) CALL dgemm('n','n',nbv,nbv,nbv,1.0d0,pa,nbv,workP,nbv,0.0d0,workS,nbv) ! PO
  IF( LOGIC_SEnKF                  ) workQ(1:nbv,1:nbv) = workS(1:nbv,1:nbv) + pa(1:nbv,1:nbv)*(DBLE(nbv)-1.0d0)
!-----------------------------------------------------------------------
!  Thybrid = (1-w)*Tetkf + w*Tpo
!-----------------------------------------------------------------------
  IF( hpo_weight>0.001 ) trans(1:nbv,1:nbv) = (1.0d0-hpo_weight)*trans(1:nbv,1:nbv) + (hpo_weight)*workQ(1:nbv,1:nbv)
!-----------------------------------------------------------------------
!  Saving ensemble transform matrix for ptb (W) for LPFGM
!-----------------------------------------------------------------------
  work4(:,:) = trans(:,:)
!-----------------------------------------------------------------------
!  T + Pa hdxb_rinv^T dep
!-----------------------------------------------------------------------
  IF (relax_alpha /= 0.0d0) THEN            !GYL
    trans = (1.0d0 - relax_alpha) * trans   !GYL
    DO i=1,nbv                              !GYL
      trans(i,i) = relax_alpha + trans(i,i) !GYL
    END DO                                  !GYL
  END IF                                    !GYL
  DO j=1,nbv
    DO i=1,nbv
      trans(i,j) = trans(i,j) + work3(i)
    END DO
  END DO
!-----------------------------------------------------------------------
!  weight dimension defined
!-----------------------------------------------------------------------
  !peff     = 1.0d0  / sum( work3(:)**2.0d0 ) ! effective particle size
     ! can be very large because of too-small w
  peff     =  (sum(abs(work3(:)))**2.0d0) / sum( work3(:)**2.0d0 ) ! effective particle size

!-----------------------------------------------------------------------
!  Inflation estimation
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
  parm(4) = (parm(1)-parm(3))/parm(2) - parm_infl
!  sigma_o = 1.0d0/REAL(nobsl,r_size)/MAXVAL(rloc(1:nobsl))
  sigma_o = 2.0d0/parm(3)*((parm_infl*parm(2)+parm(3))/parm(2))**2
  gain = sigma_b**2 / (sigma_o + sigma_b**2)
  parm_infl = parm_infl + gain * parm(4)

  IF( LOGIC_LPFGM )THEN
    work3(:) = 0.0d0
    work4(:,:) = work5(:,:)
    DO k=1,nbv
      work4(k,k) = work5(k,k) + 1.0d0
    END DO
  ENDIF

  RETURN
  END IF
END SUBROUTINE letkf_core

END MODULE common_letkf
