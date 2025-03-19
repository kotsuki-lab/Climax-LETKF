MODULE letkf_tools
!=======================================================================
!
! [PURPOSE:] Module for LETKF with SPEEDY/CLIMAX
!
! [HISTORY:]
!   01/26/2009 Takemasa Miyoshi  created
!   06/29/2024 Shunji   Kotsuki  updated for speedy/climax
!
!=======================================================================
  USE common
  USE common_mpi
  USE common_time
  USE common_XXXXXX
  USE common_mpi_XXXXXX
  USE common_letkf
  USE common_lpf
  USE letkf_obs
  USE interpolate
  USE, INTRINSIC :: ieee_arithmetic

  IMPLICIT NONE

  PRIVATE
  PUBLIC ::  das_letkf

  INTEGER,SAVE :: nobstotal

  !!! REAL(r_size),PARAMETER :: cov_infl_mul = -1.01d0 !multiplicative inflation (-->common_share.f90)
! > 0: globally constant covariance inflation
! < 0: 3D inflation values input from a GPV file "infl_mul.grd"
  REAL(r_size),PARAMETER :: sp_infl_add = 0.d0 !additive inflation
!TVS  LOGICAL,PARAMETER :: msw_vbc = .FALSE.

  ! no variable localization assumed for SPEEDY/CLIMAX 
  REAL(r_size),PARAMETER :: var_local(nv3d+nv2d,nid_obs) = 1.0d0

!SPEEDY!  REAL(r_size),PARAMETER :: var_local(nv3d+nv2d,nid_obs) = RESHAPE( &
!SPEEDY!!           U      V      T      Q     PS   RAIN
!SPEEDY!   & (/ 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0,  & ! U
!SPEEDY!   &    1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0,  & ! V
!SPEEDY!   &    1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0,  & ! T
!SPEEDY!   &    1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0,  & ! Q
!SPEEDY!   &    1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0,  & ! RH
!SPEEDY!   &    1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0,  & ! PS
!SPEEDY!   &    1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0 /)& ! RAIN
!SPEEDY!   & ,(/nv3d+nv2d,nid_obs/))

!CLIMAX!  REAL(r_size),PARAMETER :: var_local(nv3d+nv2d,nid_obs) = RESHAPE( &
!CLIMAX!!           U      V      T      Q    GEO    T2m   U10m   V10m     PS
!CLIMAX!   & (/ 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0,  & ! U
!CLIMAX!   &    1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0,  & ! V
!CLIMAX!   &    1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0,  & ! T
!CLIMAX!   &    1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0,  & ! Q
!CLIMAX!   &    1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0,  & ! RH
!CLIMAX!   &    1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0,  & ! PS
!CLIMAX!   &    1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0 /)& ! RAIN
!CLIMAX!   & ,(/nv3d+nv2d,nid_obs/))
  INTEGER,SAVE :: var_local_n2n(nv3d+nv2d)

CONTAINS
!-----------------------------------------------------------------------
! Data Assimilation
!-----------------------------------------------------------------------
SUBROUTINE das_letkf(gues3d,gues2d,anal3d,anal2d)
  IMPLICIT NONE
  CHARACTER(12) :: inflfile='infl_mul.grd'
  CHARACTER(29) :: wvecfile_o='./wvec/yyyymmddhh_MXXXXXX.grd'
  CHARACTER(21) :: nobsfile_o='./nobs/yyyymmddhh.grd'
  CHARACTER(25) :: inflfile_i='./infl_mul/yyyymmddhh.grd'
  CHARACTER(25) :: inflfile_o='./infl_mul/yyyymmddhh.grd'
  REAL(r_size),INTENT(INOUT) :: gues3d(nij1,nlev,nbv,nv3d) ! background ensemble
  REAL(r_size),INTENT(INOUT) :: gues2d(nij1,nbv,nv2d)      !  output: destroyed
  REAL(r_size),INTENT(OUT) :: anal3d(nij1,nlev,nbv,nv3d)   ! analysis ensemble
  REAL(r_size),INTENT(OUT) :: anal2d(nij1,nbv,nv2d)
  REAL(r_size),ALLOCATABLE :: mean3d(:,:,:)
  REAL(r_size),ALLOCATABLE :: mean2d(:,:)
  REAL(r_size),ALLOCATABLE :: hdxf(:,:)
  REAL(r_size),ALLOCATABLE :: rdiag(:)
  REAL(r_size),ALLOCATABLE :: rloc(:)
  REAL(r_size),ALLOCATABLE :: dep(:)
  REAL(r_size),ALLOCATABLE :: pob(:,:)                    ! obs perturbations for PO
  REAL(r_size),ALLOCATABLE :: work3d(:,:,:)   , work2d(:,:)
  REAL(r_sngl),ALLOCATABLE :: work3dg(:,:,:,:), work2dg(:,:,:)
  REAL(r_size),ALLOCATABLE :: logpfm(:,:)
  REAL(r_size) :: parm
  REAL(r_size) :: trans(nbv,nbv,nv3d+nv2d)
  LOGICAL :: ex
  INTEGER :: ij,ilev,n,m,i,j,k,nobsl,ierr


  !==> LPF ; SK 20190420
  LOGICAL      :: DBG_REPLACE = .false.
  LOGICAL      :: logic_lpfgm = .false.
  INTEGER      :: iseed
  REAL(r_size) :: srand(nbv)
  REAL(r_size) :: gr_common(nbv,nbv)            , Ident(nbv,nbv)
  REAL(r_size) :: nobs3d(nij1,nlev,nv3d)        , nobs2d(nij1,nv2d)         ! OUTPUT
  REAL(r_size) :: addo3d(nij1,nlev,nv3d)        , addo2d(nij1,nv2d)         ! OUTPUT
  REAL(r_size) :: adds3d(nij1,nlev,nv3d)        , adds2d(nij1,nv2d)         ! OUTPUT
  !!!REAL(r_size) :: wvec3d(nij1,nlev,nv3d,nbv)    , wvec2d(nij1,nv2d,nbv)     ! non-output
  !!!REAL(r_size) :: wmat3d(nij1,nlev,nv3d,nbv,nbv), wmat2d(nij1,nv2d,nbv,nbv) ! non-output
  !!!REAL(r_size) :: pvec3d(nij1,nlev,nv3d,nbv)    , pvec2d(nij1,nv2d,nbv)     ! non-output
  !!!REAL(r_size) :: pmat3d(nij1,nlev,nv3d,nbv,nbv), pmat2d(nij1,nv2d,nbv,nbv) ! non-output
  REAL(r_size) :: wvec3d(          nv3d,nbv)    , wvec2d(     nv2d,nbv)     ! non-output
  REAL(r_size) :: wmat3d(          nv3d,nbv,nbv), wmat2d(     nv2d,nbv,nbv) ! non-output
  REAL(r_size) :: lvec3d(          nv3d,nbv),     lvec2d(     nv2d,nbv)     ! non-output
  REAL(r_size) :: lmat3d(          nv3d,nbv,nbv), lmat2d(     nv2d,nbv,nbv) ! non-output
  REAL(r_size) :: amat3d(          nv3d,nbv,nbv), amat2d(     nv2d,nbv,nbv) ! non-output
  REAL(r_size) :: qmat3d(          nv3d,nbv,nbv), qmat2d(     nv2d,nbv,nbv) ! non-output
  REAL(r_size) :: pvec3d(          nv3d,nbv)    , pvec2d(     nv2d,nbv)     ! non-output
  REAL(r_size) :: pmat3d(          nv3d,nbv,nbv), pmat2d(     nv2d,nbv,nbv) ! non-output
  REAL(r_size) :: addi3d(          nbv,nv3d)    , addi2d(     nbv,nv2d)


  REAL(r_size) :: asis3d(nij1,nlev,nv3d,nbv)    , asis2d(nij1,nv2d,nbv)     ! OUTPUT
  REAL(r_size) :: peff3d(nij1,nlev,nv3d)        , peff2d(nij1,nv2d)         ! OUTPUT
  REAL(r_size) :: rsmp3d(nij1,nlev,nv3d)        , rsmp2d(nij1,nv2d)         ! OUTPUT

  CHARACTER(32) :: addofile_o='./infl_add/000001/yyyymmddhh.grd'
  CHARACTER(30) :: addsfile_o='./infl_add/sprd/yyyymmddhh.grd'
  CHARACTER(27) :: pletfile_o='./peff_letkf/yyyymmddhh.grd'
  CHARACTER(25) :: pefffile_o='./peff_lpf/yyyymmddhh.grd'
  CHARACTER(25) :: rsmpfile_o='./rsmp_lpf/yyyymmddhh.grd'
  CHARACTER(33) :: asisfile_o='./asis_lpf/yyyymmddhh_MXXXXXX.grd'

  !==> Weight Smoother
  REAL(r_size) :: sigma_g, distg, dist_zerog, sfnc, swgh
  REAL(r_size) :: wgh_nij2map(nij1,nlon,nlat)

  !==> Weight Interpolation
  INTEGER              :: iprocs, iexe, nexe, ilon, ilat
  INTEGER, ALLOCATABLE :: proc_m(:,:)
      
  REAL(r_size) :: trans3(nbv,nbv)
  REAL(r_size) :: trans2(nbv,nbv)
  REAL(r_size) :: msk(nlon,nlat),   msk_me(nij1) 
  REAL(r_size) :: wix(nlon,nlat,4), wix_me(nij1,4)
  REAL(r_size) :: wiy(nlon,nlat,4), wiy_me(nij1,4)
  REAL(r_size) :: fac(nlon,nlat,4), fac_me(nij1,4)
  REAL(r_size) :: tmpave, tmpspr, tmpinf, tmpptb(nbv), gusspr, anlspr
  REAL(r_size) :: cst(nbv,nbv)

!
  ltimer00 = MPI_WTIME() ; ltimer01 = ltimer00
  ptimer00 = MPI_WTIME() ; ptimer01 = ptimer00
  ptimer   = ptimer00
!
  WRITE(6,'(A)') 'Hello from das_letkf'
  nobstotal = nobs !+ ntvs
  WRITE(6,'(A,I8)') 'Target observation numbers : NOBS=',nobs!,', NTVS=',ntvs
  !
  ! identity matrix
  !
  Ident(:,:) = 0.0d0
  DO k=1,nbv ; Ident(k,k) = 1.0d0 ; END DO
  !SK,NO-WI;20200417!!
  !SK,NO-WI;20200417!! gauss weight preparation for filtering
  !SK,NO-WI;20200417!!
  !SK,NO-WI;20200417!IF( logic_wsth )THEN
  !SK,NO-WI;20200417!  sigma_g            = sigma_obs * 1.0d0
  !SK,NO-WI;20200417!  dist_zerog         = sigma_g * SQRT(10.0d0/3.0d0) * 2.0d0
  !SK,NO-WI;20200417!  wgh_nij2map(:,:,:) = 0.0d0
  !SK,NO-WI;20200417!  DO ij=1,nij1 ; DO j=1,nlat ; DO i=1,nlon
  !SK,NO-WI;20200417!    CALL com_distll_1(lon1(ij),lat1(ij),lon(i),lat(j),distg)
  !SK,NO-WI;20200417!    IF( distg < dist_zerog ) &
  !SK,NO-WI;20200417!      wgh_nij2map(ij,i,j) = EXP(-0.5d0 * ((distg/sigma_g)**2) )
  !SK,NO-WI;20200417!  END DO       ; END DO      ; END DO
  !SK,NO-WI;20200417!END IF
  !
  ! preparation for member parallerization
  !
  !===> setting for parallel computing
  IF( MOD(nbv,nprocs) /= 0 ) THEN
    print *, "error, nbv should be devided by the nprocs"
    STOP ; ENDIF

  nexe = nbv / nprocs
  allocate ( proc_m(0:nprocs-1,nexe) ) ; proc_m(:,:) = -999

  iprocs = -1 ; iexe  = 1
  DO m=1,nbv
    iprocs = iprocs + 1
    IF( iprocs == nprocs ) THEN
      iprocs = 0
      iexe   = iexe + 1
    ENDIF
    proc_m(iprocs,iexe)    = m
  END DO
  !!DO iexe=1,nexe
  !!  PRINT '(3i)', iexe, myrank,proc_m(myrank,iexe)
  !!END DO

  !
  ! In case of no obs
  !
  IF(nobstotal == 0) THEN
    WRITE(6,'(A)') 'No observation assimilated'
    anal3d = gues3d
    anal2d = gues2d
    RETURN
  END IF
  !
  ! Variable localization
  !
  var_local_n2n(1) = 1
  DO n=2,nv3d+nv2d
    DO i=1,n
      var_local_n2n(n) = i
      IF(MAXVAL(ABS(var_local(i,:)-var_local(n,:))) < TINY(var_local)) EXIT
    END DO
  END DO
  m = int( sum(var_local(1:(nv3d+nv2d),1:nid_obs)) )
  IF( m /= (nv3d+nv2d)*nid_obs )THEN
    PRINT *, "error, no var loc is assumed"
    STOP
  END IF
  !
  ! FCST PERTURBATIONS
  !
  ALLOCATE(mean3d(nij1,nlev,nv3d))
  ALLOCATE(mean2d(nij1,nv2d))
  CALL ensmean_grd(nbv,nij1,gues3d,gues2d,mean3d,mean2d)
  DO n=1,nv3d ; DO m=1,nbv ; DO k=1,nlev ; DO i=1,nij1
    gues3d(i,k,m,n) = gues3d(i,k,m,n) - mean3d(i,k,n)
  END DO      ; END DO     ; END DO      ; END DO
  DO n=1,nv2d ; DO m=1,nbv ; DO i=1,nij1
    gues2d(i,m,n) = gues2d(i,m,n) - mean2d(i,n)
  END DO      ; END DO     ; END DO

  !
  ! random noises for PO method (Tsuyuki2024; JMSJ)
  !
  obsptb(:,:) = 0.0d0
  IF( myrank == 0 )THEN
    DO n=1,nobs
      CALL com_randn(nbv,obsptb(n,1:nbv))
      tmpave          = sum( obsptb(n,1:nbv) ) / dble( nbv )     ! mean
      obsptb(n,1:nbv) = ( obsptb(n,1:nbv) - tmpave ) * obserr(n) ! mean adjustment  & scale
    END DO
  ENDIF
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  CALL MPI_BCAST (obsptb,nobs*nbv,MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD,ierr) !! share random numbers for processes

  !
  ! random noises for resampling (Potthast et al., 2018; MWR)
  !
  IF( dastype >= 1 )THEN
    gr_common(:,:) = 0.0d0
    IF( myrank == 0 )THEN
      DO k=1,nbv
        CALL com_randn(nbv,gr_common(:,k))
      END DO
    END IF
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    CALL MPI_BCAST (gr_common,nbv*nbv,MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD,ierr) !! share random numbers for resampling
  END IF

  !
  ! likelihood succession for local particle filter
  !
  asis3d = 1.0d0/dble(nbv)  ;  asis2d = 1.0d0/dble(nbv)
  IF( fgt_factor<1.0d0 )THEN
    !==> each member for SIS (need to save all member)
    WRITE(wvecfile_o,'(a7,i10.10,a2,i6.6,a4)') './asis_lpf/',pymdh,'_M',1,'.grd'
    INQUIRE(FILE=wvecfile_o,EXIST=ex)
    IF( ex )THEN
      ALLOCATE( work3dg(nlon,nlat,nlev,nv3d), work3d(nij1,nlev,nv3d) )
      ALLOCATE( work2dg(nlon,nlat,nv2d)     , work2d(nij1,nv2d)      )
      DO m=1,nbv
        IF(myrank == 0) THEN
          WRITE(wvecfile_o,'(a7,i10.10,a2,i6.6,a4)') './asis_lpf/',pymdh,'_M',m,'.grd'
          CALL read_grd4_1atm(wvecfile_o,work3dg,work2dg)
          IF( m==1 .or. m==nbv ) &
          WRITE(6,'(A,I3.3,3A,4f8.3)') 'MYRANK ',myrank,' is reading.. ',wvecfile_o, "min-max(3d,2d) :: ", &
            minval( work3dg(:,:,:,:) ), maxval( work3dg(:,:,:,:) ), &
            minval( work2dg(:,:,:)   ), maxval( work2dg(:,:,:)   )
        END IF
        CALL scatter_grd_mpi(0,work3dg,work2dg,work3d,work2d)
        asis3d(:,:,:,m) = work3d(:,:,:)
        asis2d(:,:,m)   = work2d(:,:)
      END DO
      DEALLOCATE( work3dg, work3d )
      DEALLOCATE( work2dg, work2d )

      asis3d(:,:,:,:) = asis3d(:,:,:,:)*( 1.0d0-fgt_factor ) +  fgt_factor/dble(nbv) ! forgetting-factor for likelihood
      asis2d(:,:,:)   = asis2d(:,:,:)  *( 1.0d0-fgt_factor ) +  fgt_factor/dble(nbv) ! forgetting-factor for likelihood
    ELSE
      WRITE(6,'(2A)') '!!WARNING: no such file exist: ',wvecfile_o
      asis3d = 1.0d0/dble(nbv)  ;  asis2d = 1.0d0/dble(nbv)
    END IF   
  ENDIF
  !
  ! multiplicative inflation
  !
  IF(cov_infl_mul > 0.0d0) THEN ! fixed multiplicative inflation parameter
    ALLOCATE( work3d(nij1,nlev,nv3d) ) ; work3d = cov_infl_mul
    ALLOCATE( work2d(nij1,nv2d)      ) ; work2d = cov_infl_mul
    work3d(:,nlev,:) = 1.01d0
  END IF
  IF(cov_infl_mul <= 0.0d0) THEN ! 3D parameter values are read-in
    ALLOCATE( work3dg(nlon,nlat,nlev,nv3d), work2dg(nlon,nlat,nv2d) )
    ALLOCATE( work3d(nij1,nlev,nv3d)      , work2d(nij1,nv2d)       )
    write(inflfile_i(12:21), '(I10.10)') pymdh
    INQUIRE(FILE=inflfile_i,EXIST=ex)
    IF(ex) THEN
      IF(myrank == 0) THEN
        WRITE(6,'(A,I3.3,2A)') 'MYRANK ',myrank,' is reading.. ',inflfile_i
        CALL read_grd4(inflfile_i,work3dg,work2dg)
        PRINT *, "  CHECK INFLATIONS :: ", minval( work3dg(:,:,:,:) ), maxval( work3dg(:,:,:,:) )
      END IF
      CALL scatter_grd_mpi(0,work3dg,work2dg,work3d,work2d)
    ELSE
      WRITE(6,'(2A)') '!!WARNING: no such file exist: ',inflfile_i
      work3d = -1.0d0 * cov_infl_mul
      work2d = -1.0d0 * cov_infl_mul
    END IF
  END IF

  !==> SK 20181015 WI based on LUT
  IF( logic_wint ) THEN
    WRITE(6,'(A,I3.3,2A)') 'MYRANK ',myrank,' is reading.. BILINLUT.bin for WIP'
    OPEN(1,file='BILINLUT.bin',form="unformatted",access="direct",recl=nlon*nlat*8,action="read")
      READ(1,rec=1) msk(:,:)  
      READ(1,rec=2) wix(:,:,1)  ;  READ(1,rec=6) wiy(:,:,1)  ;  READ(1,rec=10) fac(:,:,1)
      READ(1,rec=3) wix(:,:,2)  ;  READ(1,rec=7) wiy(:,:,2)  ;  READ(1,rec=11) fac(:,:,2)
      READ(1,rec=4) wix(:,:,3)  ;  READ(1,rec=8) wiy(:,:,3)  ;  READ(1,rec=12) fac(:,:,3)
      READ(1,rec=5) wix(:,:,4)  ;  READ(1,rec=9) wiy(:,:,4)  ;  READ(1,rec=13) fac(:,:,4)
    CLOSE(1)

    msk_me(:)=UNDEF ; wix_me(:,:)=UNDEF ; wiy_me(:,:)=UNDEF ; fac_me(:,:)=UNDEF
    DO m=1,nprocs
      DO i=1,nij1node(m)
        j    = m-1 + nprocs * (i-1)
        ilon = MOD(j,nlon) + 1
        ilat = (j-ilon+1) / nlon + 1
        IF( myrank==m-1 ) THEN
          msk_me(i)     = msk(ilon,ilat)
          wix_me(i,1:4) = wix(ilon,ilat,1:4)
          wiy_me(i,1:4) = wiy(ilon,ilat,1:4)
          fac_me(i,1:4) = fac(ilon,ilat,1:4)
        END IF
      END DO
    END DO
  END IF

  !
  ! p_full for background ensemble mean
  !
  ALLOCATE(logpfm(nij1,nlev))
  CALL calc_pfull(nij1,1,mean2d(:,iv2d_ps),logpfm)
  logpfm = DLOG(logpfm)
  !
  ptimer = MPI_WTIME()
  WRITE(6,'(A,2F10.2)') '### TIMER in DAS_LETKF (INITIALIZE):',ptimer-ptimer01,ptimer-ptimer00
  ptimer01 = ptimer

  !========================================================================================================== Initialize
  nobs3d(:,:,:)=0.0d0 ; nobs2d(:,:)=0.0d0
  addo3d(:,:,:)=0.0d0 ; addo2d(:,:)=0.0d0
  adds3d(:,:,:)=0.0d0 ; adds2d(:,:)=0.0d0
  !========================================================================================================== MAIN ASSIMILATION LOOP
  IF( dastype==3 ) logic_lpfgm = .true.
  rsmp3d(1:nij1,1:nlev,1:nv3d) = 1.0d0 ! resampling (default) 
  rsmp2d(1:nij1,       1:nv2d) = 1.0d0 ! resampling (default)

  ! To fix random number for LPF
  call mk_iseed(ymdh, 1,1,1,iseed)
  call com_rand_seed(nbv,iseed,srand)
  !!!print *, "inp",myrank, iseed

  !!!! VT :: OT for all variables
  !!!IF( type_pfmtx==5 )THEN
  !!!  var_local_n2n(1) = 1
  !!!  DO n=2,nv3d+nv2d
  !!!    DO i=1,n
  !!!      var_local_n2n(n) = i
  !!!    END DO
  !!!  END DO
  !!!ENDIF

  ALLOCATE( hdxf(1:nobstotal,1:nbv),rdiag(1:nobstotal),rloc(1:nobstotal),dep(1:nobstotal),pob(1:nobstotal,nbv) )
  DO ilev=1,nlev
    !!WRITE(6,'(A,I3)') 'ilev = ',ilev
    DO ij=1,nij1
      DO n=1,nv3d
        cst(:,:) = 0.0d0
        DO j=1,nbv ; DO i=j+1,nbv
          cst(i,j) = ( gues3d(ij,ilev,i,n)-gues3d(ij,ilev,j,n) )**2.0d0
          cst(j,i) = cst(i,j)
        END DO     ; END DO

        IF(var_local_n2n(n) < n) THEN
          work3d(ij,ilev,n)            = work3d(ij,ilev,var_local_n2n(n))
          wvec3d(        n,1:nbv)      = wvec3d(        var_local_n2n(n),1:nbv)          
          wmat3d(        n,1:nbv,1:nbv)= wmat3d(        var_local_n2n(n),1:nbv,1:nbv)    
          amat3d(        n,1:nbv,1:nbv)= amat3d(        var_local_n2n(n),1:nbv,1:nbv)  
          qmat3d(        n,1:nbv,1:nbv)= qmat3d(        var_local_n2n(n),1:nbv,1:nbv) 
          nobs3d(ij,ilev,n)            = nobs3d(ij,ilev,var_local_n2n(n))               
          
          pvec3d(        n,1:nbv)      = pvec3d(        var_local_n2n(n),1:nbv)
          pmat3d(        n,1:nbv,1:nbv)= pmat3d(        var_local_n2n(n),1:nbv,1:nbv)
          peff3d(ij,ilev,n)            = peff3d(ij,ilev,var_local_n2n(n))
        ELSE
          CALL obs_local(ij,ilev,n,hdxf,rdiag,rloc,dep,pob,nobsl,logpfm)
          
          !work ; LETKF (dastype=0) :: adaptive covariance inflation of letkf_core
          !work ; DEBUG (dastype=1) :: adaptive covariance inflation of letkf_core
          !work ; LAPF  (dastype=2) :: adaptive resampling amplitude of lpf_core   (used if type_pfmtx==0 i.e., LAPF's resamplig mtx)
          !work ; LPFGM (dastype=3) :: adaptive resampling amplitude of lpf_core   (used if type_pfmtx==0 i.e., LAPF's resamplig mtx)
          parm = work3d(ij,ilev,n)
          IF( dastype==3 ) parm = GAMMA_GMPF     !! gamma of LPFGM

          nobs3d(ij,ilev,n) = REAL( nobsl, r_size )
          CALL letkf_core(nobstotal,nobsl,hdxf,rdiag,rloc,dep,pob,parm,logic_lpfgm, &
                          wvec3d(n,1:nbv),wmat3d(n,1:nbv,1:nbv),amat3d(n,1:nbv,1:nbv),qmat3d(n,1:nbv,1:nbv),peff3d(ij,ilev,n))
          IF( dastype==0 .or. dastype==1 ) work3d(ij,ilev,n) = parm ! for LETKF        , updated 
          IF( dastype==2 .or. dastype==3 ) parm = work3d(ij,ilev,n) ! for LAPF or LPFGM, overwrite
 
          DBG_REPLACE = .false. ! true==>replaced by KK's resampling for Sinkhorn
          IF( dastype==2 .or. dastype==3  ) THEN
            CALL lpf_core  (nobstotal,nobsl,hdxf,rdiag,rloc,dep,parm,gr_common,dastype, &
                            cst,asis3d(ij,ilev,n,1:nbv),peff3d(ij,ilev,n),pvec3d(n,1:nbv),pmat3d(n,1:nbv,1:nbv),DBG_REPLACE)                  
            IF( dastype==2 .or. dastype==3 ) work3d(ij,ilev,n) = parm ! LAPF or LPFGM

            !!IF( .not. DBG_REPLACE )THEN
            !!  PRINT '(4I,3F)', myrank, ilev, ij, nobsl, &
            !!    peff3d(ij,ilev,n), minval( pmat3d(n,1:nbv,1:nbv) ), maxval( pmat3d(n,1:nbv,1:nbv) )
            !!END IF

            ! check transform matrix 
            IF( ieee_is_nan(maxval(pmat3d(n,:,:))) .or. (.not.(ieee_is_normal(maxval(pmat3d(n,:,:))))) )THEN
              PRINT '(a,4I,2f)', "caution, pmat in lpf", myrank,ilev,ij,n,minval(pmat3d(n,:,:)), maxval(pmat3d(n,:,:))
            ENDIF
          ELSE
            pvec3d(n,1:nbv)       =1.0d0/DBLE(nbv)
            pmat3d(n,1:nbv,1:nbv) = Ident(1:nbv,1:nbv)
          ENDIF
        END IF        
      END DO
      IF(ilev == 1) THEN !update 2d variable at ilev=1
        DO n=1,nv2d
          cst(:,:) = 0.0d0
          DO j=1,nbv ; DO i=j+1,nbv
            cst(i,j) = ( gues2d(ij,i,n)-gues2d(ij,j,n) )**2.0d0
            cst(j,i) = cst(i,j)
          END DO     ; END DO

          IF(var_local_n2n(nv3d+n) <= nv3d) THEN
            work2d(ij,n)             = work3d(ij,ilev,var_local_n2n(nv3d+n))
            wvec2d(   n,1:nbv)       = wvec3d(        var_local_n2n(nv3d+n),1:nbv)             
            wmat2d(   n,1:nbv,1:nbv) = wmat3d(        var_local_n2n(nv3d+n),1:nbv,1:nbv)
            amat2d(   n,1:nbv,1:nbv) = amat3d(        var_local_n2n(nv3d+n),1:nbv,1:nbv)       
            qmat2d(   n,1:nbv,1:nbv) = qmat3d(        var_local_n2n(nv3d+n),1:nbv,1:nbv)    
            nobs2d(ij,n)             = nobs3d(ij,ilev,var_local_n2n(nv3d+n))                                      

            pvec2d(   n,1:nbv)       = pvec3d(        var_local_n2n(nv3d+n),1:nbv)
            pmat2d(   n,1:nbv,1:nbv) = pmat3d(        var_local_n2n(nv3d+n),1:nbv,1:nbv) 
            peff2d(ij,n)             = peff3d(ij,ilev,var_local_n2n(nv3d+n))
          ELSE IF(var_local_n2n(nv3d+n) < nv3d+n) THEN
            work2d(ij,n)             = work2d(ij,var_local_n2n(nv3d+n)-nv3d)
            wvec2d(   n,1:nbv)       = wvec2d(   var_local_n2n(nv3d+n)-nv3d,1:nbv)        
            wmat2d(   n,1:nbv,1:nbv) = wmat2d(   var_local_n2n(nv3d+n)-nv3d,1:nbv,1:nbv) 
            amat2d(   n,1:nbv,1:nbv) = amat2d(   var_local_n2n(nv3d+n)-nv3d,1:nbv,1:nbv)  
            qmat2d(   n,1:nbv,1:nbv) = qmat2d(   var_local_n2n(nv3d+n)-nv3d,1:nbv,1:nbv)  
            nobs2d(ij,n)             = nobs2d(ij,var_local_n2n(nv3d+n)-nv3d)              
            
            pvec2d(   n,1:nbv)       = pvec2d(   var_local_n2n(nv3d+n)-nv3d,1:nbv)
            pmat2d(   n,1:nbv,1:nbv) = pmat2d(   var_local_n2n(nv3d+n)-nv3d,1:nbv,1:nbv)
            peff2d(ij,n)             = peff2d(ij,var_local_n2n(nv3d+n)-nv3d)
          ELSE
            CALL obs_local(ij,ilev,nv3d+n,hdxf,rdiag,rloc,dep,pob,nobsl,logpfm)
            parm = work2d(ij,n)
            IF( dastype==3 ) parm = GAMMA_GMPF

            nobs2d(ij,n) = REAL( nobsl, r_size )
            CALL letkf_core(nobstotal,nobsl,hdxf,rdiag,rloc,dep,pob,parm,logic_lpfgm,&
                            wvec2d(n,1:nbv),wmat2d(n,1:nbv,1:nbv),amat2d(n,1:nbv,1:nbv),qmat2d(n,1:nbv,1:nbv),peff2d(ij,n))
            IF( dastype==0 .or. dastype==1 ) work2d(ij,n) = parm !! LETKF, UPDATE

            IF( dastype==2 .or. dastype==3 ) parm = work2d(ij,n) ! LAPF or LPFGM

            DBG_REPLACE = .false. ! true==>replaced by KK's resampling for Sinkhorn
            IF( dastype==2 .or. dastype==3  ) THEN
              CALL lpf_core  (nobstotal,nobsl,hdxf,rdiag,rloc,dep,parm,gr_common,dastype, &
                              cst,asis2d(ij,n,1:nbv),peff2d(ij,n),pvec2d(n,1:nbv),pmat2d(n,1:nbv,1:nbv),DBG_REPLACE)
              work2d(ij,n) = parm ! LAPF or LPFGM
              IF( ieee_is_nan(maxval(pmat2d(n,:,:))) .or. (.not.(ieee_is_normal(maxval(pmat2d(n,:,:))))) )THEN
                PRINT '(a,4I,2f)', "caution, pmat in lpf", myrank,ilev,ij,n,minval(pmat2d(n,:,:)), maxval(pmat2d(n,:,:))
              ENDIF
            ELSE
              pvec2d(n,1:nbv)      =1.0d0/DBLE(nbv)
              pmat2d(n,1:nbv,1:nbv)=Ident(1:nbv,1:nbv)
            ENDIF
          END IF
        END DO
      END IF

    !-----------------NOTE -------------------------------------------------------------------------------------------------!
    !##   wvec :: transform vector    ; LETKF for mean (IF logic_lpfgm=.TRUE. THEN wvec=0.0d0 )
    !##   wmat :: transform matrix    ; LETKF for ptb  (IF logic_lpfgm=.TRUE. THEN wvec=transform matrix for dXb as LETKF)
    !##   amat :: transform matrix    ; PO-EnKF  for ptb (only for stochastic part, for additive inflation)
    !##   qmat :: transform matrix    ; PO-EnKF  for ptb (for ensemble ptb update , for RTPE)
    !##   pvec :: weight of paticles  ; LPF   for mean
    !##   pmat :: transform mtx       ; LPF   for particles  (!!CAUTION!! do not use pvec and pmat SIMULTANEOUSLY)
    !##   asis :: SIS(log-likelihood) ; LPF   (inout)
    !##   peff :: effective particle  ; LPF
    !##   rmsp :: resampled or not    ; 0: no,  1: yes

    !-----------------0. WEIGHT TREATMENTS       ---------------------------------------------------------------------------!
      ! currently no weight interpolation and weight smoother

    !-----------------1. JUDGEMENT OF RESAMPLING ---------------------------------------------------------------------------!
    IF( dastype==1 .or. dastype==2 .or. dastype==3 )THEN
      !===> transform matrix = identity matrix I if no resampling
        DO n=1,nv3d
          IF( peff3d(ij,ilev,n) >= resample_m  .or.  peff3d(ij,ilev,n) <= 0.0d0      )THEN !==> no  resampling              
            rsmp3d(ij,ilev,n)             = 0.0d0
            pmat3d(        n,1:nbv,1:nbv) = Ident(1:nbv,1:nbv)
          ELSE                                                                             !==> yes resampling
            asis3d(ij,ilev,n,1:nbv)       = 1.0d0/dble(nbv)
          ENDIF
        END DO
      IF( ilev==1 )THEN
        DO n=1,nv2d
          IF( peff2d(ij,     n) >= resample_m  .or.  peff2d(ij,     n) <= 0.0d0      )THEN !==> no  resampling
            rsmp2d(ij,     n)             = 0.0d0
            pmat2d(        n,1:nbv,1:nbv) = Ident(1:nbv,1:nbv)
          ELSE                                                                             !==> yes resampling              
            asis2d(ij,     n,1:nbv)       = 1.0d0/dble(nbv)
          ENDIF
        END DO
      END IF
    END IF

    IF( type_relax==3 .or. type_relax==4 .or. type_relax==5 .or. type_relax==6 )THEN ! save trans vec/mtx of LETKF
                    lvec3d(1:nv3d,1:nbv)       = wvec3d(1:nv3d,1:nbv)
      IF( ilev==1 ) lvec2d(1:nv2d,1:nbv)       = wvec2d(1:nv2d,1:nbv)
                    lmat3d(1:nv3d,1:nbv,1:nbv) = wmat3d(1:nv3d,1:nbv,1:nbv)
      IF( ilev==1 ) lmat2d(1:nv2d,1:nbv,1:nbv) = wmat2d(1:nv2d,1:nbv,1:nbv)
    ENDIF
    !-----------------2. LETKF, LPF, LPFGM CONTROLLER ----------------------------------------------------------------------!
    IF( dastype==0 )THEN      ! update :: LETKF for mean (wvec) & LETKF for ptb (wmat)
                      asis3d(ij,ilev,1:nv3d,1:nbv) = 1.0d0/dble(nbv) ! no weight succession
        IF( ilev==1 ) asis2d(ij,     1:nv2d,1:nbv) = 1.0d0/dble(nbv) ! no weight succession
    ELSE IF( dastype==1 )THEN ! update :: LPF   for mean (pvec) & LETKF for ptb (wmat)
        wvec3d(        1:nv3d,1:nbv) = pvec3d(1:nv3d,1:nbv)
        asis3d(ij,ilev,1:nv3d,1:nbv) = 1.0d0/dble(nbv) ! no weight succession
        rsmp3d(ij,ilev,1:nv3d)       = 1.0d0           ! forced to employ resampling
      IF( ilev==1 )THEN
        wvec2d(        1:nv2d,1:nbv) = pvec2d(1:nv2d,1:nbv)
        asis2d(ij,     1:nv2d,1:nbv) = 1.0d0/dble(nbv) ! no weight succession
        rsmp2d(ij,     1:nv2d)       = 1.0d0           ! forced to employ resampling
      ENDIF
    ELSE IF( dastype==2 )THEN ! update ::                       & LPF   for ensemble (pmat)
        wvec3d(        1:nv3d,1:nbv)       = 0.0d0        
        wmat3d(        1:nv3d,1:nbv,1:nbv) = pmat3d(1:nv3d,1:nbv,1:nbv)
      IF( ilev==1 )THEN
        wvec2d(        1:nv2d,1:nbv)       = 0.0d0
        wmat2d(        1:nv2d,1:nbv,1:nbv) = pmat2d(1:nv2d,1:nbv,1:nbv)
      ENDIF
    ELSE IF ( dastype==3 )THEN ! update ::                        LPFGM for ensemble (wmat*pmat)
        !===> wvec is already defined to be 0.0 in common_letkf.f90
        DO n=1,nv3d
          wmat3d(        n,1:nbv,1:nbv) = matmul( wmat3d(n,1:nbv,1:nbv),pmat3d(n,1:nbv,1:nbv) )
        END DO
      IF( ilev==1 ) THEN
        DO n=1,nv2d
          wmat2d(        n,1:nbv,1:nbv) = matmul( wmat2d(n,1:nbv,1:nbv),pmat2d(n,1:nbv,1:nbv) ) 
        ENDDO
      ENDIF      
    END IF
    ! MEMO :: LPF has only W matrix, no need to have w for mean updates

    !-----------------3. Adjustment/Relaxation to Analysis/Posterior LETKF/EnKF -------------------------------------------!
    !               LETKF                    S-EnKF
    !  mean&ptb     type_relax==3 (ATAL)     type_relax==4 (ATAE)
    !  only ptb     type_relax==5 (RTRL)     type_relax==6 (RTRE)        
    IF( alph_relax/=0.0d0 ) THEN
      IF(       type_relax==3 .or. type_relax==5 )THEN ! PTB relaxed to Posterior LETKF
                      wmat3d(1:nv3d,1:nbv,1:nbv) = (1.0d0 - alph_relax)*wmat3d(1:nv3d,1:nbv,1:nbv) + alph_relax*lmat3d(1:nv3d,1:nbv,1:nbv)
        IF( ilev==1 ) wmat2d(1:nv2d,1:nbv,1:nbv) = (1.0d0 - alph_relax)*wmat2d(1:nv2d,1:nbv,1:nbv) + alph_relax*lmat2d(1:nv2d,1:nbv,1:nbv)
      ELSE IF ( type_relax==4 .or. type_relax==6 )THEN ! PTB relaxed to Posterior S-EnKF
                      wmat3d(1:nv3d,1:nbv,1:nbv) = (1.0d0 - alph_relax)*wmat3d(1:nv3d,1:nbv,1:nbv) + alph_relax*qmat3d(1:nv3d,1:nbv,1:nbv)
        IF( ilev==1 ) wmat2d(1:nv2d,1:nbv,1:nbv) = (1.0d0 - alph_relax)*wmat2d(1:nv2d,1:nbv,1:nbv) + alph_relax*qmat2d(1:nv2d,1:nbv,1:nbv)
      ENDIF

      IF(       type_relax==3 .or. type_relax==4 )THEN ! Adjustment to Analysis LETKF=S-EnKF
                      wvec3d(1:nv3d,1:nbv)       = (1.0d0 - alph_relax)*wvec3d(1:nv3d,1:nbv)       + alph_relax*lvec3d(1:nv3d,1:nbv)
        IF( ilev==1 ) wvec2d(1:nv2d,1:nbv)       = (1.0d0 - alph_relax)*wvec2d(1:nv2d,1:nbv)       + alph_relax*lvec2d(1:nv2d,1:nbv)
      ENDIF
    ENDIF

    !-----------------4. Additive Inflation --------------------------------------------------------------------------------!
    !!!IF( s_additive > 0.00001 )THEN 
    !!!                wmat3d(1:nv3d,1:nbv,1:nbv) = wmat3d(1:nv3d,1:nbv,1:nbv) + s_additive*amat3d(1:nv3d,1:nbv,1:nbv)
    !!!  IF( ilev==1 ) wmat2d(1:nv2d,1:nbv,1:nbv) = wmat2d(1:nv2d,1:nbv,1:nbv) + s_additive*amat2d(1:nv2d,1:nbv,1:nbv)
    !!!ENDIF

    !-----------------5. ENSEMBLE UPDATES ----------------------------------------------------------------------------------!

      DO n=1,nv3d
        DO m=1,nbv
          trans3(1:nbv,m)=wmat3d(n,1:nbv,m) + wvec3d(n,1:nbv)
        END DO
        DO m=1,nbv
          anal3d(ij,ilev,m,n) = mean3d(ij,ilev,n)
          DO k=1,nbv
            anal3d(ij,ilev,m,n) = anal3d(ij,ilev,m,n) + gues3d(ij,ilev,k,n) * trans3(k,m)
          END DO
        END DO

        !==> RELAXATION (RTPP or RTPS)        
        IF( alph_relax/=0.0d0 .and. (type_relax==1 .or. type_relax==2) ) THEN
          tmpave        = sum( anal3d(ij,ilev,1:nbv,n) ) / dble( nbv )
          tmpptb(1:nbv) = anal3d(ij,ilev,1:nbv,n) - tmpave
          IF(      type_relax==1 ) THEN ! RTPP
            anal3d(ij,ilev,1:nbv,n) = tmpave + alph_relax *gues3d(ij,ilev,1:nbv,n) + &
                                      (1.0d0 - alph_relax)*tmpptb(1:nbv) 
          ELSE IF( type_relax==2 ) THEN ! RTPS
            gusspr = dsqrt( sum( gues3d(ij,ilev,1:nbv,n)**2.0d0 ) / REAL(nbv-1,r_size))
            anlspr = dsqrt( sum( tmpptb(        1:nbv  )**2.0d0 ) / REAL(nbv-1,r_size))
            IF( anlspr<gusspr .and. anlspr>0.0d0 )THEN
              tmpinf                  = 1.0d0 - alph_relax + alph_relax*(gusspr/anlspr) ! inflation factor
              anal3d(ij,ilev,1:nbv,n) = tmpave + tmpptb(1:nbv)*tmpinf
            END IF             
          END IF                                      
        ENDIF

        !==> PO-based Additive Inflation
        IF( s_additive > 0.00001 )THEN
          addi3d(1:nbv,n)=0.0d0
          DO m=1,nbv ; DO k=1,nbv            
            addi3d(m,n) = addi3d(m,n) + gues3d(ij,ilev,k,n) * amat3d(n,k,m)            
          END DO     ; END DO
          addi3d(        1:nbv,n) = addi3d(1:nbv,n)*s_additive                ! amplitude scaled
          tmpave                  = sum( addi3d(1:nbv,n) ) / dble( nbv )
          tmpspr                  = dsqrt( sum( (addi3d(1:nbv,n)-tmpave)**2.0d0 ) / REAL(nbv-1,r_size))

          !adjustment (may be unnecesary)
          !addi3d(        1:nbv,n) = addi3d(1:nbv,n) - tmpave

          anal3d(ij,ilev,1:nbv,n) = anal3d(ij,ilev,1:nbv,n) + addi3d(1:nbv,n)
          addo3d(ij,ilev,      n) =                           addi3d(1,    n) ! 1st member for output
          adds3d(ij,ilev,      n) =                           tmpspr          ! ensemble spread
        ENDIF
      END DO ! n
      IF(ilev >= 5) THEN !no analysis for upper-level Q
        DO m=1,nbv
          anal3d(ij,ilev,m,iv3d_q) = mean3d(ij,ilev,iv3d_q) + gues3d(ij,ilev,m,iv3d_q)
        END DO
      END IF

      IF(ilev == 1) THEN
        DO n=1,nv2d
          DO m=1,nbv
            trans2(1:nbv,m)=wmat2d(n,1:nbv,m)  + wvec2d(n,1:nbv)
          END DO
          DO m=1,nbv
            anal2d(ij,m,n)  = mean2d(ij,n)
            DO k=1,nbv
              anal2d(ij,m,n) = anal2d(ij,m,n) + gues2d(ij,k,n) * trans2(k,m)
            END DO
          END DO

          !==> RELAXATION (RTPP or RTPS)
          IF( alph_relax/=0.0d0 .and. (type_relax==1 .or. type_relax==2) ) THEN
            tmpave        = sum( anal2d(ij,1:nbv,n) ) / dble( nbv )
            tmpptb(1:nbv) = anal2d(ij,1:nbv,n) - tmpave
            IF(      type_relax==1 ) THEN ! RTPP
              anal2d(ij,1:nbv,n) = tmpave + alph_relax *gues2d(ij,1:nbv,n) + &
                                   (1.0d0 - alph_relax)*tmpptb(1:nbv) 
            ELSE IF( type_relax==2 ) THEN ! RTPS
              gusspr = dsqrt( sum( gues2d(ij,1:nbv,n)**2.0d0 ) / REAL(nbv-1,r_size))
              anlspr = dsqrt( sum( tmpptb(   1:nbv  )**2.0d0 ) / REAL(nbv-1,r_size))
              IF( anlspr<gusspr .and. anlspr>0.0d0 )THEN
                tmpinf             = 1.0d0 - alph_relax + alph_relax*(gusspr/anlspr) ! inflation factor
                anal2d(ij,1:nbv,n) = tmpave + tmpptb(1:nbv)*tmpinf
              END IF             
            END IF
          ENDIF

          !==> PO-based Additive inflation
          IF( s_additive > 0.00001 )THEN
            addi2d(1:nbv,n)=0.0d0
            DO m=1,nbv ; DO k=1,nbv            
              addi2d(m,n) = addi2d(m,n) + gues2d(ij,k,n) * amat2d(n,k,m)            
            END DO     ; END DO
            addi2d(   1:nbv,n) = addi2d(1:nbv,n)*s_additive           ! amplitude scaled
            tmpave             = sum( addi2d(1:nbv,n) ) / dble( nbv )            
            tmpspr             = dsqrt( sum( (addi2d(1:nbv,n)-tmpave)**2.0d0 ) / REAL(nbv-1,r_size))

            !adjustment (may be unnecesary)
            !addi2d(1:nbv,n)    = addi2d(1:nbv,n) - tmpave

            anal2d(ij,1:nbv,n) = anal2d(ij,1:nbv,n) + addi2d(1:nbv,n)
            addo2d(ij,      n) =                      addi2d(1,    n) ! 1st member for output
            adds2d(ij,      n) =                      tmpspr          ! ensemble spread
          ENDIF
        END DO !n
      END IF
    !--------------3). ENSEMBLE UPDATES (END)---------------------------------------------------------------!

    END DO !(End of IJ loop)   
    ptimer = MPI_WTIME()
    WRITE(6,'(A,2F10.2,A,i2)') '### TIMER in DAS_LETKF (MAIN LETKF):',ptimer-ptimer01,ptimer-ptimer00, "  Level: ",ilev
    ptimer01 = ptimer

  !------------------ END OF NEW CODING FOR GATHER, FILTER, SCATTER, ANALYZE-----------------------------------------------!
  END DO   !(End of Assimilation loop; ilev)
  ptimer = MPI_WTIME()
  WRITE(6,'(A,2F10.2)') '### TIMER in DAS_LETKF (END LETKFs):',ptimer-ptimer01,ptimer-ptimer00
  ptimer01 = ptimer

  !!!!===> check consistency of random number
  !!!call com_rand_seed(nbv,iseed,srand)
  !!!print '(i5,5f8.5)', myrank, srand(1:5) 

  DEALLOCATE(hdxf,rdiag,rloc,dep)

  !========================================================================================================== OUTPUT STR
  !--(1.1) all filters :: adaptive inflation factor
  IF(cov_infl_mul < 0.0d0) THEN
    CALL gather_grd_mpi(0,work3d,work2d,work3dg,work2dg)
    IF(myrank == 0) THEN
      write(inflfile_o(12:21), '(I10.10)') ymdh
      WRITE(6,'(A,I3.3,2A)') 'MYRANK ',myrank,' is writing.. ',inflfile_o
      CALL write_grd4(inflfile_o,work3dg,work2dg)
    END IF
    DEALLOCATE(work3dg,work2dg,work3d,work2d)
  END IF
  IF( ALLOCATED( work3dg ) ) DEALLOCATE( work3dg )
  IF( ALLOCATED( work2dg ) ) DEALLOCATE( work2dg ) 
  IF( ALLOCATED( work3d  ) ) DEALLOCATE( work3d  ) 
  IF( ALLOCATED( work2d  ) ) DEALLOCATE( work2d  ) 

  ALLOCATE( work3dg(nlon,nlat,nlev,nv3d), work2dg(nlon,nlat,nv2d) )
  ALLOCATE( work3d(nij1,nlev,nv3d)      , work2d(nij1,nv2d)       )

  !--(1.2) all filters : nobs
    CALL gather_grd_mpi(0,nobs3d,nobs2d,work3dg,work2dg)
    IF(myrank == 0) THEN
      write(nobsfile_o(8:17), '(I10.10)') ymdh
      WRITE(6,'(A,I3.3,2A)') 'MYRANK ',myrank,' is writing.. ',nobsfile_o
      CALL write_grd4(nobsfile_o,work3dg,work2dg)
    END IF

  !--(1.3) all filters : PO-based additive inflation
  PRINT *, myrank, s_additive, TRIM(addofile_o)
  IF( s_additive > 0.00001 )THEN
    ! 1st member
    CALL gather_grd_mpi(0,addo3d,addo2d,work3dg,work2dg)
    IF(myrank == 0) THEN
      write(addofile_o(19:28), '(I10.10)') ymdh
      WRITE(6,'(A,I3.3,2A)') 'MYRANK ',myrank,' is writing.. ',addofile_o
      CALL write_grd4(addofile_o,work3dg,work2dg)
    END IF
    ! ensemble spread
    CALL gather_grd_mpi(0,adds3d,adds2d,work3dg,work2dg)
    IF(myrank == 0) THEN
      write(addsfile_o(17:26), '(I10.10)') ymdh
      WRITE(6,'(A,I3.3,2A)') 'MYRANK ',myrank,' is writing.. ',addofile_o
      CALL write_grd4(addsfile_o,work3dg,work2dg)
    END IF
  ENDIF

!  CHARACTER(32) :: addofile_o='./infl_add/000001/yyyymmddhh.grd'
!  CHARACTER(30) :: addsfile_o='./infl_add/sprd/yyyymmddhh.grd'

  !--(1.4) all filters : additive inflation
  IF(sp_infl_add > 0.0d0) THEN
    CALL read_ens_mpi('addi',nbv,gues3d,gues2d)
    CALL ensmean_grd(nbv,nij1,gues3d,gues2d,work3d,work2d)
    DO n=1,nv3d ; DO m=1,nbv ; DO k=1,nlev ; DO i=1,nij1
      gues3d(i,k,m,n) = gues3d(i,k,m,n) - work3d(i,k,n)
    END DO      ; END DO     ; END DO      ; END DO
    DO n=1,nv2d ; DO m=1,nbv ; DO i=1,nij1
      gues2d(i,m,n) = gues2d(i,m,n) - work2d(i,n)
    END DO      ; END DO     ; END DO

    WRITE(6,'(A)') '===== Additive covariance inflation ====='
    WRITE(6,'(A,F10.4)') '  parameter:',sp_infl_add
    WRITE(6,'(A)') '========================================='
!    parm = 0.7d0
!    DO ilev=1,nlev
!      parm_infl_damp(ilev) = 1.0d0 + parm &
!        & + parm * REAL(1-ilev,r_size)/REAL(nlev_dampinfl,r_size)
!      parm_infl_damp(ilev) = MAX(parm_infl_damp(ilev),1.0d0)
!    END DO
    DO n=1,nv3d ; DO m=1,nbv ; DO ilev=1,nlev ; DO ij=1,nij1
      anal3d(ij,ilev,m,n) = anal3d(ij,ilev,m,n) + gues3d(ij,ilev,m,n) * sp_infl_add
    END DO      ; END DO     ; END DO         ; END DO
    DO n=1,nv2d ; DO m=1,nbv ; DO ij=1,nij1
      anal2d(ij,m,n) = anal2d(ij,m,n) + gues2d(ij,m,n) * sp_infl_add
    END DO      ; END DO     ; END DO
  END IF ! Additive inflation

  !!!!--(1.4) all filters : output weight vector (mainly for LETKF)
  !!!IF( logic_wout )THEN
  !!!  !==> max weight
  !!!  DO n=1,nv3d ; DO ilev=1,nlev ; DO ij=1,nij1
  !!!    work3d(ij,ilev,n) = maxval( abs(wvec3d(ij,ilev,n,:)) )
  !!!  END DO      ; END DO         ; END DO
  !!!  DO n=1,nv2d ;                  DO ij=1,nij1
  !!!    work2d(ij,n)      = maxval( abs(wvec2d(ij,n,:)     ) )
  !!!  END DO      ;                  END DO
  !!!  CALL gather_grd_mpi(0,work3d,work2d,work3dg,work2dg)
  !!!  IF(myrank == 0) THEN
  !!!    WRITE(wvecfile_o,'(a7,i10.10,a)') './wvec/',ymdh,'_Mensmax.grd'
  !!!    WRITE(6,'(A,I3.3,2A)') 'MYRANK ',myrank,' is writing.. (max member)',wvecfile_o
  !!!    CALL write_grd4_1atm(wvecfile_o,work3dg,work2dg)
  !!!  END IF
  !!!
  !!!  !==> each member for monitor (resulting weights)
  !!!  !DO m=1,nbv
  !!!  DO m=1,2
  !!!    work3d(:,:,:) = wvec3d(:,:,:,m) ; work2d(:,:)   = wvec2d(:,:,m)
  !!!    CALL gather_grd_mpi(0,work3d,work2d,work3dg,work2dg)       
  !!!    IF(myrank == 0) THEN
  !!!      WRITE(wvecfile_o,'(a7,i10.10,a2,i6.6,a4)') './wvec/',ymdh,'_M',m,'.grd'
  !!!      IF( m==1   ) WRITE(6,'(A,I3.3,2A)') 'MYRANK ',myrank,' is writing.. (fst member): ',wvecfile_o
  !!!      IF( m==nbv ) WRITE(6,'(A,I3.3,2A)') 'MYRANK ',myrank,' is writing.. (fnl member): ',wvecfile_o
  !!!      CALL write_grd4_1atm(wvecfile_o,work3dg,work2dg)
  !!!    END IF
  !!!  END DO
  !!!ENDIF

  !--(2.1) PFs : Effective Particle Size & Selectited Particle Numbers
  IF( dastype==0 )THEN
    CALL gather_grd_mpi(0,peff3d,peff2d,work3dg,work2dg)
    IF(myrank == 0) THEN
      write(pletfile_o(14:23), '(I10.10)') ymdh
      WRITE(6,'(A,I3.3,2A)') 'MYRANK ',myrank,' is writing.. ',pletfile_o
      PRINT *, "temporal monitor, min-max peff of letkf ::", minval(work3dg), maxval(work3dg)
      CALL write_grd4(pletfile_o,work3dg,work2dg)
    END IF

  ELSE IF( dastype==1 .or. dastype==2 .or. dastype==3 )THEN
    CALL gather_grd_mpi(0,peff3d,peff2d,work3dg,work2dg)
    IF(myrank == 0) THEN
      write(pefffile_o(12:21), '(I10.10)') ymdh
      WRITE(6,'(A,I3.3,2A)') 'MYRANK ',myrank,' is writing.. ',pefffile_o
      CALL write_grd4(pefffile_o,work3dg,work2dg)
    END IF

    !==> resampled or not (0: no, 1: yes)
    CALL gather_grd_mpi(0,rsmp3d,rsmp2d,work3dg,work2dg)
    IF(myrank == 0) THEN
      write(rsmpfile_o(12:21), '(I10.10)') ymdh
      WRITE(6,'(A,I3.3,2A)') 'MYRANK ',myrank,' is writing.. ',rsmpfile_o
      CALL write_grd4(rsmpfile_o,work3dg,work2dg)
    END IF

    !--(2.2) PFs : weight for each member for SIS (need to save all member)
    IF( fgt_factor < 1.0d0 )THEN
      DO m=1,nbv
        work3d(:,:,:) = asis3d(:,:,:,m) ; work2d(:,:)   = asis2d(:,:,m)
        CALL gather_grd_mpi(0,work3d,work2d,work3dg,work2dg)       
        IF(myrank == 0) THEN
          WRITE(wvecfile_o,'(a7,i10.10,a2,i6.6,a4)') './asis_lpf/',ymdh,'_M',m,'.grd'
          IF( m==1   ) WRITE(6,'(A,I3.3,2A)') 'MYRANK ',myrank,' is writing.. (fst member): ',wvecfile_o
          IF( m==nbv ) WRITE(6,'(A,I3.3,2A)') 'MYRANK ',myrank,' is writing.. (fnl member): ',wvecfile_o
          CALL write_grd4_1atm(wvecfile_o,work3dg,work2dg)
        END IF
      END DO    
    ENDIF
  ENDIF

  DEALLOCATE(work3dg,work2dg,work3d,work2d)
  !========================================================================================================== OUTPUT END


  ptimer = MPI_WTIME()
  WRITE(6,'(A,2F10.2)') '### TIMER in DAS_LETKF (EXIT LETKF):',ptimer-ptimer01,ptimer-ptimer00
  ptimer01 = ptimer
!
  DEALLOCATE(logpfm,mean3d,mean2d)
  RETURN
END SUBROUTINE das_letkf

!-----------------------------------------------------------------------
! Project global observations to local
!     (hdxf_g,dep_g,rdiag_g) -> (hdxf,dep,rdiag)
!-----------------------------------------------------------------------
SUBROUTINE obs_local(ij,ilev,nvar,hdxf,rdiag,rloc,dep,pob,nobsl,logpfm)
  IMPLICIT NONE
  INTEGER,INTENT(IN) :: ij,ilev,nvar
  REAL(r_size),INTENT(IN) :: logpfm(nij1,nlev)
  REAL(r_size),INTENT(OUT) :: hdxf(nobstotal,nbv)
  REAL(r_size),INTENT(OUT) :: rdiag(nobstotal)
  REAL(r_size),INTENT(OUT) :: rloc(nobstotal)
  REAL(r_size),INTENT(OUT) :: dep(nobstotal)
  REAL(r_size),INTENT(OUT) :: pob(nobstotal,nbv)
  INTEGER,INTENT(OUT) :: nobsl
  REAL(r_size) :: minlon,maxlon,minlat,maxlat,dist,dlev
  REAL(r_size) :: tmplon,tmplat,tmperr,tmpwgt(nlev)
  INTEGER :: tmpqc
  INTEGER,ALLOCATABLE:: nobs_use(:)
!TVS  INTEGER,ALLOCATABLE:: ntvs_use_prof(:),ntvs_use_inst(:),ntvs_use_slot(:)
  INTEGER :: imin,imax,jmin,jmax,im,ichan
  INTEGER :: n,nn,tvnn,iobs
!
! INITIALIZE
!
  IF( nobs > 0 ) THEN
    ALLOCATE(nobs_use(nobs))
  END IF
!TVS  IF( ntvs > 0 ) THEN
!TVS    ALLOCATE(ntvs_use_prof(ntvs))
!TVS    ALLOCATE(ntvs_use_inst(ntvs))
!TVS    ALLOCATE(ntvs_use_slot(ntvs))
!TVS  END IF
!
! data search
!
  minlon = lon1(ij) - dlon_zero(ij)
  maxlon = lon1(ij) + dlon_zero(ij)
  minlat = lat1(ij) - dlat_zero
  maxlat = lat1(ij) + dlat_zero
  IF(maxlon - minlon >= 360.0d0) THEN
    minlon = 0.0d0
    maxlon = 360.0d0
  END IF

  DO jmin=1,nlat-2
    IF(minlat < lat(jmin+1)) EXIT
  END DO
  DO jmax=1,nlat-2
    IF(maxlat < lat(jmax+1)) EXIT
  END DO
  nn = 1
!TVS  tvnn = 1
  IF(minlon >= 0 .AND. maxlon <= 360.0) THEN
    DO imin=1,nlon-1
      IF(minlon < lon(imin+1)) EXIT
    END DO
    DO imax=1,nlon-1
      IF(maxlon < lon(imax+1)) EXIT
    END DO
    IF( nobs > 0 ) &
    & CALL obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)
!TVS    IF( ntvs > 0 ) &
!TVS    & CALL tvs_local_sub(imin,imax,jmin,jmax,tvnn, &
!TVS    &                    ntvs_use_prof,ntvs_use_inst,ntvs_use_slot)
  ELSE IF(minlon >= 0 .AND. maxlon > 360.0) THEN
    DO imin=1,nlon-1
      IF(minlon < lon(imin+1)) EXIT
    END DO
    maxlon = maxlon - 360.0d0
    IF(maxlon > 360.0d0) THEN
      imin = 1
      imax = nlon
      IF( nobs > 0 ) &
      & CALL obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)
!TVS      IF( ntvs > 0 ) &
!TVS      & CALL tvs_local_sub(imin,imax,jmin,jmax,tvnn, &
!TVS      &                    ntvs_use_prof,ntvs_use_inst,ntvs_use_slot)
    ELSE
      DO imax=1,nlon-1
        IF(maxlon < lon(imax+1)) EXIT
      END DO
      IF(imax > imin) THEN
        imin = 1
        imax = nlon
        IF( nobs > 0 ) &
        & CALL obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)
!TVS        IF( ntvs > 0 ) &
!TVS        & CALL tvs_local_sub(imin,imax,jmin,jmax,tvnn, &
!TVS        &                    ntvs_use_prof,ntvs_use_inst,ntvs_use_slot)
      ELSE
        imin = 1
        IF( nobs > 0 ) &
        & CALL obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)
!TVS        IF( ntvs > 0 ) &
!TVS        & CALL tvs_local_sub(imin,imax,jmin,jmax,tvnn, &
!TVS        &                    ntvs_use_prof,ntvs_use_inst,ntvs_use_slot)
        DO imin=1,nlon-1
          IF(minlon < lon(imin+1)) EXIT
        END DO
        imax = nlon
        IF( nobs > 0 ) &
        & CALL obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)
!TVS        IF( ntvs > 0 ) &
!TVS        & CALL tvs_local_sub(imin,imax,jmin,jmax,tvnn, &
!TVS        &                    ntvs_use_prof,ntvs_use_inst,ntvs_use_slot)
      END IF
    END IF
  ELSE IF(minlon < 0 .AND. maxlon <= 360.0d0) THEN
    DO imax=1,nlon-1
      IF(maxlon < lon(imax+1)) EXIT
    END DO
    minlon = minlon + 360.0d0
    IF(minlon < 0) THEN
      imin = 1
      imax = nlon
      IF( nobs > 0 ) &
      & CALL obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)
!TVS      IF( ntvs > 0 ) &
!TVS      & CALL tvs_local_sub(imin,imax,jmin,jmax,tvnn, &
!TVS      &                    ntvs_use_prof,ntvs_use_inst,ntvs_use_slot)
    ELSE
      DO imin=1,nlon-1
        IF(minlon < lon(imin+1)) EXIT
      END DO
      IF(imin < imax) THEN
        imin = 1
        imax = nlon
        IF( nobs > 0 ) &
        & CALL obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)
!TVS        IF( ntvs > 0 ) &
!TVS        & CALL tvs_local_sub(imin,imax,jmin,jmax,tvnn, &
!TVS        &                    ntvs_use_prof,ntvs_use_inst,ntvs_use_slot)
      ELSE
        imin = 1
        IF( nobs > 0 ) &
        & CALL obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)
!TVS        IF( ntvs > 0 ) &
!TVS        & CALL tvs_local_sub(imin,imax,jmin,jmax,tvnn, &
!TVS        &                    ntvs_use_prof,ntvs_use_inst,ntvs_use_slot)
        DO imin=1,nlon-1
          IF(minlon < lon(imin+1)) EXIT
        END DO
        imax = nlon
        IF( nobs > 0 ) &
        & CALL obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)
!TVS        IF( ntvs > 0 ) &
!TVS        & CALL tvs_local_sub(imin,imax,jmin,jmax,tvnn, &
!TVS        &                    ntvs_use_prof,ntvs_use_inst,ntvs_use_slot)
      END IF
    END IF
  ELSE
    maxlon = maxlon - 360.0d0
    minlon = minlon + 360.0d0
    IF(maxlon > 360.0 .OR. minlon < 0) THEN
      imin = 1
      imax = nlon
      IF( nobs > 0 ) &
      & CALL obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)
!TVS      IF( ntvs > 0 ) &
!TVS      & CALL tvs_local_sub(imin,imax,jmin,jmax,tvnn, &
!TVS      &                    ntvs_use_prof,ntvs_use_inst,ntvs_use_slot)
    ELSE
      DO imin=1,nlon-1
        IF(minlon < lon(imin+1)) EXIT
      END DO
      DO imax=1,nlon-1
        IF(maxlon < lon(imax+1)) EXIT
      END DO
      IF(imin > imax) THEN
        imin = 1
        imax = nlon
        IF( nobs > 0 ) &
        & CALL obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)
!TVS        IF( ntvs > 0 ) &
!TVS        & CALL tvs_local_sub(imin,imax,jmin,jmax,tvnn, &
!TVS        &                    ntvs_use_prof,ntvs_use_inst,ntvs_use_slot)
      ELSE
        IF( nobs > 0 ) &
        & CALL obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)
!TVS        IF( ntvs > 0 ) &
!TVS        & CALL tvs_local_sub(imin,imax,jmin,jmax,tvnn, &
!TVS        &                    ntvs_use_prof,ntvs_use_inst,ntvs_use_slot)
      END IF
    END IF
  END IF
  nn = nn-1
!TVS  tvnn = tvnn -1
!TVS  IF( nn < 1 .AND. tvnn < 1 ) THEN
  IF(nn < 1) THEN
    nobsl = 0
    RETURN
  END IF
!
! CONVENTIONAL
!
  nobsl = 0
  IF(nn > 0) THEN
    DO n=1,nn
      !
      ! vertical localization
      !
      IF(NINT(obselm(nobs_use(n))) == id_ps_obs .AND. ilev > 1) THEN
        dlev = ABS(LOG(obsdat(nobs_use(n))) - logpfm(ij,ilev))
      ELSE IF(NINT(obselm(nobs_use(n))) /= id_ps_obs) THEN
        dlev = ABS(LOG(obslev(nobs_use(n))) - logpfm(ij,ilev))
      ELSE
        dlev = 0.0d0
      END IF
      IF(dlev > dist_zerov) CYCLE
      !
      ! horizontal localization
      !
      tmplon=obslon(nobs_use(n))
      tmplat=obslat(nobs_use(n))
      CALL com_distll_1( tmplon, tmplat,lon1(ij), lat1(ij), dist)
      IF(dist > dist_zero ) CYCLE
      !
      ! variable localization
      !
      SELECT CASE(NINT(obselm(nobs_use(n))))
      CASE(id_u_obs)
        iobs=1
      CASE(id_v_obs)
        iobs=2
      CASE(id_t_obs)
        iobs=3
      CASE(id_q_obs)
        iobs=4
      CASE(id_rh_obs)
        iobs=5
      CASE(id_ps_obs)
        iobs=6
      CASE(id_rain_obs)
        iobs=7
      END SELECT
      IF(var_local(nvar,iobs) < TINY(var_local)) CYCLE

      nobsl = nobsl + 1
      hdxf(nobsl,:) = obshdxf(nobs_use(n),:)
      dep(nobsl)    = obsdep(nobs_use(n))
      !
      ! Observational localization
      !
      tmperr=obserr(nobs_use(n))
      rdiag(nobsl) = tmperr * tmperr
      rloc(nobsl)  = EXP(-0.5d0 * ((dist/sigma_obs)**2 + (dlev/sigma_obsv)**2)) &
                  & * var_local(nvar,iobs)
      pob(nobsl,1:nbv) = obsptb(nobs_use(n),1:nbv)
    END DO
  END IF
!TVS!
!TVS! ATOVS
!TVS!
!TVS  IF(tvnn > 0) THEN
!TVS    DO n=1,tvnn
!TVS      tmplon=tvslon(ntvs_use_prof(n),ntvs_use_inst(n),ntvs_use_slot(n))
!TVS      tmplat=tvslat(ntvs_use_prof(n),ntvs_use_inst(n),ntvs_use_slot(n))
!TVS      CALL com_distll_1( tmplon, tmplat, lon1(ij), lat1(ij), dist)
!TVS      IF( dist > dist_zero) CYCLE
!TVS
!TVS      DO ichan=1,ntvsch(ntvs_use_inst(n))
!TVS        tmperr=tvserr(ichan,ntvs_use_prof(n),ntvs_use_inst(n),ntvs_use_slot(n))
!TVS        tmpqc=tvsqc(ichan,ntvs_use_prof(n),ntvs_use_inst(n),ntvs_use_slot(n))
!TVS        tmpwgt(:)=tvswgt(:,ichan, &
!TVS                         & ntvs_use_prof(n), &
!TVS                         & ntvs_use_inst(n), &
!TVS                         & ntvs_use_slot(n))
!TVS        IF( tmpqc == 1 .AND. tmpwgt(ilev) > 0.05D0 ) THEN
!TVS          nobsl = nobsl + 1
!TVS          DO im = 1, nbv
!TVS            hdxf(nobsl,im) = tvshdxf(im,ichan, &
!TVS                              & ntvs_use_prof(n), &
!TVS                              & ntvs_use_inst(n), &
!TVS                              & ntvs_use_slot(n))
!TVS          END DO
!TVS          dep(nobsl)    = tvsdep(ichan, &
!TVS                              & ntvs_use_prof(n), &
!TVS                              & ntvs_use_inst(n), &
!TVS                              & ntvs_use_slot(n))
!TVS          rdiag(nobsl)  = tmperr * tmperr &
!TVS                        & * exp(0.5d0 * (dist/sigma_obs)**2) &
!TVS                        & / (tmpwgt(ilev) * tmpwgt(ilev))
!TVS        END IF
!TVS      END DO
!TVS    END DO
!TVS  END IF
!
! DEBUG
! IF( ILEV == 1 .AND. ILON == 1 ) &
! & WRITE(6,*) 'ILEV,ILON,ILAT,NN,TVNN,NOBSL=',ilev,ij,nn,tvnn,nobsl
!
  IF( nobsl > nobstotal ) THEN
    WRITE(6,'(A,I5,A,I5)') 'FATAL ERROR, NOBSL=',nobsl,' > NOBSTOTAL=',nobstotal
    WRITE(6,*) 'IJ,NN,TVNN=', ij, nn, tvnn
    STOP 99
  END IF
!
  IF( nobs > 0 ) THEN
    DEALLOCATE(nobs_use)
  END IF
!TVS  IF( ntvs > 0 ) THEN
!TVS    DEALLOCATE(ntvs_use_prof)
!TVS    DEALLOCATE(ntvs_use_inst)
!TVS    DEALLOCATE(ntvs_use_slot)
!TVS  END IF
!
  RETURN
END SUBROUTINE obs_local

SUBROUTINE obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)
  INTEGER,INTENT(IN) :: imin,imax,jmin,jmax
  INTEGER,INTENT(INOUT) :: nn, nobs_use(nobs)
  INTEGER :: j,n,ib,ie,ip

  DO j=jmin,jmax
    IF(imin > 1) THEN
      ib = nobsgrd(imin-1,j)+1
    ELSE
      IF(j > 1) THEN
        ib = nobsgrd(nlon,j-1)+1
      ELSE
        ib = 1
      END IF
    END IF
    ie = nobsgrd(imax,j)
    n = ie - ib + 1
    IF(n == 0) CYCLE
    DO ip=ib,ie
      IF(nn > nobs) THEN
        WRITE(6,*) 'FATALERROR, NN > NOBS', NN, NOBS
      END IF
      nobs_use(nn) = ip
      nn = nn + 1
    END DO
  END DO

  RETURN
END SUBROUTINE obs_local_sub

!TVSSUBROUTINE tvs_local_sub(imin,imax,jmin,jmax,nn,ntvs_prof,ntvs_inst,ntvs_slot)
!TVS  INTEGER,INTENT(IN) :: imin,imax,jmin,jmax
!TVS  INTEGER,INTENT(INOUT) :: nn, ntvs_prof(ntvs), ntvs_inst(ntvs), ntvs_slot(ntvs)
!TVS  INTEGER :: j,n,ib,ie,ip
!TVS  INTEGER :: islot, iinst
!TVS
!TVS  DO j=jmin,jmax
!TVS    DO islot=1,nslots
!TVS      DO iinst=1,ninstrument
!TVS        IF(imin > 1) THEN
!TVS          ib = ntvsgrd(imin-1,j,iinst,islot)+1
!TVS        ELSE
!TVS          IF(j > 1) THEN
!TVS            ib = ntvsgrd(nlon,j-1,iinst,islot)+1
!TVS          ELSE
!TVS            ib = 1
!TVS          END IF
!TVS        END IF
!TVS        ie = ntvsgrd(imax,j,iinst,islot)
!TVS        n = ie - ib + 1
!TVS        IF(n == 0) CYCLE
!TVS        DO ip=ib,ie
!TVS          IF(nn > nobs) THEN
!TVS            WRITE(6,*) 'FATALERROR, NN > NTVS', NN, NTVS
!TVS          END IF
!TVS          ntvs_prof(nn)=ip
!TVS          ntvs_inst(nn)=iinst
!TVS          ntvs_slot(nn)=islot
!TVS          nn = nn + 1
!TVS        END DO
!TVS      END DO
!TVS    END DO
!TVS  END DO
!TVS  RETURN
!TVSEND SUBROUTINE tvs_local_sub
!TVS!-----------------------------------------------------------------------
!TVS! Data Assimilation for VARBC
!TVS!-----------------------------------------------------------------------
!TVSSUBROUTINE das_vbc(um,vm,tm,qm,qlm,psm,vbcf,vbca)
!TVS  USE common_mtx
!TVS  IMPLICIT NONE
!TVS  REAL(r_size),INTENT(IN) :: um(nij1,nlev)
!TVS  REAL(r_size),INTENT(IN) :: vm(nij1,nlev)
!TVS  REAL(r_size),INTENT(IN) :: tm(nij1,nlev)
!TVS  REAL(r_size),INTENT(IN) :: qm(nij1,nlev)
!TVS  REAL(r_size),INTENT(IN) :: qlm(nij1,nlev)
!TVS  REAL(r_size),INTENT(IN) :: psm(nij1)
!TVS  REAL(r_size),INTENT(INOUT) :: vbcf(maxvbc,maxtvsch,ninstrument)
!TVS  REAL(r_size),INTENT(OUT)   :: vbca(maxvbc,maxtvsch,ninstrument)
!TVS  REAL(r_sngl) :: u4(nlon,nlat,nlev)
!TVS  REAL(r_sngl) :: v4(nlon,nlat,nlev)
!TVS  REAL(r_sngl) :: t4(nlon,nlat,nlev)
!TVS  REAL(r_sngl) :: q4(nlon,nlat,nlev)
!TVS  REAL(r_sngl) :: ql4(nlon,nlat,nlev)
!TVS  REAL(r_sngl) :: ps4(nlon,nlat)
!TVS  REAL(r_size) :: u(nlon,nlat,nlev)
!TVS  REAL(r_size) :: v(nlon,nlat,nlev)
!TVS  REAL(r_size) :: t(nlon,nlat,nlev)
!TVS  REAL(r_size) :: q(nlon,nlat,nlev)
!TVS  REAL(r_size) :: ql(nlon,nlat,nlev)
!TVS  REAL(r_size) :: ps(nlon,nlat)
!TVS  REAL(r_size) :: p_full(nlon,nlat,nlev)
!TVS  REAL(r_size),ALLOCATABLE :: hx(:,:,:,:)
!TVS  REAL(r_size),ALLOCATABLE :: pred(:,:,:,:,:)
!TVS  INTEGER,ALLOCATABLE :: tmpqc(:,:,:)
!TVS  REAL(r_size),ALLOCATABLE :: tmpwgt(:,:,:,:)
!TVS  REAL(r_size) :: a(maxvbc,maxvbc)
!TVS  REAL(r_size) :: b(maxvbc)
!TVS  REAL(r_size) :: ainv(maxvbc,maxvbc)
!TVS  INTEGER:: ntvschan1(maxtvsch,ninstrument)
!TVS  INTEGER:: i,j,k,n,islot,nn
!TVS
!TVS  PRINT *,'Hello from das_vbc'
!TVS
!TVS  IF(ntvs == 0) THEN
!TVS    PRINT *,'No radiance data: das_vbc skipped..'
!TVS!$OMP PARALLEL WORKSHARE
!TVS    vbca = vbcf
!TVS!$OMP END PARALLEL WORKSHARE
!TVS    RETURN
!TVS  END IF
!TVS
!TVS  CALL gather_grd_mpi(0,um,vm,tm,qm,qlm,psm,u4,v4,t4,q4,ql4,ps4)
!TVS  n = nlon*nlat*nlev
!TVS  CALL MPI_BARRIER(MPI_COMM_WORLD,i)
!TVS  CALL MPI_BCAST(u4(1,1,1),n,MPI_REAL,0,MPI_COMM_WORLD,i)
!TVS  CALL MPI_BCAST(v4(1,1,1),n,MPI_REAL,0,MPI_COMM_WORLD,i)
!TVS  CALL MPI_BCAST(t4(1,1,1),n,MPI_REAL,0,MPI_COMM_WORLD,i)
!TVS  CALL MPI_BCAST(q4(1,1,1),n,MPI_REAL,0,MPI_COMM_WORLD,i)
!TVS  CALL MPI_BCAST(ql4(1,1,1),n,MPI_REAL,0,MPI_COMM_WORLD,i)
!TVS  n = nlon*nlat
!TVS  CALL MPI_BCAST(ps4(1,1),n,MPI_REAL,0,MPI_COMM_WORLD,i)
!TVS!$OMP PARALLEL WORKSHARE
!TVS  u = REAL(u4,r_size)
!TVS  v = REAL(v4,r_size)
!TVS  t = REAL(t4,r_size)
!TVS  q = REAL(q4,r_size)
!TVS  ql = REAL(ql4,r_size)
!TVS  ps = REAL(ps4,r_size)
!TVS!$OMP END PARALLEL WORKSHARE
!TVS  CALL calc_pfull(ps,p_full)
!TVS
!TVS  ALLOCATE( hx(maxtvsch,maxtvsprof,ninstrument,nslots) )
!TVS  ALLOCATE( pred(maxvbc,maxtvsch,maxtvsprof,ninstrument,nslots) )
!TVS  ALLOCATE( tmpqc(maxtvsch,maxtvsprof,ninstrument) )
!TVS  ALLOCATE( tmpwgt(nlev,maxtvsch,maxtvsprof,ninstrument) )
!TVS  DO islot=1,nslots
!TVS!    IF(SUM(ntvsprofslots(:,islot)) == 0) CYCLE
!TVS    ntvsprof(:) = ntvsprofslots(:,islot)
!TVS    CALL Trans_XtoY_tvs(u,v,t,q,ql,ps,p_full, &
!TVS      & tvslon(:,:,islot),tvslat(:,:,islot),tvszenith(:,:,islot),&
!TVS      & tvsskin(:,:,islot),tvsstmp(:,:,islot),tvsclw(:,:,islot),&
!TVS      & tvsemis(:,:,:,islot),tmpqc,hx(:,:,:,islot),tmpwgt,pred(:,:,:,:,islot))
!TVS  END DO
!TVS  DEALLOCATE(tmpqc,tmpwgt)
!TVS
!TVS!$OMP PARALLEL PRIVATE(j,k,n,a,b,ainv)
!TVS!$OMP WORKSHARE
!TVS  vbca = 0.0d0
!TVS!$OMP END WORKSHARE
!TVS!$OMP DO SCHEDULE(DYNAMIC)
!TVS  DO k=1,ninstrument
!TVS    DO j=1,maxtvsch
!TVS      !
!TVS      ! Parallel processing
!TVS      !
!TVS      IF(MOD(j+maxtvsch*(k-1)-1,nprocs) /= myrank) CYCLE
!TVS      !
!TVS      ! DATA NUMBER
!TVS      !
!TVS      ntvschan(j,k) = SUM(tvsqc(j,:,k,:))
!TVS      IF(msw_vbc .AND. ntvschan(j,k) /= 0 ) THEN
!TVS        PRINT '(3A,I3,A,I6)',' >> VBC executed for instrument,channel,ntvsl: ',&
!TVS                            & tvsname(k),',',tvsch(j,k),',',ntvschan(j,k)
!TVS        CALL vbc_local(j,k,ntvschan(j,k),hx,pred,a,b)
!TVS        CALL mtx_inv(maxvbc,a,ainv)
!TVS        vbca(:,j,k) = vbcf(:,j,k)
!TVS        DO n=1,maxvbc
!TVS          vbca(:,j,k) = vbca(:,j,k) - ainv(:,n)*b(n) !ATTN: sign for beta
!TVS        END DO
!TVS      ELSE
!TVS        PRINT '(3A,I3,A,I6)',' !! NO VBC executed for instrument,channel,ntvsl: ',&
!TVS                            & tvsname(k),',',tvsch(j,k),',',ntvschan(j,k)
!TVS        vbca(:,j,k) = vbcf(:,j,k)
!TVS      END IF
!TVS    END DO
!TVS  END DO
!TVS!$OMP END DO
!TVS!$OMP WORKSHARE
!TVS  vbcf = vbca
!TVS  ntvschan1 = ntvschan
!TVS!$OMP END WORKSHARE
!TVS!$OMP END PARALLEL
!TVS  DEALLOCATE(hx,pred)
!TVS  n = maxvbc*maxtvsch*ninstrument
!TVS  CALL MPI_BARRIER(MPI_COMM_WORLD,j)
!TVS  CALL MPI_ALLREDUCE(vbcf,vbca,n,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,j)
!TVS  n = maxtvsch*ninstrument
!TVS  CALL MPI_BARRIER(MPI_COMM_WORLD,j)
!TVS  CALL MPI_ALLREDUCE(ntvschan1,ntvschan,n,MPI_INTEGER,MPI_SUM,MPI_COMM_WORLD,j)
!TVS
!TVS  RETURN
!TVSEND SUBROUTINE das_vbc
!TVS!-----------------------------------------------------------------------
!TVS!  (in) ichan: channnel
!TVS!  (in) iinst: sensor
!TVS!  (out) a = B_beta^-1 + p R^-1 p^T
!TVS!  (out) b = p R^-1 d
!TVS!-----------------------------------------------------------------------
!TVSSUBROUTINE vbc_local(ichan,iinst,ntvsl,hx,pred,a,b)
!TVS  IMPLICIT NONE
!TVS  INTEGER,PARAMETER :: msw=1
!TVS  INTEGER,PARAMETER :: nmin=400
!TVS  INTEGER,INTENT(IN) :: ichan,iinst,ntvsl
!TVS  REAL(r_size),INTENT(IN) :: hx(maxtvsch,maxtvsprof,ninstrument,nslots)
!TVS  REAL(r_size),INTENT(IN) :: pred(maxvbc,maxtvsch,maxtvsprof,ninstrument,nslots)
!TVS  REAL(r_size),INTENT(OUT) :: a(maxvbc,maxvbc)
!TVS  REAL(r_size),INTENT(OUT) :: b(maxvbc)
!TVS  REAL(r_size) :: dep,dep0
!TVS  REAL(r_size) :: bias,bias0
!TVS  REAL(r_size) :: r,tmp
!TVS  INTEGER:: islot, iprof, i,j,n
!TVS
!TVS  a = 0.0d0
!TVS  b = 0.0d0
!TVS  dep = 0.0d0
!TVS  dep0 = 0.0d0
!TVS  bias = 0.0d0
!TVS  bias0 = 0.0d0
!TVS  n = 0
!TVS  DO islot=1,nslots
!TVS    DO iprof=1,maxtvsprof
!TVS      IF(tvsqc(ichan,iprof,iinst,islot)/=1) CYCLE
!TVS      !
!TVS      ! R
!TVS      !
!TVS      r = tvserr(ichan,iprof,iinst,islot)**2
!TVS      !
!TVS      ! p R^-1 p^T
!TVS      !
!TVS      DO j=1,maxvbc
!TVS        DO i=1,maxvbc
!TVS          a(i,j) = a(i,j) &
!TVS               & + pred(i,ichan,iprof,iinst,islot) &
!TVS               & * pred(j,ichan,iprof,iinst,islot) / r
!TVS        END DO
!TVS      END DO
!TVS      !
!TVS      ! B_beta^-1
!TVS      !
!TVS      IF(msw == 1) THEN ! Y.Sato
!TVS        IF(ntvsl < nmin) THEN
!TVS          tmp = REAL(nmin,r_size) / r
!TVS
!TVS        ELSE
!TVS          tmp = (REAL(ntvsl,r_size) &
!TVS            & / (LOG10(REAL(ntvsl,r_size)/REAL(nmin,r_size))+1.0d0)) / r
!TVS        END IF
!TVS      ELSE IF(msw == 2) THEN ! D.Dee
!TVS        tmp = REAL(ntvsl,r_size) / r
!TVS      ELSE ! Constant
!TVS        tmp = 100.0d0
!TVS      END IF
!TVS      DO i=1,maxvbc
!TVS        a(i,i) = a(i,i) + tmp
!TVS      END DO
!TVS      !
!TVS      ! p R^-1 d
!TVS      !
!TVS      b(:) = b(:) + pred(:,ichan,iprof,iinst,islot) / r &
!TVS                & *(tvsdat(ichan,iprof,iinst,islot)-hx(ichan,iprof,iinst,islot))
!TVS      bias = bias+tvsdat(ichan,iprof,iinst,islot)-hx(ichan,iprof,iinst,islot)
!TVS      dep = dep+(tvsdat(ichan,iprof,iinst,islot)-hx(ichan,iprof,iinst,islot))**2
!TVS      bias0= bias0+tvsdep(ichan,iprof,iinst,islot)
!TVS      dep0= dep0+tvsdep(ichan,iprof,iinst,islot)**2
!TVS      n = n+1
!TVS    END DO
!TVS  END DO
!TVS
!TVS  dep = SQRT(dep / REAL(n,r_size))
!TVS  dep0 = SQRT(dep0 / REAL(n,r_size))
!TVS  bias = bias / REAL(n,r_size)
!TVS  bias0 = bias0 / REAL(n,r_size)
!TVS  PRINT '(2A,I3,4F12.4)',' >> D monit: ',tvsname(iinst),tvsch(ichan,iinst),bias0,bias,dep0,dep
!TVS
!TVS  RETURN
!TVSEND SUBROUTINE vbc_local

END MODULE letkf_tools
