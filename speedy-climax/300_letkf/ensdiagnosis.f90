program ensdiagnosis
!=======================================================================
!
! [PURPOSE:] Main program of ensemble diagnosis
!
! [HISTORY:]
!   08/02/2024 Shunji Kotsuki  developed for speedy/climax
!
!=======================================================================
  USE common
  USE common_mpi
  USE common_time
  USE common_letkf
  USE common_lpf
  USE common_XXXXXX
  USE common_share
  USE common_mpi_XXXXXX

  IMPLICIT NONE

  REAL(r_size), ALLOCATABLE :: xens3d(:,:,:,:,:), xens2d(:,:,:,:)
  REAL(r_size), ALLOCATABLE :: xsum3d(:,:,:,:),   xsum2d(:,:,:)
  REAL(r_size), ALLOCATABLE :: xave3d(:,:,:,:),   xave2d(:,:,:)
  REAL(r_size), ALLOCATABLE :: xspr3d(:,:,:,:),   xspr2d(:,:,:)
  REAL(r_size), ALLOCATABLE :: xtru3d(:,:,:,:),   xtru2d(:,:,:)

  REAL(r_size), ALLOCATABLE :: xmpi3d(:,:,:,:),   xmpi2d(:,:,:) ! ensemble state
  REAL(r_size), ALLOCATABLE :: dmpi3d(:,:,:,:),   dmpi2d(:,:,:) ! ensemble diff w.r.t. reference
  REAL(r_size), ALLOCATABLE :: ampi3d(:,:,:),     ampi2d(:,:)   ! ensemble mean
  REAL(r_size), ALLOCATABLE :: smpi3d(:,:,:),     smpi2d(:,:)   ! ensemble sprd
  REAL(r_size), ALLOCATABLE :: cmpi3d(:,:,:),     cmpi2d(:,:)   ! CRPS Spread
  REAL(r_size), ALLOCATABLE :: ympi3d(:,:,:),     ympi2d(:,:)   ! reference

  INTEGER :: ii, jj, kk, ll, ij, nn, mm
  INTEGER :: im, jm, l_member
  CHARACTER(256) :: filename, rmsename, sprdname, rmaxname, rminname

  INTEGER :: ierr

  !==> ensdiag.cnf
  CHARACTER(256) :: basedir, valddir, truedir 
  LOGICAL        :: logic_mspr, logic_verf

  CALL initialize_mpi
  CALL set_timer
  rtimer00 = MPI_WTIME() ; rtimer01 = rtimer00
  CALL set_common_XXXXXX
  CALL set_common_mpi_XXXXXX
  IF(myrank == 0) print '(A6, I6, A9, I10)', 'hello from obsope,  nbv = ', nbv, ', YMDH = ', ymdh

  namelist / letkf_param / sigma_obs, nbv, pymdh, ymdh,                                  &
    logic_wout, logic_wsth, logic_wint, dastype, cov_infl_mul, s_additive, hpo_weight,   &
    resample_m, type_wnorm, type_pfmtx, type_relax, alph_relax, fgt_factor, tau_nmlize, snk_lambda, gamma_gmpf

  namelist / ensdiag_param / nbv, ymdh, basedir, valddir, truedir, logic_mspr, logic_verf
  !====================================================================================
  OPEN(1,file='letkf.cnf'  )  ;  READ(1,nml=letkf_param  )  ;  CLOSE(1)
  OPEN(1,file='ensdiag.cnf')  ;  READ(1,nml=ensdiag_param)  ;  CLOSE(1) !overwrite ymdh

  IF(myrank == 0) print *, 'Compute Ensemble Mean & Spread with MPI'
  IF(myrank == 0) print *, 'nprocs =', nprocs
  CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)

  IF( mod(nbv, nprocs) == 0 ) THEN
    l_member = nbv/nprocs
  ELSE
    l_member = idint(dint(dble(nbv)/dble(nprocs))) + 1
  ENDIF

  ALLOCATE( xens3d(nlon,nlat,nlev,nv3d,l_member), xens2d(nlon,nlat,nv2d,l_member) )
  ALLOCATE( xave3d(nlon,nlat,nlev,nv3d         ), xave2d(nlon,nlat,nv2d)          )
  ALLOCATE( xspr3d(nlon,nlat,nlev,nv3d         ), xspr2d(nlon,nlat,nv2d)          )
  ALLOCATE( xsum3d(nlon,nlat,nlev,nv3d         ), xsum2d(nlon,nlat,nv2d)          )
  ALLOCATE( xtru3d(nlon,nlat,nlev,nv3d         ), xtru2d(nlon,nlat,nv2d)          )

  ALLOCATE( xmpi3d(nij1,nlev,nbv,nv3d)          , xmpi2d(nij1,     nbv,nv2d)      )
  ALLOCATE( dmpi3d(nij1,nlev,nbv,nv3d)          , dmpi2d(nij1,     nbv,nv2d)      )
  ALLOCATE( ampi3d(nij1,nlev,    nv3d)          , ampi2d(nij1,         nv2d)      )
  ALLOCATE( smpi3d(nij1,nlev,    nv3d)          , smpi2d(nij1,         nv2d)      )
  ALLOCATE( cmpi3d(nij1,nlev,    nv3d)          , cmpi2d(nij1,         nv2d)      )
  ALLOCATE( ympi3d(nij1,nlev,    nv3d)          , ympi2d(nij1,         nv2d)      )

  CALL read_ens_mpi(     trim(basedir),nbv,xmpi3d,xmpi2d)
  rtimer = MPI_WTIME() ; rtimerl(1) = rtimer-rtimer01 ; rtimer01 = rtimer

  IF( logic_mspr )CALL write_ensmspr_mpi(trim(basedir),nbv,xmpi3d,xmpi2d)  
  rtimer = MPI_WTIME() ; rtimerl(2) = rtimer-rtimer01 ; rtimer01 = rtimer

  IF( logic_verf ) THEN ! verification
    CALL read_mspdata
    rtimer = MPI_WTIME() ; rtimerl(3) = rtimer-rtimer01 ; rtimer01 = rtimer
    
    CALL read_trudata
    rtimer = MPI_WTIME() ; rtimerl(4) = rtimer-rtimer01 ; rtimer01 = rtimer
    
    CALL calc_crps
    rtimer = MPI_WTIME() ; rtimerl(5) = rtimer-rtimer01 ; rtimer01 = rtimer
  ENDIF

  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  IF( myrank==0 )THEN
    WRITE(6,*) ''
    WRITE(6,'(A,2F10.2,A)') '### TIMER(READ  ENS ):', rtimerl(1), rtimerl(1)/sum(rtimerl(1:5))*100, ' %'
    WRITE(6,'(A,2F10.2,A)') '### TIMER(WRITE MSPR):', rtimerl(2), rtimerl(2)/sum(rtimerl(1:5))*100, ' %'
    WRITE(6,'(A,2F10.2,A)') '### TIMER(READ  MSPR):', rtimerl(3), rtimerl(3)/sum(rtimerl(1:5))*100, ' %'
    WRITE(6,'(A,2F10.2,A)') '### TIMER(READ  TRUE):', rtimerl(4), rtimerl(4)/sum(rtimerl(1:5))*100, ' %'
    WRITE(6,'(A,2F10.2,A)') '### TIMER(CALC  CRPS):', rtimerl(5), rtimerl(5)/sum(rtimerl(1:5))*100, ' %'
    WRITE(6,'(A,2F10.2,A)') '### TIMER(TOTAL)     :', sum(rtimerl(1:5)), sum(rtimerl(1:5))/sum(rtimerl(1:5))*100, ' %'
  ENDIF

  CALL MPI_Finalize(ierr)
  STOP

CONTAINS
!-----------------------------------------------------------------------!
SUBROUTINE calc_crps
  IMPLICIT NONE
  INTEGER, PARAMETER :: ng=4 ! glb, nph, trp, sph
  REAL(r_size) :: wlat(nlat,ng), slat(ng)
  REAL(r_size) :: wmpi(nij1,ng), smpi(ng), wtmp(ng)

  REAL(r_size) :: rmse3d(nlev,nv3d,ng),  rmse2d(nv2d,ng),  rmse3o(nlev,nv3d,ng),  rmse2o(nv2d,ng)  ! RMSE
  REAL(r_size) :: sprd3d(nlev,nv3d,ng),  sprd2d(nv2d,ng),  sprd3o(nlev,nv3d,ng),  sprd2o(nv2d,ng)  ! Spread
  REAL(r_size) :: crsk3d(nlev,nv3d,ng),  crsk2d(nv2d,ng),  crsk3o(nlev,nv3d,ng),  crsk2o(nv2d,ng)  ! CRPS Skill
  REAL(r_size) :: crsp3d(nlev,nv3d,ng),  crsp2d(nv2d,ng),  crsp3o(nlev,nv3d,ng),  crsp2o(nv2d,ng)  ! CRPS Spread

  REAL(r_size) :: rens3d(nlev,nv3d,ng,nbv), rens2d(nv2d,ng,nbv) ! RMSE Ensemble 
  REAL(r_size) :: rens3o(nlev,nv3d,ng,nbv), rens2o(nv2d,ng,nbv) ! RMSE Ensemble

  REAL(r_size) :: rmax3d(nlev,nv3d,ng),  rmax2d(nv2d,ng),  rmax3o(nlev,nv3d,ng),  rmax2o(nv2d,ng)  ! RMSE Ensemble Max
  REAL(r_size) :: rmin3d(nlev,nv3d,ng),  rmin2d(nv2d,ng),  rmin3o(nlev,nv3d,ng),  rmin2o(nv2d,ng)  ! RMSE Ensemble Min


  !==> preparison
  wlat(:,:)=0.0d0
  DO jj=1,nlat
    wlat(jj,1) = dcos( lat(jj)*pi/180.0d0 )
    IF     ( lat(jj)> 20.0d0 ) THEN ; wlat(jj,2)=wlat(jj,1)
    ELSE IF( lat(jj)<-20.0d0 ) THEN ; wlat(jj,3)=wlat(jj,1)
    ELSE                            ; wlat(jj,4)=wlat(jj,1)
    ENDIF
    !!IF(myrank==0) PRINT '(I,5f)',jj,lat(jj),wglb(jj),wnph(jj),wtrp(jj),wsph(jj)
  END DO
  DO ii=1,ng
    slat(ii)=sum(wlat(1:nlat,ii))*dble(nlon)
  ENDDO

  wmpi(:,:)=0.0d0
  DO ij=1,nij1
    wmpi(ij,1) = dcos( lat1(ij)*pi/180.0d0 )
    IF     ( lat1(ij)> 20.0d0 ) THEN ; wmpi(ij,2)=wmpi(ij,1)
    ELSE IF( lat1(ij)<-20.0d0 ) THEN ; wmpi(ij,3)=wmpi(ij,1)
    ELSE                             ; wmpi(ij,4)=wmpi(ij,1)
    ENDIF
  END DO

  DO nn=1,ng ; wtmp(nn)=sum(wmpi(1:nij1,nn)) ; ENDDO
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  CALL MPI_ALLREDUCE(wtmp,smpi,ng,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)

  DO nn=1,ng
    !!!PRINT '(2I5,2F15.5)', myrank, nn, slat(nn),smpi(nn)
    wmpi(1:nij1,nn)=wmpi(1:nij1,nn)/smpi(nn) 
  END DO

  !==> initialization
  rmse3d(:,:,:)=0.0d0 ; rmse2d(:,:)=0.0d0 ; sprd3d(:,:,:)=0.0d0 ; sprd2d(:,:)=0.0d0
  crsk3d(:,:,:)=0.0d0 ; crsk2d(:,:)=0.0d0 ; crsp3d(:,:,:)=0.0d0 ; crsp2d(:,:)=0.0d0
  rmax3d(:,:,:)=0.0d0 ; rmax2d(:,:)=0.0d0 ; rmin3d(:,:,:)=0.0d0 ; rmin2d(:,:)=0.0d0

  !==> RMSE / SPREAD
  DO nn=1,ng ; DO ll=1,nv3d ; DO kk=1,nlev ; DO ij=1,nij1 
    rmse3d(kk,ll,nn)=rmse3d(kk,ll,nn)+wmpi(ij,nn)*( ampi3d(ij,kk,ll)-ympi3d(ij,kk,ll) )**2.0d0
    sprd3d(kk,ll,nn)=sprd3d(kk,ll,nn)+wmpi(ij,nn)*( smpi3d(ij,kk,ll)                  )**2.0d0
!    crsp3d(kk,ll,nn)=crsp3d(kk,ll,nn)+wmpi(ij,nn)*( cmpi3d(ij,kk,ll)                  )
  END DO     ; END DO       ; END DO       ; END DO 

  DO nn=1,ng ; DO ll=1,nv2d ; DO ij=1,nij1
    rmse2d(   ll,nn)=rmse2d(   ll,nn)+wmpi(ij,nn)*( ampi2d(ij,   ll)-ympi2d(ij,   ll) )**2.0d0
    sprd2d(   ll,nn)=sprd2d(   ll,nn)+wmpi(ij,nn)*( smpi2d(ij,   ll)                  )**2.0d0
!    crsp2d(   ll,nn)=crsp2d(   ll,nn)+wmpi(ij,nn)*( cmpi2d(ij,   ll)                  )
  END DO     ; END DO       ; END DO

  CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)
  CALL MPI_REDUCE(rmse3d,rmse3o,nlev*nv3d*ng,MPI_DOUBLE_PRECISION,MPI_SUM,0,MPI_COMM_WORLD,ierr)
  CALL MPI_REDUCE(sprd3d,sprd3o,nlev*nv3d*ng,MPI_DOUBLE_PRECISION,MPI_SUM,0,MPI_COMM_WORLD,ierr)
  CALL MPI_REDUCE(rmse2d,rmse2o,     nv2d*ng,MPI_DOUBLE_PRECISION,MPI_SUM,0,MPI_COMM_WORLD,ierr)
  CALL MPI_REDUCE(sprd2d,sprd2o,     nv2d*ng,MPI_DOUBLE_PRECISION,MPI_SUM,0,MPI_COMM_WORLD,ierr)
  rmse3o(:,:,:) = dsqrt( rmse3o(:,:,:) ) ; rmse2o(:,:)   = dsqrt( rmse2o(:,:)   )
  sprd3o(:,:,:) = dsqrt( sprd3o(:,:,:) ) ; sprd2o(:,:)   = dsqrt( sprd2o(:,:)   )

!!! Regacy, comp of rmse for all members
!!!  rens3d(:,:,:,:)=0.0d0 ; rens2d(:,:,:)=0.0d0
!!!  DO nn=1,ng ; DO mm=1,nbv ; DO ll=1,nv3d ; DO kk=1,nlev ; DO ij=1,nij1 
!!!    rens3d(kk,ll,mm,nn)=rens3d(kk,ll,mm,nn)+wmpi(ij,nn)*( xmpi3d(ij,kk,mm,ll)-ympi3d(ij,kk,ll) )**2.0d0
!!!  END DO     ; END DO       ; END DO       ; END DO      ; END DO
!!!  DO nn=1,ng ; DO mm=1,nbv ; DO ll=1,nv2d ; DO ij=1,nij1
!!!    rens2d(   ll,mm,nn)=rens2d(   ll,mm,nn)+wmpi(ij,nn)*( xmpi2d(ij,   mm,ll)-ympi2d(ij,   ll) )**2.0d0
!!!  END DO     ; END DO       ; END DO      ; END DO
!!!  CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)
!!!  CALL MPI_REDUCE(rens3d,rens3o,nlev*nv3d*ng*nbv,MPI_DOUBLE_PRECISION,MPI_SUM,0,MPI_COMM_WORLD,ierr)
!!!  CALL MPI_REDUCE(rens2d,rens2o,     nv2d*ng*nbv,MPI_DOUBLE_PRECISION,MPI_SUM,0,MPI_COMM_WORLD,ierr)
!!!  rens3o(:,:,:,:) = dsqrt( rens3o(:,:,:,:) ) ; rens2o(:,:,:)   = dsqrt( rens2o(:,:,:)   )
!!!  DO nn=1,ng ; DO ll=1,nv3d ; DO kk=1,nlev
!!!    rmax3o(kk,ll,nn) = maxval( rens3o(kk,ll,1:nbv,nn) )
!!!    rmin3o(kk,ll,nn) = minval( rens3o(kk,ll,1:nbv,nn) )
!!!  END DO     ; END DO       ; END DO
!!!
!!!  DO nn=1,ng ; DO ll=1,nv2d
!!!    rmax2o(   ll,nn) = maxval( rens2o(   ll,1:nbv,nn) )
!!!    rmin2o(   ll,nn) = minval( rens2o(   ll,1:nbv,nn) )
!!!  END DO     ; END DO

  !==> Ensemle Diff w.r.t. Reference
  DO mm=1,nbv
    dmpi3d(1:nij1,1:nlev,mm,1:nv3d) = abs( xmpi3d(1:nij1,1:nlev,mm,1:nv3d)-ympi3d(1:nij1,1:nlev,1:nv3d) )
    dmpi2d(1:nij1,       mm,1:nv2d) = abs( xmpi2d(1:nij1,       mm,1:nv2d)-ympi2d(1:nij1,       1:nv2d) )
  END DO

  !==> RMSE / SPREAD
  DO nn=1,ng ; DO ll=1,nv3d ; DO kk=1,nlev ; DO ij=1,nij1 
    rmax3d(kk,ll,nn)=rmax3d(kk,ll,nn)+wmpi(ij,nn)*( maxval(dmpi3d(ij,kk,1:nbv,ll)) )**2.0d0
    rmin3d(kk,ll,nn)=rmin3d(kk,ll,nn)+wmpi(ij,nn)*( minval(dmpi3d(ij,kk,1:nbv,ll)) )**2.0d0
  END DO     ; END DO       ; END DO       ; END DO 

  DO nn=1,ng ; DO ll=1,nv2d ; DO ij=1,nij1
    rmax2d(   ll,nn)=rmax2d(   ll,nn)+wmpi(ij,nn)*( maxval(dmpi2d(ij,   1:nbv,ll)) )**2.0d0
    rmin2d(   ll,nn)=rmin2d(   ll,nn)+wmpi(ij,nn)*( minval(dmpi2d(ij,   1:nbv,ll)) )**2.0d0
  END DO     ; END DO       ; END DO

  CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)
  CALL MPI_REDUCE(rmax3d,rmax3o,nlev*nv3d*ng,MPI_DOUBLE_PRECISION,MPI_SUM,0,MPI_COMM_WORLD,ierr)
  CALL MPI_REDUCE(rmin3d,rmin3o,nlev*nv3d*ng,MPI_DOUBLE_PRECISION,MPI_SUM,0,MPI_COMM_WORLD,ierr)
  CALL MPI_REDUCE(rmax2d,rmax2o,     nv2d*ng,MPI_DOUBLE_PRECISION,MPI_SUM,0,MPI_COMM_WORLD,ierr)
  CALL MPI_REDUCE(rmin2d,rmin2o,     nv2d*ng,MPI_DOUBLE_PRECISION,MPI_SUM,0,MPI_COMM_WORLD,ierr)
  rmax3o(:,:,:) = dsqrt( rmax3o(:,:,:) ) ; rmax2o(:,:)   = dsqrt( rmax2o(:,:)   )
  rmin3o(:,:,:) = dsqrt( rmin3o(:,:,:) ) ; rmin2o(:,:)   = dsqrt( rmin2o(:,:)   )

  !==> CRPS, Skill and Spread terms
  DO nn=1,ng ; DO ll=1,nv3d
    !skill
    DO im=1,nbv ; DO kk=1,nlev ; DO ij=1,nij1    
          crsk3d(kk,ll,nn)=crsk3d(kk,ll,nn)+wmpi(ij,nn)*abs( xmpi3d(ij,kk,im,ll)-ympi3d(ij,kk,   ll) )/dble(nbv) 
    END DO      ; END DO       ; END DO
    
    !spread
    DO im=1,nbv ; DO jm=1,nbv
      IF( im /= jm )THEN
        DO kk=1,nlev ; DO ij=1,nij1
          crsp3d(kk,ll,nn)=crsp3d(kk,ll,nn)+wmpi(ij,nn)*abs( xmpi3d(ij,kk,im,ll)-xmpi3d(ij,kk,jm,ll) )/dble(2*nbv*(nbv-1))
        END DO       ; END DO
      ENDIF
    END DO      ; END DO
  END DO     ; END DO

  DO nn=1,ng ; DO ll=1,nv2d
    DO im=1,nbv ; DO ij=1,nij1    
          crsk2d(   ll,nn)=crsk2d(   ll,nn)+wmpi(ij,nn)*abs( xmpi2d(ij,   im,ll)-ympi2d(ij,      ll) )/dble(nbv)
    END DO      ; END DO
    DO im=1,nbv ; DO jm=1,nbv
      IF( im /= jm )THEN
        DO ij=1,nij1
          crsp2d(   ll,nn)=crsp2d(   ll,nn)+wmpi(ij,nn)*abs( xmpi2d(ij,   im,ll)-xmpi2d(ij   ,jm,ll) )/dble(2*nbv*(nbv-1))
        END DO
      ENDIF
    END DO      ; END DO      
  END DO     ; END DO
  
  CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)
  CALL MPI_REDUCE(crsk3d,crsk3o,nlev*nv3d*ng,MPI_DOUBLE_PRECISION,MPI_SUM,0,MPI_COMM_WORLD,ierr)
  CALL MPI_REDUCE(crsp3d,crsp3o,nlev*nv3d*ng,MPI_DOUBLE_PRECISION,MPI_SUM,0,MPI_COMM_WORLD,ierr)
  CALL MPI_REDUCE(crsk2d,crsk2o,     nv2d*ng,MPI_DOUBLE_PRECISION,MPI_SUM,0,MPI_COMM_WORLD,ierr)
  CALL MPI_REDUCE(crsp2d,crsp2o,     nv2d*ng,MPI_DOUBLE_PRECISION,MPI_SUM,0,MPI_COMM_WORLD,ierr)

  IF( myrank == 0 )THEN  
    WRITE(rmsename,'(2a,i10.10,a)') trim(valddir),"/",ymdh,"_rmse.txt"
    WRITE(sprdname,'(2a,i10.10,a)') trim(valddir),"/",ymdh,"_sprd.txt"
    WRITE(rmaxname,'(2a,i10.10,a)') trim(valddir),"/",ymdh,"_rmax.txt"
    WRITE(rminname,'(2a,i10.10,a)') trim(valddir),"/",ymdh,"_rmin.txt"
    OPEN(1,file=trim(rmsename),form="formatted",action="write")
    OPEN(2,file=trim(sprdname),form="formatted",action="write")
    OPEN(3,file=trim(rmaxname),form="formatted",action="write")
    OPEN(4,file=trim(rminname),form="formatted",action="write")
      DO nn=1,ng
        WRITE(1,'(I11,100F20.10)') ymdh, ((rmse3o(kk,ll,nn),kk=1,nlev),ll=1,nv3d),(rmse2o(ll,nn),ll=1,nv2d)
        WRITE(2,'(I11,100F20.10)') ymdh, ((sprd3o(kk,ll,nn),kk=1,nlev),ll=1,nv3d),(sprd2o(ll,nn),ll=1,nv2d)
        WRITE(3,'(I11,100F20.10)') ymdh, ((rmax3o(kk,ll,nn),kk=1,nlev),ll=1,nv3d),(rmax2o(ll,nn),ll=1,nv2d)
        WRITE(4,'(I11,100F20.10)') ymdh, ((rmin3o(kk,ll,nn),kk=1,nlev),ll=1,nv3d),(rmin2o(ll,nn),ll=1,nv2d)
      END DO
    CLOSE(1)
    CLOSE(2)
    CLOSE(3)
    CLOSE(4)

    WRITE(rmsename,'(2a,i10.10,a)') trim(valddir),"/",ymdh,"_crps-skill.txt"
    WRITE(sprdname,'(2a,i10.10,a)') trim(valddir),"/",ymdh,"_crps-spred.txt"
    WRITE(filename,'(2a,i10.10,a)') trim(valddir),"/",ymdh,"_crps-sk_sp.txt"
    OPEN(1,file=trim(rmsename),form="formatted",action="write")
    OPEN(2,file=trim(sprdname),form="formatted",action="write")
    OPEN(3,file=trim(filename),form="formatted",action="write")
      DO nn=1,ng
        WRITE(1,'(I11,100F20.10)') ymdh,  ((crsk3o(kk,ll,nn),kk=1,nlev),ll=1,nv3d),(crsk2o(ll,nn),ll=1,nv2d)
        WRITE(2,'(I11,100F20.10)') ymdh,  ((crsp3o(kk,ll,nn),kk=1,nlev),ll=1,nv3d),(crsp2o(ll,nn),ll=1,nv2d)
        WRITE(3,'(I11,100F20.10)') ymdh, (((crsk3o(kk,ll,nn)-crsp3o(kk,ll,nn)),kk=1,nlev),ll=1,nv3d), &
                                          ((crsk2o(   ll,nn)-crsp2o(   ll,nn)),ll=1,nv2d)
      END DO
    CLOSE(1)
    CLOSE(2)
    CLOSE(3)
  ENDIF
END SUBROUTINE calc_crps
!-----------------------------------------------------------------------!
SUBROUTINE read_ensdata
  IMPLICIT NONE
  INTEGER :: im_tmp
  REAL(4) :: xtmp3d(nlon, nlat, nlev, nv3d), xtmp2d(nlon, nlat, nv2d)
  
  !--- read model data ---!
  im_tmp = 1
  DO im = 1 + myrank, nbv, nprocs
    WRITE(filename,'(2a,i6.6,a,i10.10,a)') trim(basedir),"/",im,"/",ymdh,".grd"
    CALL read_grd4(filename, xtmp3d, xtmp2d)
    xens3d(:,:,:,:,im_tmp) = dble(xtmp3d(:,:,:,:))
    xens2d(:,:,:,  im_tmp) = dble(xtmp2d(:,:,:)  )
    im_tmp = im_tmp + 1
  END DO

  RETURN
END SUBROUTINE read_ensdata
!-----------------------------------------------------------------------!
SUBROUTINE read_trudata
  IMPLICIT NONE
  INTEGER :: im_tmp, n, im, l
  REAL(4) :: xtmp3d(nlon, nlat, nlev, nv3d), xtmp2d(nlon, nlat, nv2d)
  
  !--- read model data ---!
  WRITE(filename,'(2a,i10.10,a)') trim(truedir),"/",ymdh,".grd"
  CALL read_grd4(filename, xtmp3d, xtmp2d)
  xtru3d(:,:,:,:) = dble(xtmp3d(:,:,:,:))
  xtru2d(:,:,:  ) = dble(xtmp2d(:,:,:)  )

  DO n=0,nprocs-1
    im = n+1 + (l-1)*nprocs
    IF(im <= nbv) THEN
      CALL scatter_grd_mpi(n,xtmp3d,xtmp2d,ympi3d(:,:,:),ympi2d(:,:))
    END IF
  END DO

  RETURN
END SUBROUTINE read_trudata
!-----------------------------------------------------------------------!
SUBROUTINE read_mspdata
  IMPLICIT NONE
  INTEGER :: im_tmp, n, im, l
  REAL(4) :: xtmp3d(nlon, nlat, nlev, nv3d), xtmp2d(nlon, nlat, nv2d)
  
  !--- read mean data ---!
  WRITE(filename,'(2a,i10.10,a)') trim(basedir),"/mean/",ymdh,".grd"
  CALL read_grd4(filename, xtmp3d, xtmp2d)
  DO n=0,nprocs-1
    im = n+1 + (l-1)*nprocs
    IF(im <= nbv) THEN
      CALL scatter_grd_mpi(n,xtmp3d,xtmp2d,ampi3d(:,:,:),ampi2d(:,:))
    END IF
  END DO

  !--- read sprd data ---!
  WRITE(filename,'(2a,i10.10,a)') trim(basedir),"/sprd/",ymdh,".grd"
  CALL read_grd4(filename, xtmp3d, xtmp2d)
  DO n=0,nprocs-1
    im = n+1 + (l-1)*nprocs
    IF(im <= nbv) THEN
      CALL scatter_grd_mpi(n,xtmp3d,xtmp2d,smpi3d(:,:,:),smpi2d(:,:))
    END IF
  END DO

  !!!!--- read crps spread data ---!
  !!!WRITE(filename,'(2a,i10.10,a)') trim(basedir),"/crsp/",ymdh,".grd"
  !!!CALL read_grd4(filename, xtmp3d, xtmp2d)
  !!!DO n=0,nprocs-1
  !!!  im = n+1 + (l-1)*nprocs
  !!!  IF(im <= nbv) THEN
  !!!    CALL scatter_grd_mpi(n,xtmp3d,xtmp2d,cmpi3d(:,:,:),cmpi2d(:,:))
  !!!  END IF
  !!!END DO
  
  RETURN
END SUBROUTINE read_mspdata
!-----------------------------------------------------------------------!
SUBROUTINE calc_ensmean
  IMPLICIT NONE
  INTEGER :: count, datatype, op, comm, ierr

  xave3d(:,:,:,:) = 0.d0 ; xave2d(:,:,:) = 0.d0
  xsum3d(:,:,:,:) = 0.d0 ; xsum2d(:,:,:) = 0.d0
  DO im = 1, l_member
    xsum3d(:,:,:,:) = xsum3d(:,:,:,:) + xens3d(:,:,:,:,im)
    xsum2d(:,:,:)   = xsum2d(:,:,:)   + xens2d(:,:,:,  im)
  END DO

  count = nlon*nlat*nlev*nv3d
  CALL MPI_ALLREDUCE(xsum3d,xave3d,count,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
  count = nlon*nlat*nv2d
  CALL MPI_ALLREDUCE(xsum2d,xave2d,count,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)

  xave3d(:,:,:,:) = xave3d(:,:,:,:)/dble(nbv)
  xave2d(:,:,:)   = xave2d(:,:,:)  /dble(nbv)

  RETURN
END SUBROUTINE calc_ensmean
!-----------------------------------------------------------------------!
SUBROUTINE calc_enssprd
  IMPLICIT NONE
  INTEGER :: count, datatype, op, comm, ierr

  xsum3d(:,:,:,:) = 0.d0 ; xsum2d(:,:,:) = 0.d0
  xspr3d(:,:,:,:) = 0.d0 ; xspr2d(:,:,:) = 0.d0
  DO im = 1, l_member
    IF( sum(xens3d(:,:,1,1,im)) /= 0.d0 ) THEN
      xsum3d(:,:,:,:) = xsum3d(:,:,:,:) + (xens3d(:,:,:,:,im) - xave3d(:,:,:,:))**2
      xsum2d(:,:,:)   = xsum2d(:,:,:)   + (xens2d(:,:,:,  im) - xave2d(:,:,:)  )**2
    END IF
  END DO 

  count = nlon*nlat*nlev*nv3d
  CALL MPI_REDUCE(xsum3d,xspr3d,count,MPI_DOUBLE_PRECISION,MPI_SUM,0,MPI_COMM_WORLD,ierr)
  count = nlon*nlat*nv2d
  CALL MPI_REDUCE(xsum2d,xspr2d,count,MPI_DOUBLE_PRECISION,MPI_SUM,0,MPI_COMM_WORLD,ierr)

  IF( myrank == 0 ) THEN
    xspr3d(:,:,:,:) = dsqrt(xspr3d(:,:,:,:)/dble(nbv-1))
    xspr2d(:,:,:)   = dsqrt(xspr2d(:,:,:)  /dble(nbv-1))
  END IF

  RETURN
END SUBROUTINE calc_enssprd
!-----------------------------------------------------------------------!
SUBROUTINE output_data
  IMPLICIT NONE
  REAL(4) :: xtmp3d(nlon, nlat, nlev, nv3d),   xtmp2d(nlon, nlat, nv2d)
  REAL(4) :: xchk3d(nlon, nlat, nlev, nv3d),   xchk2d(nlon, nlat, nv2d)

  IF( myrank == 0 )THEN
    !--- write mixed analysis mean data (model) ---!
    WRITE(filename,'(2a,i10.10,a)') trim(basedir),"/mean/",ymdh,".grd"
    xtmp3d(:,:,:,:) = REAL(xave3d(:,:,:,:))
    xtmp2d(:,:,:)   = REAL(xave2d(:,:,:)  )
    CALL write_grd4(filename, xtmp3d, xtmp2d)

    !DEBUG!WRITE(filename,'(2a,i10.10,a)') trim(basedir),"/mean/",ymdh,".grd.old"
    !DEBUG!CALL read_grd4( filename, xchk3d, xchk2d)    
    !DEBUG!DO LL=1,nv3d ; DO KK=1,nlev ; DO JJ=1,nlat ; DO II=1,nlon
    !DEBUG!  WRITE(6,*), xchk3d(II,JJ,KK,LL), xtmp3d(II,JJ,KK,LL), xchk3d(II,JJ,KK,LL)-xtmp3d(II,JJ,KK,LL)
    !DEBUG!END DO       ; END DO       ; END DO       ; END DO 

    !--- write mixed analysis spread data (model) ---!
    WRITE(filename,'(2a,i10.10,a)') trim(basedir),"/sprd/",ymdh,".grd"
    xtmp3d(:,:,:,:) = REAL(xspr3d(:,:,:,:))
    xtmp2d(:,:,:)   = REAL(xspr2d(:,:,:)  )
    CALL write_grd4(filename, xtmp3d, xtmp2d)

    !DEBUG!WRITE(filename,'(2a,i10.10,a)') trim(basedir),"/sprd/",ymdh,".grd.old"
    !DEBUG!CALL read_grd4( filename, xchk3d, xchk2d)
    !DEBUG!DO LL=1,nv3d ; DO KK=1,nlev ; DO JJ=1,nlat ; DO II=1,nlon
    !DEBUG!  WRITE(6,*), xchk3d(II,JJ,KK,LL), xtmp3d(II,JJ,KK,LL), xchk3d(II,JJ,KK,LL)-xtmp3d(II,JJ,KK,LL)
    !DEBUG!END DO       ; END DO       ; END DO       ; END DO 

  END IF

  RETURN
END SUBROUTINE output_data
  !-----------------------------------------------------------------------!
END PROGRAM ensdiagnosis
