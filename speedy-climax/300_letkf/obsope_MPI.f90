PROGRAM obsope
!=======================================================================
!
! [PURPOSE:] Main program of observation operator
!
! [HISTORY:]
!   04/03/2013 Takemasa Miyoshi  created
!   06/30/2024 Shunji   Kotsuki  modified for speedy/climax
!
!=======================================================================
  USE common
  USE common_mpi
  USE common_letkf
  USE common_lpf
  USE common_XXXXXX
  USE common_share
  USE common_obs_XXXXXX

  IMPLICIT NONE
! CHARACTER(9) :: obsinfile='obsin.dat'                  !IN
  CHARACTER(18) :: obsinfile='obs/yyyymmddhh.dat'        !IN
  CHARACTER(26) :: guesfile='gues/000000/yyyymmddhh.grd' !IN
  CHARACTER(15) :: obsoutfile='obs01000000.dat'          !OUT
  CHARACTER(15) :: cnffile='letkf.cnf000000'             !OUT
  REAL(r_size),ALLOCATABLE :: elem(:)
  REAL(r_size),ALLOCATABLE :: otyp(:)
  REAL(r_size),ALLOCATABLE :: rlon(:)
  REAL(r_size),ALLOCATABLE :: rlat(:)
  REAL(r_size),ALLOCATABLE :: rlev(:)
  REAL(r_size),ALLOCATABLE :: odat(:)
  REAL(r_size),ALLOCATABLE :: oerr(:)
  REAL(r_size),ALLOCATABLE :: ohx(:)
  INTEGER,ALLOCATABLE :: oqc(:)
  REAL(r_size),ALLOCATABLE :: v3d(:,:,:,:)
  REAL(r_size),ALLOCATABLE :: v2d(:,:,:)
  REAL(r_size),ALLOCATABLE :: p_full(:,:,:)
  REAL(r_size),PARAMETER :: threshold_dz=1000.0d0
  REAL(r_size) :: dz,tg,qg
  INTEGER :: nobs
  REAL(r_size) :: ri,rj,rk
  INTEGER :: n
! INTEGER :: nbv                                          ! KK
  character(4) :: yyyy                                    ! KK
  character(2) :: mm, dd, hh                              ! KK
  INTEGER :: i, ista, iend                                ! KK
  INTEGER :: ierr

  CALL initialize_mpi
  CALL set_common_XXXXXX
  if(myrank == 0) print '(A6, I6, A9, I10)', 'hello from obsope,  nbv = ', nbv, ', YMDH = ', ymdh

  namelist / letkf_param / sigma_obs, nbv, pymdh, ymdh,                    &
    logic_wout, logic_wsth, logic_wint, dastype, cov_infl_mul, s_additive, hpo_weight,   &
    resample_m, type_wnorm, type_pfmtx, type_relax, alph_relax, fgt_factor, tau_nmlize, snk_lambda, gamma_gmpf
!-----------------------------------------------------------------------
  open(1,file='letkf.cnf')
    read(1,nml=letkf_param)
  close(1)

! if(myrank == 0) then
!   namelist / MEMBER_INFO / &
!     nbv,  &
!     yyyy, &
!     mm,   &
!     dd,   &
!     hh
!   open(10, file='./MEMINFO', form='formatted', status='old')
!   read(10, nml=MEMBER_INFO)
!   close(10)
! end if
! CALL MPI_BCAST(nbv, 1, MPI_INTEGER,   0, MPI_COMM_WORLD, ierr)
! CALL MPI_BCAST(yyyy, 4, MPI_CHARACTER, 0, MPI_COMM_WORLD, ierr)
! CALL MPI_BCAST(mm,   2, MPI_CHARACTER, 0, MPI_COMM_WORLD, ierr)
! CALL MPI_BCAST(dd,   2, MPI_CHARACTER, 0, MPI_COMM_WORLD, ierr)
! CALL MPI_BCAST(hh,   2, MPI_CHARACTER, 0, MPI_COMM_WORLD, ierr)

  write(obsinfile(5:14), '(I10.10)') ymdh
  IF( myrank == 0) CALL get_nobs(obsinfile,7,nobs) 
    ! (1) elm, (2) lon, (3) lat, (4) pre, (5) obs, (6) err, (7) typ
  CALL MPI_BCAST(nobs, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

  ALLOCATE( elem(nobs) )
  ALLOCATE( otyp(nobs) )
  ALLOCATE( rlon(nobs) )
  ALLOCATE( rlat(nobs) )
  ALLOCATE( rlev(nobs) )
  ALLOCATE( odat(nobs) )
  ALLOCATE( oerr(nobs) )
  ALLOCATE( ohx(nobs) )
  ALLOCATE( oqc(nobs) )

  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

  CALL read_obs(obsinfile,nobs,elem,rlon,rlat,rlev,odat,oerr,otyp)
! IF( myrank == 0 ) CALL read_obs(obsinfile,nobs,elem,rlon,rlat,rlev,odat,oerr)
! CALL MPI_BCAST(elem, nobs, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)
! CALL MPI_BCAST(rlon, nobs, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)
! CALL MPI_BCAST(rlat, nobs, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)
! CALL MPI_BCAST(rlev, nobs, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)
! CALL MPI_BCAST(odat, nobs, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)
! CALL MPI_BCAST(oerr, nobs, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)
! CALL MPI_BCAST(ohx , nobs, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)
! CALL MPI_BCAST(oqc , nobs, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
! print *, myrank, elem(obs-1), elem(10)
! print *, myrank, rlon(nobs-1), rlon(10)
! print *, myrank, rlat(nobs-1), rlat(10)
! print *, myrank, rlev(nobs-1), rlev(10)
! print *, myrank, odat(nobs-1), odat(10)
! print *, myrank, oerr(nobs-1), oerr(10)
! print *, myrank, ohx(nobs-1), ohx(10)
! print *, myrank, oqc(nobs-1), oqc(10)
  ALLOCATE( v3d(nlon,nlat,nlev,nv3d) )
  ALLOCATE( v2d(nlon,nlat,nv2d) )
  ALLOCATE( p_full(nlon,nlat,nlev) )

  ista = myrank+1                                                           ! KK
  iend = nbv                                                                ! KK
  do i = ista, iend, nprocs                                                 ! KK

!   write(guesfile(6:11), '(I6.6)') myrank+nbv                              ! KK
    write(guesfile(6:11), '(I6.6)') i                                       ! KK
    write(guesfile(13:22), '(I10.10)') ymdh                                 ! KK
    CALL read_grd(guesfile,v3d,v2d)

    p_full(:,:,:) = 0.d0                                                    ! KK
    CALL calc_pfull(nlon,nlat,v2d(:,:,iv2d_ps),p_full)


    ohx=0.0d0
    oqc=0
    DO n=1,nobs
      CALL phys2ijk(p_full,elem(n),rlon(n),rlat(n),rlev(n),ri,rj,rk)
      IF(CEILING(ri) < 2 .OR. nlon+1 < CEILING(ri)) THEN
 !      WRITE(6,'(A)') '* X-coordinate out of range'
 !      WRITE(6,'(A,F6.2,A,F6.2)') '*   ri=',ri,', rlon=',rlon(n)
        CYCLE
      END IF
      IF(CEILING(rj) < 2 .OR. nlat < CEILING(rj)) THEN
 !      WRITE(6,'(A)') '* Y-coordinate out of range'
 !      WRITE(6,'(A,F6.2,A,F6.2)') '*   rj=',rj,', rlat=',rlat(n)
        CYCLE
      END IF
      IF(CEILING(rk) > nlev) THEN
        CALL itpl_2d(phi0,ri,rj,dz)
 !      WRITE(6,'(A)') '* Z-coordinate out of range'
 !      WRITE(6,'(A,F6.2,A,F10.2,A,F6.2,A,F6.2,A,F10.2)') &
 !       & '*   rk=',rk,', rlev=',rlev(n),&
 !       & ', (lon,lat)=(',rlon(n),',',rlat(n),'), phi0=',dz
        CYCLE
      END IF
      IF(CEILING(rk) < 2 .AND. NINT(elem(n)) /= id_ps_obs) THEN
        IF(NINT(elem(n)) == id_u_obs .OR. NINT(elem(n)) == id_v_obs) THEN
          rk = 1.00001d0
        ELSE
          CALL itpl_2d(phi0,ri,rj,dz)
 !        WRITE(6,'(A)') '* Z-coordinate out of range'
 !        WRITE(6,'(A,F6.2,A,F10.2,A,F6.2,A,F6.2,A,F10.2)') &
 !         & '*   rk=',rk,', rlev=',rlev(n),&
 !         & ', (lon,lat)=(',rlon(n),',',rlat(n),'), phi0=',dz
          CYCLE
        END IF
      END IF
      IF(NINT(elem(n)) == id_ps_obs .AND. odat(n) < -100.0d0) THEN
        CYCLE
      END IF
!     IF(NINT(tmpelm(nn+n)) == id_ps_obs) THEN
!       CALL itpl_2d(phi0,ri,rj,dz)
!       dz = dz - tmplev(nn+n)
!       IF(ABS(dz) < threshold_dz) THEN ! pressure adjustment threshold
!         CALL itpl_2d(t(:,:,1),ri,rj,tg)
!         CALL itpl_2d(q(:,:,1),ri,rj,qg)
!         CALL prsadj(tmpdat(nn+n),dz,tg,qg)
!       ELSE
!         PRINT '(A)','PS obs vertical adjustment beyond threshold'
!         PRINT '(A,F10.2,A,F6.2,A,F6.2,A)',&
!           & '  dz=',dz,', (lon,lat)=(',tmplon(nn+n),',',tmplat(nn+n),')'
!         CYCLE
!       END IF
!     END IF
      !
      ! observational operator
      !
      CALL Trans_XtoY(elem(n),ri,rj,rk,v3d,v2d,p_full,ohx(n))
      oqc(n) = 1
    END DO

!   write(obsoutfile(6:11), '(I6.6)') myrank+nbv                            ! KK
    write(obsoutfile(6:11), '(I6.6)') i                                     ! KK
    CALL write_obs2(obsoutfile,nobs,elem,rlon,rlat,rlev,odat,oerr,ohx,oqc,otyp)
    ! (1) elm, (2) lon, (3) lat, (4) pre, (5) obs, (6) err, (7) h(x), (8) qc, (9) typ
  end do                                                                    ! KK

  DEALLOCATE( elem,otyp,rlon,rlat,rlev,odat,oerr,ohx,oqc,v3d,v2d,p_full )

  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

!=== make information file ===!
  ! fort.2 for CLIMAX model
! open(9, file='fort.2', form='formatted')
! write(9, *) "1111"
! write(9, *) yyyy
! write(9, *) mm
! write(9, *) dd
! write(9, *) hh
! close(9)

  ! ymhd.txt
! open(11, file='ymdh.txt', form='formatted')
! write(11, '(A10)') tymdh
! close(11)

  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  CALL finalize_mpi

  stop

END PROGRAM obsope
