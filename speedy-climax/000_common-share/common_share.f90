MODULE common_share
!=======================================================================
!
! [PURPOSE:] Common Information for SPEEDY
!
! [HISTORY:]
!   10/15/2004 Takemasa Miyoshi  created
!   01/23/2009 Takemasa Miyoshi  modified
!   07/25/2024 Shunji   Kotsuki  updated for speedy/climax letkf
!
!=======================================================================
!$USE OMP_LIB
  USE common
  USE common_XXXXXX
  IMPLICIT NONE
  PUBLIC

  !===> letkf.cnf
  REAL(r_size),SAVE :: sigma_obs  = 9999.0d3
  INTEGER,     SAVE :: pymdh      = 0
  INTEGER,     SAVE :: ymdh       = 0
  LOGICAL,     SAVE :: logic_wout = .false.
  LOGICAL,     SAVE :: logic_wsth = .false.
  LOGICAL,     SAVE :: logic_wint = .false.
  INTEGER,     SAVE :: dastype    = 0
  REAL(r_size),SAVE :: cov_infl_mul = -1.00d0
  !REAL(r_size),SAVE :: s_additive = 0.0d0   defined in commmon_letkf.f90
  !REAL(r_size),SAVE :: hpo_weight = 0.0d0   defined in commmon_letkf.f90
  INTEGER,     SAVE :: resample_m = 0
  !INTEGER,     SAVE :: type_wnorm = 0       defined in common_lpf.f90
  !INTEGER,     SAVE :: type_pfmtx = 0       defined in common_lpf.f90
  !INTEGER,     SAVE :: type_relax = 0
  REAL(r_size),SAVE :: alph_relax = 0.0d0
  REAL(r_size),SAVE :: fgt_factor = 1.0d0
  !REAL(r_size),SAVE :: tau_nmlize = 0.0d0   defined in common_lpf.f90
  !REAL(r_size),SAVE :: snk_lambda = 40.0d0  defined in common_lpf.f90
  !REAL(r_size),SAVE :: gamma_gmpf = 1.5d0   defined in common_lpf.f90

CONTAINS
!-----------------------------------------------------------------------
! File I/O
!-----------------------------------------------------------------------
!-- Read a grid file ---------------------------------------------------
SUBROUTINE read_grd(filename,v3d,v2d)
  IMPLICIT NONE
  CHARACTER(*),INTENT(IN) :: filename
  REAL(r_size),INTENT(OUT) :: v3d(nlon,nlat,nlev,nv3d)
  REAL(r_size),INTENT(OUT) :: v2d(nlon,nlat,nv2d)
  REAL(r_sngl) :: buf4(nlon,nlat)
  INTEGER :: iunit,iolen
  INTEGER :: k,n,irec

  iunit=11
  INQUIRE(IOLENGTH=iolen) iolen
  OPEN(iunit,FILE=filename,FORM='unformatted',ACCESS='direct',RECL=nij0*iolen)

  irec=1
  DO n=1,nv3d
    DO k=1,nlev
      READ(iunit,REC=irec) buf4
      irec = irec + 1
      v3d(:,:,k,n) = REAL(buf4,r_size)
    END DO
  END DO

  DO n=1,nv2d
    READ(iunit,REC=irec) buf4
    irec = irec + 1
    v2d(:,:,n) = REAL(buf4,r_size)
  END DO

  CLOSE(iunit)

  RETURN
END SUBROUTINE read_grd
!-----------------------------------------------------------------------
SUBROUTINE read_grd4(filename,v3d,v2d)
  IMPLICIT NONE
  CHARACTER(*),INTENT(IN) :: filename
  REAL(r_sngl),INTENT(OUT) :: v3d(nlon,nlat,nlev,nv3d)
  REAL(r_sngl),INTENT(OUT) :: v2d(nlon,nlat,nv2d)
  INTEGER :: iunit,iolen
  INTEGER :: i,j,k,n,irec

  iunit=11
  INQUIRE(IOLENGTH=iolen) iolen
  OPEN(iunit,FILE=filename,FORM='unformatted',ACCESS='direct',RECL=nij0*iolen)

  irec=1
  DO n=1,nv3d
    DO k=1,nlev
      READ(iunit,REC=irec) ((v3d(i,j,k,n),i=1,nlon),j=1,nlat)
      irec = irec + 1
    END DO
  END DO

  DO n=1,nv2d
    READ(iunit,REC=irec) ((v2d(i,j,n),i=1,nlon),j=1,nlat)
    irec = irec + 1
  END DO

  CLOSE(iunit)

  RETURN
END SUBROUTINE read_grd4
!-----------------------------------------------------------------------
SUBROUTINE read_grd4_1atm(filename,v3d,v2d)
  IMPLICIT NONE
  CHARACTER(*),INTENT(IN) :: filename
  REAL(r_sngl),INTENT(OUT) :: v3d(nlon,nlat,nlev,nv3d)
  REAL(r_sngl),INTENT(OUT) :: v2d(nlon,nlat,nv2d)
  INTEGER :: iunit,iolen
  INTEGER :: i,j,k,n,irec

  iunit=11
  INQUIRE(IOLENGTH=iolen) iolen
  OPEN(iunit,FILE=filename,FORM='unformatted',ACCESS='direct',RECL=nij0*iolen)

  irec=1
!  DO n=1,nv3d
  DO n=1,1
    DO k=1,nlev
      READ(iunit,REC=irec) ((v3d(i,j,k,n),i=1,nlon),j=1,nlat)
      irec = irec + 1
    END DO
  END DO
  DO n=2,nv3d
    v3d(:,:,:,n) = v3d(:,:,:,1) 
  END DO

  DO n=1,nv2d
    !!READ(iunit,REC=irec) ((v2d(i,j,n),i=1,nlon),j=1,nlat)
    !!irec = irec + 1
    v2d(:,:,n) = v3d(:,:,1,1)
  END DO
  CLOSE(iunit)

  RETURN
END SUBROUTINE read_grd4_1atm
!-- Write a grid file -------------------------------------------------
SUBROUTINE write_grd(filename,v3d,v2d)
  IMPLICIT NONE
  CHARACTER(*),INTENT(IN) :: filename
  REAL(r_size),INTENT(IN) :: v3d(nlon,nlat,nlev,nv3d)
  REAL(r_size),INTENT(IN) :: v2d(nlon,nlat,nv2d)
  REAL(r_sngl) :: buf4(nlon,nlat)
  INTEGER :: iunit,iolen
  INTEGER :: k,n,irec

  iunit=55
  INQUIRE(IOLENGTH=iolen) iolen
  OPEN(iunit,FILE=filename,FORM='unformatted',ACCESS='direct',RECL=nij0*iolen)

  irec=1
  DO n=1,nv3d
    DO k=1,nlev
      buf4 = REAL(v3d(:,:,k,n),r_sngl)
      WRITE(iunit,REC=irec) buf4
      irec = irec + 1
    END DO
  END DO

  DO n=1,nv2d
    buf4 = REAL(v2d(:,:,n),r_sngl)
    WRITE(iunit,REC=irec) buf4
    irec = irec + 1
  END DO

  CLOSE(iunit)

  RETURN
END SUBROUTINE write_grd
!-----------------------------------------------------------------------
SUBROUTINE write_grd4(filename,v3d,v2d)
  IMPLICIT NONE
  CHARACTER(*),INTENT(IN) :: filename
  REAL(r_sngl),INTENT(IN) :: v3d(nlon,nlat,nlev,nv3d)
  REAL(r_sngl),INTENT(IN) :: v2d(nlon,nlat,nv2d)
  INTEGER :: iunit,iolen
  INTEGER :: i,j,k,n,irec

  iunit=55
  INQUIRE(IOLENGTH=iolen) iolen
  OPEN(iunit,FILE=filename,FORM='unformatted',ACCESS='direct',RECL=nij0*iolen)

  irec=1
  DO n=1,nv3d
    DO k=1,nlev
      WRITE(iunit,REC=irec) ((v3d(i,j,k,n),i=1,nlon),j=1,nlat)
      irec = irec + 1
    END DO
  END DO

  DO n=1,nv2d
    WRITE(iunit,REC=irec) ((v2d(i,j,n),i=1,nlon),j=1,nlat)
    irec = irec + 1
  END DO

  CLOSE(iunit)

  RETURN
END SUBROUTINE write_grd4
!-----------------------------------------------------------------------
!--> P.A. 20180606 for wvec
SUBROUTINE write_grd4_1atm(filename,v3d,v2d)
  IMPLICIT NONE
  CHARACTER(*),INTENT(IN) :: filename
  REAL(r_sngl),INTENT(IN) :: v3d(nlon,nlat,nlev,nv3d)
  REAL(r_sngl),INTENT(IN) :: v2d(nlon,nlat,nv2d)
  INTEGER :: iunit,iolen
  INTEGER :: i,j,k,n,irec

  iunit=55
  INQUIRE(IOLENGTH=iolen) iolen
  OPEN(iunit,FILE=filename,FORM='unformatted',ACCESS='direct',RECL=nij0*iolen)

  irec=1
  !!DO n=1,nv3d
  DO n=1,1
    DO k=1,nlev
      WRITE(iunit,REC=irec) ((v3d(i,j,k,n),i=1,nlon),j=1,nlat)
      irec = irec + 1
    END DO
  END DO

  !!DO n=1,nv2d
  !!  WRITE(iunit,REC=irec) ((v2d(i,j,n),i=1,nlon),j=1,nlat)
  !!  irec = irec + 1
  !!END DO

  CLOSE(iunit)

  RETURN
END SUBROUTINE write_grd4_1atm
!-----------------------------------------------------------------------
! Monitor
!-----------------------------------------------------------------------
SUBROUTINE monit_grd(v3d,v2d)
  IMPLICIT NONE
  REAL(r_size),INTENT(IN) :: v3d(nlon,nlat,nlev,nv3d)
  REAL(r_size),INTENT(IN) :: v2d(nlon,nlat,nv2d)
  INTEGER :: k,n

  DO k=1,nlev
    WRITE(6,'(I2,A)') k,'th level'
    DO n=1,nv3d
      WRITE(6,'(A,2ES10.2)') element(n),MAXVAL(v3d(:,:,k,n)),MINVAL(v3d(:,:,k,n))
    END DO
  END DO

  DO n=1,nv2d
    WRITE(6,'(A,2ES10.2)') element(nv3d+n),MAXVAL(v2d(:,:,n)),MINVAL(v2d(:,:,n))
  END DO

  RETURN
END SUBROUTINE monit_grd
!-----------------------------------------------------------------------
! Ensemble manipulations
!-----------------------------------------------------------------------
SUBROUTINE ensmean_grd(member,nij,v3d,v2d,v3dm,v2dm)
  IMPLICIT NONE
  INTEGER,INTENT(IN) :: member
  INTEGER,INTENT(IN) :: nij
  REAL(r_size),INTENT(IN) :: v3d(nij,nlev,member,nv3d)
  REAL(r_size),INTENT(IN) :: v2d(nij,member,nv2d)
  REAL(r_size),INTENT(OUT) :: v3dm(nij,nlev,nv3d)
  REAL(r_size),INTENT(OUT) :: v2dm(nij,nv2d)
  INTEGER :: i,k,m,n

  DO n=1,nv3d
!$OMP PARALLEL DO PRIVATE(i,k,m)
    DO k=1,nlev
      DO i=1,nij
        v3dm(i,k,n) = v3d(i,k,1,n)
        DO m=2,member
          v3dm(i,k,n) = v3dm(i,k,n) + v3d(i,k,m,n)
        END DO
        v3dm(i,k,n) = v3dm(i,k,n) / REAL(member,r_size)
      END DO
    END DO
!$OMP END PARALLEL DO
  END DO

  DO n=1,nv2d
    DO i=1,nij
      v2dm(i,n) = v2d(i,1,n)
      DO m=2,member
        v2dm(i,n) = v2dm(i,n) + v2d(i,m,n)
      END DO
      v2dm(i,n) = v2dm(i,n) / REAL(member,r_size)
    END DO
  END DO

  RETURN
END SUBROUTINE ensmean_grd
!-----------------------------------------------------------------------
SUBROUTINE enskldv_grd(member,nij,v3d,v2d,v3dk,v2dk)
  IMPLICIT NONE
  INTEGER,INTENT(IN) :: member
  INTEGER,INTENT(IN) :: nij
  REAL(r_size),INTENT(IN) :: v3d(nij,nlev,member,nv3d)
  REAL(r_size),INTENT(IN) :: v2d(nij,member,nv2d)
  REAL(r_size),INTENT(OUT) :: v3dk(nij,nlev,nv3d)
  REAL(r_size),INTENT(OUT) :: v2dk(nij,nv2d)
  INTEGER :: i,k,m,n

  DO n=1,nv3d
!$OMP PARALLEL DO PRIVATE(i,k,m)
    DO k=1,nlev
      DO i=1,nij
        call com_kld( member, v3d(i,k,1:member,n), v3dk(i,k,n) )
      END DO
    END DO
!$OMP END PARALLEL DO
  END DO

  DO n=1,nv2d
    DO i=1,nij
      call com_kld( member, v2d(i,1:member,n), v2dk(i,n) )
    END DO
  END DO

  RETURN
END SUBROUTINE enskldv_grd
!-----------------------------------------------------------------------
SUBROUTINE enscrsp_grd(member,nij,v3d,v2d,v3dk,v2dk)
  IMPLICIT NONE
  INTEGER,INTENT(IN) :: member
  INTEGER,INTENT(IN) :: nij
  REAL(r_size),INTENT(IN) :: v3d(nij,nlev,member,nv3d)
  REAL(r_size),INTENT(IN) :: v2d(nij,member,nv2d)
  REAL(r_size),INTENT(OUT) :: v3dk(nij,nlev,nv3d)
  REAL(r_size),INTENT(OUT) :: v2dk(nij,nv2d)
  INTEGER :: i,k,m,n

  DO n=1,nv3d
!$OMP PARALLEL DO PRIVATE(i,k,m)
    DO k=1,nlev
      DO i=1,nij
        call com_crsp( member, v3d(i,k,1:member,n), v3dk(i,k,n) )
      END DO
    END DO
!$OMP END PARALLEL DO
  END DO

  DO n=1,nv2d
    DO i=1,nij
      call com_crsp( member, v2d(i,1:member,n), v2dk(i,n) )
    END DO
  END DO

  RETURN
END SUBROUTINE enscrsp_grd
END MODULE common_share
