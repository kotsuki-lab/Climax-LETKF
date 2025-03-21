MODULE common_XXXXXX
!=======================================================================
!
! [PURPOSE:] Common Information for SPEEDY
!
! [HISTORY:]
!   10/15/2004 Takemasa Miyoshi  created
!   01/23/2009 Takemasa Miyoshi  modified
!   07/25/2024 Shunji   Kotsuki  updated for speedy/climax letf
!
!=======================================================================
!$USE OMP_LIB
  USE common
  IMPLICIT NONE
  PUBLIC
!-----------------------------------------------------------------------
! General parameters
!-----------------------------------------------------------------------
  INTEGER,PARAMETER :: nlon=96
  INTEGER,PARAMETER :: nlat=nlon/2
  INTEGER,PARAMETER :: nlev=7
  INTEGER,PARAMETER :: nv3d=4 ! u,v,t,q
  INTEGER,PARAMETER :: nv2d=2 ! ps,rain
  INTEGER,PARAMETER :: iv3d_u=1
  INTEGER,PARAMETER :: iv3d_v=2
  INTEGER,PARAMETER :: iv3d_t=3
  INTEGER,PARAMETER :: iv3d_q=4
  INTEGER,PARAMETER :: iv2d_ps=1
  INTEGER,PARAMETER :: iv2d_rain=2
  INTEGER,PARAMETER :: nij0=nlon*nlat
  INTEGER,PARAMETER :: nlevall=nlev*nv3d+nv2d
  INTEGER,PARAMETER :: ngpv=nij0*nlevall
  REAL(r_size),SAVE :: lon(nlon)
  REAL(r_size),SAVE :: lat(nlat)
  REAL(r_size),SAVE :: sig(nlev)
  REAL(r_size),SAVE :: dx(nlat)
  REAL(r_size),SAVE :: dy(nlat)
  REAL(r_size),SAVE :: dy2(nlat)
  REAL(r_size),SAVE :: fcori(nlat)
  REAL(r_size),SAVE :: phi0(nlon,nlat)
  CHARACTER(4),SAVE :: element(nv3d+nv2d)

CONTAINS
!-----------------------------------------------------------------------
! Set the parameters
!-----------------------------------------------------------------------
!SUBROUTINE set_common_speedy
SUBROUTINE set_common_XXXXXX
  IMPLICIT NONE
  INTEGER :: i,j

  WRITE(6,'(A)') 'Hello from set_common_XXXXXX(speedy)'
  !
  ! Elements
  !
  element(iv3d_u) = 'U   '
  element(iv3d_v) = 'V   '
  element(iv3d_t) = 'T   '
  element(iv3d_q) = 'Q   '
  element(nv3d+iv2d_ps)   = 'PS  '
  element(nv3d+iv2d_rain) = 'RAIN'
  !
  ! Lon, Lat, Sigma
  !
!$OMP PARALLEL DO PRIVATE(i)
  DO i=1,nlon
    lon(i) = 360.d0/nlon*(i-1)
  END DO
!$OMP END PARALLEL DO
  lat(1) = -87.159d0
  lat(2) = -83.479d0
  lat(3) = -79.777d0
  lat(4) = -76.070d0
  lat(5) = -72.362d0
  lat(6) = -68.652d0
  lat(7) = -64.942d0
  lat(8) = -61.232d0
  lat(9) = -57.521d0
  lat(10)= -53.810d0
  lat(11)= -50.099d0
  lat(12)= -46.389d0
  lat(13)= -42.678d0
  lat(14)= -38.967d0
  lat(15)= -35.256d0
  lat(16)= -31.545d0
  lat(17)= -27.833d0
  lat(18)= -24.122d0
  lat(19)= -20.411d0
  lat(20)= -16.700d0
  lat(21)= -12.989d0
  lat(22)=  -9.278d0
  lat(23)=  -5.567d0
  lat(24)=  -1.856d0
  lat(25)=   1.856d0
  lat(26)=   5.567d0
  lat(27)=   9.278d0
  lat(28)=  12.989d0
  lat(29)=  16.700d0
  lat(30)=  20.411d0
  lat(31)=  24.122d0
  lat(32)=  27.833d0
  lat(33)=  31.545d0
  lat(34)=  35.256d0
  lat(35)=  38.967d0
  lat(36)=  42.678d0
  lat(37)=  46.389d0
  lat(38)=  50.099d0
  lat(39)=  53.810d0
  lat(40)=  57.521d0
  lat(41)=  61.232d0
  lat(42)=  64.942d0
  lat(43)=  68.652d0
  lat(44)=  72.362d0
  lat(45)=  76.070d0
  lat(46)=  79.777d0
  lat(47)=  83.479d0
  lat(48)=  87.159d0

  sig(1) = .95d0
  sig(2) = .835d0
  sig(3) = .685d0
  sig(4) = .51d0
  sig(5) = .34d0
  sig(6) = .2d0
  sig(7) = .08d0
  !
  ! dx and dy
  !
!$OMP PARALLEL
!$OMP WORKSHARE
  dx(:) = 2.0d0 * pi * re * cos(lat(:) * pi / 180.0d0) / REAL(nlon,r_size)
!$OMP END WORKSHARE

!$OMP DO
  DO i=1,nlat-1
    dy(i) = 2.0d0 * pi * re * (lat(i+1) - lat(i)) / 360.0d0
  END DO
!$OMP END DO
!$OMP END PARALLEL
  dy(nlat) = 2.0d0 * pi * re * (90.0d0 - lat(nlat)) / 180.0d0

!$OMP PARALLEL DO
  DO i=2,nlat
    dy2(i) = (dy(i-1) + dy(i)) * 0.5d0
  END DO
!$OMP END PARALLEL DO
  dy2(1) = (dy(nlat) + dy(1)) * 0.5d0
  !
  ! Corioris parameter
  !
!$OMP PARALLEL WORKSHARE
  fcori(:) = 2.0d0 * r_omega * sin(lat(:)*pi/180.0d0)
!$OMP END PARALLEL WORKSHARE
  !
  ! Surface geoptential (Read Orography file)
  !
  !JSS2,BUG???,20190417SK!READ(21) phi0
  !!open(31,file="./orography_t30.dat",form="unformatted",access="sequential",status="old")
  !!  read(31) phi0(:,:)
  !!close(31)
  open(31, file="./orography_t30.dat", form="unformatted")  ! KK
    READ(31) phi0
  close(31)

  RETURN
!END SUBROUTINE set_common_speedy
END SUBROUTINE set_common_XXXXXX
!-----------------------------------------------------------------------
! p_full
!-----------------------------------------------------------------------
SUBROUTINE calc_pfull(ix,jy,ps,p_full)
  IMPLICIT NONE
  INTEGER,INTENT(IN) :: ix,jy
  REAL(r_size),INTENT(IN) :: ps(ix,jy)
  REAL(r_size),INTENT(OUT) :: p_full(ix,jy,nlev)
  INTEGER :: i,j,k

!$OMP PARALLEL DO PRIVATE(i,j,k)
  DO k=1,nlev
    DO j=1,jy
      DO i=1,ix
        p_full(i,j,k) = ps(i,j) * sig(k)
      END DO
    END DO
  END DO
!$OMP END PARALLEL DO

  RETURN
END SUBROUTINE calc_pfull
END MODULE common_XXXXXX
