program main
  implicit none

  !===> setting of climax
  integer, parameter :: nlon  = 64
  integer, parameter :: nlat  = 32
  integer, parameter :: nlev  =  7
  integer, parameter :: nv3d  =  5
  integer, parameter :: nv2d  =  4
  integer, parameter :: nvar  =  (nlev*nv3d) + nv2d 
  integer, parameter :: nhour =  6
  integer, parameter :: qrecs = 22 ! start rec of Qv for kg/kg --> g/kg
  integer, parameter :: qrece = 28 ! end   rec of Qv for kg/kg --> g/kg
  integer, parameter :: grecs = 29 ! start rec of Geo for Geoporential Height [m]
  integer, parameter :: grece = 35 ! end   rec of Geo for Geoporential Height [m]
  integer, parameter :: precs = 39 ! start rec of Surface Pressure [Pa] --> [hPa]
  integer, parameter :: prece = 39 ! end   rec of Surface Pressure [Pa] --> [hPa]
  real(8), parameter :: dx    = 5.625d0
  real(8), parameter :: gg    = 9.81d0

  !===> setting of constant parameter
  real(4), parameter :: undef4 = -9.99d33
  real(8), parameter :: undef8 = -9.99d33
  real(8), parameter :: pi     =  3.1415926535d0
  real(8), parameter :: pi_180 = pi / 180.0d0
  REAL(8), PARAMETER :: lfactor     = 2.0d0*dsqrt(10.0d0/3.0d0)

  !===>speedy.cnf
  integer,        save :: adate, bdate, sdate, edate, pdate, qdate
  integer,        save :: nbv, nrunme
  character(500), save :: natdir, spndir, expdir, expname, obsname, cloc
  character(500), save :: outtime_me, outtime_sp, outtrun_me, outtrun_sp, outtave_me, outtave_sp

  !===> save variables
  integer, save :: nsamp, nrec, nmax

  !====> global variables
  integer :: idate, m, mm, i, j, ii, jj, k, kk, irec, icon, igrd, timer
  real(8) :: asamp, psamp, www, scale
  real(8) :: clon(nlon), clat(nlat)

  character(255) :: truname, xmename, xspname, pefname, outfile
  character(2)   :: CRGN
  character(4)   :: CVAR
  logical :: ex

  integer, parameter :: tmax = 99999
  real(4) :: tru(nlon,nlat,nvar) ! truth (weather bench)
  real(4) :: xme(nlon,nlat,nvar) ! ensemble mean
  real(4) :: xsp(nlon,nlat,nvar) ! ensemble spread
  real(4) :: xrmse(nvar), xrmse_a(nvar), xrmse_time(nvar,tmax), xrmse_trun(nvar)
  real(4) :: xsprd(nvar), xsprd_a(nvar), xsprd_time(nvar,tmax), xsprd_trun(nvar)

  namelist / climax_param / adate, bdate, sdate, edate, pdate, qdate, nbv, nrunme, natdir, spndir, expdir, &
                            outtime_me, outtime_sp, outtrun_me, outtrun_sp, outtave_me, outtave_sp, &
                            expname, obsname, cloc
  !================================================================================================== (1) PREPARATION
  open(1,file="climax.cnf") ; read(1,nml=climax_param) ; close(1)

  !=====> (1) preparation
  !print '(2a)', "  Calc... Standard  RMSE"
  do i=1,nlon  ;  clon(i) =    0.0d0 + dble(i-1)*dx ;  end do
  do i=1,nlat  ;  clat(i) = -87.1875 + dble(i-1)*dx ; END DO

  !==================================================================================================

  print '(3a)',">>>>>=====================   calc... RMSE&SPRD  ===================<<<<< ", CVAR
  !!write(outfile,'(6a)') trim(outdir),"/time_",trim(obsname),"_",trim(expname),".txt"
  !!open(1,file=trim(outfile),form="formatted",action="write")
  open(101,file=trim(outtime_me),form="formatted",action="write")
  open(102,file=trim(outtime_sp),form="formatted",action="write")
  open(103,file=trim(outtrun_me),form="formatted",action="write")
  open(104,file=trim(outtrun_sp),form="formatted",action="write")

  !=====> (2) get nsamp & RMSE
  idate = adate ! initial time
  nsamp = 0
  timer = 0
  xrmse_a(:)=0.0d0 ; xrmse_time(:,:)=undef8
  xsprd_a(:)=0.0d0 ; xsprd_time(:,:)=undef8
    do
    if(      idate<bdate        ) goto 20
    if( mod( idate,10000 )==100 ) &
      print '(5a,i10)',"      calc... RMSE&SPRD  ===>> ", trim(obsname)," ",trim(expdir)," :: ",idate

    timer = timer + 1
    write(truname,'(1a,i10.10,a)') trim(natdir),            idate,".grd"
    write(xmename,'(2a,i10.10,a)') trim(expdir),"/mean/",   idate,".grd"
    write(xspname,'(2a,i10.10,a)') trim(expdir),"/sprd/",   idate,".grd"
    write(pefname,'(2a,i10.10,a)') trim(expdir),"/../peff_lpf/",    idate,".grd"

    open(10,file=trim(truname),form="unformatted",access="direct",recl=nlon*nlat*nvar*4,action="read")
      read(10,rec=1) tru(:,:,:) ; close(10)
      tru(:,:,qrecs:qrece)=tru(:,:,qrecs:qrece)*1000.0d0 ! [kg/kg] --> [g/kg]
      tru(:,:,grecs:grece)=tru(:,:,grecs:grece)/gg       ! geopotential --> geopotential height [m]
      tru(:,:,precs:prece)=tru(:,:,precs:prece)/100.0d0  ! [Pa]    --> [hPa]

    inquire(FILE=trim(xmename),EXIST=ex)
    if( ex .or. idate<=(sdate+700) ) then ! at least one week
      open(11,file=trim(xmename),form="unformatted",access="direct",recl=nlon*nlat*nvar*4,action="read")
      open(12,file=trim(xspname),form="unformatted",access="direct",recl=nlon*nlat*nvar*4,action="read")
        read(11,rec=1) xme(:,:,:) ; close(11)
        read(12,rec=1) xsp(:,:,:) ; close(12)
        xme(:,:,qrecs:qrece)=xme(:,:,qrecs:qrece)*1000.0d0 ! [kg/kg] --> [g/kg]
        xsp(:,:,qrecs:qrece)=xsp(:,:,qrecs:qrece)*1000.0d0 ! [kg/kg] --> [g/kg]
        xme(:,:,grecs:grece)=xme(:,:,grecs:grece)/gg       ! geopotential --> geopotential height [m]
        xsp(:,:,grecs:grece)=xsp(:,:,grecs:grece)/gg       ! geopotential --> geopotential height [m]
        xme(:,:,precs:prece)=xme(:,:,precs:prece)/100.0d0  ! [Pa]    --> [hPa]
        xsp(:,:,precs:prece)=xsp(:,:,precs:prece)/100.0d0  ! [Pa]    --> [hPa]
    else
      xme(:,:,:)=undef4
      xsp(:,:,:)=undef4
      print '(2a)', " maybe because of divergence, cannot find file :: ", trim(xmename)
      stop
    endif

!LPF!    if( idate >= sdate ) then
!LPF!      inquire(FILE=trim(pefname),EXIST=ex)
!LPF!      if( ex )then
!LPF!        open(20,file=trim(pefname),form="unformatted",access="direct",recl=nlon*nlat*4,action="read")
!LPF!          read(20,rec=k) pef(:,:)
!LPF!        close(20)
!LPF!      else
!LPF!        pef(:,:) = undef4
!LPF!      end if
!LPF!    else
!LPF!      pef(:,:) = dble(nbv)
!LPF!    end if

    !===> for all time (global-mean rmse) 
    call calc_rmsesprd (nlon,nlat,nvar,clat,pi_180,tru,xme,xsp,xrmse,xsprd)
    !!write(6,'(a6,2i5,21F8.3,7f13.9,7f12.3,3f8.3,f12.3)') trim(cloc), nbv, idate, xrmse(:)

    xrmse_time(:,timer) = xrmse(:)
    xsprd_time(:,timer) = xsprd(:)

    xrmse_trun(:) = undef8
    xsprd_trun(:) = undef8
    do k=1,nvar
      if( timer >=nrunme ) xrmse_trun(k) = sum( xrmse_time(k,timer-nrunme+1:timer) ) / dble( nrunme )
      if( timer >=nrunme ) xsprd_trun(k) = sum( xsprd_time(k,timer-nrunme+1:timer) ) / dble( nrunme ) 
    end do

      write(101,'(2i12,f9.3,21F7.3,7f8.4,7f8.3,3f7.3,f10.3)') idate, timer, real(timer)/4.0d0, xrmse_time(:,timer)
      write(102,'(2i12,f9.3,21F7.3,7f8.4,7f8.3,3f7.3,f10.3)') idate, timer, real(timer)/4.0d0, xsprd_time(:,timer)
    if( timer >=nrunme )then
      write(103,'(2i12,f9.3,21F7.3,7f8.4,7f8.3,3f7.3,f10.3)') idate, timer, real(timer)/4.0d0, xrmse_trun(:)
      write(104,'(2i12,f9.3,21F7.3,7f8.4,7f8.3,3f7.3,f10.3)') idate, timer, real(timer)/4.0d0, xsprd_trun(:)
    end if

!TMP!
!TMP!    if( timer>= nrunme ) &
!TMP!    write(1,'(2i12,f9.3,4f8.4,4f8.4)')                                     &
!TMP!                                idate, timer, real(timer)/4.0d0,           & 
!TMP!                                ermse*scale, esprd*scale,                  &
!TMP!                                ermse_tave*scale, esprd_tave*scale,        &
!TMP!                                epeff, epeff_tave, ekldv, ekldv_tave
!TMP!    !write(6,'(2i12,f9.3,4f8.4,2f8.2)')                                     &
!TMP!    !                            idate, timer, real(timer)/4.0d0,           & 
!TMP!    !                            ermse, esprd, ermse_tave, esprd_tave, epeff, epeff_tave
!TMP!
!TMP!    !===> for time-mean rmse
    if( pdate<=idate .and. idate<=qdate )then
      nsamp = nsamp + 1
      xrmse_a(:)=xrmse_a(:)+xrmse(:)
      xsprd_a(:)=xsprd_a(:)+xsprd(:)
    endif

20  continue
    call update_date(idate,nhour)
    if ( nsamp .ge. 10000 ) then
      write(6,*) 'nsamp is too large'
      stop
    end if
    if( idate > edate  ) goto 10
  end do
10 continue
  close(101) ; close(102) ; close(103) ; close(104)

  xrmse_a(:)=xrmse_a(:)/dble(nsamp)
  xsprd_a(:)=xsprd_a(:)/dble(nsamp)
!TMP!  if( .not. (0.0d0<abs(erm_a) .and. abs(erm_a)<999.9d0) ) erm_a = 999.9d0  
!TMP!  if( .not. (0.0d0<abs(esp_a) .and. abs(esp_a)<999.9d0) ) esp_a = 999.9d0
  open(1,file=trim(outtave_me),form="formatted",action="write")
  open(2,file=trim(outtave_sp),form="formatted",action="write")
    write(1,'(a6,i3,i5,21F7.3,7f8.4,7f8.3,3f7.3,f10.3)') trim(cloc), nbv, nsamp, xrmse_a(:) ; close(1)
    write(2,'(a6,i3,i5,21F7.3,7f8.4,7f8.3,3f7.3,f10.3)') trim(cloc), nbv, nsamp, xsprd_a(:) ; close(2)
    !write(6,'(a6,i3,i5,21F7.3,7f8.4,7f8.3,3f7.3,f10.3)') trim(cloc), nbv, nsamp, xrmse_a(:)
  print '(2a)',"      -->", trim(outtave_me)
  print '(2a)',"      -->", trim(outtave_sp)
1111 continue

end
!=========================================================
subroutine calc_dist_deg(xlon1,xlon2,xlat1,xlat2,dist)
!=========================================================
  implicit none
  double precision xlon1,xlon2,xlat1,xlat2
  double precision lon1,lon2,lat1,lat2,cosd,dist

  REAL(8),PARAMETER :: pi=3.1415926535d0
  REAL(8),PARAMETER :: re=6371.3d3
  REAL(8),PARAMETER :: r180=1.0d0/180.0d0

  lon1 = xlon1 * pi * r180 ! [rad]
  lon2 = xlon2 * pi * r180 ! [rad]
  lat1 = xlat1 * pi * r180 ! [rad]
  lat2 = xlat2 * pi * r180 ! [rad]

  cosd = SIN(lat1)*SIN(lat2) + COS(lat1)*COS(lat2)*COS(lon2-lon1)
  cosd = MIN( 1.d0,cosd)
  cosd = MAX(-1.d0,cosd)
  dist = ACOS( cosd ) * re

end
!=========================================================
subroutine calc_rmsesprd(nlon,nlat,nvar,clat,pi_180,tru,xme,xsp,xrmse,xsprd)
!=========================================================
  implicit none
  integer, intent(in)  :: nlon, nlat, nvar
  real(8), intent(in)  :: clat(nlat), pi_180
  real(4), intent(in)  :: tru(nlon,nlat,nvar), xme(nlon,nlat,nvar), xsp(nlon,nlat,nvar)
  real(4), intent(out) :: xrmse(nvar), xsprd(nvar) 

  integer :: i, j, k
  real(8) :: www, asamp

  asamp = 0.0d0
  xrmse(:)=0.0d0 ; xsprd(:)=0.0d0 
  do j=1,nlat
    www = dcos( clat(j)*pi_180 )
    do i=1,nlon
      asamp = asamp + www
      do k = 1,nvar
        xrmse(k) = xrmse(k) + www * ( xme(i,j,k) - tru(i,j,k) )**2.0d0 
        xsprd(k) = xsprd(k) + www * ( xsp(i,j,k)              )**2.0d0
      end do
    end do
  end do
  xrmse(:)=sqrt(xrmse(:)/asamp)
  xsprd(:)=sqrt(xsprd(:)/asamp)

end subroutine calc_rmsesprd
!=========================================================
subroutine update_date(date,nhour)
!=========================================================
  implicit none
  integer date,nhour,date1
  integer year,mon,day,hour,mondays

  date1 = date
  year = int( date / 1000000 )  ;  date = date - year * 1000000
  mon  = int( date / 10000 )    ;  date = date - mon  * 10000
  day  = int( date / 100 )      ;  hour = date - day * 100

  hour = hour + nhour

10 continue
  if( hour.ge.24 ) then
    hour = hour - 24
    day  = day + 1
    call get_mondays(year,mon,mondays)
    if ( day.gt.mondays ) then
      day = 1
      mon = mon + 1
      if( mon.ge.13 ) then
        mon = 1
        year = year + 1
      end if ! mon
    end if ! day
  end if ! hour
  if( hour.ge.24 ) goto 10

  date = year*1000000 + mon*10000 + day*100 + hour
end subroutine update_date

!=========================================================
subroutine get_mondays(year,mon,mondays)
!=========================================================
  implicit none
  integer year,mon,mondays

  mondays=31
  if( mon ==  4 ) mondays = 30
  if( mon ==  6 ) mondays = 30
  if( mon ==  9 ) mondays = 30
  if( mon == 11 ) mondays = 30
  if( mon ==  2 ) then
    mondays = 28
    if( mod(year,4)==0 ) mondays = 29
  end if
end subroutine get_mondays
