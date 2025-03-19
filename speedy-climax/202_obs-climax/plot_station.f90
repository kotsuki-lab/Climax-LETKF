program plot_station
  implicit none

  integer, parameter :: nlon = 64, nlat = 32
  real(4) :: temp = 10
  real(4) :: lon(nlon)
  real(4) :: lat(nlat)
  real(4) :: pi
  character(128) :: stid
  integer :: nflag, nlev
  real(4) :: tim
  integer :: i, j, nstation, cdummy
  integer :: ios
  integer :: ix, iy

  pi = 4.0*atan(1.0)
  do i = 1, nlon
    lon(i) = 0.0 + 5.625*(i-1)
  end do
  DO i=1,nlat
    lat(i) = -87.1875 + 5.625*real(i-1)
  END DO

  open(20, file='markplot_gridpnt.gs')
  DO j=1,nlat ; DO i=1,nlon
    write(20, *) "'./gsfile/markplot",lon(i),lat(j),"-c 1 -m 3 -s 0.02'"
  END DO      ; END DO


  open(10, file='station.tbl.tmp')
  open(11, file='datafile.dat', form='unformatted', access='stream')
  open(12, file='obs_map.txt')
  open(20, file='markplot_station.gs')
  open(21, file='markplot_statsml.gs')

  nstation = 0
  read(10, '(A)') cdummy
  read(10, '(A)') cdummy
  do
    read(10, '(2I3)', iostat=ios) i, j
    if(ios /= 0) exit
    nstation = nstation + 1

    tim = 0.0
    nlev = 1
    nflag = 1
    write(stid, *) nstation
    write(11) trim(stid), j, i, tim, nlev, nflag
    write(11) temp
!   print *, i, j, nstation, trim(stid)
    write(12, *) lon(i), lat(j), "1"
    print *, nstation, i, j, lon(i), lat(j)
    write(20, *) "'./gsfile/markplot",lon(i),lat(j),"-c 2  -m 6 -s 0.12'"
    write(21, *) "'./gsfile/markplot",lon(i),lat(j),"-c 1  -m 6 -s 0.08'"
  end do

  nlev = 0
  write(11) trim(stid), j, i, tim, nlev, nflag

  close(10)
  close(11)
  close(12)
  close(20)
  close(21)


  stop
end program plot_station
