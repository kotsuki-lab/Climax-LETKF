#/bin/bash
# singularity run --bind /data02,/data10 /export/home/imageshare/oneapi-hpckit_latest.sif

for OBSNET in "raob" ; do
  ln -s station_${OBSNET}_climax.tbl station.tbl.tmp

  #==> txt make
  ifort plot_station.f90 -o plot_station.exe -assume byterecl #-convert big_endian
  ./plot_station.exe

#  #=== GMT ===#
#  psout="./obs_map.ps"
#  pngout="./obs_map.png"
#  pscoast -R0/360/-90/90 -Jq180/0.028 -Ba60g30/a30g30WSne -A7000 -W1 -Di -X0.7 -K  > $psout
#  psxy ./obs_map.txt -R -Jq -Sx0.1 -W3 -G0/0/0 -O >> $psout
#
#  convert -rotate 90 -density 720 -resize 900 $psout $pngout
#  convert $pngout -background white -flatten -alpha off $pngout
#
#  mv $pngout ./figure/obs_map_${OBSNET}.png
#  rm $psout
  mv markplot_station.gs FIGrads/markplot_station_${OBSNET}.gs
  mv markplot_statsml.gs FIGrads/markplot_statsml_${OBSNET}.gs
  mv markplot_gridpnt.gs FIGrads/markplot_gridpnt_${OBSNET}.gs

  unlink station.tbl.tmp
done


exit
