#!/bin/bash
# memo: work on fuji, 20240708

CWD=`pwd`
rm ./fig_010_time/*.eps ./fig_010_time/*.png
gnuplot "gscript_010_RTPX.gnu"


###==> convert fig.
cd ./fig_010_time/

FNUMTMP=`find ./ -name "*.eps" -print`
#FNUMTMP=`find ./ -name "????.eps" -print`
for FNUM in ` echo ${FNUMTMP}` ; do
#  FNAME=`echo ${FNUM}|cut -c 1-13`
  basename=${FNUM##*/}
  filename=${basename%.*}
  FOUT=${filename}.png
  FEPS=${filename}.eps
  echo "   converting... " $FNUM " ==>  " $FOUT

  singularity run /export/home/imageshare/imagemagick_latest.sif convert -density 120 -resize 900 $FNUM $FOUT
  singularity run /export/home/imageshare/imagemagick_latest.sif convert $FOUT -background white -flatten -alpha off ${FOUT}
  singularity run /export/home/imageshare/imagemagick_latest.sif convert -gravity center -crop 900x510+0+00 ${FOUT} ${FOUT}
  rm $FNUM  
done

cd $CWD/fig_010_time/
singularity run /export/home/imageshare/imagemagick_latest.sif convert \
  +append time_raob_wb_rmse_T500_def.png  time_raob_wb_rmse_Z500_def.png  montage_def.png



echo "end script 010_mkfig_times.sh"
exit

