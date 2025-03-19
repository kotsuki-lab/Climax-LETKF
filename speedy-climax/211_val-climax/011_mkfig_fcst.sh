#!/bin/bash
# memo: work on fuji, 20240708

CWD=`pwd`
rm ./fig_011_fcst/*.eps ./fig_011_fcst/*.png
gnuplot "gscript_011.gnu"

###==> convert fig.
cd ./fig_011_fcst/

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


cd $CWD/fig_011_fcst/
singularity run /export/home/imageshare/imagemagick_latest.sif convert +append time_raob_wb_rmse_T500_def.png time_raob_wb_rmse_Z500_def.png  montage.png



echo "end script 011_mkfig_times.sh"
exit

