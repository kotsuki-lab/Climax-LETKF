#!/bin/bash
# memo: work on fuji, 20240708

CWD=`pwd`
rm ./fig_020_box/*.eps ./fig_020_box/*.png
gnuplot "gscript_020.gnu"

###==> convert fig.
cd ./fig_020_box/

FNUMTMP=`find ./ -name "*.eps" -print`
#FNUMTMP=`find ./ -name "????.eps" -print`
for FNUM in ` echo ${FNUMTMP}` ; do
#  FNAME=`echo ${FNUM}|cut -c 1-13`
  basename=${FNUM##*/}
  filename=${basename%.*}
  FOUT=${filename}.png
  FEPS=${filename}.eps
  echo "   converting... " $FNUM " ==>  " $FOUT

  singularity run /export/home/imageshare/imagemagick_latest.sif convert -density 120 -resize 820 $FNUM $FOUT
  singularity run /export/home/imageshare/imagemagick_latest.sif convert $FOUT -background white -flatten -alpha off ${FOUT}
  singularity run /export/home/imageshare/imagemagick_latest.sif convert -gravity center -crop 820x650+00+00 ${FOUT} ${FOUT}
  rm $FNUM  
done 

cd $CWD/fig_020_box/
singularity run /export/home/imageshare/imagemagick_latest.sif convert +append M000020_a_Uxxx.png  M000020_b_Vxxx.png  M000020_c_Txxx.png  temp_1.png
singularity run /export/home/imageshare/imagemagick_latest.sif convert +append M000020_d_Qxxx.png  M000020_e_Zxxx.png  M000020_f_Psrf.png  temp_2.png
singularity run /export/home/imageshare/imagemagick_latest.sif convert -append temp_1.png temp_2.png                                       montage.png
rm temp_1.png temp_2.png

echo "end script 020_mkfig_box-bars.sh"
exit
