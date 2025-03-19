{
  for (i = 1; i <= NF; i++) {
    sum[i] += $i;   # 各列の値を加算
    count[i] += 1;  # 各列の行数をカウント
  }
}
END {
  for (i = 1; i <= NF; i++) {
    if (count[i] > 0) {
      printf "%f ", sum[i] / count[i];  # 平均を計算して横に並べる
    }
  }
  print "";  # 改行を追加
}
