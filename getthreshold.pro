function getthreshold,pic
  threshold = mean(pic)
  repeat begin
    threshold0 = threshold
    indw = where(pic gt threshold)
    indb = where(pic le threshold)
    aver_w = mean(pic[indw])
    aver_b = mean(pic[indb])
    threshold = (aver_w + aver_b)/2
  endrep until abs(threshold0 - threshold) lt 0.5
  return,threshold
end