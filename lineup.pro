function lineup,p1,p2
  n = max(abs(p2-p1))+1
  p = fltarr(2,n)
  dp = float(p2 - p1)
  np = dp/(n-1)
  for ind = 0,n-1 do begin
    p[*,ind] = p1 + np*ind
  endfor
  return,p
end