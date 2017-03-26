pro linkup,data,out,th = th
    n = n_elements(data[0,*])
    xout = [data[0,0]]
    yout = [data[1,0]]
    x0 = data[0,0]
    y0 = data[1,0]
    if ~keyword_set(th) then th = 3
    for ii = 1,n-1 do begin
        if abs(data[1,ii] - y0) gt th then continue         
        newdata = lineup([x0,y0],data[*,ii])
        xout = [xout,reform(newdata[0,*])]
        yout = [yout,reform(newdata[1,*])]
        x0 = data[0,ii]
        y0 = data[1,ii]
    endfor
    n = n_elements(xout)
    out  = fltarr(2,n)
    out[0,*] = xout
    out[1,*] = yout
end