; subroutines for mask generation
;-----------------------------------------------------------------
Function AutoGetMask, ima
;COMPILE_OPT STRICTARR

	size_x = (size(ima))[1]
	size_y = (size(ima))[2]

	mask = bytarr(size_x,size_y)
	fixMask = bytarr(size_x, size_y)

	imaHist = histogram(ima, min=0)
	imaMax = max(ima, min=imaMin)
	histMax1 = max(imaHist, grayMax1)
	imaHist = median(imaHist,5)
	histderiv = imaHist - shift(imaHist,1)
	mini1 = min(where(histDeriv[grayMax1+1:*] gt 0)) + grayMax1+1
	histMax2 = max(imaHist[mini1:*], grayMax2)
	grayMax2 += mini1
	histMin2 = min(imaHist[mini1:grayMax2], mini2)
	mini2 +=mini1

	mask = ima gt (mini1+mini2-1)/2.0
	mask = median(mask, 5)
	return, mask
;	index = where(mask eq 1, count)
;	while count gt 0 do begin
;		pos = array_indices(mask, index[0])
;		r = search2d(mask, pos[0], pos[1], 1, 1)
;		if n_elements(r) gt size_x*size_y/3 then fixMask[r] = 1
;		mask[r] = 2
;		index = where(mask eq 1, count)
;	endwhile
;	return, fixmask
End
;-------------------------------------------------------------
function GetEdge, mask
;COMPILE_OPT STRICTARR
  newMask = mask
  dim = size(mask,/dimensions)
  newMask[0,*] = 0b & newMask[dim[0]-1,*] = 0b
  newMask[*,0] = 0b & newMask[*,dim[1]-1] = 0b
  edgeMask = roberts(newmask) gt 0
  start = (where(edgeMask eq 1))[0]
  help,start
  while start ne -1 do begin
    pos = array_indices(dim,start,/dimensions)
    r = search2d(edgeMask, pos[0], pos[1], 1.0, 1.0)
    if n_elements(r) gt 400 then begin
      EdgeMask[r] = 2
    endif else EdgeMask[r] = 0
    start = (where(edgeMask eq 1))[0]
  endwhile
  edge = where(EdgeMask eq 2)
  return, edge[0] eq -1 ? [0,0] : array_indices(dim,edge,/dimensions)
end