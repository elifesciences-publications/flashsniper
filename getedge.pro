;+
; :Description:
;    GetEdge computes the edge of the mask.
;
; :Params:
;    mask: the binary image from the flash data
;
;
;
; :Author: Kaitao Li, Suntao (suntao1991@pku.edu.cn)
;-20141214
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