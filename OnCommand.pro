Pro OnCommand, Event
  widget_control,event.top,get_uvalue=pState
  AddT75T90, pState
  print,'Done'
End
;-----------------------------------------------------------------
;Pro OnCommand, Event
Pro OnCommand_ExportRiseGroupTrace, Event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF
; export group trace
;flashPara [   0   ,  1  , 2 ,  3 ,   4   ,  5   , 6 ,  7  ,  8 ,    9   ,  10 ,  11 ,  12  , 13 , 14 ,  15 , 16 , 18, 19]
;flashPara [FlashNo, Peak, F0, Amp, Tstart, Tpeak, RT, RT90, T50, EndBase, Note, FAHM, dBase, T75, T90, FDHM, Dia, t1, t2]
  CATCH, Error_status
  IF Error_status NE 0 THEN BEGIN
    PRINT, '  Error message: ', !ERROR_STATE.MSG
    if n_elements(unit1) then free_lun,unit1
    if n_elements(unit2) then free_lun,unit2
    if n_elements(unit3) then free_lun,unit3
    return
    CATCH, /CANCEL
  ENDIF
  
  widget_control, event.top, get_uvalue=pState
  if ~ptr_valid(pState) then return
  if ~obj_valid((*pState).result) then return
  ROINum = (*pState).result->length()
  if ROINum eq 0 then return
  path = file_dirname((*pState).file[0])
  file = file_basename((*pState).file[0])
  
  openw,unit1,path+'\GroupTrace-1.txt',/get_lun,/append,width=2000
  openw,unit2,path+'\GroupTrace-2.txt',/get_lun,/append,width=2000
  openw,unit3,path+'\GroupTrace-3.txt',/get_lun,/append,width=2000
  for ROINo=0,ROINum-1 do begin
    info = (*pState).result->GetProperty(ROINo,trace=trace,flashPara=flashPara,flashNum=flashNum)
    if ~ptr_valid(flashPara) then continue
    if flashNum gt 1 then continue
    trace = reform(*trace[0])
    if (size(trace))[0] eq 2 then trace = reform(trace[0,*])
    trace = smooth(trace,3,edge=1)
    F0 = (*flashPara)[2]
    peak = (*flashPara)[1]
    peakTime = (*flashPara)[5]
    void = min(abs(*(*pState).CorrectedTime-peakTime),peakPos)
    t1 = peakPos-30
    t2 = peakPos+30
    subtrace = trace[t1>0:t2<((*pState).ts-1)]
    subtrace = (subtrace-F0)/(peak-F0)
    subtrace = string(subtrace,format='(f8.3)')
    if t1 lt 0 then subtrace = [replicate('NaN',-t1),subtrace]
    if t2 ge (*pState).ts then $
      subtrace = [subtrace,replicate('NaN',t2-(*pState).ts+1)]
;    if t1 lt 0 then subtrace = [replicate(!values.F_NAN,-t1),subtrace]
;    if t2 ge (*pState).ts then $
;      subtrace = [subtrace,replicate(!values.F_NAN,t2-(*pState).ts+1)]
    case (*flashPara)[10] of 
    1: begin      
      printf,unit1,file,ROINo+1,subtrace      
    end
    2: begin      
      printf,unit2,file,ROINo+1,subtrace      
    end
    3: begin      
      printf,unit3,file,ROINo+1,subtrace
    end
    else:
    endcase
;    case (*flashPara)[10] of 
;    1: begin
;      openw,unit,(*pState).file[0]+'-GroupTrace-1.txt',/get_lun,/append,width=2000
;      printf,unit,strtrim(ROINo+1,1),subtrace
;      free_lun,unit
;    end
;    2: begin
;      openw,unit,(*pState).file[0]+'-GroupTrace-2.txt',/get_lun,/append,width=2000
;      printf,unit,strtrim(ROINo+1,1),subtrace
;      free_lun,unit
;    end
;    3: begin
;      openw,unit,(*pState).file[0]+'-GroupTrace-3.txt',/get_lun,/append,width=2000
;      printf,unit,strtrim(ROINo+1,1),subtrace
;      free_lun,unit
;    end
;    else:
;    endcase
  endfor  
  free_lun,unit1
  free_lun,unit2
  free_lun,unit3
End
;-----------------------------------------------------------------
;Pro OnCommand, Event
Pro OnCommand_ExportDBaseGroupTrace, Event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF
; export group trace
;flashPara [   0   ,  1  , 2 ,  3 ,   4   ,  5   , 6 ,  7  ,  8 ,    9   ,  10 ,  11 ,  12  , 13 , 14 ,  15 , 16 , 18, 19]
;flashPara [FlashNo, Peak, F0, Amp, Tstart, Tpeak, RT, RT90, T50, EndBase, Note, FAHM, dBase, T75, T90, FDHM, Dia, t1, t2]
  CATCH, Error_status
  IF Error_status NE 0 THEN BEGIN
    PRINT, '  Error message: ', !ERROR_STATE.MSG
;    if n_elements(unit1) then free_lun,unit1,/force
;    if n_elements(unit2) then free_lun,unit2,/force
;    if n_elements(unit3) then free_lun,unit3,/force
    close,1,2,3
    return
    CATCH, /CANCEL
  ENDIF
  
  widget_control, event.top, get_uvalue=pState
  if ~ptr_valid(pState) then return
  if ~obj_valid((*pState).result) then return
  ROINum = (*pState).result->length()
  if ROINum eq 0 then return
  path = file_dirname((*pState).file[0])
  file = file_basename((*pState).file[0])
  
  close,1,2,3
  openw,1,path+'\dBaseGroupTrace-1.txt',/append,width=2000
  openw,2,path+'\dBaseGroupTrace-2.txt',/append,width=2000
  openw,3,path+'\dBaseGroupTrace-3.txt',/append,width=2000
;  openw,unit1,path+'\dBaseGroupTrace-1.txt',/get_lun,/append,width=2000
;  openw,unit2,path+'\dBaseGroupTrace-2.txt',/get_lun,/append,width=2000
;  openw,unit3,path+'\dBaseGroupTrace-3.txt',/get_lun,/append,width=2000
;  print,unit1,unit2,unit3
  for ROINo=0,ROINum-1 do begin
    info = (*pState).result->GetProperty(ROINo,trace=trace,flashPara=flashPara,flashNum=flashNum)
    if ~ptr_valid(flashPara) then continue
    if flashNum gt 1 then continue
    if (*flashPara)[10] ge 5 then return
        
    if (*flashPara)[12] ge -0.02 and (*flashPara)[12] le 0.02 then begin
      trace = reform(*trace[0])
      if (size(trace))[0] eq 2 then trace = reform(trace[0,*])
      trace = smooth(trace,3,edge=1)
      F0 = (*flashPara)[2]
      peak = (*flashPara)[1]
      peakTime = (*flashPara)[5]
      void = min(abs(*(*pState).CorrectedTime-peakTime),peakPos)
      t1 = peakPos-30
      t2 = peakPos+30
      subtrace = trace[t1>0:t2<((*pState).ts-1)]
      subtrace = (subtrace-F0)/(peak-F0)
      subtrace = string(subtrace,format='(f8.3)')
      if t1 lt 0 then subtrace = [replicate('NaN',-t1),subtrace]
      if t2 ge (*pState).ts then $
        subtrace = [subtrace,replicate('NaN',t2-(*pState).ts+1)]
      printf,1,strtrim((*flashPara)[12],1),' ',file,ROINo+1,subtrace
;      printf,unit1,(*flashPara)[12],file,ROINo+1,subtrace
      continue      
    endif
    if (*flashPara)[12] ge -0.22 and (*flashPara)[12] le -0.18 then begin
      trace = reform(*trace[0])
      if (size(trace))[0] eq 2 then trace = reform(trace[0,*])
      trace = smooth(trace,3,edge=1)
      F0 = (*flashPara)[2]
      peak = (*flashPara)[1]
      peakTime = (*flashPara)[5]
      void = min(abs(*(*pState).CorrectedTime-peakTime),peakPos)
      t1 = peakPos-30
      t2 = peakPos+30
      subtrace = trace[t1>0:t2<((*pState).ts-1)]
      subtrace = (subtrace-F0)/(peak-F0)
      subtrace = string(subtrace,format='(f8.3)')
      if t1 lt 0 then subtrace = [replicate('NaN',-t1),subtrace]
      if t2 ge (*pState).ts then $
        subtrace = [subtrace,replicate('NaN',t2-(*pState).ts+1)]
      printf,2,strtrim((*flashPara)[12],1),' ',file,ROINo+1,subtrace
;      printf,unit2,(*flashPara)[12],file,ROINo+1,subtrace
      continue      
    endif
    if (*flashPara)[12] ge 0.08 and (*flashPara)[12] le 0.12 then begin 
      trace = reform(*trace[0])
      if (size(trace))[0] eq 2 then trace = reform(trace[0,*])
      trace = smooth(trace,3,edge=1)
      F0 = (*flashPara)[2]
      peak = (*flashPara)[1]
      peakTime = (*flashPara)[5]
      void = min(abs(*(*pState).CorrectedTime-peakTime),peakPos)
      t1 = peakPos-30
      t2 = peakPos+30
      subtrace = trace[t1>0:t2<((*pState).ts-1)]
      subtrace = (subtrace-F0)/(peak-F0)
      subtrace = string(subtrace,format='(f8.3)')
      if t1 lt 0 then subtrace = [replicate('NaN',-t1),subtrace]
      if t2 ge (*pState).ts then $
        subtrace = [subtrace,replicate('NaN',t2-(*pState).ts+1)]
      printf,3,strtrim((*flashPara)[12],1),' ',file,ROINo+1,subtrace
;      printf,unit3,(*flashPara)[12],file,ROINo+1,subtrace
      continue      
    endif
;    case (*flashPara)[10] of 
;    1: begin
;      openw,unit,(*pState).file[0]+'-GroupTrace-1.txt',/get_lun,/append,width=2000
;      printf,unit,strtrim(ROINo+1,1),subtrace
;      free_lun,unit
;    end
;    2: begin
;      openw,unit,(*pState).file[0]+'-GroupTrace-2.txt',/get_lun,/append,width=2000
;      printf,unit,strtrim(ROINo+1,1),subtrace
;      free_lun,unit
;    end
;    3: begin
;      openw,unit,(*pState).file[0]+'-GroupTrace-3.txt',/get_lun,/append,width=2000
;      printf,unit,strtrim(ROINo+1,1),subtrace
;      free_lun,unit
;    end
;    else:
;    endcase
  endfor  
;  free_lun,unit1,unit2,unit3,/force
  close,1,2,3
End
;-----------------------------------------------------------------
Pro OnCommand_mask, Event
;Pro OnCommand, Event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  widget_control, event.top,get_uvalue=pState
  maskContour = read_tiff((*pState).file[0]+'-maskContour.tif')
  maskContour = round(maskContour*(*pState).pxs)
  print, max(maskContour)
  levels = fltarr(11)
  for k=0,n_elements(levels)-1 do begin
    region = where(maskContour eq k+1, count)
    levels[k] = count
  endfor
  window,1 & plot, levels
  print, reform(levels,1,n_elements(levels))
  return
  index = where(*(*pState).mask eq 1, count)
  indices = array_indices(*(*pState).mask, index)
  maskContour = uint(*(*pState).mask)
  edge = roberts(*(*pState).mask) gt 0
;  window,0,xs=(*pState).xs,ys=(*pState).ys
;  tvscl, edge
  tempEdge = edge
  start = (where(tempEdge eq 1))[0]
  while start ne -1 do begin
    pos = array_indices(tempEdge, start)
    r = search2d(tempEdge, pos[0], pos[1], 1.0, 1.0)
    if n_elements(r) gt 400 then begin
      tempEdge[r] = 2
    endif else tempEdge[r] = 0
    start = (where(tempEdge eq 1))[0]
  endwhile
; x1 = (p[0]-dim[1]/2) > 0
; x2 = (p[0]+dim[1]/2) < (dim[0]-1)
; localMask = bytarr(dim[0],dim[1])
; localMask[x1:x2,*] = 1
; tempEdge *= localMask
; window,11,xs=900,ys=256 & tvscl,tempEdge
  boundry = where(tempEdge eq 2)
  boundry = array_indices(tempEdge, boundry)
  for i=0L,count-1 do begin
;  for i=10000,10000 do begin
    distance = sqrt((boundry[0,*]-indices[0,i])^2 + (boundry[1,*]-indices[1,i])^2)
    maskContour[indices[0,i],indices[1,i]] += round(min(distance))*(*pState).pxs
  endfor
  window,0,xs=(*pState).xs,ys=(*pState).ys
  tvscl, maskContour
  write_tiff, (*pState).file[0]+'-maskContour.tif',maskContour

;  window,2,xs=1000,ys=300
;  contour,maskContour,levels=indgen(11)
  return
End
;-----------------------------------------------------------------
Pro OnCommand_ModifyPreviousArea, Event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF
  widget_control, Event.top, get_uvalue=pState
  if ~ptr_valid(pState) then return
  if ~obj_valid((*pState).result) then return
  ROINum = (*pState).result->length()
  if ROINum le 0 then return
  
  for ROINo=0,ROINum-1 do begin
    info = (*pState).result->GetProperty(ROINo,ROIPara=ROIPara,oROI=oROI)
    if info eq -1 then continue
    roiMask = oROI->ComputeMask(dimensions=[(*pState).xs,(*pState).ys])
    index = where(roiMask gt 0)
    ROIPara[0] = n_elements(index)*(*pState).pxs^2
    info = (*pState).result->SetProperty(ROINo,ROIPara=ROIPara)
  endfor
  
End

