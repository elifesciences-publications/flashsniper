;-----------------------------------------------------------------
pro OnAnalyzeAllROI, Event
;COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF
  ans = dialog_message('Begin analyze all ROI?',/question)
  if ans eq 'No' then return
  widget_control, event.top, get_uvalue=pState

  if obj_valid((*pState).result) eq 0 then return
  if (*pState).result->length() eq 0 then return

  idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
  range = widget_info(idROISlider, /slider_min_max)
  fEvent = {top:event.top, value:0}
  for k=range[0],range[1] do begin
    fevent.value=k
    widget_control, idROISlider, set_value=k
    OnChangeROI, fevent
    OnAnalyzeROI, event, /silent
  endfor
  TraceDrawInitialize, (*pState).TraceDraw
end
;-----------------------------------------------------------------
pro AddT75T90, pState
COMPILE_OPT STRICTARR
  CATCH, Error_status
  IF Error_status NE 0 THEN BEGIN
    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
    return
    CATCH, /CANCEL
  ENDIF

  if obj_valid((*pState).result) eq 0 then return
  ROINum = (*pState).result->length()
  if ROINum le 0 then return
  interval = *(*pState).correctedTime-shift(*(*pState).correctedTime,1)
for ROINo=1,ROINum do begin
  info = (*pState).result->GetProperty(ROINo-1,trace=trace,flashPara=pFlashPara,flashNum=FlashNum)
  if info eq -1 then begin
    void = dialog_message('The ROI No. is out of range!',/error)
    continue
  endif
  if ~ptr_valid(pFlashPara) then begin
    continue
  endif
  trace = reform((*trace[0])[0,*])
  dim = size(*pFlashPara)
  for flashNo=0,flashNum-1 do begin
    void = min(abs(*(*pState).correctedTime-(*pFlashPara)[18,flashNo]),t1)
    void = min(abs(*(*pState).correctedTime-(*pFlashPara)[19,flashNo]),t2)
    void = min(abs(*(*pState).correctedTime-(*pFlashPara)[5,flashNo]),peakPos)
    F0 = (*pFlashPara)[2,flashNo]
    peak = (*pFlashPara)[1,flashNo]
print,t1,t2,peakpos
    f75 = 0.75*F0+0.25*peak
    if trace[t2] ge f75 then T75 = !values.F_NaN $  ;  if end point is above the mid line
    else begin
      Line75 = trace[peakPos:t2] ge f75
      T75 = total((interval[peakpos:t2])[where(line75 eq 1)])
;      lastPoint = max(where(Line75 eq 1))+peakPos
;      t751 = (*(*pState).correctedTime)[lastPoint]
;      t752 = (*(*pState).correctedTime)[lastPoint+1]
;      f751 = trace[lastPoint]
;      f752 = trace[lastPoint+1]
;      T75 = (t752-t751)*(f75-f751)/(f752-f751)+t751-(*pFlashPara)[5,flashNo]
    endelse
    (*pFlashPara)[13,flashNo] = T75
    f90 = 0.9*F0+0.1*peak
    if trace[t2] ge f90 then T90 = !values.F_NaN $  ;  if end point is above the mid line
    else begin
      Line90 = trace[peakPos:t2] ge f90
      T90 = total((interval[peakpos:t2])[where(line90 eq 1)])
;      lastPoint = max(where(Line90 eq 1))+peakPos
;      t901 = (*(*pState).correctedTime)[lastPoint]
;      t902 = (*(*pState).correctedTime)[lastPoint+1]
;      f901 = trace[lastPoint]
;      f902 = trace[lastPoint+1]
;      T90 = (t902-t901)*(f90-f901)/(f902-f901)+t901-(*pFlashPara)[5,flashNo]
    endelse
    (*pFlashPara)[14,flashNo] = T90
  endfor
; get flash parameters
;flashPara [   0   ,  1  , 2 ,  3 ,   4   ,  5   , 6 ,  7  ,  8 ,    9   ,  10 ,  11 ,  12  , 13 , 14 ,  15 , 16 , 18, 19]
;flashPara [FlashNo, Peak, F0, Amp, Tstart, Tpeak, RT, RT90, T50, EndBase, Note, FAHM, dBase, T75, T90, FDHM, Dia, t1, t2]
  widget_control, (*pState).ResultTable, get_value=table
  ROINoColumn = uint(reform(table[0,*]))
  lastRow = max(where(ROINoColumn ne 0))
  if lastRow eq -1 then return
  row = where(ROINoColumn eq ROINo,n_row)-1
  row=row[0]
  if row eq -2 then continue
  for i=1,FlashNum do begin
    widget_control,(*pState).ResultTable,use_table_select=[3,row+i,19,row+i], $
      set_value=[string((*pFlashPara)[0,i-1],format='(i0)'), $
      string((*pFlashPara)[1:9,i-1],format='(f10.2)'), $
      string((*pFlashPara)[10,i-1],format='(i0)'), $
      string((*pFlashPara)[11:16,i-1],format='(f10.2)')]
  endfor
endfor
End
;-----------------------------------------------------------------
pro AddFDHM, pState
COMPILE_OPT STRICTARR
  CATCH, Error_status
  IF Error_status NE 0 THEN BEGIN
    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
    return
    CATCH, /CANCEL
  ENDIF

  if obj_valid((*pState).result) eq 0 then return
  ROINum = (*pState).result->length()
  if ROINum le 0 then return
  interval = *(*pState).correctedTime-shift(*(*pState).correctedTime,1)
for ROINo=1,ROINum do begin
  info = (*pState).result->GetProperty(ROINo-1,trace=trace,flashPara=pFlashPara,flashNum=FlashNum)
  if info eq -1 then begin
    void = dialog_message('The ROI No. is out of range!',/error)
    continue
  endif
  if ~ptr_valid(pFlashPara) then begin
    continue
  endif
  trace = reform((*trace[0])[0,*])
  dim = size(*pFlashPara)
  for flashNo=0,flashNum-1 do begin
    void = min(abs(*(*pState).correctedTime-(*pFlashPara)[18,flashNo]),t1)
    void = min(abs(*(*pState).correctedTime-(*pFlashPara)[19,flashNo]),t2)
    void = min(abs(*(*pState).correctedTime-(*pFlashPara)[4,flashNo]),startPos)
    F0 = (*pFlashPara)[2,flashNo]
    peak = (*pFlashPara)[1,flashNo]
;   print,t1,t2,startPos
    if endBase ge f50 then FDHM = !values.F_NaN $
    else begin
      line50 = trace[startPos:t2] ge f50
      firstPoint = min(where(line50 eq 1))+startPos
      lastPoint = max(where(line50 eq 1))+startPos
      t502 = (*(*pState).correctedTime)[firstPoint]
      t501 = (*(*pState).correctedTime)[(firstPoint-1)>0]
      f502 = trace[firstPoint]
      f501 = trace[(firstPoint-1)>0]
      firstTime = (t502-t501)*(f50-f501)/(f502-f501)+t501
      t501 = (*(*pState).correctedTime)[lastPoint]
      t502 = (*(*pState).correctedTime)[(lastPoint+1)<((*pState).ts-1)]
      f501 = trace[lastPoint]
      f502 = trace[(lastPoint+1)<((*pState).ts-1)]
      lastTime = (t502-t501)*(f50-f501)/(f502-f501)+t501
      FDHM = lastTime - firstTime
    endelse
    (*pFlashPara)[15,flashNo] = FDHM
  endfor
; get flash parameters
;flashPara [   0   ,  1  , 2 ,  3 ,   4   ,  5   , 6 ,  7  ,  8 ,    9   ,  10 ,  11 ,  12  , 13 , 14 ,  15 , 16 , 18, 19]
;flashPara [FlashNo, Peak, F0, Amp, Tstart, Tpeak, RT, RT90, T50, EndBase, Note, FAHM, dBase, T75, T90, FDHM, Dia, t1, t2]
  widget_control, (*pState).ResultTable, get_value=table
  ROINoColumn = uint(reform(table[0,*]))
  lastRow = max(where(ROINoColumn ne 0))
  if lastRow eq -1 then return
  row = where(ROINoColumn eq ROINo,n_row)-1
  row=row[0]
  if row eq -2 then continue
  for i=1,FlashNum do begin
    widget_control,(*pState).ResultTable,use_table_select=[3,row+i,19,row+i], $
      set_value=[string((*pFlashPara)[0,i-1],format='(i0)'), $
      string((*pFlashPara)[1:9,i-1],format='(f10.2)'), $
      string((*pFlashPara)[10,i-1],format='(i0)'), $
      string((*pFlashPara)[11:16,i-1],format='(f10.2)')]
  endfor
endfor
End
;-----------------------------------------------------------------
pro ModifyFAHM, pState
COMPILE_OPT STRICTARR
  CATCH, Error_status
  IF Error_status NE 0 THEN BEGIN
    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
    return
    CATCH, /CANCEL
  ENDIF

  if obj_valid((*pState).result) eq 0 then return
  ROINum = (*pState).result->length()
  if ROINum le 0 then return
  interval = *(*pState).correctedTime-shift(*(*pState).correctedTime,1)

  ROIS = (*pState).oAutoROIModel->Get(/all)
  n = n_elements(ROIs)
  ROIMap = replicate(1b,(*pState).xs,(*pState).ys)
  for i=0,n-1 do begin
    roiMask = ROIs[i]->ComputeMask(dimensions=[(*pState).xs,(*pState).ys])
    ROIMap[where(roiMask gt 0)] = 0b
  endfor

for ROINo=1,ROINum do begin
  info = (*pState).result->GetProperty(ROINo-1,trace=trace,flashPara=pFlashPara,flashNum=FlashNum)
  if info eq -1 then begin
    void = dialog_message('The ROI No. is out of range!',/error)
    continue
  endif
  if ~ptr_valid(pFlashPara) then begin
    continue
  endif
  roiMask = oROI->ComputeMask(dimensions=[(*pState).xs,(*pState).ys])
  forbidenMask = ROIMap or roiMask
  dim = size(*pFlashPara)
  for flashNo=0,flashNum-1 do begin
    void = min(abs(*(*pState).correctedTime-(*pFlashPara)[4,flashNo]),startPos)
    void = min(abs(*(*pState).correctedTime-(*pFlashPara)[5,flashNo]),peakPos)
    t0 = (startPos-2)>0
    for k=t0,startPos do begin
      if k gt t0 then baseRegion += (*(*pState).img[0])[*,*,k] $
      else baseRegion = (*(*pState).img[0])[*,*,k]
    endfor
    baseRegion /= (startPos-t0+1)
    peakRegion = (*(*pState).img[0])[*,*,peakPos]
    peakRegion -= baseRegion
    peakRegion = convol(median(peakRegion,3), GaussianMask(0.7), /center, /edge_truncate)
    centerPeak = max(peakRegion[where(roiMask eq 1)])
    HMMask = peakRegion ge centerPeak*0.5
    mask1 = HMMask and forbidenMask
    mask2 = mask1 and roiMask
    halfMaximum = where(mask2 eq 0)
    oldFAHM = n_elements(halfMaximum)*(*pState).pxs^2 ;& print,oldFAHM
    step = 0
    while step lt 10 do begin
      step++
      roiMask = dilate(roiMask,[[0,1,0],[1,1,1],[0,1,0]])
      mask2 = mask1 and roiMask
      halfMaximum = where(mask2 eq 1)
      FAHM = n_elements(halfMaximum)*(*pState).pxs^2
      if FAHM-oldFAHM le 0.05 then break
      oldFAHM = FAHM
    endwhile
    (*pFlashPara)[11,flashNo] = FAHM ;& print,FAHM,step
    ;--------------------------- diameter----------------------
    tempMask = bytarr((*pState).xs,(*pState).ys)
    tempMask[halfMaximum] = 1b
    tempEdge = tempMask-erode(tempMask,replicate(1,3,3))
    index = where(tempEdge eq 1)
    edgeX = index mod (*pState).xs
    edgey = uint(index/(*pState).xs)
    n = n_elements(edgeX)
    X1 = rebin(reform(edgeX,n,1),n,n)
    X2 = rebin(reform(edgeX,1,n),n,n)
    Y1 = rebin(reform(edgeY,n,1),n,n)
    Y2 = rebin(reform(edgeY,1,n),n,n)
    distMap = sqrt((X1-X2)^2+(Y1-Y2)^2)
    distArray = distMap[reverse(sort(distMap))]
    maxLength = mean(distArray[0:5])*(*pState).pxs
    (*pFlashPara)[16,flashNo] = maxLength
  endfor
; get flash parameters
;flashPara [   0   ,  1  , 2 ,  3 ,   4   ,  5   , 6 ,  7  ,  8 ,    9   ,  10 ,  11 ,  12  , 13 , 14 ,  15 , 16 , 18, 19]
;flashPara [FlashNo, Peak, F0, Amp, Tstart, Tpeak, RT, RT90, T50, EndBase, Note, FAHM, dBase, T75, T90, FDHM, Dia, t1, t2]
  widget_control, (*pState).ResultTable, get_value=table
  ROINoColumn = uint(reform(table[0,*]))
  lastRow = max(where(ROINoColumn ne 0))
  if lastRow eq -1 then return
  row = where(ROINoColumn eq ROINo,n_row)-1
  row=row[0]
  if row eq -2 then continue
  for i=1,FlashNum do begin
    widget_control,(*pState).ResultTable,use_table_select=[3,row+i,19,row+i], $
      set_value=[string((*pFlashPara)[0,i-1],format='(i0)'), $
      string((*pFlashPara)[1:9,i-1],format='(f10.2)'), $
      string((*pFlashPara)[10,i-1],format='(i0)'), $
      string((*pFlashPara)[11:16,i-1],format='(f10.2)')]
  endfor
endfor
end
;-----------------------------------------------------------------
pro OnModifyFlashPara, Event
COMPILE_OPT STRICTARR
  CATCH, Error_status
  IF Error_status NE 0 THEN BEGIN
    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
    return
    CATCH, /CANCEL
  ENDIF

  widget_control, event.top, get_uvalue=pState
  if obj_valid((*pState).result) eq 0 then return
  idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
  widget_control, idROISlider, get_value=ROINo
  info = (*pState).result->GetProperty(ROINo-1,oROI=oROI,trace=trace,flashPara=pFlashPara,flashNum=oldFlashNum)
  if info eq -1 then begin
    void = dialog_message('The ROI No. is out of range!',/error)
  endif
  if ~ptr_valid(pFlashPara) then begin
    return
  endif
  PlotTrace, pState, trace[0],title=file_basename((*pState).file[0])+'  ROI '+strtrim(ROINo,1)
  trace = reform((*trace[0])[0,*])
;  trace = smooth(trace,3,edge=1)

  dim = size(*pFlashPara)
  if dim[1] eq 9 then begin  ;first extention
    if dim[0] eq 1 then begin
      para = fltarr(20)
      para[0] = (*pFlashPara)[0:6]
      para[8] = (*pFlashPara)[7:8]
    endif else begin
      para = fltarr(20,dim[2])
      para[0:6,*] = (*pFlashPara)[0:6,*]
      para[8:9,*] = (*pFlashPara)[7:8,*]
    endelse
    *pFlashPara = para
  endif
  if dim[1] eq 11 then begin  ;second extention
    if dim[0] eq 1 then begin
      para = fltarr(20)
      para[0] = (*pFlashPara)[0:10]
    endif else begin
      para = fltarr(20,dim[2])
      para[0:10,*] = (*pFlashPara)[0:10,*]
    endelse
    *pFlashPara = para
  endif

  widget_control, (*pState).ResultTable, get_value=table
  ROINoColumn = uint(reform(table[0,*]))
  lastRow = max(where(ROINoColumn ne 0))
  if lastRow eq -1 then return
  row = where(ROINoColumn eq ROINo,n_row)-1
  row=row[0]

  if ptr_valid((*pState).peak) then begin
    peak = median(*(*pState).peak,3)
    roiMask = oROI->ComputeMask(dimensions=[(*pState).xs,(*pState).ys])
    index = where(roiMask gt 0)
    centerPeak = max(median(peak[index],3))
    index = index[where(peak[index] ge centerPeak*0.5)]
    FAHM = n_elements(uniq(index))*(*pState).pxs^2
  endif else FAHM = 0

  baseLine = BaseLineFit(*(*pState).correctedTime,trace)
  eLine1 = trace-baseLine
  filter1 = DIGITAL_FILTER(0,0.28,50,5)
  eLine1 = convol(eLine1,filter1,/edge_truncate)
  ePeak = max(eLine1,ePeakpos)
  if epeakPos le 2 then begin
    return
  endif

  void = min(abs(*(*pState).correctedTime-(*pFlashPara)[4]),t0)
  t0 = t0[0]
  void = min(abs(trace[ePeakPos:*]-(*pFlashPara)[9]),t2)
  t2 = t2[0]+ePeakPos

  x = (*(*pState).correctedTime)[0:t0]
  para = ladfit(x,trace[0:t0])
  x = *(*pState).correctedTime
  pre_baseline = para[0]+para[1]*x
  eLine2 = trace-pre_baseLine
  eLine2 = convol(eLine2,filter1,/edge_truncate)

  t1 = (t0-2)>0
  rF01 = total(eLine1[t1:t0])/(t0-t1+1)
  rF02 = total(eLine2[t1:t0])/(t0-t1+1)
  rF11 = mean(eLine1[t2-2:(t2+2)<((*pState).ts-1)])
  rF12 = mean(eLine2[t2-2:(t2+2)<((*pState).ts-1)])
  dBase = ((*pFlashPara)[9]-(*pFlashPara)[2])/(*pFlashPara)[2]
  dBase1 = (rF11-rF01)/(*pFlashPara)[2]
  dBase2 = (rF12-rF02)/(*pFlashPara)[2]
  (*pFlashPara)[11,*] = FAHM
  (*pFlashPara)[12,*] = dBase
  (*pFlashPara)[13,*] = dBase1
  (*pFlashPara)[14,*] = dBase2
  (*pFlashPara)[18,*] = t1
  (*pFlashPara)[19,*] = t2
;  print, area1,dBase,dBase1,dBase2
; get flash parameters
;flashPara [   0   ,  1  , 2 ,  3 ,   4   ,  5   , 6 ,  7  ,  8 ,    9   ,  10 ,  11 ,  12  , 13 , 14 ,  15 , 16 , 18, 19]
;flashPara [FlashNo, Peak, F0, Amp, Tstart, Tpeak, RT, RT90, T50, EndBase, Note, FAHM, dBase, T75, T90, FDHM, Dia, t1, t2]
  for i=1,oldFlashNum do begin
    widget_control,(*pState).ResultTable,use_table_select=[3,row+i,19,row+i], $
      set_value=[string((*pFlashPara)[0,i-1],format='(i0)'), $
      string((*pFlashPara)[1:9,i-1],format='(f10.2)'), $
      string((*pFlashPara)[10,i-1],format='(i0)'), $
      string((*pFlashPara)[11:16,i-1],format='(f10.2)')]
  endfor
End
;-----------------------------------------------------------------
pro OnAnalyzeROI, Event, silent=silent
COMPILE_OPT STRICTARR
  CATCH, Error_status
  IF Error_status NE 0 THEN BEGIN
    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
    return
    CATCH, /CANCEL
  ENDIF

  widget_control, event.top, get_uvalue=pState
  if obj_valid((*pState).result) eq 0 then return
  idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
  widget_control, idROISlider, get_value=ROINo
  info = (*pState).result->GetProperty(ROINo-1,oROI=oROI,trace=trace,ROIPara=ROIPara,flashpara=pFlashPara,flashNum=oldFlashNum)
  if info eq -1 then begin
    if ~keyword_set(silent) then void = dialog_message('The ROI No. is out of range!',/error)
    return
  endif
  if ~ptr_valid(pFlashPara) then begin
    pFlashPara = ptr_new(fltarr(20))
    info = (*pState).result->SetProperty(ROINo-1,flashPara=pFlashPara)
  endif
  PlotTrace, pState, trace[0],title=file_basename((*pState).file[0])+'  ROI '+strtrim(ROINo,1);, /fit
  trace = reform((*trace[0])[0,*])
;  trace = smooth(trace,3,edge=1)
  interval = *(*pState).correctedTime-shift(*(*pState).correctedTime,1)

  widget_control, (*pState).ResultTable, get_value=table
  ROINoColumn = uint(reform(table[0,*]))
  lastRow = max(where(ROINoColumn ne 0))
  if lastRow eq -1 then return
  row = where(ROINoColumn eq ROINo,n_row)-1
  row=row[0]

  ;---------------------- Auto locate t1,t2 -----------------
  baseLine = BaseLineFit(*(*pState).correctedTime,trace)
  eLine = trace-baseLine & eLine1 = eLine
  filter1 = DIGITAL_FILTER(0,0.28,50,5)
  filter2 = DIGITAL_FILTER(0,0.4,50,15)
  eLine = convol(eLine,filter1,/edge_truncate)
;  trace = convol(trace,filter2,/edge_truncate)
  eLine1 = eLine
  
  ;-----------------------------
  eLine1_gra=gradient_trace(eLine1)/trace;;Gradient for peak detection, Added by suntao,20150711
  epeak_gra=max(eLine1_gra,epeakpos_gra)
  st=epeakpos_gra-10
  st=st ge 0? st:0
  ed=st+20
  ed=ed le n_elements(eLine1)-1? ed:n_elements(eLine1)-1
  epeak=max(eLine1[st:ed],ePeakpos)
  ePeakpos+=st
  print,'epeakpos',epeakpos,epeak_gra/trace[epeakpos_gra]
  ;---------------------------------------------
  ;ePeak = max(eLine,ePeakpos)
  if epeakPos le 2 then begin
    return
  endif
  t1 = (epeakPos-12) > 0
  x = (*(*pState).correctedTime)[0:t1]
  para = ladfit(x,trace[0:t1])
  x = (*(*pState).correctedTime)[0:epeakPos]
  
  PRE_BASELINE=min(trace[round(0.6*epeakpos):epeakpos]);;Modified by suntao,20150712
  
  ;pre_baseline = para[0]+para[1]*x
;  pre_baseline = convol(trace[0:epeakpos],filter1,/edge_truncate)
  deltaLine = trace[0:epeakPos]-pre_baseLine
  para = poly_fit((*(*pState).correctedTime)[0:epeakpos]-(*(*pState).correctedTime)[0], $
    deltaLine,6,yfit=fitEline)
;  fitEline = convol(deltaLine,filter1,/edge_truncate)
  t11 = max(where(fitEline le 0))
  for i=epeakPos-3,1,-1 do begin
    if fiteLine[i] ge fiteLine[i+1] and fiteLine[i-1] ge fiteLine[i+1] then begin
      t1 = i & break
    endif
  endfor
  
  x = (*(*pState).correctedTime)[0:t1]
  para = ladfit(x,trace[0:t1])
  x = *(*pState).correctedTime
  eLine2 = trace-para[0]-para[1]*x
  eLine2 = convol(eLine2,filter1,/edge_truncate)

  if epeakpos lt (*pState).ts-6 then begin
    x = (*(*pState).correctedTime)[epeakpos:*]-(*(*pState).correctedTime)[epeakPos]
    para = poly_fit(x,trace[ePeakPos:*],4,yfit=fitdecayline)
;    fitdecayline = convol(trace[ePeakPos:*],filter1,/edge_truncate)
    oplot,x+(*(*pState).correctedTime)[epeakPos],fitdecayline
    t2 = (*pState).ts-1
    for i=3,(*pState).ts-2-epeakPos do begin
      if fitdecayline[i] gt fitdecayline[i-1] and fitdecayline[i+1] gt fitdecayline[i-1] then begin
        t2 = i+ePeakPos
        break
      endif
    endfor
  endif else t2 = (*pState).ts-1
  wset,(*pState).traceWindow
  oplot, replicate((*(*pState).correctedTime)[t1], 2), [0,255], color='CCFF99'XL
  oplot, replicate((*(*pState).correctedTime)[t2], 2), [0,255], color='CCFF99'XL
;-------------------------------
;  return

; get flash parameters
;flashPara [   0   ,  1  , 2 ,  3 ,   4   ,  5   , 6 ,  7  ,  8 ,    9   ,  10 ,  11 ,  12  , 13 , 14 ,  15 , 16 , 18, 19]
;flashPara [FlashNo, Peak, F0, Amp, Tstart, Tpeak, RT, RT90, T50, EndBase, Note, FAHM, dBase, T75, T90, FDHM, Dia, t1, t2]
  flashNo = 1
  flashPara = fltarr(20)
  flashPara[0] = flashNo
  peak = MAX(trace[t1:t2],peakPos)
  PeakPos += t1
  flashPara[1] = peak
  peakTime = (*(*pState).correctedTime)[peakPos]
  flashPara[5] = peakTime
  oplot, [(peakTime-2)>0,(peakTime+2)<((*pState).ts-1)], [peak,peak], color='0000FF'XL
  oplot, [peakTime,peakTime], [peak-5,peak+5], color='0000FF'XL

  t0 = (t1-3) > 0
  avg = total(trace[t0:t1+2])/(t1+3-t0)
  tagLine = trace ge avg
  
  startPos = max(where(tagLine[t0:peakpos] eq 0) > 0) + t0;;Fixed a bug, peakpos (peak),suntao,20150711
  
  startTime = (*(*pState).correctedTime)[startPos]
  flashPara[4] = startTime
  t0 = (startPos-2)>0
  F0 = total(trace[t0:startPos])/(startPos-t0+1)
  flashPara[2] = F0
  oplot, [(startTime-2)>0,(startTime+2)<((*pState).ts-1)], [F0,F0], color='0000FF'XL
  oplot, [startTime,startTime], [F0-5,F0+5], color='0000FF'XL
  f90 = 0.9*peak+0.1*F0
  line90 = trace[t1:t2] ge f90
  firstPoint = min(where(line90 eq 1))+t1
  t901 = (*(*pState).correctedTime)[firstPoint]
  t902 = (*(*pState).correctedTime)[firstPoint-1]
  f901 = trace[firstPoint]
  f902 = trace[firstPoint-1]
  PT90 = (t902-t901)*(f90-f901)/(f902-f901)+t901
  flashPara[7] = PT90-startTime

  deltaF = (peak-F0)/F0
  flashPara[3] = deltaF
  flashPara[6] = flashPara[5]-flashPara[4]
; print, peak/baseLine[peakPos]-1,deltaF

  endBase = mean(trace[t2-2:(t2+2)<((*pState).ts-1)])
  flashPara[9] = endBase
  oplot, indgen(5)+(*(*pState).correctedTime)[t2]-4, replicate(endBase,5), color='0000FF'XL
  dBase = (flashPara[9]-flashPara[2])/flashPara[2]
  flashPara[12] = dBase

  f50 = 0.5*(peak+F0)
  if endBase ge f50 then T50 = !values.F_NaN $
  else begin
    line50 = trace[peakPos:t2] ge f50
    lastPoint = max(where(line50 eq 1))+peakPos
    t501 = (*(*pState).correctedTime)[lastPoint]
    t502 = (*(*pState).correctedTime)[(lastPoint+1)<((*pState).ts-1)]
    f501 = trace[lastPoint]
    f502 = trace[(lastPoint+1)<((*pState).ts-1)]
    T50 = (t502-t501)*(f50-f501)/(f502-f501)+t501-flashPara[5]
    oplot, [flashPara[5],flashPara[5]+T50], replicate(0.5*(peak+F0),2), color='0000FF'XL
  endelse
  flashPara[8] = T50
  if endBase ge f50 then FDHM = !values.F_NaN $
  else begin
    line50 = trace[startPos:t2] ge f50
    firstPoint = min(where(line50 eq 1))+startPos
    lastPoint = max(where(line50 eq 1))+startPos
    t502 = (*(*pState).correctedTime)[firstPoint]
    t501 = (*(*pState).correctedTime)[(firstPoint-1)>0]
    f502 = trace[firstPoint]
    f501 = trace[(firstPoint-1)>0]
    firstTime = (t502-t501)*(f50-f501)/(f502-f501)+t501
    t501 = (*(*pState).correctedTime)[lastPoint]
    t502 = (*(*pState).correctedTime)[(lastPoint+1)<((*pState).ts-1)]
    f501 = trace[lastPoint]
    f502 = trace[(lastPoint+1)<((*pState).ts-1)]
    lastTime = (t502-t501)*(f50-f501)/(f502-f501)+t501
    FDHM = lastTime - firstTime
  endelse
  flashPara[15] = FDHM
  f75 = 0.75*F0+0.25*peak
  if endBase ge f75 then T75 = !values.F_NaN $
  else begin
    Line75 = trace[peakPos:t2] ge f75
    T75 = total((interval[peakpos:t2])[where(line75 eq 1)])
;      lastPoint = max(where(Line75 eq 1))+peakPos
;      t751 = (*(*pState).correctedTime)[lastPoint]
;      t752 = (*(*pState).correctedTime)[lastPoint+1]
;      f751 = trace[lastPoint]
;      f752 = trace[lastPoint+1]
;      T75 = (t752-t751)*(f75-f751)/(f752-f751)+t751-(*pFlashPara)[5,flashNo]
  endelse
  flashPara[13] = T75
  f90 = 0.9*F0+0.1*peak
  if endBase ge f90 then T90 = !values.F_NaN $
  else begin
    Line90 = trace[peakPos:t2] ge f90
    T90 = total((interval[peakpos:t2])[where(line90 eq 1)])
;      lastPoint = max(where(Line90 eq 1))+peakPos
;      t901 = (*(*pState).correctedTime)[lastPoint]
;      t902 = (*(*pState).correctedTime)[lastPoint+1]
;      f901 = trace[lastPoint]
;      f902 = trace[lastPoint+1]
;      T90 = (t902-t901)*(f90-f901)/(f902-f901)+t901-(*pFlashPara)[5,flashNo]
  endelse
  flashPara[14] = T90

  ;-------------------------- FAHM -----------------------------
  ROIS = (*pState).oAutoROIModel->Get(/all)
  n = n_elements(ROIs)
  forbidenMask = replicate(1b,(*pState).xs,(*pState).ys)
  for i=0,n-1 do begin
    if ROIs[i] eq oROI then continue
    roiMask = ROIs[i]->ComputeMask(dimensions=[(*pState).xs,(*pState).ys])
    forbidenMask[where(roiMask gt 0)] = 0b
  endfor
  t0 = (startPos-2)>0
  for k=t0,startPos do begin
    if k gt t0 then baseRegion += (*(*pState).img[0])[*,*,k] $
    else baseRegion = (*(*pState).img[0])[*,*,k]
  endfor
  baseRegion /= (startPos-t0+1)
  peakRegion = (*(*pState).img[0])[*,*,peakPos]
  peakRegion -= baseRegion
  peakRegion = convol(median(peakRegion,3), GaussianMask(0.7), /center, /edge_truncate)
  roiMask = oROI->ComputeMask(dimensions=[(*pState).xs,(*pState).ys]) gt 0
  oldROIMask = roiMask
  centerPeak = max(peakRegion[where(roiMask eq 1)])
  HMMask = peakRegion ge centerPeak*0.5
  mask1 = HMMask and forbidenMask
  mask2 = mask1 and roiMask
  halfMaximum = where(mask2 eq 0)
  oldFAHM = n_elements(halfMaximum)*(*pState).pxs^2 ;& print,oldFAHM
  step = 0
  while step lt 10 do begin
    step++
    roiMask = dilate(roiMask,[[0,1,0],[1,1,1],[0,1,0]])
    mask2 = mask1 and roiMask
    halfMaximum = where(mask2 eq 1)
    FAHM = n_elements(halfMaximum)*(*pState).pxs^2
    if FAHM-oldFAHM le 0.05 then break
    oldFAHM = FAHM
  endwhile
  flashPara[11] = FAHM

  ;--------------------------- diameter ---------------------------
  tempMask = bytarr((*pState).xs,(*pState).ys)
  if halfmaximum lt 0 then halfmaximum=0;;added by suntao,20150712
  tempMask[halfMaximum] = 1b
  tempEdge = tempMask-erode(tempMask,replicate(1,3,3))
  index = where(tempEdge eq 1)
  edgeX = index mod (*pState).xs
  edgey = uint(index/(*pState).xs)
  n = n_elements(edgeX)
  X1 = rebin(reform(edgeX,n,1),n,n)
  X2 = rebin(reform(edgeX,1,n),n,n)
  Y1 = rebin(reform(edgeY,n,1),n,n)
  Y2 = rebin(reform(edgeY,1,n),n,n)
  distMap = sqrt((X1-X2)^2+(Y1-Y2)^2)
  distArray = distMap[reverse(sort(distMap))]
  maxLength = mean(distArray[0:5])*(*pState).pxs
  flashPara[16] = maxLength
;  maxi = max(distMap,maxPos)
;  p1 = maxPos[0] mod n
;  p2 = maxPos[0]/n
;  print, maxi,distArray[0:5]
;  window,1,xs=(*pState).xs,ys=(*pState).ys & tvscl,tempMask+roiMask
;  window,2,xs=(*pState).xs,ys=(*pState).ys & tvscl,tempEdge
;  plots,[edgeX[p1],edgeX[p2]],[edgeY[p1],edgeY[p2]],color=250,/dev
  ;----------------------------------------------------------------------

  flashPara[18] = t1
  flashPara[19] = t2
  info = (*pState).result->SetProperty(ROINo-1,flashNum=flashNo)
  if flashNo gt 1 then begin
    *pFlashPara = [[*pFlashPara],[flashPara]]
  endif else *pFlashPara = flashPara
  if flashNo lt oldFlashNum then $
    widget_control,(*pState).ResultTable,use_table_select=[0,row+flashNo+1,2,row+oldFlashNum],/delete_rows $
  else if flashNo gt oldFlashNum then $
    widget_control,(*pState).ResultTable,use_table_select=[0,row+flashNo,19,row+flashNo],insert_rows=1,alignment=2
  for i=1,flashNo do begin
    widget_control,(*pState).ResultTable,use_table_select=[0,row+i,19,row+i], $
      set_value=[string(ROINo,format='(i0)'), $
      string(ROIPara,format='(f10.2)'), $
      string((*pFlashPara)[0,i-1],format='(i0)'), $
      string((*pFlashPara)[1:9,i-1],format='(f10.2)'), $
      string((*pFlashPara)[10,i-1],format='(i0)'), $
      string((*pFlashPara)[11:16,i-1],format='(f10.2)')]
  endfor
end

PRO OnAnalyzeAllROI_Manual, event
COMPILE_OPT STRICTARR
  widget_control, event.top, get_uvalue=pState

  if obj_valid((*pState).result) eq 0 then return
  if (*pState).result->length() eq 0 then return

  idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')

  widget_control, idROISlider, get_value=ROINo

  range = widget_info(idROISlider, /slider_min_max)
  (*pState).cc = 1
  fEvent = {top:event.top, value:0}
  for k=ROINo,range[1] do begin
  	if (*pState).cc eq 0 then break
    fevent.value=k
    widget_control, idROISlider, set_value=k
    OnChangeROI, fevent
    OnAnalyzeROI_Manual, event;, /silent
  endfor

END


pro OnAnalyzeROI_Manual, Event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  widget_control, event.top, get_uvalue=pState
  if obj_valid((*pState).result) eq 0 then return
  idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
  widget_control, idROISlider, get_value=ROINo
  info = (*pState).result->GetProperty(ROINo-1,oROI=oROI,trace=trace,ROIPara=ROIPara,flashPara=pFlashPara,flashNum=oldFlashNum)
  if info eq -1 then begin
    void = dialog_message('The ROI No. is out of range!',/error)
  endif
  if ~ptr_valid(pFlashPara) then begin
    pFlashPara = ptr_new(fltarr(20))
    info = (*pState).result->SetProperty(ROINo-1,flashPara=pFlashPara)
  endif
  PlotTrace, pState, trace[0],title=file_basename((*pState).file[0])+'  ROI '+strtrim(ROINo,1);, /fit
  trace = reform((*trace[0])[0,*])
;  trace = smooth(trace,3,edge=1)
  interval = *(*pState).correctedTime-shift(*(*pState).correctedTime,1)

  filter1 = DIGITAL_FILTER(0,0.28,50,5)
  filter2 = DIGITAL_FILTER(0,0.4,50,15)
  baseLine = BaseLineFit(*(*pState).correctedTime,trace)
  eLine1 = trace-baseLine
  filter1 = DIGITAL_FILTER(0,0.28,50,5)
  eLine1 = convol(eLine1,filter1,/edge_truncate)
  
  eLine1_gra=gradient_trace(eLine1);;Gradient for peak detection, Added by suntao,20150711
  epeak_gra=max(eLine1_gra,epeakpos_gra)
  st=epeakpos_gra-10
  st=st ge 0? st:0
  ed=st+20
  ed=ed le n_elements(eLine1)-1? ed:n_elements(eLine1)-1
  epeak=max(eLine1[st:ed],ePeakpos)
  ePeakpos+=st
  
  ;ePeak = max(eLine1,ePeakpos)
  if epeakPos le 2 then begin
    return
  endif

  widget_control, (*pState).ResultTable, get_value=table
  ROINoColumn = uint(reform(table[0,*]))
  lastRow = max(where(ROINoColumn ne 0))
  if lastRow eq -1 then return
  row = where(ROINoColumn eq ROINo,n_row)-1
  row = row[0]

flashNo = 1
(*pState).cc = 1
while 1 do begin
  xyouts, 20,15, 'Left click: Start point      Right Click: Skip this ROI' ,color='0000FF'XL,/dev

  cursor, t1, f1, 3, /data
  xyouts, 20,15, 'Left click: Start point      Right Click: Skip this ROI' ,color='EDEDDD'XL,/dev
  if !mouse.BUTTON eq 4 then return
  if !mouse.BUTTON eq 2 then begin
  	(*pState).cc = 0
  	return
  endif
    oplot, replicate(t1, 200), findgen(200), color='CC6699'XL
    temp = min(abs(*(*pState).correctedTime-t1),t1)
  xyouts, 320,15, 'Left click: End Point' ,color='0000FF'XL,/dev
  cursor, t2, f2, 3, /data
  xyouts, 320,15, 'Left click: End Point' ,color='EDEDDD'XL,/dev
;  t2 = (*pState).ts-2
  oplot, replicate(t2, 200), findgen(200), color='CC6699'XL
  temp = min(abs(*(*pState).correctedTime-t2),t2)
  if t2 lt  t1 then return;;;; added by suntao 20140620
  x = (*(*pState).correctedTime)[0:t1]
  para = ladfit(x,trace[0:t1])
  x = *(*pState).correctedTime
  pre_baseline = para[0]+para[1]*x
  eLine2 = trace-pre_baseLine
  eLine2 = convol(eLine2,filter1,/edge_truncate)

; get flash parameters
;flashPara [   0   ,  1  , 2 ,  3 ,   4   ,  5   , 6 ,  7  ,  8 ,    9   ,  10 ,  11 ,  12  , 13 , 14 ,  15 , 16 , 18, 19]
;flashPara [FlashNo, Peak, F0, Amp, Tstart, Tpeak, RT, RT90, T50, EndBase, Note, FAHM, dBase, T75, T90, FDHM, Dia, t1, t2]
  flashPara = fltarr(20)
  flashPara[0] = flashNo
  peak = MAX(trace[t1:t2],peakPos)
  peakPos += t1
  flashPara[1] = peak
  peakTime = (*(*pState).correctedTime)[peakPos]
  flashPara[5] = peakTime
  oplot, [(peakTime-2)>0,(peakTime+2)<((*pState).ts-1)], [peak,peak], color='0000FF'XL
  oplot, [peakTime,peakTime], [peak-5,peak+5], color='0000FF'XL

  t0 = (t1-1) > 0
  avg = mean(trace[t0:t1+1])
  tagLine = trace gt avg
  startPos = max(where(tagLine[t0:peakPos] eq 0) > 0) + t0
  startTime = (*(*pState).correctedTime)[startPos]
  flashPara[4] = startTime
  F0 = mean(trace[(startPos-2)>0:startPos]) & flashPara[2] = F0
  oplot, [(startTime-2)>0,(startTime+2)<((*pState).ts-1)], [F0,F0], color='0000FF'XL
  oplot, [startTime,startTime], [F0-5,F0+5], color='0000FF'XL
  f90 = 0.9*peak+0.1*F0
  line90 = trace[t1:t2] ge f90
  firstPoint = min(where(line90 eq 1))+t1
  t901 = (*(*pState).correctedTime)[firstPoint]
  t902 = (*(*pState).correctedTime)[firstPoint-1]
  f901 = trace[firstPoint]
  f902 = trace[firstPoint-1]
  PT90 = (t902-t901)*(f90-f901)/(f902-f901)+t901
  flashPara[7] = PT90-startTime

  deltaF = (peak-F0)/F0 & flashPara[3] = deltaF
  flashPara[6] = flashPara[5]-flashPara[4]

  endBase = mean(trace[t2-1:(t2+1)<((*pState).ts-1)])
  flashPara[9] = endBase
  oplot, indgen(5)+(*(*pState).correctedTime)[t2]-4, replicate(endBase,5), color='0000FF'XL
  dBase = (flashPara[9]-flashPara[2])/flashPara[2]
  flashPara[12] = dBase

  f50 = 0.5*(peak+F0)
  if endBase ge f50 then begin
    T50 = !values.F_NaN 
;    print,'if is excuted'
    lastpoint=t2 ;;Added by suntao, lastpoint is not defined in if case.
  endif else begin
;    print,'else id excuted'
    line50 = trace[peakPos:t2] ge f50
    lastPoint = max(where(line50 eq 1))+peakPos
    t501 = (*(*pState).correctedTime)[lastPoint]
    t502 = (*(*pState).correctedTime)[(lastPoint+1)<((*pState).ts-1)]
    f501 = trace[lastPoint]
    f502 = trace[(lastPoint+1)<((*pState).ts-1)]
    T50 = (t502-t501)*(f50-f501)/(f502-f501)+t501-flashPara[5]
    oplot, [flashPara[5],flashPara[5]+T50], replicate(0.5*(peak+F0),2), color='0000FF'XL
  endelse
  flashPara[8] = T50
  if endBase ge f50 then FDHM = !values.F_NaN $
  else begin
    line50 = trace[startPos:t2] ge f50
    firstPoint = min(where(line50 eq 1))+startPos
    lastPoint = max(where(line50 eq 1))+startPos
    t502 = (*(*pState).correctedTime)[firstPoint]
    t501 = (*(*pState).correctedTime)[(firstPoint-1)>0]
    f502 = trace[firstPoint]
    f501 = trace[(firstPoint-1)>0]
    firstTime = (t502-t501)*(f50-f501)/(f502-f501)+t501
    t501 = (*(*pState).correctedTime)[lastPoint]
    t502 = (*(*pState).correctedTime)[(lastPoint+1)<((*pState).ts-1)]
    f501 = trace[lastPoint]
    f502 = trace[(lastPoint+1)<((*pState).ts-1)]
    lastTime = (t502-t501)*(f50-f501)/(f502-f501)+t501
    FDHM = lastTime - firstTime
  endelse
  flashPara[15] = FDHM
  f75 = 0.75*F0+0.25*peak
  if endBase ge f75 then T75 = !values.F_NaN $
  else begin
    Line75 = trace[peakPos:t2] ge f75
    T75 = total((interval[peakpos:t2])[where(line75 eq 1)])
;      lastPoint = max(where(Line75 eq 1))+peakPos
;      t751 = (*(*pState).correctedTime)[lastPoint]
;      t752 = (*(*pState).correctedTime)[lastPoint+1]
;      f751 = trace[lastPoint]
;      f752 = trace[lastPoint+1]
;      T75 = (t752-t751)*(f75-f751)/(f752-f751)+t751-(*pFlashPara)[5,flashNo]
  endelse
  flashPara[13] = T75
  f90 = 0.9*F0+0.1*peak
  if endBase ge f90 then T90 = !values.F_NaN $
  else begin
    Line90 = trace[peakPos:t2] ge f90
    T90 = total((interval[peakpos:t2])[where(line90 eq 1)])
;      lastPoint = max(where(Line90 eq 1))+peakPos
;      t901 = (*(*pState).correctedTime)[lastPoint]
;      t902 = (*(*pState).correctedTime)[lastPoint+1]
;      f901 = trace[lastPoint]
;      f902 = trace[lastPoint+1]
;      T90 = (t902-t901)*(f90-f901)/(f902-f901)+t901-(*pFlashPara)[5,flashNo]
  endelse
  flashPara[14] = T90

  ;-------------------------- FAHM -----------------------------
  ROIS = (*pState).oAutoROIModel->Get(/all)
  n = n_elements(ROIs)
  forbidenMask = replicate(1b,(*pState).xs,(*pState).ys)
;  for i=0,n-1 do begin
;    if ROIs[i] eq oROI then continue
;    roiMask = ROIs[i]->ComputeMask(dimensions=[(*pState).xs,(*pState).ys])
;    forbidenMask[where(roiMask gt 0)] = 0b
;  endfor
  t0 = (startPos-2)>0
  for k=t0,startPos do begin
    if k gt t0 then baseRegion += (*(*pState).img[0])[*,*,k] $
    else baseRegion = (*(*pState).img[0])[*,*,k]
  endfor
  baseRegion /= (startPos-t0+1)
  peakRegion = (*(*pState).img[0])[*,*,peakPos]
  peakRegion -= baseRegion
  peakRegion = convol(median(peakRegion,3), GaussianMask(0.7), /center, /edge_truncate)
  roiMask = oROI->ComputeMask(dimensions=[(*pState).xs,(*pState).ys]) gt 0
  oldROIMask = roiMask
  centerPeak = max(peakRegion[where(roiMask eq 1)])
  HMMask = peakRegion ge centerPeak*0.5
  mask1 = HMMask and forbidenMask
  mask2 = mask1 and roiMask
  halfMaximum = where(mask2 eq 0)
  oldFAHM = n_elements(halfMaximum)*(*pState).pxs^2 ;& print,oldFAHM
  step = 0
  while step lt 10 do begin
    step++
    roiMask = dilate(roiMask,[[0,1,0],[1,1,1],[0,1,0]])
    mask2 = mask1 and roiMask
    halfMaximum = where(mask2 eq 1)
    FAHM = n_elements(halfMaximum)*(*pState).pxs^2
    if FAHM-oldFAHM le 0.05 then break
    oldFAHM = FAHM
  endwhile
  flashPara[11] = FAHM

  ;--------------------------- diameter----------------------
  tempMask = bytarr((*pState).xs,(*pState).ys)
  tempMask[halfMaximum] = 1b
  tempEdge = tempMask-erode(tempMask,replicate(1,3,3))
  index = where(tempEdge eq 1)
  edgeX = index mod (*pState).xs
  edgey = uint(index/(*pState).xs)
  n = n_elements(edgeX)
  X1 = rebin(reform(edgeX,n,1),n,n)
  X2 = rebin(reform(edgeX,1,n),n,n)
  Y1 = rebin(reform(edgeY,n,1),n,n)
  Y2 = rebin(reform(edgeY,1,n),n,n)
  distMap = sqrt((X1-X2)^2+(Y1-Y2)^2)
  distArray = distMap[reverse(sort(distMap))]
  maxLength = mean(distArray[0:5])*(*pState).pxs
  flashPara[16] = maxLength
;  maxi = max(distMap,maxPos)
;  p1 = maxPos[0] mod n
;  p2 = maxPos[0]/n
;  print, maxi,distArray[0:5]
;  window,1,xs=(*pState).xs,ys=(*pState).ys & tvscl,tempMask+roiMask
;  window,2,xs=(*pState).xs,ys=(*pState).ys & tvscl,tempEdge
;  plots,[edgeX[p1],edgeX[p2]],[edgeY[p1],edgeY[p2]],color=250,/dev
  ;----------------------------------------------------------------------
;=============================ADD BY XJJ 20120829 flash end time in note

	flashPara[10] = (*(*pState).correctedTime)[lastpoint]
;	print,flashpara[10]
;=============================end add by xjj 20120829	
  flashPara[18] = t1
  flashPara[19] = t2
  print,t2
  if flashNo gt 1 then begin
    *pFlashPara = [[*pFlashPara],[flashPara]]
  endif else *pFlashPara = flashPara

  xyouts, 100,300, 'LEFT: Done       MIDDLE: Add a flash' ,color='0000FF'XL,/dev
  cursor,x,y,3
  xyouts, 100,300, 'LEFT: Done       MIDDLE: Add a flash' ,color='EDEDDD'XL,/dev
  if !mouse.BUTTON eq 2 then begin
    flashNo++
  endif else begin
    info = (*pState).result->SetProperty(ROINo-1,flashNum=flashNo)
    break
  endelse
endwhile
  if flashNo lt oldFlashNum then $
    widget_control,(*pState).ResultTable,use_table_select=[0,row+flashNo+1,2,row+oldFlashNum],/delete_rows $
  else if flashNo gt oldFlashNum then $
    widget_control,(*pState).ResultTable,use_table_select=[0,row+1,19,row+1],insert_rows=flashNo-oldFlashNum,alignment=2
  for i=1,flashNo do begin
    widget_control,(*pState).ResultTable,use_table_select=[0,row+i,19,row+i], $
      set_value=[string(ROINo,format='(i0)'), $
      string(ROIPara,format='(f10.2)'), $
      string((*pFlashPara)[0,i-1],format='(i0)'), $
      string((*pFlashPara)[1:9,i-1],format='(f10.2)'), $
      string((*pFlashPara)[10,i-1],format='(i0)'), $
      string((*pFlashPara)[11:16,i-1],format='(f10.2)')]
  endfor
end

pro endline
end