
Pro PlotCellSignal, pgs, error=error, select=select
COMPILE_OPT STRICTARR

device, decomposed=1
;print,select
  for k=0,(*pgs).stackNum-1 do begin
    wset, (*pgs).winArray[k]
    if keyword_set(error) then erase $
    else plot, *(*(*pgs).pState).correctedTime ,*(*pgs).cellSignal[k], psym=4, /ynozero, $
      title='Channel '+strtrim(string(k),1), background='FFFFFF'XL, color='000000'XL
    oplot,(*(*(*pgs).pState).correctedTime)[*(*pgs).validframe],(*(*pgs).cellSignal[k])[*(*pgs).validframe], $
      color='0000FF'XL, thick=2,psym=5
    if keyword_set(select) then begin
      oplot, replicate((*(*(*pgs).pState).correctedTime)[select[0]],10), lindgen(10)*30, color='FF0000'XL
      oplot, replicate((*(*(*pgs).pState).correctedTime)[select[1]],10), lindgen(10)*30, color='FF0000'XL
    endif
  endfor

End

;-----------------------------------------------------------------
pro OnSelectMask, Event
COMPILE_OPT STRICTARR

  if event.select eq 0 then return
  
  widget_control, event.top, get_uvalue=pgs  
  if (*(*pgs).cellSignalMask[0])[0] gt 0 then begin
    (*pgs).cellSignal = (*pgs).cellSignalMask
    Plotcellsignal, pgs
    return
  endif
  
  if ~ptr_valid((*(*pgs).pState).mask) then begin
    void = dialog_message('Mask is not defined! Please get mask first!', /error)
    fake_event = {top:(*(*pgs).pState).MainBase}
    OnGetMask, fake_event
  endif
  cellMask = erode(*(*(*pgs).pState).mask, replicate(1,31,31))
  cell = where(cellMask eq 1)

  for k=0,(*pgs).stackNum-1 do begin
    for i=0,(*(*pgs).pState).ts-1 do begin    
      frame = (*(*(*pgs).pState).img[k])[*,*,i]
      frame_cell = frame[cell]
;      cellMin = min(frame_cell, max=cellMax)
;      cellMin = fix(cellMin) & cellMax = round(cellMax+0.499)
;      cellHist = histogram(frame_cell, min=cellMin)
;      cellX = findgen(n_elements(cellHist)) + cellMin
;      cellHistMax = max(cellHist, maxPos)
;      A = [(cellHist[maxPos]), cellX[maxPos], 3]
;      gaussPara = GaussFit(cellX[(maxPos-5)>0:maxPos+5],cellHist[(maxPos-5)>0:maxPos+5],A,estimates=A,nterms=3)
;      fitHist = A[0]*exp(-(cellX-A[1])^2/2/A[2]^2)
;      valid = frame_cell[where(frame_cell ge (A[1]-2*abs(A[2])) and frame_cell le (A[1]+2*abs(A[2])))]
      m1 = mean(frame_cell)
      s1 = stddev(frame_cell)
      valid = frame_cell[where(frame_cell ge (m1-2*s1) and frame_cell le (m1+2*s1))]
      (*(*pgs).cellSignalMask[k])[i] = mean(valid)
    endfor
  endfor
  (*pgs).cellSignal = (*pgs).cellSignalMask ;& print, *(*pgs).cellSignal[0], format='(f0.2)'
  Plotcellsignal, pgs
  for k=0,(*pgs).stackNum-1 do $
    *(*pgs).normCellSignal[k] = ((*(*pgs).cellSignal[k])[0] eq 0) ? $ 
      0 : *(*pgs).cellSignal[k]/(*(*pgs).cellSignal[k])[0]
end

;-----------------------------------------------------------------
Pro GetROIAverage, pgs
COMPILE_OPT STRICTARR

  for k=0,(*pgs).stackNum-1 do *(*pgs).cellSignalROI[k] = fltarr((*(*pgs).pState).ts)
  N = total((*pgs).ROIArray)
  for ROINo=0,n_elements((*pgs).ROIArray)-1 do begin
    if (*pgs).ROIArray[ROINo] eq 0b then continue
    for i =0,(*(*pgs).pState).ts-1 do begin
      for k=0,(*pgs).stackNum-1 do begin
        frame = reform((*(*(*pgs).pState).img[k])[*,*,i])
        (*(*pgs).cellSignalROI[k])[i] += mean(frame[*(*pgs).ROIMask[ROINo]])
      endfor
    endfor
  endfor
  for k=0,(*pgs).stackNum-1 do begin
    *(*pgs).cellSignalROI[k] /= N
    *(*pgs).normCellSignal[k] = ((*(*pgs).cellSignalROI[k])[0] eq 0) ? $
        0 : *(*pgs).cellSignalROI[k]/(*(*pgs).cellSignalROI[k])[0]
  endfor
  
End

;-----------------------------------------------------------------
pro OnSelectROI, Event
COMPILE_OPT STRICTARR

  widget_control, event.top, get_uvalue=pgs
  if event.select eq 0 then begin
    widget_control, (*pgs).ROIBase, sensitive=0
    return
  endif  
  
  widget_control, (*pgs).ROIBase, sensitive=1
  if (*(*pgs).cellSignalROI[0])[0] gt 0 then begin
    (*pgs).cellSignal = (*pgs).cellSignalROI
    Plotcellsignal, pgs
    return
  endif
  
  if total((*pgs).ROIArray) lt 1 then begin
    void = dialog_message('No ROI is selected!' ,/error)
    Plotcellsignal, pgs, /error
    return
  endif
  GetROIAverage, pgs
  (*pgs).cellSignal = (*pgs).cellSignalROI
  Plotcellsignal, pgs
  
end

;-----------------------------------------------------------------
pro OnSetROI, Event
COMPILE_OPT STRICTARR

  widget_control, event.top, get_uvalue=pgs
  
  ROINo = where((*pgs).ROICheck eq event.id)
  (*pgs).ROIArray[ROINo] = event.select eq 1
  if total((*pgs).ROIArray) lt 1 then begin
    void = dialog_message('No ROI is selected!' ,/error)
    Plotcellsignal, pgs, /error
    return
  endif
  GetROIAverage, pgs
  (*pgs).cellSignal = (*pgs).cellSignalROI
  Plotcellsignal, pgs
  
end

;-----------------------------------------------------------------
Pro SetParaTable, pgs
COMPILE_OPT STRICTARR

  para = [[0.6,-0.001,1.0,0.6,-0.01,50.0,0.5],[replicate(0.0,7)]]
  for k=0,(*pgs).stackNum-1 do begin
    widget_control, (*pgs).ParaTable, use_table_select=[0,2*k,6,2*k+1],set_value=para
  endfor  

end

;-----------------------------------------------------------------
Pro OnSelectFrame, Event
COMPILE_OPT STRICTARR

  widget_control, event.top, get_uvalue=pgs
  select = widget_info((*pgs).frameTable,/table_select)
  PlotCellSignal, pgs, select=[select[1],select[3]]
  
End

;-----------------------------------------------------------------
pro OnSetStart, Event
COMPILE_OPT STRICTARR

  widget_control, event.top, get_uvalue=pgs
  
  select = widget_info((*pgs).frameTable,/table_select)
  if select[1] gt 0 then begin
    widget_control, (*pgs).frameTable, use_table_select=[1,0,1,select[1]-1], set_value= replicate('No',1,select[1])
    (*pgs).frameStatus[0:select[1]-1] = 0b
    *(*pgs).validframe = where((*pgs).frameStatus eq 1b)
    Plotcellsignal, pgs
  endif
end

;-----------------------------------------------------------------
pro OnSetEnd, Event
COMPILE_OPT STRICTARR

  widget_control, event.top, get_uvalue=pgs
  
  select = widget_info((*pgs).frameTable,/table_select)
  if select[3] lt (*(*pgs).pState).ts-1 then begin
    widget_control, (*pgs).frameTable, use_table_select=[1,select[3]+1,1,(*(*pgs).pState).ts-1], set_value= replicate('No',1,select[1])
    (*pgs).frameStatus[select[3]+1:(*(*pgs).pState).ts-1] = 0b
    *(*pgs).validframe = where((*pgs).frameStatus eq 1b)
    Plotcellsignal, pgs
  endif
end

;-----------------------------------------------------------------
pro OnSetValid, Event
COMPILE_OPT STRICTARR

  widget_control, event.top, get_uvalue=pgs
  
  select = widget_info((*pgs).frameTable,/table_select)
  widget_control, (*pgs).frameTable, use_table_select=[1,select[1],1,select[3]] $
    , set_value= replicate('Yes',1,select[3]-select[1]+1)
  (*pgs).frameStatus[select[1]:select[3]] = 1b
  *(*pgs).validframe = where((*pgs).frameStatus eq 1b)
  Plotcellsignal, pgs
end

;-----------------------------------------------------------------
pro OnSetInvalid, Event
COMPILE_OPT STRICTARR

  widget_control, event.top, get_uvalue=pgs
  
  select = widget_info((*pgs).frameTable,/table_select)
  widget_control, (*pgs).frameTable, use_table_select=[1,select[1],1,select[3]] $
    , set_value= replicate('No',1,select[3]-select[1]+1)
  (*pgs).frameStatus[select[1]:select[3]] = 0b
  *(*pgs).validframe = where((*pgs).frameStatus eq 1b)
  Plotcellsignal, pgs
end

;-----------------------------------------------------------------
pro OnResetFrameTable, Event
COMPILE_OPT STRICTARR

  widget_control, event.top, get_uvalue=pgs
  widget_control, (*pgs).frameTable, use_table_select=[1,0,1,(*(*pgs).pState).ts-1] $
      , set_value= replicate('Yes',1,(*(*pgs).pState).ts)
  (*pgs).frameStatus[*] = 1b
  *(*pgs).validframe = where((*pgs).frameStatus eq 1b)
  Plotcellsignal, pgs
end

;-----------------------------------------------------------------
pro OnDone, Event, selection=selection
COMPILE_OPT STRICTARR

  widget_control, event.top, get_uvalue=pgs
  widget_control, (*pgs).CellSignalBase, sensitive=0
  widget_control,(*pgs).tabBase,  set_tab_current=1
  widget_control, (*pgs).FitBase, sensitive=1
  if ~keyword_set(selection) then begin
    SetParaTable, pgs
    (*pgs).degradation = (*pgs).ExpFitSignal
    GetExpFitSignal, pgs
  endif else begin
    case selection of
    'Double Exp Fit': begin
      SetParaTable, pgs
      (*pgs).degradation = (*pgs).ExpFitSignal
      GetExpFitSignal, pgs
    end
    'Filtration': begin
      (*pgs).degradation = (*pgs).FiltrationSignal
      GetFiltrationSignal, pgs
    end
    'Polynomial Fit': begin
      (*pgs).degradation = (*pgs).PolyFitSignal
      GetPolyFitSignal, pgs
    end
    else: begin
      SetParaTable, pgs
      (*pgs).degradation = (*pgs).ExpFitSignal
      GetExpFitSignal, pgs
    endelse
    endcase
  endelse
end

;-----------------------------------------------------------------
pro OnCancel, Event
COMPILE_OPT STRICTARR

  widget_control, event.top,/destroy  
end

;-----------------------------------------------------------------
Pro PlotDegradation, pgs, k
COMPILE_OPT STRICTARR

device, decomposed=1

  wset, (*pgs).winArray[k]
  plot, *(*(*pgs).pState).correctedTime, *(*pgs).normCellSignal[k], psym=4, $
    title='Channel '+strtrim(string(k),1), background='FFFFFF'XL, color='000000'XL, $
    /ynozero
  oplot,*(*(*pgs).pState).correctedTime, *(*pgs).degradation[k],color='0000FF'XL, $
    thick=2
;  print, *(*pgs).normCellSignal[k], format='(f0.2)'
;  print, *(*pgs).degradation[k], format='(f0.2)'
end

;-----------------------------------------------------------------
Pro GetFiltrationSignal, pgs
COMPILE_OPT STRICTARR

  for k=0,(*pgs).stackNum-1 do begin    
    y = (*(*pgs).normCellSignal[k])[*(*pgs).validFrame]
    if n_elements(*(*pgs).validFrame) lt  (*(*pgs).pState).ts then begin
      x = (*(*(*pgs).pState).correctedTime)[*(*pgs).validFrame]
      Y2 = SPL_INIT(X, Y)
      x2 = *(*(*pgs).pState).correctedTime
      y = SPL_INTERP(X, Y, Y2, X2)
    endif
    if (*pgs).MedianWidth gt 1 then y = median(y,(*pgs).MedianWidth)
    if (*pgs).SmoothWidth gt 1 then y = smooth(y,(*pgs).SmoothWidth,edge=1)
    *(*pgs).fultrationSignal[k] = y
    PlotDegradation, pgs, k
  endfor
  
end

;-----------------------------------------------------------------
Pro DoubleExpFitFunction, X, A, F, pder
COMPILE_OPT STRICTARR

;Y=A[0]*exp(A[1]*(X-A[2])^2)+A[3]*exp(A[4]*(X-A[5])^2)+A[6]

  ax=exp(A[1]*(X-A[2])^2) & bx=exp(A[4]*(X-A[5])^2)
  F=A[0]*ax+A[3]*bx+A[6]
  if N_PARAMS() ge 4 then pder=[[ax],[A[0]*ax*(X-A[2])^2],[A[0]*ax*(X-A[2])*A[1]*(-2.0)], $
      [bx],[A[3]*bx*(X-A[5])^2],[A[3]*bx*(X-a[5])*A[4]*(-2.0)],[replicate(1.0, n_elements(X))]]

end

;-----------------------------------------------------------------
Function DoubleExp, X, A
COMPILE_OPT STRICTARR

  ax=exp(A[1]*(X-A[2])^2) & bx=exp(A[4]*(X-A[5])^2)
  F=A[0]*ax+A[3]*bx+A[6]
  return, F
end
;-----------------------------------------------------------------
Pro GetExpFitSignal, pgs
COMPILE_OPT STRICTARR

  validX = (*(*(*pgs).pState).correctedTime)[*(*pgs).validFrame]
  X = *(*(*pgs).pState).correctedTime
  for k=0,(*pgs).stackNum-1 do begin
    validY = (*(*pgs).normCellSignal[k])[*(*pgs).validFrame]
    weights = 1.0/validY
    widget_control, (*pgs).paraTable, use_table_select=[0,2*k,6,2*k], get_value=A
    validYFit = curvefit(validX,validY,weights,A,function_name='DoubleExpFitFunction',/double,ITMAX=50,status=s)
    *(*pgs).ExpFitSignal[k] = DoubleExp(X,A)
    widget_control, (*pgs).paraTable, use_table_select=[0,2*k+1,6,2*k+1], set_value=A
    PlotDegradation, pgs, k
  endfor
  
end

;-----------------------------------------------------------------
Pro GetPolyFitSignal, pgs
COMPILE_OPT STRICTARR

  validX = (*(*(*pgs).pState).correctedTime)[*(*pgs).validFrame]
  validXArray = replicate(1.0,n_elements(validX))  
  for i=1,(*pgs).PolyOrder do validXArray = [[validXArray],[validX^i]]
  
  if n_elements(*(*pgs).validFrame) eq n_elements(*(*(*pgs).pState).correctedTime) then $
    XArray = validXArray else begin
    X = *(*(*pgs).pState).correctedTime
    XArray = replicate(1.0,n_elements(X))
    for i=1,(*pgs).PolyOrder do XArray = [[XArray],[X^i]]
  endelse
  
  regularArray = validXArray##transpose(validXArray)
  LUDC, regularArray, INDEX
  for k=0,(*pgs).stackNum-1 do begin
    validY = transpose((*(*pgs).normCellSignal[k])[*(*pgs).validFrame])
    B = validXArray##validY    
    A = LUSOL(regularArray, INDEX, reform(B))
    *(*pgs).PolyFitSignal[k] = transpose(XArray)##A
    PlotDegradation, pgs, k
  endfor

end

;-----------------------------------------------------------------
pro OnSelectFit, Event
COMPILE_OPT STRICTARR

  widget_control, event.top, get_uvalue=pgs
  
  case event.index of
  1 : begin     ; filtration and interplate
    widget_control, (*pgs).FiltrationBase, sensitive=1
    widget_control, (*pgs).ExpFitBase, sensitive=0
    widget_control, (*pgs).PolyFitBase, sensitive=0
    (*pgs).degradation = (*pgs).fultrationSignal
    GetFiltrationSignal, pgs
  end
  0 : begin     ; Double Exp fit
    widget_control, (*pgs).FiltrationBase, sensitive=0
    widget_control, (*pgs).ExpFitBase, sensitive=1
    widget_control, (*pgs).PolyFitBase, sensitive=0
    (*pgs).degradation = (*pgs).ExpFitSignal
    GetExpFitSignal, pgs
  end
  2 : begin     ; ploynomial fit
    widget_control, (*pgs).FiltrationBase, sensitive=0
    widget_control, (*pgs).ExpFitBase, sensitive=0
    widget_control, (*pgs).PolyFitBase, sensitive=1
    (*pgs).degradation = (*pgs).PolyFitSignal
    GetPolyFitSignal, pgs
  end
  else:
  endcase
  
end

;-----------------------------------------------------------------
pro OnChangeMedianWidth, Event 
COMPILE_OPT STRICTARR

  widget_control, event.top, get_uvalue=pgs  
  (*pgs).MedianWidth = 1*event.index+1
  GetFiltrationSignal, pgs
end

;-----------------------------------------------------------------
pro OnChangeSmoothWidth, Event  
COMPILE_OPT STRICTARR

  widget_control, event.top, get_uvalue=pgs
  (*pgs).SmoothWidth = 1*event.index+1
  GetFiltrationSignal, pgs
end

;-----------------------------------------------------------------
pro OnChangePara, Event
COMPILE_OPT STRICTARR

  widget_control, event.top, get_uvalue=pgs
  GetExpFitSignal, pgs  
end

;-----------------------------------------------------------------
pro OnResetParaTable, Event
COMPILE_OPT STRICTARR

  widget_control, event.top, get_uvalue=pgs
  SetParaTable, pgs
  GetExpFitSignal, pgs
end

;-----------------------------------------------------------------
pro OnChangePolyOrder, Event
COMPILE_OPT STRICTARR

  widget_control, event.top, get_uvalue=pgs
  (*pgs).PolyOrder = event.value
  GetPolyFitSignal, pgs
end

;-----------------------------------------------------------------
pro OnCorrection, Event
COMPILE_OPT STRICTARR

  widget_control, event.top, get_uvalue=pgs
  (*pgs).IsCorrection = 1b
  
  widget_control, /hourglass
;  maskRegion = where(*(*(*pgs).pState).mask eq 1)
;  if maskRegion[0] eq -1 then maskRegion = lindgen((*(*pgs).pState).xs*(*(*pgs).pState).ys)
  for k=0,(*pgs).stackNum-1 do begin
;  for k=1,(*pgs).stackNum-1 do begin
    for i=0,(*(*pgs).pState).ts-1 do begin
      frame = reform((*(*(*pgs).pState).img[k])[*,*,i])
;      frame[maskRegion] /= (*(*pgs).degradation[k])[i]
;      (*(*(*pgs).pState).img[k])[*,*,i] = frame
      (*(*(*pgs).pState).img[k])[*,*,i] = frame/(*(*pgs).degradation[k])[i]
    endfor
  endfor
  widget_control, event.top,/destroy
  
end
Pro PhotobleachCorrection_Event, Event
COMPILE_OPT STRICTARR


End

Pro PhotobleachCorrection_CleanUp, pgs
COMPILE_OPT STRICTARR

  ptr_free, (*pgs).validFrame,(*pgs).fitFrame,(*pgs).cellSignalMask, $
    (*pgs).cellSignalROI,(*pgs).NormCellSignal,(*pgs).fultrationSignal, $
    (*pgs).ExpFitSignal,(*pgs).PolyFitSignal
End

Function PhotobleachCorrection, pState, GROUP_LEADER=wGroup, selection=selection, auto=auto
COMPILE_OPT STRICTARR

;  Resolve_Routine, 'PhotobleachCorrection_eventcb',/COMPILE_FULL_FILE  ; Load event callback routines

  WID_BASE_0 = Widget_Base( GROUP_LEADER=wGroup, UNAME='WID_BASE_0'  $
      ,XOFFSET=100 ,YOFFSET=200 ,SCR_XSIZE=850 ,SCR_YSIZE=477, tlb_frame_attr=9  $
      ,TITLE='Photobleach Correction' ,SPACE=3 ,XPAD=3 ,YPAD=3,/modal)


  DrawBase = Widget_Base(WID_BASE_0, UNAME='DrawBase' ,FRAME=1 ,XPAD=0 ,YPAD=0  $
      ,XOFFSET=10 ,YOFFSET=15 ,SCR_XSIZE=417 ,SCR_YSIZE=417 ,/SCROLL ,SPACE=10 ,ROW=1  $
      ,XSIZE=400*(*pState).stackNum+10*((*pState).stackNum-1) ,YSIZE=400)
  winArray = lonarr((*pState).stackNum)
  for k=0,(*pState).stackNum-1 do begin
    WID_DRAW_0 = Widget_Draw(DrawBase, UNAME='WID_DRAW_0' ,FRAME=1  $
      ,SCR_XSIZE=400 ,SCR_YSIZE=400 ,RETAIN=2)
    winArray[k] = WID_DRAW_0
  endfor


  WID_TAB_0 = Widget_Tab(WID_BASE_0, UNAME='WID_TAB_0' ,FRAME=1  $
      ,XOFFSET=440 ,YOFFSET=15 ,SCR_XSIZE=380 ,SCR_YSIZE=417)


  CellSignalBase = Widget_Base(WID_TAB_0, UNAME='CellSignalBase' ,XPAD=10 ,YPAD=10 ,ROW=1  $
      ,SCR_XSIZE=370 ,SCR_YSIZE=389 ,/ALIGN_CENTER ,TITLE='Cell Signal' ,SPACE=10)

  WID_BASE_1 = Widget_Base(CellSignalBase, UNAME='WID_BASE_1'  $
      ,XOFFSET=10 ,YOFFSET=10 ,/BASE_ALIGN_CENTER ,SPACE=5 ,COLUMN=1)
  WID_LABEL_0 = Widget_Label(WID_BASE_1, UNAME='WID_LABEL_0'  $
      ,/ALIGN_LEFT ,VALUE='Get cell signal from:')

  WID_BASE_2 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_2' ,XOFFSET=2  $
      ,YOFFSET=20 ,ROW=1 ,/EXCLUSIVE)
  Mask_Button = Widget_Button(WID_BASE_2, UNAME='Mask_Button'  $
      ,/ALIGN_LEFT ,VALUE='Mask' ,event_pro='OnSelectMask')
  ROI_Button = Widget_Button(WID_BASE_2, UNAME='ROI_Button'  $
      ,XOFFSET=56 ,/ALIGN_LEFT ,VALUE='ROI' ,event_pro='OnSelectROI')


  ROIBase = Widget_Base(WID_BASE_1 ,FRAME=1 ,YOFFSET=51 ,SCR_XSIZE=110 $
      ,SCR_YSIZE=84 ,SENSITIVE=0 ,COLUMN=2 ,/NONEXCLUSIVE)
  ROIArray = [0b] & ROICheck = [-1] & ROIMask = ptrarr(1)
  if obj_valid((*pState).result) then begin
  if (*pState).result->length() gt 0 then begin
    ROINum = (*pState).result->length()
    ROIArray = bytarr(ROINum)
    ROICheck = lonarr(ROINum)
    ROIMask = ptrarr(ROINum)
    for k=0,ROINum-1 do begin
      ROI = Widget_Button(ROIBase ,SCR_XSIZE=50 ,SCR_YSIZE=25 $
        ,/ALIGN_LEFT ,VALUE='ROI '+strtrim(string(k+1),1) ,event_pro='OnSetROI')
      widget_control, ROI, /set_button
      ROIArray[k] = 1b & ROICheck[k] = ROI
      info = (*pState).result->GetProperty(k, oROI=currentROI)
      Mask = currentROI->ComputeMask(dimensions=[(*pState).xs,(*pState).ys])
      indices = where(Mask gt 0)
      ROIMask[k] = ptr_new(indices>0,/no_copy)
    endfor
  endif
  endif


  WID_BASE_4 = Widget_Base(WID_BASE_1,XOFFSET=10 ,YOFFSET=140 ,SPACE=6 ,XPAD=5 ,COLUMN=1)
  WID_BUTTON_3 = Widget_Button(WID_BASE_4, UNAME='WID_BUTTON_3'  $
      ,XOFFSET=5 ,SCR_XSIZE=80 ,SCR_YSIZE=22 ,/ALIGN_CENTER  $
      ,VALUE='Set As Start' ,event_pro='OnSetStart')
  WID_BUTTON_4 = Widget_Button(WID_BASE_4, UNAME='WID_BUTTON_4'  $
      ,XOFFSET=5 ,YOFFSET=28 ,SCR_XSIZE=80 ,SCR_YSIZE=22  $
      ,/ALIGN_CENTER ,VALUE='Set As End' ,event_pro='OnSetEnd')
  WID_BUTTON_5 = Widget_Button(WID_BASE_4, UNAME='WID_BUTTON_5'  $
      ,XOFFSET=5 ,YOFFSET=56 ,SCR_XSIZE=80 ,SCR_YSIZE=22  $
      ,/ALIGN_CENTER ,VALUE='Set As Valid' ,event_pro='OnSetValid')
  WID_BUTTON_6 = Widget_Button(WID_BASE_4, UNAME='WID_BUTTON_6'  $
      ,XOFFSET=5 ,YOFFSET=84 ,SCR_XSIZE=80 ,SCR_YSIZE=22  $
      ,/ALIGN_CENTER ,VALUE='Set As Invalid' ,event_pro='OnSetInvalid')
  WID_BUTTON_7 = Widget_Button(WID_BASE_4, UNAME='WID_BUTTON_7'  $
      ,XOFFSET=5 ,YOFFSET=112 ,SCR_XSIZE=80 ,SCR_YSIZE=22  $
      ,/ALIGN_CENTER ,VALUE='Rset' ,event_pro='OnResetFrameTable')
  WID_BUTTON_12 = Widget_Button(WID_BASE_4, UNAME='WID_BUTTON_12'  $
      ,XOFFSET=5 ,YOFFSET=140 ,SCR_XSIZE=80 ,SCR_YSIZE=22  $
      ,/ALIGN_CENTER ,VALUE='Cancel' ,event_pro='OnCancel')
   WID_BUTTON_11 = Widget_Button(WID_BASE_4, UNAME='WID_BUTTON_11'  $
      ,XOFFSET=5 ,YOFFSET=122 ,SCR_XSIZE=80 ,SCR_YSIZE=22  $
      ,/ALIGN_CENTER ,VALUE='Done' ,event_pro='OnDone')

  FrameTable = Widget_Table(CellSignalBase, UNAME='FrameTable' ,XOFFSET=130 ,YOFFSET=10 $
      ,XSIZE=2 ,YSIZE=(*pState).ts ,X_SCROLL_SIZE=2 ,Y_SCROLL_SIZE=18, /no_row_headers, $
      column_labels=['Frame', 'Valid'], frame=1, /all_events, event_pro='OnSelectFrame')


  FitBase = Widget_Base(WID_TAB_0, UNAME='FitBase' ,SCR_XSIZE=370  $
      ,SCR_YSIZE=389 ,TITLE='Correction' ,SPACE=3 ,XPAD=3 ,YPAD=3, sensitive=0)

  WID_BASE_5 = Widget_Base(FitBase,XOFFSET=10,YOFFSET=9 ,SCR_XSIZE=320 $
      ,SCR_YSIZE=33 ,SPACE=3 ,XPAD=3 ,YPAD=3)
  WID_LABEL_1 = Widget_Label(WID_BASE_5, UNAME='WID_LABEL_1' ,YOFFSET=7 $
      ,/ALIGN_LEFT ,VALUE='Estimate degradation function by')
  FitList = Widget_Droplist(WID_BASE_5, UNAME='FitList' ,XOFFSET=160 ,YOFFSET=5 ,event_pro='OnSelectFit' $
      ,VALUE=['Double Exp Fit', 'Filtration & Interplotation', 'Polynomial Fit' ])

  FiltrationBase = Widget_Base(FitBase,FRAME=1 ,XOFFSET=10 ,YOFFSET=50 $
      ,SCR_XSIZE=345 ,SCR_YSIZE=35 ,SPACE=3 ,XPAD=3 ,YPAD=3, sensitive=0)
  WID_LABEL_2 = Widget_Label(FiltrationBase ,XOFFSET=60 ,YOFFSET=7 ,/ALIGN_LEFT ,VALUE='Median width')
  MedianWidthList = Widget_Droplist(FiltrationBase, UNAME='MedianWidthList' ,XOFFSET=130 ,YOFFSET=5 $
      ,SCR_XSIZE=35 ,SCR_YSIZE=22 ,VALUE=[ '1', '3', '5', '7', '9' ] ,event_pro='OnChangeMedianWidth')
  WID_LABEL_3 = Widget_Label(FiltrationBase ,XOFFSET=195 ,YOFFSET=7 ,/ALIGN_LEFT ,VALUE='Smooth width')
  SmoothWidthList = Widget_Droplist(FiltrationBase ,UNAME='SmoothWidthList' ,XOFFSET=265 ,YOFFSET=5 $
      ,SCR_XSIZE=35 ,SCR_YSIZE=22 ,VALUE=[ '1', '3', '5', '7', '9' ] ,event_pro='OnChangeSmoothWidth')
  WID_LABEL_4 = Widget_Label(FiltrationBase ,XOFFSET=5 ,YOFFSET=7 ,/ALIGN_LEFT ,VALUE='Filtration :')

  ExpFitBase = Widget_Base(FitBase,FRAME=1 ,XOFFSET=10 ,YOFFSET=95 $
      ,SCR_XSIZE=345 ,SCR_YSIZE=150 ,SPACE=3 ,XPAD=3 ,YPAD=3, sensitive=1)
  WID_LABEL_5 = Widget_Label(ExpFitBase ,XOFFSET=5 ,YOFFSET=5 ,/ALIGN_LEFT $
      ,VALUE='Double exp fit : y=a0*exp(a1(x-a2)^2)+a3*exp(a4(x-a5)^2)+a6')
  WID_BUTTON_1 = Widget_Button(ExpFitBase, XOFFSET=300 ,YOFFSET=1 ,SCR_XSIZE=40 ,SCR_YSIZE=20  $
      ,/ALIGN_CENTER ,VALUE='Reset' ,event_pro='OnResetParaTable')
  Row_Labels = ['Ch1-Init','Ch1-Fit']
  if (*pState).stackNum gt 1 then begin
    for k=1,(*pState).stackNum-1 do begin
      Row_labels = [Row_Labels,'Ch'+strtrim(string(k+1),1)+'-Init','Ch'+strtrim(string(k+1),1)+'-Fit']
    endfor
  endif
  ParaTable = Widget_Table(ExpFitBase ,YOFFSET=30 ,SCR_XSIZE=345 ,SCR_YSIZE=110 ,/EDITABLE, Alignment=1  $
      ,Column_Labels=['a0','a1','a2','a3','a4','a5','a6'] ,ROW_LABELS=Row_labels ,XSIZE=7 ,YSIZE=2*(*pState).stackNum $
      ,X_SCROLL_SIZE=2 ,Y_SCROLL_SIZE=4 ,event_pro='OnChangePara' ,Column_widths=replicate(60,7))

  PolyFitBase = Widget_Base(FitBase ,FRAME=1 ,XOFFSET=10 ,YOFFSET=255 $
      ,SCR_XSIZE=345 ,SCR_YSIZE=50 ,SPACE=3 ,XPAD=3 ,YPAD=3, sensitive=0)
  WID_LABEL_6 = Widget_Label(PolyFitBase ,XOFFSET=5 ,YOFFSET=13 ,/ALIGN_LEFT ,VALUE='Polynomial fit :')
  WID_LABEL_7 = Widget_Label(PolyFitBase ,XOFFSET=80 ,YOFFSET=13 ,/ALIGN_LEFT ,VALUE='Polynomial order')
  PolySlider = Widget_Slider(PolyFitBase, UNAME='PolySlider' ,XOFFSET=170 ,SCR_XSIZE=100 $
      ,SCR_YSIZE=30 ,MINIMUM=3 ,MAXIMUM=8 ,VALUE=4 ,event_pro='OnChangePolyOrder')

  WID_BASE_9 = Widget_Base(FitBase, UNAME='WID_BASE_9' ,XOFFSET=10  $
      ,YOFFSET=315 ,SCR_XSIZE=345 ,SCR_YSIZE=40 ,SPACE=3 ,XPAD=3 ,YPAD=3)
  WID_BUTTON_10 = Widget_Button(WID_BASE_9, UNAME='WID_BUTTON_10'  $
      ,XOFFSET=210 ,YOFFSET=5 ,SCR_XSIZE=80 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Cancel' ,event_pro='OnCancel')
  WID_BUTTON_11 = Widget_Button(WID_BASE_9, UNAME='WID_BUTTON_11'  $
      ,XOFFSET=40 ,YOFFSET=5 ,SCR_XSIZE=150 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Procede With Correction' ,event_pro='OnCorrection')

  widget_control, frameTable, use_table_select=[0,0,0,(*pState).ts-1], set_value=indgen(1,(*pState).ts)
  widget_control, frameTable, use_table_select=[1,0,1,(*pState).ts-1], set_value=replicate('Yes',1,(*pState).ts)
  widget_control, MedianWidthList, set_droplist_select = 1
  widget_control, SmoothWidthList, set_droplist_select = 1
  Widget_Control, /REALIZE, WID_BASE_0
  for k=0,(*pState).stackNum-1 do begin
    widget_control, winArray[k], get_value=win
    winArray[k] = win
  endfor

  gs = { IsCorrection: 0b, $
      pState: pState, $
      stackNum: (*pState).stackNum, $
      tabBase: wid_tab_0, $
      CellSignalBase: CellSignalBase, $
      FitBase: FitBase, $
      winArray: winArray, $
      ROIBase: ROIBase, $
      ROIArray: ROIArray, $
      ROICheck: ROICheck, $
      ROIMask: ROIMask, $
      FrameTable: FrameTable, $
      FitList: FitList, $
      FiltrationBase: FiltrationBase, $
      MedianWithList: MedianWidthList, $
      SmoothWidthList: SmoothWidthList, $
      ExpFitBase: ExpFitBase, $
      paraTable: paraTable, $
      PolyFitBase: PolyFitBase, $
      validFrame: ptr_new(lindgen((*pState).ts)), $
      FrameStatus: replicate(1b,(*pState).ts), $
      MedianWidth: 3, $
      SmoothWidth: 3, $
      ExpPara: dblarr(7,(*pState).stackNum), $
      PolyOrder: 4, $
      fitFrame: ptr_new(fltarr((*pState).ts)), $
      cellSignalMask: ptrarr((*pState).stackNum), $
      cellSignalROI: ptrarr((*pState).stackNum), $
      cellSignal: ptrarr((*pState).stackNum), $
      NormCellSignal: ptrarr((*pState).stackNum), $
      fultrationSignal: ptrarr((*pState).stackNum), $
      ExpFitSignal: ptrarr((*pState).stackNum), $
      PolyFitSignal: ptrarr((*pState).stackNum), $
      degradation : ptrarr((*pState).stackNum) }

  for k=0,(*pState).stackNum-1 do begin
    gs.cellSignalMask[k] = ptr_new(fltarr((*pState).ts))
    gs.cellSignalROI[k] = ptr_new(fltarr((*pState).ts))
    gs.cellSignal[k] = ptr_new(fltarr((*pState).ts))
    gs.NormCellSignal[k]= ptr_new(fltarr((*pState).ts))
    gs.fultrationSignal [k]= ptr_new(fltarr((*pState).ts))
    gs.ExpFitSignal[k]= ptr_new(fltarr((*pState).ts))
    gs.PolyFitSignal[k]= ptr_new(fltarr((*pState).ts))
    gs.degradation[k] = ptr_new(fltarr((*pState).ts))
  endfor
  pgs = ptr_new(gs, /no_copy)

  widget_control, WID_BASE_0, set_uvalue=pgs
  
  if keyword_set(auto) then begin
    fake_event = {top:WID_BASE_0, select:1b}
    OnSelectMask, fake_event
    OnDone, fake_event, selection=selection
    OnCorrection, fake_event
    IsCorrection = (*pgs).IsCorrection
    PhotobleachCorrection_CleanUp, pgs
    return, IsCorrection    
  endif
  
  XManager, 'PhotobleachCorrection', WID_BASE_0, /NO_BLOCK

  IsCorrection = (*pgs).IsCorrection
  PhotobleachCorrection_CleanUp, pgs
  return, IsCorrection
end
