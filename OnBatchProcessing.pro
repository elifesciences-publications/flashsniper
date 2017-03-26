;-----------------------------------------------------------------
Pro OnBatchProcessing_Modify_previous_result, Event
;Pro OnBatchProcessing, Event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF
; modify old result , add new paras to *.sav
  path = 'E:\My Documents\Data for Work\Flash Sniper\for detection\'
  savefiles = dialog_pickfile(path=path,get_path=path,filter='*.sav',/multiple_files)
  if savefiles[0] eq '' then return

  for fileNo=0, n_elements(savefiles)-1 do begin
    OnLoadStatus, Event, savefile=savefiles[fileNo], /silent
    OnCommand_ModifyPreviousArea, Event
    OnSaveStatus, Event, /silent
    OnExportResult, Event
  endfor
End

;-----------------------------------------------------------------
Pro OnBatchProcessing_validation_old, Event
;Pro OnBatchProcessing, Event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

; validation, from original synthetic data

  path = 'E:\My Documents\Data for Work\Flash Sniper\New Synthetic Data\eYFP-15ul-72h-cellC14-NaClO1mM\'
  files = dialog_pickfile(path=path,get_path=path,filter='*.dat',/multiple_files)
  if files[0] eq '' then return
  files = files[sort(files)]
  flashSitesMap = dialog_pickfile(path=path,filter='*.tif')
  while flashSitesMap[0] eq '' do flashSitesMap = dialog_pickfile(path=path,filter='*.tif')
  flashSites = read_tiff(flashSitesMap)

  thr = [4,4.6,5.2,5.8]
  validation = [-1,-1,-1,-1]
  close,10 & openw,10,path+'\total validation result.txt',width=1000,/append
  printf,10,'file', ' Nd', ' Nt', ' peak', ' area'
  for fileNo=0, n_elements(files)-1 do begin
    output = file_basename(files[fileNo])
    OnOpen, Event, file=files[fileNo] ,/silent
    OnPhotobleachCorrection, Event, selection='Polynomial Fit'
    close,11 & openw,11,files[fileNo]+'-validation result.txt'
    printf,11,thr
    printf,11,' Nd', ' Nt', ' peak', ' area'
    for thrNo=0,n_elements(thr)-1 do begin
      OnAutoDetection, Event,thr=thr[thrNo],validation=validation,flashSites=flashSites
      printf,11, validation
      printf,11, ' '
      Nd = n_elements(validation)/4
      Nt = total(long(validation[1,*]))
      trueSites = where(validation[1,*] eq 1)
      peak = (trueSites[0] eq -1) ? -1 : mean(validation[2,trueSites])
      area = (trueSites[0] eq -1) ? -1 : mean(validation[3,trueSites])
      output = [output,string([Nd,Nt,peak,area])]
    endfor
    close,11
    printf,10,output
    OnSaveStatus, Event, savefile=files[fileNo], /silent
  endfor
  close,10
end
;-----------------------------------------------------------------
Pro OnBatchProcessing_sav, Event
;Pro OnBatchProcessing, Event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

; validation, from .sav file

  path = 'E:\My Documents\Data for Work\Flash Sniper\Synthetic Data\eYFP-15ul-72h-cellC14-NaClO1mM\'
  savefiles = dialog_pickfile(path=path,get_path=path,filter='*.sav',/multiple_files)
  if savefiles[0] eq '' then return
  saveFiles = saveFiles[sort(saveFiles)]
  flashSitesMap = dialog_pickfile(path=path,filter='*.tif')
  while flashSitesMap[0] eq '' do flashSitesMap = dialog_pickfile(path=path,filter='*.tif')
  flashSites = read_tiff(flashSitesMap)

  thr = [4,4.6,5.2,5.8]
  validation = [-1,-1,-1,-1]
  close,1 & openw,1,path+'\total validation result.txt',width=1000
  printf,1,'file', ' Nd', ' Nt', ' peak', ' area'
  for fileNo=0, n_elements(savefiles)-1 do begin
    OnLoadStatus, Event, savefile=savefiles[fileNo], /silent
    output = file_basename(savefiles[fileNo])
    for thrNo=0,n_elements(thr)-1 do begin
      OnAutoDetection, Event,thr=thr[thrNo],validation=validation,flashSites=flashSites
      print, fileNo, '    Threshold:',thr[thrNo]
      print, validation
      Nd = n_elements(validation)/4
      Nt = total(long(validation[1,*]))
      trueSites = where(validation[1,*] eq 1)
      peak = (trueSites[0] eq -1) ? -1 : mean(validation[2,trueSites])
      area = (trueSites[0] eq -1) ? -1 : mean(validation[3,trueSites])
      output = [output,string([Nd,Nt,peak,area])]
    endfor
    print,'OUTPUT:', output
    printf,1,output
    OnSaveStatus, Event, savefile=savefiles[fileNo], /silent
  endfor
  close,1

;  window,10,xs=900,ys=256
;  for k=0,99 do begin
;    tvscl,(*(*pState).ima)[k,*,*]
;    wait,0.1
;  endfor
end

;-----------------------------------------------------------------
Function GetRiseGroup1Trace, Event, ROINo
COMPILE_OPT STRICTARR
; export group3 trace, normalized and aligned by Tstart2
;flashPara [   0   ,  1  , 2 ,  3 ,   4   ,  5   , 6 ,  7  ,  8 ,    9   ,  10 ,  11 ,  12  , 13 , 14 ,  15 , 16 , 18, 19]
;flashPara [FlashNo, Peak, F0, Amp, Tstart, Tpeak, RT, RT90, T50, EndBase, Note, FAHM, dBase, T75, T90, FDHM, Dia, t1, t2]
  CATCH, Error_status
  IF Error_status NE 0 THEN BEGIN
    PRINT, '  Error message: ', !ERROR_STATE.MSG
    return,replicate(!values.F_NAN,61)
    CATCH, /CANCEL
  ENDIF
  widget_control,event.top,get_uvalue=pState

  if ~obj_valid((*pState).result) then begin
    print,'result does not exist!'
    return,replicate(!values.F_NAN,61)
  endif
  ROINum = (*pState).result->length()
  if ROINum eq 0 then begin
    print,'ROI number is zero!'
    return,replicate(!values.F_NAN,61)
  endif
  info = (*pState).result->GetProperty(ROINo-1,trace=trace,flashPara=flashPara,flashNum=flashNum)
  if ~ptr_valid(flashPara) then begin
    print,ROINo,' Error getting FlashPara!'
    return,replicate(!values.F_NAN,61)
  endif
  trace = reform(*trace[0])
  if (size(trace))[0] eq 2 then trace = reform(trace[0,*])
;  trace = smooth(trace,3,edge=1)
  Ts = (*flashPara)[4,0]
  Tp = (*flashPara)[5,0]
  F0 = (*flashPara)[2,0]
  Peak = (*flashPara)[1,0]
  void = min(abs(*(*pState).CorrectedTime-Tp),pPos)
  t1 = pPos-30
  t2 = pPos+30
  subtrace = trace[t1>0:t2<((*pState).ts-1)]
  subtrace = (subtrace-F0)/(Peak-F0)
  if t1 lt 0 then begin
    subtrace = [replicate(!values.F_NAN,-t1),subtrace]
  endif
  if t2 ge (*pState).ts then begin
    subtrace = [subtrace,replicate(!values.F_NAN,t2-(*pState).ts+1)]
  endif

  return, subtrace
End
;-----------------------------------------------------------------
Function GetRiseGroup1Ch2Trace, Event, ROINo
COMPILE_OPT STRICTARR
; export group3 trace, normalized and aligned by Tstart2
;flashPara [   0   ,  1  , 2 ,  3 ,   4   ,  5   , 6 ,  7  ,  8 ,    9   ,  10 ,  11 ,  12  , 13 , 14 ,  15 , 16 , 18, 19]
;flashPara [FlashNo, Peak, F0, Amp, Tstart, Tpeak, RT, RT90, T50, EndBase, Note, FAHM, dBase, T75, T90, FDHM, Dia, t1, t2]
  CATCH, Error_status
  IF Error_status NE 0 THEN BEGIN
    PRINT, '  Error message: ', !ERROR_STATE.MSG
    return,replicate(!values.F_NAN,61)
    CATCH, /CANCEL
  ENDIF
  widget_control,event.top,get_uvalue=pState

  if ~obj_valid((*pState).result) then begin
    print,'result does not exist!'
    return,replicate(!values.F_NAN,61)
  endif
  ROINum = (*pState).result->length()
  if ROINum eq 0 then begin
    print,'ROI number is zero!'
    return,replicate(!values.F_NAN,61)
  endif
  info = (*pState).result->GetProperty(ROINo-1,trace=trace,flashPara=flashPara,flashNum=flashNum)
  if ~ptr_valid(flashPara) then begin
    print,ROINo,' Error getting FlashPara!'
    return,replicate(!values.F_NAN,61)
  endif
  trace = reform(*trace[0])
  if (size(trace))[0] ne 2 then begin
    return,replicate(!values.F_NAN,61)
  endif
  trace = reform(trace[1,*]) ; get Ch2, the signal of TMRM/Calcein red
;  trace = smooth(trace,3,edge=1)
  Ts = (*flashPara)[4,0]
  Tp = (*flashPara)[5,0]
  F0 = trace[Ts]
  Peak = trace[Tp]
  void = min(abs(*(*pState).CorrectedTime-Tp),pPos)
  t1 = pPos-30
  t2 = pPos+30
  subtrace = trace[t1>0:t2<((*pState).ts-1)]
  subtrace = (subtrace-F0)/(Peak-F0)
  if t1 lt 0 then begin
    subtrace = [replicate(!values.F_NAN,-t1),subtrace]
  endif
  if t2 ge (*pState).ts then begin
    subtrace = [subtrace,replicate(!values.F_NAN,t2-(*pState).ts+1)]
  endif

  return, subtrace
End
;-----------------------------------------------------------------
Function GetRiseGroup3Trace, Event, ROINo
COMPILE_OPT STRICTARR
; export group3 trace, normalized and aligned by Tstart2
;flashPara [   0   ,  1  , 2 ,  3 ,   4   ,  5   , 6 ,  7  ,  8 ,    9   ,  10 ,  11 ,  12  , 13 , 14 ,  15 , 16 , 18, 19]
;flashPara [FlashNo, Peak, F0, Amp, Tstart, Tpeak, RT, RT90, T50, EndBase, Note, FAHM, dBase, T75, T90, FDHM, Dia, t1, t2]
  CATCH, Error_status
  IF Error_status NE 0 THEN BEGIN
    PRINT, '  Error message: ', !ERROR_STATE.MSG
    tempTrace = replicate(!values.F_NAN,61)
    return,{subtrace1:tempTrace,subTrace2:tempTrace,subTrace3:tempTrace}
    CATCH, /CANCEL
  ENDIF
  widget_control,event.top,get_uvalue=pState

  if ~obj_valid((*pState).result) then begin
    print,'result does not exist!'
    tempTrace = replicate(!values.F_NAN,61)
    return,{subtrace1:tempTrace,subTrace2:tempTrace,subTrace3:tempTrace}
  endif
  ROINum = (*pState).result->length()
  if ROINum eq 0 then begin
    print,'ROI number is zero!'
    tempTrace = replicate(!values.F_NAN,61)
    return,{subtrace1:tempTrace,subTrace2:tempTrace,subTrace3:tempTrace}
  endif
  info = (*pState).result->GetProperty(ROINo-1,trace=trace,flashPara=flashPara,flashNum=flashNum)
  if ~ptr_valid(flashPara) then begin
    print,ROINo,' Error getting FlashPara!'
    tempTrace = replicate(!values.F_NAN,61)
    return,{subtrace1:tempTrace,subTrace2:tempTrace,subTrace3:tempTrace}
  endif
  trace = reform(*trace[0])
  if (size(trace))[0] eq 2 then trace = reform(trace[0,*])
;  trace = smooth(trace,3,edge=1)
  F01 = (*flashPara)[2,0]
  F02 = (*flashPara)[2,1]
  P2 = (*flashPara)[1,1]
  Ts2 = (*flashPara)[4,1]
  Tp2 = (*flashPara)[5,1]
  void = min(abs(*(*pState).CorrectedTime-Ts2),sPos)
  t1 = sPos-30
  t2 = sPos+30
  subtrace = trace[t1>0:t2<((*pState).ts-1)]
  subtrace1 = (subtrace-F01)/(F02-F01)
  subtrace2 = (subtrace-F01)/(P2-F01)
  if t1 lt 0 then begin
    subtrace1 = [replicate(!values.F_NAN,-t1),subtrace1]
    subtrace2 = [replicate(!values.F_NAN,-t1),subtrace2]
  endif
  if t2 ge (*pState).ts then begin
    subtrace1 = [subtrace1,replicate(!values.F_NAN,t2-(*pState).ts+1)]
    subtrace2 = [subtrace2,replicate(!values.F_NAN,t2-(*pState).ts+1)]
  endif
  void = min(abs(*(*pState).CorrectedTime-Tp2),pPos)
  t1 = pPos-30
  t2 = pPos+30
  subtrace3 = trace[t1>0:t2<((*pState).ts-1)]
  subtrace3 = (subtrace3-F01)/(P2-F01)
  if t1 lt 0 then begin
    subtrace3 = [replicate(!values.F_NAN,-t1),subtrace3]
  endif
  if t2 ge (*pState).ts then begin
    subtrace3 = [subtrace3,replicate(!values.F_NAN,t2-(*pState).ts+1)]
  endif

  return, {subtrace1:subtrace1,subtrace2:subtrace2,subtrace3:subtrace3}
End

;-----------------------------------------------------------------
Function GetRiseGroup3Ch2Trace, Event, ROINo
COMPILE_OPT STRICTARR
; export group3 trace, normalized and aligned by Tstart2
;flashPara [   0   ,  1  , 2 ,  3 ,   4   ,  5   , 6 ,  7  ,  8 ,    9   ,  10 ,  11 ,  12  , 13 , 14 ,  15 , 16 , 18, 19]
;flashPara [FlashNo, Peak, F0, Amp, Tstart, Tpeak, RT, RT90, T50, EndBase, Note, FAHM, dBase, T75, T90, FDHM, Dia, t1, t2]
  CATCH, Error_status
  IF Error_status NE 0 THEN BEGIN
    PRINT, '  Error message: ', !ERROR_STATE.MSG
    tempTrace = replicate(!values.F_NAN,61)
    return,{subtrace1:tempTrace,subTrace2:tempTrace,subTrace3:tempTrace}
    CATCH, /CANCEL
  ENDIF
  widget_control,event.top,get_uvalue=pState

  if ~obj_valid((*pState).result) then begin
    print,'result does not exist!'
    tempTrace = replicate(!values.F_NAN,61)
    return,{subtrace1:tempTrace,subTrace2:tempTrace,subTrace3:tempTrace}
  endif
  ROINum = (*pState).result->length()
  if ROINum eq 0 then begin
    print,'ROI number is zero!'
    tempTrace = replicate(!values.F_NAN,61)
    return,{subtrace1:tempTrace,subTrace2:tempTrace,subTrace3:tempTrace}
  endif
  info = (*pState).result->GetProperty(ROINo-1,trace=trace,flashPara=flashPara,flashNum=flashNum)
  if ~ptr_valid(flashPara) then begin
    print,ROINo,' Error getting FlashPara!'
    tempTrace = replicate(!values.F_NAN,61)
    return,{subtrace1:tempTrace,subTrace2:tempTrace,subTrace3:tempTrace}
  endif
  trace = reform(*trace[0])
  if (size(trace))[0] ne 2 then begin
    tempTrace = replicate(!values.F_NAN,61)
    return,{subtrace1:tempTrace,subTrace2:tempTrace,subTrace3:tempTrace}
  endif
  trace = reform(trace[1,*]) ; get Ch2, the signal of TMRM/Calcein red
  trace1 = smooth(trace,3,edge=1)
  Ts1 = (*flashPara)[4,0]
  Ts2 = (*flashPara)[4,1]
  Tp2 = (*flashPara)[5,1]
  F01 = trace1[Ts1]
  F02 = trace1[Ts2]
  P2 = trace1[Tp2]
  void = min(abs(*(*pState).CorrectedTime-Ts2),sPos)
  t1 = sPos-30
  t2 = sPos+30
  subtrace = trace[t1>0:t2<((*pState).ts-1)]
  subtrace1 = (subtrace-F01)/(F02-F01)
  subtrace2 = (subtrace-F01)/(P2-F01)
  if t1 lt 0 then begin
    subtrace1 = [replicate(!values.F_NAN,-t1),subtrace1]
    subtrace2 = [replicate(!values.F_NAN,-t1),subtrace2]
  endif
  if t2 ge (*pState).ts then begin
    subtrace1 = [subtrace1,replicate(!values.F_NAN,t2-(*pState).ts+1)]
    subtrace2 = [subtrace2,replicate(!values.F_NAN,t2-(*pState).ts+1)]
  endif
  void = min(abs(*(*pState).CorrectedTime-Tp2),pPos)
  t1 = pPos-30
  t2 = pPos+30
  subtrace3 = trace[t1>0:t2<((*pState).ts-1)]
  subtrace3 = (subtrace3-F01)/(P2-F01)
  if t1 lt 0 then begin
    subtrace3 = [replicate(!values.F_NAN,-t1),subtrace3]
  endif
  if t2 ge (*pState).ts then begin
    subtrace3 = [subtrace3,replicate(!values.F_NAN,t2-(*pState).ts+1)]
  endif

  return, {subtrace1:subtrace1,subtrace2:subtrace2,subtrace3:subtrace3}
End
;-----------------------------------------------------------------
Function GetGroupTrace, Event, ROINo
COMPILE_OPT STRICTARR

;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT, '  Error message: ', !ERROR_STATE.MSG
;    return, -1
;    CATCH, /CANCEL
;  ENDIF
  widget_control, Event.top, GET_UVALUE=pState

  if ~obj_valid((*pState).result) then begin
    print,'result does not exist!'
    return,-1
  endif
  ROINum = (*pState).result->length()
  if ROINum eq 0 then begin
    print,'ROI number is zero!'
    return,-1
  endif
  info = (*pState).result->GetProperty(ROINo-1,trace=trace,flashPara=flashPara,flashNum=flashNum)
  if ~ptr_valid(flashPara) then begin
    print,ROINo,' Error getting FlashPara!'
    return,-1
  endif
; export group trace
;flashPara [   0   ,  1  , 2 ,  3 ,   4   ,  5   , 6 ,  7  ,  8 ,    9   ,  10 ,  11 ,  12  , 13 , 14 ,  15 , 16 , 18, 19]
;flashPara [FlashNo, Peak, F0, Amp, Tstart, Tpeak, RT, RT90, T50, EndBase, Note, FAHM, dBase, T75, T90, FDHM, Dia, t1, t2]

  result = {dBase: (*flashPara)[12], $
    note: strtrim((*flashPara)[10],1), $
    subtrace1: ptr_new(), $
    subtrace2: ptr_new() }
  result.dBase = (*flashPara)[12]
  result.note = (*flashPara)[10]

  trace = reform(*trace[0])
  if (size(trace))[0] eq 2 then begin
    trace1 = reform(trace[0,*])
    trace2 = reform(trace[1,*])
  endif else trace1 = trace
  trace1 = smooth(trace1,3,edge=1)
  F01 = (*flashPara)[2]
  peak1 = (*flashPara)[1]
  peakTime1 = (*flashPara)[5]
  void = min(abs(*(*pState).CorrectedTime-peakTime1),peakPos1)
  t1 = peakPos1-30
  t2 = peakPos1+30
  subtrace1 = trace1[t1>0:t2<((*pState).ts-1)]
  subtrace1 = (subtrace1-F01)/(peak1-F01)
  if t1 lt 0 then subtrace1 = [replicate(!values.F_NAN,-t1),subtrace1]
  if t2 ge (*pState).ts then $
    subtrace1 = [subtrace1,replicate(!values.F_NAN,t2-(*pState).ts+1)]
  result.subtrace1 = ptr_new(subtrace1,/no_copy)

  if n_elements(trace2) eq 0 then return, result
  trace2 = smooth(trace2,3,edge=1)
  void = min(abs(*(*pState).CorrectedTime-(*flashPara)[4]),startPos)
  F02 = mean(trace2[(startPos-2)>0:(startPos+2)<((*pState).ts-1)])
  subtrace2 = trace2[t1>0:t2<((*pState).ts-1)]
  subtrace2 = subtrace2/F02
  if t1 lt 0 then subtrace2 = [replicate(!values.F_NAN,-t1),subtrace2]
  if t2 ge (*pState).ts then $
    subtrace2 = [subtrace2,replicate(!values.F_NAN,t2-(*pState).ts+1)]
  result.subtrace2 = ptr_new(subtrace2,/no_copy)
  return, result
End
;-----------------------------------------------------------------
pro OnBatchProcessing, Event
;pro OnBatchProcessing_ExportRiseGroup1Trace, Event
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  path = 'D:\My Documents\Data for Work\Flash Sniper\for detection\'
  files = dialog_pickfile(path=path,get_path=path,filter='*.txt',/multiple_files)
  if files[0] eq '' then return
  fileNum = n_elements(files)
  sTemplate = ascii_template(files[0])
  for fileNo=0,fileNum-1 do begin
    in = read_ascii(files[fileNo],template=sTemplate)
    saveFiles = in.(0)
    ROIs = in.(1)
    lineNum = n_elements(ROIs)
    close,2*fileNo+1,2*fileNo+2
    openw,2*fileNo+1,files[fileNo]+'-488-Trace.txt',/append,width=2000
    openw,2*fileNo+2,files[fileNo]+'-543-Trace.txt',/append,width=2000
    for lineNo=0,lineNum-1 do begin
      print, 'Group ',strtrim(fileNo+1,1), '  No ',strtrim(lineNo+1,1),' of ',strtrim(lineNum,1)
      if lineNo gt 0 then begin
      if saveFiles[lineNo] eq saveFiles[lineNo-1] then begin
        result = GetRiseGroup1Trace(event,ROIs[lineNo])
        printf,2*fileNo+1,format='(a0," ",i0,61f8.3)',savefiles[lineNo],ROIS[lineNo],result
        result = GetRiseGroup1Ch2Trace(event,ROIs[lineNo])
        printf,2*fileNo+2,format='(a0," ",i0,61f8.3)',savefiles[lineNo],ROIS[lineNo],result
      endif else begin
        saveFile = path+strmid(saveFiles[lineNo],0,strpos(saveFiles[lineNo],'.lsm'))+'-FlashSniper 3.0.sav'
        OnLoadStatus, Event, savefile=saveFile, /silent
        result = GetRiseGroup1Trace(event,ROIs[lineNo])
        printf,2*fileNo+1,format='(a0," ",i0,61f8.3)',savefiles[lineNo],ROIS[lineNo],result
        result = GetRiseGroup1Ch2Trace(event,ROIs[lineNo])
        printf,2*fileNo+2,format='(a0," ",i0,61f8.3)',savefiles[lineNo],ROIS[lineNo],result
      endelse
      endif else begin
        saveFile = path+strmid(saveFiles[lineNo],0,strpos(saveFiles[lineNo],'.lsm'))+'-FlashSniper 3.0.sav'
        OnLoadStatus, Event, savefile=saveFile, /silent
        result = GetRiseGroup1Trace(event,ROIs[lineNo])
        printf,2*fileNo+1,format='(a0," ",i0,61f8.3)',savefiles[lineNo],ROIS[lineNo],result
        result = GetRiseGroup1Ch2Trace(event,ROIs[lineNo])
        printf,2*fileNo+2,format='(a0," ",i0,61f8.3)',savefiles[lineNo],ROIS[lineNo],result
      endelse
    endfor
    close,2*fileNo+1,2*fileNo+2
  endfor
  void = dialog_message('Mission Accomplished!')

End
;-----------------------------------------------------------------
;pro OnBatchProcessing, Event
pro OnBatchProcessing_ExportRiseGroup3Trace, Event
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  path = 'D:\My Documents\Data for Work\Flash Sniper\for detection\'
  files = dialog_pickfile(path=path,get_path=path,filter='*.txt',/multiple_files)
  if files[0] eq '' then return
  fileNum = n_elements(files)
  sTemplate = ascii_template(files[0])
  for fileNo=0, fileNum-1 do begin
    in = read_ascii(files[fileNo],template=sTemplate)
    saveFiles = in.(0)
    ROIs = in.(1)
    lineNum = n_elements(ROIs)
    close,6*fileNo+1,6*fileNo+2,6*fileNo+3,6*fileNo+4,6*fileNo+5,6*fileNo+6
    openw,6*fileNo+1,files[fileNo]+'-488-Trace1.txt',/append,width=2000
    openw,6*fileNo+2,files[fileNo]+'-488-Trace2.txt',/append,width=2000
    openw,6*fileNo+3,files[fileNo]+'-488-Trace3.txt',/append,width=2000
    openw,6*fileNo+4,files[fileNo]+'-543-Trace1.txt',/append,width=2000
    openw,6*fileNo+5,files[fileNo]+'-543-Trace2.txt',/append,width=2000
    openw,6*fileNo+6,files[fileNo]+'-543-Trace3.txt',/append,width=2000
    for lineNo=0,lineNum-1 do begin
      print, 'Group ',strtrim(fileNo+1,1), '  No ',strtrim(lineNo+1,1),' of ',strtrim(lineNum,1)
      if lineNo gt 0 then begin
      if saveFiles[lineNo] eq saveFiles[lineNo-1] then begin
        result = GetRiseGroup3Trace(event,ROIs[lineNo])
        printf,6*fileNo+1,format='(a0," ",i0,61f8.3)',savefiles[lineNo],ROIS[lineNo],result.subtrace1
        printf,6*fileNo+2,format='(a0," ",i0,61f8.3)',savefiles[lineNo],ROIS[lineNo],result.subtrace2
        printf,6*fileNo+3,format='(a0," ",i0,61f8.3)',savefiles[lineNo],ROIS[lineNo],result.subtrace3
        result = GetRiseGroup3Ch2Trace(event,ROIs[lineNo])
        printf,6*fileNo+4,format='(a0," ",i0,61f8.3)',savefiles[lineNo],ROIS[lineNo],result.subtrace1
        printf,6*fileNo+5,format='(a0," ",i0,61f8.3)',savefiles[lineNo],ROIS[lineNo],result.subtrace2
        printf,6*fileNo+6,format='(a0," ",i0,61f8.3)',savefiles[lineNo],ROIS[lineNo],result.subtrace3
      endif else begin
        saveFile = path+strmid(saveFiles[lineNo],0,strpos(saveFiles[lineNo],'.lsm'))+'-FlashSniper 3.0.sav'
        OnLoadStatus, Event, savefile=saveFile, /silent
        result = GetRiseGroup3Trace(event,ROIs[lineNo])
        printf,6*fileNo+1,format='(a0," ",i0,61f8.3)',savefiles[lineNo],ROIS[lineNo],result.subtrace1
        printf,6*fileNo+2,format='(a0," ",i0,61f8.3)',savefiles[lineNo],ROIS[lineNo],result.subtrace2
        printf,6*fileNo+3,format='(a0," ",i0,61f8.3)',savefiles[lineNo],ROIS[lineNo],result.subtrace3
        result = GetRiseGroup3Ch2Trace(event,ROIs[lineNo])
        printf,6*fileNo+4,format='(a0," ",i0,61f8.3)',savefiles[lineNo],ROIS[lineNo],result.subtrace1
        printf,6*fileNo+5,format='(a0," ",i0,61f8.3)',savefiles[lineNo],ROIS[lineNo],result.subtrace2
        printf,6*fileNo+6,format='(a0," ",i0,61f8.3)',savefiles[lineNo],ROIS[lineNo],result.subtrace3
      endelse
      endif else begin
        saveFile = path+strmid(saveFiles[lineNo],0,strpos(saveFiles[lineNo],'.lsm'))+'-FlashSniper 3.0.sav'
        OnLoadStatus, Event, savefile=saveFile, /silent
        result = GetRiseGroup3Trace(event,ROIs[lineNo])
        printf,6*fileNo+1,format='(a0," ",i0,61f8.3)',savefiles[lineNo],ROIS[lineNo],result.subtrace1
        printf,6*fileNo+2,format='(a0," ",i0,61f8.3)',savefiles[lineNo],ROIS[lineNo],result.subtrace2
        printf,6*fileNo+3,format='(a0," ",i0,61f8.3)',savefiles[lineNo],ROIS[lineNo],result.subtrace3
        result = GetRiseGroup3Ch2Trace(event,ROIs[lineNo])
        printf,6*fileNo+4,format='(a0," ",i0,61f8.3)',savefiles[lineNo],ROIS[lineNo],result.subtrace1
        printf,6*fileNo+5,format='(a0," ",i0,61f8.3)',savefiles[lineNo],ROIS[lineNo],result.subtrace2
        printf,6*fileNo+6,format='(a0," ",i0,61f8.3)',savefiles[lineNo],ROIS[lineNo],result.subtrace3
      endelse
    endfor
    close,6*fileNo+1,6*fileNo+2,6*fileNo+3,6*fileNo+4,6*fileNo+5,6*fileNo+6
  endfor
  void = dialog_message('Mission Accomplished!')

End
;-----------------------------------------------------------------
;Pro OnBatchProcessing, Event
pro OnBatchProcessing_ExportdBaseGroupTrace, Event
;Export DBaseGroupTrace
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF
  path = 'E:\My Documents\Data for Work\Flash Sniper\for detection\'
  files = dialog_pickfile(path=path,get_path=path,filter='*.txt',/multiple_files)
  if files[0] eq '' then return
  fileNum = n_elements(files)
  sTemplate = ascii_template(files[0])
  for fileNo=0, fileNum-1 do begin
    in = read_ascii(files[fileNo],template=sTemplate)
    saveFiles = in.(0)
    ROIs = in.(1)
    lineNum = n_elements(ROIs)
    close,fileNo+1,fileNo+4
    openw,fileNo+1,files[fileNo]+'-Trace488.txt',/append,width=2000
    openw,fileNo+4,files[fileNo]+'-Trace405.txt',/append,width=2000

; Export RiseGroup
    for lineNo=0,lineNum-1 do begin
      print, 'Group ',strtrim(fileNo+1,1), '  No ',strtrim(lineNo+1,1),' of ',strtrim(lineNum,1)
      if lineNo gt 0 then begin
        if saveFiles[lineNo] eq saveFiles[lineNo-1] then begin
          result = GetGroupTrace(event,ROIs[lineNo])
          printf,fileNo+1,format='(a0,f8.3," ",a0," ",i0,61f8.3)', $
            result.note,result.dBase,savefiles[lineNo],ROIS[lineNo],*result.subtrace1
          if ptr_valid(result.subtrace2) then $
          printf,fileNo+4,format='(a0,f8.3," ",a0," ",i0,61f8.3)', $
            result.note,result.dBase,savefiles[lineNo],ROIS[lineNo],*result.subtrace2
          ptr_free, result.subtrace1,result.subtrace2
        endif else begin
          saveFile = path+strmid(saveFiles[lineNo],0,strpos(saveFiles[lineNo],'.lsm'))+'-FlashSniper 3.0.sav'
          OnLoadStatus, Event, savefile=saveFile, /silent
          result = GetGroupTrace(event,ROIs[lineNo])
          printf,fileNo+1,format='(a0,f8.3," ",a0," ",i0,61f8.3)', $
            result.note,result.dBase,savefiles[lineNo],ROIS[lineNo],*result.subtrace1
          if ptr_valid(result.subtrace2) then $
          printf,fileNo+4,format='(a0,f8.3," ",a0," ",i0,61f8.3)', $
            result.note,result.dBase,savefiles[lineNo],ROIS[lineNo],*result.subtrace2
          ptr_free, result.subtrace1,result.subtrace2
        endelse
      endif else begin
        saveFile = path+strmid(saveFiles[lineNo],0,strpos(saveFiles[lineNo],'.lsm'))+'-FlashSniper 3.0.sav'
        OnLoadStatus, Event, savefile=saveFile, /silent
        result = GetGroupTrace(event,ROIs[lineNo])
        printf,fileNo+1,format='(a0,f8.3," ",a0," ",i0,61f8.3)', $
          result.note,result.dBase,savefiles[lineNo],ROIS[lineNo],*result.subtrace1
        if ptr_valid(result.subtrace2) then $
        printf,fileNo+4,format='(a0,f8.3," ",a0," ",i0,61f8.3)', $
          result.note,result.dBase,savefiles[lineNo],ROIS[lineNo],*result.subtrace2
        ptr_free, result.subtrace1,result.subtrace2
      endelse
    endfor
; export dBaseGroup
;    for lineNo=0,lineNum-1 do begin
;      print, 'Group ',strtrim(fileNo+1,1), '  No ',strtrim(lineNo+1,1),' of ',strtrim(lineNum,1)
;      if lineNo gt 0 then begin
;        if saveFiles[lineNo] eq saveFiles[lineNo-1] then begin
;          result = GetGroupTrace(event,ROIs[lineNo])
;          printf,fileNo+1,format='(a0,f8.3," ",a0," ",i0,61f8.3)', $
;            strtrim(result.note,1),result.dBase,savefiles[lineNo],ROIS[lineNo],*result.subtrace1
;          if ptr_valid(result.subtrace2) then $
;          printf,fileNo+4,format='(a0,f8.3," ",a0," ",i0,61f8.3)', $
;            strtrim(result.note,1),result.dBase,savefiles[lineNo],ROIS[lineNo],*result.subtrace2
;          ptr_free, result.subtrace1,result.subtrace2
;        endif else begin
;          saveFile = path+strmid(saveFiles[lineNo],0,strpos(saveFiles[lineNo],'.lsm'))+'-FlashSniper 3.0.sav'
;          OnLoadStatus, Event, savefile=saveFile, /silent
;          result = GetGroupTrace(event,ROIs[lineNo])
;          printf,fileNo+1,format='(a0,f8.3," ",a0," ",i0,61f8.3)', $
;            strtrim(result.note,1),result.dBase,savefiles[lineNo],ROIS[lineNo],*result.subtrace1
;          if ptr_valid(result.subtrace2) then $
;          printf,fileNo+4,format='(a0,f8.3," ",a0," ",i0,61f8.3)', $
;            strtrim(result.note,1),result.dBase,savefiles[lineNo],ROIS[lineNo],*result.subtrace2
;          ptr_free, result.subtrace1,result.subtrace2
;        endelse
;      endif else begin
;        saveFile = path+strmid(saveFiles[lineNo],0,strpos(saveFiles[lineNo],'.lsm'))+'-FlashSniper 3.0.sav'
;        OnLoadStatus, Event, savefile=saveFile, /silent
;        result = GetGroupTrace(event,ROIs[lineNo])
;        printf,fileNo+1,format='(a0,f8.3," ",a0," ",i0,61f8.3)', $
;          strtrim(result.note,1),result.dBase,savefiles[lineNo],ROIS[lineNo],*result.subtrace1
;        if ptr_valid(result.subtrace2) then $
;        printf,fileNo+4,format='(a0,f8.3," ",a0," ",i0,61f8.3)', $
;          strtrim(result.note,1),result.dBase,savefiles[lineNo],ROIS[lineNo],*result.subtrace2
;        ptr_free, result.subtrace1,result.subtrace2
;      endelse
;    endfor
    close,fileNo+1,fileNo+4
  endfor
  void = dialog_message('Mission Accomplished!')

End
;-----------------------------------------------------------------
;Pro OnBatchProcessing, Event
Pro OnBatchProcessing_exportTrace1, Event
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF
  path = 'I:\for detection\'
  savefiles = dialog_pickfile(path=path,get_path=path,filter='*.sav',/multiple_files)
  if savefiles[0] eq '' then return
  saveFiles = saveFiles[sort(saveFiles)]
  fileNum = n_elements(savefiles)
  for fileNo=0, fileNum-1 do begin
    print, fileNo+1, 'of', fileNum
    OnLoadStatus, Event, savefile=savefiles[fileNo], /silent
    OnCommand, event
  endfor
  void = dialog_message('Mission Accomplished!')

End
;-----------------------------------------------------------------
;Pro OnBatchProcessing, Event
Pro OnBatchProcessing_add405, Event
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF
  path = 'E:\My Documents\Data for Work\Flash Sniper\for detection\'
  savefiles = dialog_pickfile(path=path,get_path=path,filter='*.sav',/multiple_files,/must_exist,/read)
  if savefiles[0] eq '' then return
  fileNum = n_elements(savefiles)
  for fileNo=0, fileNum-1 do begin
    print, fileNo+1, ' of', fileNum
    OnLoadStatus, Event, savefile=savefiles[fileNo], /silent
    widget_control,event.top,get_uvalue=pState
    if (*pState).stackNum eq 2 then continue
    lsmfile = strmid(savefiles[fileNo],0,strpos(savefiles[fileNo],'-FlashSniper'))
    if strpos(lsmfile,'.lsm') eq -1 then lsmfile += '.lsm'
    input = widget_read_lsm(lsmfile,2,imgInfo,/auto)
    if ~ptr_valid(input) then begin
      print, fileNo, savefiles[fileno],' single channel, not corrected!'
      continue
    endif

    ChangepState, input, pState
    ImageFiltration, pState, stackNo=2
    widget_control,event.top,set_uvalue=pState
    OnChannelAlignment, Event, /data
    AddTrace405, pState

    if widget_info((*pState).AnimationBase, /valid_id) then begin  ; destroy old animation base
      (*pState).oWindow->RemoveWindowEventObserver, (*pState).oObserver
      obj_destroy, (*pState).oObserver
      (*pState).oWindow->SetProperty, Graphics_Tree=obj_new()
      obj_destroy, (*pState).oWindow
      widget_control, (*pState).AnimationBase, /destroy
    endif
    CreatAnimationBase, pState
    CreatAnimationModel, pState

;    OnSaveStatus, Event, savefile=savefiles[fileNo], /silent
  endfor
  void = dialog_message('Mission Accomplished!')

End
;-----------------------------------------------------------------
;Pro OnBatchProcessing, Event
Pro OnBatchProcessing_405photobleach_correction, Event
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF
  path = 'E:\My Documents\Data for Work\Flash Sniper\for detection\'
  savefiles = dialog_pickfile(path=path,get_path=path,filter='*.sav',/multiple_files,/must_exist,/read)
  if savefiles[0] eq '' then return
  fileNum = n_elements(savefiles)
  for fileNo=0, fileNum-1 do begin
    print, fileNo+1, ' of', fileNum
    OnLoadStatus, Event, savefile=savefiles[fileNo], /silent
    widget_control,event.top,get_uvalue=pState
    if (*pState).stackNum eq 1 then continue
    OnPhotobleachCorrection, Event
    OnSaveStatus, Event, savefile=savefiles[fileNo], /silent
  endfor
  void = dialog_message('Mission Accomplished!')

End
;-----------------------------------------------------------------
Pro OnBatchProcessing_validation, Event
;Pro OnBatchProcessing, Event
COMPILE_OPT STRICTARR
; perform validation to a list of files
  path = 'D:\My Documents\Data for Work\Flash Sniper\for validation\'
  savefiles = dialog_pickfile(path=path,get_path=path,filter='*.sav',/multiple_files)
  if savefiles[0] eq '' then return

  for fileNo=0, n_elements(savefiles)-1 do begin
    OnLoadStatus, Event, savefile=savefiles[fileNo], /silent
;    OnSetValidationParameters, event
    OnValidation, Event, SNR=3.0
;    print, saveFiles[fileNo]
;    OnPlay, event
  endfor
End