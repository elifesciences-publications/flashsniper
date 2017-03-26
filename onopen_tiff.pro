 pro OnOpen_tiff, Event, file=file, silent=silent
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  ; select *.lsm file
  if n_elements(file) eq 0 then begin
    filter = [['*.lsm','*.dat','*.bmp','*.tif','*.jpg'], ['LSM','Dat','Bitmap','TIFF','JPEG']]
  ; filter = [['*.tif','*.lsm','*.bmp','*.jpg'], ['TIFF','LSM','Bitmap','JPEG']]
    RecentFileSav = !dir+'\User Config\Flash Sniper\recent files.sav'
    if ~file_test(RecentFileSav) then begin
      file_mkdir, !dir+'\User Config\Flash Sniper\' & print, 'Creating user config directory......'
      file = dialog_pickfile(filter=filter,get_path=path, /Read, /multiple_files)
      if file[0] eq '' then return
      RecentFile = file
    endif else begin
      Restore, RecentFileSav
;     file = strmid(RecentFile[0], 0, strpos(RecentFile[0],'.lsm')+4)
      file = recentFile[0]
      file = dialog_pickfile(filter=filter,get_path=path,file=file, /Read, /multiple_files,/must_exist)
      if file[0] eq '' then return
      if ~file_test(file[0]) then begin
        void = dialog_message('Can not find input file!')
        return
      endif
      RecentFile = [file,RecentFile]
      if n_elements(RecentFile) gt 10 then RecentFile = RecentFile[0:9]
    endelse
    save, RecentFile, filename=RecentFileSav
  endif

  ; cleanup last result
  widget_control, event.top, get_uvalue=pState
  if ptr_valid(pState) then begin
    if widget_info((*pState).AnimationBase, /valid_id) then begin
      widget_control, (*pState).AnimationBase, /destroy
    endif
    if widget_info((*pState).DetectionThrBase, /valid_id) then begin
      widget_control, (*pState).DetectionThrBase, /destroy
    endif
    if obj_valid((*pState).result) then obj_destroy, (*pState).result   ; destroy objects except oWindow
    if obj_valid((*pState).oView) then begin
      obj_destroy, (*pState).oView
      (*pState).oWindow->RemoveWindowEventObserver, (*pState).oObserver
      obj_destroy, (*pState).oObserver
    endif
    widget_control, (*pState).ResultTable, get_value=table
      table = replicate('',size(table,/dimensions))
      widget_control, (*pState).ResultTable, set_value=table
  endif
  PTR_FREE, PTR_VALID()   ; destroy pointers

  widget_control, event.top, base_set_title='Flash Sniper      '+file[0]
  widget_control, widget_info(event.top,find_by_uname='Open'), get_uvalue=pCursor
  if ~ptr_valid(pCursor) then begin
    pCursor = CreatCursor()
    widget_control, widget_info(event.top,find_by_uname='Open'), set_uvalue=pCursor
  endif
  file = file[sort(file)]

  ; read lsm image files
  if strmid(file[0],strlen(file[0])-4,4) eq '.lsm' then begin
    input = widget_read_lsm(file[0],0,imgInfo,group_leader=event.top)
    if ~ptr_valid(input[0]) then return

    time = *imgInfo.timeStamp
    stackNum = n_elements(input)
;   print,stackNum
    img = ptrarr(stackNum)
    ;for i=0, stackNum-1 do  input[i] =ptr_new(bytscl(*input[i]));;Added by suntao, temp modi for fenggaomin
    
    for i=0, stackNum-1 do  img[i] = input[i]
    ; time stamp correction
    frameNum = imgInfo.imgSize[2]
    timeNum = n_elements(time)
    if timeNum gt frameNum then begin
      time = time[0:frameNum-1]
    endif else begin
      if timeNum lt frameNum then begin
        time = [time, fltarr(frameNum-timeNum)]
      endif
    endelse

    fileNum = n_elements(file)
    if fileNum gt 1 then begin
      channelSelection = *imgInfo.channelselection
      channel = where(channelSelection eq 1)
      channel = (channel[0] eq -1) ? -1 : channel+1
  ;   print,channel
      for k=1, fileNum-1 do begin
        input1 = widget_read_lsm(file[k],channel,imgInfo1,/auto)
        if imgInfo1.channelNum ne imgInfo.channelNum then begin
          print, file_basename(file[k]), '  Inconsistent of channel number!'
        endif
        if max(*imgInfo1.channelselection ne *imgInfo.channelselection) then begin
          print, file_basename(file[k]), '  Inconsistent of channel selection!'
        endif
        if max(imgInfo1.imgSize[0:1] ne imgInfo.imgSize[0:1])  then begin
          print, file_basename(file[k]), '  Inconsistent of image size!'
        endif

        if max(imgInfo1.pixelSize[0:1] ne imgInfo.pixelSize[0:1]) then begin
          print, file_basename(file[k]), '  Inconsistent of pixel size!'
        endif
        subTime = *imgInfo1.timeStamp
        ; time stamp correction
        frameNum = imgInfo1.imgSize[2]
        timeNum = n_elements(subTime)
        if timeNum gt frameNum then begin
          subTime = subTime[0:frameNum-1]
        endif else begin
          if timeNum lt frameNum then begin
            subTime = [subTime, fltarr(frameNum-timeNum)]
          endif
        endelse
        time = [time, subTime]
        for i=0, stackNum-1 do *img[i] = [[[*img[i]]], [[*input1[i]]]]
        ptr_free, input1, imgInfo1.channelName, imgInfo1.channelColor, imgInfo1.channelFilter, imgInfo1.channelSelection, $
            imgInfo1.laserInfo, imgInfo1.timeStamp, imgInfo1.vectorOverlay, imgInfo1.ROIOverlay
      endfor
    endif
    xs = imgInfo.imgSize[0] & ys = imgInfo.imgSize[1] & ts = n_elements(time)
    pxs = imgInfo.pixelSize[0] & pys = imgInfo.pixelSize[1] & pts = imgInfo.pixelSize[2]
    stackColor = (*imgInfo.channelColor)[0:2,where(*imgInfo.channelSelection eq 1)]
    ptr_free, imgInfo.channelName, imgInfo.channelColor, imgInfo.channelFilter, imgInfo.channelSelection, $
          imgInfo.laserInfo, imgInfo.timeStamp, imgInfo.vectorOverlay, imgInfo.ROIOverlay
  endif else if strmid(file[0],strlen(file[0])-4,4) eq '.dat' then begin
    xs = 900L & ys = 256L & ts = 100L
    pxs = 1.0 & pys = 1.0 & pts = 1.0
    input = bytarr(xs,ys,ts)
    close,1 & openr,1,file[0]
    readu,1,xs,ys,ts,pxs,pys,pts,input
    close,1
    stackNum = 1 & stackcolor = [0,255,0]
    img = ptr_new(input)
    time = findgen(ts)
  endif else begin
    input = read_image(file[0])
    if strmid(file[0],strlen(file[0])-4,4) eq '.tif' or strmid(file[0],strlen(file[0])-4,4) eq 'tiff' $
    then input = reverse(input,2)
    stackNum = 1 & stackcolor = [0,255,0]
    dim = size(input, /dimensions) & xs = dim[0] & ys = dim[1]
    img = ptr_new(input)
    fileNum = n_elements(file) & ts = fileNum
    if fileNum gt 1 then begin
      for k=1, fileNum-1 do begin
        input = read_image(file[k])
        if strmid(file[k],strlen(file[k])-4,4) eq '.tif' or strmid(file[k],strlen(file[k])-4,4) eq 'tiff' $
        then input = reverse(input,2)
        *img[0] = [[[*img[0]]], [[input]]]
      endfor
    endif
    pxs = 1.0 & pys = 1.0 & pts = 1.0
    time = findgen(ts)
  endelse

  ; Cut or resize the original image if it's too big
; if float(xs)*ys*ts gt 512.0*512*100 then begin
;   sizeInfo = strtrim(string(xs),1)+'*'+strtrim(string(ys),1)+'*'+strtrim(string(ts),1)
;   if dialog_message('Image stack is too big ('+sizeInfo+') ! Cut an ROI?', /question) eq 'Yes' then begin
;     window,0,xs=xs,ys=ys,title='Left click: play movie    Right click: select ROI' & tv, (*img[0])[*,*,0]
;     while 1b do begin
;       cursor,x,y, /up
;       if !mouse.button eq 4 then break
;       for k=1,ts-1 do begin
;         tv, (*img[0])[*,*,k] & wait, 0.05
;       endfor
;     endwhile
;     box_cursor,x0,y0,nx,ny
;     wdelete,0
;     for i=0, stackNum-1 do *img[i] = (*img[i])[x0:x0+nx,y0:y0+ny,*]
;     xs = nx+1 & ys = ny+1
;   endif else begin
;     if dialog_message('Up sample the original image stack ('+sizeInfo+') ?', /question) eq 'Yes' then begin
;       xs /= 2 & ys /= 2
;       for i=0, stackNum-1 do *img[i] = congrid(*img[i],xs,ys,ts,/interp)
;     endif
;   endelse
; endif
; widget_control, /hourglass

  ; define data structure
  HBMPara = {l1x:0, l1y:0, l2x:0, l2y:0, l3x:10, l3y:2, angle:3, unit:1, $
    IsRotation:0b, trackStart:0, trackEnd:ts-1}

  State = { file: file, $
    stackNum: stackNum, stackColor: stackColor, $
    xs: long(xs), ys: long(ys), ts: long(ts), $
    pxs: pxs, pys: pys, pts: pts,  $
    img: img, ima: PTR_NEW(), $
    startPic:0, endPic: ts-1,  $
    validFrame: ptr_new(indgen(ts)), $
    crop: [0,xs-1,0,ys-1], $
    back: ptr_new(fltarr(stackNum,ts)), $
    time: ptr_new(time), $
    correctedTime: ptr_new((time-time[0])>0), $
    shift488: [0,0],  $
    mask: PTR_NEW(),  $
    edge: PTR_NEW(),  $
    flashMask: PTR_NEW(),  $
    timing: PTR_NEW(),  $
    sum: PTR_NEW(),  $
    peak: PTR_NEW(),  $
    imaHist: ptr_new(), $
    peakHist: ptr_new(), $
    sumHist: ptr_new(), $
    MainBase: Event.top, $
    AnimationBase: -1, $
    DetectionThrBase: -1, $
    ContextBase1: -1, $
    TraceDraw: -1, $
    TraceWindow: -1, $
    ResultTable: -1, $
    oWindow: obj_new(),  $   ; not save
    oObserver: obj_new(),  $   ; not save
    oView: obj_new('IDLgrView'),  $
    oAnimationModel: objarr(stackNum),  $   ;not save
    oAutoROIModel: obj_new('IDLgrModel', Select_target=0), $
    oManualROIModel: obj_new('IDLgrModel',Select_target=0), $
    oLabelModel: obj_new('IDLgrModel',Select_target=0), $
    oROIFont: obj_new('IDLgrFont',size=12), $
    currentROI: obj_new(),  $
    result: obj_new('Chain',/alocate_heap),  $
    cursor: *pCursor, $
    pHBMPara: ptr_new(HBMPara, /no_copy), $
    option:'', $
    interval: 0.0, $
    RoiHide: 0b, $
    LabelHide: 0b, $
    Brightness: 0.0, $
    Contrast: 1.0, $
    mean:[0.0,0.0], $
    sigma: [4.0,4.0], $
    Thr: [1.2,3.3], $
    cc: 1, $
    wStatus: -1, $
    bButtonDown: 0b, $
    bTempSegment: 0b, $
    sel_rgb: BYTE([0,255,255]), $
    roi_rgb: BYTE([255, 0, 0]), $
    oSelVisual: OBJ_NEW(), $
    oSelHandle: OBJ_NEW(), $
    oTransScaleVisual: OBJ_NEW(), $
;   oPickVisual: OBJ_NEW(), $
    oSelROI: OBJ_NEW(), $
      pSavedROIData: PTR_NEW(), $
      savedROIXRange: DBLARR(2), $
      savedROIYRange: DBLARR(2), $
    buttonXY: LONARR(2), $
    init: PTR_NEW(init), $
    bFirstROI: 1B, $
    currStack: 0, $
    mode: '' ,$
    oLinescan:obj_new('idlgrmodel')$
    }
  pState = PTR_NEW(State, /allocate_heap)
;  for k=0,(*pState).stackNum-1 do begin
;       *(*pState).img[k] = float(*(*pState).img[k])
;  endfor
;  ImageFiltration, pState
;------------------ switch channel -------------------
;  ch1 = 0
;  ch2 = 1
;  if (*pState).StackNum gt 1 then begin
;    pTemp = (*pState).img[ch1]
;    (*pState).img[ch1] = (*pState).img[ch2]
;    (*pState).img[ch2] = pTemp
;    temp = (*pState).stackColor[*,ch1]
;    (*pState).stackColor[*,ch1] = (*pState).stackColor[*,ch2]
;    (*pState).stackColor[*,ch2] = temp
;  endif
;-----------------------------------------------------

  (*pState).TraceDraw = widget_info(event.top, find_by_uname='TraceWindow')
  widget_control, (*pState).TraceDraw, get_value=TraceWindow
  (*pState).TraceWindow = TraceWindow
  idResultTable = widget_info(event.top, find_by_uname='ResultTable')
  (*pState).ResultTable = idResultTable
  TraceDrawInitialize, (*pState).TraceDraw
  CreatAnimationBase, pState
  CreatAnimationModel, pState

  widget_control, event.top, set_uvalue = pState,/no_copy

  wait, 0.5
  OnGetMask, event, /auto, silent=keyword_set(silent)
end