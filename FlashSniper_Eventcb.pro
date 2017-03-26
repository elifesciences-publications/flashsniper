Pro OnTraceWindowEvent, Event
COMPILE_OPT STRICTARR
  print,event
End
;-----------------------------------------------------------------

 pro OnOpen, Event, file=file, silent=silent
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
  ;	filter = [['*.tif','*.lsm','*.bmp','*.jpg'], ['TIFF','LSM','Bitmap','JPEG']]
  	RecentFileSav = !dir+'\User Config\Flash Sniper\recent files.sav'
  	if ~file_test(RecentFileSav) then begin
  		file_mkdir, !dir+'\User Config\Flash Sniper\' & print, 'Creating user config directory......'
  		file = dialog_pickfile(filter=filter,get_path=path, /Read, /multiple_files)
  		if file[0] eq '' then return
  		RecentFile = file
  	endif else begin
  		Restore, RecentFileSav
;  		file = strmid(RecentFile[0], 0, strpos(RecentFile[0],'.lsm')+4)
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

  ; read image files
	if strmid(file[0],strlen(file[0])-4,4) eq '.lsm' then begin
		input = widget_read_lsm(file[0],0,imgInfo,group_leader=event.top)
		if ~ptr_valid(input[0]) then return

		time = *imgInfo.timeStamp
		stackNum = n_elements(input)
;		print,stackNum
		img = ptrarr(stackNum)
		;for i=0, stackNum-1 do  input[i] =ptr_new(bytscl(*input[i]));;Added by suntao, temp modi for fenggaomin
		
		for i=0, stackNum-1 do	img[i] = input[i]
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
	;		print,channel
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
  endif else if strmid(file[0],strlen(file[0])-4,4) eq '.tif' then begin

    ok = QUERY_TIFF(file[0],s)
    input = read_tiff(file[0],image_index = 0)
    input = reverse(input,2)
    stackNum = 1
    stackcolor = [0,255,0]
    dim = size(input, /dimensions) & xs = dim[0] & ys = dim[1]
    frameNum = s.NUM_IMAGES
;   print,stackNum
    img = ptr_new(input)
    ;for i=0, stackNum-1 do  input[i] =ptr_new(bytscl(*input[i]));;Added by suntao, temp modi for fenggaomin
    
    for k = 0,frameNum-1 do begin
      input = read_tiff(file[0],image_index = k)
      input = reverse(input,2)
      *img[0] = [[[*img[0]]], [[input]]]
    endfor
    ; time stamp correction
    
    
    time = findgen(frameNum)
    ts = frameNum
    pxs = 1.0 & pys = 1.0 & pts = 1.0
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
;	if float(xs)*ys*ts gt 512.0*512*100 then begin
;	  sizeInfo = strtrim(string(xs),1)+'*'+strtrim(string(ys),1)+'*'+strtrim(string(ts),1)
;		if dialog_message('Image stack is too big ('+sizeInfo+') ! Cut an ROI?', /question) eq 'Yes' then begin
;			window,0,xs=xs,ys=ys,title='Left click: play movie    Right click: select ROI' & tv, (*img[0])[*,*,0]
;			while 1b do begin
;				cursor,x,y, /up
;				if !mouse.button eq 4 then break
;				for k=1,ts-1 do begin
;					tv, (*img[0])[*,*,k] & wait, 0.05
;				endfor
;			endwhile
;			box_cursor,x0,y0,nx,ny
;			wdelete,0
;			for i=0, stackNum-1 do *img[i] = (*img[i])[x0:x0+nx,y0:y0+ny,*]
;			xs = nx+1 & ys = ny+1
;		endif else begin
;			if dialog_message('Up sample the original image stack ('+sizeInfo+') ?', /question) eq 'Yes' then begin
;				xs /= 2 & ys /= 2
;				for i=0, stackNum-1 do *img[i] = congrid(*img[i],xs,ys,ts,/interp)
;			endif
;		endelse
;	endif
;	widget_control, /hourglass

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
;	  oPickVisual: OBJ_NEW(), $
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
;-----------------------------------------------------------------

pro OnLoadStatus, Event, savefile=savfile, silent=silent
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF
  idMainBase = event.top
  widget_control, idMainBase, get_uvalue=pState

  if n_elements(savFile) eq 0 then begin
    file = ''
    if ptr_valid(pState) then begin
      if (*pState).file[0] ne '' then $
      file = (*pState).file[0] ;strmid((*pState).file[0], 0, strpos((*pState).file[0], '.lsm'))
    endif else begin
      RecentFileSav = !dir+'\User Config\Flash Sniper\recent files.sav'
      if file_test(RecentFileSav) then begin
        Restore, RecentFileSav
        if RecentFileSav[0] ne '' then $
          file = RecentFile[0] ;strmid(RecentFile[0], 0, strpos(RecentFile[0], '.lsm'))
      endif
    endelse
    if strmid(file,strlen(file)-4,4) ne '.sav' then file = file+ '-FlashSniper 3.0.sav'
    savFile = dialog_pickfile(filter='*.sav', file=file)
  endif
  if savFile[0] eq '' then return
  if ~file_test(savFile) then begin
    void = dialog_message('The selectd file is not found!',/error)
    return
  endif
  if n_elements(RecentFile) gt 0 then begin
    RecentFile = [savfile,RecentFile]
    if n_elements(RecentFile) gt 10 then RecentFile = RecentFile[0:9]
    save, RecentFile, filename=RecentFileSav
  endif

  ; Clean up old status
  if ptr_valid(pState) then begin
    if widget_info((*pState).AnimationBase, /valid_id) then begin  ; destroy old animation base
      (*pState).oWindow->RemoveWindowEventObserver, (*pState).oObserver
      obj_destroy, (*pState).oObserver
      widget_control, (*pState).AnimationBase, /destroy
    endif
    if widget_info((*pState).DetectionThrBase, /valid_id) then begin
      widget_control, (*pState).DetectionThrBase, /destroy
    endif
    if obj_valid((*pState).result) then obj_destroy, (*pState).result   ; destroy objects
    if obj_valid((*pState).oView) then begin
      obj_destroy, (*pState).oView
    endif
    widget_control, (*pState).ResultTable, get_value=table
    table = replicate('',size(table,/dimensions))
    widget_control, (*pState).ResultTable, set_value=table
  endif
  PTR_FREE, PTR_VALID()   ; destroy pointers

  ; Restore previous status
  restore, savFile, /verbose
  path = file_dirname(savFile)
  if strmid(path,strlen(path)-1,1) ne '\' then path=path+'\'
  file = file_basename((*pState).file[0])
  (*pState).file[0] = path+file
  (*pState).MainBase = idMainBase
  event.top = idMainBase
  idResultTable = widget_info(idMainBase, find_by_uname='ResultTable')
  (*pState).ResultTable = idResultTable
  (*pState).AnimationBase = -1
  (*pState).DetectionThrBase = -1
  (*pState).ContextBase1 = -1
  widget_control, idMainBase, set_uvalue=pState, base_set_title='Flash Sniper      '+SavFile

  ; Check compatiblity with old .sav files
  oldTags = tag_names(*pState)
  if (where(oldtags eq 'OROIFONT'))[0] eq -1 then begin
    *pState = create_struct(*pState,'oROIFont',obj_new('IDLgrFont',size=9))
  endif
  if (where(oldtags eq 'EDGE'))[0] eq -1 then begin
    edge = ptr_valid((*pState).mask) ? ptr_new(GetEdge(*(*pState).mask),/no_copy) : ptr_new()
    *pState = create_struct(*pState,'edge',edge)
  endif
  if (where(oldtags eq 'TRACEDRAW'))[0] eq -1 then begin
    *pState = create_struct(*pState,'TraceDraw',widget_info(event.top, find_by_uname='TraceWindow'))
  endif
  if (where(oldtags eq 'CURSOR'))[0] eq -1 then begin
    widget_control, widget_info(event.top,find_by_uname='Open'), get_uvalue=pCursor
    if ~ptr_valid(pCursor) then begin
      pCursor = CreatCursor()
      widget_control, widget_info(event.top,find_by_uname='Open'), set_uvalue=pCursor
    endif
    *pState = create_struct(*pState,'cursor',*pCursor)
  endif
  if (where(oldtags eq 'CURRENTROI'))[0] eq -1 then begin
    *pState = create_struct(*pState,'currentROI',(*pState).newROI)
  endif
  (*pState).TraceDraw = widget_info(event.top, find_by_uname='TraceWindow')
  widget_control, (*pState).TraceDraw, get_value=TraceWindow
  (*pState).TraceWindow = TraceWindow
  TraceDrawInitialize, (*pState).TraceDraw

  ; creat animation model
  CreatAnimationBase, pState
  CreatAnimationModel, pState

  if obj_valid((*pState).result) then begin
    ROINum = (*pState).result->length()
    if ROINum gt 0 then begin
      idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
      widget_control, idROISlider, set_slider_max = ROINum, set_value=1
      ; Correct result list
      row=0
      for ROINo=0, ROINum-1 do begin
        info = (*pState).result->GetProperty(ROINo,oLabel=oLabel,ROIPara=ROIPara,flashPara=flashPara,flashNum=flashNum)
        if info eq -1 then continue
        if obj_valid(oLabel) then oLabel->SetProperty, strings=string(ROINo+1,format='(i0)')
;flashPara [   0   ,  1  , 2 ,  3 ,   4   ,  5   , 6 ,  7  ,  8 ,    9   ,  10 ,  11 ,  12  ,   13  ,   14  ,  15 , 18, 19]
;flashPara [FlashNo, Peak, F0, Amp, Tstart, Tpeak, RT, RT90, T50, EndBase, Note, FAHM, dBase, dBase1, dBase2, FDHM, t1, t2]
;flashPara [   0   ,  1  , 2 ,  3 ,   4   ,  5   , 6 ,  7  ,  8 ,    9   ,  10 ,  11 ,  12  , 13 , 14 ,  15 , 16 , 18, 19]
;flashPara [FlashNo, Peak, F0, Amp, Tstart, Tpeak, RT, RT90, T50, EndBase, Note, FAHM, dBase, T75, T90, FDHM, Dia, t1, t2]
        if ptr_valid(flashPara) then begin
          dim = size(*flashPara)
          if dim[1] eq 9 then begin  ;first extention
            if dim[0] eq 1 then begin
              para = fltarr(20)
              para[0] = (*flashPara)[0:6]
              para[8] = (*flashPara)[7:8]
            endif else begin
              para = fltarr(20,dim[2])
              para[0:6,*] = (*flashPara)[0:6,*]
              para[8:9,*] = (*flashPara)[7:8,*]
            endelse
            *flashPara = para
          endif
          if dim[1] eq 11 then begin  ;second extention
            if dim[0] eq 1 then begin
              para = fltarr(20)
              para[0] = (*flashPara)[0:10]
            endif else begin
              para = fltarr(20,dim[2])
              para[0:10,*] = (*flashPara)[0:10,*]
            endelse
            *flashPara = para
          endif
          for flashNo=1,flashNum do begin
;            if flashNum eq 2 then print, row+flashNo
            widget_control,(*pState).ResultTable,use_table_select=[0,row,2,row], $
              set_value=[string(ROINo+1,format='(i0)'),string(reform(ROIPara,2s,1),format='(f10.2)')]
            widget_control,(*pState).ResultTable,use_table_select=[3,row,19,row], $
              set_value=[string(reform((*flashPara)[0,flashNo-1]),format='(i0)'), $
              string(reform((*flashPara)[1:9,flashNo-1],9,1),format='(f10.2)'), $
              string(reform((*flashPara)[10,flashNo-1]),format='(i0)'), $
              string(reform((*flashPara)[11:16,flashNo-1],6,1),format='(f10.2)')]
            row++
          endfor
        endif else begin
          for flashNo=1,flashNum do begin
;            if flashNum eq 2 then print, row+flashNo
            widget_control,(*pState).ResultTable,use_table_select=[0,row,2,row], $
              set_value=[string(ROINo+1,format='(i0)'),string(reform(ROIPara,2s,1),format='(f10.2)')]
            row++
          endfor
        endelse
      endfor
    endif
  endif

  if ~keyword_set(silent) then notice = dialog_message('Results are loaded!', /information)

end
;-----------------------------------------------------------------

pro OnSaveStatus, Event, savefile=savefile, silent=silent
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  widget_control, event.top, get_uvalue=pState
  if ~ptr_valid(pState) then return

  if n_elements(savefile) eq 0 then begin
    saveFile = strmid((*pState).file[0], 0, strpos((*pState).file[0], '.lsm')) + '-FlashSniper 3.0.sav'
    if ~keyword_set(silent) then begin
      saveFile = dialog_pickfile(filter='*.sav',file=savefile,/write)
      if saveFile[0] eq '' then return
      if file_test(saveFile) then begin
        if dialog_message('Overwrite existing sav file?',/question) eq 'No' then return
      endif
    endif
  endif
  if strmid(saveFile[0],strlen(saveFile[0])-4,4) ne '.sav' then saveFile += '.sav'

  Heap_Nosave, (*pState).ima
  Heap_Nosave, (*pState).oWindow
  Heap_Nosave, (*pState).oObserver
  Heap_Nosave, (*pState).oAnimationModel
  save, /comm, /variables, filename=saveFile,/verbose;,/compress

  if ~keyword_set(silent) then notice = dialog_message('Status are saved!', /information)
end

Pro ImportManualROI, pState, ROIMap, MMROINum
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  resolve_routine,'GetTrace', /EITHER, /NO_RECOMPILE
  if MMROINum eq 0 then return

  tag = reform(ROIMap[1,*,*])
  ROIMap = reform(ROIMap[0,*,*])
  ROIMap[where(tag ne 2)] = 0
  ROI = ROIMap[uniq(ROIMap,sort(ROIMap))]
  ROI = ROI[1:*]
  if n_elements(ROI) ne MMROINum then begin
    print,'ROI:',ROI,'   MMROINum:',MMROINum
    return
  endif
  widget_control, (*pState).ResultTable, get_value=table
  ROINoColumn = uint(reform(table[0,*]))
  lastRow = max(where(ROINoColumn ne 0))
  oldROINum = (*pState).result->length()

  MROINum = max(ROIMap)
  MMROINo = 0
  for MROINo=0,MROINum-1 do begin
    indices = where(ROIMap eq MROINo+1)
    if indices[0] eq -1 then continue
    MMROINo++
    boundry = Find_Boundary(indices, XSize=(*pState).xs, YSize=(*pState).ys)
    newRoi = obj_new('IDLgrROI', boundry[0,*], boundry[1,*], color=[255,0,0], alpha_channel=1)
    result = newRoi->ComputeGeometry(centroid=center,area=area)
    area *= ((*pState).pxs)^2 ;& print,MMROINo, area
    distance = ptr_valid((*pState).mask) ? DistToEdge(*(*pState).edge,center)*(*pState).pxs : 0
    roiMask = newROI->ComputeMask(dimensions=[(*pState).xs,(*pState).ys])
    indices = where(roiMask gt 0)
    trace = GetTrace(pState,indices)
    oLabel = obj_new('IDLgrText', string((*pState).result->Length()+1,format='(i0)'), $
      font=(*pState).oROIFont, color=[255,255,0], locations=center)
    (*pState).oLabelModel->Add, oLabel
    node = obj_new('ChainNode')
    node->SetProperty, oROI=newRoi, oLabel = oLabel, trace=trace, ROIPara=[Area,distance], flashNum=1
    info = (*pState).result->insert(node)
    (*pState).oManualROIModel->Add, newRoi
    widget_control, (*pState).ResultTable, use_table_select=[0,lastrow+MMROINo,2,lastRow+MMROINo+1], $
      set_value=[string(oldROINum+MMROINo,format='(i0)'),string([area,distance],format='(f0.2)')]
  endfor
End
;-----------------------------------------------------------------
Pro ImportAutoROI, pState, ROIMap, MAROINum
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  resolve_routine,'GetTrace', /EITHER, /NO_RECOMPILE
  if MAROINum eq 0 then return

  tag = reform(ROIMap[1,*,*])
  ROIMap = reform(ROIMap[0,*,*])
  ROIMap[where(tag ne 1)] = 0
  ROI = ROIMap[uniq(ROIMap,sort(ROIMap))]
  ROI = ROI[1:*]
  if n_elements(ROI) ne MAROINum then begin
    print,'ROI:',ROI,'   MAROINum:',MAROINum
    return
  endif
  widget_control, (*pState).ResultTable, get_value=table
  ROINoColumn = uint(reform(table[0,*]))
  lastRow = max(where(ROINoColumn ne 0))
  oldROINum = (*pState).result->length()

  MROINum = max(ROIMap)
  MAROINo = 0
  for MROINo=0,MROINum-1 do begin
    indices = where(ROIMap eq MROINo+1)
    if indices[0] eq -1 then continue
    MAROINo++
    boundry = Find_Boundary(indices, XSize=(*pState).xs, YSize=(*pState).ys)
    newRoi = obj_new('IDLgrROI', boundry[0,*], boundry[1,*], color=[255,0,0], alpha_channel=1)
    result = newRoi->ComputeGeometry(centroid=center,area=area)
    area *= ((*pState).pxs)^2 ;& print,MAROINo, area
    distance = ptr_valid((*pState).mask) ? DistToEdge(*(*pState).edge,center)*(*pState).pxs : 0
    roiMask = newROI->ComputeMask(dimensions=[(*pState).xs,(*pState).ys])
    indices = where(roiMask gt 0)
    trace = GetTrace(pState,indices)
    oLabel = obj_new('IDLgrText', string((*pState).result->Length()+1,format='(i0)'), $
      font=(*pState).oROIFont, color=[255,255,0], locations=center)
    (*pState).oLabelModel->Add, oLabel
    node = obj_new('ChainNode')
    node->SetProperty, oROI=newRoi, oLabel = oLabel, trace=trace, ROIPara=[Area,distance], flashNum=1
    info = (*pState).result->insert(node)
    (*pState).oAutoROIModel->Add, newRoi
    widget_control, (*pState).ResultTable, use_table_select=[0,lastrow+MAROINo,2,lastRow+MAROINo+1], $
      set_value=[string(oldROINum+MAROINo,format='(i0)'),string([area,distance],format='(f0.2)')]
  endfor
End
;-----------------------------------------------------------------
pro ImportROI_event, event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  case event.id of
  widget_info(event.top,find_by_uname='Done'): begin
    widget_control, event.top, get_uvalue=pLocal
    if widget_info((*pLocal).ManualAdd,/button_set) then MOption= 'Add' $
    else if widget_info((*pLocal).ManualIgnore,/button_set) then MOption= 'Ignore' $
    else MOption = 'Replace'
    if widget_info((*pLocal).AutoAdd,/button_set) then AOption= 'Add' $
    else if widget_info((*pLocal).AutoIgnore,/button_set) then AOption= 'Ignore' $
    else AOption = 'Replace'
    widget_control, event.top, /destroy

    if MOption eq 'Replace' then DeleteManualROI, (*pLocal).pState
    if AOption eq 'Replace' then DeleteAutoROI, (*pLocal).pState
    if AOption ne 'Ignore' then ImportAutoROI,(*pLocal).pState,(*pLocal).ROIMap,(*pLocal).MAROINum
    if MOption ne 'Ignore' then ImportManualROI,(*pLocal).pState,(*pLocal).ROIMap,(*pLocal).MMROINum

    (*(*pLocal).pState).oWindow->Draw,(*(*pLocal).pState).oView
    idROISlider = widget_info((*(*pLocal).pState).AnimationBase, find_by_uname='ROISlider')
    widget_control, idROISlider, set_slider_max=(*(*pLocal).pState).result->length(),set_value=1
    if (*(*pLocal).pState).result->length() gt 0 then widget_control, idROISlider,/sensitive
  end
  widget_info(event.top,find_by_uname='Cancel'): begin
    widget_control, event.top, /destroy
  end
  else:
  endcase
end
;-----------------------------------------------------------------

PRO OnImportROINew, event
;COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,'Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF


; Load the ROIs saved from a user-selected file.

	WIDGET_CONTROL, event.top, GET_UVALUE=pState
	fname = DIALOG_PICKFILE(GROUP=event.top, $
		FILE=strmid((*pState).file[0], 0, strpos((*pState).file[0], '.lsm'))+'-ROI Map.sav', $
		FILTER='*.sav', /READ)
	IF FILE_TEST(fname) THEN BEGIN
		RESTORE, fname
	ENDIF ELSE BEGIN
		void = DIALOG_MESSAGE('The selectd file is not found!',/ERROR)
		RETURN
	ENDELSE
; Clear current data

	ROINum =(*pState).result->length()
	IF ROINum lt 0 THEN BEGIN
		FOR ROINo=1,ROInum DO BEGIN
			info = (*pState).result->GetProperty(ROINo-1,oROI=oROI,oLabel=oLabel)
			(*pState).oLabelModel->Remove, oLabel
			oROI->GetProperty, parent=oParent
			oParent->remove, currentRoi
			info = (*pState).result->Delete(ROINo-1)
		ENDFOR
	ENDIF
	IF OBJ_VALID(SavROI.oAutoROI[0]) THEN ANum = N_ELEMENTS(SavROI.oAutoROI) $
		ELSE ANum = 0
	IF OBJ_VALID(SavROI.oManualROI[0]) THEN MNum = N_ELEMENTS(SavROI.oManualROI) $
		ELSE MNum = 0
	ROINum = ANum + MNum

;	SavROI = {oAutoROI: oAutoROI, $
;			  oManualROI: oManualROI, $
;			  oROINum: oROINum $
;			  }
	i = 0 & j = 0
	for ROINo=0,ROINum-1 do begin
;		indices = where(ROIMap eq MROINo+1)
;		if indices[0] eq -1 then continue
;		MMROINo++
;		boundry = Find_Boundary(indices, XSize=(*pState).xs, YSize=(*pState).ys)
;		newRoi = obj_new('IDLgrROI', boundry[0,*], boundry[1,*], color=[255,0,0], alpha_channel=1)
		IF SavROI.oROINum[ROINo] EQ 0 THEN BEGIN
			newROI = SavROI.oAutoROI[i]
			(*pState).oAutoROIModel->Add, newRoi
			i = i + 1
		ENDIF ELSE BEGIN
			newROI = SavROI.oManualROI[j]
			j = j + 1
			(*pState).oManualROIModel->Add, newRoi
		ENDELSE
		result = newRoi->ComputeGeometry(centroid=center,area=area)
		area *= ((*pState).pxs)^2 ;& print,MMROINo, area
		distance = ptr_valid((*pState).mask) ? DistToEdge(*(*pState).edge,center)*(*pState).pxs : 0
		roiMask = newROI->ComputeMask(dimensions=[(*pState).xs,(*pState).ys])
		indices = where(roiMask gt 0)
		trace = GetTrace(pState,indices)
		oLabel = obj_new('IDLgrText', string((*pState).result->Length()+1,format='(i0)'), $
		font=(*pState).oROIFont, color=[255,255,0], locations=center)
		(*pState).oLabelModel->Add, oLabel
		node = obj_new('ChainNode')
		node->SetProperty, oROI=newRoi, oLabel = oLabel, trace=trace, ROIPara=[Area,distance], flashNum=1
		info = (*pState).result->insert(node)

		widget_control, (*pState).ResultTable, use_table_select=[0,ROINo,2,ROINo+1], $
		set_value=[string(ROINo,format='(i0)'),string([area,distance],format='(f0.2)')]
	endfor


		(*pState).oWindow->Draw, (*pState).oView

		; If this is the first region, bring up the
		; region information dialog.
	idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
	widget_control, idROISlider, set_slider_max=(*pState).result->length(),set_value=1
	if (*pState).result->length() gt 0 then widget_control, idROISlider,/sensitive




END

;-----------------------------------------------------------------
Pro OnImportROI, Event
COMPILE_OPT STRICTARR
  CATCH, Error_status
  IF Error_status NE 0 THEN BEGIN
    PRINT,'Error message: ', !ERROR_STATE.MSG
    return
    CATCH, /CANCEL
  ENDIF

; MA: AutoROI on ROI Map
; MM: MunualROI on ROI Map
; CA: Current AutoROI
; CM: Current MunualROI
  widget_control, event.top, get_uvalue=pState

  ; read ROI map
  file = strmid((*pState).file[0], 0, strpos((*pState).file[0], '.lsm'))+'-ROI Map.dat'
  ROIMapFile = dialog_pickfile(filter='*.dat',file=file,/read,/must_exist)
  if ROIMapFile[0] eq '' then return
  if ~file_test(ROIMapFile[0]) then return
  header = bytarr(7)
  openr, unit, ROIMapFile, /get_lun
  readu, unit, header
  if string(header) ne 'ROI Map' then begin
    void = dialog_message('The selected .dat file is not a ROI map file!',/error)
    close, unit & free_lun, unit
    return
  endif
  MROINum = 0L & MMROINum = 0L & MAROINum = 0L
  readu, unit, MROINum, MMROINum, MAROINum
  if MROINum ne MMROINum+MAROINum then begin
    void = dialog_message('The ROI numbers are not consistent-1!',/error)
    close, unit & free_lun, unit
    return
  endif
  if MROINum lt 1 then begin
    void = dialog_message('There is no ROI on the ROI map!',/error)
    close, unit & free_lun, unit
    return
  endif
  XS = 0L & YS = 0L
  readu, unit, XS, YS
  if XS ne (*pState).xs or YS ne (*pState).ys then begin
    void = dialog_message('The ROI map size is not consistent with current image!',/error)
    close, unit & free_lun, unit
    return
  endif
  ROIMap = intarr(2,xs,ys)
  readu, unit, ROIMap
  close, unit & free_lun, unit
  if MROINum ne max(ROIMap[0,*,*]) then begin
    void = dialog_message('The ROI numbers are not consistent-2!',/error)
    return
  endif
;  window,1,xs=xs,ys=ys & tvscl,ROIMap[0,*,*] & return

  ; get current ROI numbers
  CMROINum = 0 & CAROINum=0
  if obj_valid((*pState).result) then begin
    ROINum =(*pState).result->length()
    if ROINum gt 0 then begin
      for ROINo=1,ROINum do begin
        info = (*pState).result->GetProperty(ROINo-1,oROI=oROI)
        oROI->GetProperty,parent=parent
        if parent eq (*pState).oAutoROIModel then $
          CAROINum++ else CMROINum++
      endfor
    endif
  endif

  ; choose options
  Resolve_Routine, 'ImportROI_event',/COMPILE_FULL_FILE  ; Load event callback routines

  ImportROIBase = Widget_Base( GROUP_LEADER=(*pState).MainBase ,XOFFSET=5 ,YOFFSET=9 ,SCR_XSIZE=280  $
      ,SCR_YSIZE=300 ,TITLE='Import ROI options' ,SPACE=3 ,XPAD=3 ,YPAD=3, tlb_frame_attr=1, /modal)
  WID_LABEL_0 = Widget_Label(ImportROIBase ,XOFFSET=80 ,YOFFSET=20 ,/ALIGN_LEFT ,VALUE='Manual ROI')
  WID_LABEL_1 = Widget_Label(ImportROIBase ,XOFFSET=180 ,YOFFSET=20 ,/ALIGN_LEFT ,VALUE='Auto ROI')
  WID_LABEL_2 = Widget_Label(ImportROIBase ,XOFFSET=20 ,YOFFSET=50 ,/ALIGN_LEFT ,VALUE='Current')
  WID_LABEL_3 = Widget_Label(ImportROIBase ,XOFFSET=20 ,YOFFSET=85 ,/ALIGN_LEFT ,VALUE='ROI Map')
  CM_TEXT = Widget_Text(ImportROIBase, UNAME='CM' ,XOFFSET=95 ,YOFFSET=48,XSIZE=3 ,YSIZE=1, value=string(CMROINum,format='(i3)'))
  MM_TEXT = Widget_Text(ImportROIBase, UNAME='MM' ,XOFFSET=95 ,YOFFSET=83,XSIZE=3 ,YSIZE=1, value=string(MMROINum,format='(i3)'))
  CA_TEXT = Widget_Text(ImportROIBase, UNAME='CA' ,XOFFSET=190,YOFFSET=48 ,XSIZE=3 ,YSIZE=1, value=string(CAROINum,format='(i3)'))
  MA_TEXT = Widget_Text(ImportROIBase, UNAME='MA' ,XOFFSET=190,YOFFSET=83 ,XSIZE=3 ,YSIZE=1, value=string(MAROINum,format='(i3)'))

  ManualOptions = Widget_Base(ImportROIBase, UNAME='ManualOptions'  $
      ,XOFFSET=75 ,YOFFSET=120 ,COLUMN=1 ,/EXCLUSIVE)
  ManualAdd = Widget_Button(ManualOptions,/ALIGN_LEFT ,VALUE='Add')
  ManualReplace = Widget_Button(ManualOptions,YOFFSET=22 ,/ALIGN_LEFT ,VALUE='Replace')
  ManualIgnore = Widget_Button(ManualOptions,YOFFSET=44 ,/ALIGN_LEFT ,VALUE='Ignore')

  AutoOptions = Widget_Base(ImportROIBase, UNAME='AutoOptions'  $
      ,XOFFSET=170 ,YOFFSET=120 ,TITLE='IDL' ,COLUMN=1 ,/EXCLUSIVE)
  AutoAdd = Widget_Button(AutoOptions,/ALIGN_LEFT ,VALUE='Add')
  AutoReplace = Widget_Button(AutoOptions,YOFFSET=22 ,/ALIGN_LEFT ,VALUE='Replace')
  AutoIgnore = Widget_Button(AutoOptions,YOFFSET=44 ,/ALIGN_LEFT ,VALUE='Ignore')

  Done = Widget_Button(ImportROIBase, UNAME='Done' ,XOFFSET=50  $
      ,YOFFSET=220 ,SCR_XSIZE=60 ,SCR_YSIZE=25 ,/ALIGN_CENTER ,VALUE='Done')
  Cancel = Widget_Button(ImportROIBase, UNAME='Cancel' ,XOFFSET=160  $
      ,YOFFSET=220 ,SCR_XSIZE=60 ,SCR_YSIZE=25 ,/ALIGN_CENTER ,VALUE='Cancel')

  Widget_Control, /REALIZE, ImportROIBase

  if MMROINum eq 0 then begin
    widget_control, ManualIgnore, /set_button
  endif else begin
    widget_control, ManualAdd, /set_button
  endelse
  if MAROINum eq 0 then begin
    widget_control, AutoIgnore, /set_button
  endif else begin
    if CAROINum eq 0 then $
      widget_control, AutoAdd, /set_button $
    else widget_control, AutoReplace, /set_button
  endelse
  local = {pState: pState, $
           ROIMap: ROIMap, $
           MMROINum: MMROINum, $
           MAROINum: MAROINum, $
           ManualAdd: ManualAdd, $
           ManualReplace: ManualReplace, $
           ManualIgnore: ManualIgnore, $
           AutoAdd: AutoAdd, $
           AutoReplace: AutoReplace, $
           AutoIgnore: AutoIgnore}
  widget_control, ImportROIBase, set_uvalue=ptr_new(local,/no_copy)
  XManager, 'OnImportROI', ImportROIBase,EVENT_HANDLER='ImportROI_event', /NO_BLOCK
  idROISlider = widget_info((*pState).AnimationBase,find_by_uname='ROISlider')
  widget_control,idROISlider,set_value=1,set_slider_min=1,set_slider_max=(*pState).result->length()
End
;-----------------------------------------------------------------
Pro OnExportROINew, Event, silent=silent
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

;ROIMap file structure:
;'ROI Map', ROINum:long, ManualROINum:long, AutoROINum:long, XS:long, YS:long, ROIMAP:int(2,XS,YS)

  widget_control, event.top, get_uvalue=pState
  if ~obj_valid((*pState).result) then begin
    void = dialog_message('There is no ROI to export!',/error)
    return
  endif
;  ROIMapFile = strmid((*pState).file[0], 0, strpos((*pState).file[0], '.lsm'))+'-ROI Map.dat'
;  if ~keyword_set(silent) then begin
;    ROIMapFile = dialog_pickfile(filter='*.dat',file=ROIMapfile,/write)
;    if ROIMapFile[0] eq '' then return
;    if file_test(ROIMapFile) then begin
;      if dialog_message('Overwrite existing ROI map?',/question) eq 'No' then return
;    endif
;  endif

  ROINum =(*pState).result->length()
  if ROINum lt 1 then begin
    void = dialog_message('There is no ROI to export!',/error)
    return
  endif
  info = (*pState).oAutoROIModel->Get(/ALL, COUNT=n)
  AutoROINum = n
  info = (*pState).oManualROIModel->Get(/ALL, COUNT=n)
  ManualROINum = n
;  ManualROINum = 0L & AutoROINum = 0L
;  ROIMap = intarr(2,(*pState).xs,(*pState).ys)
;  for ROINo=1,ROINum do begin
;    info = (*pState).result->GetProperty(ROINo-1,oROI=oROI)
;    mask = oROI->ComputeMask(dimensions=[(*pState).xs,(*pState).ys]) gt 0
;    ROIMap[0,*,*] += mask*ROINo
;    oROI->GetProperty,parent=parent
;    if parent eq (*pState).oAutoROIModel then begin
;      ROIMap[1,*,*] += mask
;      AutoROINum++
;    endif else begin
;      ROIMap[1,*,*] += mask*2
;      ManualROINum++
;    endelse
;  endfor
  IF AutoROINum GT 0 THEN $
  	oAutoROI = OBJARR(AutoROINum) $
  ELSE  $
  	oAutoROI = OBJ_NEW()
  IF ManualROINum GT 0 THEN $
    oManualROI = OBJARR(ManualROINum) $
  ELSE $
  	oManualROI = OBJ_NEW()
    oROINum = INTARR(ROINum)
  i = 0 & j = 0
  FOR ROINo=1,ROINum DO BEGIN
	info = (*pState).result->GetProperty(ROINo-1,oROI=oROI);,flashPara=flashPara,flashNum=flashNum)
	oROI->GetProperty, parent=parent
	IF parent EQ (*pState).oAutoROIModel THEN BEGIN
		parent->Remove, oROI
		oAutoROI[i] = oROI
		oROINum[ROINo-1] = 0
		i = i + 1
	ENDIF ELSE BEGIN
		parent->Remove, oROI
		oManualROI[j] = oROI
;		oManualROINum[j] = ROINo
		oROINum[ROINo-1] = 1
		j = j + 1
	ENDELSE
  ENDFOR
  WIDGET_CONTROL, (*pState).resulttable, GET_VALUE=ResultTable
	SavROI = {oAutoROI: oAutoROI, $
			  oManualROI: oManualROI, $
;			  ResultTable: ResultTable, $
			  oROINum: oROINum $
			  }
	fname = DIALOG_PICKFILE($
				FILE=strmid((*pState).file[0], 0, strpos((*pState).file[0], '.lsm'))+'-ROI Map.sav', $
				FILTER='*.sav', /WRITE)
	IF (STRLEN(fname) gt 0) then begin
		SAVE, SavROI, FILENAME=fname
	ENDIF
	i = 1 & j = 1
  FOR ROINo=1,ROINum DO BEGIN
		IF SavROi.oROINum[i-1] EQ 0 THEN BEGIN
			newROI = SavROI.oAutoROI[i-1]
			(*pState).oAutoROIModel->Add, newRoi
			i = i + 1
		ENDIF ELSE BEGIN
			newROI = SavROI.oManualROI[j-1]
			j = j + 1
			(*pState).oManualROIModel->Add, newRoi
		ENDELSE
  ENDFOR
  (*pState).oWindow->Draw, (*pState).oView
;  openw, unit, ROIMapFile, /get_lun
;  writeu, unit, 'ROI Map', oAutoROI, oManualROI
;  close, unit & free_lun, unit

End
;-----------------------------------------------------------------
Pro OnExportROI, Event, silent=silent
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

;ROIMap file structure:
;'ROI Map', ROINum:long, ManualROINum:long, AutoROINum:long, XS:long, YS:long, ROIMAP:int(2,XS,YS)

  widget_control, event.top, get_uvalue=pState
  if ~obj_valid((*pState).result) then begin
    void = dialog_message('There is no ROI to export!',/error)
    return
  endif
  ROIMapFile = strmid((*pState).file[0], 0, strpos((*pState).file[0], '.lsm'))+'-ROI Map.dat'
  if ~keyword_set(silent) then begin
    ROIMapFile = dialog_pickfile(filter='*.dat',file=ROIMapfile,/write)
    if ROIMapFile[0] eq '' then return
    if file_test(ROIMapFile) then begin
      if dialog_message('Overwrite existing ROI map?',/question) eq 'No' then return
    endif
  endif

  ROINum =(*pState).result->length()
  if ROINum lt 1 then begin
    void = dialog_message('There is no ROI to export!',/error)
    return
  endif
  ManualROINum = 0L & AutoROINum = 0L
  ROIMap = intarr(2,(*pState).xs,(*pState).ys)
  for ROINo=1,ROINum do begin
    info = (*pState).result->GetProperty(ROINo-1,oROI=oROI)
    mask = oROI->ComputeMask(dimensions=[(*pState).xs,(*pState).ys]) gt 0
    ROIMap[0,*,*] += mask*ROINo
    oROI->GetProperty,parent=parent
    if parent eq (*pState).oAutoROIModel then begin
      ROIMap[1,*,*] += mask
      AutoROINum++
    endif else begin
      ROIMap[1,*,*] += mask*2
      ManualROINum++
    endelse
  endfor

  openw, unit, ROIMapFile, /get_lun
  writeu, unit, 'ROI Map',long(ROINum), ManualROINum, AutoROINum, long((*pState).xs), long((*pState).ys), ROIMap
  close, unit & free_lun, unit

End
;-----------------------------------------------------------------
pro OnExit, Event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  widget_control, event.top, get_uvalue=pState
  if ptr_valid(pState) then begin
    if obj_valid((*pState).result) then obj_destroy, (*pState).result   ; destroy objects except oWindow
    if obj_valid((*pState).oView) then begin
      obj_destroy, (*pState).oView
      (*pState).oWindow->RemoveWindowEventObserver, (*pState).oObserver
      obj_destroy, (*pState).oObserver
    endif
  endif
  print, 'Exit Flash Sniper : Destroy objects'
  obj_destroy, obj_valid()
  print, 'Exit Flash Sniper : Destroy pointers'
  PTR_FREE, PTR_VALID()   ; destroy pointers
  print, 'Exit Flash Sniper : Destroy main interface'
  widget_control, event.top, /destroy

end
;-----------------------------------------------------------------
Function GetTrace, pState, index
COMPILE_OPT STRICTARR

  ori_trace = fltarr((*pState).stackNum,(*pState).ts) & norm_trace = ori_trace
;  valid = where(index ge 0)
;  if valid[0] eq -1 then return, [[ptr_new(ori_trace,/no_copy)],[ptr_new(norm_trace,/no_copy)]]
;  index = index[valid]
  n = n_elements(index)
  xys = double((*pstate).xs)*(*pstate).ys
;  t1 = systime(1)
  for k=0,(*pState).stackNum-1 do begin
    for i=0,(*pState).ts-1 do begin
      ori_trace[k,i] = total((*(*pState).img[k])[index+i*xys],/double)/n
    endfor
;    ori_trace[k,*] = smooth(median(reform(ori_trace[k,*]),3),3,edge=1)
  endfor
;  t2 = systime(1)
;  print, t2-t1
  if (where(ori_trace[*,0] le 0))[0] eq -1 then norm_trace = ori_trace/rebin(ori_trace[*,0],(*pState).stackNum,(*pState).ts)
  return, [[ptr_new(ori_trace,/no_copy)],[ptr_new(norm_trace,/no_copy)]]
End
;-----------------------------------------------------------------

Function TraceFit, time, trace, cutoff
COMPILE_OPT STRICTARR

  ; correct the bent at head and tail after fft filtration
  ts = n_elements(trace)
  extension = 30
  head_fit_para = ladfit(time[0:extension],trace[0:extension])
  head_x = time[0:extension]-(time[extension]-time[0])
  head = head_fit_para[0] + head_fit_para[1]*head_x
  tail_fit_para = ladfit(time[ts-extension+1:ts-1],trace[ts-extension+1:ts-1])
  tail_x = time[ts-extension+1:ts-1] + (time[ts-1]-time[ts-extension+1])
  tail = tail_fit_para[0] + tail_fit_para[1]*tail_x
  signal = [head, trace, tail]

  filter = BUTTERWORTH(n_elements(signal),cutoff=cutoff,order=3)
  yfit = FFT( FFT(signal, -1)*filter, 1 )
  fitTrace = yfit[extension+1:ts+extension]
  return, fitTrace
end
;-----------------------------------------------------------------
Function BaseLineFit, time, trace
COMPILE_OPT STRICTARR

  ts = n_elements(trace)
  para = ladfit(time,trace)
  baseLine = para[0]+para[1]*time
  return, baseline
  eLine = trace-baseline
  ePeak = max(eLine,peakPos)
  pre = where(indgen(ts) lt (peakPos-5))
  post = where(indgen(ts) gt (peakPos+10))
  if pre[0] eq -1 then begin
    rest = post
  endif else begin
    rest = post[0] eq -1 ? pre : [pre,post]
  endelse
  para = ladfit(time[rest],trace[rest])
  baseLine = para[0]+para[1]*time
  return, baseline

end
;-----------------------------------------------------------------
Pro PlotTrace, pState, trace, title=title, frame=frame, flashPara=flashPara, fit=fit
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  device, decomposed=1
  trace_max = max(*trace, min=trace_min)
  yrange = trace_max gt 5 ? [trace_min-5,trace_max+5] : [trace_min-0.2,trace_max+0.2]
  xrange = [0,(*(*pState).correctedTime)[(*pState).ts-1]]
  wset, (*pState).TraceWindow
  plot, *(*pState).correctedTime, replicate(0,(*pState).ts), background='EDEDDD'XL, color='000000'XL,thick=0.5, $
    xrange=xrange,yrange=yrange,xstyle=1,ystyle=1, title=title, xtitle='Time (s)', ytitle='F', font=0, $
    pos=[50,45,!D.x_size-20,!D.y_size-30],/dev
  peakThr = (*pState).Thr[0]*(*pState).sigma[0]+(*pState).mean[0]
  for i=0,(*pState).stackNum-1 do begin
    signal = reform((*trace)[i,*])
    newsignal = smooth(signal,3,edge=1)
    color = reform((*pState).stackColor[*,i]) & color = total(color*[1.0,256.0,256.0^2])
    oplot, *(*pState).correctedTime, reform((*trace)[i,*]), color=color, thick=1.5
;    oplot, *(*pState).correctedTime, newsignal, color='CC0000'XL, thick=1.5
  endfor

;flashPara [   0   ,  1  , 2 ,  3 ,   4   ,  5   , 6 ,  7  ,  8 ,    9   ,  10 ,  11 ,  12  , 13 , 14 ,  15 , 16 , 18, 19]
;flashPara [FlashNo, Peak, F0, Amp, Tstart, Tpeak, RT, RT90, T50, EndBase, Note, FAHM, dBase, T75, T90, FDHM, Dia, t1, t2]
  if n_elements(flashPara) ne 0 then begin
    if ptr_valid(flashPara) then begin
      flashNum = n_elements(*flashPara)/20
      for flashNo=0,flashNum-1 do begin
        oplot, [(*flashPara)[4,flashNo]-2,(*flashPara)[4,flashNo]+2],replicate((*flashPara)[2,flashNo],2), color='0000FF'XL ;F0--
        oplot, replicate((*flashPara)[4,flashNo],2), [(*flashPara)[2,flashNo]-2,(*flashPara)[2,flashNo]+2], color='0000FF'XL ;Tstart|
        oplot, replicate((*flashPara)[5,flashNo],2), [(*flashPara)[1,flashNo]-2,(*flashPara)[1,flashNo]+2], color='0000FF'XL ;Tpeak|
        oplot, [(*flashPara)[5,flashNo],(*flashPara)[5,flashNo]+(*flashPara)[8,flashNo]] $
          ,replicate((*flashPara)[2,flashNo]*(1+0.5*(*flashPara)[3,flashNo]),2), color='0000FF'XL ;T50--
        oplot, [(*(*pState).correctedTime)[(*flashPara)[19,flashNo]]-2,(*(*pState).correctedTime)[(*flashPara)[19,flashNo]]+2] $
          ,[(*flashPara)[9,flashNo],(*flashPara)[9,flashNo]], color='0000FF'XL
      endfor
    endif
  endif

  widget_control, (*pState).TraceDraw, get_uvalue=pixWin
  wset, pixWin.id
  device, copy=[0,0,!D.x_size,!D.y_size,0,0,(*pState).TraceWindow]
  pixWin.tag = 1b
  widget_control, (*pState).TraceDraw, set_uvalue=pixWin
  wset, (*pState).TraceWindow
End
;-----------------------------------------------------------------
pro OnSetPlotProperty, Event
COMPILE_OPT STRICTARR

End
;------------------------------------------------------------------
Function DistToEdge, edge, p
COMPILE_OPT STRICTARR

;	x1 = (p[0]-dim[1]/2) > 0
;	x2 = (p[0]+dim[1]/2) < (dim[0]-1)
;	localMask = bytarr(dim[0],dim[1])
;	localMask[x1:x2,*] = 1
;	tempEdge *= localMask
;	window,11,xs=900,ys=256 & tvscl,tempEdge
	distance = sqrt((edge[0,*]-p[0])^2 + (edge[1,*]-p[1])^2)
	return, min(distance)
End


;-----------------------------------------------------------------
pro TraceDrawInitialize, TraceDraw
;COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  widget_control, TraceDraw, get_uvalue=pixWin, get_value=TraceWindow
  wset,TraceWindow & wshow,TraceWindow
  newPic = congrid(pixWin.pic,3,!D.x_size,!D.y_size)
;  print,!D.x_size,!D.y_size
  tv, newPic, true=1
  wset, pixWin.id
  device, copy=[0,0,!D.x_size,!D.y_size,0,0,TraceWindow]
  pixWin.tag = 0b
  widget_control, TraceDraw, set_uvalue=pixWin
end

;-----------------------------------------------------------------
Function GaussianMask, s
COMPILE_OPT STRICTARR

  R = round(3*s+0.5)  ; cutoff radius
  M = dblarr(2*R+1, 2*R+1)
  for i=-R, R do begin
    for j=-R, R do begin
      M[i+R,j+R] = exp( -(float(i)^2+float(j)^2)/2/s/s )/(2*!Pi*s*s)
    endfor
  endfor
  M /= total(M)
  return, M
End
;-----------------------------------------------------------------
Pro ImageFiltration, pState, stackNo=stackNo
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  M = GaussianMask(0.7)
  device, decomposed=0
  tvlct, 0,200,0,41
  timer = Obj_New("ShowProgress", (*pState).MainBase, Color=41,title=' ',  $
      message='Filtering data in progress...', xsize=400, ysize=25)
  timer->start
  totalN = (*pState).stackNum*(*pState).ts
  for k=0,(*pState).stackNum-1 do begin
    subN = k*(*pState).ts
    if n_elements(stackNo) gt 0 then begin
      if (where(stackNo eq k+1))[0] eq -1 then continue
    endif
    for i=0,(*pState).ts-1 do begin
      (*(*pState).img[k])[0,0,i] = convol(median((*(*pState).img[k])[*,*,i],3), M, /center, /edge_truncate)
      if i mod 10 eq 0 then begin
        percent = (i+subN)*100/totalN
        timer->update, percent
      endif
    endfor
  endfor
  timer->Destroy
end
;-----------------------------------------------------------------
Pro OnImageFiltration, Event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  if dialog_message('Are you sure to perform image filtration?',/question) eq 'No' then return
  widget_control,event.top,get_uvalue=pState
  if ~ptr_valid(pState) then return
  ImageFiltration, pState
  ;destroy old animation model
  (*pState).oWindow->RemoveWindowEventObserver, (*pState).oObserver
  obj_destroy, (*pState).oObserver
  (*pState).oView->Remove,  (*pState).oAnimationModel
  obj_destroy, (*pState).oAnimationModel
  ; creat new animation model and observer
  CreatAnimationModel,pState
End
;-----------------------------------------------------------------
Pro OnSpatialCrop, Event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  if dialog_message('Are you sure to crop the image stack in spatial domain?' ,/question) eq 'No' then return

  widget_control, event.top, get_uvalue=pState
  window,0,xs=(*pState).xs,ys=(*pState).ys,title='Left click: play movie    Right click: select ROI'
  tvscl, (*(*pState).img[0])[*,*,0]
  while 1b do begin
    cursor,x,y, /up
    if !mouse.button eq 4 then break
    for k=1,(*pState).ts-1 do begin
      tv, (*(*pState).img[0])[*,*,k] & wait, 0.05
    endfor
  endwhile
  box_cursor,x0,y0,nx,ny
  wdelete,0

  x1 = (x0>0)<((*pState).xs-1)
  x2 = ((x0+nx)>0)<((*pState).xs-1)
  y1 = (y0>0)<((*pState).ys-1)
  y2 = ((y0+ny)>0)<((*pState).ys-1)
  if x1 gt x2 then begin
    temp=x1 & x1=x2 & x2=temp
  endif
  if y1 gt y2 then begin
    temp=y1 & y1=y2 & y2=temp
  endif
  for i=0, (*pState).stackNum-1 do $
    *(*pState).img[i] = (*(*pState).img[i])[x1:x2,y1:y2,*]
  *(*pState).mask = (*(*pState).mask)[x1:x2,y1:y2]
  (*pState).xs = x2-x1+1 & (*pState).ys = y2-y1+1

  if widget_info((*pState).AnimationBase, /valid_id) then begin  ; destroy old animation base
    (*pState).oWindow->RemoveWindowEventObserver, (*pState).oObserver
    obj_destroy, (*pState).oObserver
    (*pState).oWindow->SetProperty, Graphics_Tree=obj_new()
    obj_destroy, (*pState).oWindow
    widget_control, (*pState).AnimationBase, /destroy
    for k=0,(*pState).stackNum-1 do (*pState).oView->Remove,(*pState).oAnimationModel[k]
  endif
  CreatAnimationBase, pState
  CreatAnimationModel, pState

  if x0 eq 0 and y0 eq 0 then return
  if ~obj_valid((*pState).result) then return
  ROINum = (*pState).result->length()
  if ROINum gt 0 then begin
    for k=1,ROINum do begin
      info = (*pState).result->GetProperty(k-1, oROI=oROI, oLabel=oLabel)
      if obj_valid(roi) then roi->Translate, -x0, -y0
      ;if roi is out of the current image range, delete that
      void = oROI->ComputeGeometry(centroid=centroid)
      olabel->SetProperty, locations=centroid
    endfor
  endif
  (*pState).oWindow->Draw, (*pState).oView

End

;-----------------------------------------------------------------
Pro OnTemporalCrop, Event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  Widget_control,Event.top,get_uvalue=pState

  GetValidFrame,pState,group_leader=event.top
  if n_elements(*(*pState).ValidFrame) eq (*pState).ts then return

  for k=0,(*pState).stackNum-1 do *(*pState).img[k] = (*(*pState).img[k])[*,*,*(*pState).validFrame]
  (*pState).ts = n_elements(*(*pState).validFrame)
  *(*pState).time = (*(*pState).time)[*(*pState).validFrame]
  *(*pState).correctedTime = (*(*pState).correctedTime)[*(*pState).validFrame]
  *(*pState).back = (*(*pState).back)[*,*(*pState).validFrame]

  ; destroy old animation base
  if widget_info((*pState).AnimationBase, /valid_id) then begin
    (*pState).oWindow->RemoveWindowEventObserver, (*pState).oObserver
    obj_destroy, (*pState).oObserver
    (*pState).oWindow->SetProperty, Graphics_Tree=obj_new()
    obj_destroy, (*pState).oWindow
    widget_control, (*pState).AnimationBase, /destroy
    for k=0,(*pState).stackNum-1 do (*pState).oView->Remove,(*pState).oAnimationModel[k]
  endif
  CreatAnimationBase, pState
  CreatAnimationModel, pState

  if ~obj_valid((*pState).result) then return
  ROINum = (*pState).result->length()
  if ROINum gt 0 then begin
    widget_control, widget_info((*pState).AnimationBase,find_by_uname='ROISlider'),get_value=ROINo
    for k=1,ROINum do begin
      info = (*pState).result->GetProperty(k-1, oROI=oROI)
      roiMask = oROI->ComputeMask(dimensions=[(*pState).xs,(*pState).ys])
      indices = where(roiMask gt 0)
      trace = GetTrace(pState, indices)
      info = (*pState).result->SetProperty(roiNo, trace=trace)
      if k eq ROINo then begin
        wset, (*pState).TraceWindow & PlotTrace, pState, trace[0],title=file_basename((*pState).file[0])+'  ROI '+strtrim(ROINo,1)
      endif
    endfor
  endif

End
;-----------------------------------------------------------------------------------------
Function AdjustContrast, img, Brightness, Contrast
COMPILE_OPT STRICTARR

  new_img = (double(img)-127)*Contrast+127+ brightness
  new_img = new_img > 0
  new_img = new_img < 255
  return, round(new_img)
End
;-----------------------------------------------------------------
pro OnAdjustContrast, Event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  widget_control, event.top, get_uvalue=pState
  old_brightness = (*pState).brightness
  old_contrast = (*pState).contrast
  para = [old_brightness,old_contrast]
  para = AdjustImage((*(*pState).img[0])[*,*,0], para, group_leader=event.top)
  print,para
  (*pState).brightness = para[0]
  (*pState).contrast = para[1]
  print, para
  if (*pState).brightness ne old_brightness or (*pState).contrast ne old_contrast then begin
    (*pState).oWindow->RemoveWindowEventObserver, (*pState).oObserver
    obj_destroy, (*pState).oObserver
    obj_destroy, (*pState).oAnimationModel
    CreatAnimationModel, pState
  endif
end

;-----------------------------------------------------------------
;;Designed by suntao,20150711
pro OnMotionCompensation_ST,event
  COMPILE_OPT STRICTARR
  CATCH, Error_status
  IF Error_status NE 0 THEN BEGIN
    PRINT,'  Error message: ', !ERROR_STATE.MSG
    return
    CATCH, /CANCEL
  ENDIF

  widget_control, event.top, get_uvalue=pState
  widget_control,widget_info((*pState).AnimationBase,find_by_uname='FrameSlider'),get_value=refFrame
  ;if SetHBMPara(pState,refFrame) eq 0 then return

  answer = dialog_message('Apply motion compensation to all channels?', /question)
  if answer ne 'Yes' then return
  
  temp=image_register_3d(smooth(*(*pState).img[0],3),type=2,offset=offset,crop=1) 
  offsetx=reform(offset[0,*])
  offsety=reform(offset[1,*])
  print,offset
  ;;Affine transform with the fitted parameters
  offsetx_valid=offsetx
  x=findgen(n_elements(offsetx))
  ans=poly_fit(x,offsetx_valid,2,/double,yfit=yfit_x)
  
  offsety_valid=offsety
  x=findgen(n_elements(offsetx))
  ans=poly_fit(x,offsety_valid,2,/double,yfit=yfit_y)
  
    xoffset_max=round(max(offset[0,*]))
    yoffset_max=round(max(offset[1,*]))
    xoffset_min=round(min(offset[0,*]))
    yoffset_min=round(min(offset[1,*]))
     
  for i=0,(*pState).stackNum-1 do begin
    for k=(*(*pState).pHBMPara).trackStart,(*(*pState).pHBMPara).trackEnd do begin
      t=affine_translation(-yfit_x[k],-yfit_y[k])
      ;print,'K',k
      img_re = affine_transform(reform((*(*pState).img[i])[*,*,k]), t,missing=0)
      if xoffset_max gt 0 then img_re[0:xoffset_max,*]=img_re[0:xoffset_max,*]*0
      if yoffset_max gt 0 then img_re[*,0:yoffset_max]=img_re[*,0:yoffset_max]*0
      if xoffset_min lt 0 then img_re[sz[0]+xoffset_min:sz[0]-1,*]*=0
      if yoffset_min lt 0 then img_re[*,sz[0]+yoffset_min:sz[0]-1]*=0
      (*(*pState).img[i])[*,*,k]=img_re
      ;tvscl,(*(*pState).img[i])[*,*,k]
    endfor
  endfor
  (*pState).oWindow->RemoveWindowEventObserver, (*pState).oObserver
  obj_destroy, (*pState).oObserver
  obj_destroy, (*pState).oAnimationModel
  CreatAnimationModel, pState
end
;--------------
pro OnMotionCompensation, Event
COMPILE_OPT STRICTARR
  CATCH, Error_status
  IF Error_status NE 0 THEN BEGIN
    PRINT,'  Error message: ', !ERROR_STATE.MSG
    return
    CATCH, /CANCEL
  ENDIF

  widget_control, event.top, get_uvalue=pState
  widget_control,widget_info((*pState).AnimationBase,find_by_uname='FrameSlider'),get_value=refFrame
  if SetHBMPara(pState,refFrame) eq 0 then return

  device,decomposed=1
  show = fltarr(3,(*pState).xs,(*pState).ys)
  show[0,*,*] = bytscl((*(*pState).img[0])[*,*,refFrame])
  window,0,xs=(*pState).xs,ys=(*pState).ys
  tv, show, true=1
  box_cursor,x0,y0,nx,ny

  roi = [x0,x0+nx,y0,y0+ny]
  extend_roi = [(x0-60)>0,(x0+nx+60)<((*pState).xs-1),(y0-30)>0,(y0+ny+30)<((*pState).ys-1)]
  relative_roi = [roi[0]-extend_roi[0],roi[1]-extend_roi[0],roi[2]-extend_roi[2],roi[3]-extend_roi[2]]
  cor1 = fltarr((*pState).ts-1)
  offset = lonarr(2,(*pState).ts)
  refImg = GaussianBlur(median((*(*pState).img[0])[*,*,refFrame],3),1)
  show[0,*,*] = bytscl(refImg)

  device,decomposed=0
  timer = Obj_New("ShowProgress", (*pState).MainBase, Color='00DD00'XL,title=' ',  $
      message='Detecting interframe motion...', xsize=400, ysize=25, /CancelButton)
  timer->start
  for k=(*(*pState).pHBMPara).trackStart,(*(*pState).pHBMPara).trackEnd do begin
    cancelled = timer->CheckCancel()
    IF cancelled THEN BEGIN
       ok = Dialog_Message('User cancelled operation.')
       timer->Destroy
       wdelete, 0
       Return
    ENDIF
    if (*(*pState).pHBMPara).trackStart ne (*(*pState).pHBMPara).trackEnd then begin
      percent = (k-(*(*pState).pHBMPara).trackStart)*100/((*(*pState).pHBMPara).trackEnd-(*(*pState).pHBMPara).trackStart)
      timer->update, percent
    endif
    if k eq refFrame then begin
      offset[*,k] = [0,0]
      continue
    endif
    tarImg = GaussianBlur(median((*(*pState).img[0])[*,*,k],3),1)
    mv = -DetectMotion(tarImg, refImg, roi, (*pState).pHBMPara)
    offset[*,k] = mv[0:1] ;& print,mv
    show[1,*,*] = bytscl(shift(tarImg, mv[0],mv[1]))
    wset,0 & tvscl,show,true=1
  endfor
  timer->destroy

; offset[0,*] = smooth(reform(offset[0,*]),3)
  print,'ok'
  while 1 do begin
    xyouts,100,100,'Left Click: Play movie     Right Click: Done',/dev
    cursor,x,y,4,/dev
    if !mouse.button eq 4 then break
    for k=(*(*pState).pHBMPara).trackStart,(*(*pState).pHBMPara).trackEnd do begin
      show[1,*,*] = bytscl(shift((*(*pState).img[0])[*,*,k], offset[0,k], offset[1,k]))
      tvscl, show, true=1
      wait, .02
    endfor
  endwhile
  wdelete, 0

  answer = dialog_message('Apply motion compensation to all channels?', /question)
  if answer ne 'Yes' then return
  for i=0,(*pState).stackNum-1 do begin
    for k=(*(*pState).pHBMPara).trackStart,(*(*pState).pHBMPara).trackEnd do $
      (*(*pState).img[i])[*,*,k] = shift((*(*pState).img[i])[*,*,k], offset[0,k], offset[1,k])
  endfor

  (*pState).oWindow->RemoveWindowEventObserver, (*pState).oObserver
  obj_destroy, (*pState).oObserver
  obj_destroy, (*pState).oAnimationModel
  CreatAnimationModel, pState

end
;-----------------------------------------------------------------
pro OnChannelAlignment, Event, data=data
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  widget_control, event.top, get_uvalue=pState
  if ~ptr_valid(pState) then return
  if (*pState).stackNum le 1 then return
  device,decomposed=1
  show = fltarr(3,(*pState).xs,(*pState).ys)
  show[0,*,*] = (*(*pState).img[0])[*,*,0]
  window,0,xs=(*pState).xs,ys=(*pState).ys
  tv, show, true=1
  box_cursor,x0,y0,nx,ny

  roi = [x0,x0+nx,y0,y0+ny]
  extend_roi = [(x0-60)>0,(x0+nx+60)<((*pState).xs-1),(y0-30)>0,(y0+ny+30)<((*pState).ys-1)]
  relative_roi = [roi[0]-extend_roi[0],roi[1]-extend_roi[0],roi[2]-extend_roi[2],roi[3]-extend_roi[2]]
  cor1 = fltarr((*pState).ts-1)
  offset = lonarr(2,(*pState).ts)
  pPara = ptr_new({l1x:0,l1y:0,l2x:0,l2y:0,l3x:15,l3y:15})
  for i=1,(*pState).stackNum-1 do begin
    for k=0,(*pState).ts-1 do begin
      refImg = GaussianBlur(median((*(*pState).img[0])[*,*,k],3),1)
      tarImg = GaussianBlur(median((*(*pState).img[i])[*,*,k],3),1)
      mv = -DetectMotion(tarImg, refImg, roi, pPara)
      offset[*,k] = mv[0:1]
      show[0,*,*] = refImg
      show[1,*,*] = bytscl(shift(tarImg, mv[0],mv[1]))
      wset,0 & tvscl,show,true=1
    endfor
    window,1 & plot,offset[0,*]
    window,2 & plot,offset[1,*]
    while 1 do begin
      wset,0
      xyouts,100,100,'Left Click: Play movie     Right Click: Done',/dev
      cursor,x,y,3,/dev
      if !mouse.button eq 4 then break
      for k=0,(*pState).ts-1 do begin
        show[0,*,*] = (*(*pState).img[0])[*,*,k]
        show[1,*,*] = bytscl(shift((*(*pState).img[i])[*,*,k], offset[0,k], offset[1,k]))
        tvscl, show, true=1
        wait, .02
      endfor
    endwhile
    wdelete,0,1,2

    answer = dialog_message('Apply motion compensation to this channel?', /question)
    if answer ne 'Yes' then return
    for k=0,(*pState).ts-1 do $
      (*(*pState).img[i])[*,*,k] = shift((*(*pState).img[i])[*,*,k], offset[0,k], offset[1,k])
  endfor
  if keyword_set(data) then return
  ; destroy old animation base
  (*pState).oWindow->RemoveWindowEventObserver, (*pState).oObserver
  obj_destroy, (*pState).oObserver
  obj_destroy, (*pState).oAnimationModel
  CreatAnimationModel, pState

End
;-----------------------------------------------------------------
Pro OnEditMask, Event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  widget_control, event.top, get_uvalue=pState
  seed = ((*pState).ts le 1) ? *(*pState).img[0] : total(*(*pState).img[0], 3)/(*pState).ts
  *(*pState).mask = ManuallyGetMask(seed, mask=*(*pState).mask)

  if *(*pState).mask[0] eq -1 then begin
    void = dialog_message('Invalid mask!',/error)
    return
  endif
End
;-----------------------------------------------------------------
pro OnGetMask, Event, auto=auto, silent=silent
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

	widget_control, event.top, get_uvalue=pState

;----------------- this part is written for skeletal muscle flash analysis-------------
;mask = replicate(1b,(*pState).xs,(*pState).ys)
;(*pState).edge = ptr_new(GetEdge(mask),/ no_copy)
;write_tiff, (*pState).file[0]+'-mask.tif', mask*255
;(*pState).mask = ptr_new(mask, /no_copy)
;return
;--------------------------------------------------------------------------------------

  seed = ((*pState).ts le 1) ? *(*pState).img[0] : total(*(*pState).img[0], 3)/(*pState).ts
  imb = bytarr(3,(*pState).xs,(*pState).ys)
	imb[1,*,*] = bytscl(seed)
	device, decomposed=1

  if keyword_set(auto) then begin
    if file_test((*pState).file[0]+'-mask.tif') eq 1 then begin
        mask = read_tiff((*pState).file[0]+'-mask.tif')/255
        dim = size(mask,/dimensions)
        if dim[0] eq (*pState).xs and dim[1] eq (*pState).ys then begin
          imb[0,*,*] = mask*150
          window,1,xs=(*pState).xs,ys=(*pState).ys,title='**** Old Mask ****'
          tv, imb, true=1
          if ~keyword_set(silent) then begin
            if dialog_message('Use old mask?', /question) eq 'No' then mask = AutoGetMask(seed)
          endif
        endif else mask = AutoGetMask(seed)
    endif else begin
      mask = AutoGetMask(seed)
    endelse
      imb[0,*,*] = mask*150
      window,1,xs=(*pState).xs,ys=(*pState).ys,title='**** Cell Mask ****'
      tv,imb,true=1
    if ~keyword_set(silent) then begin
      if dialog_message('Edit mask?', /question, /default_no) eq 'Yes' then begin
        wdelete, 1
        mask = ManuallyGetMask(seed, mask=mask)

      endif
    endif else wdelete,1
  endif else begin
    mask = ManuallyGetMask(seed)
  endelse
  wdelete,1
  if mask[0] eq -1 then begin
    void = dialog_message('Invalid mask!',/Warning)
    return
  endif
  edge=GetEdge(mask)
  (*pState).edge = ptr_new(edge,/ no_copy)

;;*********20091024forWarris*********
;
;
;	path = file_dirname((*pState).file)
;	file = path + '\\' + file_basename((*pState).file[0],'.lsm') + 'fluorescence.txt'
;	openw,lun,file,/get_lun
;	printf,lun,format='(%"time\tfluorescence")'
;	lum = fltarr((*pState).ts)
;	index = where(mask eq 1)
;	n = n_elements(index)
;	xys = double((*pstate).xs)*(*pstate).ys
;;	pos = array_indices(mask,index)
;	for t=0,(*pState).ts-1 do begin
;		lum[t] = total((*(*pState).img[0])[index+t*xys],/double)/n
;		printf,lun,format='(%"%d\t%f")',t,lum[t]
;	endfor
;	free_lun,lun
;;***********************************

	write_tiff, (*pState).file[0]+'-mask.tif', mask*255
	(*pState).mask = ptr_new(mask, /no_copy)
end

;-----------------------------------------------------------------
Pro OnGetBackFromMask, Event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  widget_control, event.top, get_uvalue=pState

  if max((*(*pState).back)[0,*]) ne 0 then begin
    answer1 = dialog_message('You have already set the background.' + $
          ' Do you want to change it anyway?',/question)
    if answer1 eq 'No' then return
  endif

  if ~ptr_valid((*pState).mask) then OnGetMask,event
  backMask = erode((1-*(*pState).mask),replicate(1,21,21))
  backRegion = where(backMask eq 1)
  back = fltarr((*pState).stackNum,(*pState).ts)

  for k=0,(*pState).stackNum-1 do begin
    for i=0,(*pState).ts-1 do begin
      frame = (*(*pState).img[k])[*,*,i]
      back[k,i] = mean(frame[backRegion])
      (*(*pState).img[k])[0,0,i] = frame - back[k,i]
      (*(*pState).back)[k,i] += back[k,i]
    endfor
  endfor
  wset, (*pState).TraceWindow & PlotTrace, pState, (*pState).back, title='Background'
  wait, 0.5

  ; correct pre- existing ROI traces
  if ~ptr_valid((*pState).result) then begin
    ROINum = (*pState).result->length()
    if ROINum gt 0 then begin
      for i=0, ROINum-1 do begin
        info = (*pState).result->GetProperty(i, trace=trace)
        *trace[0] -= back
        *trace[1] = (where((*trace[0])[*,0] le 0))[0] ne -1 ? *trace[1]*0 : $
            *trace[0]/rebin((*trace[0])[*,0],(*pState).stackNum,(*pState).ts)
      endfor
    endif
  endif

  ; destroy old animation base
;  (*pState).oWindow->RemoveWindowEventObserver, (*pState).oObserver
;  obj_destroy, (*pState).oObserver
;  obj_destroy, (*pState).oAnimationModel
;  CreatAnimationModel, pState

End

;-----------------------------------------------------------------
Pro OnGetBackFromROI, Event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

	widget_control, event.top, get_uvalue=pState

	if ~obj_valid((*pState).result) then begin
	  warning = dialog_message('Please set a ROI first!', /error)
    return
	endif
	if (*pState).result->length() le 0 then begin
		warning = dialog_message('Please set a ROI first!', /error)
		return
	endif
	if max((*(*pState).back)[0,*]) ne 0 then begin
		answer1 = dialog_message('You have already set the background.' + $
					' Do you want to change it anyway?',/question)
		if answer1 eq 'No' then return
	endif
	answer2 = dialog_message('Set ROI1 as the background ROI?')
	if answer2 eq 'No' then return

	info = (*pState).result->GetProperty(0, trace=trace)
	*(*pState).back += *trace[0]
	deltaBack = *trace[0]
	wset, (*pState).TraceWindow &	PlotTrace, pState, (*pState).back, title='Background'
	wait, 0.5

	; Subtract background from image
	for k=0,(*pState).stackNum-1 do begin
		for i=0,(*pState).ts-1 do begin
			(*(*pState).img[k])[*,*,i] -= replicate((*trace[0])[k,i], (*pState).xs,(*pState).ys,1)
		endfor
	endfor

	; correct pre- existing ROI traces
	for i=0, (*pState).result->length()-1 do begin
    info = (*pState).result->GetProperty(i, trace=trace)
    *trace[0] -= deltaBack
    *trace[1] = (where((*trace[0])[*,0] le 0))[0] ne -1 ? *trace[1]*0 : $
        *trace[0]/rebin((*trace[0])[*,0],(*pState).stackNum,(*pState).ts)
  endfor

	; destroy old animation base
;	(*pState).oWindow->RemoveWindowEventObserver, (*pState).oObserver
;	obj_destroy, (*pState).oObserver
;	obj_destroy, (*pState).oAnimationModel
;	CreatAnimationModel, pState

End
;-----------------------------------------------------------------
pro OnPhotobleachCorrection, Event, selection=selection
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  widget_control, event.top, GET_UVALUE=pState
  IsCorrection = 0b
  if n_elements(selection) ne 0 then $
    IsCorrection = PhotobleachCorrection(pState, group_leader=event.TOP, selection=selection, /auto) $
  else IsCorrection = PhotobleachCorrection(pState, group_leader=event.TOP)
  if IsCorrection eq 0b then return

	;destroy old animation model
	(*pState).oWindow->RemoveWindowEventObserver, (*pState).oObserver
	obj_destroy, (*pState).oObserver
	(*pState).oView->Remove,  (*pState).oAnimationModel
	obj_destroy, (*pState).oAnimationModel

	; creat new animation model and observer
	CreatAnimationModel,pState

  if ~obj_valid((*pState).result) then return
  ROINum = (*pState).result->length()
  if ROINum le 0 then return
  if ptr_valid((*pState).peak) then begin
    peak = median(*(*pState).peak,3)
  endif
  widget_control, widget_info((*pState).AnimationBase,find_by_uname='ROISlider'),get_value=ROINo
  for k=0,ROINum-1 do begin
    info = (*pState).result->GetProperty(k, oROI=oROI)
    if info eq -1 then continue
    roiMask = oROI->ComputeMask(dimensions=[(*pState).xs,(*pState).ys])
    index = where(roiMask gt 0)
    if n_elements(peak) gt 0 then begin
      centerPeak = max(peak[index])
      index = index[where(peak[index] ge centerPeak*0.5)]
    endif
    trace = GetTrace(pState,index)
    info = (*pState).result->SetProperty(k, trace=trace)
    if k eq ROINo-1 then begin
      wset, (*pState).TraceWindow & PlotTrace, pState, trace[0],title=file_basename((*pState).file[0])+'  ROI '+strtrim(ROINo,1)
    endif
  endfor
end
;-----------------------------------------------------------------
Function SubtractBaseline, pState
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF
t1 = systime(1)

;	if ~ptr_valid((*pState).ima) then (*pState).ima = ptr_new(fltarr((*pState).xs,(*pState).ys,(*pState).ts), /no_copy)
;	if ~ptr_valid((*pState).timing) then (*pState).timing = ptr_new(replicate(-1,(*pState).xs, (*pState).ys), /no_copy)
;	if ~ptr_valid((*pState).sum) then (*pState).sum = ptr_new(fltarr((*pState).xs,(*pState).ys), /no_copy)
	if ~ptr_valid((*pState).peak) then (*pState).peak = ptr_new(fltarr((*pState).xs,(*pState).ys), /no_copy)

	index = where(*(*pState).mask eq 1, count)
  indices = dindgen((*pState).ts)*(*pState).xs*(*pState).ys
	X = indgen((*pState).ts)
	MaxRange = [0,(*pState).ts-1]
  FitRange = [0,(*pState).ts-1]
	fitX = X[FitRange[0]:FitRange[1]]
  device, decomposed=0
  tvlct, 0,200,0,41
  timer = Obj_New("ShowProgress", (*pState).MainBase, Color=41,title=' ',  $
      message='Subtracting baseline...', xsize=400, ysize=25, /CancelButton)
  timer->start
  
  stack = float((*(*pState).img[0]))
  stack = temporary(smooth(stack,7,edge = 1))
	
	for i=0L,count-1 do begin
	  cancelled = timer->CheckCancel()
    IF cancelled THEN BEGIN
       ok = Dialog_Message('User cancelled operation.')
       timer->Destroy
       ptr_free, (*pState).ima,(*pState).timing,(*pState).sum,(*pState).peak
       RETURN,-1
    ENDIF
    if (i mod 100) eq 0 then begin
      percent = i*100/count
      timer->update, percent
    endif
    
		signal = stack[indices+index[i]]
		corLine = signal
		tmp = max(blk_stddev(corLine[MaxRange[0]:MaxRange[1]],10))
		(*(*pState).peak)[index[i]] = tmp
	endfor

	loadct,8,rgb = rgb ,/silent
;	write_tiff,'D:\PeakMap.tif',reverse(bytscl(*(*pState).peak),2),red=rgb[*,0],green=rgb[*,1],blue=rgb[*,2];; modified by suntao 20141214 no save
	timer->destroy		
	t2 = systime(1)
	print, 'Time consumption for Correct baseline: ', t2 - t1, ' s'
	return, 1
End

;-----------------------------------------------------------------
pro OnSubtractBaseline, Event
  widget_control, event.top, get_uvalue=pState
  if SubtractBaseline(pState) eq -1 then return
end
;-------------------------------------------------------------------------
Function CalculateFeature, pState
return,1 ;
COMPILE_OPT STRICTARR

  Catch, errorStatus
  if (errorStatus ne 0) then begin
    Catch, /CANCEL
    void = dialog_message('Error while calculating features!', /error)
    return, -1
  endif

  device, decomposed=1
  ; -------- ima Hist -------
;  bin = 0.5
;  mask = erode(*(*pState).mask,replicate(1,3,3))
;  index = where(mask eq 1)
;  n = n_elements(index)
;  xys = (*pState).xs*(*pState).ys
;  pIndices = ptr_new(lonarr((*pState).ts*n),/no_copy)
;  for i=0L,(*pState).ts-1 do (*pIndices)[i*n:(i+1)*n-1] = index+i*xys
;
;  pima = ptr_new((*(*pState).ima)[*pIndices],/no_copy)
;  ptr_free, pIndices
;  imaMin = min(*pima, max=imaMax)
;  imaMin = fix(imaMin) & imaMax = round(imaMax+0.499); &print,imamin,imamax
;  imaHist = histogram(*pima, min=imaMin,binsize=bin)
;  ptr_free, pima
;  imaX = findgen(n_elements(imaHist))*bin + imaMin
;  (*pState).imaHist = ptr_new([[imaX],[imaHist]])
;  imaHistMax = max(imaHist,maxPos)
;  newimaHist = imaHist & newimaHist[maxPos] = newimaHist[maxPos-1]*1.2
;  A = [(newimaHist[maxPos]), imaX[maxPos], 3]; & print, A
;  gaussPara = GaussFit(imaX[maxPos-15:maxPos+15],newimaHist[maxPos-15:maxPos+15],A,estimates=A,nterms=3)
;  fitHist = A[0]*exp(-(imaX-A[1])^2/2/A[2]^2); & print,A
;  (*pState).mean[1] = A[1]
;  (*pState).sigma[1] = abs(A[2])
;  print, 'MEAN of IMA', A[1]
;  print, 'STDDEV of IMA', A[2]
;
;  window,1,xs=600,ys=400
;  plot,imaX,newimaHist,psym=-5,background='FFFFFF'XL,color='000000'XL, $
;      title='Histogram of Image stack',xtitle='F',ytitle='Num'
;  oplot,imaX,fithist,thick=2,color='0000FF'XL
  ;--------------------- imaHist output -----------------------
;  openw,unit,(*pState).file[0]+'-deltaF histogram.txt',/get_lun
;  printf,unit,systime(),'    MEAN: ',strtrim(A[1],1),'    ST.DEV: ',strtrim(abs(A[2]),1)
;  printf,unit,'X DeltaFHist FitDeltaFHist'
;  for i=0,n_elements(imaX)-1 do printf,unit,strtrim(imaX[i],1),imaHist[i],fitHist[i]
;  free_lun,unit
  ;---------------------------------------------------------

  ; -------- peak Hist --------
  bin=0.5
  peak = (*(*pState).peak)[where(*(*pState).mask eq 1)]
  peakMin = min(peak, max=peakMax)
  peakMin = fix(peakMin) & peakMax = round(peakMax+0.499)
  peakHist = histogram(peak, min=peakMin, nbins = 100);binsize=bin)
  peakX = findgen(n_elements(peakHist))*bin + peakMin
  (*pState).peakHist = ptr_new([[peakX],[peakHist]])
  peakHistMax = max(peakHist, maxPos)
  A = [(peakHist[maxPos]), peakX[maxPos], 3]
  gaussPara = GaussFit(peakX[(maxPos-15)>0:maxPos+5],peakHist[(maxPos-15)>0:maxPos+5],A,estimates=A,nterms=3)
  fitHist = A[0]*exp(-(peakX-A[1])^2/2/A[2]^2)
  (*pState).mean[0] = A[1]
  (*pState).sigma[0] = abs(A[2])
  print, 'MEAN of peak', A[1]
  print, 'STDDEV of peak', A[2]

  window,2,xs=600,ys=400
  plot,peakX,peakHist,psym=-5,background='FFFFFF'XL,color='000000'XL, $
      title='Histogram of Peak',xtitle='F',ytitle='Num'
  oplot,peakX,fithist,thick=2,color='0000FF'XL
  ;--------------------- peakHist output -----------------------
;  openw,unit,(*pState).file[0]+'-peak histogram.txt',/get_lun
;  printf,unit,systime(),'    MEAN: ',strtrim(A[1],1),'    ST.DEV: ',strtrim(abs(A[2]),1)
;  printf,unit,'X PeakHist FitPeakHist'
;  for i=0,n_elements(peakX)-1 do printf,unit,strtrim(peakX[i],1),peakHist[i],fitHist[i]
;  free_lun,unit
  ;---------------------------------------------------------
  peakThr = max(where(peakHist gt peakHistMax*0.1)) + 1
  print,' Best Cri : ', peakThr/A[2]
wdelete,1,2
  return, 1

  ; -------- sumHist --------
  sumMin = min(*(*pState).sum, max=sumMax)
  sumMin = fix(sumMin) & sumMax = round(sumMax+0.499); &print,summin,summax
  bin = 10
  sumHist = histogram((*(*pState).sum)[where(*(*pState).mask eq 1)], min=sumMin,binsize=bin)
  sumX = findgen(n_elements(sumHist))*bin + sumMin
  (*pState).sumHist = ptr_new([[sumX],[sumHist]])
  sumHistMax = max(sumHist,maxPos)
  A = [sumHist[maxPos], sumX[maxPos], 20]
  gaussPara = GaussFit(sumX[(maxPos-10)>0:maxPos+5],sumHist[(maxPos-10)>0:maxPos+5],A,estimates=A,nterms=3)
  fitHist = A[0]*exp(-(sumX-A[1])^2/2/A[2]^2)
  (*pState).mean[1] = A[1]
  (*pState).sigma[1] = A[2]
  print, 'MEAN of sum', A[1]
  print, 'STDDEV of sum', A[2]

  window,3,xs=600,ys=400
  plot,sumX,sumHist,psym=-5,background='FFFFFF'XL,color='000000'XL, $
      title='Histogram of Sum',xtitle='Sum',ytitle='Num'
  oplot,sumX,fithist,thick=2,color='0000FF'XL

  sumThr = max(where(sumHist gt sumHistmax*0.1))*10
;  print, sumThr,sumThr/A[2]

  wdelete,1,2,3
End
;-----------------------------------------------------------------
pro OnCalculateFeature, Event

end
;-----------------------------------------------------------------
Pro OnAutoDetection, Event, thr=thr, validation=validation, flashSites=flashSites
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  widget_control, event.top, get_uvalue=pState
  if ~ptr_valid(pState) then return
  if ~ptr_valid((*pState).peak) then begin
    if SubtractBaseline(pState) eq -1 then return
;    if CalculateFeature(pState) eq -1 then return
  endif
;  if (*pState).sigma[0] eq 0 then begin
;    if CalculateFeature(pState) eq -1 then return
;  endif
  if N_elements(thr) NE 0 then begin
    (*pState).Thr[0] = thr[0]
    validation = [-1,-1,-1,-1]
    DetectFlash, pState, feature=-1, validation=validation, flashSites=flashSites
  endif else begin
  m1 = memory(/current)
    DetectFlash, pState
  print, memory(/current)-m1
;    if ~widget_info((*pState).DetectionThrBase,/valid_id) then $
;      SetDetectionThreshold, pState, group_leader=event.top
  endelse
End
;-----------------------------------------------------------------
;-----------------------------------------------------------------
Pro BaselineSubtraction, trace
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF
return
  trace1 = reform(*trace)
  n = n_elements(trace1)
  x1 = indgen(n)
  para1 = ladfit(x1,trace1)
  y1 = para1[0]+para1[1]*x1
  e1 = trace1-y1
  m1 = mean(e1)
  s1 = stddev(e1)
  valid1 = where(abs(e1) le m1+s1)
  trace2 = trace1[valid1]
  x2 = x1[valid1]
  para2 = ladfit(x2,trace2)
  y2 = para2[0]+para2[1]*x2
  e2 = trace2-y2

  filter = DIGITAL_FILTER(0,0.2,50,10)
;  y1 = convol(trace1,filter,/edge_truncate)
;  e1 = trace1-y1
   window,1,xs=600,ys=300,xpos=400,ypos=10
   plot, x1,trace1,background='FFFFFF'XL,color='000000'XL
   oplot, x1,y1,color='0000FF'XL
   window,2,xs=600,ys=300,xpos=400,ypos=320
  plot, x1,e1,background='FFFFFF'XL,color='000000'XL
  oplot, x2,e2,color='0000FF'XL
;  window,2,xs=600,ys=300,xpos=400,ypos=320
;  plot, x1,e1,background='FFFFFF'XL,color='000000'XL
return
  trace1 = convol(trace1,digital_filter(0,0.4,50,5),/edge_truncate)
  y0 = trace1
  d = 5
  alpha = 1.0
  beta = 1.0
  step=0
  while step lt 10 do begin
    for k=0,n-1 do begin
      newy = findgen(2*d+1)-d+y0[k]
      eInt1 = k eq 0 ? abs(newy-y0[k+1]) : abs(newy-y0[k-1])
      eExt = abs(trace1[k]-newy)
      eTotal = alpha*eInt1 + beta*eExt
      min_eTotal = min(eTotal,pos) ;& help,pos
      y0[k] = newy[pos[0]]
    endfor
    wset,1
    plot, x1,trace1,background='FFFFFF'XL,color='000000'XL
    oplot, x1,y0,color='0000FF'XL
    step++
    wait, 0.05
  endwhile
  wset,1 & oplot, x1,y1,color='FF0000'XL
  window,2,xs=600,ys=300,xpos=400,ypos=320
  plot, x1,trace1-y0,background='FFFFFF'XL,color='000000'XL
  oplot, x1,e1,color='0000FF'XL
End
;-----------------------------------------------------------------


;-----------------------------------------------------------------

Pro OnResultTableEvents, event
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

;{WIDGET_TABLE_CELL_SEL, ID:0L, TOP:0L, HANDLER:0L, TYPE:4,
;   SEL_LEFT:0L, SEL_TOP:0L, SEL_RIGHT:0L, SEL_BOTTOM:0L}
;  print,'select table'
  widget_control, event.top, get_uvalue=pState
  if ~ptr_valid(pState) then return
  if ~obj_valid((*pState).result) then return

  case event.type of
  4: begin   ; select or disselect table
    if event.sel_top eq -1 then return
    widget_control, event.id, use_table_select=[0,event.sel_top,0,event.sel_top],get_value=ROINo
    ROINo = uint(ROINo[0])
    if ROINo le 0 or ROINo gt (*pState).result->length() then return
    idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
    widget_control, idROISlider, set_value=ROINo
    OnChangeROI, {top:event.top, value:ROINo}
  end
  0: begin   ; insert a character
    sel = widget_info(event.id, /table_select)
    if sel[0] ne 13 then return
    widget_control, event.id, use_table_select=[0,sel[1],0,sel[3]],get_value=ROINo
    if ROINo le 0 or ROINo gt (*pState).result->length() then return
    value = uint(event.ch)-48
    info = (*pState).result->GetProperty(ROINo-1, flashPara=flashPara)
    if info eq -1 then begin
      print,'The ROI No. is out of range!'
      return
    endif
    if value eq 65501 then return
    if value lt 0 or value gt 9 then begin
      (*flashPara)[10] = 0
      widget_control, event.id, use_table_select=sel,set_value=0
      return
    endif
    if ~ptr_valid(flashPara) then begin
      flasPara = ptr_new(fltarr(20))
      info = (*pState).result->SetProperty(ROINo-1, flashPara=flashPara)
    endif
    (*flashPara)[10,*] = value
;    print, *flashPara
  end
  else:
  endcase
; right click ---> change value?

End

;-----------------------------------------------------------------
Pro OnCaptureTraceWindow, Event
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF
  widget_control, event.top, get_uvalue=pState
  widget_control, widget_info((*pState).AnimationBase, find_by_uname='ROISlider'), get_value=ROINo
  file = (*pState).file[0]+'-ROI'+strtrim(ROINo,1)+'-trace.jpg'
;  file = dialog_pickfile(file=file)
;  if file[0] eq '' then return
  wset,(*pState).TraceWindow
  write_jpeg,file,tvrd(true=1),true=1
End
;-----------------------------------------------------------------
Pro OnBackup, Event
  OnSaveStatus, Event, /silent
  OnExportROI, Event, /silent
  OnExportSingleWithROI, Event, /silent
  OnExportResult, Event
;  print,'Backup accomplished!'
  answer = DIALOG_MESSAGE('Backup accomplished!',/CENTER)
End
;-----------------------------------------------------------------
pro OnAbout, Event
  proInfo = ['Flash Sniper #Alpha 3.0', $
         'Copyright @ Lab of Calcium Signalling, IMM, PKU', $
         'All Rights Reserved', $
         'KaitaoLi@pku.edu.cn', $
         'Wenxin@pku.edu.cn']
  answer = DIALOG_MESSAGE(proInfo,/CENTER, /INFORMATION, TITLE='About Flash Sniper')
end

;-----------------------------------------------------------------
pro Initialize, wWidget
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

; RegisterCursor
  event = {event, ID: wWidget, $
          TOP: wWidget, $
          HANDLER: 1, $
          SELECT: 1}
  wait, .2
  OnOpen, event
end
;-----------------------------------------------------------------