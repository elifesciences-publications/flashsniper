
Pro OnExportDeadMap, Event
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF
  widget_control, event.top, get_uvalue=pState
  if ~ptr_valid(pState) then return
  if ~obj_valid((*pState).result) then return
  ROINum = (*pState).result->length()
  if ROINum eq 0 then return

  deadMap = bytarr((*pstate).xs,(*pstate).ys)
  for ROINo=0,ROINum-1 do begin
    info = (*pState).result->GetProperty(ROINo,oROI=oROI)
    index = where(oROI->ComputeMask(dimensions=[(*pState).xs,(*pState).ys]) gt 0)
    deadMap[index] = 1b
  endfor
  write_tiff, (*pState).file[0]+'-dead ROI map.tif', reverse(deadMap,2)*255

End
;-----------------------------------------------------------------

pro OnExportResult, Event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF
;flashPara [   0   ,  1  , 2 ,  3 ,   4   ,  5   , 6 ,  7  ,  8 ,    9   ,  10 ,  11 ,  12  , 13 , 14 ,  15 , 16 , 18, 19]
;flashPara [FlashNo, Peak, F0, Amp, Tstart, Tpeak, RT, RT90, T50, EndBase, Note, FAHM, dBase, T75, T90, FDHM, Dia, t1, t2]
  widget_control, event.top, get_uvalue=pState
  if obj_valid((*pState).result) eq 0 then return

; pathPos = strpos((*pState).file, '\', /reverse_search) & fileLen = strlen((*pState).file)
; fileName = strmid((*pState).file, pathPos+1, fileLen-pathPos-1)

  openw, unit, file_dirname((*pState).file[0])+'//'+file_basename((*pState).file[0],'.lsm')+'-result.txt', /append, width=800, /get_lun
  ;'result.txt', /append, width=800, /get_lun
  printf, unit, format='(%"%s\t%s\t%s")',systime(),file_dirname((*pState).file),file_basename((*pState).file)
  printf, unit, format='(%"Dimension(xyt):\t%d\t%d\t%d")',(*pState).xs,(*pState).ys,(*pState).ts
  printf, unit, format='(%"PixelSize_x(um):\t%f\tPixelSize_t(ms):\t%f")',(*pState).pxs,(*pState).pts
  if ptr_valid((*pState).mask) then begin
    printf, unit, format='(%"CellArea(um2):\t%f")', total(float(*(*pState).mask))*(*pState).pxs^2
    ;cellMask = erode(*(*pState).mask, replicate(1,11,11));;modified by suntao,20151109, caused error
    cellmask=*(*pState).mask
    cell = where(cellMask eq 1)
    if cell[0] eq -1 then cell=0;;added by suntao,20150717
    avgSignal = fltarr((*pState).stackNum)
    for k=0,(*pState).stackNum-1 do begin
      frame =(*(*pState).img[k])[*,*,0]
      avgSignal[k] = mean(frame[cell])
      printf, unit, format='(%"CellAverage-Ch%d:\t%f")',k+1,avgSignal[k]
    endfor
    if (*pState).stackNum gt 1 then $
      printf, unit, format='(%"CellAverage-Ch1/Ch2:\t%f")',avgSignal[0]/avgSignal[1]
  endif
  printf, unit, format='(%"ROINo\tArea\tDistance\tFlashNo\tPeak\tF0\tdF/F0\tTstart\tTpeak\tRT\tRT90\tT50\tEndBase\tNote\tFAHM\tdBase\tT75\tT90\tFDHM")'
  for k=0,(*pState).result->length()-1 do begin
    info = (*pState).result->GetProperty(k,ROIPara=ROIpara,flashPara=flashPara,flashNum=flashNum)
    if ptr_valid(flashPara) then begin
      for i=0, flashNum-1 do $
        printf,unit,format='(%"%d\t%f\t%f\t%d\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%d\t%f\t%f\t%f\t%f\t%f")' $
          ,k+1,ROIPara,(*flashPara)[0:15,i]
    endif else begin
      printf,unit,format='(%"%d\t%f\t%f")',k+1,ROIPara
    endelse
  endfor
  printf, unit, ''
  free_lun, unit

  fileName = file_basename((*pState).file[0])
  pathName = file_dirname((*pState).file[0])
  openw, unit, pathname+'\total result.txt',/append,width=800,/get_lun
  for k=0,(*pState).result->length()-1 do begin
    info = (*pState).result->GetProperty(k,ROIPara=ROIpara,flashPara=flashPara,flashNum=flashNum)
    if ptr_valid(flashPara) then begin
      for i=0, flashNum-1 do $
        printf,unit,format='(%"%s\t%d\t%f\t%f\t%d\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%d\t%f\t%f\t%f\t%f\t%f")' $
          ,fileName,k+1,ROIPara,(*flashPara)[0:15,i]
    endif else begin
      printf,unit,format='(%"%s\t%d\t%f\t%f")',fileName,k+1,ROIPara
    endelse
  endfor
  free_lun, unit
end

;-----------------------------------------------------------------
pro OnExportTrace, Event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  widget_control, event.top, get_uvalue=pState

  save_ori = dialog_pickfile(file=file_basename((*pState).file[0],'.lsm')+'-Original ROI Mean Result.txt', path=file_dirname((*pState).file[0]), /write, $
        /overwrite_prompt, title='Save original ROI mean results')
  if save_ori[0] eq '' then return
  pos = strpos(save_ori, 'Original')
  save_norm = strmid(save_ori, 0,pos) + 'Normalized ROI Mean Result.txt'

  stackNo = replicate('Ch',(*pState).stackNum) + string(indgen((*pState).stackNum)+1, format='(i0)')
  ROINum = (*pState).result->length()
  if ROINum gt 0 then ROINo = replicate('ROI',ROINum) + string(indgen(ROINum)+1,format='(i0)')
  title = [' Time ']
  for k=0,(*pState).stackNum-1 do begin
    title = [title,stackNo[k]+'-BG ']
    if ROINum gt 0 then begin
    for i=0,ROINum-1 do title = [title,stackNo[k]+'-'+ROINo[i]+' ']
    title = [title,stackNo[k]+'-Mean ']
    endif
  endfor

  ori_block = ptrarr((*pState).stackNum, /allocate_heap)
  norm_block = ptrarr((*pState).stackNum, /allocate_heap)
  for k=0,(*pState).stackNum-1 do begin
    *ori_block[k] = reform((*(*pState).back)[k,*])
    *norm_block[k] = reform((*(*pState).back)[k,*])
  endfor

  if ROINum gt 0 then begin
  for i=0, ROINum-1 do begin
    info = (*pState).result->GetProperty(i, trace=trace)
    for k=0, (*pState).stackNum-1 do begin
      *ori_block[k] = [[*ori_block[k]],[reform((*trace[0])[k,*])]]
      *norm_block[k] = [[*norm_block[k]],[reform((*trace[1])[k,*])]]
      if i eq ROINum-1 then begin
        if ROINum eq 1 then begin
          ori_mean = reform((*ori_block[k])[*,1:*])
          *ori_block[k] = [[*ori_block[k]],[ori_mean]]
          norm_mean = reform((*norm_block[k])[*,1:*])
          *norm_block[k] = [[*norm_block[k]],[norm_mean]]
        endif else begin
          ori_mean = ((*ori_block[k])[0,0] eq 0) ? total((*ori_block[k])[*,1:*], 2)/ROINum : total((*ori_block[k])[*,2:*], 2)/(ROINum-1)
          *ori_block[k] = [[*ori_block[k]],[ori_mean]]
          norm_mean = ((*norm_block[k])[0,0] eq 0) ? total((*norm_block[k])[*,1:*], 2)/ROINum : total((*norm_block[k])[*,2:*], 2)/(ROINum-1)
          *norm_block[k] = [[*norm_block[k]],[norm_mean]]
        endelse
      endif
    endfor
  endfor
  endif
  ori_whole_block = *(*pState).correctedtime & norm_whole_block = ori_whole_block
  for k=0, (*pState).stackNum-1 do begin
    ori_whole_block = [[ori_whole_block], [*ori_block[k]]]
    norm_whole_block = [[norm_whole_block], [*norm_block[k]]]
  endfor

  openw,unit,save_ori, width=10000,/get_lun & printf,unit,title
  printf,unit, transpose(ori_whole_block) & free_lun,unit
  openw,unit,save_norm, width=10000,/get_lun & printf,unit,title
  printf,unit, transpose(norm_whole_block) & free_lun,unit

  ptr_free, ori_block, norm_block
end

;-----------------------------------------------------------------
pro OnExportSingleWithROI, Event, silent=silent
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  widget_control, event.top, get_uvalue=pState
  idFrameSlider = widget_info((*pState).AnimationBase, find_by_uname='FrameSlider')
  widget_control, idFrameSlider, get_value=k
  file=file_basename((*pState).file[0],'.lsm')+'-'+strtrim(string(k),1)
  if ~keyword_set(silent) then file = dialog_pickfile(filter='*.tif',file=file, path=file_dirname((*pState).file[0]), /write)
  if file eq '' then return
  (*pState).oWindow->GetProperty, image_data=imgrgb
  write_tiff, file+'-ROI.tif', reverse(imgrgb,3)
end

;-----------------------------------------------------------------
pro OnExportSingleNoROI, Event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  widget_control, event.top, get_uvalue=pState
  idFrameSlider = widget_info((*pState).AnimationBase, find_by_uname='FrameSlider')
  widget_control, idFrameSlider, get_value=k

  file = dialog_pickfile(filter='*.tif',file=file_basename((*pState).file[0],'.lsm')+'-'+strtrim(string(k),1), path=file_dirname((*pState).file[0]), /write)
  if file eq '' then return

  imgrgb = bytarr(3,(*pState).xs,(*pState).ys)
  imgrgb[1,*,*] = AdjustContrast((*(*pState).img[0])[*,*,k], (*pState).brightness, (*pState).contrast)
  write_tiff, file+'-noROI.tif', reverse(imgrgb,3)

end
;-----------------------------------------------------------------

pro OnExportSeqWithROI, Event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  widget_control, event.top, get_uvalue=pState
  file = dialog_pickfile(filter='*.tif',file=(*pState).file[0]+'-1000',get_path=path, /write)
  if file eq '' then return

  for k=0,(*pState).ts-1 do begin
    (*pState).oObserver->OnSlider, (*pState).oWindow, k
    (*pState).oWindow->GetProperty, image_data=imgrgb
    write_tiff, file+'-'+strtrim(string(1000+k),1)+'-ROI.tif', reverse(imgrgb,3)
  endfor
end
;-----------------------------------------------------------------
pro OnExportSeqNoROI, Event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  widget_control, event.top, get_uvalue=pState
  file = dialog_pickfile(filter='*.tif',file=(*pState).file[0]+'-1000',get_path=path, /write)
  if file eq '' then return

  imgrgb = bytarr(3,(*pState).xs,(*pState).ys)
  for k=0,(*pState).ts-1 do begin
    imgrgb[1,*,*] = AdjustContrast((*(*pState).img[0])[*,*,k], (*pState).brightness, (*pState).contrast)
    write_tiff, file+'-'+strtrim(string(1000+k),1)+'-noROI.tif', reverse(imgrgb,3)
  endfor
end
;-----------------------------------------------------------------
Pro OnExportAveragedImage, Event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  widget_control, event.top,get_uvalue=pState

  img = total(*(*pState).img[0],3)/(*pState).ts
  imgrgb = bytarr(3,(*pState).xs,(*pState).ys)
  imgrgb[1,*,*] = bytscl(img)
  write_tiff, (*pState).file[0]+'-Averaged Image.tif', reverse(imgrgb,3)

;  img1 = img*0
;  for i=0,(*pState).xs-1 do begin
;  for j=0,(*pState).ys-1 do begin
;    img1[i,j] = median((*(*pState).img[0])[i,j,*])
;  endfor
;  endfor
;  imgrgb1 = bytarr(3,(*pState).xs,(*pState).ys)
;  imgrgb[1,*,*] = bytscl(img)
;  write_tiff, (*pState).file[0]+'-Median Image.tif', reverse(imgrgb,3)
end
;-----------------------------------------------------------------
Pro OnExport3DROISingle, Event
COMPILE_OPT STRICTARR

; get 2.5D of single flash
  widget_control, event.top, get_uvalue=pState
  if ~obj_valid((*pState).result) then return
  if (*pState).result->length() lt 1 then return

  idROISlider = widget_info((*pState).AnimationBase,find_by_uname='ROISlider')
  widget_control, idROISlider, get_value=ROINo
  info = (*pState).result->GetProperty(ROINo-1,oROI=oROI)
  if info eq -1 then begin
    void = dialog_message('The ROI No. is out of range!',/error)
  endif
  oROI->GetProperty, data=data
  min_x = min(data[0,*],max=max_x)
  min_y = min(data[1,*],max=max_y)
  xs = max_x-min_x+1
  ys = max_y-min_y+1

  device, decomposed=0
  loadct,27,/silent

  img = reform((*(*pState).img[0])[min_x:max_x,min_y:max_y,0])
  window,1,xs=600,ys=500
  for a=0,360,10 do begin
    shade_surf,img,ax=a,zst=5,xst=4,yst=4,shades=bytscl(img)
    wait,.5
  endfor

End
;-----------------------------------------------------------------
pro OnExportROISequence, Event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  widget_control, event.top,get_uvalue=pState
  idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
  widget_control, idROISlider, get_value=ROINo
  info = (*pState).result->GetProperty(ROINo-1, oROI=oROI,trace=trace)
  if info eq -1 then return
  filename = dialog_pickfile(file=(*pState).file[0],get_path=path, /write)
  if filename eq '' then return

  oROI->getproperty,data=data
  min_x = min(data[0,*],max=max_x)
  min_y = min(data[1,*],max=max_y)
  min_x = uint(min_x) & max_x = uint(max_x)
  min_y = uint(min_y) & max_y = uint(max_y)
  xs = (max_x-min_x+1)*1
  ys = (max_y-min_y+1)*1
  print,xs*(*pState).pxs,ys*(*pState).pys
;  window,1,xs=800,ys=300
  device,decomposed=0
  loadct,33
  tvlct,r,g,b,/get

  for i=0,(*pState).stackNum-1 do begin
    for k=0,(*pState).ts-1 do begin
      temp = reform((*(*pState).img[i])[min_x:max_x,min_y:max_y,k])
      if i eq 1 then begin
        temp *= 2.5
        temp = (temp > 0)<255
      endif
;      temp = (temp-0)*3>0
;      temp = temp<230
      temp = smooth(temp,3,edge=1)
;      wset,i & tv,temp,k
      temp = rebin(temp,4*xs,4*ys)
      write_tiff,filename+'-ROI'+strtrim(ROINo,1)+'-Ch'+strtrim(i+1,1)+'-'+strtrim(1000+k,1)+'.tif',reverse(temp,2),red=r,green=g,blue=b
    endfor
  endfor
;------------- test arrangement --------------
;  trace = reform((*trace[0])[0,*])
;  f_max = max(trace,min=f_min)
;  t_max = max(*(*pState).correctedTime,min=t_min)
;  trace = median(trace,3)
;  trace = smooth(trace,3,edge=1)
;  nxs = 1200
;  nys = 900
;  img = bytarr(nxs,nys)
;  window,2,xs=nxs,ys=nys
;  for k=0,(*pState).ts-1 do begin
;    if k mod 2 eq 0 then continue
;    temp = reform((*(*pState).img[0])[min_x:max_x,min_y:max_y,k])
;    temp = smooth(temp,3,edge=1)
;    x0 = uint(((*(*pState).correctedTime)[k]-t_min)/(t_max-t_min)*(nxs-xs))
;    y0 = uint((trace[k]-f_min)/(f_max-f_min)*(nys-ys))
;    img[x0>0:(x0+xs-1)<(nxs-1),y0>0:(y0+ys-1)<(nys-1)] = temp
;  endfor
;  tv, img
end
;------------------------------------------------------------------

;-----------------------------------------------------------------
pro OnExportTraceToIplot, Event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  widget_control, event.top,get_uvalue=pState
  idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
  widget_control, idROISlider, get_value=ROINo
  info = (*pState).result->GetProperty(ROINo-1, oROI=oROI,trace=trace)
  num = info
  loadct,33,rgb = rgb,/silent
  color = reform(rgb[round((roino-1)*255.0/num),*])
  if info eq -1 then return
  trace = reform(trace)
				signal = reform((*trace[1]))
				ans = itgetcurrent()
				if strpos(ans,'PLOT TOOL', /REVERSE_SEARCH) eq -1 then begin
				iplot,*(*pState).correctedTime,reform(signal[0,*]),color = color;,/overplot
				endif else begin
				iplot,*(*pState).correctedTime,reform(signal[0,*]),color = color,/overplot
				endelse
				
end
;------------------------------------------------------------------
Pro OnExportAllROI, Event
COMPILE_OPT STRICTARR
; show all flashes on averaged image
  path = 'E:\Wen Xin\idl_pic\'
  file_mkdir,path
  widget_control,event.top,get_uvalue=pState
  if ~obj_valid((*pState).result) then return
  if (*pState).result->length() lt 1 then return

  base = total(*(*pState).img[0],3)/(*pState).ts

  for ROINo=0,(*pState).result->length()-1 do begin
    info = (*pState).result->GetProperty(ROINo,oROI=oROI,trace=trace)
    if info eq -1 then begin
      void = dialog_message('The ROI No. is out of range!',/error)
    endif
    oROI->GetProperty, data=data
    min_x = min(data[0,*],max=max_x)
    min_y = min(data[1,*],max=max_y)
    xs = max_x-min_x+1
    ys = max_y-min_y+1
    trace = reform((*trace[0])[0,*])
    fitPara = ladfit(*(*pState).correctedTime,trace)
    baseline = fitpara[0]+fitpara[1]**(*pState).correctedTime
    residual = trace - baseline
    peak = max(residual,peakT)
    frame = reform((*(*pState).img[0])[*,*,peakT])
    frame = smooth(frame,3,edge=1)
    frame = median(frame,3)
;    base[min_x:max_x,min_y:max_y] = frame[min_x:max_x,min_y:max_y]
    roiMask = oROI->ComputeMask(dimensions=[(*pState).xs,(*pState).ys])
    index = where(roiMask gt 0)
    base[index] = frame[index]

  endfor
  device,decomposed=0
  window,1,xs=(*pState).xs,ys=(*pState).ys
  loadct,8
  tvscl,base
  img = tvrd(true=1)
  ima = bytarr(3,(*pState).xs,(*pState).ys)
  ima[0:2,*,*]=img
  write_tiff,path+'1-8-test.tif',reverse(ima,3)

  window,2,xs=(*pState).xs,ys=(*pState).ys
  loadct,33
  tvscl,base
  img = tvrd(true=1)
  ima = bytarr(3,(*pState).xs,(*pState).ys)
  ima[0:2,*,*]=img
  write_tiff,path+'1-33-test.tif',reverse(ima,3)

End