pro DetectFlash, pState, deadMap=deadMap
COMPILE_OPT STRICTARR
t1 = systime(1)
;device,decomposed=0
;loadct,8
;tvlct,r,g,b,/get

;  timing = *(*pState).timing
  peak = median(*((*pState).peak),3)
  peakThr = mthreshold(peak)
  print,'peakThr=',peakThr,'suntao'
  valid = where(*(*pState).mask eq 1)
  if valid[0] eq -1 then return
  peakAvg = mean(peak[valid])
  peakMax = max(peak[where(*(*pState).mask eq 1)])
  if peakThr ge peakMax then begin
    notice = dialog_message('The peak threshold is too high!')
    return
  endif
  
  peakMask = peak gt peakThr
  flashMask = *(*pState).mask and PeakMask

  
  ; Morphological modification of flashMask
  r1 = 2 & r2 = 1.5
  disc1 = SHIFT(DIST(2*r1+1), r1, r1) LE r1
  flashMask = morph_close(flashMask,disc1)
;  window,11,xs=(*pState).xs,ys=(*pState).ys & tvscl, flashMask
  disc2 = SHIFT(DIST(2*r2+1), r2, r2) LE r2
;  flashMask = morph_open(flashMask,disc1)
  flashMask = morph_open(flashMask,disc2); modefied by Sun Tao 20130606
;  base = total(*(*pState).img[0],3)/(*pState).ts
;  maskPeak = GaussianBlur(base,1.7)*dilate(flashMask,replicate(1,5,5))
;
;  watershedimg = watershed(-MaskPeak,connectivity=8,nregions=nregions)
;  flashMask = flashmask*(watershedimg gt 1)

 ;------------------------ Object Flash Sites --------------------------
 t2 = systime(1)
  if obj_valid((*pState).oAutoROIModel) then DeleteAutoROI, pState
  (*pState).oWindow->Draw, (*pState).oView  
  widget_control, (*pState).ResultTable, get_value=table
  ROINoColumn = uint(reform(table[0,*]))
  lastRow = max(where(ROINoColumn ne 0))
  oldROINum = (*pState).result->length()

  areaAvg = 0.5
  minAreaThr = areaAvg/2.0
  maxAreaThr = areaAvg*2

  contour,flashMask, levels=[1], xmargin=[0,0], ymargin=[0,0], path_info=pathinfo, path_xy=pathxy, $
       /path_data_coords, /closed, /noerase
  regionNum = n_elements(pathinfo)
  if regionNum le 0 then return
  count = 0
  for regionNo=0,regionNum-1 do begin
    boundry = pathxy[*, pathinfo[regionNo].offset:pathinfo[regionNo].offset+pathinfo[regionNo].n-1]
    newRoi = obj_new('IDLgrROI', boundry[0,*], boundry[1,*], color=[255,0,0], alpha_channel=1)
    result = newRoi->ComputeGeometry(centroid=center)
    if n_elements(deadMap) ne 0 then begin
      if deadMap[center[0],center[1]] eq 1 then begin
        obj_destroy, newROI
        continue
      endif
    endif
    roiMask = newROI->ComputeMask(dimensions=[(*pState).xs,(*pState).ys])
    index = where(roiMask gt 0)
    area = n_elements(index)*(*pState).pxs^2
    if area le minareaThr then begin
      obj_destroy, newROI
      continue
    endif
    distance = ptr_valid((*pState).mask) ? DistToEdge(*(*pState).edge,center)*(*pState).pxs : 0    

    centerPeak = max((median(peak,3))[index])
    index = index[where(peak[index] gt centerPeak*0.4)]
    trace = GetTrace(pState,index)
    signal = reform((*trace[0])[0,*])
    fitPara = ladfit(*(*pState).correctedTime,signal)
    baseLine = fitPara[0]+fitPara[1]**(*pState).correctedTime
    signal -= baseLine
    sitePeak = max(smooth(signal,5,edge=1),peakTime)

		

      oLabel = obj_new('IDLgrText', string((*pState).result->Length()+1,format='(i0)'), $
        font=(*pState).oROIFont, color=[255,255,0], locations=center)
;    endelse
    (*pState).oLabelModel->Add, oLabel

    node = obj_new('ChainNode')
    node->SetProperty, oROI=newRoi, oLabel = oLabel, trace=trace, ROIPara=[Area,distance], flashNum=1
    info = (*pState).result->insert(node)
    (*pState).oAutoROIModel->Add, newRoi
    count++
    widget_control, (*pState).ResultTable, use_table_select=[0,lastrow+count,2,lastRow+count], $
      set_value=[string(oldROINum+count,format='(i0)'),string([area,distance],format='(f0.3)')]
  endfor
  if ptr_valid((*pState).flashmask) then *(*pState).flashmask = flashMask $
  else (*pState).flashmask = ptr_new(flashMask gt 0, /no_copy)
  (*pState).oWindow->Draw, (*pState).oView
  print, strtrim(count,1),' flash sites are detected!'

  ROINum = (*pState).result->length() 
  idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
  if ROINum lt 1 then widget_control, idROISlider, sensitive=0 $
  else widget_control, idROISlider,set_slider_max=ROINum,set_slider_min=1,set_value=1,/sensitive

t3 = systime(1)
;  tvscl,flashmask
  print, 'Time consumption for detecting flash: ', t3 - t1, ' s'
end