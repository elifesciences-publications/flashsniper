pro OnSemiAutoDetection, event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF
  
  widget_control, event.top, get_uvalue=pState
  if obj_valid((*pState).result) eq 0 then return
  if ~ptr_valid((*pState).peak) then begin
    OnSubtractBaseline, Event
    OnCalculateFeature, Event
  endif
  idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
  widget_control, idROISlider, get_value=ROINo
  info = (*pState).result->GetProperty(ROINo-1,oROI=oROI,trace=trace,ROIPara=ROIPara,flashNum=oldFlashNum)
  if info eq -1 then begin
    void = dialog_message('The ROI No. is out of range!',/error)
  endif
  
step = 0
while step lt 10 do begin  
  roiMask = oROI->ComputeMask(dimensions=[(*pState).xs,(*pState).ys]) gt 0
  peak = median(*(*pState).peak,3)
  r1 = 1 & r2 = 1.5
  disc1 = SHIFT(DIST(2*r1+1), r1, r1) LE r1
  disc2 = SHIFT(DIST(2*r2+1), r2, r2) LE r2
  thrH = (*pState).Thr[0] & thrL = (*pState).Thr[0];-0.4
  for thr=thrH,thrL,-0.2 do begin
    peakThr = thr*(*pState).sigma[0]+(*pState).mean[0]
    flashMask = (peak gt peakThr)*roiMask
    flashMask = median(flashMask,3)
    flashMask = morph_close(flashMask,disc1)
    flashMask = morph_open(flashMask,disc1) 
    flashMask *= roiMask
    index = where(flashMask eq 1)
    if index[0] eq -1 then continue
  endfor
;  window,1,xs=(*pState).xs,ys=(*pState).ys & tvscl, flashmask
  contour,flashMask, levels=[1], xmargin=[0,0], ymargin=[0,0], path_info=pathinfo, path_xy=pathxy, $
    /path_data_coords, /closed, /noerase
  if n_elements(pathxy) eq 0 then return
  boundry = pathxy;[*, pathinfo[0].offset:pathinfo[0].offset+pathinfo[0].n-1]
  oROI->SetProperty,data=boundry,style=2
  newroiMask = oROI->ComputeMask(dimensions=[(*pState).xs,(*pState).ys]) gt 0
  if max(newROIMask - roiMask) eq 0 then break
  step++
endwhile

  result = oRoi->ComputeGeometry(centroid=center, area=area)
  area *= ((*pState).pxs)^2
  distance = ptr_valid((*pState).mask) ? DistToEdge(*(*pState).edge,center)*(*pState).pxs : 0
  
  centerPeak = max(median(peak[index],3))
  index = index[where(peak[index] gt centerPeak*0.5)]
  trace = GetTrace(pState,index)
  info = (*pState).result->SetProperty(ROINo-1,oROI=oROI,trace=trace,ROIPara=[Area,distance],flashNum=1)
  if info eq -1 then begin
    void = dialog_message('The ROI No. is out of range!',/error)
  endif
  oroi->GetProperty,parent=oParent
  if oParent eq (*pState).oManualROIModel then begin
    (*pState).oManualROIModel->Remove, oROI
    (*pState).oAutoROIModel->Add, oRoi
  endif
  (*pState).oWindow->Draw, (*pState).oView
  
  widget_control, (*pState).ResultTable, get_value=table
  ROINoColumn = uint(reform(table[0,*]))
  row = where(ROINoColumn eq ROINo,n_row)
  if n_row eq 0 then return
  widget_control, (*pState).ResultTable, use_table_select=[1,row[0],2,row[0]], $
    set_value=[string([area,distance],format='(f0.3)')]
  if n_row gt 1 then $
    widget_control, (*pState).ResultTable, use_table_select=[1,row[1:*],2,row[1:*]],/delete_rows
end