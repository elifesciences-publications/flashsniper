Pro ChangepState, input, pState
  for k=0,(*pState).stackNum-1 do (*pState).oView->Remove,(*pState).oAnimationModel[k]
  obj_destroy,(*pState).oAnimationModel
  
  *input[0] = float((*input[0])[*,*,*(*pState).validFrame])
  ;spatial crop of 405
  
  State = { file: (*pState).file, $
    stackNum: 2, $
    stackColor: [[(*pState).stackcolor],[255,0,0]], $
    xs: (*pState).xs, $
    ys: (*pState).ys, $
    ts: (*pState).ts, $
    pxs: (*pState).pxs, $
    pys: (*pState).pys, $
    pts: (*pState).pts, $
    img: [(*pState).img,input[0]], $
    ima: (*pState).ima, $
    startPic:(*pState).startPic, $
    endPic:(*pState).endPic,  $
    validFrame: (*pState).validFrame, $
    crop: (*pState).crop, $
    back: [[*(*pState).back],fltarr(1,(*pState).ts)], $
    time: (*pState).time, $
    correctedTime: (*pState).correctedTime, $
    shift488: (*pState).shift488,  $
    mask: (*pState).mask,  $
    edge: (*pState).edge,  $
    flashMask: (*pState).flashMask,  $
    timing: (*pState).timing,  $
    sum: (*pState).sum,  $
    peak: (*pState).peak,  $
    imaHist: (*pState).imaHist, $
    peakHist: (*pState).peakHist, $
    sumHist: (*pState).sumHist, $
    MainBase: (*pState).MainBase, $
    AnimationBase: (*pState).AnimationBase, $
    DetectionThrBase: (*pState).DetectionThrBase, $
    ContextBase1: (*pState).ContextBase1, $
    TraceDraw: (*pState).TraceDraw, $
    TraceWindow: (*pState).TraceWindow, $
    ResultTable: (*pState).ResultTable, $
    oWindow: (*pState).oWindow,  $   ; not save
    oObserver: (*pState).oObserver,  $   ; not save
    oView: (*pState).oView,  $
    oAnimationModel: objarr(2),  $   ;not save
    oAutoROIModel: (*pState).oAutoROIModel, $
    oManualROIModel: (*pState).oManualROIModel, $
    oLabelModel: (*pState).oLabelModel, $
    oROIFont: (*pState).oROIFont, $
    newRoi: (*pState).newROI,  $
    result: (*pState).result,  $
    cursor: (*pState).cursor, $
    pHBMPara: (*pState).pHBMPara, $
    option:(*pState).option, $
    interval: (*pState).interval, $
    RoiHide: (*pState).ROIHide, $
    LabelHide: (*pState).LabelHide, $
    Brightness: (*pState).Brightness, $
    Contrast: (*pState).Contrast, $
    mean:(*pState).mean, $
    sigma: (*pState).sigma, $
    Thr: (*pState).Thr }

  pState = ptr_new(sTate,/no_copy)
End

Pro AddTrace405, pState
  if ~ptr_valid(pState) then return
  if ~obj_valid((*pState).result) then return
  ROINum = (*pState).result->length()
  if ROINum eq 0 then return
  if ptr_valid((*pState).peak) then begin
    peak = median(*(*pState).peak,3)
  endif  
  for ROINo=1, ROINum do begin
    info = (*pState).result->GetProperty(ROINo-1,oROI=oROI,trace=trace)
    if info eq -1 then continue
    
    roiMask = oROI->ComputeMask(dimensions=[(*pState).xs,(*pState).ys])
    index = where(roiMask gt 0)
    if n_elements(peak) gt 0 then begin
      centerPeak = max(peak[index])
      index = index[where(peak[index] ge centerPeak*0.5)]
    endif
    trace = GetTrace(pState,index)
    info = (*pState).result->SetProperty(ROINo-1,trace=trace)
  endfor
End