
Pro SetTrace, trace, peak, riseTime, decayTime
  trace[0] = findgen(riseTime+1)*peak/riseTime
  trace[riseTime] = reverse(indgen(decayTime+1)*peak/decayTime)
end
;-----------------------------------------------------------------
Function ImageQuality, pState

  ; -------- imaHist -------
  bin = 1
  mask = erode(*(*pState).mask,replicate(1,3,3))
  index = where(mask eq 1)
  n = n_elements(index)
  xys = (*pState).xs*(*pState).ys
  pimg = ptr_new(fltarr((*pState).ts*n),/no_copy)
  for i=0L,(*pState).ts-1 do (*pImg)[i*n:(i+1)*n-1] = (*(*pState).img[0])[index+i*xys]

  imgMin = min(*pimg, max=imgMax);+10
  imgMin = fix(imgMin) & imgMax = round(imgMax+0.499); &print,imgmin,imgmax
  imgHist = histogram(*pimg, min=imgMin,binsize=bin)
  imgX = findgen(n_elements(imgHist))*bin + imgMin
  imgHistMax = max(imgHist,maxPos)
  newimgHist = imgHist & newimgHist[maxPos] = newimgHist[maxPos+1]*1.2
  A = [(newimgHist[maxPos]), imgX[maxPos], 3]; & print, A
  gaussPara = GaussFit(imgX[(maxPos-20)>0:maxPos+15],newimgHist[(maxPos-20)>0:maxPos+15],A,estimates=A,nterms=3)
  fitHist = A[0]*exp(-(imgX-A[1])^2/2/A[2]^2); & print,A
  SNR = a[1]/(*pState).sigma[1]^2
  ptr_free, pimg

  quality = { SNR: SNR, $
    imgM: A[1], $
    imgS: abs(A[2]), $
    imaM: (*pState).mean[1], $
    imaS: (*pState).sigma[1], $
    peakM: (*pState).mean[0], $
    peakS: (*pState).sigma[0] }
  return, quality
End
;-----------------------------------------------------------------
Function AnalyzeValidation, pState, validPara, siteMap
  Nd = (*pState).result->length()
  if Nd eq 0 then begin
    output = [string([0,0,0]),string([0.0,1.0,0.0,0.0])]
    return, output
  endif

  Nt = 0
  DAmp = 0.0
  newSiteMap = uint(siteMap gt 0)
  for ROINo=0,(*pState).result->length()-1 do begin
    info = (*pState).result->GetProperty(ROINo,oROI=oROI,trace=trace,FlashPara=pFlashPara)
    info = oROI->ComputeGeometry(centroid=centroid)
    ROIMask = oROI->ComputeMask(dimensions=[(*pState).xs,(*pState).ys]) gt 0
    valid = where(newSiteMap+ROIMask eq 4)
    if valid[0] ne -1 then begin
      Nd--
      continue
    endif
    valid = where(newSiteMap+ROIMask eq 2)
    if valid[0] eq -1 then continue
    Nt++
    DAmp += (*pFlashPara)[3]
    tag = search2d(newSiteMap,valid[0] mod (*pState).xs,valid[0]/(*pState).xs,1,1,/diagonal)
    if tag[0] ne -1 then newSiteMap[tag] = 3
;   newtrace = reform(*trace[0])
;   baseLine = BaseLineFit(*(*pState).correctedTime,newtrace)
;   eLine = newtrace-baseLine
;   peak = max(eLine,peakpos)
;   DAmp += newtrace[peakPos]/baseline[peakPos]-1
  endfor
  if Nt gt 0 then DAmp /= Nt
  Nf = Nd-Nt
  Sen = float(Nt)/validPara.siteN
  FNR = 1-Sen
  PPV = float(Nt)/Nd
  output = [string([Nd,Nt,Nf]),string([Sen,FNR,PPV,DAmp])]
  return, output

End
;-----------------------------------------------------------------
Pro OnValidation, Event, SNR=SNR
COMPILE_OPT STRICTARR

  on_error,0
  widget_control, event.top, GET_UVALUE=pState

  ; load or set validation parameters
  validPara_file = (*pState).file[0]+'-validation parameters. sav'
  if file_test(validPara_file) then begin
    restore, validPara_file
    pValidPara = ptr_new(validPara,/no_copy)
  endif else begin
    pValidPara = SetValidationParameters(pState,group_leader=event.top)
    if ptr_valid(pValidPara) then begin
      validPara = *pValidPara
      save, ValidPara, filename=validPara_file
    endif else begin
      void = dialog_message('User canceled operation!',/information)
      return
    endelse
  endelse
  *(*pValidPara).testK = [2.0,3.0,4.0]

  ; calculate paras in constructing new image stack
  quality = ImageQuality(pState)
;  openw,unit,file_dirname((*pState).file[0])+'\SNR LOG.txt',/append,width=2000,/get_lun
;  printf,unit,file_basename((*pState).file[0]),quality.SNR
;  free_lun,unit
  if n_elements(SNR) ne 0 then begin
    *(*pState).img[0] = temporary(*(*pState).img[0])+quality.imgM*(snr/quality.SNR-1)
    quality = ImageQuality(pState)
  endif else SNR=0.0

  ; Initialize embeded flash parameters
  sites = ptrarr((*pValidPara).siteN)
  flash = ptrarr((*pValidPara).siteN)
  peakT = lonarr((*pValidPara).siteN)
  F0Arr = fltarr((*pValidPara).siteN)
  trace = fltarr((*pstate).ts)
  xys = double((*pstate).xs)*(*pstate).ys
  siteMap = bytarr((*pstate).xs,(*pstate).ys)
  for siteNo=0,(*pValidPara).siteN-1 do begin
    info = (*pValidPara).sites->GetProperty(siteNo,oROI=oROI)
    index = where(oROI->ComputeMask(dimensions=[(*pState).xs,(*pState).ys]) gt 0)
    siteMap[index] = 1b
    peakTime = long(randomu(seed)*((*pState).ts-40))+20
    peakT[siteNo] = peakTime
    startTime = peakTime-(*pValidPara).riseTime
;    F0Arr[siteNo] = mean((*(*pState).img[0])[index,index+(startTime-1)*xys,index+(startTime-2)*xys])
    pTrace = GetTrace(pState,index)
    newtrace = reform(*pTrace[0])
    para = ladfit(*(*pState).correctedTime,newtrace)
    baseLine = para[0]+para[1]**(*pState).correctedTime
    F0Arr[siteNo] = baseline[peakTime]
    F0Arr[siteNo] = baseline[startTime]
    sites[siteNo] = ptr_new(index,/no_copy)
    flash[siteNo] = ptr_new(fltarr((*pState).ts),/no_copy)
  endfor

  ; Check dead ROI map
  if file_test((*pState).file[0]+'-dead ROI map.tif') then begin
    deadMap = read_tiff((*pState).file[0]+'-dead ROI map.tif') gt 0
    deadMap = reverse(deadMap,2)
    overlap = where(uint(deadMap)+siteMap eq 2)
    while overlap[0] ne -1 do begin
      pos = array_indices([(*pState).xs,(*pState).ys],overlap[0],/dimensions)
      temp = search2d(deadMap,pos[0],pos[1],1,1)
      deadMap[temp] = 0
      overlap = where(uint(deadMap)+siteMap eq 2)
    endwhile
  endif

  ;-----------------------------------------
;  ptr_free, sites, flash, (*pValidPara).testK
;  obj_destroy, (*pValidPara).sites
;  ptr_free, pValidPara
;  print, ptr_valid(pValidPara)
;  return
  ;-----------------------------------------

  ; backup the original image stack
  pathName = file_dirname((*pState).file[0])
  fileName = file_basename((*pState).file[0])
  originalImageFile = (*pState).file[0]+'-SNR '+strtrim(snr,1)+'.dat'
  if ~file_test(originalImageFile) then begin
    openw, unit,originalImageFile,/get_lun
    writeu, unit, *(*pState).img[0]
    free_lun, unit
  endif

  ; begin validation test
  ampMin = 0.0
  ampMax = 0.5
  ampStep = 0.02
;  ampMin = (*pValidPara).ampMin
;  ampMax = (*pValidPara).ampMax
;  ampStep = (*pValidPara).ampStep
  for amp=ampMin,ampMax,ampStep do begin
    newFileName = (*pState).file[0]+'-'+string(amp,format='(f0.2)')
    print,'The testing amplitude is ', strtrim(amp,1)

    ; load original image stack
    openr, unit,originalImageFile,/get_lun
    readu, unit, *(*pState).img[0]
    free_lun, unit

    ; generate new image stack
    trace[0] = findgen((*pValidPara).riseTime+1)*amp/(*pValidPara).riseTime
    trace[(*pValidPara).riseTime] = reverse(indgen((*pValidPara).decayTime+1)*amp/(*pValidPara).decayTime)
    for SiteNo=0,(*pValidPara).siteN-1 do *flash[SiteNo] = shift(trace,PeakT[SiteNo])
    for k=0,(*pState).ts-1 do begin
      imgTemp = reform((*(*pState).img[0])[*,*,k])
      for SiteNo=0,(*pValidPara).siteN-1 do begin
;        F0 = mean(imgTemp[*sites[SiteNo]])
        m = mean(imgTemp[*sites[SiteNo]])
        F0 = F0Arr[siteNo]
        imgTemp[*sites[SiteNo]] = (imgTemp[*sites[SiteNo]]-m+F0*(1+*flash[SiteNo])[k]) < 255
      endfor
      (*(*pState).img[0])[*,*,k] = imgTemp
    endfor

    ; update new image display
;    (*pState).oWindow->RemoveWindowEventObserver, (*pState).oObserver
;    obj_destroy, (*pState).oObserver
;    obj_destroy, (*pState).oAnimationModel
;    CreatAnimationModel, pState

    ; Get features
    featureFile = newFileName+'-Feature-SNR '+strtrim(SNR,1)+'.dat'
    if file_test(featureFile) then begin
      openr, unit,featureFile,/get_lun
      imaHistDim = [1L,2L] & peakHistDim = [1L,2L]
      mean = (*pState).mean & sigma = (*pState).sigma
      peak = fltarr((*pState).xs,(*pState).ys)
      timing = intarr((*pState).xs,(*pState).ys)
      readu, unit, peak, timing, mean, sigma, imaHistDim, peakHistDim
      (*pState).peak = ptr_new(peak,/no_copy) & (*pState).timing = ptr_new(timing,/no_copy)
      (*pState).mean = mean & (*pState).sigma = sigma
      (*pState).imaHist = ptr_new(fltarr(imaHistDim),/no_copy)
      (*pState).peakHist = ptr_new(fltarr(peakHistDim),/no_copy)
      readu, unit, *(*pState).imaHist, *(*pState).peakHist
      free_lun, unit ;& print,'feature loaded'
;      write_tiff,newFileName+'-peak.tif',reverse(bytscl(median(*(*pState).peak,3)),2)
    endif else begin
      info = SubtractBaseline(pState)
      info = CalculateFeature(pState)
      openw, unit,featureFile,/get_lun
      writeu, unit, *(*pState).peak,*(*pState).timing,(*pState).mean,(*pState).sigma, $
        long(size(*(*pState).imaHist,/dimensions)), long(size(*(*pState).peakHist,/dimensions)), $
        *(*pState).imaHist, *(*pState).peakHist
      free_lun, unit
      write_tiff,newFileName+'-peak.tif',reverse(bytscl(median(*(*pState).peak,3)),2)
    endelse

    ; detect and analyze
    output = ''
    for k=0,n_elements(*(*pValidPara).testK)-1 do begin
      (*pState).Thr[0] = (*(*pValidPara).testK)[k]
      if n_elements(deadMap) ne 0 then $
        DetectFlash, pState, deadMap=deadMap $
      else DetectFlash, pState
      OnAnalyzeAllROI, Event
      result = AnalyzeValidation(pState, *pValidPara,siteMap)
      output = [output, string((*(*pValidPara).testK)[k],format='(f0.1)'),string(amp,format='(f5.2)'),result]
    endfor

    ; write validation result
    openw, unit, (*pState).file[0]+'-validation result-SNR'+strtrim(SNR,1)+'.txt', width=2000 ,/append ,/get_lun
    if amp eq ampMin then begin
      printf, unit,'SNR',quality.snr,' imgM',quality.imgM,' imgS',quality.imgS, $
        ' imaM',quality.imaM,' imaS',quality.imaS,' peakM',quality.peakM,' peakS',quality.peakS
      SectionTitle = ['K ','EAmp ','Nd ','Nt ','Nf ','Sen ','FNR ','PPV ','DAmp ']
      title = ''
      for k=0,n_elements(*(*pValidPara).testK)-1 do  title = [title,sectionTitle]
      printf, unit, title
    endif
    printf, unit, output
    free_lun, unit

    ; write to log
;    openw, unit, 'E:\My Documents\Data for Work\Flash Sniper\for validation\validation log.txt' $
;      , width=800 ,/append ,/get_lun
;    printf, unit, fileName,amp
;    free_lun, unit
  endfor
  ptr_free, sites, flash, (*pValidPara).testK
  obj_destroy, (*pValidPara).sites
  ptr_free, pValidPara

  ; load original image stack
;  openr, unit,originalImageFile,/get_lun
;  readu, unit, *(*pState).img[0]
;  free_lun, unit

End