;-------------------------------------------------------------------------------------
Pro OnCommandAnimation, Event
COMPILE_OPT STRICTARR
  widget_control, event.top, get_uvalue=pState
		ptr_free, (*pState).ima,(*pState).timing,(*pState).sum,(*pState).peak
	widget_control,event.top,set_uvalue = pstate
End
;-------------------------------------------------------------------------------------
Pro OnCommandAnimation5, Event
COMPILE_OPT STRICTARR
;synthetic linescane illustration of flash detection

  widget_control, event.top, get_uvalue=pState
  if ~obj_valid((*pState).result) then return
  if (*pState).result->length() lt 1 then return
  
  path = 'E:\My Documents\Data for Work\Cover figure\New\'
 
  idROISlider = widget_info((*pState).AnimationBase,find_by_uname='ROISlider')
  widget_control, idROISlider, get_value=ROINo
  info = (*pState).result->GetProperty(ROINo-1,oROI=oROI)
  if info eq -1 then begin
    void = dialog_message('The ROI No. is out of range!',/error)
  endif
  oROI->GetProperty, data=data
  min_x = min(data[0,*],max=max_x)
  min_y = min(data[1,*],max=max_y)
  mid_x = (min_x+max_x)/2
  mid_y = (min_y+max_y)/2
  r = 50
  xs = 2*r+1
  ts = (*pState).ts
  img = fltarr(ts,xs)
  for i=0,ts-1 do img[i,*] = reform((*(*pState).img[0])[mid_x-r:mid_x+r,mid_y,i])
;  for i=0,ts-1 do img[i,*] = reform((*(*pState).img[0])[mid_x,mid_y-r:mid_y+r,i])
  device, decomposed=0
  loadct,33,/silent
  tvlct, r,g,b,/get
  
  window,0,xs=ts,ys=xs
  tv,img
  write_tiff,'E:\synthetic linescan.tif',reverse(rebin(img*0.9,2*ts,2*xs),2),red=r,green=g,blue=b
;  return
;  window,1,xs=600,ys=500
;  newshow3,img
;  ima = tvrd(true=1) ;& print,min(total(ima,1))
;  mask = total(ima,1) gt min(total(ima,1))
;  dim = size(ima,/dimensions)
;  ima_trans = bytarr(4,dim[1],dim[2])
;  ima_trans[0:2,*,*] = ima
;  ima_trans[3,*,*] = mask*255
;  write_tiff,'E:\synthetic linescan-TV 3D.tif', reverse(ima_trans,3)      
;  shade_surf,img,zst=5,xst=4,yst=4,shades=img,/save,zrange=[-10,300]
;  ima = tvrd(true=1) ;& print,min(total(ima,1))
;  mask = total(ima,1) gt min(total(ima,1))
;  dim = size(ima,/dimensions)
;  ima_trans = bytarr(4,dim[1],dim[2])
;  ima_trans[0:2,*,*] = ima
;  ima_trans[3,*,*] = mask*255
;  write_tiff,'E:\synthetic linescan-2.5D.tif',reverse(ima_trans,3)
  
  X = *(*pState).correctedTime
  MaxRange = [2,(*pState).ts-1]
  FitRange = [0,(*pState).ts-1]
  fitX = X[FitRange[0]:FitRange[1]]
  peak = fltarr(xs)
  timing = fltarr(xs)
  residualImg = fltarr(ts,xs)
  for i=0,xs-1 do begin
    signal = reform(img[*,i])
    fitPara = ladfit(fitX,signal[FitRange[0]:FitRange[1]])
    baseLine = fitpara[0] + fitpara[1]*X
    residual = signal - baseLine
;    residual = median(residual,3)
    residualImg[*,i] = residual
    peak[i] = max((smooth(residual,5))[MaxRange[0]:MaxRange[1]],peakTime)
    timing[i] = peakTime[0]+MaxRange[0]
  endfor
  residualImg = ((residualImg-10)*2.2>0)<255
  residualImg = median(residualImg,3)
  window,2,xs=ts,ys=xs
  tvscl,residualImg
  write_tiff,'E:\synthetic linescan-signal change.tif',reverse(rebin(bytscl(residualImg),2*ts,2*xs),2),red=r,green=g,blue=b
;  window,3,xs=600,ys=500
;  newshow3,residualImg
;  ima = tvrd(true=1) ;& print,min(total(ima,1))
;  mask = total(ima,1) gt min(total(ima,1))
;  dim = size(ima,/dimensions)
;  ima_trans = bytarr(4,dim[1],dim[2])
;  ima_trans[0:2,*,*] = ima
;  ima_trans[3,*,*] = mask*255
;  write_tiff,'E:\synthetic linescan-signal change-TV 3D.tif', reverse(ima_trans,3)      
;  shade_surf,residualImg,zst=5,xst=4,yst=4,shades=residualImg-min(residualImg),/save,zrange=[-10,300]
;  ima = tvrd(true=1) ;& print,min(total(ima,1))
;  mask = total(ima,1) gt min(total(ima,1))
;  dim = size(ima,/dimensions)
;  ima_trans = bytarr(4,dim[1],dim[2])
;  ima_trans[0:2,*,*] = ima
;  ima_trans[3,*,*] = mask*255
;  write_tiff,'E:\synthetic linescan-signal change-2.5D.tif',reverse(ima_trans,3)
  
;  window,4,xs=400,ys=300
;  plot, peak
;  window,5,xs=400,ys=300
;  plot, timing
;  print, '---- peak -----'
;  print, peak, format='(1f10.3)'
  print, '---- timging ----'
  print, (*(*pState).correctedTime)[timing], format='(1f10.3)'
End

;-----------------------------------------------------------------
Pro OnCommandAnimation4, Event
COMPILE_OPT STRICTARR
; show the difference before and after photobleach correction

  widget_control, event.top,get_uvalue=pState
  img1_trans = bytarr(4,(*pState).xs,(*pState).ys)
  img1_trans[3,*,*] = *(*pState).mask*255
  img2_trans = img1_trans
  
  img1 = (reform((*(*pState).img[0])[*,*,0])>0)<255
  img2 = (reform((*(*pState).img[0])[*,*,(*pState).ts-1])>0)<255
  img1_trans[1,*,*] = img1
  img2_trans[1,*,*] = img2
  write_tiff, 'D:\photobleach 1_trans.tif', reverse(img1_trans,3)
  write_tiff, 'D:\photobleach 2_trans.tif', reverse(img2_trans,3)
  
  edgeMask = bytarr((*pState).xs,(*pState).ys)
  edgeMask[(*(*pState).edge)[0,*],(*(*pState).edge)[1,*]] = 1b
  temp = bytarr((*pState).xs+2,(*pState).ys+2)
  temp[1:(*pState).xs,1:(*pState).ys] = edgeMask
  temp = dilate(temp,replicate(1,3,3))
  edgeMask = temp[1:(*pState).xs,1:(*pState).ys]
  edge = where(edgeMask eq 1b)
  edgeColor = 255
  img1[edge] = edgeColor
  img2[edge] = edgeColor
  device, decomposed=0
  loadct, 8
  tvlct, r,g,b,/get
;  window,1,xs=(*pState).xs,ys=(*pState).ys & tv,img1
;  window,2,xs=(*pState).xs,ys=(*pState).ys & tv,img2
  write_tiff, 'D:\photobleach 1.tif', reverse(img1,2), red=r,green=g,blue=b
  write_tiff, 'D:\photobleach 2.tif', reverse(img2,2), red=r,green=g,blue=b
End
;-----------------------------------------------------------------
Pro OnCommandAnimation3, Event
COMPILE_OPT STRICTARR
; show local watershed segmentation result

  widget_control, event.top,get_uvalue=pState
  idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
  widget_control, idROISlider, get_value=ROINo
  info = (*pState).result->GetProperty(ROINo-1, oROI=oROI)
  if info eq -1 then return
  
  oROI->getproperty,data=data
  min_x = min(data[0,*],max=max_x)
  min_y = min(data[1,*],max=max_y)
  xs = (max_x-min_x+1)*2
  ys = (max_y-min_y+1)*2
;  baseImage = total((*(*pState).img[0])[*,*,0:2],3)/3
  baseImage = (*(*pState).img[0])[*,*,0:2]
  localBase = baseImage[min_x:max_x,min_y:max_y,0]
  localBase = rebin(localBase,2*xs,2*ys)
  
  peak = median(*(*pState).peak,3)
  peak = GaussianBlur(peak,1)
  localPeakMap = peak[min_x:max_x,min_y:max_y]
  localPeakMap = rebin(localPeakMap,2*xs,2*ys)
  
  peakThr = (*pState).Thr[0]*(*pState).sigma[0]+(*pState).mean[0]
  peakMask = peak gt peakThr
  localPeakMask = peakMask[min_x:max_x,min_y:max_y]
  localPeakMask = localPeakMap gt peakThr
  
  localpeakmap = GaussianBlur(localpeakmap,1.2)
  watershedimg = watershed(-peak*peakMask,connectivity=8,nregions=nregions)
  localwatershedimg = watershedimg[min_x:max_x,min_y:max_y]
  localwatershedimg = watershed(-localpeakMap,connectivity=8,nregions=nregions)
  localwatershedMask = localwatershedimg gt 1

  window,1,xs=400,ys=300
  tvscl,localBase,0
  tvscl,localPeakMap,1
  tvscl,localPeakMask,2
  tvscl,localwatershedimg*localpeakMask,3
  tvscl,localwatershedMask*localpeakMap,4
  write_tiff, 'D:\local base.tif', reverse(bytscl(localBase)*0.9,2)
  write_tiff, 'D:\local peakmap.tif', reverse(bytscl(localpeakmap)*0.9,2)
  write_tiff, 'D:\local peakmask.tif', reverse(bytscl(localpeakmask),2)
  write_tiff, 'D:\local watershedmask.tif', reverse(bytscl(localwatershedmask),2)
End
;-----------------------------------------------------------------
Pro OnCommandAnimation2, Event
COMPILE_OPT STRICTARR
; plant two or more flashes in one frame

  widget_control, event.top, get_uvalue=pState
  if ~obj_valid((*pState).result) then return
  if (*pState).result->length() lt 1 then return
  
  path = 'E:\My Documents\Data for Work\Cover figure\New\'
 
  basal = smooth(total((*(*pState).img[0])[*,*,0:9],3)/10,3,edge=1)
  
  idROISlider = widget_info((*pState).AnimationBase,find_by_uname='ROISlider')
  widget_control, idROISlider, get_value=ROINo
  info = (*pState).result->GetProperty(ROINo-1,oROI=oROI,trace=trace)
  if info eq -1 then begin
    void = dialog_message('The ROI No. is out of range!',/error)
  endif
  oROI->GetProperty, data=data
  min_x = min(data[0,*],max=max_x)
  min_y = min(data[1,*],max=max_y)
  xs = (max_x-min_x+1)*2
  ys = (max_y-min_y+1)*2
  base = basal[min_x:max_x,min_y:max_y]
  trace = (*trace[0])[0,*]
  peak = max(trace,peakT)
  
  device, decomposed=0
  loadct,3,/silent
  surf = ptrarr(18)
  
  k1 = peakT-10
  k2 = peakT+15
  ts = k2-k1+1
  zrange = [0,255]
  
  window,11,xs=ts*xs,ys=ys,xpos=0,ypos=100
  window,12,xs=ts*xs,ys=ys,xpos=0,ypos=150
  ima = intarr(ts*xs,ys)  
  
  window,1,xs=200,ys=150
  window,2,xs=200,ys=150
  shade_surf,rebin(reform((*(*pState).img[0])[min_x:max_x,min_y:max_y,peakT]),xs,ys),zrange=zrange,zstyle=1,image=surf,az=1
  dim = size(surf,/dimensions) & print,dim
  window,21,xs=ts*dim[0],ys=dim[1]
  window,22,xs=ts*dim[0],ys=dim[1]
;  window,21,xs=2000,ys=400
;  window,22,xs=2000,ys=400
   
  for k=k1,k2 do begin
    img = reform((*(*pState).img[0])[min_x:max_x,min_y:max_y,k])
;    img = total(((*(*pState).img[0])[min_x:max_x,min_y:max_y,k-1:k+1]),3)/3
    img = rebin(img,xs,ys)
    img = smooth(img,3,edge=1)
    
    wset,1 & shade_surf,img,az=1,zrange=zrange,zstyle=1,image=surf1
    wset,2 & shade_surf,img,az=1,zrange=zrange,zstyle=1,image=surf2,shades=bytscl(img)
    wset,21 & tv,surf1,k-k1
    wset,22 & tv,surf2,k-k1
    print,size(surf1,/dimensions)
    img *= 80/peak
    img = (img>0)<255
    wset,11 & tv,img,k-k1
    ima[(k-k1)*xs,0] = img
    img[0,*]=255 & img[xs-1,*]=255
    img[*,0]=255 & img[*,ys-1]=255
    wset,12 & tv,img,k-k1    
;    wait,0.05
  endfor

  wset,11
  window,1,xs=2000,ys=200
  shade_surf,ima,ax=30,az=1,zrange=zrange,image=surf1,xst=4,yst=4,zst=5
  window,2,xs=2000,ys=200
  shade_surf,ima,ax=30,az=1,zrange=zrange,image=surf2,xst=4,yst=4,zst=5,shades=bytscl(ima)
  
  wset,1 & ima=tvrd(true=1)
  write_tiff,path+'2.5D_series_red_'+strtrim(string(ROINo),1)+'-1.tif', reverse(ima,3)
  wset,2 & ima=tvrd(true=1)
  write_tiff,path+'2.5D_series_red_'+strtrim(string(ROINo),1)+'-2.tif', reverse(ima,3)
  wset,21 & ima=tvrd(true=1)
  write_tiff,path+'2.5D_series_red_'+strtrim(string(ROINo),1)+'-3.tif', reverse(ima,3)
  wset,22 & ima=tvrd(true=1)
  write_tiff,path+'2.5D_series_red_'+strtrim(string(ROINo),1)+'-4.tif', reverse(ima,3)
  wset,11 & ima=tvrd(true=1)
  write_tiff,path+'2D_series_red_'+strtrim(string(ROINo),1)+'-1.tif', reverse(ima,3)
  wset,12 & ima=tvrd(true=1)
  write_tiff,path+'2D_series_red_'+strtrim(string(ROINo),1)+'-2.tif', reverse(ima,3)
  
End
;-----------------------------------------------------------------
Pro OnCommandAnimation1, Event
COMPILE_OPT STRICTARR
; get 2.5D of partial cell with flashes (for cell cover, cut edges)

  widget_control, event.top, get_uvalue=pState
  if ~obj_valid((*pState).result) then return
  if (*pState).result->length() lt 1 then return
  
  path = 'E:\My Documents\Data for Work\Cover figure\New\'
 
  basal = total((*(*pState).img[0])[*,*,0:9],3)/10
  
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
  roiMask = oROI->ComputeMask(dimensions=[(*pState).xs,(*pState).ys])
  index = where(roiMask gt 0)
  indices = array_indices([(*pState).xs,(*pState).ys],index,/dimensions)
  xi = reform(indices[0,*])
  yi = reform(indices[1,*])
  
;  for i=11,16 do window,i,xs=1000,ys=600,xpos=(i-11)*110,ypos=(i-11)*70
  frame = basal
  ROI1 = 4
  ROI2 = 1
  ax = 30
  az = [20,100,110,140]
  device, decomposed=0
  loadct,33,/silent
  
  info = (*pState).result->GetProperty(ROI1-1,oROI=oROI,trace=trace)
  if info eq -1 then begin
    void = dialog_message('The ROI No. is out of range!',/error)
  endif
  oROI->GetProperty, data=data
  min_x1 = min(data[0,*],max=max_x1)
  min_y1 = min(data[1,*],max=max_y1)
  xs1 = max_x1-min_x1+1
  ys1 = max_y1-min_y1+1
  trace = (*trace[0])[0,*]
  peak = max(trace,peakT)
  frame1 = reform((*(*pState).img[0])[*,*,peakT])
  
  info = (*pState).result->GetProperty(ROI2-1,oROI=oROI,trace=trace)
  if info eq -1 then begin
    void = dialog_message('The ROI No. is out of range!',/error)
  endif
  oROI->GetProperty, data=data
  min_x2 = min(data[0,*],max=max_x2)
  min_y2 = min(data[1,*],max=max_y2)
  xs2 = max_x2-min_x2+1
  ys2 = max_y2-min_y2+1
  trace = (*trace[0])[0,*]
  peak = max(trace,peakT)
  frame2 = reform((*(*pState).img[0])[*,*,peakT])
    
  zrange = [0,255]
  frame[min_x1:max_x1,min_y1:max_y1] = frame1[min_x1:max_x1,min_y1:max_y1]
  frame[min_x2:max_x2,min_y2:max_y2] = frame2[min_x2:max_x2,min_y2:max_y2]
  
  img = frame[min_x:max_x,min_y:max_y]
  img = smooth(img,3,edge=1)
  img = rot(img,-11,/interp)
  mask = bytarr(xs,ys)
  mask[xi-min_x,yi-min_y] = 1b
  mask = rot(mask,-11,/interp)
  mask = morph_close(mask,replicate(1,25,3))
  mask = morph_open(mask,replicate(1,25,3))
  mask = median(mask,3)
  img[where(mask eq 0b)] = -1  
;  img = reverse(img,2)
;  img = reverse(img,1)
window,1 & tvscl,img,1 & tvscl,mask,2
;  retall
 
  window,0,xs=1400,ys=800
  for i=0,n_elements(az)-1 do begin
    wset,0
    shade_surf,img,ax=ax,az=az[i],zrange=zrange,zst=5,xst=4,yst=4,min_value=0,shades=bytscl(img)
    ima = tvrd(true=1) ;& print,min(total(ima,1))
    mask = total(ima,1) gt min(total(ima,1))
    dim = size(ima,/dimensions)
    ima_trans = bytarr(4,dim[1],dim[2])
    ima_trans[0:2,*,*] = ima
    ima_trans[3,*,*] = mask*255
    write_tiff,path+'2.5D_cell_flash_'+strtrim(string(ROINo),1)+'-'+strtrim(string(ROI1),1)+'-' + $
      strtrim(string(ROI2),1)+'-'+strtrim(string(az[i]),1)+'.tif', reverse(ima_trans,3)
    wait,0.01
  endfor
  
End