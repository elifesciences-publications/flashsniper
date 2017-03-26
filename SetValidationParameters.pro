Pro OnSetValidationParameters, event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF
  widget_control, event.top, GET_UVALUE=pState
  validPara_file = (*pState).file[0]+'-validation parameters. sav'
  if file_test(validPara_file) then begin
    restore, validPara_file
;    pValidPara = SetValidationParameters(pState,group_leader=event.top, validPara=validPara)
    validPara.ampMin = 0.0
    validPara.ampMax = 1.0
    *validPara.testK = findgen(3)*1+2.0
    for siteNo=0,validPara.siteN-1 do begin
      info = validPara.sites->GetProperty(siteNo,flashNum=peakTime)
      info = validPara.sites->SetProperty(siteNo,flashNum=peakTime gt 15)
    endfor
    pValidPara = ptr_new(validPara,/no_copy)
  endif else begin
    pValidPara = SetValidationParameters(pState,group_leader=event.top)
  endelse
  if ptr_valid(pValidPara) then begin
;    print, 'valid para'
    validPara = *pValidPara
    save, ValidPara, filename=validPara_file
    ptr_free, (*pValidPara).testK
    obj_destroy, (*pValidPara).sites
    ptr_free, pValidPara
  endif
End
;-----------------------------------------------------------------
pro SetValidationParameters_event
end
;-----------------------------------------------------------------
Function SetSite, cx, cy, r
  r2 = r^2
  site = [[-1,-1]]
  for i=cx-r,cx+r do begin
    for j=cy-r,cy+r do begin
      if (i-cx)^2+(j-cy)^2 le r2 then site = [[site],[i,j]]
    endfor
  endfor
  return, site[*,1:*]
end 
;-----------------------------------------------------------------
pro OnChangSiteRadius, Event
  widget_control, event.top, get_uvalue=pLocal
  (*pLocal).validPara.siteR = event.value
  if (*pLocal).validPara.siteN gt 0 then begin
    for siteNo=0,(*pLocal).validPara.siteN-1 do begin
      info = (*pLocal).validPara.sites->GetProperty(siteNo,oROI=oROI,ROIPara=ROIPara)
      oROI->SetProperty, data=SetSite(ROIPara[0],ROIPara[1],(*pLocal).validPara.siteR)
    endfor
    (*pLocal).oWin->Draw, (*pLocal).oView
  endif
end
;-----------------------------------------------------------------
Pro OnValidationWindowEvent, event
  if event.type ne 0  then return
  
  widget_control, event.top, get_uvalue=pLocal
  case event.press of
  4: begin   ; On right button press: select and delete a ROI
    selectObj= (*PLocal).oWin->Select((*PLocal).oView, [event.x,event.y],dimensions=[8,8])
    if n_elements(selectObj) gt 0 then begin   ; select
      for i=1, n_elements(selectObj)-1 do begin
        if obj_isa(selectObj[i], 'IDLgrROI') then begin
          currentRoi = (*PLocal).validPara.sites->SearchROI(selectOBJ[i],roiNo=roiNo)
          (*PLocal).oSiteModel->remove, selectObj[i]
          info = (*PLocal).validPara.sites->Delete(ROINo)
          (*pLocal).oWin->Draw, (*pLocal).oView
          (*pLocal).validPara.siteN = (*pLocal).validPara.sites->length()
          widget_control, (*pLocal).SiteNum_TEXT, set_value=strtrim((*pLocal).validPara.siteN,1)
          break
        endif
      endfor
    endif
  end
  1: begin   ; Create a new ROI
    oROI = obj_new('IDLgrROI', color=[255,100,0], data=SetSite(event.x,event.y,(*pLocal).validPara.siteR))
    (*pLocal).oSiteModel->add, oROI
    node = obj_new('ChainNode')
    node->SetProperty, oROI=oROI, ROIPara=[event.x,event.y]
    info = (*pLocal).validPara.sites->insert(node)
    (*pLocal).oWin->Draw, (*pLocal).oView
    (*pLocal).validPara.siteN = (*pLocal).validPara.sites->length()
    widget_control, (*pLocal).SiteNum_TEXT, set_value=strtrim((*pLocal).validPara.siteN,1)
  end
  else:
  endcase
End
;-----------------------------------------------------------------
Function ScanString, str, sep
  n = strlen(str)
  if n eq 0 then return,4.0
  sepPos = (strpos(str,sep))[0]
  if sepPos eq -1 or sepPos eq n-1 then return,float(str)
  
  value = float(strmid(str,0,sepPos))
  for k=sepPos+1,n-1 do begin
    if strmid(str,k,1) ne ',' then continue
    temp = strmid(str,sepPos+1,k-sepPos-1)
    sepPos = k
    if temp ne '' then value = [value,float(temp)]
  endfor
  if sepPos lt n-1 then value = [value,float(strmid(str,sepPos+1,n-sepPos-1))]
  return, value
End
;-----------------------------------------------------------------
pro OnSetParameter, Event
  widget_control, event.top, get_uvalue=pLocal
  if (*pLocal).validPara.siteN eq 0 then begin
    void = dialog_message('No flash sites have been set. Set one or click CANCEL!', /information)
    return
  endif
  widget_control, (*pLocal).riseTime_TEXT, get_value=riseTime
  (*pLocal).validPara.riseTime=float(riseTime)
  widget_control, (*pLocal).decayTime_TEXT, get_value=decayTime
  (*pLocal).validPara.decayTime=float(decayTime)
  widget_control, (*pLocal).ampMin_TEXT, get_value=ampMin
  (*pLocal).validPara.ampMin=float(ampMin)
  widget_control, (*pLocal).ampMax_TEXT, get_value=ampMax
  (*pLocal).validPara.ampMax=float(ampMax)
  widget_control, (*pLocal).ampStep_TEXT, get_value=ampStep
  (*pLocal).validPara.ampStep=float(ampStep)
  widget_control, (*pLocal).testK_TEXT, get_value=testK
  (*pLocal).validPara.testK=ptr_new(ScanString(testK[0],','),/no_copy)
  (*pLocal).IsSet = 1b
  widget_control, event.top, /destroy
end
;-----------------------------------------------------------------
pro OnCancel, Event
  widget_control, event.top, /destroy
end
;-----------------------------------------------------------------
Function SetValidationParameters, pState, GROUP_LEADER=wGroup, validPara=validPara
  dim = size(img, /dimensions)
;  print, validPara
  if n_elements(validPara) eq 0 then begin
    validPara = { siteN: 0L, $
      siteR: 4L, $
      riseTime: 6L, $
      decayTime: 14L, $
      ampMin: 0.0, $
      ampMax: 1.0, $
      ampStep: 0.02, $
      testK: ptr_new([2,3,4]), $
      sites: obj_new('Chain') }
  endif
  WID_BASE_0 = Widget_Base( GROUP_LEADER=wGroup, UNAME='WID_BASE_0'  $
      ,XOFFSET=5 ,YOFFSET=5 ,TITLE='Set Validation Parameters'  $
      ,SPACE=3 ,XPAD=3 ,YPAD=3 ,ROW=1,tlb_frame_attr=9,/modal )
  
  WID_BASE_1 = Widget_Base(WID_BASE_0,XOFFSET=3 ,YOFFSET=3 ,SPACE=5 ,COLUMN=1)
      
  WID_BASE_2 = Widget_Base(WID_BASE_1 ,FRAME=1  $
      ,SCR_XSIZE=170 ,SCR_YSIZE=80 ,SPACE=3 ,XPAD=3 ,YPAD=3)  
  WID_LABEL_0 = Widget_Label(WID_BASE_2, UNAME='WID_LABEL_0'  $
      ,XOFFSET=3 ,YOFFSET=3 ,/ALIGN_LEFT ,VALUE='Flash Site')  
  WID_LABEL_1 = Widget_Label(WID_BASE_2, UNAME='WID_LABEL_1'  $
      ,XOFFSET=10 ,YOFFSET=23 ,/ALIGN_LEFT ,VALUE='Number')  
  SiteNum_TEXT = Widget_Text(WID_BASE_2, UNAME='SiteNum_TEXT'  $
      ,XOFFSET=60 ,YOFFSET=20 ,VALUE=strtrim(validPara.siteN,1) ,XSIZE=2 ,YSIZE=1)  
  WID_LABEL_2 = Widget_Label(WID_BASE_2, UNAME='WID_LABEL_2'  $
      ,XOFFSET=10 ,YOFFSET=50 ,/ALIGN_LEFT ,VALUE='Radius')  
  SiteRadius_SLIDER = Widget_Slider(WID_BASE_2,  $
      UNAME='SiteRadius_SLIDER' ,XOFFSET=60 ,YOFFSET=35 ,MINIMUM=2  $
      ,MAXIMUM=6 ,VALUE=validpara.siteR ,event_pro='OnChangSiteRadius')
  
  WID_BASE_3 = Widget_Base(WID_BASE_1 ,FRAME=1  $
      ,YOFFSET=85 ,SCR_XSIZE=170 ,SCR_YSIZE=80 ,SPACE=3 ,XPAD=3 ,YPAD=3)  
  WID_LABEL_3 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_3'  $
      ,XOFFSET=3 ,YOFFSET=3 ,/ALIGN_LEFT ,VALUE='Amplitude')  
  WID_LABEL_4 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_4'  $
      ,XOFFSET=10 ,YOFFSET=23 ,/ALIGN_LEFT ,VALUE='Range  from')  
  AmpMin_TEXT = Widget_Text(WID_BASE_3, UNAME='AmpMin_TEXT' ,XOFFSET=79 ,YOFFSET=20 $
      ,/EDITABLE ,XSIZE=3 ,YSIZE=1 ,VALUE=string(validPara.ampMin,format='(f0.2)'))  
  WID_LABEL_5 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_5'  $
      ,XOFFSET=110 ,YOFFSET=23 ,/ALIGN_LEFT ,VALUE='to')  
  AmpMax_TEXT = Widget_Text(WID_BASE_3, UNAME='AmpMax_TEXT' ,XOFFSET=125 ,YOFFSET=20 $
      ,/EDITABLE ,XSIZE=3 ,YSIZE=1 ,VALUE=string(validPara.ampMax,format='(f0.2)'))  
  WID_LABEL_6 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_6'  $
      ,XOFFSET=10 ,YOFFSET=53 ,/ALIGN_LEFT ,VALUE='Step')  
  AmpStep_TEXT = Widget_Text(WID_BASE_3, UNAME='AmpStep_TEXT' ,XOFFSET=50 ,YOFFSET=50 $
      ,/EDITABLE ,XSIZE=3 ,YSIZE=1 ,VALUE=string(validPara.ampStep,format='(f0.2)'))
  
  WID_BASE_4 = Widget_Base(WID_BASE_1 ,FRAME=1  $
      ,YOFFSET=170 ,SCR_XSIZE=170 ,SCR_YSIZE=80 ,SPACE=3 ,XPAD=3 ,YPAD=3)  
  WID_LABEL_7 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_7'  $
      ,XOFFSET=3 ,YOFFSET=3 ,/ALIGN_LEFT ,VALUE='Typical Trace')  
  WID_LABEL_8 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_8'  $
      ,XOFFSET=10 ,YOFFSET=23 ,/ALIGN_LEFT ,VALUE='Rise Time (s)')  
  RiseTime_TEXT = Widget_Text(WID_BASE_4, UNAME='RiseTime_TEXT' ,XOFFSET=100 ,YOFFSET=20 $
      ,XSIZE=3 ,YSIZE=1 ,/EDITABLE ,VALUE=string(validPara.riseTime,format='(f0.2)'))  
  WID_LABEL_9 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_9'  $
      ,XOFFSET=10 ,YOFFSET=53 ,/ALIGN_LEFT ,VALUE='Decay Time (s)')  
  DecayTime_TEXT = Widget_Text(WID_BASE_4, UNAME='DecayTime_TEXT' ,XOFFSET=100 ,YOFFSET=50 $
      ,XSIZE=3 ,YSIZE=1 ,/EDITABLE ,VALUE=string(validPara.decayTime,format='(f0.2)'))
  
  WID_BASE_5 = Widget_Base(WID_BASE_1 ,FRAME=1  $
      ,YOFFSET=255 ,SCR_XSIZE=170 ,SCR_YSIZE=30 ,SPACE=3 ,XPAD=3 ,YPAD=3)  
  WID_LABEL_10 = Widget_Label(WID_BASE_5, UNAME='WID_LABEL_10'  $
      ,XOFFSET=3 ,YOFFSET=6 ,/ALIGN_LEFT ,VALUE='Test K')
  kValue = string((*validpara.testK)[0],format='(f0.2)')
  if n_elements(*validpara.testK) gt 1 then begin
    for i=1,n_elements(*validpara.testK)-1 do kvalue = [kvalue,',',string((*validpara.testK)[i],format='(f0.2)')]
  endif  
  TestK_TEXT = Widget_Text(WID_BASE_5, UNAME='TestK_TEXT' ,XOFFSET=50  $
      ,YOFFSET=3 ,VALUE=kValue ,XSIZE=10 ,YSIZE=1 ,/EDITABLE)
      
  WID_BASE_6 = Widget_Base(WID_BASE_0 ,XOFFSET=176 ,YOFFSET=3 ,COLUMN=1)  
  Site_DRAW = Widget_Draw(WID_BASE_6, UNAME='Site_DRAW' ,SCR_XSIZE=(*pState).xs ,SCR_YSIZE=(*pState).ys $
      ,classname='IDLitWindow' ,EVENT_PRO='OnValidationWindowEvent' ,RETAIN=2 ,GRAPHICS_LEVEL=2,/button_events)  
  WID_BASE_7 = Widget_Base(WID_BASE_6, YOFFSET=256 ,SPACE=10 ,ROW=1)  
  WID_BUTTON_0 = Widget_Button(WID_BASE_7, UNAME='WID_BUTTON_0'  $
      ,FRAME=1 ,SCR_XSIZE=100 ,SCR_YSIZE=28 ,/ALIGN_CENTER  $
      ,VALUE='Set Parameters' ,event_pro='OnSetParameter')  
  WID_BUTTON_1 = Widget_Button(WID_BASE_7, UNAME='WID_BUTTON_1'  $
      ,FRAME=1 ,XOFFSET=110 ,SCR_XSIZE=100 ,SCR_YSIZE=28  $
      ,/ALIGN_CENTER ,VALUE='Cancel' ,event_pro='OnCancel')

  Widget_Control, /REALIZE, WID_BASE_0
  widget_control, site_draw, get_value=oWin
  oView = obj_new('IDLgrView',viewplane_rect=[0,0,(*pState).xs,(*pState).ys])
  oModel = obj_new('IDLgrmodel')
  oView->Add, oModel
  oImage = obj_new('IDLgrImage', data=reform((*(*pState).img[0])[*,*,0]))
  oModel->Add, oImage
  oSiteModel = obj_new('IDLgrmodel')
  oView->Add, oSiteModel
  if validPara.siteN gt 0 then begin
    for siteNo=0,validPara.siteN-1 do begin
      info = validPara.sites->GetProperty(siteNo,oROI=oROI)
      oSiteModel->Add, oROI
    endfor
  endif 
  oWin->Draw, oView
  
  local = { SiteNum_TEXT: SiteNum_TEXT, $
  SiteRadius_SLIDER: SiteRadius_SLIDER, $
  AmpMin_TEXT: AmpMin_TEXT, $
  AmpMax_TEXT: AmpMax_TEXT, $
  AmpStep_TEXT: AmpStep_TEXT, $
  RiseTime_TEXT: RiseTime_TEXT, $
  DecayTime_TEXT: DecayTime_TEXT, $
  TestK_TEXT: TestK_TEXT, $
  Site_DRAW: Site_DRAW, $
  oWin: oWin, $
  oView: oView, $
  oModel: oModel, $
  oSiteModel: oSiteModel, $
  oImage: oImage, $  
  ValidPara: validPara, $
  IsSet: 0b, $
  pState: pState }
  pLocal = ptr_new(local,/no_copy)
  widget_control, wid_base_0, set_uvalue=pLocal
  XManager, 'SetValidationParameters', WID_BASE_0, /NO_BLOCK
   
  pValidPara = ptr_new()
  if (*pLocal).IsSet eq 0b then begin   
    obj_destroy, (*pLocal).validPara.sites
  endif else begin
    pValidPara = ptr_new((*pLocal).validPara,/no_copy)
    for siteNo=0,(*pValidPara).siteN-1 do begin
      info = validPara.sites->GetProperty(siteNo,oROI=oROI)
      oSiteModel->remove, oROI
    endfor
  endelse
  
  obj_destroy, (*pLocal).oView
  ptr_free, pLocal 
  return, pValidPara
end