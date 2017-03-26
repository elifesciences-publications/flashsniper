
;Pro PlotPeakHist, gs, thr
;COMPILE_OPT STRICTARR
;  device, decomposed=1
;  wset, gs.PeakHistWindow
;;  maxi = max((*(*gs.pState).peakHist)[*,1],pos)
;;  cutoff = min(where((*(*gs.pState).peakHist)[pos:*,1] lt maxi*0.01))+pos
;;  cutoff = (cutoff+10) < (n_elements((*(*gs.pState).peakHist)[*,0])-1)
;;, (*(*gs.pState).peakHist)[0:cutoff,0]				[0:cutoff,1]
;;  plot,(*(*gs.pState).peakHist), $
;;    psym=-4, background='FFFFFF'XL, color='000000'XL, title='Peak Feature Histogram', $
;;    xtitle='F', font=0, thick=1.2
;;
;;  oplot, replicate(thr,10), findgen(10)*1e10, color='FF0000'XL, thick=2
;  peak = median(*(*gs.pState).peak,3)
;;  (*pState).Thr[0] = (pthreshold(*(*pstate).peak,0.1) - (*pState).mean[0])/(*pState).sigma[0]
;  peakThr = pthreshold(peak,0.01*(*gs.pState).Thr[0])
;  mask = peak gt peakThr
;  tvscl,peak,0,0,2
;  tvscl,mask,0,0,1
;End
;;----------------------------------------------------------
;pro SetDetectionThreshold_event, Event
;COMPILE_OPT STRICTARR
;
;  widget_control, event.top, get_uvalue=gs
;
;  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
;      widget_info(Event.id, /tree_root) : event.id)
;
;  case wTarget of
;    Widget_Info(event.top, FIND_BY_UNAME='PeakThrSlider'): begin
;      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_SLIDER' )then begin
;;        widget_control, gs.PeakThrLabel, set_value=strtrim(string(event.value));/10.0),1)
;;        (*gs.pState).Thr[0] = 101-event.value;/10.0
;        PlotPeakHist, gs,101-(*gs.pState).Thr[0];*(*gs.pState).sigma[0]+(*gs.pState).mean[0]
;        if event.drag eq 0 then begin
;          DetectFlash, gs.pState
;        endif
;      endif
;    end
;    else:
;  endcase
;
;end
;
;;----------------------------------------------------------
;pro SetDetectionThreshold, pState, GROUP_LEADER=wGroup
;COMPILE_OPT STRICTARR
;
;	maxp = max(*(*pstate).peak,min = minp)	
;	xs = (*pstate).xs+20
;	ys = (*pstate).ys+50
;	
;  DetectionThrBase = Widget_Base( GROUP_LEADER=wGroup,  $
;      UNAME='DetectionThrBase' ,XOFFSET=900 ,YOFFSET=100 ,SCR_XSIZE=xs  $
;      ,SCR_YSIZE=ys ,TITLE='Set Detection Threshold' ,SPACE=3  $
;      ,XPAD=3 ,YPAD=3)
;      
;
;;	maxp = (maxp - (*pstate).mean[0])/(*pstate).sigma[0]*10
;;	minp = (minp - (*pstate).mean[0])/(*pstate).sigma[0]*10
;	
;  WID_BASE_0 = Widget_Base(DetectionThrBase, UNAME='WID_BASE_0'  $
;      ,XOFFSET=10 ,YOFFSET=10 ,SCR_XSIZE=410 ,SCR_YSIZE=55  $
;      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)
;  PeakThrSlider = Widget_Slider(WID_BASE_0, UNAME='PeakThrSlider'  $
;      ,XOFFSET=80 ,YOFFSET=10 ,SCR_XSIZE=190 ,SCR_YSIZE=32  $
;      ,TITLE='Peak threshold (%)' ,MINIMUM=0  $
;      ,MAXIMUM=100, value=101-(*pState).Thr[0], /drag)
;;  PeakThrLabel = Widget_Label(WID_BASE_0, UNAME='PeakThrLabel'  $
;;      ,XOFFSET=275 ,YOFFSET=14 ,/ALIGN_c ,VALUE=strtrim(string((*pState).Thr[0]),1))
;
;  WID_BASE_1 = Widget_Base(DetectionThrBase, UNAME='WID_BASE_1'  $
;      ,XOFFSET=10 ,YOFFSET=60 ,SPACE=10 ,ROW=1)
;  PeakHistDraw = Widget_Draw(WID_BASE_1, UNAME='PeakHistWindow'  $
;      ,SCR_XSIZE=xs ,SCR_YSIZE=ys)
;
;  Widget_Control, /REALIZE, DetectionThrBase
;
;  (*pState).DetectionThrBase = DetectionThrBase
;  widget_control, PeakHistDraw, get_value=PeakHistWindow
;  gs = {pState: pState, $
;        PeakThrSlider: PeakThrSlider, $
;;        PeakThrLabel: PeakThrLabel, $
;        PeakHistWindow: PeakHistWindow }
;  PlotPeakHist, gs, 100-(*gs.pState).Thr[0];*(*gs.pState).sigma[0]+(*gs.pState).mean[0]
;
;  widget_control, DetectionThrBase, Set_uvalue=gs, /no_copy
;
;  XManager, 'SetDetectionThreshold', DetectionThrBase, /NO_BLOCK
;
;end