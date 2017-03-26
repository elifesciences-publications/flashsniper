
function BlockMatching, tar_f, ref_f, ROIx, ROIy, est_mv, sr, max_r, theta=theta, unit=unit
COMPILE_OPT STRICTARR
;  print,'sr',sr
	srx = sr[0] & sry = sr[1]
	final_mv = [0,0,0]
	ref_block = ref_f[ROIx,ROIy]
	ref_nor = sqrt(total(ref_block^2,/double))
	max_r = -1

	if keyword_set(theta) then begin
		if keyword_set(unit) then n = theta/unit $
		else begin
			unit = 1d & n = theta
		endelse
		x0 = ROIx[0] + est_mv[0] & y0 = ROIy[0] + est_mv[1]
		ii = ROIx-ROIx[0] & jj = ROIy-ROIy[0]
		for i=-srx, srx do begin
		for j=-sry,sry do begin
		for k=-n,n do begin
			rad = k*unit*!PI/180.0 + angle0
			xx = ii*cos(rad) -jj*sin(rad) + x0 + i
			yy = ii*sin(rad) + jj*cos(rad) + y0 + j
			tar_block = Bilinear(tar_f, xx,yy) ;& wset,1 & tvscl, ref_block, 0 & tvscl, tar_block, 1
;			tar_nor = sqrt(total(tar_block^2,/double))
;			r = total(ref_block*tar_block)/ref_nor/tar_nor
			r = correlate(tar_block,ref_block)
			if r gt max_r then begin
				max_r = r
				final_mv = [i,j,k*unit]
			endif
		endfor
		endfor
		endfor
		final_mv += est_mv
	endif else begin
		rr = fltarr(2*srx+1,2*sry+1)
		for i=-srx, srx do begin
		for j=-sry,sry do begin
			tar_block = tar_f[ROIx+i+est_mv[0],ROIy+j+est_mv[1]]
;			tar_nor = sqrt(total(tar_block^2,/double))
;			r = total(ref_block*tar_block)/ref_nor/tar_nor
			r = correlate(tar_block,ref_block)			
			rr[i+srx,j+sry] = r
			if r gt max_r then begin
				max_r = r
				final_mv[0:1] = [i,j]
			endif
		endfor
		endfor
;		window,1 & plot, rr[*,sry], /ynozero & cursor,x,y,2
;		print,rr
		final_mv[0:1] += est_mv[0:1]
	endelse
	return, final_mv
end

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Function DetectMotion, tar_frame, ref_frame, roi, pPara
COMPILE_OPT STRICTARR

; new_roi = old_roi + offset
	tar_frame = double(tar_frame)
	ref_frame = double(ref_frame)
	dim = size(tar_frame,/dimension)
	xs = dim[0] & ys = dim[1]
	mv = [0,0,0]
	para = [[(*pPara).l1x,(*pPara).l1y],[(*pPara).l2x,(*pPara).l2y],[(*pPara).l3x,(*pPara).l3y]]
;	print,para

	for level=0,0,-1 do begin
		sub_roi = fix(roi/2^level)
		ROIx = lindgen(sub_roi[1]-sub_roi[0]+1, sub_roi[3]-sub_roi[2]+1) mod (sub_roi[1]-sub_roi[0]+1)
		ROIy = lindgen(sub_roi[1]-sub_roi[0]+1, sub_roi[3]-sub_roi[2]+1) / (sub_roi[1]-sub_roi[0]+1)
		mv = BlockMatching(congrid(tar_frame,xs/2^level,ys/2^level,/interp), $
			congrid(ref_frame,xs/2^level,ys/2^level,/interp), ROIx, ROIy, [mv[0:1]*2,mv[2]], para[*,2-level], max_r)
	endfor
;	print, max_r
	return,mv
End

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Function GaussianBlur, ima, sigma
COMPILE_OPT STRICTARR

	M = GaussianMask(sigma)
	img = convol(double(ima), M, /center, /edge_truncate)
	return, img

End

;-----------------------------------------------------------------
pro SetHBMPara_event, event
common SetHBMPara, start
  widget_control, event.top, get_uvalue=pState
  case event.id of
    Widget_Info(event.top, FIND_BY_UNAME='Reset'): begin
      widget_control, widget_info(event.top, find_by_uname='l1x'), $
          set_value=string((*(*pState).pHBMPara).l1x, format='(i0)')
      widget_control, widget_info(event.top, find_by_uname='l1y'), $
          set_value=string((*(*pState).pHBMPara).l1y, format='(i0)')
      widget_control, widget_info(event.top, find_by_uname='l2x'), $
          set_value=string((*(*pState).pHBMPara).l2x, format='(i0)')
     widget_control, widget_info(event.top, find_by_uname='l2y'), $
          set_value=string((*(*pState).pHBMPara).l2y, format='(i0)')
      widget_control, widget_info(event.top, find_by_uname='l3x'), $
          set_value=string((*(*pState).pHBMPara).l3x, format='(i0)')
      widget_control, widget_info(event.top, find_by_uname='l3y'), $
          set_value=string((*(*pState).pHBMPara).l3y, format='(i0)')
      widget_control, widget_info(event.top, find_by_uname='angle'), $
          set_value=string((*(*pState).pHBMPara).angle, format='(i0)')
      widget_control, widget_info(event.top, find_by_uname='unit'), $
          set_value=string((*(*pState).pHBMPara).unit, format='(i0)')
      widget_control, widget_info(event.top, find_by_uname='TrackStart'), set_value='0'
      widget_control, widget_info(event.top, find_by_uname='TrackEnd'), $
          set_value=string((*pState).ts-1, format='(i0)')
    end
    Widget_Info(event.top, FIND_BY_UNAME='Rotation'): begin
      (*(*pState).pHBMPara).IsRotation = event.select
      widget_control, widget_info(event.top, find_by_uname='angle'), editable=event.select
      widget_control, widget_info(event.top, find_by_uname='unit'), editable=event.select
    end
    Widget_Info(event.top, FIND_BY_UNAME='Start'): begin
      widget_control, widget_info(event.top, find_by_uname='l1x'), get_value=l1x
      widget_control, widget_info(event.top, find_by_uname='l1y'), get_value=l1y
      widget_control, widget_info(event.top, find_by_uname='l2x'), get_value=l2x
      widget_control, widget_info(event.top, find_by_uname='l2y'), get_value=l2y
      widget_control, widget_info(event.top, find_by_uname='l3x'), get_value=l3x
      widget_control, widget_info(event.top, find_by_uname='l3y'), get_value=l3y
      widget_control, widget_info(event.top, find_by_uname='angle'), get_value=angle
      widget_control, widget_info(event.top, find_by_uname='unit'), get_value=unit
      widget_control, widget_info(event.top, find_by_uname='TrackStart'), get_value=trackStart
      widget_control, widget_info(event.top, find_by_uname='TrackEnd'), get_value=trackEnd
      (*(*pState).pHBMPara).l1x = uint(l1x[0])
      (*(*pState).pHBMPara).l1y = uint(l1y[0])
      (*(*pState).pHBMPara).l2x = uint(l2x[0])
      (*(*pState).pHBMPara).l2y = uint(l2y[0])
      (*(*pState).pHBMPara).l3x = uint(l3x[0])
      (*(*pState).pHBMPara).l3y = uint(l3y[0])
      (*(*pState).pHBMPara).angle = uint(angle[0])
      (*(*pState).pHBMPara).unit = uint(unit[0])
      trackStart = uint(trackStart[0]) > 0
      trackEnd = uint(trackEnd[0]) < ((*pState).ts-1)
      if trackStart gt trackEnd then begin
        temp=trackStart & trackStart=trackEnd & trackEnd=temp
      endif
      (*(*pState).pHBMPara).trackStart = trackStart
      (*(*pState).pHBMPara).trackEnd = trackEnd
      start = 1
      widget_control, event.top, /destroy
    end
    Widget_Info(event.top, FIND_BY_UNAME='Cancel'): begin
      widget_control, event.top, /destroy
    end
    else:
  endcase

end
;-----------------------------------------------------------------
Function SetHBMPara, pState, refFrame
common SetHBMPara
COMPILE_OPT STRICTARR

  track_base = Widget_Base( GROUP_LEADER=(*pState).MainBase, UNAME='WID_BASE_0'  $
      ,XOFFSET=500,YOFFSET=500 ,SCR_XSIZE=240 ,SCR_YSIZE=290, tab_mode=1  $
      ,TITLE='Hierarchical Block Matching' ,SPACE=3 ,XPAD=3 ,YPAD=3 ,/MODAL)
  WID_BASE_1 = Widget_Base(track_base, UNAME='WID_BASE_1' ,XOFFSET=10  $
      ,YOFFSET=10 ,SCR_XSIZE=225 ,SCR_YSIZE=147 ,SPACE=3,XPAD=3 ,YPAD=3)
  WID_LABEL_0 = Widget_Label(WID_BASE_1, UNAME='WID_LABEL_0'  $
      ,XOFFSET=65 ,YOFFSET=10 ,/ALIGN_LEFT ,VALUE='Searching Region')
  WID_LABEL_1 = Widget_Label(WID_BASE_1, UNAME='WID_LABEL_1'  $
      ,XOFFSET=10 ,YOFFSET=35 ,/ALIGN_LEFT ,VALUE='Level')
  WID_LABEL_2 = Widget_Label(WID_BASE_1, UNAME='WID_LABEL_2'  $
      ,XOFFSET=18 ,YOFFSET=60 ,/ALIGN_CENTER ,VALUE='1')
  WID_LABEL_3 = Widget_Label(WID_BASE_1, UNAME='WID_LABEL_3'  $
      ,XOFFSET=18 ,YOFFSET=85 ,/ALIGN_CENTER ,VALUE='2')
  WID_LABEL_4 = Widget_Label(WID_BASE_1, UNAME='WID_LABEL_4'  $
      ,XOFFSET=18 ,YOFFSET=110 ,/ALIGN_CENTER ,VALUE='3')
  WID_LABEL_5 = Widget_Label(WID_BASE_1, UNAME='WID_LABEL_5'  $
      ,XOFFSET=60 ,YOFFSET=35 ,/ALIGN_CENTER ,VALUE='X')
  WID_LABEL_6 = Widget_Label(WID_BASE_1, UNAME='WID_LABEL_6'  $
      ,XOFFSET=90 ,YOFFSET=35 ,/ALIGN_CENTER ,VALUE='Y')


  l1x = Widget_Text(WID_BASE_1, UNAME='l1x' ,XOFFSET=55 ,YOFFSET=57  $
      ,/EDITABLE ,XSIZE=3 ,YSIZE=1, value=string((*(*pState).pHBMPara).l1x, format='(i0)'))
  l1y = Widget_Text(WID_BASE_1, UNAME='l1y' ,XOFFSET=85 ,YOFFSET=57  $
      ,/EDITABLE ,XSIZE=3 ,YSIZE=1, value=string((*(*pState).pHBMPara).l1y, format='(i0)'))
  l2x = Widget_Text(WID_BASE_1, UNAME='l2x' ,XOFFSET=55 ,YOFFSET=83  $
      ,/EDITABLE ,XSIZE=3 ,YSIZE=1, value=string((*(*pState).pHBMPara).l2x, format='(i0)'))
  l2y = Widget_Text(WID_BASE_1, UNAME='l2y' ,XOFFSET=85 ,YOFFSET=83  $
      ,/EDITABLE ,XSIZE=3 ,YSIZE=1, value=string((*(*pState).pHBMPara).l2y, format='(i0)'))
  l3x = Widget_Text(WID_BASE_1, UNAME='l3x' ,XOFFSET=55 ,YOFFSET=108  $
      ,/EDITABLE ,XSIZE=3 ,YSIZE=1, value=string((*(*pState).pHBMPara).l3x, format='(i0)'))
  l3y = Widget_Text(WID_BASE_1, UNAME='l3y' ,XOFFSET=85 ,YOFFSET=108  $
      ,/EDITABLE ,XSIZE=3 ,YSIZE=1, value=string((*(*pState).pHBMPara).l3y, format='(i0)'))
  angle = Widget_Text(WID_BASE_1, UNAME='angle' ,XOFFSET=170 ,YOFFSET=84 $
      ,XSIZE=3 ,YSIZE=1, value=string((*(*pState).pHBMPara).angle, format='(i0)'))
  unit = Widget_Text(WID_BASE_1, UNAME='unit' ,XOFFSET=170 ,YOFFSET=109 $
      ,XSIZE=3 ,YSIZE=1, value=string((*(*pState).pHBMPara).unit, format='(i0)'))

  WID_BASE_2 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_2'  $
      ,XOFFSET=130 ,YOFFSET=54 ,TITLE='IDL' ,COLUMN=1 ,/NONEXCLUSIVE)
  rotation = Widget_Button(WID_BASE_2, UNAME='Rotation' ,/ALIGN_LEFT  $
      ,VALUE='Rotation')
  WID_LABEL_7 = Widget_Label(WID_BASE_1, UNAME='WID_LABEL_7'  $
      ,XOFFSET=130 ,YOFFSET=86 ,/ALIGN_LEFT ,VALUE='Angle')
  WID_LABEL_8 = Widget_Label(WID_BASE_1, UNAME='WID_LABEL_8'  $
      ,XOFFSET=130 ,YOFFSET=111 ,/ALIGN_LEFT ,VALUE='Step')

  WID_LABEL_9 = Widget_Label(track_base, UNAME='WID_LABEL_9'  $
      ,XOFFSET=18 ,YOFFSET=160 ,/ALIGN_LEFT ,VALUE='Reference Frame')
  ReferenceFrame = Widget_Text(track_base, UNAME='ReferenceFrame'  $
      ,XOFFSET=110 ,YOFFSET=158 ,XSIZE=3 ,YSIZE=1, value=string(refFrame,format='(i0)'))
  WID_LABEL_10 = Widget_Label(track_base, UNAME='WID_LABEL_10'  $
      ,XOFFSET=18 ,YOFFSET=192 ,/ALIGN_LEFT ,VALUE='Track edge from')
  TrackStart = Widget_Text(track_base, UNAME='TrackStart' ,XOFFSET=110 ,YOFFSET=190 $
      ,/EDITABLE ,XSIZE=3 ,YSIZE=1, value=string((*(*pState).pHBMPara).trackStart,format='(i0)'))
  WID_LABEL_11 = Widget_Label(track_base, UNAME='WID_LABEL_11'  $
      ,XOFFSET=140 ,YOFFSET=192 ,/ALIGN_LEFT ,VALUE='to')
  TrackEnd = Widget_Text(track_base, UNAME='TrackEnd' ,XOFFSET=155 ,YOFFSET=190 $
      ,/EDITABLE ,XSIZE=3 ,YSIZE=1, value=string((*(*pState).pHBMPara).trackEnd,format='(i0)'))

  WID_BUTTON_0 = Widget_Button(track_base, UNAME='Reset'  $
      ,XOFFSET=15 ,YOFFSET=227 ,SCR_XSIZE=60 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='Reset')
  WID_BUTTON_1 = Widget_Button(track_base, UNAME='Start'  $
      ,XOFFSET=85 ,YOFFSET=227 ,SCR_XSIZE=60 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='Start')
  WID_BUTTON_2 = Widget_Button(track_base, UNAME='Cancel'  $
      ,XOFFSET=155 ,YOFFSET=227 ,SCR_XSIZE=60 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='Cancel')

  Widget_Control, /REALIZE, track_base
  widget_control,track_base, set_uvalue=pState
  if (*(*pState).pHBMPara).IsRotation eq 1 then widget_control, rotation, /set_button
  widget_control, wid_button_1, /input_focus
  start = 0
  XManager, 'SetHBMPara', track_base, event_handler='SetHBMPara_event', /NO_BLOCK
  return, start
end

pro OnTextureAlignment, Event
COMPILE_OPT STRICTARR
  widget_control, event.top, get_uvalue=pState
  widget_control, widget_info((*pState).AnimationBase, find_by_uname='FrameSlider'),get_value=refFrame
  para = { start: 0, $
    refFrame: refFrame, $
    r: 8, $
    fStart: 0, $
    fEnd: (*pState).ts-1 }
  para = SetTextureAlignmentPara(para,event.top)
  if para.start eq 0 then return
  img_ref = reform((*(*pState).img[0])[*,*,para.refFrame])
  timer = Obj_New("ShowProgress", (*pState).MainBase, Color='DDDD00'XL,title=' ',  $
      message='Performing texture alignment...', xsize=400, ysize=25)
  timer->start
  for k=para.fStart,para.fEnd do begin
    if k eq para.refFrame then continue
    if para.fStart ne para.fEnd then begin
      percent = (k-para.fStart)*100/(para.fEnd-para.fStart)
      timer->update, percent
    endif
    img_tar = reform((*(*pState).img[0])[*,*,k])
    for j=0,(*pState).ys-1 do begin
      line_ref = reform(img_ref[20:(*pState).xs-20,j])
      ref_nor = sqrt(total(line_ref^2,/double))
      vMot = indgen(2*para.r+1)-para.r
      vCor = fltarr(2*para.r+1)
      for i=0,2*para.r do begin
        line_tar = reform(img_tar[20+vMot[i]:(*pState).xs-20+vMot[i],j])
        tar_nor = sqrt(total(line_tar^2,/double))
        vCor[i] = total(line_ref*line_tar)/ref_nor/tar_nor
      endfor
      maxCor = max(vCor,pos)
      img_tar[*,j] = shift(img_tar[*,j],-vMot[pos])
      for i=0,(*pState).stackNum-1 do begin
        (*(*pState).img[i])[*,j,k] = shift((*(*pState).img[i])[*,j,k],-vMot[pos])
      endfor
    endfor
  endfor
  timer->destroy
  (*pState).oWindow->RemoveWindowEventObserver, (*pState).oObserver
  obj_destroy, (*pState).oObserver
  obj_destroy, (*pState).oAnimationModel
  CreatAnimationModel, pState
End

Pro SetTextureAlignmentPara_event, Event
common TextureAlignment, localPara

  case event.id of
    Widget_Info(event.top, FIND_BY_UNAME='Reset'): begin
      widget_control, widget_info(event.top, find_by_uname='ReferenceFrame'), $
          set_value=string(localPara.refFrame, format='(i0)')
      widget_control, widget_info(event.top, find_by_uname='Radius'), $
          set_value=string(localPara.r, format='(i0)')
      widget_control, widget_info(event.top, find_by_uname='TrackStart'), $
          set_value=string(localPara.fStart, format='(i0)')
      widget_control, widget_info(event.top, find_by_uname='TrackEnd'), $
          set_value=string(localPara.fEnd, format='(i0)')
    end
    Widget_Info(event.top, FIND_BY_UNAME='Start'): begin
      widget_control, widget_info(event.top, find_by_uname='ReferenceFrame'), get_value=refFrame
      widget_control, widget_info(event.top, find_by_uname='Radius'), get_value=r
      widget_control, widget_info(event.top, find_by_uname='TrackStart'), get_value=fStart
      widget_control, widget_info(event.top, find_by_uname='TrackEnd'), get_value=fEnd
      localPara.refFrame = uint(refFrame[0])
      localPara.r = uint(r[0])
      fStart = uint(fStart[0]) > 0
      fEnd = uint(fEnd[0])
      if fStart gt fEnd then begin
        temp=fStart & fStart=fEnd & fEnd=temp
      endif
      localPara.fStart = fStart
      localPara.fEnd = fEnd
      localPara.start = 1
      widget_control, event.top, /destroy
    end
    Widget_Info(event.top, FIND_BY_UNAME='Cancel'): begin
      widget_control, event.top, /destroy
    end
    else:
  endcase
End

Function SetTextureAlignmentPara, para,group_leader
common TextureAlignment
  
  WID_BASE_0 = Widget_Base( GROUP_LEADER=group_leader, UNAME='WID_BASE_0'  $
      ,XOFFSET=5 ,YOFFSET=5 ,SCR_XSIZE=233 ,SCR_YSIZE=147 ,TITLE='Set'+ $
      ' parameters' ,SPACE=3 ,XPAD=3 ,YPAD=3,/modal)  
  WID_LABEL_0 = Widget_Label(WID_BASE_0, UNAME='WID_LABEL_0'  $
      ,XOFFSET=20 ,YOFFSET=15 ,/ALIGN_LEFT ,VALUE='Reference Frame')  
  ReferenceFrame = Widget_Text(WID_BASE_0, UNAME='ReferenceFrame'  $
      ,XOFFSET=115 ,YOFFSET=13 ,XSIZE=3 ,YSIZE=1,/editable,value=strtrim(para.refFrame,1))  
  WID_LABEL_1 = Widget_Label(WID_BASE_0, UNAME='WID_LABEL_1'  $
      ,XOFFSET=20 ,YOFFSET=45 ,/ALIGN_LEFT ,VALUE='Search Radius')  
  Radius = Widget_Text(WID_BASE_0, UNAME='Radius' ,XOFFSET=115  $
      ,YOFFSET=42 ,XSIZE=3 ,YSIZE=1,/editable,value=strtrim(para.r,1))  
  WID_LABEL_2 = Widget_Label(WID_BASE_0, UNAME='WID_LABEL_2'  $
      ,XOFFSET=20 ,YOFFSET=75 ,/ALIGN_LEFT ,VALUE='Start from')  
  TrackStart = Widget_Text(WID_BASE_0, UNAME='TrackStart' ,XOFFSET=72  $
      ,YOFFSET=72 ,XSIZE=3 ,YSIZE=1,/editable,value=strtrim(para.fStart,1))  
  WID_LABEL_3 = Widget_Label(WID_BASE_0, UNAME='WID_LABEL_3'  $
      ,XOFFSET=100 ,YOFFSET=75 ,/ALIGN_LEFT ,VALUE='to')  
  TrackEnd = Widget_Text(WID_BASE_0, UNAME='TrackEnd' ,XOFFSET=115  $
      ,YOFFSET=72 ,XSIZE=3 ,YSIZE=1,/editable,value=strtrim(para.fEnd,1))  
  Reset = Widget_Button(WID_BASE_0, UNAME='Reset' ,XOFFSET=160  $
      ,YOFFSET=12 ,SCR_XSIZE=50 ,SCR_YSIZE=22 ,/ALIGN_CENTER  $
      ,VALUE='Reset')  
  Start = Widget_Button(WID_BASE_0, UNAME='Start' ,XOFFSET=160  $
      ,YOFFSET=42 ,SCR_XSIZE=50 ,SCR_YSIZE=22 ,/ALIGN_CENTER  $
      ,VALUE='Start')  
  Cancel = Widget_Button(WID_BASE_0, UNAME='Cancel' ,XOFFSET=160  $
      ,YOFFSET=72 ,SCR_XSIZE=50 ,SCR_YSIZE=22 ,/ALIGN_CENTER  $
      ,VALUE='Cancel')
  Widget_Control, /REALIZE, WID_BASE_0
  localPara = para
  XManager, 'SetTextureAlignmentPara', WID_BASE_0, /NO_BLOCK  
  return, localPara
end