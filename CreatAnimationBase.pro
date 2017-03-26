;------------------------------------------------------------------------------
pro roi__SetROI, pState, oROI
COMPILE_OPT STRICTARR

	oOldSelROI = (*pState).oSelROI
	; Reset formerly selected ROI's color.
	if (OBJ_VALID(oOldSelROI) ne 0) then $
		oOldSelROI->SetProperty, COLOR=(*pState).roi_rgb

	if (OBJ_VALID(oROI) ne 0) then begin
		; Set newly selected ROI's color.
		oROI->SetProperty, COLOR=(*pState).sel_rgb

		; Reshape the current selection visual, if any, to match the
			; selected region.
		roi__ReshapeSelectionVisual, pState, oROI

		oSelROI = (*pState).result->SearchROI(oROI, roiNo=roiNo)
		result = oROI->ComputeGeometry(centroid=center,area=area)
		info = (*pState).result->GetProperty(roiNo, olabel=oLabel, trace=trace)
		oLabel->SetProperty, locations=center[0:1]
		idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
		widget_control, idROISlider, set_value=roiNo+1
	endif else begin
		; Hide any selection visuals.
		if (OBJ_VALID((*pState).oSelVisual) ne 0) then $
			(*pState).oSelVisual->SetProperty, /HIDE
	endelse

	(*pState).oSelROI = oROI
END
;------------------------------------------------------------------------------

PRO roi__Done, pState, event
; finish creation
	init = *(*pState).init
	init.x = event.x & init.y = event.y & init.IsOption = 1b
	result = (*pState).currentROI->ComputeGeometry(centroid=center, area=area)
	init.center = center[0:1]
	node = obj_new('ChainNode')
	ROINum = (*pState).result->length()
	oLabel = obj_new('IDLgrText', string(ROINum+1,format='(i0)'), $
		font=(*pState).oROIFont, color=[255,255,0], locations=init.center)
	roiMask = (*pState).currentROI->ComputeMask(dimensions=[(*pState).xs,(*pState).ys])
	indices = where(roiMask gt 0)
	trace = GetTrace(pState, indices)
	PlotTrace, pState, trace[0],title=file_basename((*pState).file[0])+'  ROI '+strtrim(ROINum+1,1)
	area *= ((*pState).pxs)^2
	distance = ptr_valid((*pState).mask) ? DistToEdge(*(*pState).edge,center)*(*pState).pxs : 0
	node->SetProperty,oROI=(*pState).currentROI,olabel=oLabel,trace=trace,ROIPara=[area,distance],flashNum=1
	(*pState).oLabelModel->Add, oLabel
	info = (*pState).result->insert(node)
	widget_control, (*pState).ResultTable, get_value=table
	ROINoColumn = uint(reform(table[0,*]))
	lastRow = max(where(ROINoColumn ne 0))
	widget_control, (*pState).ResultTable, use_table_select=[0,lastrow+1,2,lastRow+1], $
	set_value=[string(ROINum+1,format='(i0)'),string([area,distance],format='(f0.2)')]
	idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
	widget_control,idROISlider,set_slider_min=1,set_slider_max=ROINum+1,set_value=ROINum+1,sensitive=1

END
PRO roi__linescan, pState, event
; finish creation
; finish creation
	init = *(*pState).init
	if keyword_set(event) then begin
	init.x = event.x & init.y = event.y & init.IsOption = 1b

	
	result = (*pState).currentROI->ComputeGeometry(centroid=center, area=area)
	init.center = center[0:1]
	node = obj_new('ChainNode')
	ROINum = (*pState).result->length()
	oLabel = obj_new('IDLgrText', string(ROINum+1,format='(i0)'), $
		font=(*pState).oROIFont, color=[255,255,0], locations=init.center)
	roiMask = (*pState).currentROI->ComputeMask(dimensions=[(*pState).xs,(*pState).ys])
	indices = where(roiMask gt 0)
	trace = GetTrace(pState, indices)
	PlotTrace, pState, trace[0],title=file_basename((*pState).file[0])+'  ROI '+strtrim(ROINum+1,1)
	area *= ((*pState).pxs)^2
	distance = ptr_valid((*pState).mask) ? DistToEdge(*(*pState).edge,center)*(*pState).pxs : 0
	node->SetProperty,oROI=(*pState).currentROI,olabel=oLabel,trace=trace,ROIPara=[area,distance],flashNum=1
	(*pState).oLabelModel->Add, oLabel
	info = (*pState).result->insert(node)
	widget_control, (*pState).ResultTable, get_value=table
	ROINoColumn = uint(reform(table[0,*]))
	lastRow = max(where(ROINoColumn ne 0))
	widget_control, (*pState).ResultTable, use_table_select=[0,lastrow+1,2,lastRow+1], $
	set_value=[string(ROINum+1,format='(i0)'),string([area,distance],format='(f0.2)')]
	idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
	widget_control,idROISlider,set_slider_min=1,set_slider_max=ROINum+1,set_value=ROINum+1,sensitive=1
endif
	(*pState).currentROI->getproperty,data=data 
	xys = double((*pstate).xs)*(*pstate).ys	
	wset, (*pState).TraceWindow
	n = n_elements(data[0,*])
	signal = fltarr((*pstate).ts,n)
    for ii=0,(*pState).ts-1 do begin
    	x = reform(data[0,*])
    	y = reform(data[1,*])
    	img = (*(*pstate).img[0])[*,*,ii]
      signal[ii,*] = reform(img[x,y])
    endfor
  signal = smooth(signal,3,edge = 1)
	show = congrid(signal,750,350,/interp)
	device,decomp = 0
	tvlct,oldt,/get
	loadct,33,/silent
	tv,show
	tvlct,oldt
	device,decomp = 1
END
;------------------------------------------------------------------------------
pro roi__ButtonPress, event
COMPILE_OPT STRICTARR

	WIDGET_CONTROL, event.top, GET_UVALUE=pState

	; Convert from viewport coordinates to image coordinates.

	case (*pState).mode of
		'TRANSLATE-SCALE': begin
			if (event.press eq 1) then begin  ; Left mouse button.
;				widget_control, event.id, draw_keyboard_events=1
				; Temporarily hide all regions for selection.
				(*pState).oAutoROIModel->SetProperty, /HIDE
				(*pState).oManualROIModel->SetProperty, /HIDE
				(*pState).oLinescan->SetProperty, /HIDE
				(*pState).oLabelModel->SetProperty, /HIDE
				(*pState).oAnimationModel[(*pState).currStack]->SetProperty, /HIDE

				; Check if a selection visual was hit.
				oSel = (*pState).oWindow->Select((*pState).oView, $
					[event.x, event.y])
				selType = SIZE(oSel, /TYPE)
				if (selType eq 11) then begin  ; Object reference?
					; A selection visual handle was hit.
					(*pState).oSelHandle = oSel[0]

					oSel[0]->GetProperty, NAME=handleName
					case handleName of
						'SCALE_LL': bSaveROIData = 1b
						'SCALE_LR': bSaveROIData = 1b
						'SCALE_UL': bSaveROIData = 1b
						'SCALE_UR': bSaveROIData = 1b
						else: bSaveROIData = 0b
					endcase
					if (bSaveROIData ne 0) then begin
						oROI = (*pState).oSelROI
						
						if (OBJ_VALID(oROI) ne 0) then begin

							oROI->GetProperty, DATA=roiData, $
								ROI_XRANGE=xrange, ROI_YRANGE=yrange
							; Translate to origin.
							roiData[0,*] = roiData[0,*] - xrange[0]
							roiData[1,*] = roiData[1,*] - yrange[0]
							(*pState).pSavedROIData = PTR_NEW(roiData)
							(*pState).savedROIXRange = xrange
							(*pState).savedROIYRange = yrange
						endif else $
							(*pState).pSavedROIData = PTR_NEW()
					endif
				endif else $
				(*pState).oSelHandle = OBJ_NEW()

				; Restore the regions.
				(*pState).oAutoROIModel->SetProperty, HIDE=0
				(*pState).oManualROIModel->SetProperty, HIDE=0
				(*pState).oLineScan->SetProperty, HIDE=0
				(*pState).oLabelModel->SetProperty, HIDE=0
				(*pState).oAnimationModel[(*pState).currStack]->SetProperty, HIDE=0

				; If a selection visual was not hit...
				if (OBJ_VALID((*pState).oSelHandle) eq 0) then begin

					; Temporarily hide the selection visual.
					(*pState).oTransScaleVisual->SetProperty, /HIDE
     				(*pState).oLabelModel->SetProperty, /HIDE
					(*pState).oAnimationModel[(*pState).currStack]->SetProperty, /HIDE

					; Check if a region was hit.
					oSel = (*pState).oWindow->Select((*pState).oView, $
						DIMENSIONS=[16,16], $
						[event.x, event.y])
					(*pState).oLabelModel->SetProperty, HIDE=0
					(*pState).oAnimationModel[(*pState).currStack]->SetProperty, HIDE=0

					selType = SIZE(oSel, /TYPE)
					if (selType eq 11) then begin  ; Object reference?
						oROI = oSel[0]
;==============================begin insert==============================				
				oROI->Getproperty,style = style
				if style eq 1 then begin
					 (*pState).currentROI = oROI
					 roi__linescan,pState
					 goto,out
				endif
;==============================end insert==============================
						; Mark the region as selected.
						roi__SetROI, pState, oROI
					endif else $
						; Mark no regions as being currently selected.
						roi__SetROI, pState, OBJ_NEW()
				endif
			
out:		(*pState).bButtonDown = 1b
				(*pState).buttonXY = [event.x, event.y]

				(*pState).oWindow->Draw, (*pState).oView
			endif
		end

		'RECTANGLE': begin
			if (event.press eq 1) then begin  ; Left mouse button.
			; Change the color of the previously selected ROI.
				oOldSelROI = (*pState).oSelROI
				if (OBJ_VALID(oOldSelROI) ne 0) then begin
					oOldSelROI->SetProperty, COLOR=(*pState).roi_rgb
			    endif
				; Create a new rectangle region.
				oROI = OBJ_NEW('IDLgrROI', $
					COLOR=(*pState).sel_rgb, $
					STYLE=0 $
					)
				(*pState).currentROI = oROI
				(*pState).oManualROIModel->Add, oROI

				; Set initial corner for the rectangle.
				oROI->AppendData, [event.x, event.y, 0]

				(*pState).oWindow->Draw, (*pState).oView

				(*pState).bButtonDown = 1b
				(*pState).buttonXY = [event.x, event.y]
			endif
		end

		'ELLIPSE': begin
			if (event.press eq 1) then begin  ; Left mouse button.
				; Change the color of the previously selected ROI.
				oOldSelROI = (*pState).oSelROI
				if (OBJ_VALID(oOldSelROI) ne 0) then $
					oOldSelROI->SetProperty, COLOR=(*pState).roi_rgb

				; Create a new ellipse region.
				oROI = OBJ_NEW('IDLgrROI', $
					COLOR=(*pState).sel_rgb, $
					STYLE=0 $
					)

				(*pState).CurrentROI = oROI
				(*pState).oManualROIModel->Add, oROI

				; Initialize the ellipse as a single point.
				oROI->AppendData, [event.x, event.y, 0]

				(*pState).oWindow->Draw, (*pState).oView

				(*pState).bButtonDown = 1b
				(*pState).buttonXY = [event.x, event.y]
			endif
		end
		
		'FREEHAND DRAW': begin
			if (event.press eq 1) then begin
				oROI = (*pState).CurrentROI
				if (OBJ_VALID(oROI) eq 0) then begin
					oOldSelROI = (*pState).oSelROI
					if (OBJ_VALID(oOldSelROI) ne 0) then $
						oOldSelROI->SetProperty, COLOR=(*pState).roi_rgb

					oROI = OBJ_NEW('IDLgrROI', $
						COLOR=(*pState).sel_rgb, $
						STYLE=1 $
						)

					(*pState).currentROI = oROI
					(*pState).oManualROIModel->Add, oROI
				endif

				oROI->AppendData, [event.x, event.y, 0]

				(*pState).oWindow->Draw, (*pState).oView
				(*pState).bButtonDown = event.clicks
			endif
		end
		
		; Segmented ROI
		'POLYGON DRAW': begin
			if (event.press eq 1) then begin
				oROI = (*pState).currentROI
				if (OBJ_VALID(oROI) eq 0) then begin
					oOldSelROI = (*pState).oSelROI
					if (OBJ_VALID(oOldSelROI) ne 0) then $
					oOldSelROI->SetProperty, COLOR=(*pState).roi_rgb

					oROI = OBJ_NEW('IDLgrROI', $
						COLOR=(*pState).sel_rgb, $
						STYLE=1 $
						)
					(*pState).CurrentROI = oROI
					(*pState).oManualROIModel->Add, oROI
					(*pState).oWindow->Draw, (*pState).oView
				endif

				if (event.clicks eq 1) then begin
					; If dragging a temporary segment, start a new one.
					; Otherwise, append a new vertex.
					if ((*pState).bTempSegment eq 1) then $
						(*pState).bTempSegment = 0 $
					else $
						oROI->AppendData, [event.x, event.y, 0]
				endif

				(*pState).bButtonDown = event.clicks
			endif
		end
;=======================================begin insert======================
		'CURVE DRAW': begin
			if (event.press eq 1) then begin
				oROI = (*pState).CurrentROI
				if (OBJ_VALID(oROI) eq 0) then begin
					oOldSelROI = (*pState).oSelROI
					if (OBJ_VALID(oOldSelROI) ne 0) then $
						oOldSelROI->SetProperty, COLOR=(*pState).roi_rgb

					oROI = OBJ_NEW('IDLgrROI', $
						COLOR=(*pState).sel_rgb, $
						STYLE=1 $
						)

					(*pState).currentROI = oROI
					(*pState).oLineScan->Add, oROI
				endif

				oROI->AppendData, [event.x, event.y, 0]

				(*pState).oWindow->Draw, (*pState).oView
				(*pState).bButtonDown = event.clicks
			endif
		end
		
		; Segmented ROI
		'LINE DRAW': begin
			if (event.press eq 1) then begin
				oROI = (*pState).currentROI
				if (OBJ_VALID(oROI) eq 0) then begin
					oOldSelROI = (*pState).oSelROI
					if (OBJ_VALID(oOldSelROI) ne 0) then $
					oOldSelROI->SetProperty, COLOR=(*pState).roi_rgb

					oROI = OBJ_NEW('IDLgrROI', $
						COLOR=(*pState).sel_rgb, $
						STYLE=1 $
						)
					(*pState).CurrentROI = oROI
					(*pState).oLineScan->Add, oROI
					(*pState).oWindow->Draw, (*pState).oView
				endif

				if (event.clicks eq 1) then begin
					; If dragging a temporary segment, start a new one.
					; Otherwise, append a new vertex.
					if ((*pState).bTempSegment eq 1) then $
						(*pState).bTempSegment = 0 $
					else $
						oROI->AppendData, [event.x, event.y, 0]
				endif

				(*pState).bButtonDown = event.clicks
			endif
		end
;=======================================end insert========================
		; Pick ROI.
		'SELECTION': begin
			(*pState).oAnimationModel[(*pState).currStack]->SetProperty, HIDE=1
			(*pState).oLabelModel->SetProperty, /HIDE
			oSel = (*pState).oWindow->Select((*pState).oView, $
				DIMENSIONS=[8,8], $
				[event.x, event.y])
			(*pState).oAnimationModel[(*pState).currStack]->SetProperty, HIDE=0
			(*pState).oLabelModel->SetProperty, HIDE=0
			selType = SIZE(oSel, /TYPE)
			if (selType eq 11) then begin  ; Object reference?
				oROI = oSel[0]
				roi__SetROI, pState, oROI
			endif
			(*pState).bButtonDown = 1b

		end
	endcase
end
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
pro roi__Motion, event
COMPILE_OPT STRICTARR

	WIDGET_CONTROL, event.top, GET_UVALUE=pState

;ON_ERROR, (*pState).debug ? 0 : 2
	idFrameSlider = widget_info((*pState).AnimationBase, find_by_uname='FrameSlider')
	WIDGET_CONTROL, idFrameSlider, GET_VALUE = t
	if event.x lt 0 or event.y lt 0 or event.x gt ((*pstate).xs-1) or event.y gt ((*pstate).ys-1) then begin
	zf = 'outside'
	endif else zf = STRING( (*(*pState).img[(*pState).currStack])[event.x, event.y, t], FORMAT='(i5)')
	value = $
		'x:' + STRING(event.x, FORMAT='(i5)') + '  ' + $
		'y:' + STRING(event.y, FORMAT='(i5)') + '  ' + $
		'F:' + zf

		append = ' '
		if OBJ_VALID((*pState).currentROI) then begin
			if (*pState).mode eq 'POLYGON DRAW' then begin
				append = '  Double-click to finish'
			endif
		endif
	value = value + append
	WIDGET_CONTROL, (*pState).wStatus, SET_VALUE=value

	case (*pState).mode of
		'TRANSLATE-SCALE': begin
			if ((*pState).bButtonDown ne 0) then begin
				oROI = (*pState).oSelROI

				; If this is the first motion event since the button press,
				; then temporarily disable the region statistics (until
				; the button release.
				if ((*pState).bButtonDown eq 1) then begin
					; Determine if ROI Info dialog is currently realized.

					(*pState).bButtonDown = 2
				endif

				; First check if the mouse down occurred within a
				; selection visual handle.
				oSelHandle = (*pState).oSelHandle
				if (OBJ_VALID(oSelHandle) ne 0) then begin
					oSelHandle->GetProperty, NAME=handleName

					if (handleName eq 'TRANSLATE') then begin
						; Mouse down occurred within translation box.
						roi__TranslateROI, pState, oROI, event.x, event.y
					endif else if (handleName eq 'ROTATE') then begin
						roi__RotateROI, pState, oROI, event.x, event.y
					endif else begin
						; Mouse down must have occurred in a scale handle.
						roi__ScaleROI, pState, oROI, event.x, event.y
					endelse

				endif else begin
					; Translate currently selected region, if any.
					roi__TranslateROI, pState, oROI, event.x, event.y
				endelse

				(*pState).oWindow->Draw, (*pState).oView

				; Store new button location.
				(*pState).buttonXY = [event.x,event.y]
			endif
		end

		'RECTANGLE': begin
			oROI = (*pState).currentROI
			if (OBJ_VALID(oROI) EQ 0) then return

			; If button down, reposition rectangle corner.
			if ((*pState).bButtonDown NE 0) then begin

				style_point = 0
				style_line = 1
				style_closed = 2

				x0 = (*pState).buttonXY[0]
				y0 = (*pState).buttonXY[1]
				x1 = event.x
				y1 = event.y
				if (x0 eq x1) then begin
					if (y0 eq y1) then begin
						newBox = [[x0,y0,0.0]]
						style = style_point
					endif else begin
						newBox = [[x0,y0,0.0], [x0,y1,0.0]]
						style = style_line
					endelse
				endif else if (y0 eq y1) then begin
					newBox = [[x0,y0,0.0], [x1,y0,0.0]]
					style = style_line
				endif else begin
					newBox = [[x0,y0,0.0],[x1,y0,0.0],[x1,y1,0.0],[x0,y1,0.0]]
					style = style_closed
				endelse

				oROI->GetProperty, N_VERTS=nVerts
				oROI->ReplaceData, newBox, START=0, FINISH=nVerts-1
				oROI->SetProperty, STYLE=style

				init = *(*pState).init
				init.x = event.x & init.y = event.y & init.IsOption = 1b
				result = OROI->ComputeGeometry(centroid=centroid)
				init.center = centroid[0:1]
				*(*pState).init = init

				(*pState).oWindow->Draw, (*pState).oView
			endif
		end

		'ELLIPSE': begin
			oROI = (*pState).currentROI
			if (OBJ_VALID(oROI) EQ 0) then return

			; If button down, reposition radii.
			if ((*pState).bButtonDown NE 0) then begin

				style_point = 0
				style_line = 1
				style_closed = 2

				x0 = (*pState).buttonXY[0]
				y0 = (*pState).buttonXY[1]
				x1 = event.x
				y1 = event.y

				if (x0 eq x1) then begin
					if (y0 eq y1) then begin
						newX = [x0]
						newY = [y0]
						newZ = [0.0]
						style = style_point
					endif else begin
						vertRad = (y1 gt y0) ? (y1-y0) : (y0-y1)
						newX = [x0,x0]
						newY = [y0-vertRad,y0+vertRad]
						newZ = [0.0,0.0]
						style = style_line
					endelse
				endif else if (y0 eq y1) then begin
					horizRad = (x1 gt x0) ? (x1-x0) : (x0-x1)
					newX = [x0-horizRad,x0+horizRad]
					newY = [y0,y0]
					newZ = [0.0,0.0]
					style = style_line
				endif else begin
					horizRad = (x1 gt x0) ? (x1-x0) : (x0-x1)
					vertRad = (y1 gt y0) ? (y1-y0) : (y0-y1)

					; Number of vertices is dependent upon the greater
					; of the two radii.
					nPts = (horizRad > vertRad) * 4
					a = FINDGEN(nPts) * ( (2 * !PI) / (nPts-1) )
					newX = COS(a) * horizRad + x0
					newY = SIN(a) * vertRad + y0
					newZ = REPLICATE(0.0, nPts)
					style = style_closed
				endelse

				oROI->GetProperty, N_VERTS=nVerts
				oROI->ReplaceData, newX, newY, newZ, START=0, FINISH=nVerts-1
				oROI->SetProperty, STYLE=style

				(*pState).oWindow->Draw, (*pState).oView
			endif
		end

		; Freehand ROI
		'FREEHAND DRAW': begin
			oROI = (*pState).currentROI
			if (OBJ_VALID(oROI) EQ 0) then return

			; If button down, append a vertex.
			if ((*pState).bButtonDown NE 0) then begin
				oROI->AppendData, [event.x, event.y]
				(*pState).oWindow->Draw, (*pState).oView
				(*pState).bTempSegment = 1b
			endif
		end

		; Segmented ROI
		'POLYGON DRAW': begin
			oROI = (*pState).currentROI
			if (OBJ_VALID(oROI) EQ 0) then return

			; Replace the final vertex with current mouse location.
			if ((*pState).bTempSegment eq 0) then begin
				oROI->AppendData, [event.x, event.y]
				(*pState).oWindow->Draw, (*pState).oView
				(*pState).bTempSegment = 1b
			endif else begin
				oROI->ReplaceData, [event.x, event.y]
				(*pState).oWindow->Draw, (*pState).oView
			endelse
		end
;============================begin insert=========================================			
		; Freehand ROI
		'CURVE DRAW': begin
			oROI = (*pState).currentROI
			if (OBJ_VALID(oROI) EQ 0) then return

			; If button down, append a vertex.
			if ((*pState).bButtonDown NE 0) then begin
				oROI->AppendData, [event.x, event.y]
				(*pState).oWindow->Draw, (*pState).oView
				(*pState).bTempSegment = 1b
			endif
		end

		; Segmented ROI
		'LINE DRAW': begin
			oROI = (*pState).currentROI
			if (OBJ_VALID(oROI) EQ 0) then return

			; Replace the final vertex with current mouse location.
			if ((*pState).bTempSegment eq 0) then begin
				oROI->AppendData, [event.x, event.y]
				(*pState).oWindow->Draw, (*pState).oView
				(*pState).bTempSegment = 1b
			endif else begin
				oROI->ReplaceData, [event.x, event.y]
				(*pState).oWindow->Draw, (*pState).oView
			endelse
		end
;============================end insert=========================================		
		; Pick ROI/Vertex.
		'SELECTION': begin
			if ((*pState).bButtonDown ne 0) then begin
				; Pick nearest vertex in ROI.
				oROI = (*pState).oSelROI

				if not OBJ_VALID(oROI) then $
					RETURN

;				vertIndx = oROI->PickVertex((*pState).oWindow, $
;					(*pState).oView, $
;					[event.x, event.y])
;				if (vertIndx ge 0) then begin
;					; Show the pick vertex.
;					oPickPolyline = (*pState).oPickVisual->Get()
;					oPickPolyline->SetProperty, HIDE=0
;
;					(*pState).oPickVisual->Reset
;					oROI->GetProperty, DATA=vertData
;					selVert = vertData[*,vertIndx]
;					(*pState).oPickVisual->Translate, $
;						selVert[0], selVert[1], selVert[2]
					init = *(*pState).init
					init.x = event.x & init.y = event.y & init.IsOption = 1b
					result = (*pState).oSelROI->ComputeGeometry(centroid=centroid)
					init.center = centroid[0:1]
					*(*pState).init = init
					(*pState).oWindow->Draw, (*pState).oView
;				endif
			endif
		end
	endcase

end
;------------------------------------------------------------------------------
pro roi__ButtonRelease, event
COMPILE_OPT STRICTARR

	WIDGET_CONTROL, event.top, GET_UVALUE=pState

;	; Convert from viewport coordinates to image coordinates.
;	WIDGET_CONTROL, (*pState).wDraw, GET_DRAW_VIEW=viewport
;	xImage = sEvent.x + viewport[0]
;	yImage = sEvent.y + viewport[1]


	case (*pState).mode of
		'TRANSLATE-SCALE': begin
			if (event.release ne 1) then break  ; Left mouse button.

			; Restore region statistics.

			; Free any pointers to saved ROI data.
			if (PTR_VALID((*pState).pSavedROIData) ne 0) then $
				PTR_FREE, (*pState).pSavedROIData
			(*pState).pSavedROIData = PTR_NEW()

			; Restore button state.
			(*pState).bButtonDown = 0b
			(*pState).oWindow->SetCurrentCursor, 'CROSSHAIR'
;			widget_control, event.id, draw_keyboard_events=0


			oSelROI = (*pState).result->SearchROI((*pState).oSelROI, roiNo=roiNo)
			if not obj_valid(oSelROI) then return
			result = (*pState).oSelROI->ComputeGeometry(centroid=center,area=area)
			info = (*pState).result->GetProperty(roiNo, olabel=oLabel, trace=trace)
			oLabel->SetProperty, locations=center[0:1]
			idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
			widget_control, idROISlider, set_value=roiNo+1

				roiMask = (*pState).oSelROI->ComputeMask(dimensions=[(*pState).xs,(*pState).ys])
				indices = where(roiMask gt 0)
				trace = GetTrace(pState, indices)
;				result = (*pState).oSelROI->ComputeGeometry(centroid=center, area=area)
				area *= ((*pState).pxs)^2
				distance = ptr_valid((*pState).mask) ? DistToEdge(*(*pState).edge,center)*(*pState).pxs : 0
				info = (*pState).result->SetProperty(roiNo, trace=trace, ROIPara=[area,distance])
				widget_control, (*pState).ResultTable, get_value=table
				ROINoColumn = uint(reform(table[0,*]))
				row = where(ROINoColumn eq ROINo+1,n_row)
				row = row[0]
				if row ne -1 then widget_control, (*pState).ResultTable, use_table_select=[1,row,2,row+n_row-1], $
					set_value=string(rebin([area,distance],2,n_row),format='(f0.2)')

			(*pState).oWindow->SetCurrentCursor, 'CROSSHAIR'
			oROI = (*pState).currentROI
			if obj_valid(oROI) then begin
			oROI->Getproperty,style = style
			endif else style = 0
			(*pState).currentROI = OBJ_NEW()
			if style ne 1 then begin
			wset, (*pState).TraceWindow
			PlotTrace, pState, trace[0],title=file_basename((*pState).file[0])+'  ROI '+strtrim(ROINo+1,1)
			BaselineSubtraction, trace[0]
			endif
		end

		'RECTANGLE': begin
			if (event.release ne 1) then break  ; Left mouse button.
			if ((*pState).bButtonDown ne 1) then break  ; button was down

			; Reset button down state.
			(*pState).bButtonDown = 0b

			oROI = (*pState).currentROI
			if (not OBJ_VALID(oROI)) then break

			; Ensure that the rectangle has at 4 vertices.
			oROI->GetProperty, DATA=roiData
			if ((N_ELEMENTS(roiData)/3) eq 4) then begin
				; The rectangle region is valid.  Give it a name
				roi__Done, pState, event
				(*pState).currentROI = OBJ_NEW()

				; Set the region as current.
				roi__SetROI, pState, oROI

				; If this is the first region, bring up the
				; region information dialog.
				if ((*pState).bFirstROI eq 1b) then begin
					WIDGET_CONTROL, /HOURGLASS
					(*pState).bFirstROI = 0b
;					xroiInfo, pState, GROUP_LEADER=sEvent.top
				endif
				(*pState).oWindow->Draw, (*pState).oView
			endif else begin
				; Fewer than 4 vertices; delete.
				(*pState).oManualROIModel->Remove, oROI
				OBJ_DESTROY, oROI
				(*pState).currentROI = OBJ_NEW()

				; Reset color of formerly selected ROI.
				oOldSelROI = (*pState).oSelROI
				if (OBJ_VALID(oOldSelROI) ne 0) then $
					oOldSelROI->SetProperty, $
						COLOR=(*pState).sel_rgb

				(*pState).oWindow->Draw, (*pState).oView
			endelse

		end

		'ELLIPSE': begin
			if (event.release ne 1) then break  ; Left mouse button only.
			if ((*pState).bButtonDown ne 1) then break  ; button was down

			; Reset button down state.
			(*pState).bButtonDown = 0b

			oROI = (*pState).currentROI
			if (not OBJ_VALID(oROI)) then break

			; Ensure that the ellipse has at least 4 vertices.
			oROI->GetProperty, DATA=roiData
			if ((N_ELEMENTS(roiData)/3) ge 4) then begin
				; The ellipse region is valid.  Give it a name
				; and add it to the appropriate containers.
				roi__Done, pState, event
				(*pState).currentROI = OBJ_NEW()


				; Set the region as current.
				roi__SetROI, pState, oROI

				; If this is the first region, bring up the
				; region information dialog.
				if ((*pState).bFirstROI eq 1b) then begin
					WIDGET_CONTROL, /HOURGLASS
					(*pState).bFirstROI = 0b
				endif
			endif else begin
				; Fewer than 4 vertices; delete.
				(*pState).oManualROIModel->Remove, oROI
				OBJ_DESTROY, oROI
				(*pState).currentROI = OBJ_NEW()

				; Reset color of formerly selected ROI.
				oOldSelROI = (*pState).oSelROI
				if (OBJ_VALID(oOldSelROI) ne 0) then $
					oOldSelROI->SetProperty, $
					COLOR=(*pState).sel_rgb

				(*pState).oWindow->Draw, (*pState).oView
			endelse
		end

		; Freehand ROI
		'FREEHAND DRAW': begin
			if (event.release ne 1) then break
			if ((*pState).bButtonDown ne 1) then break  ; Left mouse button.

			; Reset button down state.
			(*pState).bButtonDown = 0b
			(*pState).bTempSegment = 0b

			; End ROI
			oROI = (*pState).currentROI
			if (not OBJ_VALID(oROI)) then break

			; Ensure that the region has at 3 vertices.
			oROI->GetProperty, DATA=roiData
			if ((N_ELEMENTS(roiData)/3) ge 3) then begin

				roi__Done, pState, event
				(*pState).currentROI = OBJ_NEW()

				; Set the region as current.
				roi__SetROI, pState, oROI

				oROI->SetProperty, STYLE=2
				(*pState).oWindow->Draw, (*pState).oView

				; If this is the first region, bring up the
				; region information dialog.
				if ((*pState).bFirstROI eq 1b) then begin
					WIDGET_CONTROL, /HOURGLASS
					(*pState).bFirstROI = 0b
				endif

			endif else begin
				; Fewer than 3 vertices; delete.
				(*pState).oManualROIModel->Remove, oROI
				OBJ_DESTROY, oROI
				(*pState).currentROI = OBJ_NEW()

				; Reset color of formerly selected ROI.
				oOldSelROI = (*pState).oSelROI
				if (OBJ_VALID(oOldSelROI) ne 0) then $
					oOldSelROI->SetProperty, $
					COLOR=(*pState).sel_rgb

				(*pState).oWindow->Draw, (*pState).oView

			endelse
		end  ; FREEHAND DRAW

		; Segmented ROI
		'POLYGON DRAW': begin
			; Double-click or right mouse-button up.
			if not ((event.release eq 1 and (*pState).bButtonDown eq 2) or $
				(event.release eq 4)) then break

			; Reset button down state.
			(*pState).bButtonDown = 0b

			oROI = (*pState).currentROI
			if (not OBJ_VALID(oROI)) then break

			value = $
				'x:' + STRING(event.x, FORMAT='(i5)') + '  ' + $
				'y:' + STRING(event.y, FORMAT='(i5)')


			WIDGET_CONTROL, (*pState).wStatus, SET_VALUE=value

			; Ensure that the region has at 3 vertices.
			oROI->GetProperty, DATA=roiData
			if ((N_ELEMENTS(roiData)/3) ge 3) then begin
				; The region is valid.  Give it a name and add it
				; to the appropriate containers.
				roi__Done, pState, event
				(*pState).currentROI = OBJ_NEW()

				; Set the region as current.
				roi__SetROI, pState, oROI

				oROI->SetProperty, STYLE=2
				(*pState).oWindow->Draw, (*pState).oView

				; If this is the first region, bring up the
				; region information dialog.
				if ((*pState).bFirstROI eq 1b) then begin
					WIDGET_CONTROL, /HOURGLASS
					(*pState).bFirstROI = 0b
				endif

				; Reset state.
				(*pState).bTempSegment = 0b

			endif else begin
				; Fewer than 3 vertices; delete.
				(*pState).oManualROIModel->Remove, oROI
				OBJ_DESTROY, oROI
				(*pState).currentROI = OBJ_NEW()

				; Reset color of formerly selected ROI.
				oOldSelROI = (*pState).oSelROI
				if (OBJ_VALID(oOldSelROI) ne 0) then $
					oOldSelROI->SetProperty, COLOR=(*pState).sel_rgb
				(*pState).oWindow->Draw, (*pState).oView

				; Reset state.
				(*pState).bTempSegment = 0b
			endelse

			; This is a special case. For a segmented polygon, we don't want
			; to check for the right mouse button below because this will
			; pop up the context menu. So just bail out here.
			return
		end   ; POLYGON DRAW
;==============================================begin insert=================================
		; Freehand ROI
		'CURVE DRAW': begin
			if (event.release ne 1) then break
			if ((*pState).bButtonDown ne 1) then break  ; Left mouse button.

			; Reset button down state.
			(*pState).bButtonDown = 0b
			(*pState).bTempSegment = 0b

			; End ROI
			oROI = (*pState).currentROI
			if (not OBJ_VALID(oROI)) then break

			; Ensure that the region has at 3 vertices.
			oROI->GetProperty, DATA=roiData
			linkup,roiData,data,th = max([(*pstate).xs,(*pstate).ys])
			oROI->SetProperty, DATA=data
			if ((N_ELEMENTS(roiData)/3) ge 3) then begin

				roi__linescan, pState, event
				(*pState).currentROI = OBJ_NEW()

				; Set the region as current.
				roi__SetROI, pState, oROI

				(*pState).oWindow->Draw, (*pState).oView

				; If this is the first region, bring up the
				; region information dialog.
				if ((*pState).bFirstROI eq 1b) then begin
					WIDGET_CONTROL, /HOURGLASS
					(*pState).bFirstROI = 0b
				endif

			endif else begin
				; Fewer than 3 vertices; delete.
				(*pState).oLineScan->Remove, oROI
				OBJ_DESTROY, oROI
				(*pState).currentROI = OBJ_NEW()

				; Reset color of formerly selected ROI.
				oOldSelROI = (*pState).oSelROI
				if (OBJ_VALID(oOldSelROI) ne 0) then $
					oOldSelROI->SetProperty, $
					COLOR=(*pState).sel_rgb

				(*pState).oWindow->Draw, (*pState).oView

			endelse
		end  ; FREEHAND DRAW

		; Segmented ROI
		'LINE DRAW': begin
			; Double-click or right mouse-button up.
			if not ((event.release eq 1 and (*pState).bButtonDown eq 2) or $
				(event.release eq 4)) then break

			; Reset button down state.
			(*pState).bButtonDown = 0b

			oROI = (*pState).currentROI
			if (not OBJ_VALID(oROI)) then break

			value = $
				'x:' + STRING(event.x, FORMAT='(i5)') + '  ' + $
				'y:' + STRING(event.y, FORMAT='(i5)')


			WIDGET_CONTROL, (*pState).wStatus, SET_VALUE=value

			; Ensure that the region has at 3 vertices.
			oROI->GetProperty, DATA=roiData
			linkup,roiData,data,th = max([(*pstate).xs,(*pstate).ys])
			oROI->SetProperty, DATA=data
			if ((N_ELEMENTS(roiData)/3) ge 2) then begin
				; The region is valid.  Give it a name and add it
				; to the appropriate containers.
				roi__linescan, pState, event
				(*pState).currentROI = OBJ_NEW()

				; Set the region as current.
				roi__SetROI, pState, oROI

				(*pState).oWindow->Draw, (*pState).oView

				; If this is the first region, bring up the
				; region information dialog.
				if ((*pState).bFirstROI eq 1b) then begin
					WIDGET_CONTROL, /HOURGLASS
					(*pState).bFirstROI = 0b
				endif

				; Reset state.
				(*pState).bTempSegment = 0b

			endif else begin
				; Fewer than 3 vertices; delete.
				(*pState).oLineScan->Remove, oROI
				OBJ_DESTROY, oROI
				(*pState).currentROI = OBJ_NEW()

				; Reset color of formerly selected ROI.
				oOldSelROI = (*pState).oSelROI
				if (OBJ_VALID(oOldSelROI) ne 0) then $
					oOldSelROI->SetProperty, COLOR=(*pState).sel_rgb
				(*pState).oWindow->Draw, (*pState).oView

				; Reset state.
				(*pState).bTempSegment = 0b
			endelse

			; This is a special case. For a segmented polygon, we don't want
			; to check for the right mouse button below because this will
			; pop up the context menu. So just bail out here.
			return
		end   ; POLYGON DRAW
;==============================================end insert===================================
		; Pick ROI.
		'SELECTION': begin
			(*pState).bButtonDown = 0b

			oSelROI = (*pState).result->SearchROI((*pState).oSelROI, roiNo=roiNo)
			result = (*pState).oSelROI->ComputeGeometry(centroid=center,area=area)
			info = (*pState).result->GetProperty(roiNo, olabel=oLabel, trace=trace)
			oLabel->SetProperty, locations=center[0:1]
			idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
			widget_control, idROISlider, set_value=roiNo+1
			init = *(*pState).init
			if init.IsOption eq 1b then begin
				roiMask = (*pState).oSelROI->ComputeMask(dimensions=[(*pState).xs,(*pState).ys])
				indices = where(roiMask gt 0)
				trace = GetTrace(pState, indices)
				result = (*pState).oSelROI->ComputeGeometry(centroid=center, area=area)
				area *= ((*pState).pxs)^2
				distance = ptr_valid((*pState).mask) ? DistToEdge(*(*pState).edge,center)*(*pState).pxs : 0
				info = (*pState).result->SetProperty(roiNo, trace=trace, ROIPara=[area,distance])
				widget_control, (*pState).ResultTable, get_value=table
				ROINoColumn = uint(reform(table[0,*]))
				row = where(ROINoColumn eq ROINo+1,n_row)
				row = row[0]
				if row ne -1 then widget_control, (*pState).ResultTable, use_table_select=[1,row,2,row+n_row-1], $
					set_value=string(rebin([area,distance],2,n_row),format='(f0.2)')
			endif
			(*pState).oWindow->SetCurrentCursor, 'CROSSHAIR'
			wset, (*pState).TraceWindow
			PlotTrace, pState, trace[0],title=file_basename((*pState).file[0])+'  ROI '+strtrim(ROINo+1,1)
			BaselineSubtraction, trace[0]

		end
	endcase


	; On a right mouse up, perform a selection, then display the
	; appropriate context menu.
	if (event.release eq 4) then begin
		; Temporarily hide the image for selection.
		(*pState).oAnimationModel[(*pState).currstack]->SetProperty, HIDE=1
		(*pState).oLabelModel->SetProperty, HIDE=1
		; Temporarily hide the selection visual.
		if (OBJ_VALID((*pState).oSelVisual) ne 0) then begin
			(*pState).oSelVisual->GetProperty, HIDE=oldHide
			(*pState).oSelVisual->SetProperty, /HIDE
		endif

		; Check if a region was hit.
		oSel = (*pState).oWindow->Select((*pState).oView, $
		     DIMENSIONS=[16,16], $
		     [event.x, event.y])
		(*pState).oLabelModel->SetProperty, HIDE=0
		(*pState).oAnimationModel[(*pState).currstack]->SetProperty, HIDE=0
		; Restore the selection visual.
		if (OBJ_VALID((*pState).oSelVisual) ne 0) then $
			(*pState).oSelVisual->SetProperty, HIDE=oldHide

		selType = SIZE(oSel, /TYPE)
		if (selType eq 11) then begin  ; Object reference?
			oROI = oSel[0]

			; Mark the region as selected.
			roi__SetROI, pState, oROI

		endif else $
			roi__SetROI, pState, OBJ_NEW()




;		if (OBJ_VALID((*pState).oSelROI) ne 0) then $
;;			WIDGET_DISPLAYCONTEXTMENU, event.id, event.x, event.y, $
;;			(*pState).wROIContextMenu
;			WIDGET_DISPLAYCONTEXTMENU, (*pState).AnimationBase, $
;				event.x+15, (*pState).ys-event.y+43, (*pState).ContextBase1
	endif
	(*pState).oWindow->Draw, (*pState).oView
end
;------------------------------------------------------------------------------
pro roi__ScaleROI, pState, oROI, sEventX, sEventY

    COMPILE_OPT STRICTARR

    ; If the region is not valid, nothing to do.
    if (OBJ_VALID(oROI) eq 0) then $
        RETURN

	(*pState).oWindow->SetCurrentCursor, Image = (*pState).cursor.image[2,*], $
		Mask = (*pState).cursor.mask[2,*], Hotspot = (*pState).cursor.hotSpot[2,*]

    ; Retrieve original bounding box corners.
    (*pState).oTransScaleVisual->GetProperty, UVALUE=pTSState
    (*pTSState).oScaleLLModel->GetProperty, UVALUE=xyLL
    (*pTSState).oScaleURModel->GetProperty, UVALUE=xyUR
    x0 = xyLL[0]
    x1 = xyUR[0]
    y0 = xyLL[1]
    y1 = xyUR[1]

    ; Compute deltas relative to previous mouse location.
    dx = sEventX - (*pState).buttonXY[0]
    dy = sEventY - (*pState).buttonXY[1]

    ; Compute new bounding box corners.
    (*pState).oSelHandle->GetProperty, NAME=handleName
    case handleName of
        'SCALE_LL': begin
            newx0 = x0 + dx
            newy0 = y0 + dy

            bSwapX = (newx0 gt x1) ? 1b : 0b
            bSwapY = (newy0 gt y1) ? 1b : 0b

            if (bSwapX) then begin
                newx0 = x1
                newx1 = newx0
                if (bSwapY) then $
                   (*pState).oSelHandle = (*pTSState).oScaleURModel $
                else $
                   (*pState).oSelHandle = (*pTSState).oScaleLRModel
            endif else $
                newx1 = x1

            if (bSwapY) then begin
                newy0 = y1
                newy1 = newy0
                if (not bSwapX) then $
                   (*pState).oSelHandle = (*pTSState).oScaleULModel
            endif else $
                newy1 = y1
        end

        'SCALE_LR': begin
            newx1 = x1 + dx
            newy0 = y0 + dy

            bSwapX = (x0 gt newx1) ? 1b : 0b
            bSwapY = (newy0 gt y1) ? 1b : 0b

            if (bSwapX) then begin
                newx0 = newx1
                newx1 = x0
                if (bSwapY) then $
                    (*pState).oSelHandle = (*pTSState).oScaleULModel $
                else $
                    (*pState).oSelHandle = (*pTSState).oScaleLLModel
            endif else $
                newx0 = x0

            if (bSwapY) then begin
                newy1 = newy0
                newy0 = y1
                if (not bSwapX) then $
                    (*pState).oSelHandle = (*pTSState).oScaleURModel
            endif else $
                newy1 = y1
        end

        'SCALE_UL': begin
            newx0 = x0 + dx
            newy1 = y1 + dy

            bSwapX = (newx0 gt x1) ? 1b : 0b
            bSwapY = (y0 gt newy1) ? 1b : 0b

            if (bSwapX) then begin
                newx1 = newx0
                newx0 = x1
                if (bSwapY) then $
                    (*pState).oSelHandle = (*pTSState).oScaleLRModel $
                else $
                    (*pState).oSelHandle = (*pTSState).oScaleURModel
            endif else $
                newx1 = x1

            if (bSwapY) then begin
                newy0 = newy1
                newy1 = y0
                if (not bSwapX) then $
                    (*pState).oSelHandle = (*pTSState).oScaleLLModel
            endif else $
                newy0 = y0
        end

        'SCALE_UR': begin
            newx1 = x1 + dx
            newy1 = y1 + dy

            bSwapX = (x0 gt newx1) ? 1b : 0b
            bSwapY = (y0 gt newy1) ? 1b : 0b

            if (bSwapX) then begin
                newx0 = newx1
                newx1 = x0
                if (bSwapY) then $
                    (*pState).oSelHandle = (*pTSState).oScaleLLModel $
                else $
                    (*pState).oSelHandle = (*pTSState).oScaleULModel
            endif else $
                newx0 = x0

            if (bSwapY) then begin
                newy0 = newy1
                newy1 = y0
                if (not bSwapX) then $
                    (*pState).oSelHandle = (*pTSState).oScaleLRModel
            endif else $
                newy0 = y0
        end

        else: begin
            newx0 = x0
            newx1 = x1
            newy0 = y0
            newy1 = y1
            bSwapX = 0b
            bSwapY = 0b
        end
    endcase

   ; Compute scale factors relative to original ranges.
   origXRange = (*pState).savedROIXRange
   origYRange = (*pState).savedROIYRange
   sx = float(newx1-newx0+1) / float(origXRange[1]-origXRange[0]+1)
   sy = float(newy1-newy0+1) / float(origYRange[1]-origYRange[0]+1)

   ; Swap the original data as needed.
   newROIData = *(*pState).pSavedROIData
   if (bSwapX) then begin
       newROIData[0,*] = (origXRange[1]-origXRange[0]) - newROIData[0,*]
   endif
   if (bSwapY) then $
       newROIData[1,*] = (origYRange[1]-origYRange[0]) - newROIData[1,*]
   if (bSwapX or bSwapY) then $
       *(*pState).pSavedROIData = newROIData

   ; Scale the original data, and store result.
   newROIData[0,*] = newROIData[0,*] * sx + newx0
   newROIData[1,*] = newROIData[1,*] * sy + newy0
   oROI->ReplaceData, newROIData
	idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
	widget_control, idROISlider, get_value=ROINo
	info = (*pState).result->GetProperty(ROINo-1,oLabel = oLabel)
	result = oROI->ComputeGeometry(centroid=center)
	oLabel->SetProperty, locations=center[0:1]

   ; Translate the scale selection visual handles.
   (*pTSState).oScaleLLModel->Reset
   (*pTSState).oScaleLLModel->Translate, newx0, newy0, 0
   (*pTSState).oScaleLRModel->Reset
   (*pTSState).oScaleLRModel->Translate, newx1, newy0, 0
   (*pTSState).oScaleULModel->Reset
   (*pTSState).oScaleULModel->Translate, newx0, newy1, 0
   (*pTSState).oScaleURModel->Reset
   (*pTSState).oScaleURModel->Translate, newx1, newy1, 0

	dx = (newx1 - newx0) / 3 & dy = (newy1 - newy0) / 3
   ; Translate the rotate selection visual handles.
	(*pTSState).oRotateBModel->Reset
	(*pTSState).oRotateBModel->Translate, newx0+dx, newy0, 0
	(*pTSState).oRotateLModel->Reset
	(*pTSState).oRotateLModel->Translate, newx0, newy1-dy, 0
	(*pTSState).oRotateUModel->Reset
	(*pTSState).oRotateUModel->Translate, newx1-dx, newy1, 0
	(*pTSState).oRotateRModel->Reset
	(*pTSState).oRotateRModel->Translate, newx1, newy0+dy, 0

   ; Keep track of the new bounding box corners.
   (*pTSState).oScaleLLModel->SetProperty, UVALUE=[newx0,newy0]
   (*pTSState).oScaleURModel->SetProperty, UVALUE=[newx1,newy1]

   ; Reset the translation box (selection visual).
   newBox = [[newx0,newy0],[newx1,newy0],[newx1,newy1],[newx0,newy1]]
   (*pTSState).oTransModel->Reset
   (*pTSState).oTransBoxOutline->SetProperty, DATA=newBox
end
;------------------------------------------------------------------------------
pro roi__RotateROI, pState, oROI, sEventX, sEventY

COMPILE_OPT STRICTARR

    if (OBJ_VALID(oROI) eq 0) then $
        RETURN
	idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
	widget_control, idROISlider, get_value=ROINo
	info = (*pState).result->GetProperty(ROINo-1,oLabel = oLabel)
	result = oROI->ComputeGeometry(centroid=center)


	(*pState).oWindow->SetCurrentCursor, Image = (*pState).cursor.image[1,*], $
		Mask = (*pState).cursor.mask[1,*], Hotspot = (*pState).cursor.hotSpot[1,*]

    ; Compute deltas relative to previous mouse location.
	v0 = center[0:1] - [(*pState).buttonXY[0],(*pState).buttonXY[1]]
	d0 = sqrt(total(v0^2))
	v = center[0:1] - [sEventx,sEventy]
	d = sqrt(total(v^2))
	cosTheta = total(v0 * v) / d0 / d
	direction = OuterProduct([v0,0],[v,0])
	theta = (direction[2] ge 0) ? acos(cosTheta)*180/!DPI : -acos(cosTheta)*180/!DPI ;& print,theta
	oROI->Rotate, [0,0,1], theta, center=center[0:1]


    ; Translate the region.    oROI->Translate, dx, dy

	oLabel->SetProperty, locations=center[0:1]

	oROI->GetProperty, ROI_XRANGE=xrange, ROI_YRANGE=yrange
	x0 = MIN(xrange, MAX=x1)
	y0 = MIN(yrange, MAX=y1)
	newBox = [[x0,y0,0.0],[x1,y0,0.0],[x1,y1,0.0],[x0,y1,0.0]]


	(*pState).oTransScaleVisual->GetProperty, UVALUE=pTSState
	; Move translate/scale selection visual along with the region.
	(*pTSState).oTransModel->Reset
	(*pTSState).oTransBoxOutline->SetProperty, DATA=newBox

	; Translate the rotate selection visual handles.
	dx = (x1 - x0) / 3 & dy = (y1 - y0) / 3
	(*pTSState).oRotateBModel->Reset
	(*pTSState).oRotateBModel->Translate, x0+dx, y0, 0
	(*pTSState).oRotateLModel->Reset
	(*pTSState).oRotateLModel->Translate, x0, y1-dy, 0
	(*pTSState).oRotateUModel->Reset
	(*pTSState).oRotateUModel->Translate, x1-dx, y1, 0
	(*pTSState).oRotateRModel->Reset
	(*pTSState).oRotateRModel->Translate, x1, y0+dy, 0

	; Translate the scale selection visual handles.
	(*pTSState).oScaleLLModel->Reset
	(*pTSState).oScaleLLModel->Translate, x0, y0, 0
	(*pTSState).oScaleLRModel->Reset
	(*pTSState).oScaleLRModel->Translate, x1, y0, 0
	(*pTSState).oScaleULModel->Reset
	(*pTSState).oScaleULModel->Translate, x0, y1, 0
	(*pTSState).oScaleURModel->Reset
	(*pTSState).oScaleURModel->Translate, x1, y1, 0
	(*pTSState).oScaleLLModel->SetProperty, UVALUE=[x0,y0]
	(*pTSState).oScaleURModel->SetProperty, UVALUE=[x1,y1]


    ; Update bounding box corners.
    (*pTSState).oScaleLLModel->SetProperty, UVALUE=[x0,y0]
    (*pTSState).oScaleURModel->SetProperty, UVALUE=[x1,y1]
end

;------------------------------------------------------------------------------
pro roi__TranslateROI, pState, oROI, sEventX, sEventY

COMPILE_OPT STRICTARR

    ; If the region is not valid, nothing to do.
    if (OBJ_VALID(oROI) eq 0) then $
        RETURN

	(*pState).oWindow->SetCurrentCursor, Image = (*pState).cursor.image[0,*], $
		Mask = (*pState).cursor.mask[0,*], Hotspot = (*pState).cursor.hotSpot[0,*]

    ; Compute deltas relative to previous mouse location.
    dx = sEventX - (*pState).buttonXY[0]
    dy = sEventY - (*pState).buttonXY[1]

    ; Translate the region.
    oROI->Translate, dx, dy

	idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
	widget_control, idROISlider, get_value=ROINo
	info = (*pState).result->GetProperty(ROINo-1,oLabel = oLabel)
	result = oROI->ComputeGeometry(centroid=center)
	oLabel->SetProperty, locations=center[0:1]

    ; Move translate/scale selection visual along with the region.
    (*pState).oTransScaleVisual->GetProperty, UVALUE=pTSState
    (*pTSState).oTransModel->Translate, dx, dy, 0

    (*pTSState).oScaleLLModel->Translate, dx, dy, 0
    (*pTSState).oScaleLRModel->Translate, dx, dy, 0
    (*pTSState).oScaleULModel->Translate, dx, dy, 0
    (*pTSState).oScaleURModel->Translate, dx, dy, 0

	; Translate the rotate selection visual handles.
	(*pTSState).oRotateBModel->Translate, dx, dy, 0
	(*pTSState).oRotateUModel->Translate, dx, dy, 0
	(*pTSState).oRotateLModel->Translate, dx, dy, 0
	(*pTSState).oRotateRModel->Translate, dx, dy, 0

    ; Update bounding box corners.
    (*pTSState).oScaleLLModel->GetProperty, UVALUE=oldXY
    (*pTSState).oScaleLLModel->SetProperty, UVALUE=oldXY+[dx,dy]
    (*pTSState).oScaleURModel->GetProperty, UVALUE=oldXY
    (*pTSState).oScaleURModel->SetProperty, UVALUE=oldXY+[dx,dy]
end
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;function roi__CreatePickVisual
;    oModel = OBJ_NEW('IDLgrModel', HIDE=1)
;
;    ; Crosshair polyline.
;    oPolyline = OBJ_NEW('IDLgrPolyline', [[-8,0],[8,0],[0,-8],[0,8]], $
;                        POLYLINES=[2,0,1,2,2,3], COLOR=[255,255,0])
;
;    oModel->Add, oPolyline
;
;    return, oModel
;end
;------------------------------------------------------------------------------
function roi__CreateTransScaleVisual, COLOR=color
    oModel = OBJ_NEW('IDLgrModel', /HIDE)

    transpWhiteImage = BYTARR(4,2,2)
    transpWhiteImage[0:2,*,*] = 255
    transpWhiteImage[3,*,*] = 0
    oTranspImage = OBJ_NEW('IDLgrImage', transpWhiteImage, INTERLEAVE=0, $
        /HIDE)
    oModel->Add, oTranspImage ; Add for cleanup

    ; Create rotate handles.
    oRotateBox = OBJ_NEW('IDLgrPolygon', [-3,3,3,-3],[-3,-3,3,3], $
        TEXTURE_MAP=oTranspImage, TEXTURE_COORD=[[0,0],[1,0],[1,1],[0,1]], $
        NAME='ROTATE_BOX', COLOR=[255,255,255])
    oRotateBoxOutline = OBJ_NEW('IDLgrPolygon', [-3,3,3,-3],[-3,-3,3,3], $
        STYLE=1, NAME='ROTATE_BOX_OUTLINE', COLOR=color)

    oRotateBModel = OBJ_NEW('IDLgrModel', /SELECT_TARGET, NAME='ROTATE')
    oRotateBModel->Add, oRotateBox
    oRotateBModel->Add, oRotateBoxOutline
    oModel->Add, oRotateBModel

    oRotateRModel = OBJ_NEW('IDLgrModel', /SELECT_TARGET, NAME='ROTATE')
    oRotateRModel->Add, oRotateBox, /ALIAS
    oRotateRModel->Add, oRotateBoxOutline, /ALIAS
    oModel->Add, oRotateRModel

    oRotateUModel = OBJ_NEW('IDLgrModel', /SELECT_TARGET, NAME='ROTATE')
    oRotateUModel->Add, oRotateBox, /ALIAS
    oRotateUModel->Add, oRotateBoxOutline, /ALIAS
    oModel->Add, oRotateUModel

    oRotateLModel = OBJ_NEW('IDLgrModel', /SELECT_TARGET, NAME='ROTATE')
    oRotateLModel->Add, oRotateBox, /ALIAS
    oRotateLModel->Add, oRotateBoxOutline, /ALIAS
    oModel->Add, oRotateLModel

    ; Create scale handles.
    oScaleBox = OBJ_NEW('IDLgrPolygon', [-4,4,4,-4],[-4,-4,4,4], $
        TEXTURE_MAP=oTranspImage, TEXTURE_COORD=[[0,0],[1,0],[1,1],[0,1]], $
        NAME='SCALE_BOX', COLOR=[255,255,255])
    oScaleBoxOutline = OBJ_NEW('IDLgrPolygon', [-4,4,4,-4],[-4,-4,4,4], $
        STYLE=1, NAME='SCALE_BOX_OUTLINE', COLOR=color)

    oScaleLLModel = OBJ_NEW('IDLgrModel', /SELECT_TARGET, NAME='SCALE_LL')
    oScaleLLModel->Add, oScaleBox
    oScaleLLModel->Add, oScaleBoxOutline
    oModel->Add, oScaleLLModel

    oScaleLRModel = OBJ_NEW('IDLgrModel', /SELECT_TARGET, NAME='SCALE_LR')
    oScaleLRModel->Add, oScaleBox, /ALIAS
    oScaleLRModel->Add, oScaleBoxOutline, /ALIAS
    oModel->Add, oScaleLRModel

    oScaleULModel = OBJ_NEW('IDLgrModel', /SELECT_TARGET, NAME='SCALE_UL')
    oScaleULModel->Add, oScaleBox, /ALIAS
    oScaleULModel->Add, oScaleBoxOutline, /ALIAS
    oModel->Add, oScaleULModel

    oScaleURModel = OBJ_NEW('IDLgrModel', /SELECT_TARGET, NAME='SCALE_UR')
    oScaleURModel->Add, oScaleBox, /ALIAS
    oScaleURModel->Add, oScaleBoxOutline, /ALIAS
    oModel->Add, oScaleURModel

    ; Create translation bounding box.
    oTransModel = OBJ_NEW('IDLgrModel', /SELECT_TARGET, NAME='TRANSLATE')
    oTransBoxOutline = OBJ_NEW('IDLgrPolygon', STYLE=1, LINESTYLE=1, $
        NAME='TRANSLATE_BOX_OUTLINE',  COLOR=color)
    oTransModel->Add, oTransBoxOutline
    oModel->Add, oTransModel

    ; Save object references for easy access.
    sState = {$
        oScaleLLModel: oScaleLLModel, $
        oScaleLRModel: oScaleLRModel, $
        oScaleULModel: oScaleULModel, $
        oScaleURModel: oScaleURModel, $
        oScaleBoxOutline: oScaleBoxOutline, $
        oRotateBModel: oRotateBModel, $
        oRotateLModel: oRotateLModel, $
        oRotateUModel: oRotateUModel, $
        oRotateRModel: oRotateRModel, $
        oRotateBoxOutline: oRotateBoxOutline, $
        oTransModel: oTransModel, $
        oTransBoxOutline: oTransBoxOutline $
    }
    oModel->SetProperty, UVALUE=PTR_NEW(sState,/NO_COPY)

    return, oModel
end
;------------------------------------------------------------------------------
pro roi__ReshapeSelectionVisual, pState, oROI

    COMPILE_OPT STRICTARR

    ; If the region is not valid, hide any selection visuals and return.
    if (OBJ_VALID(oROI) eq 0) then begin
        if (OBJ_VALID((*pState).oSelVisual) ne 0) then $
            (*pState).oSelVisual->SetProperty, HIDE=1
        RETURN
    endif

    case (*pState).mode of
        'TRANSLATE-SCALE': begin
            ; Update translate/scale selection visual to match
            ; selected region's bounding box.
            oROI->GetProperty, ROI_XRANGE=xrange, ROI_YRANGE=yrange
            x0 = MIN(xrange, MAX=x1)
            y0 = MIN(yrange, MAX=y1)
            newBox = [[x0,y0,0.0],[x1,y0,0.0],[x1,y1,0.0],[x0,y1,0.0]]

            (*pState).oTransScaleVisual->GetProperty, UVALUE=pTSState

            (*pTSState).oTransModel->Reset
            (*pTSState).oTransBoxOutline->SetProperty, DATA=newBox

			dx = (x1 - x0) / 3 & dy = (y1 - y0) / 3
            (*pTSState).oRotateBModel->Reset
            (*pTSState).oRotateBModel->Translate, x0+dx, y0, 0
            (*pTSState).oRotateLModel->Reset
            (*pTSState).oRotateLModel->Translate, x0, y1-dy, 0
            (*pTSState).oRotateUModel->Reset
            (*pTSState).oRotateUModel->Translate, x1-dx, y1, 0
            (*pTSState).oRotateRModel->Reset
            (*pTSState).oRotateRModel->Translate, x1, y0+dy, 0

            (*pTSState).oScaleLLModel->Reset
            (*pTSState).oScaleLLModel->Translate, x0, y0, 0
            (*pTSState).oScaleLRModel->Reset
            (*pTSState).oScaleLRModel->Translate, x1, y0, 0
            (*pTSState).oScaleULModel->Reset
            (*pTSState).oScaleULModel->Translate, x0, y1, 0
            (*pTSState).oScaleURModel->Reset
            (*pTSState).oScaleURModel->Translate, x1, y1, 0
            (*pTSState).oScaleLLModel->SetProperty, UVALUE=[x0,y0]
            (*pTSState).oScaleURModel->SetProperty, UVALUE=[x1,y1]

            ; Make sure the selection visual is visible.
            (*pState).oTransScaleVisual->SetProperty, HIDE=0
        end

        'SELECTION': begin
            ; Make sure the pick visual is visible.
;            (*pState).oPickVisual->SetProperty, HIDE=0

            ; For now, hide vertex within the visual since we do not
            ; know where to position it.
;            oPickPolyline = (*pState).oPickVisual->Get()
;            oPickPolyline->SetProperty, HIDE=1
        end

        else: begin
        end
    endcase
end
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
pro OnHideROI, Event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  widget_control, event.top, get_uvalue=pState
  if ~obj_valid((*pState).oManualRoiModel) and ~obj_valid((*pState).oAutoRoiModel) then return
  tags = tag_names(*pState)
  if where(tags eq 'ROIHIDE') eq -1 then return
;  print,tags
  if (*pState).RoiHide eq 0b then begin
    widget_control, event.id, set_value='images\HideROI.bmp', /bitmap
    (*pState).RoiHide = 1b
    if obj_valid((*pState).oAutoRoiModel) then (*pState).oAutoRoiModel->SetProperty, hide=1
    if obj_valid((*pState).oManualRoiModel) then (*pState).oManualRoiModel->SetProperty, hide=1
    if obj_valid((*pState).oManualRoiModel) then (*pState).oLineScan->SetProperty, hide=1
  endif else begin
    widget_control, event.id, set_value='images\ShowROI.bmp', /bitmap
    (*pState).RoiHide = 0b
    if obj_valid((*pState).oAutoRoiModel) then (*pState).oAutoRoiModel->SetProperty, hide=0
    if obj_valid((*pState).oManualRoiModel) then (*pState).oManualRoiModel->SetProperty, hide=0
    if obj_valid((*pState).oManualRoiModel) then (*pState).oLineScan->SetProperty, hide=0
  endelse
  (*pState).oWindow->Draw, (*pState).oView
end

;-----------------------------------------------------------------
pro OnHideLabel, Event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  widget_control, event.top, get_uvalue=pState
  if ~obj_valid((*pState).oLabelModel) then return
  tags = tag_names(*pState)
  if where(tags eq 'LABELHIDE') eq -1 then return

  if (*pState).LabelHide eq 0b then begin
    widget_control, event.id, set_value='images\HideLabel.bmp', /bitmap
    (*pState).LabelHide = 1b
    (*pState).oLabelModel->SetProperty, hide=1
  endif else begin
    widget_control, event.id, set_value='images\ShowLabel.bmp', /bitmap
    (*pState).LabelHide = 0b
    (*pState).oLabelModel->SetProperty, hide=0
  endelse
  (*pState).oWindow->Draw, (*pState).oView
end

;-----------------------------------------------------------------
pro OnChangeROI, Event, currentRoi=currentRoi, trace=trace
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  widget_control, event.top, get_uvalue=pState
  print,event.value
  info = (*pState).result->GetProperty(event.value[n_elements(event.value)-1]-1, $
    oROI = selROI, oLabel=oLabel, trace=trace, flashPara=flashPara)
  if info eq -1 then begin
;    void = dialog_message('The ROI No. is out of range!',/error)
    return
  endif
  if obj_valid(selRoi) then begin
    if obj_valid((*pState).oSelROI) then (*pState).oSelROI->SetProperty, color=(*pState).roi_rgb
    (*pState).oSelROI = selRoi
    (*pState).oSelROI->setProperty, color=(*pState).sel_rgb
    (*pState).oWindow->Draw, (*pState).oView
    if ~ptr_valid(flashPara) then begin
      PlotTrace, pState, trace[0],title=file_basename((*pState).file[0])+'  ROI '+strtrim(event.value,1)
    endif else begin
      PlotTrace, pState, trace[0],title=file_basename((*pState).file[0])+'  ROI '+strtrim(event.value,1),flashPara=flashPara
    endelse
;    BaselineSubtraction,trace[0]
  endif
;  signal = reform((*trace[0])[0,*])
;  signal = median(signal,3)
;  signal = smooth(signal,3,edge=1)
;  window,1,xs=800,ys=600
;  plot, signal, yrange=[-20,100]
;  oplot, intarr(n_elements(signal))
;  d1 = signal-shift(signal,1)
;  d2 = d1-shift(d1,1)
;  oplot, d1
end

;-----------------------------------------------------------------
Pro DeleteROI, pState, ROINo
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  info = (*pState).result->GetProperty(ROINo-1,oROI = currentRoi,oLabel = oLabel,flashNum=flashNum)
  if info eq -1 then begin
    void = dialog_message('The ROI No. is out of range!',/error)
  endif
  (*pState).oLabelModel->Remove, oLabel
  currentRoi->GetProperty, parent=oParent
  oParent->remove, currentRoi
  info = (*pState).result->Delete(ROINo-1)
  ROINum = (*pState).result->length()

  ; Correct ROI label
  if ROINum ge ROINo then begin
    for i=ROINo-1, ROINum-1 do begin
      info = (*pState).result->GetProperty(i, oLabel=oLabel)
      oLabel->SetProperty, strings=string(i+1,format='(i0)')
    endfor
  endif

  ; Update TraceWindow content and ROISlider value
  idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
  if ROINum eq 0 then begin
    TraceDrawInitialize, (*pState).TraceDraw
    widget_control, idROISlider, sensitive=0
  endif else begin
    widget_control, idROISlider, set_slider_max=ROINum
    case ROINo-1 of
    0: value = 1
    ROINum: value = ROINum
    else: value = ROINo
    endcase
    event = {top:(*pState).MainBase, value:value}
    OnChangeROI, event
  endelse

  ; update ResultList
  widget_control, (*pState).ResultTable, get_value=table
  ROINoColumn = uint(reform(table[0,*]))
  lastRow = max(where(ROINoColumn ne 0))
  if lastRow eq -1 then return
  row = where(ROINoColumn eq ROINo,n_row)
  if row[0]+n_row-1 eq lastrow then begin
    table[*,row[0]:row[0]+n_row-1] = ''
  endif else begin
    table[0,row[0]+n_row:lastRow] = table[0,row[0]+n_row:lastRow]-1
    table[*,row[0]:lastRow-n_row] = table[*,row[0]+n_row:lastRow]
    table[*,lastRow-n_row+1:lastRow] = ''
  endelse
  widget_control, (*pState).ResultTable, set_value=table
End

;-----------------------------------------------------------------
pro OnDeleteROI, Event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  widget_control, event.top, get_uvalue=pState
; print, (*pState).result->length()
  if obj_valid((*pState).result) eq 0 then return
  ROINum = (*pState).result->length()
  if ROINum le 0 then begin
    notice = dialog_message('There is no flash left!', /Error)
    return
  endif
  answer = dialog_message('Are you sure to delete this flash?', /question)
  if answer eq 'No' then return
  ;help
  idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
  widget_control, idROISlider, get_value=ROINo
  DeleteROI, pState, ROINo
  (*pState).oWindow->Draw, (*pState).oView
end
;-----------------------------------------------------------------
pro OnDeleteChosenROIs, event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

	widget_control, event.top, get_uvalue=pState
;	print, (*pState).result->length()
	if obj_valid((*pState).result) eq 0 then return
	ROINum = (*pState).result->length()
	if ROINum le 0 then begin
	notice = dialog_message('There is no flash left!', /Error)
	return
	endif
	answer = dialog_message('Are you sure to delete the flash?', /question)
	if answer eq 'No' then return

	info = (*pState).result->GetProperty(ROINum-1,oROI = currentRoi)
	flag = 0
	FOR i=0, ROINum-1 DO BEGIN
		info = (*pState).result->GetProperty(flag, oLabel=oLabel)
		oLabel->GetProperty, LOCATIONS=location
		roiMask = currentROI->ContainsPoints(location)
		IF roiMask EQ 1 THEN BEGIN
			DeleteROI, pState, flag+1
			flag = flag - 1
		ENDIF
		flag = flag + 1
;n = n_elements(roiMask[1,*])
;
;
;	idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
;
;(*pState).oAnimationModel[(*pState).currStack]->SetProperty, HIDE=1
;(*pState).oLabelModel->SetProperty, /HIDE
;
;
;idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
;for i=0, n-1 do begin
;	x = roiMask[1,i] mod (*pState).xs
;	y = roiMask[1,i] / (*pState).xs
;	oSel = (*pState).oWindow->Select((*pState).oView, [x,y],  DIMENSIONS=[1,1])
;	selType = SIZE(oSel, /TYPE)
;	if (selType eq 11) then begin  ; Object reference?
;	oROI = oSel[0]
;
;  widget_control, idROISlider, get_value=ROINo
;  DeleteROI, pState, ROINo
;  endif
	ENDFOR
;(*pState).oAnimationModel[(*pState).currStack]->SetProperty, HIDE=0
;(*pState).oLabelModel->SetProperty, HIDE=0


;	widget_control, idROISlider, get_value=ROINo
;	DeleteROI, pState, ROINo
	(*pState).oWindow->Draw, (*pState).oView
end
;-----------------------------------------------------------------
Pro DeleteManualROI, pState
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF
  next = 0
  while 1 do begin
    info = (*pState).result->GetProperty(next,oROI = currentRoi,oLabel = oLabel,flashNum=flashNum)
    if info eq -1 then begin
      break
    endif
    currentRoi->GetProperty, parent=oParent
    if oParent eq (*pState).oManualROIModel then begin
      (*pState).oLabelModel->Remove, oLabel
      oParent->remove, currentRoi
      info = (*pState).result->Delete(next)
    endif else begin
      next++
      oLabel->SetProperty, strings=string(next,format='(i0)')
      continue
    endelse
  endwhile
  ROINum = (*pState).result->length()

  ; Update TraceWindow content and ROISlider value
  idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
  if ROINum eq 0 then begin
    TraceDrawInitialize, (*pState).TraceDraw
    widget_control, idROISlider, sensitive=0
  endif else begin
    widget_control, idROISlider, set_slider_max=ROINum
    event = {top:(*pState).MainBase, value:ROINum}
    OnChangeROI, event
  endelse

  ; update ResultList
;flashPara [   0   ,  1  , 2 ,  3 ,   4   ,  5   , 6 ,  7  ,  8 ,    9   ,  10 ,  11 ,  12  , 13 , 14 ,  15 , 18, 19]
;flashPara [FlashNo, Peak, F0, Amp, Tstart, Tpeak, RT, RT90, T50, EndBase, Note, FAHM, dBase, T75, T90, FDHM, t1, t2]
  widget_control, (*pState).ResultTable, get_value=table
  dim = size(table,/dimensions)
  table[*] = ''
  if ROINum gt 0 then begin
    row=0
    for ROINo=0, ROINum-1 do begin
      info = (*pState).result->GetProperty(ROINo,oLabel=oLabel,ROIPara=ROIPara,flashPara=flashPara,flashNum=flashNum)
      if obj_valid(oLabel) then oLabel->SetProperty, strings=string(ROINo+1,format='(i0)')
      for flashNo=1,flashNum do begin
        table[0:2,row] = [string(ROINo+1,format='(i0)'),string(reform(ROIPara,2,1),format='(f10.2)')]
        if ptr_valid(flashPara) then begin
          dim = size(*flashPara)
          if dim[1] eq 9 then begin
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
          table[3:*,row] = [string(reform((*flashPara)[0,flashNo-1]),format='(i0)'), $
              string(reform((*flashPara)[1:9,flashNo-1],9,1),format='(f10.2)'), $
              string(reform((*flashPara)[10,flashNo-1]),format='(i0)'), $
              string(reform((*flashPara)[11:16,flashNo-1],6,1),format='(f10.2)')]
        endif
        row++
      endfor
    endfor
    endif

  widget_control, (*pState).ResultTable, set_value=table
End

;-----------------------------------------------------------------
pro OnDeleteManualROI, Event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  answer = dialog_message('Are you sure to delete all Manual ROI?', /question)
  if answer eq 'No' then return
  widget_control, event.top, get_uvalue=pState
  DeleteManualROI, pState
  (*pState).oWindow->Draw, (*pState).oView
  TraceDrawInitialize, (*pState).TraceDraw
  idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
  ROINum = (*pState).result->length()
  if ROINum eq 0 then begin
    widget_control, idROISlider, sensitive=0
  endif else begin
    widget_control, idROISlider, set_value=1, set_slider_max=ROINum
  endelse
End
Pro DeleteAutoROI, pState
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF
next = 0
  while 1 do begin
    info = (*pState).result->GetProperty(next,oROI = currentRoi,oLabel = oLabel,flashNum=flashNum)
    if info eq -1 then begin
      break
    endif
    currentRoi->GetProperty, parent=oParent
    if oParent eq (*pState).oAutoROIModel then begin
      (*pState).oLabelModel->Remove, oLabel
      oParent->remove, currentRoi
      info = (*pState).result->Delete(next)
    endif else begin
      next++
      oLabel->SetProperty, strings=string(next,format='(i0)')
      continue
    endelse
  endwhile
  ROINum = (*pState).result->length()

  ; Update TraceWindow content and ROISlider value
  idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
  if ROINum eq 0 then begin
    TraceDrawInitialize, (*pState).TraceDraw
    widget_control, idROISlider, sensitive=0
  endif else begin
    widget_control, idROISlider, set_slider_max=ROINum
    event = {top:(*pState).MainBase, value:ROINum}
    OnChangeROI, event
  endelse

  ; update ResultList
;flashPara [   0   ,  1  , 2 ,  3 ,   4   ,  5   , 6 ,  7  ,  8 ,    9   ,  10 ,  11 ,  12  , 13 , 14 ,  15 , 16 , 18, 19]
;flashPara [FlashNo, Peak, F0, Amp, Tstart, Tpeak, RT, RT90, T50, EndBase, Note, FAHM, dBase, T75, T90, FDHM, Dia, t1, t2]
  widget_control, (*pState).ResultTable, get_value=table
  dim = size(table,/dimensions)
  table[*] = ''
  if ROINum gt 0 then begin
    row=0
    for ROINo=0, ROINum-1 do begin
      info = (*pState).result->GetProperty(ROINo,oLabel=oLabel,ROIPara=ROIPara,flashPara=flashPara,flashNum=flashNum)
      if obj_valid(oLabel) then oLabel->SetProperty, strings=string(ROINo+1,format='(i0)')
      for flashNo=1,flashNum do begin
        table[0:2,row] = [string(ROINo+1,format='(i0)'),string(reform(ROIPara,2,1),format='(f10.2)')]
        if ptr_valid(flashPara) then begin
          dim = size(*flashPara)
          if dim[1] eq 9 then begin
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
          table[3:*,row] = [string(reform((*flashPara)[0,flashNo-1]),format='(i0)'), $
              string(reform((*flashPara)[1:9,flashNo-1],9,1),format='(f10.2)'), $
              string(reform((*flashPara)[10,flashNo-1]),format='(i0)'), $
              string(reform((*flashPara)[11:16,flashNo-1],6,1),format='(f10.2)')]
        endif
        row++
      endfor
    endfor
    endif

  widget_control, (*pState).ResultTable, set_value=table
End

;-----------------------------------------------------------------
pro OnDeleteAutoROI, event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  answer = dialog_message('Are you sure to delete all Auto ROI?', /question)
  if answer eq 'No' then return
  widget_control, event.top, get_uvalue=pState
  DeleteAutoROI, pState
  (*pState).oWindow->Draw, (*pState).oView
  idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
  ROINum = (*pState).result->length()
  if ROINum eq 0 then begin
    widget_control, idROISlider, sensitive=0
  endif else begin
    widget_control, idROISlider, set_value=1, set_slider_max=ROINum
  endelse
  TraceDrawInitialize, (*pState).TraceDraw
End


; -----------------------------------------------------------------------------------------------
Function OuterProduct, a, b
COMPILE_OPT STRICTARR

  if n_elements(a) ne 3 or n_elements(b) ne 3 then return, -1
  a = double(a) & b = double(b)
  result = [0,0,0]
  result[0] = a[1]*b[2] - a[2]*b[1]
  result[1] = a[2]*b[0] - a[0]*b[2]
  result[2] = a[0]*b[1] - a[1]*b[0]
  return, result
End

;-----------------------------------------------------------------
;Pro OnImageWindowEvent, Event
;COMPILE_OPT STRICTARR
;;  CATCH, Error_status
;;  IF Error_status NE 0 THEN BEGIN
;;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;;    return
;;    CATCH, /CANCEL
;;  ENDIF
;
;  widget_control, event.top, get_uvalue=pState
;
;  if obj_valid( (*pState).result) eq 0 then return
;
;;		'DRAW': begin
;;		Handle all events in the draw area.
;
;	case event.type of
;; 				Button Press
;		0: roi__ButtonPress, event
;
;;				Button Release
;		1: begin
;			widget_control, event.id, get_uvalue=init
;			roi__ButtonRelease, event
;			init.IsOption = 0b
;			widget_control, event.id, set_uvalue=init
;			;      (*pState).currentROI = obj_new()
;;					(*pState).option = ''
;;					widget_control, event.id, draw_keyboard_events=0
;		end
;
;; 				Motion
;		2: roi__Motion, event
;
;		else: begin
;		end
;	endcase
;
;  if event.type eq 1 and event.release eq 4 then begin
;    WIDGET_DISPLAYCONTEXTMENU, (*pState).AnimationBase, event.x+15, (*pState).ys-event.y+43, (*pState).ContextBase1
;    return
;  endif
;
;;  if obj_valid((*pState).currentROI) then begin   ; Options
;  if (*pState).option ne '' then begin
;    widget_control, event.id, get_uvalue=init
;    case event.type of
;    5: begin
;      if event.press eq 1 then begin    ; On keyboard press: define options
;        case string(event.ch) of
;        'r': begin
;          (*pState).option = 'rotation'
;          (*pState).oWindow->SetCurrentCursor, Image = (*pState).cursor.image[1,*], $
;            Mask = (*pState).cursor.mask[1,*], Hotspot = (*pState).cursor.hotSpot[1,*]
;        end
;        'R': begin
;          (*pState).option = 'rotation'
;          (*pState).oWindow->SetCurrentCursor, Image = (*pState).cursor.image[1,*], $
;            Mask = (*pState).cursor.mask[1,*], Hotspot = (*pState).cursor.hotSpot[1,*]
;        end
;        's': begin
;          (*pState).option = 'scale'
;          (*pState).oWindow->SetCurrentCursor, Image = (*pState).cursor.image[2,*], $
;            Mask = (*pState).cursor.mask[2,*], Hotspot = (*pState).cursor.hotSpot[2,*]
;        end
;        'S': begin
;          (*pState).option = 'scale'
;          (*pState).oWindow->SetCurrentCursor, Image = (*pState).cursor.image[2,*], $
;            Mask = (*pState).cursor.mask[2,*], Hotspot = (*pState).cursor.hotSpot[2,*]
;        end
;        else:
;        endcase
;      endif else begin
;        (*pState).option = 'translation'   ; On keyboard release: restore options
;        (*pState).oWindow->SetCurrentCursor, Image = (*pState).cursor.image[0,*], $
;          Mask = (*pState).cursor.mask[0,*], Hotspot = (*pState).cursor.hotSpot[0,*]
;      endelse
;    end
;    2: begin    ; On mouse motion: excute options
;      case (*pState).option of
;      'creation': begin
;        (*pState).currentROI->GetProperty, data=data
;        x0=data[0,0] & y0=data[1,0] & x1=event.x & y1=event.y
;        (*pState).currentROI->SetProperty, data=[[x0,y0],[x1,y0],[x1,y1],[x0,y1]]
;        (*pState).oWindow->Draw, (*pState).oView
;        return
;      end
;      'translation': begin
;        Tx = event.x - init.x & Ty = event.y- init.y
;        (*pState).currentROI->Translate, Tx, Ty
;        (*pState).oWindow->Draw, (*pState).oView
;      end
;      'scale': begin
;         d0 = double(abs(init.center-[init.x,init.y]) > 1)
;         d = abs(init.center-[event.x,event.y]) > 1
;         Scale = [1.0,1.0]
;         if d0[0] ne 0 and d[0] ne 0 then scale[0] = d[0]/d0[0]
;         if d0[1] ne 0 and d[1] ne 0 then scale[1] = d[1]/d0[1]
;         (*pState).currentROI->Translate, -init.center
;         (*pState).currentROI->Scale, Scale
;         (*pState).currentROI->Translate, init.center
;         (*pState).oWindow->Draw, (*pState).oView
;      end
;      'rotation': begin
;        v0 = init.center-[init.x,init.y] & d0 = sqrt(total(v0^2))
;        v = init.center-[event.x,event.y] & d = sqrt(total(v^2))
;        cosTheta = total(v0*v)/d0/d
;        direction = OuterProduct([v0,0],[v,0])
;        theta = (direction[2] ge 0) ? acos(cosTheta)*180/!DPI : -acos(cosTheta)*180/!DPI ;& print,theta
;        (*pState).currentROI->Rotate, [0,0,1], theta, center=init.center
;        (*pState).oWindow->Draw, (*pState).oView
;      end
;      else:
;      endcase
;      init.x = event.x & init.y = event.y & init.IsOption = 1b
;      result = (*pState).currentROI->ComputeGeometry(centroid=centroid)
;      init.center = centroid[0:1]
;      widget_control, event.id, set_uvalue=init
;    end
;    1: begin   ; On mouse release: finish options
;;      (*pState).currentROI->SetProperty, color=[255,0,0]
;;      (*pState).oWindow->Draw, (*pState).oView
;      if (*pState).option eq 'creation' then begin    ; finish creation
;        init.x = event.x & init.y = event.y & init.IsOption = 1b
;        result = (*pState).currentROI->ComputeGeometry(centroid=center, area=area)
;        init.center = center[0:1]
;        if area gt 9 then begin     ; the roi must have enough area
;          node = obj_new('ChainNode')
;          ROINum = (*pState).result->length()
;          oLabel = obj_new('IDLgrText', string(ROINum+1,format='(i0)'), $
;            font=(*pState).oROIFont, color=[255,255,0], locations=init.center)
;          roiMask = (*pState).currentROI->ComputeMask(dimensions=[(*pState).xs,(*pState).ys])
;          indices = where(roiMask gt 0)
;          trace = GetTrace(pState, indices)
;          PlotTrace, pState, trace[0],title=file_basename((*pState).file[0])+'  ROI '+strtrim(ROINum+1,1)
;          area *= ((*pState).pxs)^2
;          distance = ptr_valid((*pState).mask) ? DistToEdge(*(*pState).edge,center)*(*pState).pxs : 0
;          node->SetProperty,oROI=(*pState).currentROI,olabel=oLabel,trace=trace,ROIPara=[area,distance],flashNum=1
;          (*pState).oLabelModel->Add, oLabel
;          info = (*pState).result->insert(node)
;          widget_control, (*pState).ResultTable, get_value=table
;          ROINoColumn = uint(reform(table[0,*]))
;          lastRow = max(where(ROINoColumn ne 0))
;          widget_control, (*pState).ResultTable, use_table_select=[0,lastrow+1,2,lastRow+1], $
;            set_value=[string(ROINum+1,format='(i0)'),string([area,distance],format='(f0.2)')]
;          idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
;          widget_control,idROISlider,set_slider_min=1,set_slider_max=ROINum+1,set_value=ROINum+1,sensitive=1
;        endif else begin
;          (*pState).oManualROIModel->Remove, (*pState).currentROI
;        endelse
;      endif else begin    ; finish other options
;        currentRoi = (*pState).result->SearchROI((*pState).currentROI, roiNo=roiNo)
;        info = (*pState).result->GetProperty(roiNo, olabel=oLabel, trace=trace)
;        oLabel->SetProperty, locations=init.center
;        idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
;        widget_control, idROISlider, set_value=roiNo+1
;        if init.IsOption eq 1b then begin
;          roiMask = (*pState).currentROI->ComputeMask(dimensions=[(*pState).xs,(*pState).ys])
;          indices = where(roiMask gt 0)
;          trace = GetTrace(pState, indices)
;          result = (*pState).currentROI->ComputeGeometry(centroid=center, area=area)
;          area *= ((*pState).pxs)^2
;          distance = ptr_valid((*pState).mask) ? DistToEdge(*(*pState).edge,center)*(*pState).pxs : 0
;          info = (*pState).result->SetProperty(roiNo, trace=trace, ROIPara=[area,distance])
;          widget_control, (*pState).ResultTable, get_value=table
;          ROINoColumn = uint(reform(table[0,*]))
;          row = where(ROINoColumn eq ROINo+1,n_row)
;          row = row[0]
;          if row ne -1 then widget_control, (*pState).ResultTable, use_table_select=[1,row,2,row+n_row-1], $
;            set_value=string(rebin([area,distance],2,n_row),format='(f0.2)')
;        endif
;        (*pState).oWindow->SetCurrentCursor, 'CROSSHAIR'
;        wset, (*pState).TraceWindow
;        PlotTrace, pState, trace[0],title=file_basename((*pState).file[0])+'  ROI '+strtrim(ROINo+1,1)
;        BaselineSubtraction, trace[0]
;      endelse
;      init.IsOption = 0b
;      widget_control, event.id, set_uvalue=init
;;      (*pState).currentROI = obj_new()
;      (*pState).option = ''
;      widget_control, event.id, draw_keyboard_events=0
;      (*pState).oWindow->Draw, (*pState).oView
;    end
;    else:
;    endcase
;  endif else begin
;    if event.type eq 0 and event.press eq 1 then begin   ; On left button press: select or create a roi
;      if obj_valid((*pState).currentROI) then begin
;        (*pState).currentROI->SetProperty, color=[255,0,0]
;        (*pState).oWindow->Draw, (*pState).oView
;      endif
;      selectObj= (*pState).oWindow->Select((*pState).oView, [event.x,event.y],dimensions=[8,8])
;      if n_elements(selectObj) gt 1 then begin   ; select
;        for i=1, n_elements(selectObj)-1 do begin
;          if obj_isa(selectObj[i], 'IDLgrROI') then begin
;            currentROI = selectObj[i] & break
;          endif
;        endfor
;      endif
;      if obj_valid(currentROI) then begin    ; An roi is selected
;        (*pState).currentROI = currentROI
;        (*pState).currentROI->GetProperty, parent=oParent
;        (*pState).currentROI->SetProperty, color=[100,200,255]
;        widget_control, event.id, get_uvalue=init
;        init.x = event.x & init.y = event.y & init.IsOption = 0b
;        result = (*pState).currentROI->ComputeGeometry(centroid=centroid)
;        init.center = centroid[0:1]
;        widget_control, event.id, set_uvalue=init
;        if oParent eq (*pState).oManualROIModel then begin
;          (*pState).option = 'translation'
;          widget_control, event.id, draw_keyboard_events=1
;          (*pState).oWindow->SetCurrentCursor, Image = (*pState).cursor.image[0,*], $
;            Mask = (*pState).cursor.mask[0,*], Hotspot = (*pState).cursor.hotSpot[0,*]
;        endif else (*pState).option = 'selection'
;        (*pState).oWindow->Draw, (*pState).oView
;      endif else begin   ; Create new roi
;        (*pState).option = 'creation'
;        roi = obj_new('IDLgrROI', color=[0,255,255], data=[event.x,event.y], alpha_channel=1.0)
;        (*pState).oManualROIModel->add, roi
;        (*pState).currentROI = roi
;      end
;      widget_control, event.id, draw_motion_events=1
;    endif
;  endelse
;
;end

;-----------------------------------------------------------------
Pro OnChangeAnimationChannel, event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  widget_control, event.top, get_uvalue=pState
  widget_control, event.id, get_uvalue=old_channel
;  print,event.index
  if  event.index eq old_channel then return
  (*pState).oAnimationModel[old_channel]->SetProperty, hide=1
  (*pState).oAnimationModel[event.index]->SetProperty, hide=0
  (*pState).oObserver->SetProperty, oIMAGES=(*pState).oAnimationModel[event.index]
  (*pState).oWindow->Draw, (*pState).oView
  (*pState).currStack = event.index
  widget_control, event.id, set_uvalue=event.index
End

;-----------------------------------------------------------------
pro OnChangeFPS, Event
COMPILE_OPT STRICTARR
  widget_control, event.top, get_uvalue=pState
  (*pState).interval = 1.0/event.value
;  print,event.value
  (*pState).oWindow->SetTimerInterval, (*pState).interval
end

;-----------------------------------------------------------------
pro OnPlay, Event
COMPILE_OPT STRICTARR
  widget_control, event.top, get_uvalue=pState
  idFrameSlider = widget_info((*pState).AnimationBase, find_by_uname='FrameSlider')
  widget_control, idFrameSlider, sensitive=0
  WIDGET_CONTROL, Event.top, TIMER=(*pState).interval
  (*pState).oWindow->SetEventMask, TIMER_EVENTS=1
end

;-----------------------------------------------------------------
pro OnStop, Event
COMPILE_OPT STRICTARR
  widget_control, event.top, get_uvalue=pState
  (*pState).oWindow->SetEventMask, TIMER_EVENTS=0
  WIDGET_CONTROL, Event.top, TIMER=1000
  (*pState).oWindow->Draw, (*pState).oView
  idFrameSlider = widget_info((*pState).AnimationBase, find_by_uname='FrameSlider')
  widget_control, idFrameSlider, sensitive=1
end

;-----------------------------------------------------------------
Pro OnChangeFrame, Event
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF
  widget_control, event.top, get_uvalue=pState
  (*pState).oObserver->OnSlider, (*pState).oWindow, event.value

  widget_control,(*pState).traceDraw,get_uvalue=pixWin
  widget_control,(*pState).traceDraw,get_value=owin
  help,(*pstate).tracedraw
  help,pixWin,/str
  help,owin
  if pixWin.tag eq 0b then return
  if ~obj_valid((*pState).result) then return
  if (*pState).result->length() lt 1 then return

  idROISlider = widget_info((*pState).AnimationBase, find_by_uname='ROISlider')
  widget_control, idROISlider, get_value=ROINo
  info = (*pState).result->GetProperty(ROINo-1,trace=trace)
  if info eq -1 then return
  wset, (*pState).TraceWindow
  device, copy=[0,0,!D.x_size,!D.Y_SIZE,0,0,pixWin.id]
  oplot, replicate((*(*pState).correctedTime)[event.value],50),lindgen(50)*10-250,$
    color='00FFFF'XL,thick=2
;    print,'on',event.value
end

;-----------------------------------------------------------------
Pro OnExportImage, Event
  widget_control, event.top, get_uvalue=pState
;  print,(*pState).ContextBase1
  WIDGET_DISPLAYCONTEXTMENU,(*pState).AnimationBase,200,30,(*pState).ContextBase1
End

;-----------------------------------------------------------------
pro CreatAnimationModel, pState
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  device,decomposed=0 ;& window,1
  (*pState).oObserver = OBJ_NEW('timer_observer')
  for k=0,(*pState).stackNum-1 do begin
    (*pState).oAnimationModel[k] = OBJ_NEW('IDLgrModel', RENDER_METHOD=1,select_target=0)
    red = ((*pState).stackColor[0,k] ne 0) ? indgen(256) : intarr(256)
    green = ((*pState).stackColor[1,k] ne 0) ? indgen(256) : intarr(256)
    blue = ((*pState).stackColor[2,k] ne 0) ? indgen(256) : intarr(256)
    oPalette = OBJ_NEW('IDLgrPalette',red_values=red,green_values=green,blue_values=blue)
      FOR i=0, (*pState).ts-1 do begin
        tmpimg = ((*pState).brightness eq 1 and (*pState).contrast eq 0) ? (*(*pState).img[k])[*,*,i] $
          : AdjustContrast((*(*pState).img[k])[*,*,i], (*pState).brightness, (*pState).contrast)
;         wset,1 & tv,tmpimg>0
        oImageColl = OBJ_NEW('IDLgrImage', byte((tmpimg>0)<255), PALETTE=oPalette)
        (*pState).oAnimationModel[k]->Add, oImageColl
      ENDFOR
      if k ne 0 then (*pState).oAnimationModel[k]->SetProperty, hide=1
      (*pState).oView->Add, (*pState).oAnimationModel[k], position=k
  endfor

  (*pState).oObserver->SetProperty, oIMAGES=(*pState).oAnimationModel[0], $
    idFrameSlider=widget_info((*pState).AnimationBase, find_by_uname='FrameSlider')
  (*pState).oWindow->AddWindowEventObserver, (*pState).oObserver
  idFPSSlider = widget_info((*pState).AnimationBase, find_by_uname='FPSSlider')
  widget_control, idFPSSlider, get_value=rate
  (*pState).interval = 1.0/rate
  (*pState).oWindow->SetTimerInterval, (*pState).interval

  tags = tag_names(*pState)
  if where(tags eq 'ROIHIDE') ne -1 then begin
    (*pState).ROIHide = 0b
    (*pState).oAutoROIModel->SetProperty, hide=0
    (*pState).oManualROIModel->SetProperty, hide=0
    (*pState).oLineScan->SetProperty, hide=0
  endif
  if where(tags eq 'LABELHIDE') ne -1 then begin
    (*pState).LabelHide = 0b
    (*pState).oLabelModel->SetProperty, hide=0
  endif
  (*pState).oWindow->Draw, (*pState).oView

end

; -----------------------------------------------------------------------------------------------
Pro CreatAnimationBase_Event, event

    WIDGET_CONTROL, event.id, GET_UVALUE=uval
    
;    help,uval
    if ptr_valid(uval) eq 1 then return ;Added by Sun Tao 
    case uval of
		'DRAW': begin
;		Handle all events in the draw area.

			case event.type of
; 				Button Press
				0: roi__ButtonPress, event

;				Button Release
				1: begin
					WIDGET_CONTROL, event.top, GET_UVALUE=pState
					if obj_valid( (*pState).result) NE 0 then $
						IF ~OBJ_VALID((*pState).currentROI) then $
							if event.release eq 4 then $
								WIDGET_DISPLAYCONTEXTMENU, (*pState).AnimationBase, event.x+15, (*pState).ys-event.y+43, (*pState).ContextBase1

					init = *(*pState).init
					roi__ButtonRelease, event
					init.IsOption = 0b
					*(*pState).init = init
			;       (*pState).currentROI = obj_new()
;					(*pState).option = ''
;					widget_control, event.id, draw_keyboard_events=0
;						return
				end

; 				Motion
				2: roi__Motion, event

				else: begin
				end
			endcase
		end

		'TRANSLATE-SCALE': begin
;		Translate/Scale tool selected.
			WIDGET_CONTROL, event.top, GET_UVALUE=pState
			if (*pState).mode ne 'TRANSLATE-SCALE' then begin
				(*pState).mode = 'TRANSLATE-SCALE'

; 				Disable old selection visual, if any.
				oSelVisual = (*pState).oSelVisual
				if (OBJ_VALID(oSelVisual) ne 0) then begin
					oSelVisual->SetProperty, /HIDE
					(*pState).oSelVisual = OBJ_NEW()
				endif

; 				Set the translate/scale selection visual as current.
				(*pState).oSelVisual = (*pState).oTransScaleVisual
				roi__ReshapeSelectionVisual, pState, (*pState).oSelROI

				(*pState).oWindow->Draw, (*pState).oView
			endif
		end

		'RECTANGLE': begin
; 			Rectangle ROI tool selected.
			WIDGET_CONTROL, event.top, GET_UVALUE=pState
			if (*pState).mode ne 'RECTANGLE' then begin
				(*pState).mode = 'RECTANGLE'

; 				Disable old selection visual, if any.
				oSelVisual = (*pState).oSelVisual
				if (OBJ_VALID(oSelVisual) ne 0) then begin
					oSelVisual->SetProperty, /HIDE
					(*pState).oSelVisual = OBJ_NEW()
				endif

				(*pState).oWindow->Draw, (*pState).oView
			endif
		end

		'ELLIPSE': begin
; 			Ellipse ROI tool selected.
			WIDGET_CONTROL, event.top, GET_UVALUE=pState
			if (*pState).mode ne 'ELLIPSE' then begin
				(*pState).mode = 'ELLIPSE'

; 				Disable old selection visual, if any.
				oSelVisual = (*pState).oSelVisual
				if (OBJ_VALID(oSelVisual) ne 0) then begin
					oSelVisual->SetProperty, /HIDE
					(*pState).oSelVisual = OBJ_NEW()
				endif

				(*pState).oWindow->Draw, (*pState).oView
			endif
		end

		'FREEPOLY': begin
; 			Freehand ROI tool selected.
			WIDGET_CONTROL, event.top, GET_UVALUE=pState
			if (*pState).mode ne 'FREEHAND DRAW' then begin
				(*pState).mode = 'FREEHAND DRAW'

;				 Disable old selection visual, if any.
				oSelVisual = (*pState).oSelVisual
				if (OBJ_VALID(oSelVisual) ne 0) then begin
					oSelVisual->SetProperty, /HIDE
					(*pState).oSelVisual = OBJ_NEW()
				endif

				(*pState).oWindow->Draw, (*pState).oView
			endif
		end
		
		'FREEHAND DRAW': begin
; 			Freehand ROI tool selected.
			WIDGET_CONTROL, event.top, GET_UVALUE=pState
			if (*pState).mode ne 'FREEHAND DRAW' then begin
				(*pState).mode = 'FREEHAND DRAW'

;				 Disable old selection visual, if any.
				oSelVisual = (*pState).oSelVisual
				if (OBJ_VALID(oSelVisual) ne 0) then begin
					oSelVisual->SetProperty, /HIDE
					(*pState).oSelVisual = OBJ_NEW()
				endif

				(*pState).oWindow->Draw, (*pState).oView
			endif
		end
;=======================================begin insert==========================		
			'Curve': begin
; 			Freehand ROI tool selected.
			WIDGET_CONTROL, event.top, GET_UVALUE=pState
			if (*pState).mode ne 'CURVE DRAW' then begin
				(*pState).mode = 'CURVE DRAW'

;				 Disable old selection visual, if any.
				oSelVisual = (*pState).oSelVisual
				if (OBJ_VALID(oSelVisual) ne 0) then begin
					oSelVisual->SetProperty, /HIDE
					(*pState).oSelVisual = OBJ_NEW()
				endif

				(*pState).oWindow->Draw, (*pState).oView
			endif
		end
			'Line': begin
; 			Segmented ROI tool selected.
			WIDGET_CONTROL, event.top, GET_UVALUE=pState
			if (*pState).mode ne 'LINE DRAW' then begin
				(*pState).mode = 'LINE DRAW'

;				Disable old selection visual, if any.
				oSelVisual = (*pState).oSelVisual
				if (OBJ_VALID(oSelVisual) ne 0) then begin
					oSelVisual->SetProperty, /HIDE
					(*pState).oSelVisual = OBJ_NEW()
				endif

				(*pState).oWindow->Draw, (*pState).oView
			endif
		end
;=======================================end insert==========================		
		'SEGPOLY': begin
; 			Segmented ROI tool selected.
			WIDGET_CONTROL, event.top, GET_UVALUE=pState
			if (*pState).mode ne 'POLYGON DRAW' then begin
				(*pState).mode = 'POLYGON DRAW'

;				Disable old selection visual, if any.
				oSelVisual = (*pState).oSelVisual
				if (OBJ_VALID(oSelVisual) ne 0) then begin
					oSelVisual->SetProperty, /HIDE
					(*pState).oSelVisual = OBJ_NEW()
				endif

				(*pState).oWindow->Draw, (*pState).oView
			endif
		end

		'PICK': begin
;			Pick tool selected.
			WIDGET_CONTROL, event.top, GET_UVALUE=pState
			if (*pState).mode ne 'SELECTION' then begin
				(*pState).mode = 'SELECTION'

;				Disable old selection visual, if any.
				oSelVisual = (*pState).oSelVisual
				if (OBJ_VALID(oSelVisual) ne 0) then begin
					oSelVisual->SetProperty, /HIDE
					(*pState).oSelVisual = OBJ_NEW()
				endif

;				Set the vertex picking visual as the current selection visual.
;				(*pState).oSelVisual = (*pState).oPickVisual
				roi__ReshapeSelectionVisual, pState, (*pState).oSelROI

				(*pState).oWindow->Draw, (*pState).oView
			endif
		end
		ELSE:
	ENDCASE
End

;-----------------------------------------------------------------
Pro CreatAnimationBase, pState
COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  AnimationBase = Widget_Base( GROUP_LEADER=(*pState).MainBase, UNAME='AnimationBase'  $
      ,XOFFSET=100 ,YOFFSET=650 ,TITLE='Animation' ,tlb_frame_attr=9,/context_events)

  WID_BASE_1 = Widget_Base(AnimationBase, FRAME=1 ,XOFFSET=10 ,YOFFSET=3 ,SPACE=3 ,XPAD=3 ,YPAD=3)
  ChannelList = Widget_Droplist(WID_BASE_1, UNAME='ChannelList' ,YOFFSET=2 ,SCR_XSIZE=55 ,SCR_YSIZE=22  $
      ,EVENT_PRO='OnChangeAnimationChannel' ,TITLE='Ch' ,VALUE=string(indgen((*pState).stackNum)+1,format='(i0)'))
  WID_LABEL_1 = Widget_Label(WID_BASE_1, UNAME='WID_LABEL_1'  $
      ,XOFFSET=70 ,YOFFSET=6 ,/ALIGN_LEFT ,VALUE='FPS')
  FPSSlider = Widget_Slider(WID_BASE_1, UNAME='FPSSlider' ,XOFFSET=95 ,YOFFSET=3 $
      ,SCR_XSIZE=30 ,SCR_YSIZE=20 ,EVENT_PRO='OnChangeFPS' ,/VERTICAL ,MINIMUM=6 ,MAXIMUM=12)
  WID_BASE_2 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_2' ,XOFFSET=140 ,TITLE='IDL' ,ROW=1)
  Play = Widget_Button(WID_BASE_2, UNAME='Play' ,/ALIGN_CENTER ,TOOLTIP='Play' $
      ,VALUE='images\Play' ,/BITMAP,EVENT_PRO='OnPlay')
  Stop = Widget_Button(WID_BASE_2, UNAME='Stop' ,XOFFSET=35 ,/ALIGN_CENTER ,$
      TOOLTIP='Stop' ,VALUE='images\Stop' ,/BITMAP,EVENT_PRO='OnStop')
  EportImage = Widget_Button(WID_BASE_2, UNAME='EportImage' ,XOFFSET=72 ,/ALIGN_CENTER $
      ,TOOLTIP='Export Image' ,VALUE='images\ExportImage' ,/BITMAP,EVENT_PRO='OnExportImage')
  HideROI = Widget_Button(WID_BASE_2, UNAME='HideROI' ,/ALIGN_CENTER ,accelerator="F3"$
      ,TOOLTIP='Hide ROI' ,VALUE='images\ShowROI' ,/BITMAP,EVENT_PRO='OnHideROI')
  HideLabel = Widget_Button(WID_BASE_2, UNAME='HideLabel' ,/ALIGN_CENTER ,accelerator="F4"$
      ,TOOLTIP='Hide Label' ,VALUE='images\ShowLabel' ,/BITMAP,EVENT_PRO='OnHideLabel')
  SemiAutoDetection = Widget_Button(WID_BASE_2, UNAME='SemiAutoDetection' ,/ALIGN_CENTER $
      ,TOOLTIP='SemiAutoDetection' ,VALUE='images\Autodetection' ,/BITMAP,EVENT_PRO='OnSemiAutoDetection')
  Command_Animation = Widget_Button(WID_BASE_2, UNAME='Command_Animation' ,/ALIGN_CENTER $
      ,TOOLTIP='Command' ,VALUE='images\Command' ,/BITMAP,EVENT_PRO='OnCommandAnimation')

  WID_BASE_3 = Widget_Base(AnimationBase,/row,xoffset=0,yoffset=35,xpad=10,ypad=5,/align_top)
  ImageWindow = Widget_Draw(WID_BASE_3, UNAME='ImageWindow', frame=1, UVALUE='DRAW' $
      ,SCR_XSIZE=(*pState).xs ,SCR_YSIZE=(*pState).ys ,classname='IDLitWindow' $
      ;,EVENT_PRO='OnImageWindowEvent'
      ,RETAIN=2 ,GRAPHICS_LEVEL=2,/button_events,/motion_events,/keyboard_events)

  FrameSlider = Widget_Slider(WID_BASE_3, UNAME='FrameSlider' ,SCR_XSIZE=35 ,SCR_YSIZE=300<(*pState).ys $
      ,EVENT_PRO='OnChangeFrame' ,/VERTICAL ,VALUE=0, /Drag,sensitive=(*pState).ts gt 1)
  if (*pState).ts gt 1 then widget_control,FrameSlider,set_slider_min=0,set_slider_max=(*pState).ts-1

  WID_BASE_4 = Widget_Base(WID_BASE_3 ,FRAME=1 ,XOFFSET=950 ,YOFFSET=44 ,COLUMN=1)
  ROISlider = Widget_Slider(WID_BASE_4, UNAME='ROISlider' ,SCR_XSIZE=24 $
      ,SCR_YSIZE=50 ,TITLE='ROI',EVENT_PRO='OnChangeROI',MINIMUM=0,MAXIMUM=1,sensitive=0)
  DeleteROI = Widget_Button(WID_BASE_4, UNAME='DeleteROI' ,/ALIGN_CENTER,accelerator="Del" $
      ,TOOLTIP='Delete ROI' ,VALUE='images\DeleteROI' ,/BITMAP,EVENT_PRO='OnDeleteROI')
;---
  DeleteChosenROI = Widget_Button(WID_BASE_4, UNAME='DeleteROI' ,/ALIGN_CENTER, $accelerator="Del" $
      TOOLTIP='Delete Chosen ROIs' ,VALUE='images\DeleteChosenROIs' ,/BITMAP,EVENT_PRO='OnDeleteChosenROIs')
;---
  DeleteManualROI = Widget_Button(WID_BASE_4, UNAME='DeleteManualROI' ,/ALIGN_CENTER $
      ,TOOLTIP='Delete All Manual ROI' ,VALUE='images\DeleteManualROI' ,/BITMAP,EVENT_PRO='OnDeleteManualROI')
  DeleteAutoROI = Widget_Button(WID_BASE_4, UNAME='DeleteAutoROI' ,/ALIGN_CENTER $
      ,TOOLTIP='Delete ALL Auto ROI' ,VALUE='images\DeleteAutoROI' ,/BITMAP,EVENT_PRO='OnDeleteAutoROI')
  AnalyzeROI_Manual = Widget_Button(WID_BASE_4, UNAME='AnalyzeROI' ,/ALIGN_CENTER $
      ,TOOLTIP='Analyze ROI Manually' ,VALUE='images\ManualAnalyzeROI' ,/BITMAP,EVENT_PRO='OnAnalyzeROI_Manual')
  AnalyzeROI = Widget_Button(WID_BASE_4, UNAME='AnalyzeROI' ,/ALIGN_CENTER $
      ,TOOLTIP='Analyze Flash' ,VALUE='images\AnalyzeROI' ,/BITMAP,EVENT_PRO='OnAnalyzeROI')
  AnalyzeAllROI = Widget_Button(WID_BASE_4, UNAME='AnalyzeAllROI' ,/ALIGN_CENTER $
      ,TOOLTIP='Analyze All Flash' ,VALUE='images\AnalyzeAllROI' ,/BITMAP,EVENT_PRO='OnAnalyzeAllROI')

	Separator = Widget_Label(WID_BASE_2,xsize=1,ysize=20,scr_xsize=8,scr_ysize=20, value='')
	Separator = Widget_Text(WID_BASE_2,xsize=1,ysize=20,scr_xsize=4,scr_ysize=20)
	Separator = Widget_Label(WID_BASE_2,xsize=1,ysize=20,scr_xsize=8,scr_ysize=20, value='')

	_tools = STRUPCASE(['Polygon Draw', 'Rectangle Draw', 'Freehand Draw','Line Draw','Curve Draw','Translate-Scale'])
	wExcToolbarBase = WIDGET_BASE(WID_BASE_2, /ROW, $
		/EXCLUSIVE, SPACE=0, /TOOLBAR)


	wSegPoly = WIDGET_BUTTON(wExcToolbarBase, VALUE='images\segpoly', $
		/BITMAP, TOOLTIP='Draw Polygon ROIs', UNAME='polygon_mode', $
		UVALUE='SEGPOLY', accelerator="ctrl+1")
	wRectangle = WIDGET_BUTTON(wExcToolbarBase, VALUE='images\rectangl.bmp', $
		/BITMAP, TOOLTIP='Draw Rectangle ROIs', UNAME='rectangle_mode', $
		UVALUE='RECTANGLE', accelerator="ctrl+2")
	wEllipse = WIDGET_BUTTON(wExcToolbarBase, VALUE='images\ellipse.bmp', $
		/BITMAP, TOOLTIP='Draw Ellipse ROIs', UNAME='ellipse_mode', $
		UVALUE='ELLIPSE', accelerator="ctrl+3")

	wFreePoly = WIDGET_BUTTON(wExcToolbarBase, VALUE='images\freepoly', $
		/BITMAP, TOOLTIP='Draw Freehand ROIs', UNAME='freehand_mode', $
		UVALUE='FREEPOLY', accelerator="ctrl+4")		
		
	wTransScale = WIDGET_BUTTON(wExcToolbarBase, VALUE='images\arrow', $
		/BITMAP, TOOLTIP='Translate/Scale ROIs', UNAME='translate-scale_mode', $
		UVALUE='TRANSLATE-SCALE', accelerator="ctrl+w")
	wPick = WIDGET_BUTTON(wExcToolbarBase, VALUE='images\select.bmp', $
		/BITMAP, TOOLTIP='Select ROI', UNAME='selection_mode', $
		UVALUE='PICK', accelerator="ctrl+q")
		
	wline = WIDGET_BUTTON(wExcToolbarBase, VALUE='Line', $
		 TOOLTIP='Draw Line', UNAME='line_mode', $
		UVALUE='Line', accelerator="ctrl+5")

	wcurve = WIDGET_BUTTON(wExcToolbarBase, VALUE='Curve', $
		 TOOLTIP='Draw Curve', UNAME='curve_mode', $
		UVALUE='Curve', accelerator="ctrl+6")		
				
	WIDGET_CONTROL,  WIDGET_INFO(wExcToolbarBase, /CHILD), /SET_BUTTON



  ContextBase1 = Widget_Base(AnimationBase, UNAME='ContextBase1', row=2, xpad=0, ypad=0, space=1, $
      /Context_menu, SCR_XSIZE=232 ,SCR_YSIZE=136)
  M_BUTTON6 = Widget_Button(ContextBase1, /ALIGN_CENTER ,VALUE='Export Averaged Image', event_pro='OnExportAveragedImage')
  M_BUTTON5 = Widget_Button(ContextBase1, /ALIGN_CENTER ,VALUE='Export 2.5D ROI', event_pro='OnExport3DROISingle')
  M_BUTTON4 = Widget_Button(ContextBase1, /ALIGN_CENTER ,VALUE='Export ROI Sequence', event_pro='OnExportROISequence')
  M_BUTTON8 = Widget_Button(ContextBase1, /ALIGN_CENTER ,VALUE='Export Trace Iplot', event_pro='OnExportTraceToIplot')  
  M_BUTTON7 = Widget_Button(ContextBase1, /ALIGN_CENTER ,VALUE='Export All ROI', event_pro='OnExportAllROI')
  M_BUTTON3 = Widget_Button(ContextBase1, /ALIGN_CENTER ,VALUE='Export DeadMap', event_pro='OnExportDeadMap')
  M_BUTTON1 = Widget_Button(ContextBase1, /ALIGN_CENTER ,/MENU ,VALUE='Export Single')
  M_BUTTON2 = Widget_Button(ContextBase1, /ALIGN_CENTER ,/MENU ,VALUE='Export Sequence')
  button1 = widget_button(M_BUTTON1, value='With ROI', event_pro='OnExportSingleWithROI')
  button2 = widget_button(M_BUTTON1, value='Without ROI', event_pro='OnExportSingleNoROI')
  button3 = widget_button(M_BUTTON2, value='With ROI', event_pro='OnExportSeqWithROI')
  button4 = widget_button(M_BUTTON2, value='Without ROI', event_pro='OnExportSeqWithROI')

	case _tools[0] of
		'TRANSLATE-SCALE':$
			; If user provided regions, explain how to translate/scale.
			value = $
				'Click left mouse to select an ROI; drag to translate/scale/rotate.'

        'FREEHAND DRAW': value = $
            'Click and drag left mouse to draw freehand ROI.'

        'POLYGON DRAW': begin
        	; Determine if the user provided regions.
;			haveRegions = 0
;			if (N_ELEMENTS(regions_in) gt 0) then begin
;				if SIZE(regions_in, /TNAME) eq 'OBJREF' then begin
;					if OBJ_VALID(regions_in[0]) then $
;						haveRegions = 1
;				endif
;			endif
;
;			if (haveRegions ne 0) then begin
				value = $
					'Click mouse to draw segmented ROI; double click to finish.'
;			endif else begin
;				; Otherwise, advise selection of a drawing tool.
;				value = $
;					'Choose a tool to draw an ROI.'
;			endelse
		end
        'SELECTION': value = $
            'Click left mouse to pick region & mark nearest vertex.'
    endcase


;	value = 'Welcome'
	wStatusBase = WIDGET_BASE(AnimationBase, /column, XOFFSET=40, YOFFSET=60+(*pState).ys)
	wStatus = WIDGET_LABEL( $
		wStatusBase, $
		VALUE=value, $
		/DYNAMIC_RESIZE, $
		/ALIGN_LEFT $
		)
	wStatus1 = WIDGET_LABEL( $
		wStatusBase, $
		VALUE='Draw ROI: Ctrl+1~4;  Pick: Ctrl+q;  Reshape: Ctrl+w', $
		/DYNAMIC_RESIZE, $
		/ALIGN_LEFT $
		)
	(*pState).wStatus = wStatus
	(*pState).mode = _tools[0]




  widget_control, AnimationBase, /realize
  xmanager, 'CreatAnimationBase', AnimationBase, /no_block

  ; Initialize animation interface
  init ={center: [0,0], x:0, y:0,IsOption:0b}
  *(*pState).init = init
  old_channel = 0
  widget_control, ChannelList, set_uvalue=old_channel
  (*pState).AnimationBase = AnimationBase
  (*pState).ContextBase1 = ContextBase1
  widget_control, ImageWindow, get_value = oWindow
  (*pState).oWindow = oWindow
  (*pState).oView->SetProperty, viewplane_rect=[0,0,(*pState).xs,(*pState).ys]
  if obj_valid((*pState).result) then begin
    ROINum = (*pState).result->length()
    if ROINum gt 0 then begin
      widget_control, ROISlider, set_slider_min=1, set_slider_max=ROINum,set_value=1, sensitive=1
    endif
  endif

  ; Add ManualROIModel to view
  (*pState).oManualROIModel->GetProperty, parent=oParent
  if ~obj_valid(oParent) then $
    (*pState).oView->Add, (*pState).oManualROIModel $
  else if oParent ne (*pState).oView then begin
    oParent->Remove, ( *pState).oManualROIModel
    (*pState).oView->Add, ( *pState).oManualROIModel
    obj_destroy, oParent
  endif
  
  ; Add oLineScan to view
  (*pState).oLineScan->GetProperty, parent=oParent
  if ~obj_valid(oParent) then $
    (*pState).oView->Add, (*pState).oLineScan $
  else if oParent ne (*pState).oView then begin
    oParent->Remove, ( *pState).oLineScan
    (*pState).oView->Add, ( *pState).oLineScan
    obj_destroy, oParent
  endif
  
  ; Add labelModel to view
  (*pState).oLabelModel->GetProperty, parent=oParent
  if ~obj_valid(oParent) then $
    (*pState).oView->Add, (*pState).oLabelModel $
  else if oParent ne (*pState).oView then begin
    oParent->Remove, ( *pState).oLabelModel
    (*pState).oView->Add, ( *pState).oLabelModel
    obj_destroy, oParent
  endif

  ; Add PickVisual and ReshapeVisual to view
;    ; Create a selection visual for vertex picking.
;    (*pState).oPickVisual = roi__CreatePickVisual()
;    (*pState).oPickVisual->GetProperty, parent=oParent
;    if ~OBJ_VALID(oParent) THEN $
;		(*pState).oView->Add, (*pState).oPickVisual $
;	else if oParent ne (*pState).oView then begin
;		oParent->Remove, ( *pState).oPickVisual
;		(*pState).oView->Add, ( *pState).oPickVisual
;		obj_destroy, oParent
;	endif

    ; Create a translate/scale model to be used as a selection visual.
    (*pState).oTransScaleVisual = $
    	roi__CreateTransScaleVisual(COLOR=(*pState).sel_rgb)
    (*pState).oTransScaleVisual->GetProperty, parent=oParent
    if ~OBJ_VALID(oParent) THEN $
		(*pState).oView->Add, (*pState).oTransScaleVisual $
	else if oParent ne (*pState).oView then begin
		oParent->Remove, ( *pState).oTransScaleVisual
		(*pState).oView->Add, (*pState).oTransScaleVisual
		obj_destroy, oParent
	endif

  ; Add AutoROIModel to view
  if obj_valid((*pState).oAutoROIModel) then begin
    (*pState).oAutoROIModel->GetProperty, parent=oParent
    if ~obj_valid(oParent) then $
      (*pState).oView->Add, (*pState).oAutoROIModel $
    else if oParent ne (*pState).oView then begin
      oParent->Remove, ( *pState).oAutoROIModel
      (*pState).oView->Add, ( *pState).oAutoROIModel
      obj_destroy, oParent
    endif
  endif

  (*pState).oWindow->SetProperty, Graphics_Tree = (*pState).oView

  widget_control, AnimationBase, set_uvalue=pState
; xmanager, 'CreatAnimationModel', AnimationBase, /no_block,event_handler='OnBaseTimer'

End