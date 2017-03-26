pro GetValidFrame_event, Event
COMPILE_OPT STRICTARR

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)
  widget_control, event.top, get_uvalue=gs

  wWidget =  Event.top

  case wTarget of
    widget_info(wWidget, find_by_uname='FrameTable'): begin
      if event.type eq 4 then begin
      if event.sel_left ne -1 then begin
;        print, event.sel_top, event.sel_bottom, event.sel_left, event.sel_right
        frame = event.sel_top
        widget_control, widget_info((*gs.pState).AnimationBase,find_by_uname='FrameSlider'),set_value=frame
        (*gs.pState).oObserver->OnSlider, (*gs.pState).oWindow, frame
      endif
      endif
    end
  widget_info(wWidget, find_by_uname='SetStart'): begin
    select = widget_info(gs.frameTable,/table_select)
    if select[1] gt gs.endFrame then begin
      void = dialog_message('Start frame must be less than end frame!',  /error)
      return
    endif else begin
      gs.startFrame = select[1]
      if select[1] gt 0 then begin
        widget_control, gs.frameTable, use_table_select=[1,0,1,select[1]-1] $
          , set_value= replicate('No',1,select[1])
      endif
    endelse 
  end
  widget_info(wWidget, find_by_uname='SetEnd'): begin
    select = widget_info(gs.frameTable,/table_select)
    if select[3] lt gs.startFrame then begin
      void = dialog_message('End frame must be larger than start frame!',  /error)
    endif else begin
      gs.endFrame = select[3]
      if select[3] lt (*gs.pState).ts-1 then begin
        widget_control, gs.frameTable, use_table_select=[1,select[3]+1,1,(*gs.pState).ts-1] $
          , set_value= replicate('No',1,select[1])
      endif
    endelse
  end  

  widget_info(wWidget, find_by_uname='SetInvalid'): begin
    select = widget_info(gs.frameTable,/table_select)
    if select[1] gt gs.startFrame and select[3] lt gs.endFrame then begin
      widget_control, gs.frameTable, use_table_select=[1,select[1],1,select[3]] $
        , set_value= replicate('No',1,select[3]-select[1]+1)
    endif else begin
      void = dialog_message('Selected frames must be larger than start and less than end!', /error)
    endelse
  end
  widget_info(wWidget, find_by_uname='SetValid'): begin
    select = widget_info(gs.frameTable,/table_select)
    if select[1] gt gs.startFrame and select[3] lt gs.endFrame then begin
      widget_control, gs.frameTable, use_table_select=[1,select[1],1,select[3]] $
        , set_value= replicate('Yes',1,select[3]-select[1]+1)
    endif else begin
      void = dialog_message('Selected frames must be larger than start and less than end!', /error)
    endelse
  end
  widget_info(wWidget, find_by_uname='Reset'): begin   
    gs.startFrame = 0
    gs.endFrame = (*gs.pState).ts-1
    widget_control, gs.frameTable, use_table_select=[1,0,1,(*gs.pState).ts-1] $
      , set_value= replicate('Yes',1,(*gs.pState).ts)
  end
  widget_info(wWidget, find_by_uname='Done'): begin
    widget_control, gs.frameTable, use_table_select=[1,0,1,(*gs.pState).ts-1], get_value=valid
    *(*gs.pState).validFrame = where(valid eq 'Yes')
    widget_control, event.top, /destroy
    return
  end
    else:
  endcase
  
  widget_control, event.top, set_uvalue=gs
end

; ----------------------------------------------------------------------------
pro GetValidFrame, pState, GROUP_LEADER=wGroup
COMPILE_OPT STRICTARR

  ; pState, startFrame, endFrame, idFrameTable
  WID_BASE_0 = Widget_Base( GROUP_LEADER=wGroup ,XOFFSET=5 ,YOFFSET=200, /modal $
      ,SCR_XSIZE=300 ,SCR_YSIZE=320 ,TITLE='Set Valid Frame' ,SPACE=3 ,XPAD=3 ,YPAD=3)
  
  FrameTable = Widget_Table(WID_BASE_0, UNAME='FrameTable' ,FRAME=1  $
      ,XOFFSET=20 ,YOFFSET=20 ,/NO_ROW_HEADERS ,XSIZE=2 ,YSIZE=(*pState).ts, /All_Events  $
      ,X_SCROLL_SIZE=2 ,Y_SCROLL_SIZE=12,COLUMN_LABELS=['Frame','Valid'])
  
  WID_BASE_1 = Widget_Base(WID_BASE_0, UNAME='WID_BASE_1'  $
      ,XOFFSET=190 ,YOFFSET=20 ,TITLE='IDL' ,SPACE=15 ,COLUMN=1)  
  SetStart = Widget_Button(WID_BASE_1, UNAME='SetStart' ,SCR_XSIZE=75  $
      ,SCR_YSIZE=25 ,/ALIGN_CENTER ,VALUE='Set As Start')  
  SetEnd = Widget_Button(WID_BASE_1, UNAME='SetEnd' ,YOFFSET=40  $
      ,SCR_XSIZE=75 ,SCR_YSIZE=25 ,/ALIGN_CENTER ,VALUE='Set As End')  
  SetInvalid = Widget_Button(WID_BASE_1, UNAME='SetInvalid'  $
      ,YOFFSET=80 ,SCR_XSIZE=75 ,SCR_YSIZE=25 ,/ALIGN_CENTER  $
      ,VALUE='Set As Invalid')  
  SetValid = Widget_Button(WID_BASE_1, UNAME='SetValid' ,YOFFSET=120  $
      ,SCR_XSIZE=75 ,SCR_YSIZE=25 ,/ALIGN_CENTER ,VALUE='SetAsValid')  
  Reset = Widget_Button(WID_BASE_1, UNAME='Reset' ,YOFFSET=160  $
      ,SCR_XSIZE=75 ,SCR_YSIZE=25 ,/ALIGN_CENTER ,VALUE='Reset')  
  Done = Widget_Button(WID_BASE_1, UNAME='Done' ,YOFFSET=200  $
      ,SCR_XSIZE=75 ,SCR_YSIZE=25 ,/ALIGN_CENTER ,VALUE='Done')

  Widget_Control, /REALIZE, WID_BASE_0
  widget_control, frameTable, use_table_select=[0,0,0,(*pState).ts-1], set_value= indgen(1,(*pState).ts)
  widget_control, frameTable, use_table_select=[1,0,1,(*pState).ts-1], set_value= replicate('Yes',1,(*pState).ts)
  gs = {pState: pState, startFrame:0, endFrame: (*pState).ts-1, frameTable: frameTable}
  widget_control, wid_base_0, set_uvalue=gs
  
  XManager, 'GetValidFrame', WID_BASE_0, /NO_BLOCK 
   
end
