;; $ManuallyGetMask.PRO$
;
; Copyright @ Calcium signalling lab, IMM, PKU.
; All rights reserved.
; Unauthorized reproduction prohibited.
;+
; NAME:
;       ManuallyGetMask
;
; PURPOSE:
;       This function provides a interface for mask generation and modification.
;
; CATEGORY:
;       IMAGE PROCESSING.
;
; CALLING SEQUENCE:
;       Result = ManuallyGetMask(seed, mask=mask, zoom=zoom, group_Leader=group_Leader)
;
; INPUTS:
;       seed:    the seed image for generate mask, both gray scale image and true color image are acceptable.
;
; KEYWORDS:
;       mask:    the input keyword indicating previous definied mask
;
;       zoom:    the zoom factor for geometrical transformed seed image. NOT READY FOR USE RIGHT NOW!
;
;       group_leader:    the group leader of the sub interface.
;
; MODIFICATION HISTORY:
;       Written by:  Wen Xin and Li Kaitao, Feb 2009
;       Modified:

pro OnMaskDrawEvent, event
  widget_control, event.top, get_uvalue=pLocalState

  case event.type of
  0: begin
    widget_control,event.id,draw_motion_events=1
    case event.press of
    1: begin
      (*pLocalState).option = 'erase'
      x1 = ((event.x-(*pLocalState).r)*(*pLocalState).zoom[0] ge 0) ? (event.x-(*pLocalState).r)*(*pLocalState).zoom[0] : 0
      x2 = ((event.x+(*pLocalState).r)*(*pLocalState).zoom[0] lt (*pLocalState).xs) ? (event.x+(*pLocalState).r)*(*pLocalState).zoom[0] : (*pLocalState).xs-1
      y1 = ((event.y-(*pLocalState).r)*(*pLocalState).zoom[1] ge 0) ? (event.y-(*pLocalState).r)*(*pLocalState).zoom[1] : 0
      y2 = ((event.y+(*pLocalState).r)*(*pLocalState).zoom[1] lt (*pLocalState).ys) ? (event.y+(*pLocalState).r)*(*pLocalState).zoom[1] : (*pLocalState).ys-1
      (*pLocalState).mask[x1:x2, y1:y2] = 0
    end
    4: begin
      (*pLocalState).option = 'fill'
      x1 = ((event.x-(*pLocalState).r)*(*pLocalState).zoom[0] ge 0) ? (event.x-(*pLocalState).r)*(*pLocalState).zoom[0] : 0
      x2 = ((event.x+(*pLocalState).r)*(*pLocalState).zoom[0] lt (*pLocalState).xs) ? (event.x+(*pLocalState).r)*(*pLocalState).zoom[0] : (*pLocalState).xs-1
      y1 = ((event.y-(*pLocalState).r)*(*pLocalState).zoom[1] ge 0) ? (event.y-(*pLocalState).r)*(*pLocalState).zoom[1] : 0
      y2 = ((event.y+(*pLocalState).r)*(*pLocalState).zoom[1] lt (*pLocalState).ys) ? (event.y+(*pLocalState).r)*(*pLocalState).zoom[1] : (*pLocalState).ys-1
      (*pLocalState).mask[x1:x2, y1:y2] = 1
    end
    else: return
    endcase
  end
  1: begin
    widget_control,event.id,draw_motion_events=0
    (*pLocalState).option = ''
  end
  2: begin
    case (*pLocalState).option of
    'erase': begin
      x1 = ((event.x-(*pLocalState).r)*(*pLocalState).zoom[0] ge 0) ? (event.x-(*pLocalState).r)*(*pLocalState).zoom[0] : 0
      x2 = ((event.x+(*pLocalState).r)*(*pLocalState).zoom[0] lt (*pLocalState).xs) ? (event.x+(*pLocalState).r)*(*pLocalState).zoom[0] : (*pLocalState).xs-1
      y1 = ((event.y-(*pLocalState).r)*(*pLocalState).zoom[1] ge 0) ? (event.y-(*pLocalState).r)*(*pLocalState).zoom[1] : 0
      y2 = ((event.y+(*pLocalState).r)*(*pLocalState).zoom[1] lt (*pLocalState).ys) ? (event.y+(*pLocalState).r)*(*pLocalState).zoom[1] : (*pLocalState).ys-1
      (*pLocalState).mask[x1:x2, y1:y2] = 0
    end
    'fill': begin
      x1 = ((event.x-(*pLocalState).r)*(*pLocalState).zoom[0] ge 0) ? (event.x-(*pLocalState).r)*(*pLocalState).zoom[0] : 0
      x2 = ((event.x+(*pLocalState).r)*(*pLocalState).zoom[0] lt (*pLocalState).xs) ? (event.x+(*pLocalState).r)*(*pLocalState).zoom[0] : (*pLocalState).xs-1
      y1 = ((event.y-(*pLocalState).r)*(*pLocalState).zoom[1] ge 0) ? (event.y-(*pLocalState).r)*(*pLocalState).zoom[1] : 0
      y2 = ((event.y+(*pLocalState).r)*(*pLocalState).zoom[1] lt (*pLocalState).ys) ? (event.y+(*pLocalState).r)*(*pLocalState).zoom[1] : (*pLocalState).ys-1
      (*pLocalState).mask[x1:x2, y1:y2] = 1
    end
    else: return
    endcase
  end
  else: return
  endcase
  (*pLocalState).imgrgb[0,*,*] = (*pLocalState).mask*150
  (*pLocalState).oImg->SetProperty, data=(*pLocalState).imgrgb
  (*pLocalState).oWin->Draw, (*pLocalState).oView

end

;----------------------------------------------------------------
pro OnChangeThrSlider, event
  widget_control, event.top, get_uvalue=pLocalState
  (*pLocalState).mask = (*pLocalState).seed ge event.value
  (*pLocalState).mask = morph_close((*pLocalState).mask,replicate(1,3,3))
  (*pLocalState).mask = morph_open((*pLocalState).mask,replicate(1,3,3))
;  help,event,/structure
  (*pLocalState).imgrgb[0,*,*] = (*pLocalState).mask*150
  (*pLocalState).oImg->SetProperty, data=(*pLocalState).imgrgb
  (*pLocalState).oWin->Draw, (*pLocalState).oView
end
;--------------------------------------------------------------------
pro OnChangeRadius,event
  widget_control, event.top, get_uvalue=pLocalState,/no_copy
;  help,event,/structure
 (*pLocalState).r = event.value
  widget_control, event.top, set_uvalue=pLocalState,/no_copy
end
;-----------------------------------------------------------------------
pro OnFinishManuallyGetMask, event
  widget_control, event.top, /destroy
end
;----------------------------------------------------------------
Function ManuallyGetMask, seed, mask=mask, zoom=zoom, group_Leader=group_Leader

  dim = size(seed)
  case dim[0] of
  2:
  3: seed = reform(total(float(seed),1)/dim[1])
  else: begin
    void = dialog_message('This is not a valid image!',/error)
    return, -1
  endelse
  endcase
  xs = dim[1]
  ys = dim[2]
  thr = mean(seed)
  if n_elements(mask) gt 0 then mask = mask else mask = seed gt thr
  if n_elements(zoom) gt 0 then begin
    if n_elements(zoom) eq 2 then zoom = zoom $
    else begin
      void = dialog_message('Keyword "Zoom" should be a two elements array!',/error)
      return, -1
    endelse
  endif else zoom = [1,1]
  imgrgb = bytarr(3,xs,ys)
  imgrgb[0,*,*] = mask*150
  imgrgb[1,*,*] = bytscl(seed)

  mainBase = widget_base(/column,title='Generate Mask Mannually(Left: erase   Right: fill)',group_leader=group_leader,xoffset=70,yoffset=200)
  drawBase = widget_base(mainbase)
  buttonBase = widget_base(mainbase,/row,space=30)
  maskDraw = widget_draw(drawBase,scr_xs=xs,scr_ys=ys,classname='IDLgrWindow' $
    ,EVENT_PRO='OnMaskDrawEvent' ,RETAIN=2 ,GRAPHICS_LEVEL=2,/button_events)
  thrSlider = widget_slider(buttonbase,min=min(median(seed,3)),max=max(median(seed,3)), $
    xsize=150, ysize=40,xoffset=0,yoffset=0, title='Threshold', event_pro='OnChangeThrSlider',/drag,value=thr)
  radiusSlider = widget_slider(buttonbase,min=0,max=10, $
    xsize=150, ysize=40, title='Radius',event_pro='OnChangeRadius',value=8)
  doneButton = widget_button(buttonBase,value='Done',event_pro='OnFinishManuallyGetMask',xsize = 40,ysize = 5 )

  widget_control,radiusSlider,get_value=r
  widget_control,mainbase,/realize
  widget_control,maskDraw,get_value=owin
  localState = { mainBase: mainBase, $
        maskDraw: maskDraw, $
        thrSlider: thrSlider, $
        seed: seed, $
        imgrgb: imgrgb, $
        r:8,$
        mask: mask, $
        zoom: zoom, $
        xs: xs, $
        ys: ys, $
        option:'', $
        oWin: oWin, $
        oView: obj_new('IDLgrView',viewplane_rect=[0,0,xs,ys]), $
        oModel: obj_new('IDLgrModel'), $
        oimg: obj_new('IDLgrImage',data=imgrgb) }
  localState.oView->add, localState.omodel
  localState.oModel->add, localState.oImg
  localState.oWin->draw, localState.oView
  pLocalState = ptr_new(localState,/no_copy)

  widget_control,mainbase,set_uvalue=pLocalState
  xmanager, 'ManuallyGetMask', mainbase;,/no_block

  mask = (*pLocalState).mask
  obj_destroy,(*pLocalState).oWin
  obj_destroy, (*pLocalState).oView
  ptr_free, pLocalState
  return,uint(mask)
End