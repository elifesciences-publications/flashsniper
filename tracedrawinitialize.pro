pro TraceDrawInitialize, TraceDraw
;COMPILE_OPT STRICTARR
;  CATCH, Error_status
;  IF Error_status NE 0 THEN BEGIN
;    PRINT,strtrim(ROINo), '  Error message: ', !ERROR_STATE.MSG
;    return
;    CATCH, /CANCEL
;  ENDIF

  widget_control, TraceDraw, get_uvalue=pixWin, get_value=TraceWindow
  wset,TraceWindow & wshow,TraceWindow
  newPic = congrid(pixWin.pic,3,!D.x_size,!D.y_size)
;  print,!D.x_size,!D.y_size
  tv, newPic, true=1
  wset, pixWin.id
  device, copy=[0,0,!D.x_size,!D.y_size,0,0,TraceWindow]
  pixWin.tag = 0b
  widget_control, TraceDraw, set_uvalue=pixWin
end