;-----------------------------------------------------------------------------------------
Function AdjustContrast, img, Brightness, Contrast
COMPILE_OPT STRICTARR

  new_img = (double(img)-127)*Contrast+127+ brightness
  new_img = new_img > 0
  new_img = new_img < 255
  return, round(new_img)
End
;-----------------------------------------------------------------------------------------
Pro SetBrightness, event
COMPILE_OPT STRICTARR
  widget_control, event.top, get_uvalue=pL
  widget_control, (*pL).slider_c, get_value=contrast
  (*pL).imgrgb[1,*,*] = AdjustContrast((*pL).img,event.value,contrast/100.0)
  wset,(*pL).win & tv,(*pL).imgrgb,true=1
End
;-----------------------------------------------------------------------------------------
Pro SetContrast, event
COMPILE_OPT STRICTARR
  widget_control, event.top, get_uvalue=pL
  widget_control, (*pL).slider_b, get_value=brightness
  (*pL).imgrgb[1,*,*] = AdjustContrast((*pL).img,brightness,event.value/100.0)
  wset,(*pL).win & tv,(*pL).imgrgb, true=1
End
;-----------------------------------------------------------------------------------------
Pro ResetImage, event
COMPILE_OPT STRICTARR
  widget_control, event.top, get_uvalue=pL
  widget_control, (*pL).slider_b, set_value=(*pL).para[0]
  widget_control, (*pL).slider_c, set_value=(*pL).para[1]
  (*pL).imgrgb[1,*,*] = img
  wset,(*pL).win & tv,(*pL).imgrgb,true=1
End
;-----------------------------------------------------------------------------------------
Pro ReturnPara, event
COMPILE_OPT STRICTARR
  widget_control, event.top, get_uvalue=pL
  widget_control, (*pL).slider_b, get_value=brightness
  widget_control, (*pL).slider_c, get_value=contrast
  (*pL).para[0] = brightness
  (*pL).para[1] = contrast/100.0
  widget_control, event.top, /destroy
End
;-----------------------------------------------------------------------------------------
Function AdjustImage, image, para, group_leader=group_leader
COMPILE_OPT STRICTARR
	img = image
	dim = size(img,/dimensions)
  print,para
	if keyword_set(group_leader) then base = widget_base(row=6, title='Brightness and Contrast', tlb_frame_attr=9, group_leader=group_leader, /modal) $
	else base = widget_base(row=6, title='Brightness and Contrast', tlb_frame_attr=9)
	base1 = widget_base(base,column=2, space=20)
	slider_b = widget_slider(base1,title='Brightness', minimum=-100, maximum=200, value=para[0], /drag, $
					xsize=dim[0]/2-10, event_pro='SetBrightness')
	slider_c = widget_slider(base1,title='Contrast', minimum=0, maximum=500, value=para[1]*100, /drag, $
					xsize=dim[0]/2-10, event_pro='SetContrast')
	base2 = widget_base(base)
	draw = widget_draw(base2,xsize=dim[0],ysize=dim[1],scr_xsize=dim[0],scr_ysize=dim[1])
	base3 = widget_base(base, column=2, /align_center, /base_align_center)
	button1 = widget_button(base3, value='OK', event_pro='ReturnPara', xsize=100)
	button2 = widget_button(base3, value='Reset', event_pro='ResetImage', xsize=100)
	widget_control, base, /realize
	widget_control, draw, get_value=win

	imgrgb = bytarr(3,dim[0],dim[1])
	imgrgb[1,*,*] = img
	device, decomposed=1
	wset, win & tv, imgrgb, true=1
	
  localState = { img:img, $
      imgrgb:imgrgb, $
      slider_b:slider_b, $
      slider_c:slider_c, $
      win:win, $
      para:para }
  pL = ptr_new(localState,/no_copy)
  widget_control,base,set_uvalue=pL

	xmanager, 'base', base;, /no_block
	para = (*pL).para
	print,para
	return, para
End
