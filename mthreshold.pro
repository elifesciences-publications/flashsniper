pro mthreshold_event,event
		widget_control,event.top,get_uvalue = pstate
		thr = event.value
		widget_control,(*pstate).widget.slider,set_value = thr
		(*pstate).data.oimage->getproperty,data = zimage
		image = (*pstate).data.image
		zimage[0,*,*] = 200*(image ge thr)
		(*pstate).data.oimage->setproperty,data = zimage
		(*pstate).widget.owindow->draw,(*pstate).widget.oview
		(*pstate).param.threshold = thr
end

function mthreshold,image
	  device,decomp = 1
		sz = size(image,/dim)
		sz = float(sz)/2
		base = widget_base(/align_center,row = 2,tlb_frame_attr = 1)
		slider = widget_slider(base,xs = sz[0])
		draw = widget_draw(base,graph=2,retain = 2,xs = sz[0],ys = sz[1])
		widget_control,base,/realize
		widget_control,draw,get_value = owindow
		
	  oimage = obj_new('idlgrimage',image)
		oimage->getproperty,xr = xr,yr = yr		
		sz = [xr[1],yr[1]]
		zimage = bytarr(3,sz[0],sz[1])
		zimage[1,*,*] = bytscl(image)
		zm = 50
		minp = min(image*zm,max = maxp)
	  thr = getthreshold(image*zm)
	  zimage[0,*,*] = 200*(image*zm ge thr)
		oimage->setproperty,data = zimage
	  xm = [0,1/(sz[0]-1)]
	  ym = [0,1/(sz[1]-1)]
	  oimage->setproperty,xc = xm,yc = ym	  

		oview = obj_new('idlgrview',viewplane = [0,0,1,1],units = 3,color = [0,0,0])	  
		omodel = obj_new('idlgrmodel')
		omodel->add,oimage
		oview->add,omodel	  
		owindow->draw,oview		
	  
		data = {oimage:oimage,image:image*zm}
		widget = {owindow:owindow,oview:oview,slider:slider}
		param = {threshold:thr}
		state = {widget:widget,data:data,param:param}
		
		pstate = ptr_new(state,/no_copy)
	  widget_control,base,set_uvalue = pstate
	  widget_control,slider,set_slider_min = minp,set_slider_max = maxp,set_value = thr
;	  print,thr
	  xmanager,'mthreshold',base
	  thr = (*pstate).param.threshold
	  return,thr/zm
end