
Pro ChainNode::SetProperty, oROI=oROI, oLabel=oLabel, trace=trace, $
      ROIPara=ROIPara, flashNum=flashNum, flashPara=flashPara

  if keyword_set(oROI) then self.oROI = oROI
  if keyword_set(oLabel) then self.oLabel = oLabel
  if keyword_set(trace) then self.trace = trace
  if keyword_set(ROIPara) then self.ROIPara = ROIPara
  if keyword_set(flashNum) then self.flashNum = flashNum
  if keyword_set(flashPara) then self.flashPara = flashPara

End

Pro ChainNode__Define

  ChainNode = { ChainNode, oROI:obj_new(), oLabel:obj_new(), trace:ptrarr(2,/allocate_heap), $
      ROIPara:dblarr(2), flashNum:1, flashPara:ptr_new(dblarr(20),/allocate_heap), next:ptr_new()}
End

Function Chain::IsEmpty
	return, self.first eq ptr_new()
End

Function Chain::SearchROI, oROI, roiNo=roiNo
	roiNo = 0
	current = self.first
	while current ne ptr_new() do begin
		if (*current).oROI eq oROI then begin
			if keyword_set(roiNo) then roiNo = roiNo
			return, (*current).oROI
		endif
		roiNo++
		current = (*current).next
	endwhile
	return, -1
End

Function Chain::Length
	if self.first eq ptr_new() then return, 0 $
	else if self.first eq self.last then return, 1 $
	else begin
		current = self.first
		length = 0
		while (*current).next ne ptr_new() do begin
			length++
			current = (*current).next
		endwhile
		self.length = length+1
		return, length+1
	endelse
End

Function Chain::Insert, x
  if self.length eq 0 then begin
    self.first = ptr_new(x)
    self.last = self.first
  endif else begin
    x.next = ptr_new()
    (*self.last).next = ptr_new(x)
    self.last = (*self.last).next
  endelse
  self.length = self->length()
  return,1
End

Function Chain::Delete, k
	if k ge self.length or k lt 0 then begin
		print, 'Out of range!'
		return, -1
	endif
	if k eq 0 then begin
		if self.length eq 1 then begin
			x = self.first
			self.first = (self.last = ptr_new())
		endif else begin
			x = self.first
			self.first = (*x).next
		endelse
    obj_destroy, (*x).oROI
    obj_destroy, (*x).oLabel
    ptr_free, (*x).trace, (*x).flashPara
    ptr_free, x
		self.length--
		return, 1
	endif
	count = 0
	current = self.first
	while count lt k-1 do begin
		count++
		current = (*current).next
	endwhile
	x = (*current).next
	if k eq self->length()-1 then begin
		(*current).next = ptr_new()
		self.last = current
	endif else (*current).next = (*x).next
	obj_destroy, (*x).oROI
	obj_destroy, (*x).oLabel
  ptr_free, (*x).trace, (*x).flashPara
	ptr_free,x
	self.length = self->length()
	return, 1
End

Function Chain::GetProperty, k, oROI=oROI, oLabel=oLabel, trace=trace, $
      ROIPara=ROIPara, flashNum=flashNum, flashPara=flashPara

	if  k ge self.length or k lt 0 then begin
		print, 'Out of range!'
		return, -1
	endif
	count = 0
	current = self.first
	while count lt k do begin
		count++
		current = (*current).next
	endwhile
	ROIPara = (*current).ROIPara
	flashPara = (*current).flashPara
	flashNum = (*current).flashNum
	oLabel = (*current).oLabel
	oROI = (*current).oROI
	trace = (*current).trace
	return, self.length
End

Function Chain::SetProperty, k, oROI=oROI, oLabel=oLabel, trace=trace, $
      ROIPara=ROIPara, flashNum=flashNum, flashPara=flashPara

		if self.length le k or k lt 0 then begin
			print, 'Out of range!'
			return, -1
		endif
		count = 0
		current = self.first
		while count lt k do begin
			count++
			current = (*current).next
		endwhile
  if keyword_set(oROI) then (*current).oROI = oROI
  if keyword_set(oLabel) then (*current).oLabel = oLabel
  if keyword_set(trace) then (*current).trace = trace
  if keyword_set(ROIPara) then (*current).ROIPara = ROIPara
  if keyword_set(flashNum) then (*current).flashNum = flashNum
  if keyword_set(flashPara) then (*current).flashPara = flashPara
  return, 1
End

Pro Chain::CleanUp

	while self.length gt 0 do begin
		if obj_valid((*self.last).oROI) then $
			obj_destroy, (*self.last).oROI
		info = self->Delete(self.length-1)
		self.length--
	endwhile
	ptr_free, self.last
	ptr_free, self.first
End

Pro Chain__define

	Chain = {Chain, length:0, first:ptr_new(), last:ptr_new()}
End