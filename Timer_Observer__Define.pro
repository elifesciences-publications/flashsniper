; Create custom timer behavior object.

; Initialize class.
FUNCTION timer_observer::init
    self.inc = 1
    RETURN, 1
END

; Obligatory cleanup.
PRO timer_observer::cleanup
END

; OnTimer handles notifications that a timer even has occurred
; in the window. Updates currentImage with the increment amount.
; The nImages variable is based on therrr number of image frames.
; The ACTIVE_POSITION defines the zero-based index number of the
; model container item to be drawn
PRO timer_observer::OnTimer, oWin
    nImages = self.oImages->Count()
    self.currentImage += self.inc
    IF self.currentImage GE nImages THEN self.currentImage = 0
    IF self.currentImage LT 0 THEN self.currentImage = nImages-1
    self.oImages->SetProperty, ACTIVE_POSITION=self.currentImage
    if widget_info(self.wFrameIndicator,/valid_id) then widget_control,self.wFrameIndicator,set_value=self.currentImage

    ; Draw the model containing proper frame in the window.
    oWin->Draw
END

; Object SetProperty method. OIMAGES is the animation model contents
PRO timer_observer::SetProperty, $
    CURRENT_IMAGE = currentImage, $
    INCREMENT = inc, $
    OIMAGES = oImages, $
    idFrameSlider = idFrameSlider, $
    HIDE = HIDE

    IF N_ELEMENTS(currentImage) GT 0 THEN $
        self.currentImage = currentImage
    IF N_ELEMENTS(inc) GT 0 THEN $
        self.inc = inc
    IF N_ELEMENTS(oImages) GT 0 THEN $
        self.oImages = oImages
    IF N_ELEMENTS(idFrameSlider) GT 0 THEN $
        self.wFrameIndicator = idFrameSlider
    IF N_ELEMENTS(HIDE) GE 1 THEN $
    	self.oImages->SetProperty, HIDE=1
    IF N_ELEMENTS(HIDE) GE 0 THEN $
    	self.oImages->SetProperty, HIDE=0
END

; Object GetProperty method.
PRO timer_observer::GetProperty, $
    CURRENT_IMAGE = currentImage, $
    INCREMENT = inc, $
    OIMAGES = oImages, $
    idFrameSlider = idFrameSlider

    IF ARG_PRESENT(currentImage) THEN $
        currentImage = self.currentImage
    IF ARG_PRESENT(inc) THEN $
        inc = self.inc
	  IF ARG_PRESENT(oImages) THEN $
        oImages = self.oImages
    IF ARG_PRESENT(idFrameSlider) THEN $
        idFrameSlider = self.wFrameIndicator
END

PRO timer_observer::OnSlider, oWin, k
    nImages = self.oImages->Count()
    self.currentImage = k
    IF self.currentImage GE nImages THEN self.currentImage = 0
    IF self.currentImage LT 0 THEN self.currentImage = nImages-1
    self.oImages->SetProperty, ACTIVE_POSITION=self.currentImage
    oWin->Draw
END

; Define timer_observer instance data.
PRO timer_observer__define
    struct = { timer_observer, $
        currentImage: 0L, $
        inc: 0L, $
        oImages: OBJ_NEW(), $
        wFrameIndicator: 0L, $
        timerImages: 0L $
    }
end