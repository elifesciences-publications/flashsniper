pro FlashSniper_event, Event
COMPILE_OPT STRICTARR

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)
  if wTarget eq event.top then begin
    if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_KILL_REQUEST' )then begin
      if dialog_message('Exit Flash Sniper?', /QUESTION) eq 'No' then return
;     WriteUserConfig, pState
      OnExit, Event
    endif
  endif
End

pro FlashSniper
COMPILE_OPT STRICTARR

  if XREGISTERED('FlashSniper',/noshow) ge 1 then begin
      r = dialog_message("Only one instance can run.", /error)
      return
  endif
;  Resolve_all
;  Resolve_Routine, 'IDLitNotifier__define',/COMPILE_FULL_FILE, /EITHER, /NO_RECOMPILE
;	filepath = routine_filepath('flashsniper')
;	dir = file_dirname(filepath)
;	cd,dir
  MainBase = Widget_Base(UNAME='MainBase' ,XOFFSET=5 ,YOFFSET=10 ,xsize=770 ,ysize=650, $
      tab_mode=1 ,TITLE='Flash Sniper' ,SPACE=3 ,XPAD=3 ,YPAD=3 $
      ,MBAR=MainBase_MBAR,/TLB_KILL_REQUEST_EVENTS)

  W_MENU_0 = Widget_Button(MainBase_MBAR, UNAME='W_MENU_0' ,/MENU ,VALUE='File')
  W_MENU_4 = Widget_Button(W_MENU_0, UNAME='W_MENU_4' ,VALUE='Open',EVENT_PRO='OnOpen')
  W_MENU_5 = Widget_Button(W_MENU_0, UNAME='W_MENU_5' ,VALUE='Load Status',EVENT_PRO='OnLoadStatus')
  W_MENU_6 = Widget_Button(W_MENU_0, UNAME='W_MENU_6' ,VALUE='Save Status',EVENT_PRO='OnSaveStatus')
  W_MENU_12 = Widget_Button(W_MENU_0, UNAME='W_MENU_12' ,/MENU ,VALUE='Import')
  W_MENU_131 = Widget_Button(W_MENU_12, UNAME='W_MENU_131' ,VALUE='ROI-New',EVENT_PRO='OnImportROINew')
  W_MENU_13 = Widget_Button(W_MENU_12, UNAME='W_MENU_13' ,VALUE='ROI',EVENT_PRO='OnImportROI')
  W_MENU_7 = Widget_Button(W_MENU_0, UNAME='W_MENU_7' ,/MENU ,VALUE='Export')
  W_MENU_141 = Widget_Button(W_MENU_7, UNAME='W_MENU_141' ,VALUE='ROI-New',EVENT_PRO='OnExportROINew')
  W_MENU_14 = Widget_Button(W_MENU_7, UNAME='W_MENU_14' ,VALUE='ROI',EVENT_PRO='OnExportROI')
  W_MENU_8 = Widget_Button(W_MENU_7, UNAME='W_MENU_8' ,VALUE='Result',EVENT_PRO='OnExportResult')
  W_MENU_9 = Widget_Button(W_MENU_7, UNAME='W_MENU_9' ,VALUE='Trace',EVENT_PRO='OnExportTrace')
  W_MENU_11 = Widget_Button(W_MENU_0, UNAME='W_MENU_11' ,/SEPARATOR ,VALUE='Exit',EVENT_PRO='OnExit')

  W_MENU_1 = Widget_Button(MainBase_MBAR, UNAME='W_MENU_1' ,/MENU ,VALUE='Edit')
  W_MENU_20 = Widget_Button(W_MENU_1, UNAME='W_MENU_20',/MENU, VALUE='Crop Image Stack')
  W_MENU_21 = Widget_Button(W_MENU_20, UNAME='W_MENU_21' ,VALUE='Spatial',EVENT_PRO='OnSpatialCrop')
  W_MENU_22 = Widget_Button(W_MENU_20, UNAME='W_MENU_22' ,VALUE='Temporal',EVENT_PRO='OnTemporalCrop')
  W_MENU_10 = Widget_Button(W_MENU_1, UNAME='W_MENU_10' ,VALUE='Get Mask',EVENT_PRO='OnGetMask')
  W_MENU_29 = Widget_Button(W_MENU_1, UNAME='W_MENU_29' ,VALUE='Edit Mask',EVENT_PRO='OnEditMask')
  W_MENU_12 = Widget_Button(W_MENU_1, UNAME='W_MENU_12' ,/MENU ,VALUE='Get Background From')
  W_MENU_13 = Widget_Button(W_MENU_12, UNAME='W_MENU_13' ,VALUE='Back Mask',EVENT_PRO='OnGetBackFromMask')
  W_MENU_14 = Widget_Button(W_MENU_12, UNAME='W_MENU_14' ,VALUE='ROI 1',EVENT_PRO='OnGetBackFromROI')
  W_MENU_23 = Widget_Button(W_MENU_1, UNAME='W_MENU_23' ,VALUE='SetPlotProperty',EVENT_PRO='OnSetPlotProperty')

  W_MENU_2 = Widget_Button(MainBase_MBAR, UNAME='W_MENU_2' ,/MENU ,VALUE='Tools')
  W_MENU_15 = Widget_Button(W_MENU_2, UNAME='W_MENU_15' ,VALUE='Adjust Contrast',EVENT_PRO='OnAdjustContrast')
  W_MENU_16 = Widget_Button(W_MENU_2, UNAME='W_MENU_16' ,VALUE='Motion Compensation',EVENT_PRO='OnMotionCompensation_ST')
  W_MENU_32 = Widget_Button(W_MENU_2, UNAME='W_MENU_32' ,VALUE='Channel Alignment',EVENT_PRO='OnChannelAlignment')
  W_MENU_30 = Widget_Button(W_MENU_2, UNAME='W_MENU_30' ,VALUE='Texture Alignment',EVENT_PRO='OnTextureAlignment')
  W_MENU_17 = Widget_Button(W_MENU_2, UNAME='W_MENU_17' ,VALUE='Photobleach Correction',EVENT_PRO='OnPhotobleachCorrection')
  W_MENU_24 = Widget_Button(W_MENU_2, UNAME='W_MENU_24' ,VALUE='Subtract Baseline',EVENT_PRO='OnSubtractBaseline')
  W_MENU_25 = Widget_Button(W_MENU_2, UNAME='W_MENU_25' ,VALUE='Calculate Feature',EVENT_PRO='OnCalculateFeature')
  W_MENU_18 = Widget_Button(W_MENU_2, UNAME='W_MENU_18' ,VALUE='Autodetection',EVENT_PRO='OnAutodetection')
  W_MENU_31 = Widget_Button(W_MENU_2, UNAME='W_MENU_31' ,VALUE='ShowFeatures',EVENT_PRO='OnShowFeatures')

  W_MENU_26 = Widget_Button(MainBase_MBAR, UNAME='W_MENU_4' ,/MENU ,VALUE='Extension',sensitive=1)
  W_MENU_27 = Widget_Button(W_MENU_26, UNAME='W_MENU_27' ,VALUE='Set Validation Parameters',EVENT_PRO='OnSetValidationParameters')
  W_MENU_28 = Widget_Button(W_MENU_26, UNAME='W_MENU_27' ,VALUE='Validation',EVENT_PRO='OnValidation')

  W_MENU_3 = Widget_Button(MainBase_MBAR, UNAME='W_MENU_3' ,/MENU ,VALUE='Help')
  W_MENU_19 = Widget_Button(W_MENU_3, UNAME='W_MENU_19' ,VALUE='About',EVENT_PRO='OnAbout')


  ToolbarBase1 = Widget_Base(MainBase, UNAME='ToolbarBase1' ,XOFFSET=10 ,YOFFSET=5 , $
  	  SCR_XSIZE=698 ,SCR_YSIZE=24 ,ROW=1 ,/toolbar ,space=0)
  Open = Widget_Button(ToolbarBase1, UNAME='Open' ,/ALIGN_CENTER  $
      ,TOOLTIP='Open' ,VALUE='images\Open' ,/BITMAP,EVENT_PRO='OnOpen')
  LoadStatus = Widget_Button(ToolbarBase1, UNAME='LoadStatus' ,XOFFSET=41 $
      ,/ALIGN_CENTER ,TOOLTIP='Load Status' ,VALUE='images\LoadStatus' ,/BITMAP,EVENT_PRO='OnLoadStatus')
  SaveStatus = Widget_Button(ToolbarBase1, UNAME='SaveStatus' ,XOFFSET=110 $
      ,/ALIGN_CENTER ,TOOLTIP='Save Status' ,VALUE='images\SaveStatus' ,/BITMAP,EVENT_PRO='OnSaveStatus')
  ExportResult = Widget_Button(ToolbarBase1, UNAME='ExportResult' ,XOFFSET=180 $
      ,/ALIGN_CENTER ,TOOLTIP='Export Result' ,VALUE='images\ExportResult' ,/BITMAP,EVENT_PRO='OnExportResult')
  BackUp = widget_button(ToolbarBase1, UNAME='BackUp' ,XOFFSET=630 ,/ALIGN_CENTER $
      ,TOOLTIP='Backup All' ,VALUE='images\Backup' ,/BITMAP,EVENT_PRO='OnBackup')

  Separator = Widget_Label(ToolbarBase1,xsize=1,ysize=20,scr_xsize=11,scr_ysize=20, value='')
  Separator = Widget_Text(ToolbarBase1,xsize=1,ysize=20,scr_xsize=4,scr_ysize=20)
  Separator = Widget_Label(ToolbarBase1,xsize=1,ysize=20,scr_xsize=11,scr_ysize=20, value='')

  SpatialCrop = Widget_Button(ToolbarBase1, UNAME='SpatiallCrop' ,XOFFSET=630 ,/ALIGN_CENTER $
      ,TOOLTIP='Crop Image Stack in xy Dimension' ,VALUE='images\SpatialCrop' ,/BITMAP,EVENT_PRO='OnSpatialCrop')
  TemporalCrop = Widget_Button(ToolbarBase1, UNAME='TemporalCrop' ,XOFFSET=630 ,/ALIGN_CENTER $
      ,TOOLTIP='Crop Image Stack in t Dimension' ,VALUE='images\TemporalCrop' ,/BITMAP,EVENT_PRO='OnTemporalCrop')
  MotionCompensation = Widget_Button(ToolbarBase1, UNAME='MotionCompensation' ,XOFFSET=301 ,/ALIGN_CENTER $
      ,TOOLTIP='Motion Compensation' ,VALUE='images\MotionCompensation' ,/BITMAP,EVENT_PRO='OnMotionCompensation_ST')
  TextureAlignment = Widget_Button(ToolbarBase1, UNAME='MotionCompensation' ,XOFFSET=301 ,/ALIGN_CENTER $
      ,TOOLTIP='Texture Alignment' ,VALUE='images\TextureAlignment' ,/BITMAP,EVENT_PRO='OnTextureAlignment')
  Filtration = Widget_Button(ToolbarBase1, UNAME='Filtration' ,XOFFSET=630 ,/ALIGN_CENTER $
      ,TOOLTIP='Perform Image Filtration' ,VALUE='images\Smooth' ,/BITMAP,EVENT_PRO='OnImageFiltration')
  GetMask = Widget_Button(ToolbarBase1, UNAME='GetMask' ,XOFFSET=630 ,/ALIGN_CENTER $
      ,TOOLTIP='Get Mask' ,VALUE='images\GetMask' ,/BITMAP,EVENT_PRO='OnGetMask')
  GetBackROI = Widget_Button(ToolbarBase1, UNAME='GetBackGroundFromROI' ,XOFFSET=415 ,/ALIGN_CENTER $
      ,TOOLTIP='Get Background From ROI 1' ,VALUE='images\GetBackGround' ,/BITMAP,EVENT_PRO='OnGetBackFromROI')
  GetBackMask = Widget_Button(ToolbarBase1, UNAME='GetBackGroundFromMask' ,XOFFSET=415 ,/ALIGN_CENTER $
      ,TOOLTIP='Get Background From Mask' ,VALUE='images\GetBackGround' ,/BITMAP,EVENT_PRO='OnGetBackFromMask')
  Contrast = Widget_Button(ToolbarBase1, UNAME='Contrast' ,XOFFSET=630 ,/ALIGN_CENTER $
      ,TOOLTIP='Adjust Contrast' ,VALUE='images\Brightness' ,/BITMAP,EVENT_PRO='OnAdjustContrast')
  PhotobleachCorrection = Widget_Button(ToolbarBase1, UNAME='PhotobleachCorrection' ,XOFFSET=507 ,/ALIGN_CENTER $
      ,TOOLTIP='Photobleach Correction' ,VALUE='images\PhotobleachCorrection' ,/BITMAP,EVENT_PRO='OnPhotobleachCorrection')

  Separator = Widget_Label(ToolbarBase1,xsize=1,ysize=20,scr_xsize=11,scr_ysize=20, value='')
  Separator = Widget_Text(ToolbarBase1,xsize=1,ysize=20,scr_xsize=4,scr_ysize=20)
  Separator = Widget_Label(ToolbarBase1,xsize=1,ysize=20,scr_xsize=11,scr_ysize=20, value='')

  Autodetection = Widget_Button(ToolbarBase1, UNAME='Autodetection' ,XOFFSET=630 ,/ALIGN_CENTER $
      ,TOOLTIP='Autodetect Flash' ,VALUE='images\Autodetection' ,/BITMAP,EVENT_PRO='OnAutodetection')
;  AnalyzeROI_Manual = Widget_Button(ToolbarBase1, UNAME='AnalyzeROI' ,/ALIGN_CENTER ,accelerator="F5"$
;      ,TOOLTIP='Analyze ROI Manually' ,VALUE='images\AnalyzeROI' ,/BITMAP,EVENT_PRO='OnAnalyzeROI_Manual')
;  AnalyzeAll = Widget_Button(ToolbarBase1, UNAME='AnalyzeAll' ,XOFFSET=630 ,/ALIGN_CENTER $
;      ,TOOLTIP='Analyze All Flash' ,VALUE='images\AnalyzeAll' ,/BITMAP,EVENT_PRO='OnAnalyzeAllROI')
  Capture = Widget_Button(ToolbarBase1, UNAME='CaptureTraceWindow' ,XOFFSET=630 ,/ALIGN_CENTER $
      ,TOOLTIP='Capture Trace Window' ,VALUE='images\ExportImage' ,/BITMAP,EVENT_PRO='OnCaptureTraceWindow')

;  Separator = Widget_Label(ToolbarBase1,xsize=1,ysize=20,scr_xsize=20,scr_ysize=20, value='')
;  Separator = Widget_Text(ToolbarBase1,xsize=1,ysize=20,scr_xsize=4,scr_ysize=20)
;  Separator = Widget_Label(ToolbarBase1,xsize=1,ysize=20,scr_xsize=2,scr_ysize=20, value='')

  Command = Widget_Button(ToolbarBase1, UNAME='Command' ,XOFFSET=630 ,/ALIGN_CENTER $
      ,TOOLTIP='Command' ,VALUE='images\Command' ,/BITMAP,EVENT_PRO='OnCommand')
  Batch = widget_button(ToolbarBase1, UNAME='Batch' ,XOFFSET=630 ,/ALIGN_CENTER $
      ,TOOLTIP='Batch Processing' ,VALUE='images\Batch' ,/BITMAP,EVENT_PRO='OnBatchProcessing')

;********************20091024forWarris*************
  Separator = Widget_Label(ToolbarBase1,xsize=1,ysize=20,scr_xsize=11,scr_ysize=20, value='')
  Separator = Widget_Text(ToolbarBase1,xsize=1,ysize=20,scr_xsize=4,scr_ysize=20)
  Separator = Widget_Label(ToolbarBase1,xsize=1,ysize=20,scr_xsize=51,scr_ysize=20, value='')
  Separator = Widget_Text(ToolbarBase1,xsize=1,ysize=20,scr_xsize=4,scr_ysize=20)
  Separator = Widget_Label(ToolbarBase1,xsize=1,ysize=20,scr_xsize=11,scr_ysize=20, value='')

  AnalyzeROI_Manual = Widget_Button(ToolbarBase1, UNAME='AnalyzeROI' ,/ALIGN_CENTER ,accelerator="F5"$
      ,TOOLTIP='Analyze ROI Manually' ,VALUE='images\ManualAnalyzeROI' ,/BITMAP,EVENT_PRO='OnAnalyzeROI_Manual')

  Separator = Widget_Label(ToolbarBase1,xsize=1,ysize=20,scr_xsize=8,scr_ysize=20, value='')
  Separator = Widget_Text(ToolbarBase1,xsize=1,ysize=20,scr_xsize=4,scr_ysize=20)
  Separator = Widget_Label(ToolbarBase1,xsize=1,ysize=20,scr_xsize=8,scr_ysize=20, value='')

  AnalyzeAllROI_Manual = Widget_Button(ToolbarBase1, UNAME='AnalyzeROI' ,/ALIGN_CENTER ,accelerator="F5"$
      ,TOOLTIP='Analyze All ROI Manually' ,VALUE='images\ManualAnalyzeAllROI' ,/BITMAP,EVENT_PRO='OnAnalyzeAllROI_Manual')

  Separator = Widget_Label(ToolbarBase1,xsize=1,ysize=20,scr_xsize=8,scr_ysize=20, value='')
  Separator = Widget_Text(ToolbarBase1,xsize=1,ysize=20,scr_xsize=4,scr_ysize=20)
  Separator = Widget_Label(ToolbarBase1,xsize=1,ysize=20,scr_xsize=8,scr_ysize=20, value='')

  AnalyzeROI = Widget_Button(ToolbarBase1, UNAME='AnalyzeAll' ,XOFFSET=630 ,/ALIGN_CENTER $
      ,TOOLTIP='Analyze Flash' ,VALUE='images\AnalyzeROI' ,/BITMAP,EVENT_PRO='OnAnalyzeROI')

  Separator = Widget_Label(ToolbarBase1,xsize=1,ysize=20,scr_xsize=8,scr_ysize=20, value='')
  Separator = Widget_Text(ToolbarBase1,xsize=1,ysize=20,scr_xsize=4,scr_ysize=20)
  Separator = Widget_Label(ToolbarBase1,xsize=1,ysize=20,scr_xsize=8,scr_ysize=20, value='')

  AnalyzeAllROI = Widget_Button(ToolbarBase1, UNAME='AnalyzeAll' ,XOFFSET=630 ,/ALIGN_CENTER $
      ,TOOLTIP='Analyze All Flash' ,VALUE='images\AnalyzeAllROI' ,/BITMAP,EVENT_PRO='OnAnalyzeAllROI')
;**************************************************
  UpSeparator = Widget_Text(MainBase, UNAME='UpSeparator'  $
      ,XOFFSET=13 ,YOFFSET=1,SCR_XSIZE=700 ,SCR_YSIZE=4 ,XSIZE=20 ,YSIZE=1)
  DownSeparator = Widget_Text(MainBase, UNAME='DownSeparator'  $
      ,XOFFSET=13 ,YOFFSET=30 ,SCR_XSIZE=700 ,SCR_YSIZE=4 ,XSIZE=20 ,YSIZE=1)

  TraceDraw = Widget_Draw(MainBase, UNAME='TraceWindow' ,XOFFSET=10 ,YOFFSET=40 $
    ,SCR_XSIZE=750 ,SCR_YSIZE=350,retain=2,/drop_events,event_pro='OnTraceWindowEvent')

  widths=[25,35,50,30,45,45,45,45,45,45,45,45,55,45,45,45,45,45,45,45,45]
  WID_TABLE_0 = Widget_Table(MainBase, UNAME='ResultTable' ,XOFFSET=10 ,YOFFSET=400 ,SCR_XSIZE=750 ,SCR_YSIZE=237  $
      ,COLUMN_LABELS=['ROI','Area','DToEdge','Flash','Peak','F0','dF/F0','Tstart','Tpeak','RT','RT90','T50','EndBase','Note','FAHM','dBase','T75','T90','FDHM','Dia']  $
      ,XSIZE=20 ,YSIZE=3000 ,/no_row_headers,event_pro='OnResultTableEvents',/all_events );Added by suntao:and the resulttable events


  Widget_Control, /REALIZE, MainBase
  widget_control, open, set_uvalue=CreatCursor()
  ;picName1 = file_search('Flash_2.5D',/FULLY_QUALIFY_PATH )
;  help,picname1
  info=routine_info('flashsniper',/source)
  path=info.path
  filedir=file_dirname(path)
  picname=filedir+'\Images\Flash_2.5D.jpg'
  print,picname
;  picName='E:\IDLFlashsnipper\FlashSniper_xroi\Images\Flash_2.5D.jpg' ;tested by SunTao
  ;picName=filepath('Flash_2.5D.jpg',subdirectory=['examples','data'])
;  help,picName
  read_jpeg, picName, Pic
  window,/free,/pixmap,xs=750,ys=350
  pixWin = {id:!D.window, pic:pic, tag:0b}
  widget_control, traceDraw, set_uvalue=pixWin
  
  TraceDrawInitialize, TraceDraw

  XManager, 'FlashSniper', MainBase, /NO_BLOCK

end