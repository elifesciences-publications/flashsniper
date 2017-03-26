path = 'D:\My Documents\Data for Work\Flash Sniper\for validation\'
files = dialog_pickfile(path=path,get_path=path,filter='*.txt',/multiple_files)
if files[0] eq '' then retall
fileNum = n_elements(files)
;file = path+'cpYFP-72h-10uMnifedipine-cellC4.lsm-validation result.txt'

for fileNo=0,fileNum-1 do begin
  temp = (read_ascii(files[fileNo],data_start=2)).(0)
  if fileNo eq 0 then begin    
    dim = size(temp,/dimensions)
    count = intarr(dim)
    init = fltarr(dim)
  endif
  for column=0,dim[0]-1 do begin
    if (column+1) mod 9 eq 0 then begin
      for row=0,dim[1]-1 do begin
        if temp[column,row] eq 0 then continue
        init[column,row] += temp[column,row]
        count[column,row] += 1
      endfor
    endif else begin
      init[column,*] += temp[column,*]
      count[column,*] += 1
    endelse
  endfor  
  print, 'reading ',strtrim(fileNo+1,1),' of ',strtrim(fileNum,1),files[fileNo]
endfor

output = init/count
openw, unit, path+'total validation result.txt', width=2000 ,/append ,/get_lun
printf, unit, output
free_lun, unit


End