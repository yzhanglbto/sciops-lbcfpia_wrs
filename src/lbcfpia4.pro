pro lbcfpia4, filename

hdr = headfits('/Repository/' + filename)
ra = sxpar(hdr,'rotangle')
rapos = ra mod 360
if rapos lt 0 then rapos += 360

openw, 1, 'pupcen.' + strtrim(round(rapos),2) + '.dat'
;for i=1,4 do begin
;    lbcfpia,/uniqueout,/dontsend,imgsec=[50,2097,0,4607],file=filename,lbcchip=i,pupcen=pupcen
;    printf, 1, pupcen
;endfor

;lbcfpia,/uniqueout,/dontsend,imgsec=[4500,6547,1,4608],file=filename,lbcchip=1,pupcen=pupcen
;printf, 1, pupcen
lbcfpia,/uniqueout,/dontsend,file=filename,lbcchip=2,pupcen=pupcen
printf, 1, pupcen
;lbcfpia,/uniqueout,/dontsend,imgsec=[306,2353,1,4608],file=filename,lbcchip=3,pupcen=pupcen
;printf, 1, pupcen
;lbcfpia,/uniqueout,/dontsend,imgsec=[6597,8644,1,4608],file=filename,lbcchip=4,pupcen=pupcen
;printf, 1, pupcen

close, 1


end
