pro findtilt,filename,chips=chips

if not keyword_set(chips) then chips = [1,2,3,4]

;hdr = headfits('/home/telescope/Repository/' + filename)
hdr = headfits('/Repository/' + filename)
ra = sxpar(hdr,'rotangle')
rapos = ra mod 360
if rapos lt 0 then rapos += 360


pupfile = 'pupcen.' + strtrim(round(rapos),2) + '.dat'
close, 1
openw, 1, pupfile
for i=0,n_elements(chips)-1 do begin
    lbcfpia,/uniqueout,/dontsend,imgsec=[50,2097,0,4607],file=filename,lbcchip=chips[i],pupcen=pupcen
    printf, 1, pupcen
endfor
close, 1

data = read_ascii(pupfile)
x = data.field1[0,*]
y = data.field1[1,*]
sz = data.field1[2,*]

polyplane = griddata(x,y,sz)
window, 1, retain=2,xsize=1200,ysize=1200
surface, polyplane,charsize=2

scale = 42;(atan(100/(13.5*4608))*180/!pi*3600)/8
maxx = max(x,min=minx)
maxy = max(y,min=miny)

;;Fits a plane to the the XY coordinates and Average Pupil Diameter
plane = griddata(x,y,sz,METHOD='PolynomialRegression',power=1)
window, 2, retain=2,xsize=1200,ysize=1200
surface, plane,charsize=2

;;Plots the positions and diameters of the pupils, with the chips
;;drawn in
window, 3, retain=2,xsize=1200,ysize=1200
plot,  x,y, psym=2,/isotropic,xrange=[0,6143],yrange=[0,6655],xstyle=1,ystyle=1
xyouts, x,y,strtrim(sz,2)
oplot, [2048,2048],[0,4608]
oplot, [4096,4096],[0,4608]
oplot, [768,768],[4609,6655]
oplot, [5376,5376],[4609,6655]
oplot, [0,6143],[4608,4608]

;;finds the tilt in the best-fit plane and calculates RX and RY
nxel = n_elements(plane[*,0])
nyel = n_elements(plane[0,*])
RX_0 = (plane[0,0] - plane[0,nxel-1])*scale*4608/(maxy-miny)
RY_0 = (plane[nyel-1,0] - plane[0,0])*scale*4608/(maxx-minx)

print, 'RX_0 = ',strtrim(rx_0,2),'   RY_0 = ',strtrim(ry_0,2)

stop

end


