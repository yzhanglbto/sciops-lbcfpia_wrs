  ;+
  ; :Description:
  ;    Function to select pupils in LBC images e choose the best one in term of S/N ratio
  ;
  ; :Params:
  ;    data   - input image in which to find pupils
  ;    alpha  - sigma level example: alpha*sigma
  ;    dx     - dimension of the box containing the selected pupils (it should be at least 60-64 pixels to properly encompass pupils)
  ;    timest - timestamp used for the identification of the saved files in the log file
  ; 
  ; 
  ; :Keywords:
  ;    display  - to display selected pupils on screen
  ;
  ;Astro lib needed routins: sky.pro, centroid.pro, invariant_moments.pro
  ;
  ;
  ; :Author: Marco Stangalini marco.stangalini@inaf.it
  ; first version: 4/4/2014
  ; 24 Apr 2014 info log added (MS)
  ; 24 Apr 2014 check and remove of overlapping pupils (MS)
  ; 28 Apr 2014 Warning and reports added about too low S/N (MS)
  ; 29 Apr 2014 Bug fixed. The pupil selection was stuck if the initial sigma level was too low (MS)
  ; 7 May 2014 Bug fixed. The saturation check was not working properly.
  ; 9 Jun 2014 Bug fixed -- pupils selection on the edges fixed
  ; 3 Jul 2015 Separated displays for blue and red channels
  ;-
function FINDPUP, data, alpha, dx, timest, display=display, red=red, blue=blue
@WRS.common 

si=size(data)
sky, data, std, /silent
PO=0


if alpha lt 1. then begin
  print, 'Warning, the detection threshold is low. WRS will try to see if there exist a good pupil anyway...'
  printf, 33, 'Warning, the detection threshold is low. WRS will try to see if there exist a good pupil anyway...'
endif

msk=(data ge alpha*std)
S = REPLICATE(1, 5, 5)
; "Opening" operator:
msk = DILATE(ERODE(msk, S), S)
lab=label_region(msk)
nel=fltarr(max(lab)+1)
xpos=0b
ypos=0b

for n0=1,max(lab) do begin
  nel(n0)=n_elements(where(lab eq n0))
  temp=lab eq n0
  if nel(n0) gt 10 then begin
  posx=CENTROID(temp)
  xpos=[xpos, [posx(0)]]
  ypos=[ypos, [posx(1)]]
  endif
endfor





if N_ELEMENTS(XPOS) LT 2 then BEGIN 
  PRINTF, 33, 'NO PUPIL DETECTED ABOVE SELECTION THRESHOLD '
GOTO, Jump_pupils
ENDIF

;sizearray=size(data)
;xlim=sizearray(1)
;ylim=sizearray(2)
;
;
;for np=1, n_elements(xpos)-1 do begin
;  if (xpos(np) gt xlim-dx) or xpos(np) lt dx then begin
;    xpos(np)=99999
;  endif
;  if ypos(np) gt ylim-dx or ypos(np) lt dx then begin
;    ypos(np)=99999
;  endif  
;endfor
;
;edge_pupils=where(xpos eq 99999 or ypos eq 99999)
;xpos(edge_pupils)=99999
;ypos(edge_pupils)=99999
;good_pupils=where(xpos lt 99998)
;
;
;xpos=xpos(good_pupils)
;ypos=ypos(good_pupils)


xpos=xpos[1:*]
ypos=ypos[1:*]
xp=0b
yp=0b
maxval=0b
COUNTP=0B

for n0=0, n_elements(xpos)-1 do begin
  if xpos[n0] gt dx and ypos[n0] gt dx then begin
      if abs(si(1)-xpos[n0]-1) gt dx*3+2 and abs(si(2)-ypos[n0]-1) gt dx*3+2 then begin ;
      ;if abs(si(1)-xpos[n0]-1) gt dx*2+2 and abs(si(2)-ypos[n0]-1) gt dx*2+2 then begin
        temp=msk(xpos[n0]-dx:xpos[n0]+dx-1,ypos[n0]-dx:ypos[n0]+dx-1) 
        temp2=data(xpos[n0]-dx:xpos[n0]+dx-1,ypos[n0]-dx:ypos[n0]+dx-1)
        inv=INVARIANT_MOMENTS(temp)
        if  inv[1] lt 0.005  and max(temp2) lt 65535 Then Begin
        xp=[xp, [xpos[n0]]]
        yp=[yp, [ypos[n0]]]  
        maxval=[maxval, [total(temp)]] 
        COUNTP=COUNTP+1 
        endif
      endif
  endif 
endfor



PRINTF,33, COUNTP, ' CANDIDATES FOUND BELOW CCD SATURATION LEVEL'

PRINTF, 33, 'CHECKING AND REMOVING OVERLAPPING PUPILS...'
DISTANCE_MAP=FLTARR(N_ELEMENTS(XP), N_ELEMENTS(YP))
distance_map(*,*)=dx*3
FOR I=0,N_ELEMENTS(XP)-1 DO BEGIN
  FOR J=0,I-1 DO BEGIN 
  DISTANCE_MAP(I,J)=SQRT((XP[I]-XP[J])^2.+(YP[I]-YP[J])^2.)
  ENDFOR
ENDFOR




OVERLAP=WHERE(DISTANCE_MAP lt DX*2)

if overlap[0] ne -1 then begin
INDX=ARRAY_INDICES(DISTANCE_MAP, OVERLAP)
PRINTF,33, N_ELEMENTS(INDX[0,*]), ' OVERLAPPING PUPILS FOUND'
XP(INDX[0,*])=0
YP(INDX[0,*])=0
maxval(INDX[0,*])=0
YP=YP(where(yp gt 0))
xp=xp(where(xp gt 0))
maxval=maxval(where(maxval gt 0))
mmax=max(maxval, m0)
PRINTF,33, ' OVERLAPPING PUPILS REMOVED'

endif else begin
IF N_ELEMENTS(XP) GT 1 THEN BEGIN
xp=xp[1:*]
yp=yp[1:*]
mmax=max(maxval[1:*], m0)
ENDIF ELSE BEGIN
GOTO, Jump_pupils
ENDELSE
endelse





Po=[[xp[m0]],[yp[m0]]]
Sflux=fltarr(2*dx,2*dx)


;ricentramento
;for i=-dx,dx-1 do begin
;  for j=-dx,dx-1 do begin
;    Sflux(i+dx,j+dx)=total(data(po(0)-dx+i:po(0)+dx+i-1,po(1)-dx+j:po(1)+dx+j-1 ))
;  endfor
;endfor
;
;
;Smax=max(Sflux,SM)
;maxpos=array_indices(Sflux, SM)
;
;Po(0)=Po(0)-dx+maxpos(0)
;Po(1)=Po(1)-dx+maxpos(1)




pupil_mean_signal=mean(data(po[0]-dx:po[0]+dx-1, po[1]-dx:po[1]+dx-1))
if pupil_mean_signal/std lt 1.1 then begin
  print, 'Detection threshold is  too low. WRS cannot proceed. Check configuration fiels or the image.'
  printf, 33, 'Detection threshold is  too low. WRS cannot proceed. Check the image provided.'
  printf, 33, 'WRS exit without Zernike estimation. Check the parameters in the configuration file or the image provided.'
  close,33
  retall
endif

tempup=data(po[0]-dx:po[0]+dx-1, po[1]-dx:po[1]+dx-1)



if keyword_set(display) then begin
  loadct, 0
  !p.multi=[0,2,1]
  if keyword_set(red) then begin
  window,0, title='RED CHANNEL WRS PUPIL SELECTION', xs=600,ys=300
  endif else begin
  window,10, title='BLUE CHANNEL WRS PUPIL SELECTION', xs=600,ys=300
  endelse
  
  
  
  tvframe, data, /asp, title='CANDITATES'
  loadct, 13
  plots, xp, yp, /data, psym=6, symsize=3
  plots, po(0), po(1),/data, psym=6, symsize=4, color=200, thick=3
  loadct, 0
  tvframe, tempup, /asp, title='SELECTED'
  if wrs.save_wrs eq 1 then begin
    image=tvrd(TRUE=3)
    WRITE_JPEG, wrs.save_dir+'FINDPUP_results_'+timest+'.jpg', image , true=3
    printf,33, wrs.save_dir+'FINDPUP_results_'+timest+'.jpg SAVED'
  endif
  !P.MULTI=0
endif




JUMP_PUPILS:



return, Po
end
