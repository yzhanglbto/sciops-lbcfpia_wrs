pro cartop,fxy,x0,y0,rmax,r,theta,frt,missing=missing
;
; - - cartop - car(tesion) to p(olar) coordinates.
; - - Given a cartesian range of x,y and a function fxy(x,y) this
; - - procedure interpolates to find frt(r,theta).  The origin of
; - - the polar coordinate system is assumed to be at x0,y0.  The
; - - maximum radius of the coordinate system from that point is rmax.
; - - All distance units are in pixel units.
;
nparms=N_Params()
if (nparms ne 7) then begin
   Message, 'Usage:',/info
   Message, 'cartop,fxy,x0,x0,rmax,r,theta,frt,missing=missing', /info
   Message, 'Purpose: Interpolate a function to polar coordinates from ', /info
   Message, '         its values given in Cartesian coordinates', /info
   Message, 'Input:   fxy = 2-d array f, as a function of x,y indices', /info
   Message, '         x0,y0 = origin of polar coordinate system', /info
   Message, '         note - x0,y0 must be inside the cartesian image', /info
   Message, '         rmax = maximum of desired radius values', /info
   Message, '         (note units must be in pixel separation units)', /info
   Message, 'Output:  r - array of radius values', /info
   Message, 'Output:  theta - array of polar angle values', /info
   Message, '         (theta = 0 points to rt along x-axis)', /info
   Message, 'Output:  frt - array of f values as function of r, theta', /info
   Message, 'Require: The IDL interpolate function', /info
   Message, 'Written: George H. Fisher UCB/SSL 6/09/2004 (version 0.01)', /info
endif
if(keyword_set(missing)) then begin
   fmiss=missing
endif else begin
   fmiss=0.
endelse
;
; - - get sizes and ranges of radius (r) and angle (theta) arrays:
;


nr=(long(rmax))
r=(findgen(nr))
nt=long(2.*!pi*rmax+1.)
theta=2.*!pi*findgen(nt)/float(nt-1)


fsize=size(fxy)
if(fsize(0) ne 2) then begin
  print,"fxy not a 2D array"
endif

xpolar=x0+real_part(r)#cos(theta)
ypolar=y0+real_part(r)#sin(theta)


frt=interpolate(fxy,xpolar,ypolar,cubic=-0.5,missing=fmiss)


return
end
