FUNCTION TSUM,X,Y,IMIN,IMAX               ;Trapezoidal summation
;+
; NAME:
;       TSUM
; PURPOSE:
;       Trapezoidal summation of the area under a curve.   
;
; CALLING SEQUENCE:
;       Result = TSUM(y)
;              or
;       Result = TSUM( x, y, [ imin, imax ] )  
; INPUTS:
;       x = array containing independent variable.  If omitted, then
;               x is assumed to contain the index of the y variable.
;               x = lindgen( N_elements(y) ).
;       y = array containing dependent variable y = f(x)
;
; OPTIONAL INPUTS:
;       imin = index of x array at which to begin the integration, integer
;               scalar.  If omitted, then summation starts at x[0].
;       imax = index of x value at which to end the integration, integer 
;               scalar.  If omitted then the integration ends at x[npts-1].
;
; OUTPUTS:
;       result = area under the curve y=f(x) between x[imin] and x[imax].
;
; EXAMPLE:
;       IDL> x = [0.0,0.1,0.14,0.3] 
;       IDL> y = sin(x)
;       IDL> print,tsum(x,y)    ===>  0.0445843
;       
;       In this example, the exact curve can be computed analytically as 
;       1.0 - cos(0.3) = 0.0446635     
; PROCEDURE:
;       The area is determined of individual trapezoids defined by x[i],
;       x[i+1], y[i] and y[i+1].
;
;       If the data is known to be at all smooth, then a more accurate
;       integration can be found by interpolation prior to the trapezoidal
;       sums, for example, by the standard IDL User Library int_tabulated.pro.
; MODIFICATION HISTORY:
;       Written, W.B. Landsman, STI Corp. May 1986
;       Modified so X is not altered in a one parameter call Jan 1990
;       Converted to IDL V5.0   W. Landsman   September 1997
;-
; Set default parameters
 On_error,2
 npar = N_params()
   
 if npar EQ 1 then begin
    npts = N_elements(x)
    yy = x
    xx = lindgen(npts)

 endif else begin

   if ( npar LT 3 ) then imin = 0
   npts = min( [N_elements(x), N_elements(y)] )
   if ( npar LT 4 ) then imax = npts-1
   xx = x[imin:imax]
   yy = y[imin:imax]
   npts = imax - imin + 1
 endelse         
;
; Compute areas of trapezoids and sum result
;
  xdif = shift(xx,-1) - xx
  xdif = xdif[0:npts-2]
  yavg = ( yy + shift(yy,-1) ) / 2.  
  yavg = yavg[ 0:npts-2 ]       

  return, total( xdif*yavg ) 
  end     
