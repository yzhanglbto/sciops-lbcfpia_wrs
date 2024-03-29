PRO xyxy, hdra, hdrb, xa, ya, xb, yb
;+
; NAME:
;	XYXY
; PURPOSE:
;	To use a pair of headers to convert X/Y positions from one frame
;	to another.
; CALLING SEQUENCE:
;	XYXY, hdra, hdrb, xa, ya, [ xb, yb ]
; INPUTS:
;	hdra - The header containing the plate solution describing the
;	       frame of reference being converted FROM.
;	hdra - The header containing the plate solution describing the
;	       frame of reference being converted TO.
;	xa   - A scalar or vector containing the x coordinate(s) to convert.
;	ya   - A scalar or vector containing the y coordinate(s) to convert.
;	       Must have the same number of elements as 'xa'.
; OUTPUTS:
;	xb   - The converted x coordinate(s).  If this parameter is not
;	       specified, it is returned through 'xa'.
;	yb   - The converted y coordinate(s).  If this parameter is not
;	       specified, it is returned through 'ya'.
; PROCEDURE:
;	The procedures 'xyad' and 'adxy' are used to perform the 
;       conversion.     The equinoxes of each header are checked with
;       "get_equinox" to make sure that they are identical, and "precess"
;       is used if they are not
;
;       Note that all X,Y coordinates are in the IDL convention (starting with
;       0,0) and not the FITS convention (first pixel is 1,1)
; PROCEDURES USED:
;	GET_EQUINOX(), EXTAST, XYAD, ADXY, PRECESS
; MODIFICATION HISTORY:
;	Written by Michael R. Greason, Hughes-STX, 13 April 1992.
;	Updated to use ASTROMETRY structures.  J.D.Offenberg, HSTX, Jan 1993
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
On_error,2
;			Check parameters.
np = N_params()
if (np LT 4) then begin  
	print, "Syntax:  xyxy, hdra, hdrb, xa, ya [, xb, yb]"
	return
endif
if ( N_elements(xa) NE N_elements(ya) ) then begin  
	print, 'The first two parameters must have the same number of elements.'
	return
endif
epa = get_equinox( hdra, codea)
epb = get_equinox( hdrb, codeb)

;			Extract the plate solutions from the headers.  If
;			either header hasn't a plate solution, set the 
;			output coordinates to the inputs and return.
;
extast, hdra, astra, noparamsa
extast, hdrb, astrb, noparamsb
IF ( (noparamsa LT 0) OR (noparamsb LT 0)) THEN BEGIN
	xb = xa
	yb = ya
	return
endif

;			Perform the conversion.

xyad, hdra, xa, ya, a, d

if ( codea GE 0 ) and (codeb GE 0) then $
    if ( epa NE epb ) then $
           precess, a, d, epa, epb

adxy, hdrb, a, d, xb, yb

;		If 'xb' and 'yb' weren't specified in the procedure
;		call, overwrite xa and ya.

if ( np LT 6 ) then begin  
	xa = xb
	ya = yb
endif
;
return
end
