;+
; Creates a structuring element of radius R. In our case structuring
; elements are essentially filled or empy circles. R must be a
; positive number, it is rounded to the nearest integer.
;
; @param R {in} Radius of structuring element.
; @keyword Miss {in}{optional} If set then a structuring element for
;    "miss" part of "hit-or-miss" algorithm is created (an empty
;    circle), else the structuring element is a filled circle.
;
; @returns A two-dimensional byte array, containing the structuring
;    element.
;
; @history Added IDLDoc documentation on May 11, 2006
; @author Andrea Baruffolo, INAF - OAPd
;-
Function StrElm, R, Miss=Miss

  R = Fix(R+0.5)
  se = shift(dist(2*R+1),R,R)
  If KeyWord_Set(Miss) Then se = se GT R Else se = se LE R

Return, se
End
