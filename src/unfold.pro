;
; Returns image "unfolded" along ellipse
; Take NSamples along ellipse and NPts along normal
; direction, from -Dist to +Dist.
; NOTE: NSamples is set to an odd number to avoid 
; !PI/2 and 3/2*!PI (although a check is done anyway)
;
; Uses:
;   IDL funct's and pro's only
;
Function Unfold, Ima, EllP, Dist, NSamples, NPts, Display=Display
  EllP = Float(EllP)
  Dist = Float(Dist)
  ;; 
  ;; Check whether NSamples is even. If it is then
  ;; make it odd by adding one.
  ;;
  ;If NSamples MOD 2 EQ 0 Then Begin
  ;   NSamples += 1
  ;   Print, 'NSamples changed to '+String(NSamples)
  ;EndIf

  Th = FIndGen(NSamples)*2.0*!Pi/NSamples
  ;;
  ;; For each angle, compute point on ellipse
  ;; then find normal vector on that point
  ;; then compute locations of points along normal
  ;; that lie within -Dist and +Dist
  ;; finally rotate (by ellipse rot angle) 
  ;; and translate to position of ellipse center
  ;;
  x0 = EllP[0]*cos(Th)
  y0 = EllP[1]*sin(Th)

  ;Q = Where((Abs(Th-!PI/2.0) LT 2E-4) OR (Abs(Th-1.5*!PI) LT 2E-4))
  ;If Size(Q,/N_Dimensions) GT 0 Then Begin
  ;   ;; This part has to be written
  ;   Print, 'There are "dangerous" elements'
  ;EndIf

  f  = EllP[0]/EllP[1]
  f2 = f*f

  norm = Sqrt(x0*x0/f2+f2*y0*y0)

  nvx = x0/f/norm
  nvy = f*y0/norm

  dR = Dist*(FIndGen(NPts)/(NPts-1)*2.0-1.0)

  xx0 = x0#(FltArr(Npts)+1.0)
  yy0 = y0#(FltArr(Npts)+1.0)

  ;
  ; Locations of sampling points along cuts
  ;
  xx_nr = xx0+nvx#dR
  yy_nr = yy0+nvy#dR

  cr = cos(EllP[4])
  sr = sin(EllP[4])
  ;
  ; [Clockwise] rotation and shift
  ;
  xx =  xx_nr*cr+yy_nr*sr+EllP[2]
  yy = -xx_nr*sr+yy_nr*cr+EllP[3]

  If KeyWord_Set(Display) Then Begin
     Window,0,XSize=512,YSize=512
     TvScl, ConGrid(Ima,512,512)
     xd =  x0*cr+y0*sr+EllP[2]
     yd = -x0*sr+y0*cr+EllP[3]
     Print,xd
     Print,yd
     PlotS, 2*xd, 2*yd, /Device, PSym=1
  EndIf

Return, Bilinear(Float(Ima),xx,yy)
End
