FUNCTION zernike_estim, mode, grid
  ; Input: grid = set of points (polar co.) on which the Zernike must be evaluated
  ; output: vector of Zernike values on the grid
  zern_num, mode, M = m, N = n
  p = (mode MOD 2)
  R=0d0
  
  For J=0,(n-m)/2 Do Begin
    S=J
    R=R+(-1.)^J*Fact(n-J)/(Fact(S)*Fact((n+m)/2-J)$_
      *Fact((n-m)/2-J))*grid(*,0)^(n-2*J)
  Endfor
  
  IF (m EQ 0) Then ZZ=Sqrt(n+1d0)*R
  
  IF (m NE 0) Then Begin
    IF (p EQ 0) Then ZZ=Sqrt(n+1d0)*Sqrt(2d0)*Cos(m*grid(*,1))*R
    IF (p GT 0) Then ZZ=Sqrt(n+1d0)*Sqrt(2d0)*Sin(m*grid(*,1))*R
  EndIF
  return,zz
END