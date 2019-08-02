;
; Convert argument to string, different from IDL' String() function in that:
;  - bytes are treated as numbers instead of ASCII values
;  - blanks are removed
;
Function ToS,N

  Q = Size(N)
  NType = Q(Q(0)+1)

  Case NType Of
    0: Return, ''
    7: Return, N
    1: N = Fix(N)
    Else:
  EndCase

Return,StrCompress(String(N),/Remove)
End
