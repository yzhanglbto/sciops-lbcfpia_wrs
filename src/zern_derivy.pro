FUNCTION zern_derivy,j,n,m
    zern_num, j, M = m, N = n
;   zern_degree, j, n, m
   gam = fltarr(j)
   for j2=1,j do begin
      zern_num, j2, M = m2, N = n2
;      zern_degree, j2, n2, m2
      IF ((m-m2)^2 eq 1) THEN BEGIN
         IF (m NE 0) AND (m2 NE 0) THEN BEGIN
            IF ((j mod(2) EQ 0) AND (j2 mod(2) NE 0)) OR $
             ((j2 mod(2) EQ 0) AND (j mod(2) NE 0)) THEN BEGIN
              IF m2 EQ (m+1) AND (j mod(2) NE 0) THEN sig = -1 $
               ELSE IF m2 EQ (m-1) AND (j mod(2) EQ 0) THEN  sig = -1 $
                ELSE sig = 1
               gam(j2-1) =  sig*sqrt((n+1)*(n2+1)) 
            ENDIF ELSE  gam(j2-1) = 0
         ENDIF ELSE IF ((m EQ 0) AND (j2 mod(2) NE 0)) THEN $
          gam(j2-1) = sqrt(2.*(n+1)*(n2+1)) ELSE $
          IF ((m2 EQ 0) AND (j mod(2) NE 0)) THEN gam(j2-1) = sqrt(2.*(n+1)*(n2+1)) $
         ELSE  gam(j2-1) = 0 
       ENDIF ELSE gam(j2-1) = 0
   ENDFOR 
   return,gam
END
