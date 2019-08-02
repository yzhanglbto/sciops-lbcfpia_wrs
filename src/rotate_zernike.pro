function rotate_zernike, res, ra


res_temp=fltarr(21)
res_temp(0:n_elements(res)-1)=res
res=res_temp

  newres = res ; copy the array of Zernikes, zero-based indexing
  RAr = RA/180.*!PI ; convert the rotator angle to radians
  ; This does nothing to Tip and Tilt Z2, Z3
  ; Focus, Spherical, 5th Spherical have order=0 ---> azimuthal symmetry
  ; Astigmatism, order=2
  newres[4] = res[4]*cos(2.0*RAr) + res[5]*sin(2.0*RAr)  ; Z5
  newres[5] = res[5]*cos(2.0*RAr) - res[4]*sin(2.0*RAr)  ; Z6
  ; Coma, order=1
  newres[6] = res[6]*cos(1.0*RAr) + res[7]*sin(1.0*RAr)  ; Z7
  newres[7] = res[7]*cos(1.0*RAr) - res[6]*sin(1.0*RAr)  ; Z8
  newres[8] = res[8]*cos(3.0*RAr) + res[9]*sin(3.0*RAr)  ; Z9
  newres[9] = res[9]*cos(3.0*RAr) - res[8]*sin(3.0*RAr)  ; Z10
  ; 5th Astigmatism, order=2
  newres[11] = res[11]*cos(2.0*RAr) + res[12]*sin(2.0*RAr) ; Z12
  newres[12] = res[12]*cos(2.0*RAr) - res[11]*sin(2.0*RAr) ; Z13
  ; Quadrafoil, order=4
  newres[13] = res[13]*cos(4.0*RAr) + res[14]*sin(4.0*RAr) ; Z14
  newres[14] = res[14]*cos(4.0*RAr) - res[13]*sin(4.0*RAr) ; Z15
  ; 5th Coma, order=1
  newres[15] = res[15]*cos(1.0*RAr) + res[16]*sin(1.0*RAr) ; Z16
  newres[16] = res[16]*cos(1.0*RAr) - res[15]*sin(1.0*RAr) ; Z17
  ; 5th Trefoil, order=3
  newres[17] = res[17]*cos(3.0*RAr) + res[18]*sin(3.0*RAr) ; Z18
  newres[18] = res[18]*cos(3.0*RAr) - res[17]*sin(3.0*RAr) ; Z19
  ; Pentafoil, order=5
  newres[19] = res[19]*cos(5.0*RAr) + res[20]*sin(5.0*RAr) ; Z20
  newres[20] = res[20]*cos(5.0*RAr) - res[19]*sin(5.0*RAr) ; Z21


return, newres
end