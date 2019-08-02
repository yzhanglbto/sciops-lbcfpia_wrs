;; Runs a list of images through lbcfpia
;; Output files replace "fits" with "coeff" and 
;; are written to the current directory
;; Does not send corrections to the telescope

pro runlist, list

openr, 1, list
while not eof(1) do begin
    filename = ''
    readf, 1, filename
    lbcfpia, file=filename,/uniqueout,/dontsend
endwhile
close, 1

end
