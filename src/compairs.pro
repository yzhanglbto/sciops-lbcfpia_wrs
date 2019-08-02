;this program takes an input list of the coeffs files and compares the
;values for coefficents with times less than 80 seconds different
;(which I assume means the mirror parameters didn't change

;NOTE: THE FILE LIST MUST BE IN ORDER OF TIME 

pro compairs, list

openr, 1, list
openw, 2, 'compared.out'
printf, format='(a13,a27,a19,a7,a8,a8,a8,a8,a8,a8,a8,a8,a8,a8,a8,a8,a8,a8)',$
  2, 'File1','File2','T_dif','Z5_1','Z5_2','Z6_1','Z6_2','Z7_1','Z7_2',$
  'Z8_1','Z8_2','Z11_1','Z11_2','Z5_dif','Z6_dif','Z7_dif','Z8_dif','Z11_dif'
 

uttotbefore = 0
nfiles = 0
while not eof(1) do begin
    filename = ''
    readf, 1, filename

    ;;makes an array of filenames
    if nfiles eq 0 then filenames = filename $
    else filenames = [filenames,filename]

    ;;determines the ut time in seconds from the filename
    filechunks = strsplit(filename,'.',/extract)
    ut = long(filechunks[2])
    uthr = ut/10000
    utmin = (ut-uthr*10000)/100
    utsec = ut-uthr*10000-utmin*100
    uttot = uthr*3600 + utmin*60 + utsec
    timediff = uttot-uttotbefore

    ;; if the time difference is less than 80 seconds (exposure+read),
    ;; assume they make a pair of images where the mirror parameters were not
    ;; adjusted, read the appropriate coefficient files, and output a
    ;; file with the coefficients and differences.
    if timediff lt 80 then begin
        file1 = read_ascii(filenames[nfiles-1],data_start=3)
        file2 = read_ascii(filenames[nfiles],data_start=3)
        good = [4,5,6,7,10]
        file1 = round(file1.field1[1,good])
        file2 = round(file2.field1[1,good])
        for i=0,4 do begin
            if i eq 0 then finarr = [file1[i],file2[i]] $
            else finarr = [finarr,file1[i],file2[i]]
        endfor
        for i=0,4 do finarr = [finarr,file1[i]-file2[i]]
        printf, format='(a26,x,a26,x,i4,x,i7,x,i7,x,i7,x,i7,x,i7,x,i7,x,i7,x,i7,x,i7,x,i7,x,i7,x,i7,x,i7,x,i7,x,i7)',$
          2, filenames[nfiles-1],filenames[nfiles],timediff,finarr

    endif
    uttotbefore = uttot
    nfiles++
endwhile
close, 1
close, 2

end
