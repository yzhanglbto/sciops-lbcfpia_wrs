Function MkTechBlueMask, Sx, Sy, cenX, cenY, width

xx=(findgen(Sx)-CenX)#replicate(1,Sx)
yy=transpose((findgen(Sy)-cenY)#replicate(1,Sy))
mask=atan(yy,xx)

w=Float(width)/180.*!pi

q=where(mask lt !PI/2-w/2 or mask gt !PI/2+w/2, complement=nq)

mask[q]  = 1
mask[nq] = 0

Return, mask
End
