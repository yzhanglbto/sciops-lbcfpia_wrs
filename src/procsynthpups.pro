Pro ProcSynthPupils

  OpenW, OLun, "psp-measures.txt", /Get_Lun

  s = [0.,0.26,0.4,0.53,0.66,0.79,0.93,1.06,1.32,1.59,1.85,2.12]

  FileName = [ $
             "lbcb.20071029.000000.fits", $
             "lbcb.20071029.000026.fits", $
             "lbcb.20071029.000040.fits", $
             "lbcb.20071029.000053.fits", $
             "lbcb.20071029.000066.fits", $ 
             "lbcb.20071029.000079.fits", $
             "lbcb.20071029.000093.fits", $
             "lbcb.20071029.000106.fits", $
             "lbcb.20071029.000132.fits", $
             "lbcb.20071029.000159.fits", $
             "lbcb.20071029.000185.fits", $
             "lbcb.20071029.000212.fits" ]

  Dir = "C:\abaruffo\myWorks\LBC\data\Synthetic Data"

  For I=0,11 Do Begin
     lbcfpia,/donts,DataDir=Dir,File=FileName[I],rough_ires=ires,/UseFitOnly
     PrintF,OLun,s[I],ires[3],ires[4],ires[5],ires[6],ires[7],ires[10],Format="(F4.2,2X,I5,2X,I5,2X,I5,2X,I5,2X,I5,2X,I5)"
  EndFor

  Close, OLun

Return
End
