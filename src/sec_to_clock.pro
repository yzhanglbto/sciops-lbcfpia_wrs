;+
; SEC_TO_CLOCK
; Transform seconds to 'clock' (time in a long integer)
;-
Function sec_to_clock, sec

  l_sec = Long(sec)
  fact = 3600L
  hh = l_sec/fact
  l_sec = l_sec MOD fact
  fact = 60L 
  mm = l_sec/fact

Return, (hh*100L+mm)*100L+l_sec MOD fact
End
