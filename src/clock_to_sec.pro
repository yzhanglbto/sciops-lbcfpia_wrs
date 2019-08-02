;+
; CLOCK_TO_SEC
; Transform 'clock' (time in a long integer) to seconds
;-
Function clock_to_sec, clock

  l_clock = Long(clock)
  fact = 10000L
  hh = l_clock/fact
  l_clock = l_clock - hh * fact
  fact = 100L
  mm = l_clock/fact
  l_clock = l_clock - mm * fact
;;; Print,hh,mm,l_clock

Return, (hh*60L+mm)*60L+l_clock
End
