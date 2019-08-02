;+
; Class constructor. Initialize member variables.
; @returns 1, always
;-
; $Log: lbciaLogger__define.pro,v $
; Revision 1.2  2005/01/04 10:11:41  swdepot
; Added keywords in source headers
;
Function lbciaLogger::Init, FileName=FileName, _extra=extra
   Compile_Opt Idl2
   self.LogLevel = 0               ; default log level
   self.LogUnit  = -3L             ; Set to invalid LUN
   If KeyWord_Set(FileName) Then self.LogFName = FileName $
   Else self.LogFName = "lbcia.log"
   ;; Print,"lbciaLogger::Init : set log file name ok"
   self->OpenLogFile
   ;; Print,"lbciaLogger::Init : lun is ",self.LogUnit
   Return, 1
End
;+
; Helper function: checks if input parameter is a valid LUN.
; @param lun {in}{required} lun to be checked.
; @returns TRUE is input parameter is valid LUN, FALSE otherwise
;-
Function lbciaLogger::isValidLUN, lun
   Compile_Opt Idl2
   Return, (lun GE -2) AND (lun LE 128)
End
;+
; Class destructor. Closes the log file.
;-
Pro lbciaLogger::Cleanup
   Compile_Opt Idl2
   If self->isValidLUN( self.LogUnit ) AND self.LogUnit GT 0 Then $
     Free_Lun, self.LogUnit
End
;+
; Open logFile for output.
; If logFile has been created more than 24 hours ago
; then truncate it.
;-
Pro lbciaLogger::OpenLogFile

  ;;;Print, 'LBCIALOGGER: OPENING LOG FILE'

  If self->isValidLUN( self.LogUnit ) Then Begin
    Res = FStat( self.LogUnit )
    If Res.Open Ne 0 Then Return Else Free_Lun, self.LogUnit
  EndIf
  ; Print,"LUN is not valid"

  ;
  ; Increment number of file open operations
  ;
  self.NumOpens += 1

  ; Log file is not open, check if we should append log data
  ; to an existing one or create a new log file

  Appnd = 0
  OpenR, newUnit, self.LogFName, /Get_Lun, Error=OpenErr
  If OpenErr EQ 0 Then Begin
    self.LogUnit = newUnit
    Res = FStat( self.LogUnit )
    ; This is Julian day at noon
    JDAtNoon = Long(SysTime(/Julian))
    ; Compute file creation time as JD
    BD = Bin_Date( SysTime( 0, Res.CTime ) )
    JD = Long(JulDay( Long(BD[1]), Long(BD[2]), Long(BD[0]), $
                      Long(BD[3]), Long(BD[4]), Long(BD[5]) ))

    Free_Lun, self.LogUnit
    ;
    ; Keep logs for the last 3 days
    ;
    If JDAtNoon-JD LT 3 Then Appnd=1
  Endif

  ;; Print, "lbciaLogger::OpenLogFile : append is ",Appnd
  OpenW, newUnit, self.LogFName, Append=Appnd, Error=OpenErr, /Get_Lun
  If OpenErr NE 0 Then Begin
     Print, "CANNOT OPEN LOG FILE "+self.LogFName
     Print, "LOGGING TO STDOUT"
     self.LogUnit = -2 
  Endif Else self.LogUnit = newUnit
Return
End
;+
; Helper method: performs the actual writes to the log file.
; It prepends current time and user supplied prefix to the output
; string and then writes the result to the log file.
; @param Prfx {in}{required} Prefix string to be prepended to output.
; @param Strng {in}{required} String to be logged.
;-
Pro lbciaLogger::DoLog, Prfx, Strng
  Compile_Opt Idl2
  If self->isValidLUN( self.LogUnit ) Then Begin
     Res = FStat( self.LogUnit )
     If Res.Open EQ 0 Then Begin
        Print, "DOLOG: LOG FILE NOT OPEN"
        self->OpenLogFile
     EndIf
     Get_Date, curTime, /TimeTag
     Message = '['+curTime+'] '+Prfx+"> "+Strng
     PrintF, self.LogUnit, Message
     Flush, self.LogUnit
     ; If self.LogLevel GE 4 Then Print, Message
  EndIf
End
;+
; Method to set the logging level. Level goes from 0 to 4. Increasing
; the log level increases the amount of output that goes in the log
; file.
; @param Level {in}{required} log level, must be between 0 and 4. It
; is forced to 0 if negative, and to 4 if greater or equal to 5.
;-
Pro lbciaLogger::SetLogLevel, Level
   Compile_Opt Idl2
   NewLevel = Long(Level)
   If NewLevel LE 0 Then      self.LogLevel = 0L $
   Else If NewLevel GE 4 Then self.LogLevel = 4L $
   Else self.LogLevel = NewLevel
   Self->DoLog, "SFTW", "NEW debug level is "+String(self.LogLevel,Format='(I1)')
Return
End
;+
; Returns the current log level.
; @returns integer in the range [0,4]
;-
Function lbciaLogger::GetLogLevel
Return, Self.LogLevel
End
;+
; Logs the string supplied as parameter if the current log level is
; set to 4. Doesn't produce any output if log level is <= 3.
; @param Str {in}{required} string to be logged
;-
Pro lbciaLogger::ExtDbgLog, Str
   Compile_Opt Idl2
   If self.LogLevel GT 3 Then $
     Self->DoLog, "EXTD", String(Str)
Return
End
;+
; Logs the string supplied as parameter if the current log level is
; >= 3. Doesn't produce any output if log level is <= 2.
; @param Str {in}{required} string to be logged
;-
Pro lbciaLogger::DebugLog, Str
   Compile_Opt Idl2
   If self.LogLevel GT 2 Then $
     Self->DoLog, "DEBG", String(Str)
Return
End
;+
; Logs the string supplied as parameter if the current log level is
; >= 2. Doesn't produce any output if log level is <= 1.
; @param Str {in}{required} string to be logged
;-
Pro lbciaLogger::InfoLog, Str
   Compile_Opt Idl2
   If self.LogLevel GT 1 Then $
     Self->DoLog, "INFO", String(Str)
Return
End
;+
; Logs the string supplied as parameter if the current log level is
; >= 1. Doesn't produce any output if log level is = 0.
; @param Str {in}{required} string to be logged
;-
Pro lbciaLogger::WarningLog, Str
   Compile_Opt Idl2
   If self.LogLevel GT 0 Then $
     Self->DoLog, "WARN", String(Str)
Return
End
;+
; Always log the supplied string in the log file, marking it as a
; error log.
; @param Str {in}{required} string to be logged
;-
Pro lbciaLogger::ErrorLog, Str
   Compile_Opt Idl2
   Self->DoLog, "ERRL", String(Str)
Return
End
;+
; Always log the supplied string in the log file, marking it as an
; action log.
; @param Str {in}{required} string to be logged
;-
Pro lbciaLogger::ActionLog, Str
   Compile_Opt Idl2
   Self->DoLog, "ACTI", String(Str)
Return
End
;+
; Always log the supplied string in the log file, marking it as a
; data log.
; @param Str {in}{required} string to be logged
;-
Pro lbciaLogger::DataLog, Str
   Compile_Opt Idl2
   Self->DoLog, "DATA", String(Str)
Return
End
;+
; Always log to console and to file
; @param Str {in}{required} string to be logged
;-
Pro lbciaLogger::LogAllWays, Str
   Compile_Opt Idl2
   Self->DoLog, "MESG", String(Str)
   Print, String(Str)
Return
End
;+
; Print status to stdout, for debugging.
;-
Pro lbciaLogger::Status
   Compile_Opt Idl2
   Print, "Log file : "+self.LogFName
   Print, "Log unit : "+String(self.LogUnit)
   Print, "Log level: "+String(self.LogLevel)
   Print, "Num opens: "+String(self.NumOpens)
Return
End
;+
; This class provides methods for logging. All output goes to a single
; text file. It is possible to control the amount of output by setting
; the log level.
; @field LogFName Name of the log file.
; @field LogUnit Unit number associated with the log file
; @field LogLevel Current level of logging
; @author A. Baruffolo, INAF - Osservatorio Astronomico di Padova
;-
Pro lbciaLogger__Define
   Compile_Opt Idl2
   lbciaLogger = { lbciaLogger,               $
                   LogFName: "",         $
                   LogUnit:  0L,         $
                   LogLevel: 0L,         $
                   NumOpens: 0L          $
                 }
End
