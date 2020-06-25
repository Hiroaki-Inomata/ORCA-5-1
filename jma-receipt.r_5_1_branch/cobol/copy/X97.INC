            02  X97.
              03  X97-MSG.
                04  X97-VALUE   PIC X(80).
              03  X97-SCROLLEDWINDOW28.
                04  X97-JOBKANRILST.
                  05  X97-COUNT   PIC S9(9)   BINARY.
                  05  X97-ROW     PIC S9(9)   BINARY.
                  05  X97-ROWATTR PIC S9(9)   BINARY.
                  05  X97-ITEM        OCCURS  30 TIMES.
                    06  X97-RENNUM    PIC X(7).
                    06  X97-SHORINM   PIC X(26).
                    06  X97-STARTTIME PIC X(8).
                    06  X97-ENDTIME   PIC X(8).
                    06  X97-PAGE      PIC X(10).
                    06  X97-ERRMSG    PIC X(100).
                  05  X97-SELECT  PIC X       OCCURS  30 TIMES.
              03  X97-B12-STATE   PIC S9(9)   BINARY.
              03  X97-B12-LABEL   PIC X(14).
