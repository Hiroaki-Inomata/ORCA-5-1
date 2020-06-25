            02  S99.
              03  S99-MSG.
                04  S99-VALUE   PIC X(80).
              03  S99-SCROLLEDWINDOW28.
                04  S99-JOBKANRILST.
                  05  S99-COUNT   PIC S9(9)   BINARY.
                  05  S99-ROW     PIC S9(9)   BINARY.
                  05  S99-ROWATTR PIC S9(9)   BINARY.
                  05  S99-ITEM        OCCURS  30 TIMES.
                    06  S99-RENNUM    PIC X(7).
                    06  S99-SHORINM   PIC X(26).
                    06  S99-STARTTIME PIC X(8).
                    06  S99-ENDTIME   PIC X(8).
                    06  S99-PAGE      PIC X(10).
                    06  S99-ERRMSG    PIC X(100).
                  05  S99-SELECT  PIC X       OCCURS  30 TIMES.
              03  S99-B12-STATE   PIC S9(9)   BINARY.
              03  S99-B12-LABEL   PIC X(14).
