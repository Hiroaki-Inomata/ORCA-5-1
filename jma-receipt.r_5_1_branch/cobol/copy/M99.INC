            02  M99.
              03  M99-B01-STATE   PIC S9(9)   BINARY.
              03  M99-MSG.
                04  M99-VALUE   PIC X(80).
              03  M99-SCROLLEDWINDOW28.
                04  M99-JOBKANRILST.
                  05  M99-COUNT       PIC S9(9)   BINARY.
                  05  M99-ROW         PIC S9(9)   BINARY.
                  05  M99-ROWATTR     PIC S9(9)   BINARY.
                  05  M99-ITEM        OCCURS  30 TIMES.
                    06  M99-RENNUM    PIC X(7).
                    06  M99-SHORINM   PIC X(20).
                    06  M99-STARTTIME PIC X(8).
                    06  M99-ENDTIME   PIC X(8).
                    06  M99-PAGE      PIC X(10).
                    06  M99-ERRMSG    PIC X(100).
                  05  M99-SELECT  PIC X       OCCURS  30 TIMES.
              03  M99-DURATION   PIC S9(09) BINARY.
