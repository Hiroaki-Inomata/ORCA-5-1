            02  N29.
              03  N29-MSG.
                04  N29-VALUE   PIC X(80).
              03  N29-SCROLLEDWINDOW28.
                04  N29-JOBKANRILST.
                  05  N29-COUNT   PIC S9(9)   BINARY.
                  05  N29-ROW     PIC S9(9)   BINARY.
                  05  N29-ROWATTR PIC S9(9)   BINARY.
                  05  N29-ITEM        OCCURS  30 TIMES.
                    06  N29-RENNUM    PIC X(7).
                    06  N29-SHORINM   PIC X(26).
                    06  N29-STARTTIME PIC X(8).
                    06  N29-ENDTIME   PIC X(8).
                    06  N29-PAGE      PIC X(10).
                    06  N29-ERRMSG    PIC X(100).
                  05  N29-SELECT  PIC X       OCCURS  30 TIMES.
              03  N29-B12-STATE   PIC S9(9)   BINARY.
              03  N29-B12-LABEL   PIC X(14).
