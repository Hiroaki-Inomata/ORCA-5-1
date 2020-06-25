            02  Z97.
              03  Z97-MSG.
                04  Z97-VALUE   PIC X(80).
              03  Z97-SCROLLEDWINDOW28.
                04  Z97-JOBKANRILST.
                  05  Z97-COUNT   PIC S9(9)   BINARY.
                  05  Z97-ROW     PIC S9(9)   BINARY.
                  05  Z97-ROWATTR PIC S9(9)   BINARY.
                  05  Z97-ITEM        OCCURS  30 TIMES.
                    06  Z97-RENNUM    PIC X(7).
                    06  Z97-SHORINM   PIC X(26).
                    06  Z97-STARTTIME PIC X(8).
                    06  Z97-ENDTIME   PIC X(8).
                    06  Z97-PAGE      PIC X(10).
                    06  Z97-ERRMSG    PIC X(100).
                  05  Z97-SELECT  PIC X       OCCURS  30 TIMES.
              03  Z97-DURATION    PIC S9(09) BINARY.
              03  Z97-B12-STATE   PIC S9(9)   BINARY.
              03  Z97-B12-LABEL   PIC X(14).
              03  Z97-DOWNLOAD1.
                  05  Z97-OBJECTDATA   PIC X(36).
                  05  Z97-FILENAME     PIC X(1024).
