            02  L99.
              03  L99-MSG.
                04  L99-VALUE   PIC X(80).
              03  L99-SCROLLEDWINDOW28.
                04  L99-JOBKANRILST.
                  05  L99-COUNT   PIC S9(9)   BINARY.
                  05  L99-ROW     PIC S9(9)   BINARY.
                  05  L99-ROWATTR PIC S9(9)   BINARY.
                  05  L99-ITEM        OCCURS  30 TIMES.
                    06  L99-RENNUM    PIC X(7).
                    06  L99-SHORINM   PIC X(26).
                    06  L99-STARTTIME PIC X(8).
                    06  L99-ENDTIME   PIC X(8).
                    06  L99-PAGE      PIC X(10).
                    06  L99-ERRMSG    PIC X(100).
                  05  L99-SELECT  PIC X       OCCURS  30 TIMES.
              03  L99-B12-STATE   PIC S9(9)   BINARY.
              03  L99-B12-LABEL   PIC X(14).
              03  L99-DURATION   PIC S9(09) BINARY.
              03  L99-DOWNLOAD1.
                  05  L99-OBJECTDATA   PIC X(36).
                  05  L99-FILENAME     PIC X(1024).

