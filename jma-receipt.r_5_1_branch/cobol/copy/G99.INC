            02  G99.
              03  G99-MSG.
                04  G99-VALUE   PIC X(80).
              03  G99-SCROLLEDWINDOW28.
                04  G99-JOBKANRILST.
                  05  G99-COUNT   PIC S9(9)   BINARY.
                  05  G99-ROW     PIC S9(9)   BINARY.
                  05  G99-ROWATTR PIC S9(9)   BINARY.
                  05  G99-ITEM        OCCURS  30 TIMES.
                    06  G99-RENNUM    PIC X(7).
                    06  G99-SHORINM   PIC X(26).
                    06  G99-STARTTIME PIC X(8).
                    06  G99-ENDTIME   PIC X(8).
                    06  G99-PAGE      PIC X(10).
                    06  G99-ERRMSG    PIC X(100).
                  05  G99-SELECT  PIC X       OCCURS  30 TIMES.
              03  G99-B12-STATE   PIC S9(9)   BINARY.
              03  G99-B12-LABEL   PIC X(14).
              03  G99-DURATION   PIC S9(09) BINARY.
              03  G99-DOWNLOAD1.
                  05  G99-OBJECTDATA   PIC X(36).
                  05  G99-FILENAME     PIC X(1024).
