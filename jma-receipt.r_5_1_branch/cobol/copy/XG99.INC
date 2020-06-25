            02  XG99.
              03  XG99-MSG.
                04  XG99-VALUE   PIC X(80).
              03  XG99-SCROLLEDWINDOW28.
                04  XG99-JOBKANRILST.
                  05  XG99-COUNT   PIC S9(9)   BINARY.
                  05  XG99-ROW     PIC S9(9)   BINARY.
                  05  XG99-ROWATTR PIC S9(9)   BINARY.
                  05  XG99-ITEM        OCCURS  30 TIMES.
                    06  XG99-RENNUM    PIC X(7).
                    06  XG99-SHORINM   PIC X(26).
                    06  XG99-STARTTIME PIC X(8).
                    06  XG99-ENDTIME   PIC X(8).
                    06  XG99-PAGE      PIC X(10).
                    06  XG99-ERRMSG    PIC X(100).
                  05  XG99-SELECT  PIC X       OCCURS  30 TIMES.
              03  XG99-DURATION   PIC S9(09) BINARY.
              03  XG99-DOWNLOAD1.
                  05  XG99-OBJECTDATA   PIC X(36).
                  05  XG99-FILENAME     PIC X(1024).
