            02  T99.
              03  T99-B01-STATE   PIC S9(9)   BINARY.
              03  T99-B12-STATE   PIC S9(9)   BINARY.
              03  T99-B12-LABEL   PIC X(14).
              03  T99-MSG-STYLE   PIC X(20).
              03  T99-MSG.
                04  T99-VALUE   PIC X(200).
              03  T99-SCROLLEDWINDOW28.
                04  T99-JOBKANRILST.
                  05  T99-COUNT       PIC S9(9)   BINARY.
                  05  T99-ROW         PIC S9(9)   BINARY.
                  05  T99-ROWATTR     PIC S9(9)   BINARY.
                  05  T99-ITEM        OCCURS  30 TIMES.
                    06  T99-RENNUM    PIC X(7).
                    06  T99-SHORINM   PIC X(20).
                    06  T99-STARTTIME PIC X(8).
                    06  T99-ENDTIME   PIC X(8).
                    06  T99-PAGE      PIC X(10).
                    06  T99-ERRMSG    PIC X(100).
                  05  T99-SELECT  PIC X       OCCURS  30 TIMES.
              03  T99-DURATION   PIC S9(09) BINARY.
              03  T99-DOWNLOAD1.
                  05  T99-OBJECTDATA   PIC X(36).
                  05  T99-FILENAME     PIC X(1024).
