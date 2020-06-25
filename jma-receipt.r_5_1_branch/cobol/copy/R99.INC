            02  R99.
              03  R99-MSG-STYLE   PIC X(20).
              03  R99-MSG         PIC X(200).
              03  R99-SCROLLEDWINDOW28.
                04  R99-JOBKANRILST.
                  05  R99-COUNT       PIC S9(9)   BINARY.
                  05  R99-ROW         PIC S9(9)   BINARY.
                  05  R99-ROWATTR     PIC S9(9)   BINARY.
                  05  R99-ITEM        OCCURS  2 TIMES.
                    06  R99-RENNUM    PIC X(7).
                    06  R99-SHORINM   PIC X(34).
                    06  R99-STARTTIME PIC X(8).
                    06  R99-ENDTIME   PIC X(8).
                    06  R99-ERRMSG    PIC X(100).
                  05  R99-SELECT  PIC X       OCCURS  2 TIMES.
              03  R99-B12-STATE   PIC S9(9)   BINARY.
              03  R99-B12-LABEL   PIC X(14).
              03  R99-B21-STATE   PIC S9(9)   BINARY.
              03  R99-PROGRESSBAR-VALUE       PIC S9(9)   BINARY.
