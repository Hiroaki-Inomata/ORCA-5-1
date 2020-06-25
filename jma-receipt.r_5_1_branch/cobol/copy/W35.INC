        02  W35.
      ****  02  W35-FIXED12.
              03  W35-STYUKYMD.
                04  W35-VALUE   PIC X(9).
              03  W35-EDYUKYMD.
                04  W35-VALUE   PIC X(9).
              03  W35-SELNUM1.
                04  W35-VALUE   PIC S9(3).
              03  W35-SELNUM2.
                04  W35-VALUE   PIC S9(3).
              03  W35-SCROLLEDWINDOW2.
                04  W35-HPOS    PIC S9(9)   BINARY.
                04  W35-VPOS    PIC S9(9)   BINARY.
                04  W35-KENSALIST.
                  05  W35-KENSALIST-COUNT   PIC S9(9)   BINARY.
                  05  W35-KENSALIST-ROW PIC S9(9)   BINARY.
                  05  W35-ITEM        OCCURS  110 TIMES.
                    06  W35-KNO     PIC X(3).
                    06  W35-KSRYCD  PIC X(9).
                    06  W35-KNAME   PIC X(80).
                  05  W35-KENSALIST-SELECT
                          PIC X       OCCURS  110 TIMES.
              03  W35-SCROLLEDWINDOW1.
                04  W35-HPOS    PIC S9(9)   BINARY.
                04  W35-VPOS    PIC S9(9)   BINARY.
                04  W35-DATALIST.
                  05  W35-DATALIST-COUNT   PIC S9(9)   BINARY.
                  05  W35-DATALIST-ROW PIC S9(9)   BINARY.
                  05  W35-DATALIST-ITEM        OCCURS  110 TIMES.
                    06  W35-DNO     PIC X(3).
                    06  W35-DSRYCD  PIC X(9).
                    06  W35-DNAME   PIC X(80).
                  05  W35-DATALIST-SELECT
                          PIC X       OCCURS  110 TIMES.
