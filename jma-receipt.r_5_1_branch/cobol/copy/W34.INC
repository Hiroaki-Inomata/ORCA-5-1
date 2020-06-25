        02  W34.
     ****** 02  W34-FIXED1.
              03  W34-B07.
                04  W34-B07-STATE   PIC S9(9)   BINARY.
              03  W34-B06.
                04  W34-B06-STATE   PIC S9(9)   BINARY.
              03  W34-PRGID.
                04  W34-VALUE   PIC X(25).
              03  W34-SELNUM.
                04  W34-VALUE   PIC S9(3).
              03  W34-SCROLLEDWINDOW1.
                04  W34-HPOS    PIC S9(9)   BINARY.
                04  W34-VPOS    PIC S9(9)   BINARY.
                04  W34-PRGLST.
                  05  W34-PRGLST-COUNT   PIC S9(9)   BINARY.
                  05  W34-ROW PIC S9(9)   BINARY.
                  05  W34-ITEM        OCCURS  100 TIMES.
                    06  W34-TNUM    PIC X(3).
                    06  W34-TPRGID  PIC X(25).
                  05  W34-PRGLST-SELECT
                          PIC X       OCCURS  100 TIMES.
              03  W34-KBNCD-COMB.
                04  W34-KBNCD.
                  05  W34-VALUE   PIC X(20).
                04  W34-KBNCD-COUNT   PIC S9(9)   BINARY.
                04  W34-KBNCD-ITEM
                        PIC X(20)   OCCURS  50 TIMES.
              03  W34-SCROLLEDWINDOW2.
                04  W34-HPOS    PIC S9(9)   BINARY.
                04  W34-VPOS    PIC S9(9)   BINARY.
                04  W34-EXPLANTION.
                  05  W34-VALUE   PIC X(44000).
              03  W34-SCROLLEDWINDOW3.
                04  W34-HPOS    PIC S9(9)   BINARY.
                04  W34-VPOS    PIC S9(9)   BINARY.
                04  W34-OPTION.
                  05  W34-VALUE   PIC X(44000).
              03  W34-LABEL215.
                04  W34-LABEL215-STYLE   PIC X(20).
