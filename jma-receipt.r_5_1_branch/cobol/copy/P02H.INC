        02  P02H.
      ****  02  P02H-FIXED10.
              03  P02H-HBTNUM.
                04  P02H-VALUE  PIC X(3).
              03  P02H-SELNUM.
                04  P02H-VALUE  PIC S9(3).
              03  P02H-SCROLLEDWINDOW1.
                04  P02H-HPOS   PIC S9(9)   BINARY.
                04  P02H-VPOS   PIC S9(9)   BINARY.
                04  P02H-HBTNUMLST.
                  05  P02H-LIST-COUNT  PIC S9(9)   BINARY.
                  05  P02H-LIST-ROW    PIC S9(9)   BINARY.
                  05  P02H-ITEM       OCCURS  50 TIMES.
                    06  P02H-TNUM       PIC X(2).
                    06  P02H-TKOHNUM    PIC X(3).
                    06  P02H-TKOHNUMMEI PIC X(20).
                  05  P02H-LIST-SELECT
                          PIC X       OCCURS  50 TIMES.
