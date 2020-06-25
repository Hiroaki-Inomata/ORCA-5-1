        02  U02Q.
      ***** 02  U02Q-FIXED1.
              03  U02Q-SELNUM.
                04  U02Q-VALUE  PIC S9(3).
              03  U02Q-SCROLLEDWINDOW1.
                04  U02Q-HPOS   PIC S9(9)   BINARY.
                04  U02Q-VPOS   PIC S9(9)   BINARY.
                04  U02Q-QRLIST.
                  05  U02Q-COUNT  PIC S9(9)   BINARY.
                  05  U02Q-ROW    PIC S9(9)   BINARY.
                  05  U02Q-ITEM       OCCURS  50 TIMES.
                    06  U02Q-TNUM   PIC X(3).
                    06  U02Q-TPTNUM PIC X(20).
                    06  U02Q-TNAME  PIC X(50).
                    06  U02Q-TBIRTHDAY PIC X(10).
                    06  U02Q-TSEX    PIC X(2).
                    06  U02Q-THKNNUMMEI PIC X(20).
                    06  U02Q-TTIME   PIC X(5).
                  05  U02Q-SELECT
                          PIC X       OCCURS  50 TIMES.
