        02  P02Q.
      ***** 02  P02Q-FIXED1.
              03  P02Q-SELNUM.
                04  P02Q-VALUE  PIC S9(3).
              03  P02Q-SCROLLEDWINDOW1.
                04  P02Q-HPOS   PIC S9(9)   BINARY.
                04  P02Q-VPOS   PIC S9(9)   BINARY.
                04  P02Q-QRLIST.
                  05  P02Q-COUNT  PIC S9(9)   BINARY.
                  05  P02Q-ROW    PIC S9(9)   BINARY.
                  05  P02Q-ITEM       OCCURS  50 TIMES.
                    06  P02Q-TNUM   PIC X(3).
                    06  P02Q-TTIME  PIC X(5).
                    06  P02Q-THKNJANUM PIC X(8).
                    06  P02Q-TKIGONUM PIC X(80).
                    06  P02Q-THONKZKKBN PIC X(4).
                    06  P02Q-TKANANAME PIC X(50).
                    06  P02Q-TSEX   PIC X(2).
                    06  P02Q-TBIRTHDAY PIC X(10).
                    06  P02Q-TNAME PIC X(50).
                    06  P02Q-TPTNUM PIC X(20).
                    06  P02Q-TJYOTAI    PIC X(10).
                  05  P02Q-SELECT
                          PIC X       OCCURS  50 TIMES.
              03  P02Q-REGISTYMD.
                04  P02Q-VALUE  PIC X(30).
              03  P02Q-MIDASI.
                04  P02Q-VALUE  PIC X(22).
