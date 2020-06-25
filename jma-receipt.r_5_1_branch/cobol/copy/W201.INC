        02  W201.
     ****** 02  W201-FIXED32.
              03  W201-SCROLLEDWINDOW28.
                04  W201-MEILIST.
                  05  W201-MEILIST-COUNT  PIC S9(9)   BINARY.
                  05  W201-MEILIST-ROW    PIC S9(9)   BINARY.
                  05  W201-MEILIST-ROWATTR    PIC S9(9)   BINARY.
                  05  W201-ITEM       OCCURS  10 TIMES.
                    06  W201-TRENNUM PIC X(3).
                    06  W201-TKSNCD2 PIC X(20).
                    06  W201-TKSNNM2 PIC X(40).
                  05  W201-MEILIST-SELECT
                          PIC X       OCCURS  10 TIMES.
              03  W201-KSNCD2.
                04  W201-VALUE  PIC X(20).
              03  W201-KSNNM2.
                04  W201-VALUE  PIC X(40).
              03  W201-SELNUM.
                04  W201-VALUE  PIC S9(3).
