        02  W981.
   ******** 02  W981-FIXED32.
              03  W981-SELNUM.
                04  W981-VALUE  PIC S9(3).
              03  W981-SCROLLEDWINDOW28.
                04  W981-HPOS   PIC S9(9)   BINARY.
                04  W981-VPOS   PIC S9(9)   BINARY.
                04  W981-MEILIST.
                  05  W981-MEILIST-COUNT  PIC S9(9)   BINARY.
                  05  W981-MEILIST-ROW    PIC S9(9)   BINARY.
                  05  W981-MEILIST-ROWATTR    PIC S9(9)   BINARY.
                  05  W981-ITEM       OCCURS  100 TIMES.
                    06  W981-RENNUM      PIC X(3).
                    06  W981-OPID     PIC X(16).
                    06  W981-LDNAME   PIC X(20).
                    06  W981-PTNUM    PIC X(20).
                    06  W981-TIME     PIC X(20).
                  05  W981-MEILIST-SELECT
                          PIC X       OCCURS  100 TIMES.
