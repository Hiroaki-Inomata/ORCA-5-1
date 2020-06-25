        02  W321.
     ****** 02  W321-FIXED32.
              03  W321-SCROLLEDWINDOW26.
                04  W321-HPOS   PIC S9(9)   BINARY.
                04  W321-VPOS   PIC S9(9)   BINARY.
                04  W321-LIST.
                  05  W321-LIST-COUNT  PIC S9(9)   BINARY.
                  05  W321-LIST-ROW    PIC S9(9)   BINARY.
                  05  W321-LIST-ROWATTR PIC S9(9)   BINARY.
                  05  W321-ITEM       OCCURS  500 TIMES.
                    06  W321-TNUM   PIC X(4).
                    06  W321-TSRYCD PIC X(9).
                    06  W321-TOFFKBN PIC X(2).
                    06  W321-TNAME  PIC X(80).
                    06  W321-TKIKAN PIC X(20).
                  05  W321-LIST-SELECT
                          PIC X       OCCURS  500 TIMES.
              03  W321-NEXT.
                04  W321-NEXT-STYLE   PIC X(20).
                04  W321-NEXT-VALUE   PIC X(20).
              03  W321-SELNUM.
                04  W321-VALUE  PIC S9(4).
              03  W321-MIDASI.
                04  W321-VALUE  PIC X(30).
