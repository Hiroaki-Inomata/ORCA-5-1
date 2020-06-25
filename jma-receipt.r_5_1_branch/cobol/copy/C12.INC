            02  C12.
              03  C12-SCROLLEDWINDOW28.
                04  C12-HPOS    PIC S9(9)   BINARY.
                04  C12-VPOS    PIC S9(9)   BINARY.
                04  C12-BYOMEILST.
                  05  C12-BYO-COUNT       PIC S9(9)   BINARY.
                  05  C12-BYO-ROW         PIC S9(9)   BINARY.
                  05  C12-BYO-ROWATTR     PIC S9(9)   BINARY.
                  05  C12-BYO-ITEM        OCCURS  200 TIMES.
                    06  C12-TBANGO        PIC X(5).
                    06  C12-TSRYKA        PIC X(4).
                    06  C12-TBYOMEI       PIC X(50).
                    06  C12-TSRYYMD       PIC X(9).
                    06  C12-TTENKIYMD     PIC X(9).
                    06  C12-TTENKIKBN     PIC X(8).
                    06  C12-TDLTYMD       PIC X(9).
                    06  C12-TDLT-OPID     PIC X(16).
                  05  C12-BYO-SELECT      PIC X   OCCURS  200 TIMES.
