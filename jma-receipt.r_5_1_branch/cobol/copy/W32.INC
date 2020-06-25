        02  W32.
    ******  02  W32-FIXED1.
              03  W32-STYUKYMD.
                04  W32-VALUE   PIC X(10).
              03  W32-EDYUKYMD.
                04  W32-VALUE   PIC X(10).
              03  W32-SELNUM.
                04  W32-VALUE   PIC S9(2).
              03  W32-SCROLLEDWINDOW1.
                04  W32-HPOS    PIC S9(9)   BINARY.
                04  W32-VPOS    PIC S9(9)   BINARY.
                04  W32-KIKAN-LIST.
                  05  W32-KIKAN-COUNT   PIC S9(9)   BINARY.
                  05  W32-ROW PIC S9(9)   BINARY.
                  05  W32-ITEM        OCCURS  10 TIMES.
                    06  W32-TNUM    PIC X(3).
                    06  W32-TSTYUKYMD  PIC X(9).
                    06  W32-TEDYUKYMD  PIC X(9).
                  05  W32-KIKAN-SELECT
                          PIC X       OCCURS  10 TIMES.
              03  W32-KBNCD-COMB.
                04  W32-KBNCD.
                  05  W32-VALUE   PIC X(60).
                04  W32-KBNCD-COUNT   PIC S9(9)   BINARY.
                04  W32-KBNCD-ITEM
                        PIC X(60)   OCCURS  200 TIMES.
              03  W32-SANTEIHOU-COMB.
                04  W32-SANTEIHOU.
                  05  W32-VALUE   PIC X(40).
                04  W32-SANTEIHOU-COUNT   PIC S9(9)   BINARY.
                04  W32-SANTEIHOU-ITEM
                        PIC X(40)   OCCURS  10 TIMES.
              03  W32-SANTEIKBN-COMB.
                04  W32-SANTEIKBN.
                  05  W32-VALUE   PIC X(40).
                04  W32-SANTEIKBN-COUNT   PIC S9(9)   BINARY.
                04  W32-SANTEIKBN-ITEM
                        PIC X(40)   OCCURS  10 TIMES.
              03  W32-NYUGAIKBN-COMB.
                04  W32-NYUGAIKBN.
                  05  W32-VALUE   PIC X(40).
                04  W32-NYUGAIKBN-COUNT   PIC S9(9)   BINARY.
                04  W32-NYUGAIKBN-ITEM
                        PIC X(40)   OCCURS  5 TIMES.
              03  W32-HKNCOMBIKBN-COMB.
                04  W32-HKNCOMBIKBN.
                  05  W32-VALUE   PIC X(40).
                04  W32-HKNCOMBIKBN-COUNT   PIC S9(9)   BINARY.
                04  W32-HKNCOMBIKBN-ITEM
                        PIC X(40)   OCCURS  5 TIMES.
              03  W32-SRYKBN-COMB-G.
                04  W32-SRYKBN-COMB    OCCURS   13.
                  05  W32-SRYKBN.
                    07  W32-VALUE   PIC X(20).
                  05  W32-SRYKBN-COUNT   PIC S9(9)   BINARY.
                  05  W32-SRYKBN-ITEM
                        PIC X(20)   OCCURS  5 TIMES.
              03  W32-B03.
                04  W32-B03-STATE   PIC S9(9)   BINARY.
