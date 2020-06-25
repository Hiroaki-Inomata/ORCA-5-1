          02  W64.
              03  W64-SCROLLEDWINDOW28.
                04  W64-KOHNUMLST.
                  05  W64-COUNT   PIC S9(9)   BINARY.
                  05  W64-ROW PIC S9(9)   BINARY.
                  05  W64-ITEM            OCCURS  100 TIMES.
                    06  W64-TRENNUM       PIC X(3).
                    06  W64-TMEISYO       PIC X(44).
                    06  W64-TSTYUKYMD     PIC X(9).
                    06  W64-TEDYUKYMD     PIC X(9).
                    06  W64-TKOHNUM       PIC X(3) OCCURS  20.
                  05  W64-SELECT
                          PIC X       OCCURS  100 TIMES.
              03  W64-SELNUM.
                04  W64-VALUE   PIC S9(3).
              03  W64-STYUKYMD.
                04  W64-VALUE   PIC X(9).
              03  W64-EDYUKYMD.
                04  W64-VALUE   PIC X(9).
              03  W64-MEISYO.
                04  W64-VALUE   PIC X(44).
              03  W64-MEISYO2.
                04  W64-VALUE   PIC X(44).
              03  W64-KOH-OCC   OCCURS  20.
                04  W64-KOHNUM.
                  05  W64-VALUE   PIC X(3).
                04  W64-SEIDONAME.
                  05  W64-VALUE   PIC X(50).
