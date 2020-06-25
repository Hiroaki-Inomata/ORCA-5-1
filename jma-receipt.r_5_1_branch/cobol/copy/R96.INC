            02  R96.
              03  R96-SRYYM.
                04  R96-VALUE   PIC X(7).
              03  R96-NYUGAIKBNCOMBO.
                04  R96-NYUGAIKBN.
                  05  R96-VALUE   PIC X(8).
                04  R96-NYUGAIKBN-COUNT   PIC S9(9)   BINARY.
                04  R96-NYUGAIKBN-ITEM    PIC X(8)    OCCURS  2 TIMES.
              03  R96-SELNUM.
                04  R96-VALUE   PIC S9(3).
              03  R96-PTNUM.
                04  R96-VALUE   PIC X(20).
              03  R96-NAME.
                04  R96-VALUE   PIC X(30).
              03  R96-SCROLLEDWINDOW1.
                04  R96-HPOS    PIC S9(9)   BINARY.
                04  R96-VPOS    PIC S9(9)   BINARY.
                04  R96-MISEIKYULST.
                  05  R96-MISEIKYULST-COUNT   PIC S9(9)   BINARY.
                  05  R96-MISEIKYULST-ROW PIC S9(9)   BINARY.
                  05  R96-MISEIKYULST-ROWATTR  PIC S9(9)   BINARY.
                  05  R96-MISEIKYULST-ITEM        OCCURS  500 TIMES.
                    06  R96-MISEIKYU-TNUM    PIC X(3).
                    06  R96-MISEIKYU-TPTNUM  PIC X(20).
                    06  R96-MISEIKYU-TNAME   PIC X(30).
                  05  R96-MISEIKYULST-SELECT
                          PIC X       OCCURS  500 TIMES.
              03  R96-SCROLLEDWINDOW2.
                04  R96-HPOS    PIC S9(9)   BINARY.
                04  R96-VPOS    PIC S9(9)   BINARY.
                04  R96-PTNUMLST.
                  05  R96-PTNUMLST-COUNT   PIC S9(9)   BINARY.
                  05  R96-PTNUMLST-ROW PIC S9(9)   BINARY.
                  05  R96-PTNUMLST-ITEM        OCCURS  100 TIMES.
                    06  R96-PTNUM-TTEISYUTUSAKI  PIC X(4).
                    06  R96-PTNUM-TRECESYUBETU   PIC X(10).
                    06  R96-PTNUM-THKNJANUM      PIC X(8).
                    06  R96-PTNUM-THOJOKBN       PIC X(10).
                  05  R96-PTNUMLST-SELECT
                          PIC X       OCCURS  100 TIMES.
              03  R96-SCROLLEDWINDOW3.
                04  R96-HPOS    PIC S9(9)   BINARY.
                04  R96-VPOS    PIC S9(9)   BINARY.
                04  R96-KAIJYOLST.
                  05  R96-KAIJYOLST-COUNT   PIC S9(9)   BINARY.
                  05  R96-KAIJYOLST-ROW PIC S9(9)   BINARY.
                  05  R96-KAIJYOLST-ROWATTR  PIC S9(9)   BINARY.
                  05  R96-KAIJYOLST-ITEM        OCCURS  500 TIMES.
                    06  R96-KAIJYO-TNUM    PIC X(3).
                    06  R96-KAIJYO-TSRYYM  PIC X(6).
                    06  R96-KAIJYO-TPTNUM  PIC X(20).
                    06  R96-KAIJYO-TNAME   PIC X(30).
                  05  R96-KAIJYOLST-SELECT
                          PIC X       OCCURS  500 TIMES.
