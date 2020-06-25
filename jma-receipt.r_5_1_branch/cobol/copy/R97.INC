            02  R97.
              03  R97-B06-STATE          PIC S9(9)   BINARY.
              03  R97-B07-STATE          PIC S9(9)   BINARY.
              03  R97-SRYYM.
                04  R97-VALUE   PIC X(7).
              03  R97-NYUGAIKBNCOMBO.
                04  R97-NYUGAIKBN.
                  05  R97-VALUE   PIC X(8).
                04  R97-NYUGAIKBN-COUNT   PIC S9(9)   BINARY.
                04  R97-NYUGAIKBN-ITEM    PIC X(8)    OCCURS  2 TIMES.
              03  R97-HTEISYUTUSAKICOMBO.
                04  R97-HTEISYUTUSAKI.
                  05  R97-VALUE   PIC X(10).
                04  R97-HTEISYUTUSAKI-COUNT   PIC S9(9) BINARY.
                04  R97-HTEISYUTUSAKI-ITEM    PIC X(10) OCCURS 4 TIMES.
              03  R97-HYOUJIKBNCOMBO.
                04  R97-HYOUJIKBN.
                  05  R97-VALUE   PIC X(10).
                04  R97-HYOUJIKBN-COUNT   PIC S9(9)   BINARY.
                04  R97-HYOUJIKBN-ITEM    PIC X(10)    OCCURS  2 TIMES.
              03  R97-SELNUM.
                04  R97-VALUE   PIC S9(5).
              03  R97-PTNUM.
                04  R97-VALUE   PIC X(20).
              03  R97-NAME.
                04  R97-VALUE   PIC X(30).
              03  R97-TEISYUTUSAKI.
                04  R97-VALUE   PIC X(8).
              03  R97-RECESYUBETU.
                04  R97-VALUE   PIC X(20).
              03  R97-HKNJANUM.
                04  R97-VALUE   PIC X(8).
              03  R97-HKNJAMEI.
                04  R97-VALUE   PIC X(20).
              03  R97-HOJOKBN.
                04  R97-VALUE   PIC X(10).
              03  R97-SYUKA-INPUT PIC X(15).
              03  R97-SYUKA-AUTO  PIC X(15).
              03  R97-SYUKA-INPUT1  PIC X(15).
              03  R97-SYUKACOMB.
                04  R97-SYUKA.
                  05  R97-VALUE   PIC X(21).
                04  R97-SYUKA-COUNT   PIC S9(9)   BINARY.
                04  R97-SYUKA-ITEM
                        PIC X(21)   OCCURS  100 TIMES.
              03  R97-SCROLLEDWINDOW1.
                04  R97-HPOS    PIC S9(9)   BINARY.
                04  R97-VPOS    PIC S9(9)   BINARY.
                04  R97-SYUKALST.
                  05  R97-SYUKALST-COUNT   PIC S9(9)   BINARY.
                  05  R97-SYUKALST-ROW PIC S9(9)   BINARY.
                  05  R97-SYUKALST-ITEM        OCCURS  500 TIMES.
                    06  R97-SYUKA-TNUM    PIC X(5).
                    06  R97-SYUKA-TTSTPTNUMKBN  PIC X(2).
                    06  R97-SYUKA-TPTNUM  PIC X(20).
                    06  R97-SYUKA-TNAME   PIC X(30).
                    06  R97-SYUKA-TTEISYUTUSAKI   PIC X(8).
                    06  R97-SYUKA-TRECESYUBETU PIC X(10).
                    06  R97-SYUKA-THKNJANUM    PIC X(8).
                    06  R97-SYUKA-THOJOKBN     PIC X(10).
                    06  R97-SYUKA-TRECEIPT-KBN PIC X(4).
                    06  R97-SYUKA-TDISASTER    PIC X(2).
                    06  R97-SYUKA-TSYUKA-INPUT PIC X(6).
                    06  R97-SYUKA-TSYUKA-AUTO  PIC X(6).
                  05  R97-SYUKALST-SELECT
                          PIC X       OCCURS  500 TIMES.
              03  R97-SCROLLEDWINDOW2.
                04  R97-HPOS    PIC S9(9)   BINARY.
                04  R97-VPOS    PIC S9(9)   BINARY.
                04  R97-PTNUMLST.
                  05  R97-PTNUM-HYODAI.
                    06  R97-VALUE   PIC X(4).
                  05  R97-PTNUM-COUNT   PIC S9(9)   BINARY.
                  05  R97-PTNUM-ROW     PIC S9(9)   BINARY.
                  05  R97-PTNUM-ITEM        OCCURS  100 TIMES.
                    06  R97-PTNUM-TSRYKA       PIC X(15).
                    06  R97-PTNUM-TTENSU       PIC X(10).
                  05  R97-PTNUM-SELECT
                          PIC X       OCCURS  100 TIMES.
