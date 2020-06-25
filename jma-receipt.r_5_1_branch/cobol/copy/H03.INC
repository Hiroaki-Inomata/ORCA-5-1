            02  H03.
              03  H03-SELNUM.
                04  H03-VALUE   PIC S9(4).
              03  H03-SCROLLEDWINDOW1.
                04  H03-KANRI-LIST.
                  05  H03-KANRI-LIST-COUNT     PIC S9(9)   BINARY.
                  05  H03-KANRI-LIST-ROW       PIC S9(9)   BINARY.
                  05  H03-KANRI-LIST-ITEM          OCCURS  100 TIMES.
                    06  H03-KANRI-TSEQNUM      PIC X(3).
                    06  H03-KANRI-TSYORIYM     PIC X(6).
                    06  H03-KANRI-TTEISYUTUSAKI
                                               PIC X(10).
                    06  H03-KANRI-TTEISYUTUSAKI2
                                               PIC X(2).
                    06  H03-KANRI-TCNT         PIC X(4).
                    06  H03-KANRI-THOSPCD      PIC X(2).
                    06  H03-KANRI-TERRCD       PIC X(3).
                    06  H03-KANRI-TCREYMD      PIC X(9).
                  05  H03-KANRI-LIST-SELECT    PIC X
                                                   OCCURS  100 TIMES.
              03  H03-SCROLLEDWINDOW2.
                04  H03-BODY-LIST.
                  05  H03-BODY-LIST-HYODAI    PIC X(14).
                  05  H03-BODY-LIST-COUNT     PIC S9(9)   BINARY.
                  05  H03-BODY-LIST-ROW       PIC S9(9)   BINARY.
                  05  H03-BODY-LIST-ITEM          OCCURS  100 TIMES.
                    06  H03-BODY-TSEQNUM      PIC X(3).
                    06  H03-BODY-TNYUGAIKBN   PIC X(2).
                    06  H03-BODY-TPTNUM       PIC X(20).
                    06  H03-BODY-TNAME        PIC X(40).
                    06  H03-BODY-TSRYYM       PIC X(6).
                    06  H03-BODY-TRECESYUBETU PIC X(10).
                    06  H03-BODY-THKNJANUM    PIC X(8).
                    06  H03-BODY-TKIGO        PIC X(40).
                    06  H03-BODY-TRECEIPT-KBN PIC X(4).
                    06  H03-BODY-TDISASTER    PIC X(2).
                    06  H03-BODY-TERR         PIC X(100).
                    06  H03-BODY-TERRCD       PIC X(3).
                  05  H03-BODY-LIST-SELECT
                          PIC X       OCCURS  100 TIMES.
                  05  H03-BODY-LIST-FGCOLOR  PIC X(16) OCCURS 100 TIMES.
                  05  H03-BODY-LIST-BGCOLOR  PIC X(16) OCCURS 100 TIMES.
