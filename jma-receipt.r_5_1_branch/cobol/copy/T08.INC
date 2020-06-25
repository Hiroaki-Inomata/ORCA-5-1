            02  T08.
              03  T08-TEISYUTUSAKI.
                04  T08-VALUE   PIC X(4).
              03  T08-SRYYM.
                04  T08-VALUE   PIC X(6).
              03  T08-SCROLLEDWINDOW3.
                04  T08-KANRI-LIST.
                  05  T08-KANRI-LIST-COUNT   PIC S9(9)   BINARY.
                  05  T08-KANRI-LIST-ROW     PIC S9(9)   BINARY.
                  05  T08-KANRI-LIST-ITEM    OCCURS  100 TIMES.
                    06  T08-KANRI-TRENNUM  PIC X(3).
                    06  T08-KANRI-TSKYYMD  PIC X(9).
                    06  T08-KANRI-TKENSU   PIC X(4).
                    06  T08-KANRI-TCREYMD  PIC X(9).
                  05  T08-KANRI-LIST-SELECT
                          PIC X       OCCURS  100 TIMES.
              03  T08-SCROLLEDWINDOW1.
                04  T08-RECE-LIST.
                  05  T08-RECE-LIST-HYODAI1   PIC X(14).
                  05  T08-RECE-LIST-COUNT     PIC S9(9)   BINARY.
                  05  T08-RECE-LIST-ROW       PIC S9(9)   BINARY.
                  05  T08-RECE-LIST-ITEM      OCCURS  1000 TIMES.
                    06  T08-RECE-TRENNUM      PIC X(4).
                    06  T08-RECE-TNYUGAIKBN   PIC X(2).
                    06  T08-RECE-TPTNUM       PIC X(20).
                    06  T08-RECE-TNAME        PIC X(40).
                    06  T08-RECE-TBIRTHDAY    PIC X(9).
                    06  T08-RECE-TSRYYM       PIC X(6).
                    06  T08-RECE-TRECESYUBETU PIC X(10).
                    06  T08-RECE-THKNJANUM    PIC X(8).
                    06  T08-RECE-TKIGO        PIC X(40).
                    06  T08-RECE-THOJOKBN     PIC X(2).
                  05  T08-RECE-LIST-SELECT
                          PIC X       OCCURS  1000 TIMES.
