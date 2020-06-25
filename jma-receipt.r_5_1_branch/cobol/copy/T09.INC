            02  T09.
              03  T09-PTNUM.
                04  T09-VALUE   PIC X(20).
              03  T09-SRYYM.
                04  T09-VALUE   PIC X(8).
              03  T09-NAME.
                04  T09-VALUE   PIC X(20).
              03  T09-SELNUM    PIC S9(2)   OCCURS  5.
              03  T09-SCROLLEDWINDOW26.
                04  T09-RECELIST.
                  05  T09-HYODAI1 PIC X(14).
                  05  T09-COUNT   PIC S9(9)   BINARY.
                  05  T09-ROW     PIC S9(9)   BINARY.
                  05  T09-ITEM        OCCURS  20 TIMES.
                    06  T09-TRENNUM      PIC X(2).
                    06  T09-TPTNUM       PIC X(20).
                    06  T09-TNAME        PIC X(20).
                    06  T09-TNYUGAIKBN   PIC X(2).
                    06  T09-TRECESYUBETU PIC X(10).
                    06  T09-THKNJANUM    PIC X(8).
                    06  T09-THOJOKBN     PIC X(8).
                    06  T09-TKIGO        PIC X(40).
                    06  T09-TRECEIPT-KBN PIC X(4).
                    06  T09-TDISASTER    PIC X(2).
                    06  T09-TCREYMD      PIC X(9).
                    06  T09-TCHK         PIC X(1).
                  05  T09-SELECT-G.
                    06  T09-SELECT
                          PIC X       OCCURS  20 TIMES.
