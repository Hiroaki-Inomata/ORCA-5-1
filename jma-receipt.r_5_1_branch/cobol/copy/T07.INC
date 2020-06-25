            02  T07.
              03  T07-TEISYUTUSAKI.
                04  T07-VALUE   PIC X(4).
              03  T07-SRYYM.
                04  T07-VALUE   PIC X(6).
              03  T07-PTNUM.
                04  T07-VALUE   PIC X(20).
              03  T07-NAME.
                04  T07-VALUE   PIC X(40).
              03  T07-SCROLLEDWINDOW26.
                04  T07-LIST.
                  05  T07-LIST-HYODAI1   PIC X(14).
                  05  T07-LIST-COUNT     PIC S9(9)   BINARY.
                  05  T07-LIST-ROW       PIC S9(9)   BINARY.
                  05  T07-LIST-ITEM      OCCURS  100 TIMES.
                    06  T07-TRENNUM      PIC X(3).
                    06  T07-TSEIKYU      PIC X(2).
                    06  T07-TUKEYMD      PIC X(9).
                    06  T07-TUCNT        PIC X(3).
                    06  T07-TSKYYMD      PIC X(9).
                    06  T07-TSCNT        PIC X(2).
                    06  T07-TNYUGAIKBN   PIC X(2).
                    06  T07-TRECESYUBETU PIC X(10).
                    06  T07-THKNJANUM    PIC X(8).
                    06  T07-TKIGO        PIC X(40).
                  05  T07-SELECT
                          PIC X       OCCURS  100 TIMES.
