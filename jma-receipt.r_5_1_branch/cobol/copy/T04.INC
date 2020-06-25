            02  T04.
              03  T04-SRYYM.
                04  T04-VALUE   PIC X(18).
              03  T04-SELNUM.
                04  T04-VALUE   PIC S9(5).
              03  T04-TITLE.
                04  T04-VALUE   PIC X(100).
              03  T04-SCROLLEDWINDOW28.
                04  T04-FILE-INFOLST.
                  05  T04-FILE-INFOLST-COUNT   PIC S9(9)   BINARY.
                  05  T04-FILE-INFOLST-ROW     PIC S9(9)   BINARY.
                  05  T04-FILE-INFOLST-ROWATTR PIC S9(9)   BINARY.
                  05  T04-FILE-INFOLST-ITEM        OCCURS  100 TIMES.
                    06  T04-TRENNUM  PIC X(5).
                    06  T04-TSYOYMD PIC X(9).
                    06  T04-TCREYMD PIC X(9).
                    06  T04-TTITLE  PIC X(100).
                    06  T04-TCNT-ALL  PIC X(7).
                    06  T04-TERRMSG  PIC X(200).
                  05  T04-FILE-INFOLST-SELECT
                          PIC X       OCCURS  100 TIMES.
