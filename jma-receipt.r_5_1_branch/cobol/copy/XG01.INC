            02  XG01.
              03  XG01-SELNUM.
                04  XG01-VALUE  PIC S9(3).
              03  XG01-TBL-GROUP.
                04  XG01-VALUE  PIC X(14).
              03  XG01-RENNUM.
                04  XG01-VALUE  PIC S9(4).
              03  XG01-SHORI-RENNUM.
                04  XG01-VALUE  PIC S9(4).
              03  XG01-SCROLLEDWINDOW26.
                04  XG01-HPOS   PIC S9(9)   BINARY.
                04  XG01-VPOS   PIC S9(9)   BINARY.
                04  XG01-LIST.
                  05  XG01-COUNT  PIC S9(9)   BINARY.
                  05  XG01-ROW    PIC S9(9)   BINARY.
                  05  XG01-ROWATTR
                                  PIC S9(9)   BINARY.
                  05  XG01-ITEM       OCCURS  100 TIMES.
                    06  XG01-TNO           PIC X(3).
                    06  XG01-TTBL-GROUP    PIC X(14).
                    06  XG01-TRENNUM       PIC X(4).
                    06  XG01-TSHORI-RENNUM PIC X(4).
                    06  XG01-TKENSU        PIC X(5).
                    06  XG01-TSRYYM        PIC X(6).
                    06  XG01-TSRYYMD       PIC X(9).
                    06  XG01-TNYUGAIKBN    PIC X(2).
                    06  XG01-TSHORI-TITLE  PIC X(20).
                    06  XG01-TPRT-TITLE    PIC X(60).
                    06  XG01-TPUTFLG       PIC X(2).
                  05  XG01-SELECT
                          PIC X       OCCURS  100 TIMES.
