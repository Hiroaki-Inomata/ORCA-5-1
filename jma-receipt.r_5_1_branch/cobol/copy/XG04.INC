            02  XG04.
              03  XG04-SELNUM.
                04  XG04-VALUE  PIC S9(3).
              03  XG04-TBL-GROUP.
                04  XG04-VALUE  PIC X(14).
              03  XG04-RENNUM.
                04  XG04-VALUE  PIC S9(4).
              03  XG04-SHORI-RENNUM.
                04  XG04-VALUE  PIC S9(4).
              03  XG04-SCROLLEDWINDOW26.
                04  XG04-HPOS   PIC S9(9)   BINARY.
                04  XG04-VPOS   PIC S9(9)   BINARY.
                04  XG04-LIST.
                  05  XG04-COUNT  PIC S9(9)   BINARY.
                  05  XG04-ROW    PIC S9(9)   BINARY.
                  05  XG04-ROWATTR
                                  PIC S9(9)   BINARY.
                  05  XG04-ITEM       OCCURS  100 TIMES.
                    06  XG04-TNO           PIC X(3).
                    06  XG04-TTBL-GROUP    PIC X(14).
                    06  XG04-TRENNUM       PIC X(4).
                    06  XG04-TSHORI-RENNUM PIC X(4).
                    06  XG04-TKENSU        PIC X(5).
                    06  XG04-TSRYYM        PIC X(6).
                    06  XG04-TSRYYMD       PIC X(9).
                    06  XG04-TNYUGAIKBN    PIC X(2).
                    06  XG04-TSHORI-TITLE  PIC X(20).
                    06  XG04-TPRT-TITLE    PIC X(60).
                    06  XG04-TPUTFLG       PIC X(2).
                  05  XG04-SELECT
                          PIC X       OCCURS  100 TIMES.
