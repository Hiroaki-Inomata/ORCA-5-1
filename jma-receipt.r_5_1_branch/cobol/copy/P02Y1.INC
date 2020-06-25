        02  P02Y1.
      ***** 02  P02Y1-FIXED32.
              03  P02Y1-INPUT.
                04  P02Y1-VALUE PIC X(22).
              03  P02Y1-SENTAKU.
                04  P02Y1-VALUE PIC X(30).
              03  P02Y1-SELNUM    OCCURS  10.
                04  P02Y1-VALUE   PIC X(3).
              03  P02Y1-JIINCOB.
                04  P02Y1-JIIN.
                  05  P02Y1-VALUE PIC X(16).
                04  P02Y1-JIIN-COUNT PIC S9(9)   BINARY.
                04  P02Y1-JIIN-ITEM
                        PIC X(16)   OCCURS  10 TIMES.
              03  P02Y1-SCROLLEDWINDOW26.
                04  P02Y1-HPOS  PIC S9(9)   BINARY.
                04  P02Y1-VPOS  PIC S9(9)   BINARY.
                04  P02Y1-LIST.
                  05  P02Y1-COUNT PIC S9(9)   BINARY.
                  05  P02Y1-ROW   PIC S9(9)   BINARY.
                  05  P02Y1-ITEM      OCCURS  200 TIMES.
                    06  P02Y1-NO        PIC X(3).
                    06  P02Y1-NAME      PIC X(70).
                    06  P02Y1-TANI      PIC X(6).
                    06  P02Y1-KISOTEN   PIC X(10).
                    06  P02Y1-SRYKBN    PIC X(20).
                  05  P02Y1-SELECT
                          PIC X       OCCURS  200 TIMES.
              03  P02Y1-NEXT.
                04  P02Y1-NEXT-STYLE   PIC X(20).
                04  P02Y1-NEXT-VALUE   PIC X(20).
              03  P02Y1-B02.
                04  P02Y1-B02-STATE PIC S9(9)   BINARY.
              03  P02Y1-B03.
                04  P02Y1-B03-STATE PIC S9(9)   BINARY.
              03  P02Y1-B04.
                04  P02Y1-B04-STATE PIC S9(9)   BINARY.
              03  P02Y1-B05.
                04  P02Y1-B05-STATE PIC S9(9)   BINARY.
