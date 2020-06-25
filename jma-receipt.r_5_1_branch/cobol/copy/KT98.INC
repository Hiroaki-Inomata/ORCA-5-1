        02  KT98.
     *****  02  KT98-FIXED32.
              03  KT98-INPUT.
                04  KT98-VALUE  PIC X(22).
              03  KT98-SENTAKU.
                04  KT98-VALUE  PIC X(30).
              03  KT98-SELNUM   OCCURS  10.
                04  KT98-VALUE  PIC X(3).
              03  KT98-JIINCOB.
                04  KT98-JIIN.
                  05  KT98-VALUE  PIC X(16).
                04  KT98-JIIN-COUNT  PIC S9(9)   BINARY.
                04  KT98-JIIN-ITEM
                        PIC X(16)   OCCURS  10 TIMES.
              03  KT98-USERCOB.
                04  KT98-USER.
                  05  KT98-VALUE  PIC X(10).
                04  KT98-USER-COUNT  PIC S9(9)   BINARY.
                04  KT98-USER-ITEM
                        PIC X(10)   OCCURS  10 TIMES.
              03  KT98-SCROLLEDWINDOW26.
                04  KT98-HPOS   PIC S9(9)   BINARY.
                04  KT98-VPOS   PIC S9(9)   BINARY.
                04  KT98-LIST.
                  05  KT98-COUNT  PIC S9(9)   BINARY.
                  05  KT98-ROW    PIC S9(9)   BINARY.
                  05  KT98-ROWATTR PIC S9(9)   BINARY.
                  05  KT98-ITEM       OCCURS  200 TIMES.
                    06  KT98-NO     PIC X(3).
                    06  KT98-NAME   PIC X(70).
                    06  KT98-TANI   PIC X(6).
                    06  KT98-KISOTEN PIC X(11).
                    06  KT98-SETCD    PIC X(10).
                    06  KT98-SRYKBN  PIC X(2).
                  05  KT98-SELECT
                          PIC X       OCCURS  200 TIMES.
              03  KT98-NEXT.
                04  KT98-NEXT-STYLE  PIC X(20).
                04  KT98-NEXT-VALUE  PIC X(20).
