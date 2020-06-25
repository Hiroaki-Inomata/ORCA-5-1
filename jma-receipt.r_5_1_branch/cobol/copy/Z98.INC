        02  Z98.
      ******02  Z98-FIXED32.
              03  Z98-INPUT.
                04  Z98-VALUE   PIC X(22).
              03  Z98-SENTAKU.
                04  Z98-VALUE   PIC X(30).
              03  Z98-SELNUM1.
                04  Z98-VALUE   PIC X(3).
              03  Z98-JIINCOB.
                04  Z98-JIIN.
                  05  Z98-VALUE   PIC X(16).
                04  Z98-JIIN-COUNT   PIC S9(9)   BINARY.
                04  Z98-JIIN-ITEM
                        PIC X(16)   OCCURS  10 TIMES.
              03  Z98-SCROLLEDWINDOW26.
                04  Z98-HPOS    PIC S9(9)   BINARY.
                04  Z98-VPOS    PIC S9(9)   BINARY.
                04  Z98-LIST.
                  05  Z98-COUNT   PIC S9(9)   BINARY.
                  05  Z98-ROW PIC S9(9)   BINARY.
                  05  Z98-ROWATTR PIC S9(9)   BINARY.
                  05  Z98-ITEM        OCCURS  200 TIMES.
                    06  Z98-NO      PIC X(3).
                    06  Z98-NAME    PIC X(70).
                    06  Z98-TANI    PIC X(6).
                    06  Z98-KISOTEN  PIC X(11).
                    06  Z98-SETCD    PIC X(10).
                    06  Z98-SRYKBN   PIC X(02).
                  05  Z98-SELECT
                          PIC X       OCCURS  200 TIMES.
              03  Z98-NEXT.
                04  Z98-NEXT-STYLE   PIC X(20).
                04  Z98-NEXT-VALUE   PIC X(20).
              03  Z98-USERCOB.
                04  Z98-USER.
                  05  Z98-VALUE   PIC X(10).
                04  Z98-USER-COUNT   PIC S9(9)   BINARY.
                04  Z98-USER-ITEM
                        PIC X(10)   OCCURS  10 TIMES.
