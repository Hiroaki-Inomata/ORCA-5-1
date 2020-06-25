        02  W25.
     *******02  W25-FIXED32.
              03  W25-INPUT.
                04  W25-VALUE   PIC X(22).
              03  W25-SENTAKU.
                04  W25-VALUE   PIC X(30).
              03  W25-SELNUM1.
                04  W25-VALUE   PIC X(3).
              03  W25-JIINCOB.
                04  W25-JIIN.
                  05  W25-VALUE   PIC X(16).
                04  W25-JIIN-COUNT   PIC S9(9)   BINARY.
                04  W25-JIIN-ITEM
                        PIC X(16)   OCCURS  10 TIMES.
              03  W25-SCROLLEDWINDOW26.
                04  W25-HPOS    PIC S9(9)   BINARY.
                04  W25-VPOS    PIC S9(9)   BINARY.
                04  W25-LIST.
                  05  W25-COUNT   PIC S9(9)   BINARY.
                  05  W25-ROW PIC S9(9)   BINARY.
                  05  W25-ROWATTR PIC S9(9)   BINARY.
                  05  W25-ITEM        OCCURS  200 TIMES.
                    06  W25-NO      PIC X(3).
                    06  W25-NAME    PIC X(70).
                    06  W25-TANI    PIC X(6).
                    06  W25-KISOTEN  PIC X(10).
                    06  W25-SETCD    PIC X(10).
                    06  W25-SRYKBN   PIC X(02).
                  05  W25-SELECT
                          PIC X       OCCURS  200 TIMES.
              03  W25-NEXT.
                04  W25-NEXT-STYLE   PIC X(20).
                04  W25-NEXT-VALUE   PIC X(20).
              03  W25-USERCOB.
                04  W25-USER.
                  05  W25-VALUE   PIC X(10).
                04  W25-USER-COUNT   PIC S9(9)   BINARY.
                04  W25-USER-ITEM
                        PIC X(10)   OCCURS  10 TIMES.
