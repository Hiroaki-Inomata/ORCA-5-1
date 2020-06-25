        02  I48.
     ****   02  I48-FIXED32.
              03  I48-INPUT.
                04  I48-VALUE   PIC X(22).
              03  I48-SENTAKU.
                04  I48-VALUE   PIC X(30).
              03  I48-SELNUM.
                04  I48-VALUE   PIC X(3).
              03  I48-SCROLLEDWINDOW26.
                04  I48-LIST.
                  05  I48-COUNT   PIC S9(9)   BINARY.
                  05  I48-ROW PIC S9(9)   BINARY.
                  05  I48-ROWATTR PIC S9(9)   BINARY.
                  05  I48-ITEM        OCCURS  200 TIMES.
                    06  I48-NO      PIC X(3).
                    06  I48-NAME    PIC X(70).
                    06  I48-TANI    PIC X(6).
                    06  I48-KISOTEN PIC X(10).
                  05  I48-SELECT
                          PIC X       OCCURS  200 TIMES.
