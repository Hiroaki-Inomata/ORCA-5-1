        02  M98.
            03  M98-FIXED32.
              04  M98-SCROLLEDWINDOW26.
                05  M98-HPOS    PIC S9(9)   BINARY.
                05  M98-VPOS    PIC S9(9)   BINARY.
                05  M98-LIST.
                  06  M98-COUNT   PIC S9(9)   BINARY.
                  06  M98-ROW PIC S9(9)   BINARY.
                  06  M98-ITEM        OCCURS  100 TIMES.
                    07  M98-VALUE0  PIC X(3).
                    07  M98-VALUE1  PIC X(9).
                    07  M98-VALUE2  PIC X(9).
                    07  M98-VALUE3  PIC X(10).
                    07  M98-VALUE4  PIC X(6).
                    07  M98-VALUE5  PIC X(7).
                    07  M98-VALUE6  PIC X(3).
                    07  M98-VALUE7  PIC X(6).
                    07  M98-VALUE8  PIC X(35).
                  06  M98-SELECT
                          PIC X       OCCURS  100 TIMES.
              03  M98-B09.
                04  M98-B09-LABEL   PIC X(10).
