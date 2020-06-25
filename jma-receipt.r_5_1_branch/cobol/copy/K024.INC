        02  K024.
            03  K024-TITLE  PIC X(256).
            03  K024-POPUP-SUMMARY  PIC X(256).
            03  K024-POPUP-BODY PIC X(256).
            03  K024-POPUP-ICON PIC X(256).
            03  K024-POPUP-TIMEOUT  PIC S9(9)   BINARY.
            03  K024-FIXED1.
              04  K024-B01.
                05  K024-STATE  PIC S9(9)   BINARY.
              04  K024-SCROLLEDWINDOW2.
                05  K024-KANRILIST.
                  06  K024-KANRILIST-COUNT  PIC S9(9)   BINARY.
                  06  K024-KANRILIST-ROW    PIC S9(9)   BINARY.
                  06  K024-KANRILIST-SELECT
                          PIC X       OCCURS  100 TIMES.
                  06  K024-ITEM       OCCURS  100 TIMES.
                    07  K024-TNUM       PIC X(3).
                    07  K024-TTOUYYMD   PIC X(16).
                    07  K024-THOSPNAME  PIC X(200).
              04  K024-SCROLLEDWINDOW3.
                05  K024-MEILIST.
                  06  K024-MEILIST-COUNT  PIC S9(9)   BINARY.
                  06  K024-MEILIST-ROW    PIC S9(9)   BINARY.
                  06  K024-MEILIST-SELECT
                          PIC X       OCCURS  500 TIMES.
                  06  K024-ITEM       OCCURS  500 TIMES.
                    07  K024-TBANGO     PIC X(3).
                    07  K024-TMEISAI    PIC X(80).
              04  K024-B06.
                05  K024-STATE  PIC S9(9)   BINARY.
              04  K024-B07.
                05  K024-STATE  PIC S9(9)   BINARY.
              04  K024-SELNUM-G.
                05  K024-SELNUM     PIC 9(3).
              03  K024-MIDASI.
                04  K024-MIDASI-STYLE  PIC X(20).
                04  K024-MIDASI-TEXTDATA   PIC X(20).
              03  K024-B010.
                04  K024-STATE  PIC S9(9)   BINARY.
