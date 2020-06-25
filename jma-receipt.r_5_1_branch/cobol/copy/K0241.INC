        02  K0241.
            03  K0241-TITLE PIC X(256).
            03  K0241-POPUP-SUMMARY PIC X(256).
            03  K0241-POPUP-BODY    PIC X(256).
            03  K0241-POPUP-ICON    PIC X(256).
            03  K0241-POPUP-TIMEOUT PIC S9(9)   BINARY.
            03  K0241-FIXED1.
              04  K0241-B01.
                05  K0241-STATE PIC S9(9)   BINARY.
              04  K0241-SCROLLEDWINDOW2.
                05  K0241-KANRILIST.
                  06  K0241-COUNT PIC S9(9)   BINARY.
                  06  K0241-ROW   PIC S9(9)   BINARY.
                  06  K0241-SELECTDATA
                          PIC X       OCCURS  50 TIMES.
                  06  K0241-ITEM      OCCURS  50 TIMES.
                    07  K0241-TNUM       PIC X(3).
                    07  K0241-TNAME      PIC X(100).
                    07  K0241-TTOUYYMD   PIC X(10).
                    07  K0241-TRENNUM    PIC X(3).
                    07  K0241-THOSPNAME  PIC X(200).
              04  K0241-SELNUM-G.
                05  K0241-SELNUM     PIC 9(3).
              04  K0241-B02.
                05  K0241-STATE PIC S9(9)   BINARY.
              04  K0241-B12.
                05  K0241-STATE PIC S9(9)   BINARY.
              04  K0241-MIDASI.
                05  K0241-TEXTDATA  PIC X(80).
