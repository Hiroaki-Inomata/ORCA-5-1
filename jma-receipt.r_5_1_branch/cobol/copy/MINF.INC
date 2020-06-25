            02  MINF.
              03  MINF-B06.
                04  MINF-B06-STATE     PIC S9(9)   BINARY.
              03  MINF-B07.
                04  MINF-B07-STATE     PIC S9(9)   BINARY.
              03  MINF-LABEL      PIC X(06).
              03  MINF-TAISYOYMD  PIC X(22).
              03  MINF-SCROLLEDWINDOW26.
                05  MINF-HPOS          PIC S9(9)   BINARY.
                05  MINF-VPOS          PIC S9(9)   BINARY.
                05  MINF-LIST.
                  06  MINF-COUNT       PIC S9(9)   BINARY.
                  06  MINF-ROW         PIC S9(9)   BINARY.
                  06  MINF-ITEM        OCCURS  100 TIMES.
                    07  MINF-YMD        PIC X(6).
                    07  MINF-BYOGEN        PIC X(22).
                    07  MINF-NAME        PIC X(18).
                    07  MINF-SEX        PIC X(2).
                    07  MINF-SHINRYO        PIC X(2).
                    07  MINF-YAKUZAI        PIC X(2).
                    07  MINF-BYOMEI        PIC X(60).
                    07  MINF-STRYMD        PIC X(10).
                  06  MINF-SELECT      PIC X   OCCURS  100 TIMES.
              03  MINF-SRYYMD     PIC X(9).
