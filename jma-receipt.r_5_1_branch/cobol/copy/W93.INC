        02  W93.
            03  W93-TITLE   PIC X(256).
            03  W93-POPUP-SUMMARY   PIC X(256).
            03  W93-POPUP-BODY  PIC X(1024).
            03  W93-POPUP-ICON  PIC X(256).
            03  W93-POPUP-TIMEOUT   PIC S9(9)   BINARY.
            03  W93-FIXED1.
              04  W93-RECEDEN-COMB.
                05  W93-RECEDEN.
                  07  W93-VALUE   PIC X(20).
                05  W93-RECEDEN-COUNT   PIC S9(9)   BINARY.
                05  W93-RECEDEN-ITEM
                        PIC X(20)   OCCURS  10 TIMES.
              04  W93-JMAMR-COMB.
                05  W93-JMAMR.
                  07  W93-VALUE   PIC X(20).
                05  W93-JMAMR-COUNT  PIC S9(9)   BINARY.
                05  W93-JMAMR-ITEM
                        PIC X(20)   OCCURS  10 TIMES.
