        02  W92.
            03  W92-TITLE   PIC X(256).
            03  W92-POPUP-SUMMARY   PIC X(256).
            03  W92-POPUP-BODY  PIC X(1024).
            03  W92-POPUP-ICON  PIC X(256).
            03  W92-POPUP-TIMEOUT   PIC S9(9)   BINARY.
            03  W92-FIXED1.
              04  W92-FILEDIR.
                05  W92-VALUE   PIC X(200).
              04  W92-EFOUT-COMB.
                05  W92-EFOUT.
                  07  W92-VALUE   PIC X(20).
                05  W92-EFOUT-COUNT   PIC S9(9)   BINARY.
                05  W92-EFOUT-ITEM
                        PIC X(20)   OCCURS  10 TIMES.
              04  W92-SRYCD-OCC OCCURS 2.
                05  W92-SRYCD.
                  06  W92-VALUE   PIC X(9).
                05  W92-NAME.
                  06  W92-VALUE   PIC X(100).
              04  W92-NUMBER-COMB.
                05  W92-NUMBER.
                  07  W92-VALUE   PIC X(20).
                05  W92-NUMBER-COUNT   PIC S9(9)   BINARY.
                05  W92-NUMBER-ITEM
                        PIC X(20)   OCCURS  5 TIMES.
              04  W92-KOH-RECEIPT-COMB.
                05  W92-KOH-RECEIPT.
                  07  W92-VALUE   PIC X(20).
                05  W92-KOH-RECEIPT-COUNT   PIC S9(9)   BINARY.
                05  W92-KOH-RECEIPT-ITEM
                        PIC X(20)   OCCURS  5 TIMES.
