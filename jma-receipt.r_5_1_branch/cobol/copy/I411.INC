        02  I411.
            03  I411-TITLE  PIC X(256).
            03  I411-POPUP-SUMMARY  PIC X(256).
            03  I411-POPUP-BODY PIC X(1024).
            03  I411-POPUP-ICON PIC X(256).
            03  I411-POPUP-TIMEOUT  PIC S9(9)   BINARY.
      ***** 03  I411-FIXED4.
              03  I411-SELNUM.
                04  I411-VALUE  PIC X(4).
              03  I411-SCROLLEDWINDOW1.
                04  I411-HPOS   PIC S9(9)   BINARY.
                04  I411-VPOS   PIC S9(9)   BINARY.
                04  I411-HKNLIST.
                  05  I411-COUNT  PIC S9(9)   BINARY.
                  05  I411-ROW    PIC S9(9)   BINARY.
                  05  I411-ITEM       OCCURS  30 TIMES.
                    06  I411-HKNCOMBI PIC X(4).
                    06  I411-HKNCOMBIMEI PIC X(50).
                  05  I411-SELECT
                          PIC X       OCCURS  30 TIMES.
