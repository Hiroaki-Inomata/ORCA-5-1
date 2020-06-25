        02  U02X.
      ***  02  U02X-FIXED32.
              03  U02X-SELNUM.
                04  U02X-VALUE  PIC S9(3).
              03  U02X-SCROLLEDWINDOW26.
                04  U02X-LIST.
                  05  U02X-LIST-COUNT  PIC S9(9)   BINARY.
                  05  U02X-LIST-ROW    PIC S9(9)   BINARY.
                  05  U02X-LIST-ROWATTR  PIC S9(9)   BINARY.
                  05  U02X-ITEM       OCCURS  20 TIMES.
                    06  U02X-NO     PIC X(3).
                    06  U02X-ADR    PIC X(22).
                    06  U02X-PORT   PIC X(22).
                    06  U02X-MEMO   PIC X(22).
                  05  U02X-LIST-SELECT PIC X       OCCURS  20 TIMES.
              03  U02X-SCODE-COMBO.
                04  U02X-SCODE.
                  05  U02X-VALUE  PIC X(20).
                04  U02X-SCODE-COUNT  PIC S9(9)   BINARY.
                04  U02X-SCODE-ITEM   PIC X(20)   OCCURS  10 TIMES.
