        02  XF01X.
     *****  02  XF01X-FIXED32.
              03  XF01X-SELNUM.
                04  XF01X-VALUE PIC S9(3).
              03  XF01X-SCROLLEDWINDOW26.
                04  XF01X-HPOS  PIC S9(9)   BINARY.
                04  XF01X-VPOS  PIC S9(9)   BINARY.
                04  XF01X-LIST.
                  05  XF01X-LIST-COUNT PIC S9(9)   BINARY.
                  05  XF01X-LIST-ROW   PIC S9(9)   BINARY.
                  05  XF01X-ITEM      OCCURS  20 TIMES.
                    06  XF01X-NO        PIC X(3).
                    06  XF01X-ADR       PIC X(22).
                    06  XF01X-PORT      PIC X(22).
                    06  XF01X-MEMO      PIC X(22).
                  05  XF01X-LIST-SELECT
                          PIC X       OCCURS  20 TIMES.
              03  XF01X-SCODE-COMBO.
                04  XF01X-SCODE.
                  05  XF01X-VALUE PIC X(20).
                04  XF01X-SCODE-COUNT PIC S9(9)   BINARY.
                04  XF01X-SCODE-ITEM
                        PIC X(20)   OCCURS  10 TIMES.
