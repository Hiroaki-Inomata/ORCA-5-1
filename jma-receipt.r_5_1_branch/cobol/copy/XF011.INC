        02  XF011.
     *****  02  XF011-FIXED1.
              03  XF011-SELNUM.
                04  XF011-VALUE PIC S9(3).
              03  XF011-SCROLLEDWINDOW1.
                04  XF011-HPOS  PIC S9(9)   BINARY.
                04  XF011-VPOS  PIC S9(9)   BINARY.
                04  XF011-BUNREILIST.
                  05  XF011-BUNREI-COUNT PIC S9(9)   BINARY.
                  05  XF011-BUNREI-ROW   PIC S9(9)   BINARY.
                  05  XF011-BUNREI-ROWATTR PIC S9(9)   BINARY.
                  05  XF011-ITEM      OCCURS  200 TIMES.
                    06  XF011-TRENNUM   PIC X(3).
                    06  XF011-TCOMMENT  PIC X(400).
                  05  XF011-BUNREI-SELECT
                          PIC X       OCCURS  200 TIMES.
              03   XF011-NEXT.
                04   XF011-NEXT-STYLE   PIC X(20).
                04   XF011-NEXT-VALUE   PIC X(20).
