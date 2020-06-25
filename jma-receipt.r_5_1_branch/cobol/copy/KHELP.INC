        02  KHELP.
   *******  02  KHELP-FIXED32.
              03  KHELP-INPUT.
                04  KHELP-VALUE PIC X(30).
              03  KHELP-HELP.
                04  KHELP-VALUE PIC X(2000).
              03  KHELP-SELNUM.
                04  KHELP-VALUE PIC S9(4).
              03  KHELP-SCROLLEDWINDOW26.
                04  KHELP-MENULIST.
                  05  KHELP-COUNT PIC S9(9)   BINARY.
                  05  KHELP-ROW   PIC S9(9)   BINARY.
                  05  KHELP-ROWATTR   PIC S9(9)   BINARY.
                  05  KHELP-ITEM      OCCURS  100 TIMES.
                    06  KHELP-TMENUNUM   PIC X(5).
                    06  KHELP-TMENU      PIC X(30).
                  05  KHELP-SELECT
                          PIC X       OCCURS  100 TIMES.
              03  KHELP-HEAD.
                04  KHELP-HEAD-STYLE PIC X(20).
                04  KHELP-HEAD-VALUE PIC X(10).
