        02  N41.
      ***** 02  N41-FIXED6.
              03  N41-B03.
                04  N41-B03-STATE   PIC S9(9)   BINARY.
              03  N41-B12.
                04  N41-B12-STATE   PIC S9(9)   BINARY.
              03  N41-HEADMENU.
                04  N41-VALUE   PIC X(30).
              03  N41-SELNUM.
                04  N41-VALUE   PIC S9(4).
              03  N41-MENU.
                04  N41-VALUE   PIC X(30).
              03  N41-HELP.
                04  N41-VALUE   PIC X(2000).
              03  N41-MENUNUM1-CPG.
                04  N41-MENUNUM1-CP-STATE   PIC S9(9)   BINARY.
                04  N41-MENUNUM1-CP   PIC S9(4).
              03  N41-MENU1-CPG.
                04  N41-MENU1-CP-STATE   PIC S9(9)   BINARY.
                04  N41-MENU1-CP         PIC X(30).
              03  N41-MENUNUM2-CPG.
                04  N41-MENUNUM2-CP-STATE  PIC S9(9)   BINARY.
                04  N41-MENUNUM2-CP        PIC S9(4).
              03  N41-MENU2-CPG.
                04  N41-MENU2-CP-STATE   PIC S9(9)   BINARY.
                04  N41-MENU2-CP         PIC X(30).
              03  N41-HELPKBN-COMB.
                04  N41-HELPKBN.
                  05  N41-VALUE   PIC X(20).
                04  N41-HELPKBN-COUNT   PIC S9(9)   BINARY.
                04  N41-HELPKBN-ITEM
                        PIC X(20)   OCCURS  5 TIMES.
              03  N41-LDNAME-COMB.
                04  N41-LDNAME.
                  05  N41-VALUE   PIC X(40).
                04  N41-LDNAME-COUNT   PIC S9(9)   BINARY.
                04  N41-LDNAME-ITEM
                        PIC X(40)   OCCURS  50 TIMES.
              03  N41-SELNUM2.
                04  N41-VALUE   PIC S9(4).
              03  N41-MENU2.
                04  N41-VALUE   PIC X(30).
              03  N41-LDNAME-CP-COMB.
                04  N41-LDNAME-CP.
                  05  N41-VALUE   PIC X(40).
                04  N41-LDNAME-CP-STATE   PIC S9(9)   BINARY.
                04  N41-LDNAME-CP-COUNT   PIC S9(9)   BINARY.
                04  N41-LDNAME-CP-ITEM
                        PIC X(40)   OCCURS  50 TIMES.
              03  N41-HELPKBN-CP-COMB.
                04  N41-HELPKBN-CP.
                  05  N41-VALUE   PIC X(20).
                04  N41-HELPKBN-CP-STATE   PIC S9(9)   BINARY.
                04  N41-HELPKBN-CP-COUNT   PIC S9(9)   BINARY.
                04  N41-HELPKBN-CP-ITEM
                        PIC X(20)   OCCURS  5 TIMES.
              03  N41-SCROLLEDWINDOW2.
                04  N41-MENU2LIST.
                  05  N41-MENU2LIST-COUNT   PIC S9(9)   BINARY.
                  05  N41-MENU2LIST-ROW PIC S9(9)   BINARY.
                  05  N41-MENU2LIST-ROWATTR PIC S9(9)   BINARY.
                  05  N41-MENU2LIST-ITEM        OCCURS  300 TIMES.
                    06  N41-TMENUNUM2  PIC X(4).
                    06  N41-TMENU2      PIC X(30).
                  05  N41-MENU2LIST-SELECT
                          PIC X       OCCURS  300 TIMES.
              03  N41-SCROLLEDWINDOW1.
                04  N41-HEADLIST.
                  05  N41-HEADLIST-COUNT   PIC S9(9)   BINARY.
                  05  N41-HEADLIST-ROW PIC S9(9)   BINARY.
                  05  N41-HEADLIST-ROWATTR PIC S9(9)   BINARY.
                  05  N41-HEADLIST-ITEM        OCCURS  300 TIMES.
                    06  N41-TMENUNUM  PIC X(4).
                    06  N41-TMENU      PIC X(30).
                  05  N41-HEADLIST-SELECT
                          PIC X       OCCURS  300 TIMES.
