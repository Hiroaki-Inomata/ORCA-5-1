        02  K05.
      ***** 02  K05-FIXED2.
              03  K05-SETCODE.
                04  K05-VALUE   PIC X(6).
              03  K05-NAME.
                04  K05-VALUE   PIC X(80).
              03  K05-SRYKBN    OCCURS  23.
                04  K05-VALUE   PIC X(2).
              03  K05-INPUTCD   OCCURS  23.
                04  K05-VALUE   PIC X(54).
              03  K05-MEISYO    OCCURS  23.
                04  K05-VALUE   PIC X(140).
              03  K05-SURYO-G     OCCURS  23.
                04  K05-SURYO-STYLE   PIC X(20).
                04  K05-SURYO   PIC X(24).
              03  K05-NYUGAIKBNCOMB.
                04  K05-NYUGAIKBN.
                  05  K05-VALUE   PIC X(10).
                04  K05-NYUGAIKBN-COUNT   PIC S9(9)   BINARY.
                04  K05-NYUGAIKBN-ITEM
                        PIC X(10)   OCCURS  2 TIMES.
              03  K05-SCROLLEDWINDOW1.
                04  K05-RRKLIST.
                  05  K05-RRKLIST-COUNT   PIC S9(9)   BINARY.
                  05  K05-RRKLIST-ROW     PIC S9(9)   BINARY.
                  05  K05-RRKLIST-ROWATTR PIC S9(9)   BINARY.
                  05  K05-ITEM        OCCURS  10 TIMES.
                    06  K05-TRENNUM PIC X(3).
                    06  K05-TYUKOSTYMD  PIC X(12).
                    06  K05-TYUKOEDYMD  PIC X(12).
                  05  K05-RRKLIST-SELECT
                          PIC X       OCCURS  10 TIMES.
              03  K05-ROUJINCOMB.
                04  K05-ROUJIN.
                  05  K05-VALUE   PIC X(10).
                04  K05-ROUJIN-COUNT   PIC S9(9)   BINARY.
                04  K05-ROUJIN-ITEM
                        PIC X(10)   OCCURS  3 TIMES.
              03  K05-SELNUM.
                04  K05-VALUE   PIC S9(3).
              03  K05-YUKOSTYMD.
                04  K05-VALUE   PIC X(13).
              03  K05-YUKOEDYMD.
                04  K05-VALUE   PIC X(13).
              03  K05-SYORINAME.
                04  K05-SYORINAME-STYLE   PIC X(20).
                04  K05-SYORINAME-VALUE   PIC X(20).
