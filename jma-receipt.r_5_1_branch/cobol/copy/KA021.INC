        02  KA021.
      ***** 02  KA021-FIXED1.
              03  KA021-SELNUM.
                04  KA021-VALUE PIC S9(4).
              03  KA021-SCROLLEDWINDOW3.
                04  KA021-HPOS  PIC S9(9)   BINARY.
                04  KA021-VPOS  PIC S9(9)   BINARY.
                04  KA021-MEILIST.
                  05  KA021-MEILIST-COUNT PIC S9(9)   BINARY.
                  05  KA021-MEILIST-ROW   PIC S9(9)   BINARY.
                  05  KA021-ITEM      OCCURS  400 TIMES.
                    06  KA021-MNUM      PIC X(3).
                    06  KA021-MKBN      PIC X(2).
                    06  KA021-MEISAI   PIC X(200).
                  05  KA021-MEILIST-SELECT
                          PIC X       OCCURS  400 TIMES.
              03  KA021-NEXT.
                04  KA021-NEXT-STYLE   PIC X(20).
                04  KA021-NEXT-VALUE   PIC X(20).
              03  KA021-PRTYMD.
                04  KA021-VALUE PIC X(30).
              03  KA021-BTUNAME.
                04  KA021-VALUE PIC X(40).
