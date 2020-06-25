        02  KT011.
     ****   02  KT011-FIXED10.
              03  KT011-SELNUM.
                04  KT011-VALUE PIC X(3).
              03  KT011-TOUMEI.
                04  KT011-VALUE PIC X(100).
              03  KT011-KNKMEI.
                04  KT011-VALUE PIC X(100).
              03  KT011-SCROLLEDWINDOW3.
                04  KT011-HPOS  PIC S9(9)   BINARY.
                04  KT011-VPOS  PIC S9(9)   BINARY.
                04  KT011-SAYOKIJYO.
                  05  KT011-VALUE PIC X(2000).
              03  KT011-SCROLLEDWINDOW2.
                04  KT011-HPOS  PIC S9(9)   BINARY.
                04  KT011-VPOS  PIC S9(9)   BINARY.
                04  KT011-SYOJYOU.
                  05  KT011-VALUE PIC X(2000).
              03  KT011-SCROLLEDWINDOW1.
                04  KT011-HPOS  PIC S9(9)   BINARY.
                04  KT011-VPOS  PIC S9(9)   BINARY.
                04  KT011-MEDLIST.
                  05  KT011-COUNT PIC S9(9)   BINARY.
                  05  KT011-ROW   PIC S9(9)   BINARY.
                  05  KT011-ITEM      OCCURS  30 TIMES.
                    06  KT011-TSEQNO  PIC X(3).
                    06  KT011-TTOUMEI PIC X(40).
                    06  KT011-TKNKMEI PIC X(40).
                    06  KT011-TTOUYMD PIC X(9).
                    06  KT011-TSYOJYOUCD PIC X(7).
                  05  KT011-SELECT
                          PIC X       OCCURS  30 TIMES.
              03  KT011-B12.
                04  KT011-B12-STATE  PIC S9(9)   BINARY.
