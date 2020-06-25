        02  KT012.
     ****** 02  KT012-FIXED1.
              03  KT012-DOSEL.
                04  KT012-VALUE PIC X(40).
              03  KT012-SCROLLEDWINDOW2.
                04  KT012-HPOS  PIC S9(9)   BINARY.
                04  KT012-VPOS  PIC S9(9)   BINARY.
                04  KT012-RRKLIST.
                  05  KT012-RRKLIST-COUNT PIC S9(9)   BINARY.
                  05  KT012-RRKLIST-ROW   PIC S9(9)   BINARY.
                  05  KT012-RRKLIST-ROWATTR PIC S9(9)   BINARY.
                  05  KT012-ITEM      OCCURS  150 TIMES.
                    06  KT012-NO        PIC X(4).
                    06  KT012-RRKYMD    PIC X(16).
                    06  KT012-RRKSRYKA  PIC X(2).
                    06  KT012-RRKHKNCOMBI PIC X(4).
                  05  KT012-RRKLIST-SELECT
                          PIC X       OCCURS  150 TIMES.
              03  KT012-SCROLLEDWINDOW3.
                04  KT012-HPOS  PIC S9(9)   BINARY.
                04  KT012-VPOS  PIC S9(9)   BINARY.
                04  KT012-MEILIST.
                  05  KT012-MEILIST-COUNT PIC S9(9)   BINARY.
                  05  KT012-MEILIST-ROW   PIC S9(9)   BINARY.
                  05  KT012-MEILIST-ROWATTR PIC S9(9)   BINARY.
                  05  KT012-ITEM      OCCURS  800 TIMES.
                    06  KT012-MNO       PIC X(3).
                    06  KT012-MEISAI    PIC X(90).
                  05  KT012-MEILIST-SELECT
                          PIC X       OCCURS  800 TIMES.
              03  KT012-SELNUM.
                04  KT012-VALUE PIC S9(4).
              03  KT012-SRYKBN-COMB.
                04  KT012-SRYKBN.
                  05  KT012-VALUE PIC X(23).
                04  KT012-SRYKBN-COUNT PIC S9(9)   BINARY.
                04  KT012-SRYKBN-ITEM
                        PIC X(23)   OCCURS  20 TIMES.
              03  KT012-NYUGAIKBN-COMB.
                04  KT012-NYUGAIKBN.
                  05  KT012-VALUE PIC X(10).
                04  KT012-NYUGAIKBN-COUNT PIC S9(9)   BINARY.
                04  KT012-NYUGAIKBN-ITEM
                        PIC X(10)   OCCURS  2 TIMES.
              03  KT012-GRPHOSPNUM-COMB.
                04  KT012-GRPHOSPNUM.
                  05  KT012-VALUE PIC X(23).
                04  KT012-GRPHOSPNUM-COUNT PIC S9(9)   BINARY.
                04  KT012-GRPHOSPNUM-ITEM
                        PIC X(23)   OCCURS  9 TIMES.
              03  KT012-B03.
                04  KT012-B03-STATE PIC S9(9)   BINARY.
                04  KT012-B03-LABEL PIC X(20).
