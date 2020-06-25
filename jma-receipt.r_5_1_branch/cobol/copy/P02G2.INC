        02  P02G2.
      ******02  P02G2-FIXED10.
              03  P02G2-PTNUM.
                04  P02G2-VALUE PIC X(20).
              03  P02G2-ADRS1.
                04  P02G2-VALUE PIC X(100).
              03  P02G2-NAME.
                04  P02G2-VALUE PIC X(50).
              03  P02G2-ADRS2.
                04  P02G2-VALUE PIC X(100).
              03  P02G2-BIRTHDAY.
                04  P02G2-VALUE PIC X(9).
              03  P02G2-SEX.
                04  P02G2-VALUE PIC X(4).
              03  P02G2-SCROLLEDWINDOW1.
                04  P02G2-HPOS  PIC S9(9)   BINARY.
                04  P02G2-VPOS  PIC S9(9)   BINARY.
                04  P02G2-GRPLIST.
                  05  P02G2-GRPLIST-COUNT PIC S9(9)   BINARY.
                  05  P02G2-GRPLIST-ROW   PIC S9(9)   BINARY.
                  05  P02G2-ITEM      OCCURS  10 TIMES.
                    06  P02G2-LNUM      PIC X(2).
                    06  P02G2-LHOSPNAME PIC X(10).
                    06  P02G2-LPTNUM    PIC X(20).
                    06  P02G2-LPTNAME   PIC X(50).
                  05  P02G2-GRPLIST-SELECT
                          PIC X       OCCURS  10 TIMES.
              03  P02G2-GRPHOSPNUM-COMB.
                04  P02G2-GRPHOSPNUM.
                  05  P02G2-VALUE PIC X(20).
                04  P02G2-GRPHOSPNUM-COUNT PIC S9(9)   BINARY.
                04  P02G2-GRPHOSPNUM-ITEM
                        PIC X(20)   OCCURS  9 TIMES.
