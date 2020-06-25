        02  P02G.
      ***** 02  P02G-FIXED1.
              03  P02G-SELNUM.
                04  P02G-VALUE  PIC S9(3).
              03  P02G-SCROLLEDWINDOW1.
                04  P02G-HPOS   PIC S9(9)   BINARY.
                04  P02G-VPOS   PIC S9(9)   BINARY.
                04  P02G-GRPLIST.
                  05  P02G-GRPLIST-COUNT  PIC S9(9)   BINARY.
                  05  P02G-GRPLIST-ROW    PIC S9(9)   BINARY.
                  05  P02G-ITEM       OCCURS  30 TIMES.
                    06  P02G-LNUM      PIC X(3).
                    06  P02G-LPTNUM    PIC X(20).
                    06  P02G-LPTNAME   PIC X(50).
                    06  P02G-LBIRTHDAY  PIC X(9).
                    06  P02G-LADRS     PIC X(50).
                    06  P02G-LTEL      PIC X(15).
                    06  P02G-LHOSPNAME PIC X(10).
                  05  P02G-GRPLIST-SELECT
                          PIC X       OCCURS  30 TIMES.
