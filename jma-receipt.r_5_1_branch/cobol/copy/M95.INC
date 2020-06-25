        02  M95.
   ******** 02  M95-FIXED1.
            03  M95-BGCOLORS.
              04  M95-VALUE   PIC X(20).
              03  M95-NOTEBOOK1.
                04  M95-PAGENO  PIC S9(9)   BINARY.
                04  M95-FIXED2.
                  05  M95-SYSYMD.
                    06  M95-VALUE   PIC X(10).
                  05  M95-CALENDAR1.
                    06  M95-YEAR    PIC S9(9)   BINARY.
                    06  M95-MONTH   PIC S9(9)   BINARY.
                    06  M95-DAY PIC S9(9)   BINARY.
                  05  M95-JIKANKBN-COMB.
                    06  M95-JIKANKBN.
                      07  M95-VALUE   PIC X(30).
                    06  M95-JIKANKBN-COUNT   PIC S9(9)   BINARY.
                    06  M95-JIKANKBN-ITEM
                            PIC X(30)   OCCURS  9 TIMES.
                04  M95-FIXED3.
                  05  M95-SRYKA.
                    06  M95-VALUE   PIC X(2).
                  05  M95-RENNUM.
                    06  M95-VALUE   PIC X(4).
                  05  M95-DRCD-COMB.
                    06  M95-DRCD.
                      07  M95-VALUE   PIC X(30).
                    06  M95-DRCD-COUNT   PIC S9(9)   BINARY.
                    06  M95-DRCD-ITEM
                            PIC X(30)   OCCURS  99 TIMES.
                  05  M95-SYORIMEI.
                    06  M95-SYORIMEI-STYLE   PIC X(20).
                    06  M95-SYORIMEI-VALUE   PIC X(30).
                  05  M95-SCROLLEDWINDOW1.
                    06  M95-HPOS    PIC S9(9)   BINARY.
                    06  M95-VPOS    PIC S9(9)   BINARY.
                    06  M95-SYSLIST.
                      07  M95-SYSLIST-COUNT   PIC S9(9)   BINARY.
                      07  M95-SYSLIST-ROW PIC S9(9)   BINARY.
                      07  M95-SYSLIST-ROWATTR PIC S9(9)   BINARY.
                      07  M95-ITEM        OCCURS  100 TIMES.
                        08  M95-TSRYKA  PIC X(2).
                        08  M95-TSRYKAMEI  PIC X(20).
                        08  M95-TRENNUM    PIC X(4).
                        08  M95-TDRCDMEI  PIC X(50).
                      07  M95-SYSLIST-SELECT
                              PIC X       OCCURS  100 TIMES.
                04  M95-FIXED4.
                  05  M95-TAINLIST.
                    06  M95-TAINLIST-COUNT   PIC S9(9)   BINARY.
                    06  M95-TAINLIST-ROW PIC S9(9)   BINARY.
                    06  M95-TAINLIST-ROWATTR PIC S9(9)   BINARY.
                    06  M95-TAINITEM    OCCURS  9 TIMES.
                      07  M95-TAINNO   PIC X(1).
                      07  M95-TAINNAME PIC X(13).
                    06  M95-TAINLIST-SELECT
                            PIC X       OCCURS  9 TIMES.
                  05  M95-TAINNUM.
                    06  M95-VALUE   PIC X(1).
                04  M95-N03-STATE     PIC S9(9)   BINARY.

