        02  C50.
   *******  02  C50-FIXED1.
              03  C50-BUNNUM.
                04  C50-VALUE   PIC S9(3).
              03  C50-SRYYM.
                04  C50-VALUE   PIC X(6).
              03  C50-JYURRKLIST.
                04  C50-JYURRK-COUNT   PIC S9(9)   BINARY.
                04  C50-JYURRK-ROW PIC S9(9)   BINARY.
                04  C50-JYURRK-ROWATTR PIC S9(9)   BINARY.
                04  C50-ITEM        OCCURS  10 TIMES.
                  05  C50-TJYURRKNUM  PIC X(2).
                  05  C50-TJYURRKYMD  PIC X(10).
                04  C50-JYURRK-SELECT
                        PIC X       OCCURS  10 TIMES.
              03  C50-JYURRKNUM.
                04  C50-VALUE   PIC S9(2).
              03  C50-COPYTUKI.
                04  C50-VALUE   PIC S9(2).
              03  C50-COPYSRYYM.
                04  C50-VALUE   PIC X(7).
              03  C50-SELNUM.
                04  C50-VALUE   PIC X(40).
              03  C50-MIDASI-G.
                04  C50-MIDASI-STYLE   PIC X(20).
                04  C50-MIDASI         PIC X(20).
              03  C50-PTNUM.
                04  C50-VALUE   PIC X(20).
              03  C50-COMSEL.
                04  C50-VALUE   PIC S9(3).
              03  C50-NAME.
                04  C50-VALUE   PIC X(50).
              03  C50-SEX.
                04  C50-VALUE   PIC X(2).
              03  C50-BIRTHDAY.
                04  C50-VALUE   PIC X(10).
              03  C50-SCROLLEDWINDOW1.
                04  C50-RECELIST.
                  05  C50-RECE-COUNT   PIC S9(9)   BINARY.
                  05  C50-RECE-ROW PIC S9(9)   BINARY.
                  05  C50-RECE-ROWATTR PIC S9(9)   BINARY.
                  05  C50-ITEM        OCCURS  100 TIMES.
                    06  C50-TRECNUM   PIC X(3).
                    06  C50-TSRYYM    PIC X(6).
                    06  C50-TSRYKA    PIC X(10).
                    06  C50-TNYUGAIKBN   PIC X(2).
                    06  C50-THKNCOMBI    PIC X(50).
                    06  C50-TSRYDD      PIC X(3).
                  05  C50-RECE-SELECT
                          PIC X       OCCURS  100 TIMES.
              03  C50-NYUGAIKBN-COMB.
                04  C50-NYUGAIKBN.
                  05  C50-VALUE   PIC X(55).
                04  C50-NYUGAIKBN-COUNT   PIC S9(9)   BINARY.
                04  C50-NYUGAIKBN-ITEM
                        PIC X(55)   OCCURS  2 TIMES.
              03  C50-SRYKA-COMB.
                04  C50-SRYKA.
                  05  C50-VALUE   PIC X(20).
                04  C50-SRYKA-COUNT   PIC S9(9)   BINARY.
                04  C50-SRYKA-ITEM
                        PIC X(20)   OCCURS  100 TIMES.
              03  C50-HKNCOMBI-COMB.
                04  C50-HKNCOMBI.
                  05  C50-VALUE   PIC X(55).
                04  C50-HKNCOMBI-COUNT   PIC S9(9)   BINARY.
                04  C50-HKNCOMBI-ITEM
                        PIC X(55)   OCCURS  50 TIMES.
              03  C50-SCROLLEDWINDOW2.
                04  C50-HPOS    PIC S9(9)   BINARY.
                04  C50-VPOS    PIC S9(9)   BINARY.
                04  C50-COMMENTLIST.
                  05  C50-COMM-COUNT   PIC S9(9)   BINARY.
                  05  C50-COMM-ROW PIC S9(9)   BINARY.
                  05  C50-COMM-ROWATTR PIC S9(9)   BINARY.
                  05  C50-ITEM        OCCURS  100 TIMES.
                    06  C50-TCOMREN   PIC X(3).
                    06  C50-TSJKBN    PIC X(2).
                    06  C50-TRENNUM   PIC X(2).
                    06  C50-TCOMMENT  PIC X(100).
                  05  C50-COMM-SELECT
                          PIC X       OCCURS  100 TIMES.
              03  C50-SJKBN-COMB.
                04  C50-SJKBN.
                  05  C50-VALUE   PIC X(53).
                04  C50-SJKBN-COUNT   PIC S9(9)   BINARY.
                04  C50-SJKBN-ITEM
                        PIC X(53)   OCCURS  50 TIMES.
              03  C50-SCROLLEDWINDOW3.
                04  C50-HPOS    PIC S9(9)   BINARY.
                04  C50-VPOS    PIC S9(9)   BINARY.
                04  C50-COMMENT.
                  05  C50-VALUE   PIC X(2420).
              03  C50-MCNT.
                04  C50-VALUE   PIC X(20).

