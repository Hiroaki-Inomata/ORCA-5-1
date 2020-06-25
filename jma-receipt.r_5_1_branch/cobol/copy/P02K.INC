        02  P02K.
      ****  02  P02K-FIXED10.
              03  P02K-SELNUM.
                04  P02K-VALUE  PIC S9(3).
              03  P02K-SRYYM.
                04  P02K-VALUE  PIC X(7).
              03  P02K-FTNMONEY.
                04  P02K-VALUE  PIC S9(7).
              03  P02K-MSG01.
                04  P02K-MSG01-STYLE   PIC X(20).
                04  P02K-MSG01-VALUE  PIC X(30).
              03  P02K-MSG02.
                04  P02K-VALUE  PIC X(30).
              03  P02K-SRYDD.
                04  P02K-VALUE  PIC S9(2).
              03  P02K-NYUGAIKBN-COMB.
                04  P02K-NYUGAIKBN.
                  05  P02K-VALUE  PIC X(10).
                04  P02K-NYUGAIKBN-COUNT  PIC S9(9)   BINARY.
                04  P02K-NYUGAIKBN-ITEM
                        PIC X(10)   OCCURS  2 TIMES.
              03  P02K-SCROLLEDWINDOW2.
                04  P02K-HPOS   PIC S9(9)   BINARY.
                04  P02K-VPOS   PIC S9(9)   BINARY.
                04  P02K-KOHFTNLIST.
                  05  P02K-FTNLIST-COUNT  PIC S9(9)   BINARY.
                  05  P02K-FTNLIST-ROW    PIC S9(9)   BINARY.
                  05  P02K-ITEM       OCCURS  50 TIMES.
                    06  P02K-TRENNUM PIC X(3).
                    06  P02K-TSRYYMD PIC X(9).
                    06  P02K-TJITAKBN PIC X(10).
                    06  P02K-TNYUGAIKBN PIC X(4).
                    06  P02K-TFTNMONEY  PIC X(10).
                  05  P02K-FTNLIST-SELECT
                          PIC X       OCCURS  50 TIMES.
              03  P02K-MSG03.
                04  P02K-MSG03-STYLE   PIC X(20).
                04  P02K-MSG03-VALUE  PIC X(50).
              03  P02K-MSG04.
                04  P02K-MSG04-STYLE   PIC X(20).
                04  P02K-MSG04-VALUE  PIC X(30).
              03  P02K-MSG05.
                04  P02K-VALUE  PIC X(30).
              03  P02K-MSG06.
                04  P02K-VALUE  PIC X(50).
              03  P02K-MSG07.
                04  P02K-MSG07-STYLE   PIC X(20).
                04  P02K-MSG07-VALUE  PIC X(30).
