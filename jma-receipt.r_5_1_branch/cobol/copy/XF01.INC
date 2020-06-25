        02  XF01.
  ********  02  XF01-FIXED1.
              03  XF01-PTNUM.
                04  XF01-VALUE  PIC X(20).
              03  XF01-NAME.
                04  XF01-VALUE  PIC X(50).
              03  XF01-SEX.
                04  XF01-VALUE  PIC X(2).
              03  XF01-BIRTHDAY.
                04  XF01-VALUE  PIC X(10).
              03  XF01-NENREI.
                04  XF01-VALUE  PIC X(6).
              03  XF01-SELNUM.
                04  XF01-VALUE  PIC S9(3).
              03  XF01-MEMO01.
                04  XF01-VALUE  PIC X(1000).
              03  XF01-MEMO02.
                04  XF01-VALUE  PIC X(1000).
              03  XF01-SYSYMD.
                04  XF01-VALUE  PIC X(22).
              03  XF01-SCROLLEDWINDOW1.
                04  XF01-HPOS   PIC S9(9)   BINARY.
                04  XF01-VPOS   PIC S9(9)   BINARY.
                04  XF01-MEMOLIST.
                  05  XF01-MEMOLIST-COUNT  PIC S9(9)   BINARY.
                  05  XF01-ROW    PIC S9(9)   BINARY.
                  05  XF01-ITEM       OCCURS  100 TIMES.
                    06  XF01-TRENNUM  PIC X(3).
                    06  XF01-TSYSYMD  PIC X(10).
                    06  XF01-TSRYKA   PIC X(20).
                    06  XF01-TMEMO    PIC X(200).
                  05  XF01-MEMOLIST-SELECT
                          PIC X       OCCURS  100 TIMES.
              03  XF01-SRYKA01-COMB.
                04  XF01-SRYKA01.
                  05  XF01-VALUE  PIC X(20).
                04  XF01-SRYKA01-COUNT  PIC S9(9)   BINARY.
                04  XF01-SRYKA01-ITEM
                        PIC X(20)   OCCURS  41 TIMES.
              03  XF01-SRYKA02-COMB.
                04  XF01-SRYKA02.
                  05  XF01-VALUE  PIC X(20).
                04  XF01-SRYKA02-COUNT  PIC S9(9)   BINARY.
                04  XF01-SRYKA02-ITEM
                        PIC X(20)   OCCURS  41 TIMES.
              03  XF01-SYORIMEI.
                04  XF01-SYORIMEI-STYLE  PIC X(20).
                04  XF01-SYORIMEI-VALUE  PIC X(20).
              03  XF01-MIDASI.
                04  XF01-MIDASI-STYLE  PIC X(20).
                04  XF01-MIDASI-VALUE  PIC X(30).
              03  XF01-SYORI01.
                04  XF01-SYORI01-STYLE  PIC X(20).
                04  XF01-SYORI01-VALUE  PIC X(10).
              03  XF01-SYORI02.
                04  XF01-SYORI02-STYLE  PIC X(20).
                04  XF01-SYORI02-VALUE  PIC X(10).
              03  XF01-UKETUKE.
                04  XF01-UKETUKE-STYLE  PIC X(20).
                04  XF01-UKETUKE-VALUE  PIC X(20).
              03  XF01-B12S.
                04  XF01-B12S-STATE  PIC S9(9)   BINARY.


