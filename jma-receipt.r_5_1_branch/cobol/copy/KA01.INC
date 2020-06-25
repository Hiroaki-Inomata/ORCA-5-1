        02  KA01.
     *****  02  KA01-FIXED1.
              03  KA01-PTNUM.
                04  KA01-VALUE  PIC X(20).
              03  KA01-NAME.
                04  KA01-VALUE  PIC X(50).
              03  KA01-SEX.
                04  KA01-VALUE  PIC X(2).
              03  KA01-BIRTHDAY.
                04  KA01-VALUE  PIC X(10).
              03  KA01-NENREI.
                04  KA01-VALUE  PIC X(6).
              03  KA01-SELNUM1.
                04  KA01-VALUE  PIC S9(3).
              03  KA01-SRYYMD.
                04  KA01-VALUE  PIC X(9).
              03  KA01-SELNUM2.
                04  KA01-VALUE  PIC S9(3).
              03  KA01-INPUT01.
                04  KA01-VALUE  PIC X(80).
              03  KA01-INPUT02.
                04  KA01-VALUE  PIC X(80).
              03  KA01-INPUT03.
                04  KA01-VALUE  PIC X(80).
              03  KA01-INPUT04.
                04  KA01-VALUE  PIC X(80).
              03  KA01-INPUT05.
                04  KA01-VALUE  PIC X(80).
              03  KA01-INPUT06.
                04  KA01-VALUE  PIC X(80).
              03  KA01-NISUU.
                04  KA01-VALUE  PIC S9(3).
              03  KA01-SRYKA.
                04  KA01-VALUE  PIC X(12).
              03  KA01-SCROLLEDWINDOW1.
                04  KA01-RRKLIST.
                  05  KA01-RRKLIST-COUNT  PIC S9(9)   BINARY.
                  05  KA01-RRKLIST-ROW    PIC S9(9)   BINARY.
                  05  KA01-RRKLIST-ROWATTR    PIC S9(9)   BINARY.
                  05  KA01-ITEM       OCCURS  40 TIMES.
                    06  KA01-TNUM     PIC X(3).
                    06  KA01-TRRKYMD  PIC X(16).
                    06  KA01-TRRKSRYKA  PIC X(6).
                    06  KA01-TRRKHKNCOMBI  PIC X(4).
                  05  KA01-RRKLIST-SELECT
                          PIC X       OCCURS  40 TIMES.
              03  KA01-SCROLLEDWINDOW2.
                04  KA01-MEILIST.
                  05  KA01-MEILIST-COUNT  PIC S9(9)   BINARY.
                  05  KA01-MEILIST-ROW    PIC S9(9)   BINARY.
                  05  KA01-MEILIST-ROWATTR    PIC S9(9)   BINARY.
                  05  KA01-ITEM       OCCURS  400 TIMES.
                    06  KA01-MNUM   PIC X(3).
                    06  KA01-MSYORI PIC X(2).
                    06  KA01-MDEL   PIC X(2).
                    06  KA01-MEISAI PIC X(200).
                  05  KA01-MEILIST-SELECT
                          PIC X       OCCURS  400 TIMES.
              03  KA01-SYORIMEI.
                04  KA01-SYORIMEI-STYLE  PIC X(20).
                04  KA01-SYORIMEI-VALUE  PIC X(20).
              03  KA01-HKNCOMBI-COMB.
                04  KA01-HKNCOMBI.
                  05  KA01-VALUE  PIC X(65).
                04  KA01-HKNCOMBI-COUNT  PIC S9(9)   BINARY.
                04  KA01-HKNCOMBI-ITEM
                        PIC X(65)   OCCURS  30 TIMES.
              03  KA01-DRCD-COMB.
                04  KA01-DRCD.
                  05  KA01-VALUE  PIC X(29).
                04  KA01-DRCD-COUNT  PIC S9(9)   BINARY.
                04  KA01-DRCD-ITEM
                        PIC X(29)   OCCURS  99 TIMES.
              03  KA01-INNAI.
                04  KA01-INNAI-STYLE  PIC X(20).
                04  KA01-INNAI-VALUE  PIC X(16).
              03  KA01-SUURYO-G  OCCURS  6.
                04  KA01-SUURYO  PIC X(12).
              03  KA01-KOUFUYMD.
                04  KA01-VALUE  PIC X(9).
              03  KA01-KEISUURYO-G  OCCURS  6.
                04  KA01-KEISUURYO  PIC X(12).
              03  KA01-B12S.
                04   KA01-B12S-STATE   PIC S9(9)   BINARY.
