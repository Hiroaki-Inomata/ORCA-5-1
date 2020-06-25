        02  KA02.
      ***** 02  KA02-FIXED1.
              03  KA02-PTNUM.
                04  KA02-VALUE  PIC X(20).
              03  KA02-NAME.
                04  KA02-VALUE  PIC X(50).
              03  KA02-SEX.
                04  KA02-VALUE  PIC X(2).
              03  KA02-BIRTHDAY.
                04  KA02-VALUE  PIC X(10).
              03  KA02-NENREI.
                04  KA02-VALUE  PIC X(6).
              03  KA02-SELNUM1.
                04  KA02-VALUE  PIC S9(3).
              03  KA02-SRYYMD.
                04  KA02-VALUE  PIC X(9).
              03  KA02-SELNUM2.
                04  KA02-VALUE  PIC S9(3).
              03  KA02-INPUT01.
                04  KA02-VALUE  PIC X(80).
              03  KA02-INPUT02.
                04  KA02-VALUE  PIC X(80).
              03  KA02-INPUT03.
                04  KA02-VALUE  PIC X(80).
              03  KA02-INPUT04.
                04  KA02-VALUE  PIC X(80).
              03  KA02-INPUT05.
                04  KA02-VALUE  PIC X(80).
              03  KA02-NISUU.
                04  KA02-VALUE  PIC S9(3).
              03  KA02-SRYKA.
                04  KA02-VALUE  PIC X(12).
              03  KA02-SUURYO-G  OCCURS  5.
                04  KA02-SUURYO  PIC X(12).
              03  KA02-HKNCOMBI-COMB.
                04  KA02-HKNCOMBI.
                  05  KA02-VALUE  PIC X(65).
                04  KA02-HKNCOMBI-COUNT  PIC S9(9)   BINARY.
                04  KA02-HKNCOMBI-ITEM
                        PIC X(65)   OCCURS  30 TIMES.
              03  KA02-DRCD-COMB.
                04  KA02-DRCD.
                  05  KA02-VALUE  PIC X(29).
                04  KA02-DRCD-COUNT  PIC S9(9)   BINARY.
                04  KA02-DRCD-ITEM
                        PIC X(29)   OCCURS  99 TIMES.
              03  KA02-KOUFUYMD.
                04  KA02-VALUE  PIC X(11).
              03  KA02-SCROLLEDWINDOW1.
                04  KA02-HPOS   PIC S9(9)   BINARY.
                04  KA02-VPOS   PIC S9(9)   BINARY.
                04  KA02-RRKLIST.
                  05  KA02-RRKLIST-COUNT  PIC S9(9)   BINARY.
                  05  KA02-RRKLIST-ROW    PIC S9(9)   BINARY.
                  05  KA02-ITEM       OCCURS  100 TIMES.
                    06  KA02-TNUM     PIC X(3).
                    06  KA02-TRRKYMD  PIC X(16).
                    06  KA02-TRRKSRYKA  PIC X(6).
                    06  KA02-TRRKHKNCOMBI  PIC X(4).
                  05  KA02-RRKLIST-SELECT
                          PIC X       OCCURS  100 TIMES.
              03  KA02-SCROLLEDWINDOW2.
                04  KA02-HPOS   PIC S9(9)   BINARY.
                04  KA02-VPOS   PIC S9(9)   BINARY.
                04  KA02-MEILIST.
                  05  KA02-MEILIST-COUNT  PIC S9(9)   BINARY.
                  05  KA02-MEILIST-ROW    PIC S9(9)   BINARY.
                  05  KA02-ITEM       OCCURS  600 TIMES.
                    06  KA02-MNUM   PIC X(3).
                    06  KA02-MSYORI PIC X(2).
                    06  KA02-MDEL   PIC X(2).
                    06  KA02-MEISAI PIC X(200).
                  05  KA02-MEILIST-SELECT
                          PIC X       OCCURS  600 TIMES.
              03  KA02-NYUINKIKAN.
                04  KA02-VALUE  PIC X(30).
              03  KA02-INNAI.
                04  KA02-INNAI-STYLE  PIC X(20).
                04  KA02-INNAI-VALUE  PIC X(16).
              03  KA02-SYORIMEI.
                04  KA02-SYORIMEI-STYLE  PIC X(20).
                04  KA02-SYORIMEI-VALUE  PIC X(20).
              03  KA02-DAY-G.
                04  KA02-DAY         PIC X(3)
                                     OCCURS   31.
              03  KA02-KEISUURYO-G  OCCURS  5.
                04  KA02-KEISUURYO  PIC X(12).
              03  KA02-B04.
                04  KA02-B04-LABEL  PIC X(10).
              03  KA02-B09S.
                04  KA02-B09S-STATE  PIC S9(9)   BINARY.
                04  KA02-B09S-LABEL  PIC X(10).
              03  KA02-B05S.
                04  KA02-B05S-STATE  PIC S9(9)   BINARY.
                04  KA02-B05S-LABEL  PIC X(10).
