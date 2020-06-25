       02  KT02.
     ****** 02  KT02-FIXED1.
              03  KT02-PTNUM.
                04  KT02-VALUE  PIC X(20).
              03  KT02-KANANAME.
                04  KT02-VALUE  PIC X(50).
              03  KT02-NAME.
                04  KT02-VALUE  PIC X(50).
              03  KT02-SEX.
                04  KT02-VALUE  PIC X(2).
              03  KT02-HKNKUMI.
                04  KT02-VALUE  PIC X(60).
              03  KT02-RATE.
                04  KT02-VALUE  PIC X(6).
              03  KT02-LASTYMD.
                04  KT02-VALUE  PIC X(9).
              03  KT02-SYOSINYMD.
                04  KT02-VALUE  PIC X(9).
              03  KT02-MISYUMONEY.
                04  KT02-VALUE  PIC X(15).
              03  KT02-SRYKA.
                04  KT02-VALUE  PIC X(10).
              03  KT02-SRYYMD.
                04  KT02-VALUE  PIC X(14).
              03  KT02-GOKEITEN.
                04  KT02-VALUE  PIC X(15).
              03  KT02-ZAITENTOTAL.
                04  KT02-VALUE  PIC X(16).
              03  KT02-BIRTHDAY.
                04  KT02-VALUE  PIC X(9).
              03  KT02-NENREI.
                04  KT02-VALUE  PIC X(6).
              03  KT02-SSUHKNTEN.
                04  KT02-VALUE  PIC X(10).
              03  KT02-SDOHKNTEN.
                04  KT02-VALUE  PIC X(10).
              03  KT02-ZTKHKNTEN.
                04  KT02-VALUE  PIC X(10).
              03  KT02-TYKHKNTEN.
                04  KT02-VALUE  PIC X(10).
              03  KT02-CSYHKNTEN.
                04  KT02-VALUE  PIC X(10).
              03  KT02-SYCHKNTEN.
                04  KT02-VALUE  PIC X(10).
              03  KT02-SJTHKNTEN.
                04  KT02-VALUE  PIC X(10).
              03  KT02-KNSHKNTEN.
                04  KT02-VALUE  PIC X(10).
              03  KT02-GZUHKNTEN.
                04  KT02-VALUE  PIC X(10).
              03  KT02-ETCHKNTEN.
                04  KT02-VALUE  PIC X(10).
              03  KT02-SELNUM-G     OCCURS  10.
                04  KT02-SELNUM  PIC S9(3).
              03  KT02-SCROLLEDWINDOW1.
                04  KT02-HPOS   PIC S9(9)   BINARY.
                04  KT02-VPOS   PIC S9(9)   BINARY.
                04  KT02-MEILIST.
                  05  KT02-COUNT  PIC S9(9)   BINARY.
                  05  KT02-ROW    PIC S9(9)   BINARY.
                  05  KT02-ITEM       OCCURS  800 TIMES.
                    06  KT02-BANGO  PIC X(4).
                    06  KT02-DELFLG  PIC X(2).
                    06  KT02-MEISAI  PIC X(150).
                  05  KT02-SELECT
                          PIC X       OCCURS  800 TIMES.
                  05  KT02-MEI-STYLE   PIC X(20).
              03  KT02-SSNHKNTEN.
                04  KT02-VALUE  PIC X(10).
              03  KT02-HOUHKNTEN.
                04  KT02-VALUE  PIC X(10).
              03  KT02-NYUHKNTEN.
                04  KT02-VALUE  PIC X(10).
              03  KT02-MSIHKNTEN.
                04  KT02-VALUE  PIC X(10).
              03  KT02-RYOHKNTEN.
                04  KT02-VALUE  PIC X(10).
              03  KT02-BYRHKNTEN.
                04  KT02-VALUE  PIC X(10).
              03  KT02-SRYMEI14.
                04  KT02-VALUE  PIC X(8).
              03  KT02-SRYMEI15.
                04  KT02-VALUE  PIC X(8).
              03  KT02-MSGDATA.
                04  KT02-MSGDATA-STYLE   PIC X(20).
                04  KT02-MSGDATA-VALUE   PIC X(50).
              03  KT02-SYORIMEI.
                04  KT02-SYORIMEI-STYLE  PIC X(20).
                04  KT02-SYORIMEI-VALUE  PIC X(12).
