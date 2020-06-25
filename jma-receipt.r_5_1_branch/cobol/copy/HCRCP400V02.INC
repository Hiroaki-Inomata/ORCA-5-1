        01  HCRCP400V02.
            02  HCRCP400V02-TEISYUTUSAKI    PIC X(6).
            02  HCRCP400V02-PTNUM   PIC X(20).
            02  HCRCP400V02-SRYY    PIC X(2).
            02  HCRCP400V02-SRYM    PIC X(2).
            02  HCRCP400V02-PREFNUM PIC X(2).
            02  HCRCP400V02-HOSPCDN PIC X(20).
            02  HCRCP400V02-KHNTEN2
                    PIC X(7)    OCCURS  2 TIMES.
            02  HCRCP400V02-KHNTEN1 PIC X(7).
            02  HCRCP400V02-SRYKBN
                    PIC X(2)    OCCURS  35 TIMES.
            02  HCRCP400V02-TEKIYO
                    PIC X(52)   OCCURS  35 TIMES.
            02  HCRCP400V02-KOUTEN
                    PIC X(7)    OCCURS  35 TIMES.
            02  HCRCP400V02-TOTALTEN
                    PIC X(7)    OCCURS  3 TIMES.
            02  HCRCP400V02-FTNMONEY
                    PIC X(8)    OCCURS  3 TIMES.
            02  HCRCP400V02-OCR1    PIC X(67).
            02  HCRCP400V02-OCR2    PIC X(67).
            02  HCRCP400V02-OCR3    PIC X(67).
            02  HCRCP400V02-RJNFTNJANUM.
              03  HCRCP400V02-RJNFTNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCRCP400V02-KOH-TBL1.
              03  HCRCP400V02-KOHFTNJANUM1.
                04  HCRCP400V02-KOHFTNJANUM1-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCRCP400V02-KOHFTNJANUM1N   PIC X(20).
              03  HCRCP400V02-KOHJKYSNUM1.
                04  HCRCP400V02-KOHJKYSNUM1-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCRCP400V02-KOHJKYSNUM1N    PIC X(20).
            02  HCRCP400V02-KOH-TBL2.
              03  HCRCP400V02-KOHFTNJANUM2.
                04  HCRCP400V02-KOHFTNJANUM2-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCRCP400V02-KOHFTNJANUM2N   PIC X(20).
              03  HCRCP400V02-KOHJKYSNUM2.
                04  HCRCP400V02-KOHJKYSNUM2-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCRCP400V02-KOHJKYSNUM2N    PIC X(20).
            02  HCRCP400V02-HKNJANUM.
              03  HCRCP400V02-HKNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCRCP400V02-KYURATE PIC X(8).
            02  HCRCP400V02-RJNJKYSNUM.
              03  HCRCP400V02-RJNJKYSNUM-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCRCP400V02-HKNSBT1 PIC X(6).
            02  HCRCP400V02-HKNSBT2 PIC X(6).
            02  HCRCP400V02-HONKZK  PIC X(10).
            02  HCRCP400V02-KIGO    PIC X(34).
            02  HCRCP400V02-NUM PIC X(34).
            02  HCRCP400V02-KANANAME    PIC X(40).
            02  HCRCP400V02-KANANAME2   PIC X(50).
            02  HCRCP400V02-SEXNUM  PIC X(2).
            02  HCRCP400V02-BIRTHDAY    PIC X(22).
            02  HCRCP400V02-JIYU    PIC X(20).
            02  HCRCP400V02-SEXN    PIC X(2).
            02  HCRCP400V02-TOKKI
                    PIC X(12)   OCCURS  4 TIMES.
            02  HCRCP400V02-HOSPTEL PIC X(15).
            02  HCRCP400V02-SRYKA   PIC X(30).
            02  HCRCP400V02-BEDSU   PIC X(4).
            02  HCRCP400V02-BYOMEI
                    PIC X(54)   OCCURS  5 TIMES.
            02  HCRCP400V02-SRYYMD
                    PIC X(24)   OCCURS  5 TIMES.
            02  HCRCP400V02-TENKI
                    PIC X(10)   OCCURS  5 TIMES.
            02  HCRCP400V02-JINISSU PIC X(2).
            02  HCRCP400V02-JINISSU1    PIC X(2).
            02  HCRCP400V02-JINISSU2    PIC X(2).
            02  HCRCP400V02-TENSU
                    PIC X(7)    OCCURS  35 TIMES.
            02  HCRCP400V02-KAISU
                    PIC X(3)    OCCURS  35 TIMES.
            02  HCRCP400V02-KHNTEN
                    PIC X(7)    OCCURS  5 TIMES.
            02  HCRCP400V02-HOSPNAME    PIC X(80).
            02  HCRCP400V02-HOSPADRS    PIC X(100).
            02  HCRCP400V02-NAME    PIC X(50).
            02  HCRCP400V02-HEAD1-G.
              03  HCRCP400V02-HEAD1
                      PIC X(46)   OCCURS  2 TIMES.
            02  HCRCP400V02-HEAD2-G.
              03  HCRCP400V02-HEAD2
                      PIC X(2)    OCCURS  7 TIMES.
            02  HCRCP400V02-HEAD3-G.
              03  HCRCP400V02-HEAD3
                      PIC X(50)   OCCURS  2 TIMES.
            02  HCRCP400V02-HEAD4-G.
              03  HCRCP400V02-HEAD4
                      PIC X(62)   OCCURS  2 TIMES.
            02  HCRCP400V02-BODY    PIC X(52).
            02  HCRCP400V02-FOOT-G.
              03  HCRCP400V02-FOOT
                      PIC X(46)   OCCURS  4 TIMES.
            02  HCRCP400V02-SHOSHINTIME PIC X(10).
            02  HCRCP400V02-TENKIYMD
                    PIC X(6)    OCCURS  5 TIMES.
            02  HCRCP400V02-FTNMONEYSAI PIC X(11).
            02  HCRCP400V02-RENNUM  PIC X(7).
            02  HCRCP400V02-SRYKA2  PIC X(60).
            02  HCRCP400V02-KHNTEN3 PIC X(7).
            02  HCRCP400V02-SONOTA  PIC X(10).
            02  HCRCP400V02-KYUFUGAI1   PIC X(10).
            02  HCRCP400V02-KYUFUGAI2   PIC X(10).
            02  HCRCP400V02-KYUFUGAI3   PIC X(12).
            02  HCRCP400V02-KYUFUGAI-TEN    PIC X(5).
            02  HCRCP400V02-KYUFUGAI4   PIC X(2).
