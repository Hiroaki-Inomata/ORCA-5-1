        01  HCRCP400V01.
            02  HCRCP400V01-TEISYUTUSAKI    PIC X(6).
            02  HCRCP400V01-PTNUM   PIC X(20).
            02  HCRCP400V01-SRYY    PIC X(2).
            02  HCRCP400V01-SRYM    PIC X(2).
            02  HCRCP400V01-PREFNUM PIC X(2).
            02  HCRCP400V01-HOSPCDN PIC X(20).
            02  HCRCP400V01-KHNTEN2
                    PIC X(7)    OCCURS  2 TIMES.
            02  HCRCP400V01-KHNTEN1 PIC X(7).
            02  HCRCP400V01-SRYKBN
                    PIC X(2)    OCCURS  35 TIMES.
            02  HCRCP400V01-TEKIYO
                    PIC X(52)   OCCURS  35 TIMES.
            02  HCRCP400V01-KOUTEN
                    PIC X(7)    OCCURS  35 TIMES.
            02  HCRCP400V01-TOTALTEN
                    PIC X(7)    OCCURS  3 TIMES.
            02  HCRCP400V01-FTNMONEY
                    PIC X(8)    OCCURS  3 TIMES.
            02  HCRCP400V01-OCR1    PIC X(67).
            02  HCRCP400V01-OCR2    PIC X(67).
            02  HCRCP400V01-OCR3    PIC X(67).
            02  HCRCP400V01-RJNFTNJANUM.
              03  HCRCP400V01-RJNFTNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCRCP400V01-KOH-TBL1.
              03  HCRCP400V01-KOHFTNJANUM1.
                04  HCRCP400V01-KOHFTNJANUM1-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCRCP400V01-KOHFTNJANUM1N   PIC X(20).
              03  HCRCP400V01-KOHJKYSNUM1.
                04  HCRCP400V01-KOHJKYSNUM1-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCRCP400V01-KOHJKYSNUM1N    PIC X(20).
            02  HCRCP400V01-KOH-TBL2.
              03  HCRCP400V01-KOHFTNJANUM2.
                04  HCRCP400V01-KOHFTNJANUM2-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCRCP400V01-KOHFTNJANUM2N   PIC X(20).
              03  HCRCP400V01-KOHJKYSNUM2.
                04  HCRCP400V01-KOHJKYSNUM2-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCRCP400V01-KOHJKYSNUM2N    PIC X(20).
            02  HCRCP400V01-HKNJANUM.
              03  HCRCP400V01-HKNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCRCP400V01-KYURATE PIC X(8).
            02  HCRCP400V01-RJNJKYSNUM.
              03  HCRCP400V01-RJNJKYSNUM-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCRCP400V01-HKNSBT1 PIC X(6).
            02  HCRCP400V01-HKNSBT2 PIC X(6).
            02  HCRCP400V01-HONKZK  PIC X(10).
            02  HCRCP400V01-KIGO    PIC X(34).
            02  HCRCP400V01-NUM PIC X(34).
            02  HCRCP400V01-KANANAME    PIC X(40).
            02  HCRCP400V01-KANANAME2   PIC X(50).
            02  HCRCP400V01-SEXNUM  PIC X(2).
            02  HCRCP400V01-BIRTHDAY    PIC X(22).
            02  HCRCP400V01-JIYU    PIC X(20).
            02  HCRCP400V01-SEXN    PIC X(2).
            02  HCRCP400V01-TOKKI
                    PIC X(12)   OCCURS  4 TIMES.
            02  HCRCP400V01-HOSPTEL PIC X(15).
            02  HCRCP400V01-SRYKA   PIC X(30).
            02  HCRCP400V01-BEDSU   PIC X(4).
            02  HCRCP400V01-BYOMEI
                    PIC X(54)   OCCURS  5 TIMES.
            02  HCRCP400V01-SRYYMD
                    PIC X(24)   OCCURS  5 TIMES.
            02  HCRCP400V01-TENKI
                    PIC X(10)   OCCURS  5 TIMES.
            02  HCRCP400V01-JINISSU PIC X(2).
            02  HCRCP400V01-JINISSU1    PIC X(2).
            02  HCRCP400V01-JINISSU2    PIC X(2).
            02  HCRCP400V01-TENSU
                    PIC X(7)    OCCURS  35 TIMES.
            02  HCRCP400V01-KAISU
                    PIC X(3)    OCCURS  35 TIMES.
            02  HCRCP400V01-KHNTEN
                    PIC X(7)    OCCURS  5 TIMES.
            02  HCRCP400V01-HOSPNAME    PIC X(80).
            02  HCRCP400V01-HOSPADRS    PIC X(100).
            02  HCRCP400V01-NAME    PIC X(50).
            02  HCRCP400V01-HEAD1-G.
              03  HCRCP400V01-HEAD1
                      PIC X(46)   OCCURS  2 TIMES.
            02  HCRCP400V01-HEAD2-G.
              03  HCRCP400V01-HEAD2
                      PIC X(2)    OCCURS  7 TIMES.
            02  HCRCP400V01-HEAD3-G.
              03  HCRCP400V01-HEAD3
                      PIC X(50)   OCCURS  2 TIMES.
            02  HCRCP400V01-BODY    PIC X(52).
            02  HCRCP400V01-FOOT-G.
              03  HCRCP400V01-FOOT
                      PIC X(46)   OCCURS  4 TIMES.
            02  HCRCP400V01-SHOSHINTIME PIC X(10).
            02  HCRCP400V01-TENKIYMD
                    PIC X(6)    OCCURS  5 TIMES.
            02  HCRCP400V01-FTNMONEYSAI PIC X(11).
            02  HCRCP400V01-RENNUM  PIC X(7).
            02  HCRCP400V01-SRYKA2  PIC X(60).
            02  HCRCP400V01-KHNTEN3 PIC X(7).
            02  HCRCP400V01-SONOTA  PIC X(10).
            02  HCRCP400V01-KYUFUGAI1   PIC X(10).
            02  HCRCP400V01-KYUFUGAI2   PIC X(10).
            02  HCRCP400V01-KYUFUGAI3   PIC X(12).
            02  HCRCP400V01-KYUFUGAI-TEN    PIC X(5).
            02  HCRCP400V01-KYUFUGAI4   PIC X(2).
