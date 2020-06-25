        01  HCRCP402.
            02  HCRCP402-TEISYUTUSAKI   PIC X(6).
            02  HCRCP402-PTNUM  PIC X(20).
            02  HCRCP402-SRYY   PIC X(2).
            02  HCRCP402-SRYM   PIC X(2).
            02  HCRCP402-PREFNUM    PIC X(2).
            02  HCRCP402-HOSPCDN    PIC X(20).
            02  HCRCP402-KHNTEN2
                    PIC X(7)    OCCURS  2 TIMES.
            02  HCRCP402-KHNTEN1    PIC X(7).
            02  HCRCP402-SRYKBN
                    PIC X(2)    OCCURS  35 TIMES.
            02  HCRCP402-TEKIYO
                    PIC X(52)   OCCURS  35 TIMES.
            02  HCRCP402-KOUTEN
                    PIC X(7)    OCCURS  35 TIMES.
            02  HCRCP402-TOTALTEN
                    PIC X(7)    OCCURS  3 TIMES.
            02  HCRCP402-FTNMONEY
                    PIC X(8)    OCCURS  3 TIMES.
            02  HCRCP402-OCR1   PIC X(67).
            02  HCRCP402-OCR2   PIC X(67).
            02  HCRCP402-OCR3   PIC X(67).
            02  HCRCP402-RJNFTNJANUM.
              03  HCRCP402-RJNFTNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCRCP402-KOH-TBL1.
              03  HCRCP402-KOHFTNJANUM1.
                04  HCRCP402-KOHFTNJANUM1-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCRCP402-KOHFTNJANUM1N  PIC X(20).
              03  HCRCP402-KOHJKYSNUM1.
                04  HCRCP402-KOHJKYSNUM1-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCRCP402-KOHJKYSNUM1N   PIC X(20).
            02  HCRCP402-KOH-TBL2.
              03  HCRCP402-KOHFTNJANUM2.
                04  HCRCP402-KOHFTNJANUM2-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCRCP402-KOHFTNJANUM2N  PIC X(20).
              03  HCRCP402-KOHJKYSNUM2.
                04  HCRCP402-KOHJKYSNUM2-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCRCP402-KOHJKYSNUM2N   PIC X(20).
            02  HCRCP402-HKNJANUM.
              03  HCRCP402-HKNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCRCP402-KYURATE    PIC X(8).
            02  HCRCP402-RJNJKYSNUM.
              03  HCRCP402-RJNJKYSNUM-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCRCP402-HKNSBT1    PIC X(6).
            02  HCRCP402-HKNSBT2    PIC X(6).
            02  HCRCP402-HONKZK PIC X(10).
            02  HCRCP402-KIGO   PIC X(34).
            02  HCRCP402-NUM    PIC X(34).
            02  HCRCP402-KANANAME   PIC X(40).
            02  HCRCP402-KANANAME2  PIC X(50).
            02  HCRCP402-SEXNUM PIC X(2).
            02  HCRCP402-BIRTHDAY   PIC X(22).
            02  HCRCP402-JIYU   PIC X(20).
            02  HCRCP402-SEXN   PIC X(2).
            02  HCRCP402-TOKKI
                    PIC X(12)   OCCURS  4 TIMES.
            02  HCRCP402-HOSPTEL    PIC X(15).
            02  HCRCP402-SRYKA  PIC X(30).
            02  HCRCP402-BEDSU  PIC X(4).
            02  HCRCP402-BYOMEI
                    PIC X(54)   OCCURS  5 TIMES.
            02  HCRCP402-SRYYMD
                    PIC X(24)   OCCURS  5 TIMES.
            02  HCRCP402-TENKI
                    PIC X(10)   OCCURS  5 TIMES.
            02  HCRCP402-JINISSU    PIC X(2).
            02  HCRCP402-JINISSU1   PIC X(2).
            02  HCRCP402-JINISSU2   PIC X(2).
            02  HCRCP402-TENSU
                    PIC X(7)    OCCURS  35 TIMES.
            02  HCRCP402-KAISU
                    PIC X(3)    OCCURS  35 TIMES.
            02  HCRCP402-KHNTEN
                    PIC X(7)    OCCURS  5 TIMES.
            02  HCRCP402-HOSPNAME   PIC X(80).
            02  HCRCP402-HOSPADRS   PIC X(100).
            02  HCRCP402-NAME   PIC X(50).
            02  HCRCP402-HEAD1-G.
              03  HCRCP402-HEAD1
                      PIC X(46)   OCCURS  2 TIMES.
            02  HCRCP402-HEAD2-G.
              03  HCRCP402-HEAD2
                      PIC X(2)    OCCURS  7 TIMES.
            02  HCRCP402-HEAD3-G.
              03  HCRCP402-HEAD3
                      PIC X(50)   OCCURS  2 TIMES.
            02  HCRCP402-HEAD4-G.
              03  HCRCP402-HEAD4
                      PIC X(62)   OCCURS  2 TIMES.
            02  HCRCP402-BODY   PIC X(52).
            02  HCRCP402-FOOT-G.
              03  HCRCP402-FOOT
                      PIC X(46)   OCCURS  4 TIMES.
            02  HCRCP402-SHOSHINTIME    PIC X(10).
            02  HCRCP402-TENKIYMD
                    PIC X(6)    OCCURS  5 TIMES.
            02  HCRCP402-FTNMONEYSAI    PIC X(11).
            02  HCRCP402-RENNUM PIC X(7).
            02  HCRCP402-SRYKA2 PIC X(60).
            02  HCRCP402-KHNTEN3    PIC X(7).
            02  HCRCP402-SONOTA PIC X(10).
            02  HCRCP402-KYUFUGAI1  PIC X(10).
            02  HCRCP402-KYUFUGAI2  PIC X(10).
            02  HCRCP402-KYUFUGAI3  PIC X(12).
            02  HCRCP402-KYUFUGAI-TEN   PIC X(5).
            02  HCRCP402-KYUFUGAI4  PIC X(2).
            02  HCRCP402-PAGE   PIC X(5).
            02  HCRCP402-TOTALTEN1
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCRCP402-FTNMONEY1
                    PIC X(8)    OCCURS  4 TIMES.
            02  HCRCP402-TITLE  PIC X(8).
