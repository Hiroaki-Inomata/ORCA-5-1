        01  HCM11V04.
            02  HCM11V04-TEISYUTUSAKI   PIC X(6).
            02  HCM11V04-PTNUM  PIC X(20).
            02  HCM11V04-SRYY   PIC X(2).
            02  HCM11V04-SRYM   PIC X(2).
            02  HCM11V04-PREFNUM    PIC X(2).
            02  HCM11V04-HOSPCDN    PIC X(20).
            02  HCM11V04-KHNTEN2
                    PIC X(7)    OCCURS  2 TIMES.
            02  HCM11V04-KHNTEN1    PIC X(7).
            02  HCM11V04-SRYKBN
                    PIC X(2)    OCCURS  35 TIMES.
            02  HCM11V04-TEKIYO
                    PIC X(52)   OCCURS  35 TIMES.
            02  HCM11V04-KOUTEN
                    PIC X(7)    OCCURS  35 TIMES.
            02  HCM11V04-TOTALTEN
                    PIC X(7)    OCCURS  3 TIMES.
            02  HCM11V04-FTNMONEY
                    PIC X(8)    OCCURS  3 TIMES.
            02  HCM11V04-OCR1   PIC X(67).
            02  HCM11V04-OCR2   PIC X(67).
            02  HCM11V04-OCR3   PIC X(67).
            02  HCM11V04-RJNFTNJANUM.
              03  HCM11V04-RJNFTNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCM11V04-KOH-TBL1.
              03  HCM11V04-KOHFTNJANUM1.
                04  HCM11V04-KOHFTNJANUM1-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCM11V04-KOHFTNJANUM1N  PIC X(20).
              03  HCM11V04-KOHJKYSNUM1.
                04  HCM11V04-KOHJKYSNUM1-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCM11V04-KOHJKYSNUM1N   PIC X(20).
            02  HCM11V04-KOH-TBL2.
              03  HCM11V04-KOHFTNJANUM2.
                04  HCM11V04-KOHFTNJANUM2-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCM11V04-KOHFTNJANUM2N  PIC X(20).
              03  HCM11V04-KOHJKYSNUM2.
                04  HCM11V04-KOHJKYSNUM2-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCM11V04-KOHJKYSNUM2N   PIC X(20).
            02  HCM11V04-HKNJANUM.
              03  HCM11V04-HKNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCM11V04-KYURATE    PIC X(8).
            02  HCM11V04-RJNJKYSNUM.
              03  HCM11V04-RJNJKYSNUM-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM11V04-HKNSBT1    PIC X(6).
            02  HCM11V04-HKNSBT2    PIC X(6).
            02  HCM11V04-HONKZK PIC X(10).
            02  HCM11V04-KIGO   PIC X(34).
            02  HCM11V04-NUM    PIC X(34).
            02  HCM11V04-KANANAME   PIC X(40).
            02  HCM11V04-KANANAME2  PIC X(50).
            02  HCM11V04-SEXNUM PIC X(2).
            02  HCM11V04-BIRTHDAY   PIC X(22).
            02  HCM11V04-JIYU   PIC X(20).
            02  HCM11V04-SEXN   PIC X(2).
            02  HCM11V04-TOKKI
                    PIC X(12)   OCCURS  5 TIMES.
            02  HCM11V04-HOSPTEL    PIC X(15).
            02  HCM11V04-SRYKA  PIC X(30).
            02  HCM11V04-BEDSU  PIC X(4).
            02  HCM11V04-BYOMEI
                    PIC X(54)   OCCURS  5 TIMES.
            02  HCM11V04-SRYYMD
                    PIC X(24)   OCCURS  5 TIMES.
            02  HCM11V04-TENKI
                    PIC X(10)   OCCURS  5 TIMES.
            02  HCM11V04-JINISSU    PIC X(2).
            02  HCM11V04-JINISSU1   PIC X(2).
            02  HCM11V04-JINISSU2   PIC X(2).
            02  HCM11V04-TENSU
                    PIC X(7)    OCCURS  35 TIMES.
            02  HCM11V04-KAISU
                    PIC X(3)    OCCURS  35 TIMES.
            02  HCM11V04-KHNTEN
                    PIC X(7)    OCCURS  5 TIMES.
            02  HCM11V04-HOSPNAME   PIC X(80).
            02  HCM11V04-HOSPADRS   PIC X(100).
            02  HCM11V04-NAME   PIC X(50).
            02  HCM11V04-HEAD1-G.
              03  HCM11V04-HEAD1
                      PIC X(46)   OCCURS  2 TIMES.
            02  HCM11V04-HEAD2-G.
              03  HCM11V04-HEAD2
                      PIC X(2)    OCCURS  7 TIMES.
            02  HCM11V04-HEAD3-G.
              03  HCM11V04-HEAD3
                      PIC X(50)   OCCURS  2 TIMES.
            02  HCM11V04-HEAD4-G.
              03  HCM11V04-HEAD4
                      PIC X(62)   OCCURS  2 TIMES.
            02  HCM11V04-BODY   PIC X(52).
            02  HCM11V04-FOOT-G.
              03  HCM11V04-FOOT
                      PIC X(46)   OCCURS  4 TIMES.
            02  HCM11V04-SHOSHINTIME    PIC X(10).
            02  HCM11V04-TENKIYMD
                    PIC X(6)    OCCURS  5 TIMES.
            02  HCM11V04-FTNMONEYSAI    PIC X(11).
            02  HCM11V04-FTNMONEYSAI2   PIC X(11).
            02  HCM11V04-RENNUM PIC X(7).
            02  HCM11V04-SRYKA2 PIC X(60).
            02  HCM11V04-KHNTEN3    PIC X(7).
            02  HCM11V04-SONOTA PIC X(10).
            02  HCM11V04-PAGE   PIC X(5).
            02  HCM11V04-MOJI1  PIC X(10).
            02  HCM11V04-MOJI2  PIC X(10).
            02  HCM11V04-TOTALTEN1
                    PIC X(7)    OCCURS  4 TIMES.
            02  HCM11V04-FTNMONEY1
                    PIC X(9)    OCCURS  4 TIMES.
            02  HCM11V04-FTNMONEY-GEN   PIC X(8).
            02  HCM11V04-GENMEN PIC X(10).
            02  HCM11V04-GENMENKIN  PIC X(7).
            02  HCM11V04-GENMEN2    PIC X(2).
            02  HCM11V04-LABEL  PIC X(14).
            02  HCM11V04-HEAD5  PIC X(20).
            02  HCM11V04-LOPTION    PIC X(100).
